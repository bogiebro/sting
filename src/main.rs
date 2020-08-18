extern crate gdk;
extern crate gtk;
extern crate gio;
extern crate glib;
extern crate sourceview;
extern crate sha2;
extern crate bimap;
extern crate bincode;
extern crate serde;

use gtk::prelude::*;
use gio::prelude::*;
use gtk::*;
use sourceview::{LanguageManagerExt, ViewExt, BufferExt};
use glib::GString;
use pyo3::prelude::*;
use pyo3::exceptions;
use pyo3::conversion::AsPyPointer;
use pyo3::ffi;
use sha2::{Sha256, Digest};
use sha2::digest::FixedOutput;
use sha2::digest::generic_array::GenericArray;
use bimap::BiMap;

use serde::{Serialize};
use std::cell;
use std::thread;
use std::time::Duration;
use std::sync::mpsc;
use std::sync::{Mutex, Arc};
use std::sync::atomic::{Ordering, AtomicBool};
use std::collections::HashMap;

const STYLE: &str = "
button {
  font: 12px monospace;
}

textview {
  font: 20px monospace;
}

scale {
  -gtk-icon-shadow: none;
}
";

fn build_branch_toggle(vbox: &Box) {
    let hbox = Box::new(Orientation::Horizontal, 0);
    let r1 = RadioButtonBuilder::new().draw_indicator(false).label("A").build();
    let r2 = RadioButtonBuilder::new().draw_indicator(false).label("B").build();
    r2.join_group(Some(&r1));
    hbox.pack_start(&r1, false, false, 10);
    hbox.pack_start(&r2, false, false, 10);
    vbox.pack_start(&hbox, false, false, 2);
}

fn make_header() -> Box {
    let vbox = Box::new(Orientation::Vertical, 0);
    let hbox = Box::new(Orientation::Horizontal, 0);
    let adj = Adjustment::new(0., 0., 1., 1., 1., 1.);
    let scale = Scale::new(Orientation::Horizontal, Some(&adj));
    scale.set_draw_value(false);
    hbox.pack_start(&scale, true, true, 0);
    let btn = Button::with_label("Branch");
    hbox.pack_end(&btn, false, false, 2);
    vbox.pack_end(&hbox, false, false, 0);
    vbox.set_size_request(500, -1);
    return vbox;
}

fn add_history_header(tv: &sourceview::View) -> Box {
    let tb = tv.get_buffer().unwrap();
    let tt = tb.get_tag_table().unwrap();
    let tag = TextTag::new(None);
    tag.set_property_editable(false);
    tt.add(&tag);
    let mut iter = tb.get_iter_at_line(0);
    tb.insert(&mut iter, "\n");
    let anchor = tb.create_child_anchor(&mut iter).unwrap();
    tb.insert(&mut iter, "\n");
    tb.apply_tag(&tag, &tb.get_iter_at_line(0), &iter);
    let b = make_header();
    tv.add_child_at_anchor(&b, &anchor);
    return b;
}

type Hash = [u8; 32];

#[derive(Serialize, Debug)]
enum PyExpr {
    Prim(String),
    Call(Hash, Vec<Hash>),
    BinOp(String, Hash, Hash),
    DeBruijn(usize),
    ConstInt(i64),
    ConstFloat(f64)
}

#[derive(Debug)]
struct ParsedName {
    name: String,
    args: Vec<String>,
    hash: Hash,
    line: i32
}

struct PyState {

    // This will store every definition ever made in the history of the program
    defs: HashMap<Hash, PyExpr>,

    // This allows us to look up previously hashed subexpressions when parsing,
    // and controls when we display ASTs using symbols instead of inlining
    names: BiMap<String, Hash>,

    // This allows propagating a new definition to all exprs that used the old one
    dependents: HashMap<Hash, Vec<Hash>>,

    // This allows us to replace something we previously assumed was a primitive
    // with a user defined hash
    prim_dependents: HashMap<String, Vec<Hash>>
}


fn hash_val(ptrs: PyPtrs, st: &mut PyState, val: &PyAny, args: &Vec<String>) -> PyResult<Hash> {
    let ty : *const ffi::PyObject = val.get_type().as_ptr();
    if ty == ptrs.call_ty {
        let call_args = val.getattr("args")?.iter()?.map(|a| {
            hash_val(ptrs, st, a?, args)
        }).collect::<PyResult<Vec<Hash>>>()?;
        let expr = PyExpr::Call(hash_val(ptrs, st, val.getattr("func")?, args)?, call_args);
        Ok(Sha256::digest(&bincode::serialize(&expr).unwrap()).into())
    } else if ty == ptrs.name_ty {
        let name = val.getattr("id")?.extract()?;
        match args.iter().position(|x| x == &name) {
            Some(ix) => Ok(Sha256::digest(&bincode::serialize(&PyExpr::DeBruijn(ix)).unwrap()).into()),
            None => match st.names.get_by_left(&name) {
                Some(&h) => Ok(h),
                None => {
                    Ok(Sha256::digest(&bincode::serialize(&PyExpr::Prim(name)).unwrap()).into())
                }
            }
        }
    } else if ty == ptrs.bin_ty {
        let left = hash_val(ptrs, st, val.getattr("left")?, args)?;
        let right = hash_val(ptrs, st, val.getattr("right")?, args)?;
        let op = String::from(val.getattr("op")?.get_type().name());
        Ok(Sha256::digest(&bincode::serialize(&PyExpr::BinOp(op, left, right)).unwrap()).into())
    } else if ty == ptrs.const_ty {
        let cval = val.getattr("value")?;
        let expr = cval.extract().map(PyExpr::ConstInt).or(
            cval.extract().map(PyExpr::ConstFloat))?;
        Ok(Sha256::digest(&bincode::serialize(&expr).unwrap()).into())
    } else {
        let msg = format!("Unknown ast node with type {}", val.get_type().repr()?);
        exceptions::TypeError::into(msg)
    }
}

fn hash_def(ptrs: PyPtrs, st: &mut PyState, def: &PyAny, offset: i32) -> PyResult<Vec<ParsedName>> {
    let line : i32 = def.getattr("lineno")?.extract()?;
    let args = def.getattr("args")?.getattr("args")?.iter()?.map(|x| {
        Ok(x?.getattr("arg")?.extract()?)
    }).collect::<PyResult<Vec<String>>>()?;
    let mut parsed_names = Vec::new();
    for b_r in def.getattr("body")?.iter()? {
        let b = b_r?;
        let ty : *const ffi::PyObject = b.get_type().as_ptr();
        if ty == ptrs.asn_ty {
            let name : String = b.getattr("targets")?.get_item(0)?.getattr("id")?.extract()?;
            let val = hash_val(ptrs, st, b.getattr("value")?, &args)?;
            st.names.insert(name.clone(), val);
            parsed_names.push(ParsedName{
                name: name,
                line: line + offset - 1,
                hash: hash_val(ptrs, st, b.getattr("value")?, &args)?,
                args: Vec::new()
            });
        } else if ty == ptrs.ret_ty {
            parsed_names.push(ParsedName {
                name: def.getattr("name")?.extract()?,
                line: line + offset - 1,
                hash: hash_val(ptrs, st, b.getattr("value")?, &args)?,
                args: args
            });
            return Ok(parsed_names);
        }
    }
    exceptions::TypeError::into("No return statement")
}

fn py_parse<'a>(ptrs: PyPtrs, st: &mut PyState, ast: &'a PyModule, code: &str, offset: i32) ->
        PyResult<Vec<Vec<ParsedName>>> {
    ast.call1("parse", (code,))?.getattr("body")?.iter()?.map(|x_r| {
        hash_def(ptrs, st, x_r?, offset)
    }).collect()
}

// To check which type of AST nodes we parsed, we keep a pointer to each node type
#[derive(Copy, Clone)]
struct PyPtrs {
    ret_ty: *const ffi::PyObject,
    asn_ty: *const ffi::PyObject,
    call_ty: *const ffi::PyObject,
    bin_ty: *const ffi::PyObject,
    name_ty: *const ffi::PyObject,
    const_ty: *const ffi::PyObject
}

struct Work {
    do_work: AtomicBool,
    die: AtomicBool,
    content: Mutex<Option<(GString,i32)>>
}

fn spawn_parser(receiver: mpsc::Receiver<(GString, i32)>, sender: glib::Sender<String>) {
    let work = Arc::new(Work{
        do_work: AtomicBool::new(false),
        die: AtomicBool::new(false),
        content: Mutex::new(None)
    });
    let work1 = Arc::clone(&work);
    thread::spawn(move || {
        let mut st = PyState {
            defs: HashMap::new(),
            names: BiMap::new(),
            dependents: HashMap::new(),
            prim_dependents: HashMap::new()
        };
        let gil = Python::acquire_gil();
        let py = gil.python();
        let ast = PyModule::import(py, "ast").unwrap();
        let ptrs = PyPtrs{
            ret_ty: (ast.getattr("Return").unwrap()).as_ptr(),
            asn_ty: (ast.getattr("Assign").unwrap()).as_ptr(),
            bin_ty: (ast.getattr("BinOp").unwrap()).as_ptr(),
            call_ty: (ast.getattr("Call").unwrap()).as_ptr(),
            name_ty: (ast.getattr("Name").unwrap()).as_ptr(),
            const_ty: (ast.getattr("Constant").unwrap()).as_ptr()
        };
        loop {
            {
                if work1.die.load(Ordering::Relaxed) {
                    break
                }
                work1.do_work.store(work1.content.lock().unwrap().is_some(), Ordering::Relaxed);
            }
            thread::sleep(Duration::from_millis(500));
            if work1.do_work.load(Ordering::Relaxed) {
                let lock = work1.content.lock();
                let (code, offset) = lock.unwrap().take().unwrap();
                match py_parse(ptrs, &mut st, ast, code.as_str(), offset) {
                    Ok(val) => {
                        let result = format!("{:?}", val);
                        sender.send(result).unwrap();
                    },
                    Err(e) => e.print(py)
                }
            }
        }
    });
    let work2 = Arc::clone(&work);
    thread::spawn(move || {
        loop {
            let a = receiver.recv();
            match a {
                Ok(o) => {
                    *(work2.content.lock().unwrap()) = Some(o);
                    work2.do_work.store(false, Ordering::Relaxed);
                },
                _ => work2.die.store(true, Ordering::Relaxed)
            }

        }
    });
}


fn build_ui(app: &Application) {
    let window = ApplicationWindow::new(app);
    window.set_title("Historian");
    window.set_default_size(350, 70);
    let lm = sourceview::LanguageManager::get_default().unwrap();
    let python = lm.get_language("python").unwrap();
    let tb = sourceview::Buffer::new_with_language(&python);
    let tv = sourceview::View::new_with_buffer(&tb);
    tv.set_auto_indent(true);
    tv.set_indent_on_tab(true);
    tv.set_smart_backspace(true);
    tv.set_indent_width(2);

    let (sender, receiver) = mpsc::sync_channel(1);
    let sendref = cell::RefCell::new(sender);

    let (tx, rx) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);
    spawn_parser(receiver, tx);

    rx.attach(None, move |nm| {
        eprintln!("GOT {}", nm);
        glib::Continue(true)
    });

    tb.connect_highlight_updated(move |tba, start, end| {
        let mut itrb = start.clone();
        if !tba.iter_has_context_class(&itrb, "funcdef") {
            if tba.iter_backward_to_context_class_toggle(&mut itrb, "funcdef") {
                if !tba.iter_has_context_class(&itrb, "funcdef") {
                    tba.iter_backward_to_context_class_toggle(&mut itrb, "funcdef");
                }
            }
        }
        let mut itrf = end.clone();
        tba.iter_forward_to_context_class_toggle(&mut itrf, "funcdef");
        match tba.get_text(&itrb, &itrf, true) {
            Some(s) => sendref.borrow_mut().send((s, itrb.get_line())).unwrap(),
            None => ()
        }
    });

   window.add(&tv);
   window.show_all();
}

fn main() {
    let application = Application::new(
        Some("com.github.gtk-rs.examples.basic"),
        Default::default(),
    ).expect("failed to initialize GTK application");

    application.connect_startup(|_| {
        let provider = gtk::CssProvider::new();
        provider.load_from_data(STYLE.as_bytes()).expect("Failed to load CSS");
        StyleContext::add_provider_for_screen(
            &gdk::Screen::get_default().expect("Error initializing gtk css provider."),
            &provider,
            STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
    });

    application.connect_activate(build_ui);
    application.run(&[]);
}
