extern crate gdk;
extern crate gtk;
extern crate gio;
extern crate glib;
extern crate sourceview;
extern crate sha2;

use gtk::prelude::*;
use gio::prelude::*;
use gtk::*;
use sourceview::{LanguageManagerExt, ViewExt, BufferExt};
use glib::GString;
use pyo3::prelude::*;
use sha2::digest::FixedOutput;
use sha2::digest::generic_array::GenericArray;
use sha2::Sha256;

use std::cell;
use std::thread;
use std::boxed;
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

// TODO: Get a hash of ast fields for a parsedname
// On getting a parsedName, make an entry in the table
// On getting a different entry for parsedName, make a history header.

struct Work {
    do_work: AtomicBool,
    die: AtomicBool,
    content: Mutex<Option<(GString,i32)>>
}

type Hash = GenericArray<u8, <Sha256 as FixedOutput>::OutputSize>;

// All AST nodes are assumed by be scheme-like cons cells
#[derive(Debug)]
enum PyExpr {
    PyRef(Hash),
    PyCall(boxed::Box<PyExpr>, Vec<PyExpr>),
    PyArg(u8) // DeBruijn index
}

// Every defined variable keeps track of the ast hashes its been assigned
#[derive(Debug)]
struct PyDef {
    ty: Hash,
    body: PyExpr
}

// TODO: maybe we can just reference the underlying python string instead?
#[derive(Debug)]
struct ParsedName {
    name: String,
    ast: PyDef,
    line: i32
}

fn to_pydef(_body: Vec<&PyAny>)-> PyResult<PyDef> {
    unimplemented!();
}

fn py_parse<'a>(ast: &'a PyModule, code: &str, offset: i32) ->PyResult<Vec<ParsedName>>  {
    let val : Vec<&PyAny> = ast.call1("parse", (code,))?.getattr("body")?.extract()?;
    val.iter().map(|x| {
        let l : i32 = x.getattr("lineno")?.extract()?;
        Ok(ParsedName {
            name: x.getattr("name")?.extract()?,
            ast: to_pydef(x.getattr("body")?.extract()?)?,
            line: l + offset - 1
    })}).collect()
}

fn spawn_parser(receiver: mpsc::Receiver<(GString, i32)>, sender: glib::Sender<String>) {
    let work = Arc::new(Work{
        do_work: AtomicBool::new(false),
        die: AtomicBool::new(false),
        content: Mutex::new(None)
    });
    let work1 = Arc::clone(&work);
    thread::spawn(move || {
        let gil = Python::acquire_gil();
        let py = gil.python();
        let ast = PyModule::import(py, "ast").unwrap();
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
                match py_parse(ast, code.as_str(), offset) {
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
