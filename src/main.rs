extern crate gdk;
extern crate gtk;
extern crate gio;
extern crate glib;
extern crate sourceview;
extern crate sha2;
extern crate bincode;
extern crate serde;

use gtk::prelude::*;
use gio::prelude::*;
use sourceview::{LanguageManagerExt, ViewExt, BufferExt};
use glib::GString;
use pyo3::prelude::*;
use pyo3::exceptions;
use pyo3::conversion::AsPyPointer;
use pyo3::ffi;
use sha2::{Sha256, Digest};
use serde::{Serialize};
use std::rc::Rc;
use std::default::Default;
use std::thread;
use std::time::Duration;
use std::sync::{Mutex, Arc};
use std::sync::mpsc;
use std::sync::atomic::{Ordering, AtomicBool};
use std::collections::{HashMap, HashSet, VecDeque};

// TODO: inhibit changed signal on inserting formatted code
// TODO: use gtk-test

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

// This wraps some functions in python's ast library to make it easier to use from rust
const AST_HELPERS: &str = "
import ast

def function_def(name, args, body):
    return ast.FunctionDef(
        name,
        ast.arguments(
            args=[ast.arg(a, annotation=None) for a in args],
            defaults=[], vararg=None, kwarg=None),
        decorator_list=[],
        body=body)

def call(f, args):
    return ast.Call(f, args, keywords= {})

def assign(name, val):
    return ast.Assign(targets=[ast.Name(name)], value=val)
";

fn new_history_header(nm: NameInfo, f: Rc<dyn Fn(Hash, i32) -> ()>) -> gtk::Box {
    let lenf = nm.ix as f64;
    let hbox = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    let adj = gtk::Adjustment::new(lenf, 0., lenf, 1., 1., 1.);
    adj.connect_value_changed(move |a| {
        let old_val = a.get_value();
        let new_val = old_val.round();
        if old_val != new_val {
            a.set_value(new_val);
            f(nm.hash, new_val as i32 - nm.ix as i32)
        }
    });
    let scale = gtk::Scale::new(gtk::Orientation::Horizontal, Some(&adj));
    scale.set_draw_value(false);
    hbox.pack_start(&scale, true, true, 0);
    let btn = gtk::Button::with_label("Branch");
    hbox.pack_end(&btn, false, false, 2);
    hbox.set_size_request(500, -1);
    unsafe {hbox.set_data("adj", adj);}
    hbox
}

fn add_history_header(tv: &sourceview::View, nm: NameInfo, f: Rc<dyn Fn(Hash, i32) -> ()>) {
    let lenf = nm.ix as f64;
    let tb = tv.get_buffer().unwrap();
    let child_iter = tb.get_iter_at_line(nm.line - 1);
    match child_iter.get_child_anchor() {
        Some(children) => {
            let b = &children.get_widgets()[0];
            unsafe {
                let adj : &gtk::Adjustment = b.get_data("adj").unwrap();
                adj.set_upper(lenf);
                adj.set_value(lenf);
            }
        }
        None => {
            eprintln!("NO CHILD FOUND");
            let mut iter = tb.get_iter_at_line(nm.line);
            eprintln!("ADDING AT {}", nm.line);
            let tt = tb.get_tag_table().unwrap();
            let tag = gtk::TextTag::new(None);
            tag.set_property_editable(false);
            // tag.set_property_invisible(true);
            tt.add(&tag);
            tb.insert(&mut iter, "\n");
            let anchor = tb.create_child_anchor(&mut iter).unwrap();
            tb.insert(&mut iter, "\n");
            tb.apply_tag(&tag, &tb.get_iter_at_line(0), &iter);
            let b = new_history_header(nm, f);
            tv.add_child_at_anchor(&b, &anchor);
            b.show_all();
        }
    }
}

type Hash = [u8; 32];

#[derive(Serialize, Debug)]
enum PyExpr {
    Prim(String),
    Call(Hash, Vec<Hash>),
    BinOp(String, Hash, Hash),
    DeBruijn(usize),
    ConstInt(i64),
    ConstFloat(f64),
    Lam(usize, Hash)
}

#[derive(Debug)]
struct DefText {
    text: String,
    name_info: NameInfo
}

#[derive(Debug)]
struct NameInfo {
    hash: Hash,
    ix: u32,
    line: i32
}

#[derive(Default)]
struct PyState {

    // These define the history of the project

    // Stores every definition ever made in the history of the program
    defs: HashMap<Hash, PyExpr>,

    // How to display defs to the user
    namespace: HashMap<Hash, (String, Option<Vec<String>>)>,

    // Children map to parents from which they were derived
    chain: HashMap<Hash, Hash>,

    // Which definitions are currently active
    view: HashSet<Hash>,

    // These are convenient caches that can be derived from info above

    // How to interpret names in new definitions
    names: HashMap<String, Hash>,

    // Maps from parent to child. Eventually, we'll have one map per branch
    branch_child: HashMap<Hash, Hash>,

    // How long is the chain of hashes ending at this hash?
    // Not all hashes have entries- this is just a cache for branch tips
    len: HashMap<Hash, u32>,

    // This is data necessary for the display buffer

    // Where in the buffer is the given name defined?
    lineno: HashMap<Hash, i32>

}

// Hash a PyExpr and add it to the state
fn hash_expr(st: &mut PyState, expr: PyExpr) -> Hash {
    let hash = Sha256::digest(&bincode::serialize(&expr).unwrap()).into();
    st.defs.insert(hash, expr);
    hash
}

// Get the hash of a Python ast node
fn hash_val(ptrs: PyPtrs, st: &mut PyState, val: &PyAny, args: &Vec<String>) -> PyResult<Hash> {
    let ty : *const ffi::PyObject = val.get_type().as_ptr();
    if ty == ptrs.call_ty {
        let call_args = val.getattr("args")?.iter()?.map(|a| {
            hash_val(ptrs, st, a?, args)
        }).collect::<PyResult<Vec<Hash>>>()?;
        let expr = PyExpr::Call(hash_val(ptrs, st, val.getattr("func")?, args)?, call_args);
        Ok(hash_expr(st, expr))
    } else if ty == ptrs.name_ty {
        let name = val.getattr("id")?.extract()?;
        match args.iter().position(|x| x == &name) {
            Some(ix) => Ok(hash_expr(st, PyExpr::DeBruijn(ix))),
            None => match st.names.get(&name) {
                Some(&h) => Ok(h),
                None => Ok(hash_expr(st, PyExpr::Prim(name)))
            }
        }
    } else if ty == ptrs.bin_ty {
        let left = hash_val(ptrs, st, val.getattr("left")?, args)?;
        let right = hash_val(ptrs, st, val.getattr("right")?, args)?;
        let op = String::from(val.getattr("op")?.get_type().name());
        Ok(hash_expr(st, PyExpr::BinOp(op, left, right)))
    } else if ty == ptrs.const_ty {
        let cval = val.getattr("value")?;
        let expr = cval.extract().map(PyExpr::ConstInt).or(
            cval.extract().map(PyExpr::ConstFloat))?;
        Ok(hash_expr(st, expr))
    } else {
        let msg = format!("Unknown ast node with type {}", val.get_type().repr()?);
        exceptions::TypeError::into(msg)
    }
}

// Associate a hash with a name, updating pointers to previous definitions
fn update_name(st: &mut PyState, name: String, args: Option<Vec<String>>, val: Hash) {
    let prev = st.names.get(&name).map(|x| *x);
    st.names.insert(name.clone(), val);
    st.namespace.insert(val, (name, args));
    match prev {
        Some(h) => {
            if h != val {
                let len = st.len.get(&h).unwrap_or(&1) + 1;
                st.len.insert(val, len);
                st.chain.insert(val, h);
            }
        },
        None => ()
    }
}

// Add a new definition to the state, view and lineno
fn add_def(st: &mut PyState, added: &mut Vec<Hash>, name: String, args: Option<Vec<String>>, hash: Hash, line: i32) {
    if st.lineno.get(&hash).is_none() {
        eprintln!("NO HASH {:?}", hash);
        added.push(hash);
    }
    st.view.insert(hash);
    st.lineno.insert(hash, line);
    update_name(st, name, args, hash);
}

// Store all definitions from a python ast into the state
fn hash_def(ptrs: PyPtrs, st: &mut PyState, def: &PyAny, offset: i32) -> PyResult<Vec<Hash>> {
    let args = def.getattr("args")?.getattr("args")?.iter()?.map(|x| {
        Ok(x?.getattr("arg")?.extract()?)
    }).collect::<PyResult<Vec<String>>>()?;
    let mut added = Vec::new();
    for b_r in def.getattr("body")?.iter()? {
        let b = b_r?;
        let ty : *const ffi::PyObject = b.get_type().as_ptr();
        if ty == ptrs.asn_ty {
            let name = b.getattr("targets")?.get_item(0)?.getattr("id")?.extract()?;
            let val = hash_val(ptrs, st, b.getattr("value")?, &args)?;
            let line : i32 = b.getattr("lineno")?.extract()?;
            add_def(st, &mut added, name, None, val, line + offset - 1);
        } else if ty == ptrs.ret_ty {
            let name = def.getattr("name")?.extract()?;
            let val = hash_val(ptrs, st, b.getattr("value")?, &args)?;
            let lam_val = hash_expr(st, PyExpr::Lam(args.len(), val));
            let line : i32 = def.getattr("lineno")?.extract()?;
            add_def(st, &mut added, name, Some(args), lam_val, line + offset - 1);
            return Ok(added);
        }
    }
    exceptions::TypeError::into("No return statement")
}

// Parse new definitions and add them to the state
fn py_parse<'a>(ptrs: PyPtrs, st: &mut PyState, ast: &'a PyModule, code: &str, offset: i32) ->
        PyResult<impl Iterator<Item=Hash>> {
    let vecs : PyResult<Vec<Vec<Hash>>> = ast.call1("parse",
        (code,))?.getattr("body")?.iter()?.map(|x_r| {
        hash_def(ptrs, st, x_r?, offset)
    }).collect();
    Ok(vecs?.into_iter().flatten())
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

impl PyPtrs {
    fn new(ast: &PyModule) -> PyPtrs {
        PyPtrs{
            ret_ty: (ast.getattr("Return").unwrap()).as_ptr(),
            asn_ty: (ast.getattr("Assign").unwrap()).as_ptr(),
            bin_ty: (ast.getattr("BinOp").unwrap()).as_ptr(),
            call_ty: (ast.getattr("Call").unwrap()).as_ptr(),
            name_ty: (ast.getattr("Name").unwrap()).as_ptr(),
            const_ty: (ast.getattr("Constant").unwrap()).as_ptr()
        }
    }
}

#[derive(Default)]
struct Work {
    do_work: AtomicBool,
    die: AtomicBool,
    content: Mutex<Option<Msg>>
}

// To interact with the controller, other threads must send it messages
// These are possible messages to send
enum Msg {
    ParseDef(GString, i32), // Store new definitions from the user
    GetDef(Hash, i32) // Fetch an uparsed definition n steps after this hash
}

fn setup_textview(tv: &sourceview::View) {
    tv.set_auto_indent(true);
    tv.set_indent_on_tab(true);
    tv.set_smart_backspace(true);
    tv.set_indent_width(2)
}

// If the definition the user is editing hasn't changed after a short pause,
// we send a ParseDef message to the interpreter thread.
fn spawn_parse_after_delay(work: Arc<Work>, pycall_tx: mpsc::SyncSender<Msg>) {
    thread::spawn(move || {
        loop {
            {
                if work.die.load(Ordering::Relaxed) { break }
                work.do_work.store(work.content.lock().unwrap().is_some(), Ordering::Relaxed);
            }
            thread::sleep(Duration::from_millis(600));
            if work.do_work.load(Ordering::Relaxed) {
                let msg = work.content.lock().unwrap().take().unwrap();
                pycall_tx.send(msg).unwrap();
            }
        }
    });
}

// Get the ast of a definition
fn get_def<'a>(h: &Hash, queue: &mut VecDeque<Hash>, st: &'a PyState,
        helpers: &'a PyModule, ast: &'a PyModule) -> &'a PyAny {
    let (s, oargs) = st.namespace.get(h).unwrap();
    let val = get_text(h, oargs, queue, st, helpers, ast);
    match oargs {
        Some(args) => {
            let argrefs : Vec<&str> = args.iter().map(|x| x.as_str()).collect();
            helpers.call1("function_def", (s, argrefs, val)).unwrap()
        },
        None => helpers.call1("assign", (s, val)).unwrap()
    }
}

// Get an ast node referencing a name if one exists. Otherwise produce the code for the definition
fn get_text_ref<'a>(h: &Hash, oargs: &Option<Vec<String>>, queue: &mut VecDeque<Hash>,
        st: &'a PyState, helpers: &'a PyModule, ast: &'a PyModule) -> &'a PyAny {
    st.namespace.get(h).map(|(s, _)| ast.call1("Name", (s,)).unwrap()).unwrap_or_else(||
        get_text(h, oargs, queue, st, helpers, ast))
}

fn get_text<'a>(h: &Hash, oargs: &Option<Vec<String>>, queue: &mut VecDeque<Hash>,
        st: &'a PyState, helpers: &'a PyModule, ast: &'a PyModule) -> &'a PyAny {
    match st.defs.get(h).unwrap() {
        PyExpr::Prim(prim) => ast.call1("Name", (prim,)).unwrap(),
        PyExpr::Call(h2, args) => helpers.call1(
            "call",
            (
                get_text_ref(&h2, oargs, queue, st, helpers, ast),
                args.iter().map(|a| get_text_ref(a, oargs, queue, st, helpers, ast)).collect::<Vec<&PyAny>>()
            )).unwrap(),
        PyExpr::DeBruijn(n) => ast.call1("Name", (oargs.as_ref().unwrap()[*n].as_str(),)).unwrap(),
        PyExpr::Lam(_, h2) => ast.call1("Return", (get_text_ref(&h2, oargs, queue, st, helpers, ast),)).unwrap(),
        _ => unimplemented!()
    }
}

// Start the controller. Listen for messages that want to read or write state info and respond with DefText
fn handle_msgs(pycall_rx: mpsc::Receiver<Msg>, def_changed_tx: glib::Sender<DefText>) {
    let gil = Python::acquire_gil();
    let py = gil.python();
    let ast = PyModule::import(py, "ast").unwrap();
    let unparse = PyModule::import(py, "astunparse").unwrap();
    let ptrs = PyPtrs::new(ast);
    let helpers = PyModule::from_code(py, AST_HELPERS, "ast_helpers.py", "ast_helpers").unwrap();
    let mut st: PyState = Default::default();

    let send_def_changeds = |st_imm: &PyState, mut queue: VecDeque<Hash>| {
        while let Some(vv) = queue.pop_front() {
            let vv_ast = get_def(&vv, &mut queue, st_imm, &helpers, &ast);
            let vv_txt : String = unparse.call1("unparse", (vv_ast,)).unwrap().extract().unwrap();
            let vv = NameInfo{
                hash: vv,
                ix: *st_imm.len.get(&vv).unwrap_or(&1),
                line: *st_imm.lineno.get(&vv).unwrap_or(&0)
            };
            def_changed_tx.send(DefText{text: vv_txt.trim_start().to_string(), name_info: vv}).unwrap();
        }
    };
    
    loop {
        match pycall_rx.recv().unwrap() {
            Msg::GetDef(h, ix) => {
                let mut h_iter = h;
                let mut counter = ix;
                if counter > 0 {
                    while let Some(h_child) = st.branch_child.get(&h_iter) {
                        h_iter = *h_child;
                        counter -= 1;
                        if counter == 0 { break }
                    }
                } else if counter < 0 {
                    while let Some(h_parent) = st.chain.get(&h_iter) {
                        h_iter = *h_parent;
                        counter += 1;
                        if counter == 0 { break }
                    }
                }
                let mut queue = VecDeque::with_capacity(1);
                queue.push_back(h_iter);
                send_def_changeds(&st, queue);
            }
            Msg::ParseDef(s, ix) => {
                match py_parse(ptrs, &mut st, ast, s.as_str(), ix) {
                    Ok(val) => send_def_changeds(&st, val.collect::<VecDeque<Hash>>()),
                    Err(e) => e.print(py)
                }
            }
        }
    }
}


// When we insert a header, the line numbers get off.
// Also: when we do pretty much anything, line numbers get off
// It would be much nicer to map hashes to the child widgets directly

// Create a closure to update DefText in the buffer. If this definition is shown for the first time,
// register Msg sends to the controller on slider changes
fn update_buffer(pycall_tx: mpsc::SyncSender<Msg>, tv: sourceview::View) -> impl FnMut(DefText) -> glib::Continue {
    move |upd| {
        let tbb = tv.get_buffer().unwrap();
        let tb = tbb.downcast::<sourceview::Buffer>().unwrap();
        let mut iter0 = tb.get_iter_at_line(upd.name_info.line);
        let mut iter = iter0.clone();
        tb.iter_forward_to_context_class_toggle(&mut iter, "funcdef");
        tb.iter_forward_to_context_class_toggle(&mut iter, "funcdef");
        match tb.get_text(&iter0, &iter, true) {
            Some(gstr) => eprintln!("TEXT IS {:?}", gstr.as_str()),
            _ => ()
        }
        tb.delete(&mut iter0, &mut iter);
        tb.insert(&mut iter0, &upd.text);
        let pycall_tx2 = pycall_tx.clone();
        if upd.name_info.ix > 1 {
            add_history_header(&tv, upd.name_info, Rc::new(
                move |h, ix| pycall_tx2.send(Msg::GetDef(h, ix)).unwrap()));
        }
        glib::Continue(true)
    }
}

// Create closure to update mutex with newly entered user code
fn update_work(work: Arc<Work>) -> impl Fn(&sourceview::Buffer, &gtk::TextIter, &gtk::TextIter) {
    move |tba, start, end| {
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
            Some(s) => {
                *(work.content.lock().unwrap()) = Some(Msg::ParseDef(s, itrb.get_line()));
                work.do_work.store(false, Ordering::Relaxed);
            }
            None => ()
        }
    }
}

fn build_ui(app: &gtk::Application) {
    let window = gtk::ApplicationWindow::new(app);
    window.set_title("Sting");
    window.set_default_size(350, 70);
    let lm = sourceview::LanguageManager::get_default().unwrap();
    let python = lm.get_language("python").unwrap();
    let tb = sourceview::Buffer::new_with_language(&python);
    let tv = sourceview::View::new_with_buffer(&tb);
    setup_textview(&tv);

    // When new definitions should replace parts of the buffer, the replacements go in this queue
    let (def_changed_tx, def_changed_rx) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);

    // When Msgs need to handled by the controller, they go in this queue
    let (pycall_tx, pycall_rx) = mpsc::sync_channel(5);

    // The controller may send DefTexts to the def_changed queue.
    thread::spawn(move || handle_msgs(pycall_rx, def_changed_tx));

    // When definitions change, we must update the buffer to reflect that
    def_changed_rx.attach(None, update_buffer(pycall_tx.clone(), tv.clone()));

    // When new definitons are entered by the user, they are stuck in this mutex
    let work : Arc<Work> = Default::default();

    // After a pause, user entries become ParseDef Msgs and are sent to the controller
    spawn_parse_after_delay(work.clone(), pycall_tx);
    tb.connect_highlight_updated(update_work(work.clone()));

    app.connect_shutdown(move |_| work.die.store(true, Ordering::Relaxed));
    window.add(&tv);
    window.show_all();
}

fn main() {
    let application = gtk::Application::new(
        Some("com.sting.python"),
        Default::default(),
    ).expect("failed to initialize GTK application");

    application.connect_startup(|_| {
        let provider = gtk::CssProvider::new();
        provider.load_from_data(STYLE.as_bytes()).expect("Failed to load CSS");
        gtk::StyleContext::add_provider_for_screen(
            &gdk::Screen::get_default().expect("Error initializing gtk css provider."),
            &provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
    });

    application.connect_activate(build_ui);
    application.run(&[]);
}
