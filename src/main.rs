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
use std::thread;
use std::time::Duration;
use std::sync::{Mutex, Arc};
use std::sync::atomic::{Ordering, AtomicBool};
use std::collections::{HashMap, HashSet};

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

fn build_branch_toggle(vbox: &gtk::Box) {
    let hbox = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    let r1 = gtk::RadioButtonBuilder::new().draw_indicator(false).label("A").build();
    let r2 = gtk::RadioButtonBuilder::new().draw_indicator(false).label("B").build();
    r2.join_group(Some(&r1));
    hbox.pack_start(&r1, false, false, 10);
    hbox.pack_start(&r2, false, false, 10);
    vbox.pack_start(&hbox, false, false, 2);}

fn new_history_header(nm: ParsedName, f: Rc<dyn Fn(ParsedName) -> ()>) -> gtk::Box {
    let lenf = nm.ix as f64;
    let hbox = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    let adj = gtk::Adjustment::new(lenf, 0., lenf, 1., 1., 1.);
    adj.connect_value_changed(move |a| {
        let old_val = a.get_value();
        let new_val = old_val.round();
        if old_val != new_val {
            a.set_value(new_val);
            f(ParsedName{ix:(new_val as u32) - nm.ix, ..nm})}});
    let scale = gtk::Scale::new(gtk::Orientation::Horizontal, Some(&adj));
    scale.set_draw_value(false);
    hbox.pack_start(&scale, true, true, 0);
    let btn = gtk::Button::with_label("Branch");
    hbox.pack_end(&btn, false, false, 2);
    hbox.set_size_request(500, -1);
    unsafe {hbox.set_data("adj", adj);}
    hbox}

fn add_history_header(tv: &sourceview::View, nm: ParsedName, f: Rc<dyn Fn(ParsedName) -> ()>) {
    let lenf = nm.ix as f64;
    let tb = tv.get_buffer().unwrap();
    let child_iter = tb.get_iter_at_line(nm.line - 1);
    match child_iter.get_child_anchor() {
        Some(children) => {
            let b = &children.get_widgets()[0];
            unsafe {
                let adj : &gtk::Adjustment = b.get_data("adj").unwrap();
                adj.set_upper(lenf);
                adj.set_value(lenf); }}
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
            b.show_all(); }}}

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
    hash: Hash,
    ix: u32,
    line: i32
}

struct PyState {

    // These define the history of the project

    // Stores every definition ever made in the history of the program
    defs: HashMap<Hash, PyExpr>,

    // How to display defs to the user
    namespace: HashMap<Hash, String>,

    // Children map to parents from which they were derived
    chain: HashMap<Hash, Hash>,

    // Which definitions are currently active
    view: HashSet<Hash>,

    // These are convenient caches that can be derived from info above

    // How to interpret names in new definitions
    names: HashMap<String, Hash>,

    // How long is the chain of hashes ending at this hash?
    // Not all hashes have entries- this is just a cache for branch tips
    len: HashMap<Hash, u32>,

}

impl PyState {
    fn new() -> PyState {
        PyState {
            defs: HashMap::new(),
            namespace: HashMap::new(),
            chain: HashMap::new(),
            view: HashSet::new(),
            names: HashMap::new(),
            len: HashMap::new()}}
}


fn hash_expr(st: &mut PyState, expr: PyExpr) -> Hash {
    let hash = Sha256::digest(&bincode::serialize(&expr).unwrap()).into();
    st.defs.insert(hash, expr);
    hash}

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
            Some(ix) => Ok(Sha256::digest(&bincode::serialize(&PyExpr::DeBruijn(ix)).unwrap()).into()),
            None => match st.names.get(&name) {
                Some(&h) => Ok(h),
                None => Ok(hash_expr(st, PyExpr::Prim(name)))}}
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
        exceptions::TypeError::into(msg)}}

fn update_name(st: &mut PyState, name: String, val: Hash, prev: Option<Hash>) -> u32 {
    st.names.insert(name.clone(), val);
    st.namespace.insert(val, name);
    st.view.insert(val);
    match prev {
        None => {
            st.len.insert(val, 1);
            1}
        Some(h) => {
            let len = st.len.get(&h).unwrap() + 1;
            st.len.insert(val, len);
            st.chain.insert(val, h);
            len}}}

fn add_def(st: &mut PyState, parsed_names: &mut Vec<ParsedName>, name: String, hash: Hash, line: i32) {
    match st.names.get(&name) {
        None => { update_name(st, name, hash, None); },
        Some(&h) => if h != hash {
            parsed_names.push(ParsedName{
                line: line,
                hash: hash,
                ix: update_name(st, name, hash, Some(h))})}}}

fn hash_def(ptrs: PyPtrs, st: &mut PyState, def: &PyAny, offset: i32) -> PyResult<Vec<ParsedName>> {
    let args = def.getattr("args")?.getattr("args")?.iter()?.map(|x| {
        Ok(x?.getattr("arg")?.extract()?)
    }).collect::<PyResult<Vec<String>>>()?;
    let mut parsed_names = Vec::new();
    for b_r in def.getattr("body")?.iter()? {
        let b = b_r?;
        let ty : *const ffi::PyObject = b.get_type().as_ptr();
        if ty == ptrs.asn_ty {
            let name = b.getattr("targets")?.get_item(0)?.getattr("id")?.extract()?;
            let val = hash_val(ptrs, st, b.getattr("value")?, &args)?;
            let line : i32 = b.getattr("lineno")?.extract()?;
            add_def(st, &mut parsed_names, name, val, line + offset - 1);
        } else if ty == ptrs.ret_ty {
            let name = def.getattr("name")?.extract()?;
            let val = hash_val(ptrs, st, b.getattr("value")?, &args)?;
            let line : i32 = def.getattr("lineno")?.extract()?;
            add_def(st, &mut parsed_names, name, val, line + offset - 1);
            return Ok(parsed_names); }}
    exceptions::TypeError::into("No return statement")}

fn py_parse<'a>(ptrs: PyPtrs, st: &mut PyState, ast: &'a PyModule, code: &str, offset: i32) ->
        PyResult<Vec<Vec<>>> {
    ast.call1("parse", (code,))?.getattr("body")?.iter()?.map(|x_r| {
        hash_def(ptrs, st, x_r?, offset)
    }).collect()}

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
            const_ty: (ast.getattr("Constant").unwrap()).as_ptr()}}
}

struct Work {
    do_work: AtomicBool,
    die: AtomicBool,
    content: Mutex<Option<(GString,i32)>>
}

impl Work {
    fn new() -> Work {
        Work{
            do_work: AtomicBool::new(false),
            die: AtomicBool::new(false),
            content: Mutex::new(None) }}
}

fn setup_textview(tv: &sourceview::View) {
    tv.set_auto_indent(true);
    tv.set_indent_on_tab(true);
    tv.set_smart_backspace(true);
    tv.set_indent_width(2)}

fn build_ui(app: &gtk::Application) {
    let window = gtk::ApplicationWindow::new(app);
    window.set_title("Sting");
    window.set_default_size(350, 70);
    let lm = sourceview::LanguageManager::get_default().unwrap();
    let python = lm.get_language("python").unwrap();
    let tb = sourceview::Buffer::new_with_language(&python);
    let tv = sourceview::View::new_with_buffer(&tb);
    setup_textview(&tv);

    let (def_changed_tx, def_changed_rx) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);
    let work = Arc::new(Work::new());
    let st = Arc::new(Mutex::new(PyState::new()));

    let st2 = st.clone();
    let f = Rc::new(move |pn: ParsedName| {
        let mut h = pn.hash;
        let raw_st = st2.lock().unwrap();
        for _ in 1..pn.ix {
            h = *raw_st.chain.get(&h).unwrap();
        }
        eprintln!("CURRENT IS {:?}", h);
    });

    let work1 = Arc::clone(&work);
    let st1 = Arc::clone(&st);
    thread::spawn(move || {
        let gil = Python::acquire_gil();
        let py = gil.python();
        let ast = PyModule::import(py, "ast").unwrap();
        let ptrs = PyPtrs::new(ast);
        loop {
            {
                if work1.die.load(Ordering::Relaxed) { break }
                work1.do_work.store(work1.content.lock().unwrap().is_some(), Ordering::Relaxed);
            }
            thread::sleep(Duration::from_millis(400));
            if work1.do_work.load(Ordering::Relaxed) {
                let lock = work1.content.lock();
                let mut raw_st  = st1.lock().unwrap();
                let (code, offset) = lock.unwrap().take().unwrap();
                match py_parse(ptrs, &mut raw_st, ast, code.as_str(), offset) {
                    Ok(val) => {
                        for v in val {
                            for vv in v{
                                def_changed_tx.send(vv).unwrap(); }}},
                    Err(e) => e.print(py) }}}});

    let tv2 = tv.clone();
    let g = move |nm| {
        add_history_header(&tv2, nm, f.clone());
        glib::Continue(true)
    };

    def_changed_rx.attach(None, g);

    let work2 = Arc::clone(&work);
    tb.connect_highlight_updated(move |tba, start, end| {
        let mut itrb = start.clone();
        if !tba.iter_has_context_class(&itrb, "funcdef") {
            if tba.iter_backward_to_context_class_toggle(&mut itrb, "funcdef") {
                if !tba.iter_has_context_class(&itrb, "funcdef") {
                    tba.iter_backward_to_context_class_toggle(&mut itrb, "funcdef"); } } }
        let mut itrf = end.clone();
        tba.iter_forward_to_context_class_toggle(&mut itrf, "funcdef");
        match tba.get_text(&itrb, &itrf, true) {
            Some(s) => {
                *(work2.content.lock().unwrap()) = Some((s, itrb.get_line()));
                work2.do_work.store(false, Ordering::Relaxed);}
            None => ()}});

   app.connect_shutdown(move |_| {
        work.die.store(true, Ordering::Relaxed);
   });
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
    application.run(&[]);}
