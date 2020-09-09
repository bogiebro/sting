use relm_derive::{Msg, widget};
use gtk::prelude::*;
use gtk::Orientation::Horizontal;
use gtk::WidgetExt;
use glib::GString;
use sourceview::{LanguageManagerExt, ViewExt, BufferExt, MarkExt};
use std::default::Default;
use std::collections::{HashMap, HashSet};
use std::time::{Instant, Duration};
use std::mem;
use relm::{connect, Relm, Widget, Component, timeout, create_component};
use pyo3::prelude::*;
use pyo3::exceptions;
use pyo3::ffi;
use pyo3::conversion::AsPyPointer;
use sha2::{Sha256, Digest};
use serde::{Serialize};

// TODO: prevent double-parsing on retrieval. There doesn't seem to be a good way of doing this.
// We can do a hack: record whatever hash we're about to modify. Modifications occur one by one.
// When we respond to a BufferUpdate, check if it's the one we just modified.
// If it is, don't add it to the set, and clear the warning hash


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

#[derive(Default)]
pub struct Maps {
    // Stores every definition ever made in the history of the program
    defs: HashMap<Hash, PyExpr>,

    // How to display defs to the user
    namespace: HashMap<Hash, (String, Option<Vec<String>>)>,

    // Children map to parents from which they were derived
    chain: HashMap<Hash, Hash>,

    // Which definitions are currently active
    view: HashSet<Hash>,

    // How to interpret names in new definitions
    names: HashMap<String, Hash>,

    // How long is the chain of hashes ending at this hash?
    // Not all hashes have entries- this is just a cache for branch tips
    len: HashMap<Hash, usize>,

    // Where in the buffer is the given name defined?
    marks: HashMap<Hash, sourceview::Mark>,

    // What is the widget that controls the hash's history?
    headers: HashMap<Hash, Component<Header>>,

    // What definitions are you currently editing?
    in_buffer: HashSet<Hash>
}

// To check which type of AST nodes we parsed, we keep an example of each node type
#[derive(Copy, Clone)]
struct ASTPtrs {
    ret_ty: *const ffi::PyObject,
    asn_ty: *const ffi::PyObject,
    call_ty: *const ffi::PyObject,
    bin_ty: *const ffi::PyObject,
    name_ty: *const ffi::PyObject,
    const_ty: *const ffi::PyObject
}


impl ASTPtrs {
    fn new(ast: &PyObject, py: Python) -> ASTPtrs {
        ASTPtrs {
            ret_ty: (ast.getattr(py, "Return").unwrap()).as_ptr(),
            asn_ty: (ast.getattr(py, "Assign").unwrap()).as_ptr(),
            bin_ty: (ast.getattr(py, "BinOp").unwrap()).as_ptr(),
            call_ty: (ast.getattr(py, "Call").unwrap()).as_ptr(),
            name_ty: (ast.getattr(py, "Name").unwrap()).as_ptr(),
            const_ty: (ast.getattr(py, "Constant").unwrap()).as_ptr()
        }
    }
}

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

struct PyMods {
    ast: PyObject,
    unparse: PyObject,
    helpers: PyObject,
}

impl PyMods {
    fn new<'a>(py: Python<'a>) -> PyMods {
        PyMods {
            ast: PyModule::import(py, "ast").unwrap().to_object(py),
            unparse: PyModule::import(py, "astunparse").unwrap().to_object(py),
            helpers: PyModule::from_code(py, AST_HELPERS, "ast_helpers.py", "ast_helpers").unwrap().to_object(py)
        }
    }
}

pub struct HeaderModel {
    adj: gtk::Adjustment,
    prev_val: usize,
    relm: Relm<Header>,
    hash: Hash
}

#[derive(Msg)]
pub enum HeaderMsg {
    SliderChange,
    SliderSet(Hash, usize),
    NewHash(Hash, usize),
}

#[widget]
impl Widget for Header {
    fn model(relm: &Relm<Self>, args: (Hash, usize)) -> HeaderModel {
        let (hash, len) = args;
        let lenf = len as f64;
        let adj = gtk::Adjustment::new(lenf, 1., lenf, 0., 0., 0.);
        HeaderModel {
            relm: relm.clone(),
            adj: adj,
            hash: hash,
            prev_val: len,
        }
    }

    fn update(&mut self, event: HeaderMsg) {
        match event {
            HeaderMsg::SliderChange => {
                let old_val = self.model.adj.get_value();
                let new_val = old_val.round();
                if old_val != new_val {
                    self.model.adj.set_value(new_val);
                    let uval = new_val as usize;
                    if uval != self.model.prev_val {
                        self.model.prev_val = uval;
                        let offset = (self.model.adj.get_upper() - new_val) as usize;
                        self.model.relm.stream().emit(HeaderMsg::SliderSet(self.model.hash, offset));
                    }

                }
            },
            HeaderMsg::NewHash(h,s) => {
                self.model.hash = h;
                self.model.prev_val = s;
                self.model.adj.set_upper(s as f64);
                self.model.adj.set_value(s as f64);
            },
            _ => ()
        }
    }

    fn init_view(&mut self) {
        self.hbox.set_size_request(500, -1);
        self.model.adj.set_value(self.model.prev_val as f64);
        connect!(self.model.relm, self.model.adj, connect_value_changed(_), HeaderMsg::SliderChange);
    }

    view! {
        #[name="hbox"]
        gtk::Box {
            orientation: Horizontal,
            hexpand: true,
            gtk::Scale {
                draw_value: false,
                orientation: Horizontal,
                adjustment: &self.model.adj,
                hexpand: true
            },
            gtk::Button {
                label: "Branch"
            }
        }
    }
}

pub struct Model {
    maps: Maps,
    tb: sourceview::Buffer,
    immut_tag: gtk::TextTag,
    def_tag: gtk::TextTag,
    changed_marks: Vec<sourceview::Mark>,
    relm: Relm<Win>,
    last_update: Instant,
    ast_ptrs: ASTPtrs,
    mods: PyMods
}

#[derive(Msg)]
pub enum WinMsg {
    ParseBuffer,
    BufferUpdate,
    GetDef(Hash, usize),
    Quit
}

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

#[widget]
impl Widget for Win {
    fn model(relm: &Relm<Self>, _: ()) -> Model {
        let lm = sourceview::LanguageManager::get_default().unwrap();
        let python = lm.get_language("python").unwrap();
        let gil = Python::acquire_gil();
        let py = gil.python();
        let mods = PyMods::new(py);
        let ptrs = ASTPtrs::new(&mods.ast, py);
        let tb = sourceview::Buffer::new_with_language(&python);
        let tt = tb.get_tag_table().unwrap();
        let immut_tag = gtk::TextTag::new(Some("immut"));
        let def_tag = gtk::TextTag::new(Some("definition"));
        immut_tag.set_property_editable(false);
        def_tag.set_property_background_rgba(Some(&gdk::RGBA::red()));
        tt.add(&immut_tag);
        tt.add(&def_tag);
        Model{
            tb: tb,
            immut_tag: immut_tag,
            def_tag: def_tag,
            changed_marks: Vec::new(),
            maps: Default::default(),
            relm: relm.clone(),
            last_update: Instant::now(),
            ast_ptrs: ptrs,
            mods: mods,
        }
    }

    fn add_def(&mut self, name: String, args: Option<Vec<String>>, hash: Hash, line: i32, line_end: i32) {
        if self.model.maps.in_buffer.contains(&hash) {
            eprintln!("ALREADY ADDED {:?} TO BUFFER", hash);
            return
        }
        let mut line_iter = self.model.tb.get_iter_at_line(line);
        let mut end_iter = self.model.tb.get_iter_at_line(line_end);
        if let Some(h) = self.model.maps.names.get(&name) {
            let len = self.model.maps.len.get(h).unwrap_or(&1) + 1;
            self.model.maps.len.insert(hash, len);
            self.model.maps.chain.insert(hash, *h);
            
            let prev_mark = self.model.maps.marks.remove(h).unwrap();
            let mark_copy = prev_mark.clone();
            let prev_iter = self.model.tb.get_iter_at_mark(&prev_mark);
            self.model.maps.marks.insert(hash, prev_mark);
            if line_iter != prev_iter {
                eprintln!("MOVING DEF TO PREVIOUS LOCATION");
                self.model.tb.delete(&mut line_iter, &mut end_iter);
            } else {
                self.model.tb.apply_tag(&self.model.def_tag, &line_iter, &end_iter);
            }
            match self.model.maps.headers.get(h) {
                None => {
                    let mut mark_iter = self.model.tb.get_iter_at_mark(&mark_copy);
                    let anchor = self.model.tb.create_child_anchor(&mut mark_iter).unwrap();
                    self.model.tb.insert(&mut mark_iter, "\n");
                    let start_iter = self.model.tb.get_iter_at_mark(&mark_copy);
                    self.model.tb.apply_tag(&self.model.immut_tag, &start_iter, &mark_iter);
                    self.model.tb.move_mark(&mark_copy, &mark_iter);
                    let comp = create_component((hash, len));
                    let widget: &gtk::Box = comp.widget();
                    self.view.add_child_at_anchor(widget, &anchor);
                    connect!(comp@HeaderMsg::SliderSet(hm,sm), self.model.relm, WinMsg::GetDef(hm,sm));
                    widget.show_all();
                    self.model.maps.headers.insert(hash, comp);
                },
                Some(comp) => comp.emit(HeaderMsg::NewHash(hash, len))
            }
        } else {
            let mark = self.model.tb.create_source_mark(None, "def", &line_iter).unwrap();
            self.model.tb.apply_tag(&self.model.def_tag, &line_iter, &end_iter);
            self.model.maps.marks.insert(hash, mark);
        }
        self.model.maps.view.insert(hash);
        self.model.maps.in_buffer.insert(hash);
        self.model.maps.names.insert(name.clone(), hash);
        self.model.maps.namespace.insert(hash, (name, args));
        self.get_def(&hash, &hash);
    }

    fn hash_expr(&mut self, expr: PyExpr) -> Hash {
        let hash = Sha256::digest(&bincode::serialize(&expr).unwrap()).into();
        self.model.maps.defs.insert(hash, expr);
        hash
    }

    fn hash_val(&mut self, val: &PyAny, args: &Vec<String>) -> PyResult<Hash> {
        let ty: *const ffi::PyObject = val.get_type().as_ptr();
        if ty == self.model.ast_ptrs.call_ty {
            let call_args = val.getattr("args")?.iter()?.map(|a| {
                self.hash_val(a?, args)
            }).collect::<PyResult<Vec<Hash>>>()?;
            let expr = PyExpr::Call(self.hash_val(val.getattr("func")?, args)?, call_args);
            Ok(self.hash_expr(expr))
        } else if ty == self.model.ast_ptrs.name_ty {
            let name = val.getattr("id")?.extract()?;
            match args.iter().position(|x| x == &name) {
                Some(ix) => Ok(self.hash_expr(PyExpr::DeBruijn(ix))),
                None => match self.model.maps.names.get(&name) {
                    Some(&h) => Ok(h),
                    None => Ok(self.hash_expr(PyExpr::Prim(name)))
                }
            }
        } else if ty == self.model.ast_ptrs.bin_ty {
            let left = self.hash_val(val.getattr("left")?, args)?;
            let right = self.hash_val(val.getattr("right")?, args)?;
            let op = String::from(val.getattr("op")?.get_type().name());
            Ok(self.hash_expr(PyExpr::BinOp(op, left, right)))
        } else if ty == self.model.ast_ptrs.const_ty {
            let cval = val.getattr("value")?;
            let expr = cval.extract().map(PyExpr::ConstInt).or(
                cval.extract().map(PyExpr::ConstFloat))?;
            Ok(self.hash_expr(expr))
        } else {
            let msg = format!("Unknown ast node with type {}", val.get_type().repr()?);
            exceptions::TypeError::into(msg)
        }
    }


    // Note that the ast module's line numbers are 1 based,
    // while gtk TextIter objects are 0 based.
    fn hash_def(&mut self, def: &PyAny, offset: i32) -> PyResult<()> {
        let args = def.getattr("args")?.getattr("args")?.iter()?.map(|x| {
            Ok(x?.getattr("arg")?.extract()?)
        }).collect::<PyResult<Vec<String>>>()?;
        for b_r in def.getattr("body")?.iter()? {
            let b = b_r?;
            let ty: *const ffi::PyObject = b.get_type().as_ptr();
            if ty == self.model.ast_ptrs.asn_ty {
                let name = b.getattr("targets")?.get_item(0)?.getattr("id")?.extract()?;
                let val = self.hash_val(b.getattr("value")?, &args)?;
                let line : i32 = b.getattr("lineno")?.extract()?;
                let line_end : i32 = b.getattr("end_lineno")?.extract()?;
                eprintln!("LINE {} offset {} end {}", line, offset, line_end);
                self.add_def(name, None, val, line + offset - 1, line_end + offset - 1);
            } else if ty == self.model.ast_ptrs.ret_ty {
                let name = def.getattr("name")?.extract()?;
                let val = self.hash_val(b.getattr("value")?, &args)?;
                let lam_val = self.hash_expr(PyExpr::Lam(args.len(), val));
                let line : i32 = def.getattr("lineno")?.extract()?;
                let line_end : i32 = def.getattr("end_lineno")?.extract()?;
                eprintln!("LINE {} offset {} end {}", line, offset, line_end);
                self.add_def(name, Some(args), lam_val, line + offset - 1, line_end + offset);
                return Ok(())
            } 
        }
        return exceptions::TypeError::into("Unknown statement type")
    }

    fn parse_str(&mut self, py: Python, gstr: GString, offset: i32) -> PyResult<()> {
        let astobj = self.model.mods.ast.clone();
        let ast: &PyModule = astobj.extract(py)?;
        let parsed = ast.call1("parse", (gstr.as_str(),))?;
        for x_r in parsed.getattr("body")?.iter()? {
            self.hash_def(x_r?, offset)?;
        }
        Ok(())
    }

    fn delete_region(&self, mark: &sourceview::Mark) -> gtk::TextIter {
        let mut start_iter = self.model.tb.get_iter_at_mark(mark);
        let mut end_iter = start_iter.clone();
        end_iter.forward_to_tag_toggle(Some(&self.model.def_tag));
        eprintln!("DELETING TEXT {}", start_iter.get_text(&end_iter).unwrap().as_str());
        self.model.tb.delete(&mut start_iter, &mut end_iter);
        start_iter
    }

    // Replace the defintion at `tip` in the buffer with the autoformatted definition for `h`
    fn get_def(&self, h: &Hash, tip: &Hash) {
        eprintln!("REPLACING DEF");
        let gil = Python::acquire_gil();
        let py = gil.python();
        let (s, oargs) = self.model.maps.namespace.get(h).unwrap();
        let val = self.get_text(py, h, oargs);
        let obj = match oargs {
            Some(args) => {
                let argrefs : Vec<&str> = args.iter().map(|x| x.as_str()).collect();
                self.model.mods.helpers.call_method1(py, "function_def", (s, argrefs, val)).unwrap()
            },
            None => self.model.mods.helpers.call_method1(py, "assign", (s, val)).unwrap()
        };
        let txt: String = self.model.mods.unparse.call_method1(py, "unparse", (obj,)).unwrap().extract(py).unwrap();
        eprintln!("Looking up {:?} in marks", tip);
        let mark = self.model.maps.marks.get(tip).unwrap();
        let mut start_iter = self.delete_region(mark);
        let trimmed = txt.trim_start();
        eprintln!("PUTTING STRING\n{}", trimmed);
        self.model.tb.insert(&mut start_iter, trimmed);
        let def_iter = self.model.tb.get_iter_at_mark(mark);
        self.model.tb.apply_tag(&self.model.def_tag, &def_iter, &start_iter);
    }

    fn get_text_ref(&self, py: Python, h: &Hash, oargs: &Option<Vec<String>>) -> PyObject {
        self.model.maps.namespace.get(h).map(
            |(s, _)| self.model.mods.ast.call_method1(py, "Name", (s,)).unwrap()
        ).unwrap_or_else(|| self.get_text(py, h, oargs))
    }

    fn get_text(&self, py: Python, h: &Hash, oargs: &Option<Vec<String>>) -> PyObject {
        match self.model.maps.defs.get(h).unwrap() {
            PyExpr::Prim(prim) => self.model.mods.ast.call_method1(py, "Name", (prim,)).unwrap(),
            PyExpr::Call(h2, args) => self.model.mods.helpers.call_method1(py, "call",
                (
                    self.get_text_ref(py, &h2, oargs),
                    args.iter().map(|a| self.get_text_ref(py, a, oargs)).collect::<Vec<PyObject>>()
                )).unwrap(),
            PyExpr::DeBruijn(n) => self.model.mods.ast.call_method1(
                py, "Name", (oargs.as_ref().unwrap()[*n].as_str(),)).unwrap(),
            PyExpr::Lam(_, h2) => self.model.mods.ast.call_method1(
                py, "Return", (self.get_text_ref(py, &h2, oargs),)).unwrap(),
            _ => unimplemented!()
        }
    }

    fn update(&mut self, event: WinMsg) {
        match event {
            WinMsg::ParseBuffer => {
                if Instant::now().duration_since(self.model.last_update) >= Duration::from_millis(600) {
                    let gil = Python::acquire_gil();
                    let py = gil.python();
                    let mut prev_changed_marks = Vec::new();
                    mem::swap(&mut self.model.changed_marks, &mut prev_changed_marks);
                    eprintln!("{} new regions:", prev_changed_marks.len());
                    for mark in prev_changed_marks.iter() {
                        let region_start = self.model.tb.get_iter_at_mark(mark);
                        let mut region_end = region_start.clone();
                        self.model.tb.delete_mark(mark);
                        region_end.forward_to_tag_toggle(Some(&self.model.def_tag));
                        for gstr in region_start.get_text(&region_end).into_iter() {
                            eprintln!("PARSING STRING:\n{}", gstr.as_str());
                            self.parse_str(py, gstr, region_start.get_line()).unwrap_or_else(|e| e.print(py))
                        }
                    }
                }
            },
            WinMsg::GetDef(h, offset) => {
                eprintln!("Getting a def at {}:\n {:?}", offset, h);
                let mut h_iter = h;
                let mut counter = offset;
                if counter > 0 {
                    while let Some(h_parent) = self.model.maps.chain.get(&h_iter) {
                        h_iter = *h_parent;
                        counter -= 1;
                        if counter == 0 { break }
                    }
                }
                self.get_def(&h_iter, &h);
            },
            WinMsg::BufferUpdate => {
                let ins_mark = self.model.tb.get_insert().unwrap();
                let mut region_start = self.model.tb.get_iter_at_mark(&ins_mark);
                region_start.backward_to_tag_toggle(Some(&self.model.def_tag));
                let len = self.model.tb.get_source_marks_at_iter(&mut region_start, Some("update")).len();
                if len == 0 {
                    let region_mark = self.model.tb.create_source_mark(None, "update", &region_start).unwrap();
                    self.model.changed_marks.push(region_mark);
                }
                self.model.last_update = Instant::now();
                timeout(self.model.relm.stream(), 600, || WinMsg::ParseBuffer)
            },
            WinMsg::Quit => gtk::main_quit()
        }
    }

    fn init_view(&mut self) {
        let provider = gtk::CssProvider::new();
        provider.load_from_data(STYLE.as_bytes()).unwrap();
        gtk::StyleContext::add_provider_for_screen(
            &gdk::Screen::get_default().unwrap(),
            &provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION);
        connect!(self.model.relm, self.model.tb, connect_changed(_), WinMsg::BufferUpdate)
    }

    view! {
        gtk::Window {
            #[name="view"]
            sourceview::View {
                buffer: Some(&self.model.tb),
                auto_indent: true,
                indent_on_tab: true,
                smart_backspace: true,
                show_line_numbers: true,
                tab_width: 4,
                indent_width: 4,
            },
            delete_event(_, _) => (WinMsg::Quit, gtk::Inhibit(false))
        },
    }
}

fn main() {
    Win::run(()).unwrap();
}
