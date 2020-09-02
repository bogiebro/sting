use relm_derive::{Msg, widget};
use gtk::prelude::*;
use gtk::Orientation::Horizontal;
use gtk::WidgetExt;
use glib::GString;
use sourceview::{LanguageManagerExt, ViewExt, BufferExt, MarkExt};
use std::default::Default;
use std::collections::{HashMap, HashSet};
use std::time::{Instant, Duration};
use relm::{connect, Relm, Widget, Component, timeout, create_component};
use pyo3::prelude::*;
use pyo3::exceptions;
use pyo3::ffi;
use pyo3::conversion::AsPyPointer;
use sha2::{Sha256, Digest};
use serde::{Serialize};

// TODO: make tags for every definition
// Whenever the cursor changes position, look up what tag its in or in between
// Add this region to the update list
// When we finally update, send only regions in the update list (and clear the update list)

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
    marks: HashMap<Hash, (sourceview::Mark, Option<Component<Header>>)>,
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
    relm: Relm<Header>,
    hash: Hash
}

#[derive(Msg)]
pub enum HeaderMsg {
    SliderChange,
    SliderSet(Hash, usize)
}

#[widget]
impl Widget for Header {
    fn model(relm: &Relm<Self>, args: (Hash, usize)) -> HeaderModel {
        let (hash, len) = args;
        let lenf = len as f64;
        HeaderModel {
            relm: relm.clone(),
            adj: gtk::Adjustment::new(lenf, 0., lenf, 1., 1., 1.),
            hash: hash
        }
    }

    fn update(&mut self, event: HeaderMsg) {
        match event {
            HeaderMsg::SliderChange => {
                let old_val = self.model.adj.get_value();
                let new_val = old_val.round();
                if old_val != new_val {
                    self.model.adj.set_value(new_val);
                    let offset = (self.model.adj.get_upper() - new_val) as usize;
                    self.model.relm.stream().emit(HeaderMsg::SliderSet(self.model.hash, offset));
                }
            },
            _ => ()
        }
    }

    fn init_view(&mut self) {
        self.hbox.set_size_request(500, -1);
        connect!(self.model.relm, self.model.adj, connect_value_changed(_), HeaderMsg::SliderChange)
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
    tag: gtk::TextTag,
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

// Idea: try just adding the widget to an vbox. Ensure that that works. Then go back to
// child anchor adding

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
        let tag = gtk::TextTag::new(None);
        tag.set_property_editable(false);
        tt.add(&tag);
        Model{
            tb: tb,
            tag: tag,
            maps: Default::default(),
            relm: relm.clone(),
            last_update: Instant::now(),
            ast_ptrs: ptrs,
            mods: mods,
        }
    }

    fn add_new_def(&mut self, hash: Hash, name: String, args: Option<Vec<String>>) {
        self.model.maps.view.insert(hash);
        self.model.maps.names.insert(name.clone(), hash);
        self.model.maps.namespace.insert(hash, (name, args));
        self.get_def(&hash);
    }

    fn add_def(&mut self, name: String, args: Option<Vec<String>>, hash: Hash, line: i32) {
        let prev = self.model.maps.names.get(&name).map(|x| *x);
        match prev {
            Some(h) => {
                if h != hash {
                    let len = self.model.maps.len.get(&h).unwrap_or(&1) + 1;
                    self.model.maps.len.insert(hash, len);
                    self.model.maps.chain.insert(hash, h);
                    let (mark, mcomp) = self.model.maps.marks.remove(&h).unwrap();
                    let mcomp2 = mcomp.or_else(|| {
                        let mut mark_iter = self.model.tb.get_iter_at_mark(&mark);
                        let anchor = self.model.tb.create_child_anchor(&mut mark_iter).unwrap();
                        self.model.tb.insert(&mut mark_iter, "\n");
                        let start_iter = self.model.tb.get_iter_at_mark(&mark);
                        self.model.tb.apply_tag(&self.model.tag, &start_iter, &mark_iter);
                        let comp = create_component((h, len));
                        let widget: &gtk::Box = comp.widget();
                        self.view.add_child_at_anchor(widget, &anchor);
                        widget.show_all();
                        eprintln!("Got here");
                        Some(comp)
                    });
                    self.model.maps.marks.insert(hash, (mark, mcomp2));
                    self.add_new_def(hash, name, args);
                }
            },
            None => {
                let mark = self.model.tb.create_source_mark(
                    None,
                    "def",
                    &self.model.tb.get_iter_at_line(line)).unwrap();
                self.model.maps.marks.insert(hash, (mark, None));
                self.add_new_def(hash, name, args);
            }
        }
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
                self.add_def(name, None, val, line + offset - 1);
            } else if ty == self.model.ast_ptrs.ret_ty {
                let name = def.getattr("name")?.extract()?;
                let val = self.hash_val(b.getattr("value")?, &args)?;
                let lam_val = self.hash_expr(PyExpr::Lam(args.len(), val));
                let line : i32 = def.getattr("lineno")?.extract()?;
                self.add_def(name, Some(args), lam_val, line + offset - 1);
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

    // Get the ast of a definition
    fn get_def(&self, h: &Hash) {
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
        let mark = &self.model.maps.marks.get(h).unwrap().0;
        let mut start_iter = self.model.tb.get_iter_at_mark(mark);
        let mut end_iter = mark.next(Some("def")).map(|m| self.model.tb.get_iter_at_mark(&m)).unwrap_or_else(||
            self.model.tb.get_end_iter());
        self.model.tb.delete(&mut start_iter, &mut end_iter);
        self.model.tb.insert(&mut start_iter, txt.trim_start());
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
                    let tb = &self.model.tb;
                    let gil = Python::acquire_gil();
                    let py = gil.python();
                    for gstr in tb.get_text(&tb.get_start_iter(), &tb.get_end_iter(), false).into_iter() {
                        self.parse_str(py, gstr, 0).unwrap_or_else(|e| e.print(py))
                    }
                }
            },
            WinMsg::BufferUpdate => {
                self.model.last_update = Instant::now();
                timeout(self.model.relm.stream(), 600, || WinMsg::ParseBuffer)
            },
            WinMsg::GetDef(h, offset) => {
                let mut h_iter = h;
                let mut counter = offset;
                if counter > 0 {
                    while let Some(h_child) = self.model.maps.chain.get(&h_iter) {
                        h_iter = *h_child;
                        counter -= 1;
                        if counter == 0 { break }
                    }
                }
                self.get_def(&h_iter);
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
                indent_width: 4
            },
            delete_event(_, _) => (WinMsg::Quit, gtk::Inhibit(false))
        },
    }
}

fn main() {
    Win::run(()).unwrap();
}
