extern crate gdk;
extern crate gtk;
extern crate gio;
extern crate glib;
extern crate sourceview;

use gtk::prelude::*;
use gio::prelude::*;
use gtk::*;
use sourceview::{LanguageManagerExt, ViewExt};
use glib::GString;

use std::cell;
use std::thread;
use std::time::Duration;
use std::sync::mpsc;
use std::sync::{Mutex, Arc};
use std::sync::atomic::{Ordering, AtomicBool};
use python_parser::ast;
use std::collections::HashMap;

// Note: this will only work with a pure subset of python

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

fn process_ast(symbols: &mut HashMap<&str, ast::Statement>, statemenr: ast::Statement) {
}

struct Work {
    do_work: AtomicBool,
    content: Mutex<Option<GString>>
}

fn spawn_parser(receiver: mpsc::Receiver<GString>) {
    let work = Arc::new(Work{do_work: AtomicBool::new(false), content: Mutex::new(None)});
    let work1 = Arc::clone(&work);
    thread::spawn(move || {
        let mut m = HashMap::new();
        loop {
            {
                work1.do_work.store(work1.content.lock().unwrap().is_some(), Ordering::Relaxed);
            }
            thread::sleep(Duration::from_millis(300));
            if work1.do_work.load(Ordering::Relaxed) {
                let lock = work1.content.lock();
                let code = lock.unwrap().take().unwrap();
                let (_, ss) = python_parser::file_input(python_parser::make_strspan(code.as_str())).unwrap();
                for s in ss {
                    process_ast(&mut m, s);
                }
            }
        }
    });
    let work2 = Arc::clone(&work);
    thread::spawn(move || {
        loop {
            let a = receiver.recv().unwrap();
            {
                *(work2.content.lock().unwrap()) = Some(a);
            }
            work2.do_work.store(false, Ordering::Relaxed);

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

    spawn_parser(receiver);
    tb.connect_changed(move |tba| {
      let (start, end) = tba.get_bounds();
      let thestr = tba.get_text(&start, &end, true).unwrap();
      sendref.borrow_mut().send(thestr).unwrap();
    });

    let b = add_history_header(&tv);
    build_branch_toggle(&b);
    
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
