extern crate gdk;
extern crate gtk;
extern crate gio;
extern crate sourceview;

use gtk::prelude::*;
use gio::prelude::*;
use gtk::*;
use crate::sourceview::LanguageManagerExt;
use crate::sourceview::ViewExt;

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

    application.connect_activate(|app| {
        let window = ApplicationWindow::new(app);
        window.set_title("Historian");
        window.set_default_size(350, 70);

        let vbox = Box::new(Orientation::Vertical, 0);

        let hbox = Box::new(Orientation::Horizontal, 0);
        let r1 = RadioButtonBuilder::new().draw_indicator(false).label("A").build();
        let r2 = RadioButtonBuilder::new().draw_indicator(false).label("B").build();
        r2.join_group(Some(&r1));
        hbox.pack_start(&r1, false, false, 10);
        hbox.pack_start(&r2, false, false, 10);

        let hbox2 = Box::new(Orientation::Horizontal, 0);
        let adj = Adjustment::new(0., 0., 1., 1., 1., 1.);
        let scale = Scale::new(Orientation::Horizontal, Some(&adj));
        scale.set_draw_value(false);
        hbox2.pack_start(&scale, true, true, 0);
        let btn = Button::with_label("Branch");
        hbox2.pack_end(&btn, false, false, 2);

        vbox.pack_start(&hbox, false, false, 0);
        vbox.pack_start(&hbox2, false, false, 2);

        let lm = sourceview::LanguageManager::get_default().unwrap();
        let python = lm.get_language("python").unwrap();
        let tb = sourceview::Buffer::new_with_language(&python);
        let tt = tb.get_tag_table().unwrap();

        let tag = TextTag::new(None);
        tag.set_property_editable(false);
        tt.add(&tag);
        let mut iter = tb.get_iter_at_line(0);
        tb.insert(&mut iter, "\n");
        let anchor = tb.create_child_anchor(&mut iter).unwrap();
        tb.insert(&mut iter, "\n");
        tb.apply_tag(&tag, &tb.get_iter_at_line(0), &iter);
        let tv = sourceview::View::new_with_buffer(&tb);
        tv.set_auto_indent(true);
        tv.set_indent_on_tab(true);
        tv.set_smart_backspace(true);
        tv.set_indent_width(2);
        tv.add_child_at_anchor(&vbox, &anchor);
        vbox.set_size_request(500, -1);
        window.add(&tv);
        window.show_all();
    });

    application.run(&[]);
}
