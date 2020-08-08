extern crate gtk;
extern crate gio;

use gtk::prelude::*;
use gio::prelude::*;

use gtk::*;

fn make_cell() -> Box {
  let vbox = Box::new(Orientation::Vertical, 0);

  let tb = Toolbar::new();
  let a = RadioToolButtonBuilder::new().label("A").build();
  let b = RadioToolButtonBuilder::new().label("B").build();
  tb.insert(&a, -1);
  tb.insert(&b, -1);
  b.join_group(Some(&a));
  vbox.pack_start(&tb, false, false, 0);

  let hbox = Box::new(Orientation::Horizontal, 0);
  let adj = Adjustment::new(0., 0., 1., 1., 1., 1.);
  let scale = Scale::new(Orientation::Horizontal, Some(&adj));
  scale.set_draw_value(false);
  hbox.pack_start(&scale, true, true, 0);
  let btn = Button::with_label("Branch");
  hbox.pack_end(&btn, false, false, 10);
  vbox.pack_start(&hbox, false, false, 0);

  let tv = TextView::new();
  tv.connect_preedit_changed(move |_,_| {
    println!("Clicked!");
  });
  vbox.pack_start(&tv, true, true, 0);

  return vbox;
}

// Okay, next thing we want to do is trigger an update whenever we pause

// Our convention will be that the first string in the text is the name, and
// the rest are the dependencies.

fn main() {
    let application = Application::new(
        Some("com.github.gtk-rs.examples.basic"),
        Default::default(),
    ).expect("failed to initialize GTK application");

    application.connect_activate(|app| {
        let window = ApplicationWindow::new(app);
        window.set_title("Historian");
        window.set_default_size(350, 70);
        let vbox = Box::new(Orientation::Vertical, 2);
        vbox.pack_start(&make_cell(), true, true, 0);
        vbox.pack_start(&make_cell(), true, true, 0);
        window.add(&vbox);
        window.show_all();
    });

    application.run(&[]);
}
