open GMain
open GdkKeysyms

let main () =
  let _ = GtkMain.Main.init () in

  let window =
    GWindow.window ~width:320 ~height:240 ~title:"Simple lablgtk program" ()
  in
  let vbox = GPack.vbox ~packing:window#add () in
  ignore (window#connect#destroy ~callback:Main.quit);

  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in

  ignore (factory#add_item "Quit" ~key:_Q ~callback:Main.quit);

  let button = GButton.button ~label:"Push me!" ~packing:vbox#add () in
  ignore (button#connect#clicked ~callback:(fun () -> prerr_endline "Ouch!"));

  (* Display the window and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()
