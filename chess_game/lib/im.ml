open GMain
(* open GdkKeysyms *)

let main () =
  let _ = GtkMain.Main.init () in

  let window =
    GWindow.window ~width:320 ~height:240 ~title:"Simple lablgtk program" ()
  in
  ignore (window#connect#destroy ~callback:Main.quit);

  ignore (GMisc.image ~file:"lib/pawn2.png" ~width:1 ~packing:window#add ());

  (* Display the window and enter Gtk+ main loop *)
  window#show ();
  Main.main ()

let () = main ()
