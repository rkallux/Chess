open GMain
(* open GdkKeysyms *)

let create_chessboard_window () =
  let window = GWindow.window ~width:600 ~height:600 ~title:"Board" () in
  ignore (window#connect#destroy ~callback:Main.quit);

  (* Vertical box for the Quit button + chessboard *)
  let vbox = GPack.vbox ~packing:window#add () in

  (* Quit button *)
  let quit_button = GButton.button ~label:"Quit" ~packing:vbox#pack () in
  ignore (quit_button#connect#clicked ~callback:Main.quit);

  (* Table for chessbaord *)
  let table =
    GPack.table ~rows:8 ~columns:8 ~homogeneous:true ~packing:vbox#add ()
  in

  (* Function to create a square *)
  let create_square row col =
    let button = GButton.button ~label:"" () in
    let color =
      if (row + col) mod 2 = 0 then `NAME "white" else `NAME "black"
    in
    button#misc#modify_bg [ (`NORMAL, color) ];
    ignore (* for now, clicking a square just prints its coordinates *)
      (button#connect#clicked ~callback:(fun () ->
           Printf.printf "Square clicked: %d, %d\n" row col;
           flush stdout));
    button
  in

  for row = 0 to 7 do
    for col = 0 to 7 do
      let square = create_square row col in
      table#attach ~left:col ~top:row ~expand:`BOTH ~fill:`BOTH square#coerce
    done
  done;

  window#show ();
  ()

let main () =
  let _ = GtkMain.Main.init () in
  create_chessboard_window ();
  Main.main ()

let () = main ()
