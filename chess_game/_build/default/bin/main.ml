open GMain
(* open GdkKeysyms *)

let piece_square (row : int) (col : int) =
  match (row, col) with
  | 1, _ -> "W_Pawn"
  | 0, 0 | 0, 7 -> "W_Rook"
  | 0, 1 | 0, 6 -> "W_Knight"
  | 0, 2 | 0, 5 -> "W_Bishop"
  | 0, 3 -> "W_Queen"
  | 0, 4 -> "W_King"
  | 6, _ -> "B_Pawn"
  | 7, 0 | 7, 7 -> "B_Rook"
  | 7, 1 | 7, 6 -> "B_Knight"
  | 7, 2 | 7, 5 -> "B_Bishop"
  | 7, 3 -> "B_Queen"
  | 7, 4 -> "B_King"
  | _ -> ""

let set_square_img row col =
  let pixbuf =
    GdkPixbuf.from_file ("assets/" ^ piece_square row col ^ ".png")
  in
  let dim = 65 in
  let pixbuf' = GdkPixbuf.create ~width:dim ~height:dim ~has_alpha:true () in
  GdkPixbuf.scale ~dest:pixbuf' ~width:dim ~height:dim pixbuf;
  pixbuf'

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
    if piece_square row col != "" then
      GMisc.image ~pixbuf:(set_square_img row col) ~packing:button#set_image ()
      |> ignore;
    let color =
      if (row + col) mod 2 = 0 then `NAME "white" else `NAME "green"
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
