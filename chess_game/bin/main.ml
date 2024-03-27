open GMain

(** Initialize GTK library. *)
let locale = GtkMain.Main.init ()

(** Static variables for the GUI Window width and height. *)
let width = 600

let height = 600

(**[piece_square r c] is the type of piece at row [r] and column [c] at the
   beginning of the game*)
let piece_square (row : int) (col : int) =
  match (row, col) with
  | 1, _ -> "B_Pawn"
  | 0, 0 | 0, 7 -> "B_Rook"
  | 0, 1 | 0, 6 -> "B_Knight"
  | 0, 2 | 0, 5 -> "B_Bishop"
  | 0, 3 -> "B_Queen"
  | 0, 4 -> "B_King"
  | 6, _ -> "W_Pawn"
  | 7, 0 | 7, 7 -> "W_Rook"
  | 7, 1 | 7, 6 -> "W_Knight"
  | 7, 2 | 7, 5 -> "W_Bishop"
  | 7, 3 -> "W_Queen"
  | 7, 4 -> "W_King"
  | _ -> ""

(**[set_square_img r c] generates the image to be shown at row [r] and column
   [c] as specified by [piece_square]*)
let set_square_img row col =
  let dim = 65 in
  let img = GdkPixbuf.create ~width:dim ~height:dim ~has_alpha:true () in
  GdkPixbuf.scale ~dest:img ~width:dim ~height:dim
    (GdkPixbuf.from_file ("assets/" ^ piece_square row col ^ ".png"));
  img

(**[create_chessboard_window] creates a window with a standard chess board setup*)
let create_chessboard_window () =
  let window = GWindow.window ~width ~height ~title:"Board" () in
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
      ignore
        (GMisc.image ~pixbuf:(set_square_img row col) ~packing:button#set_image
           ());
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

(**[create_homescreen_window] creates the inital wondow for game mode selection*)
let create_homescreen_window () =
  let window = GWindow.window ~width ~height ~title:"Home Screen" () in
  ignore (window#connect#destroy ~callback:Main.quit);

  let vbox = GPack.vbox ~packing:window#add () in

  let two_player_button = GButton.button ~packing:vbox#pack () in
  two_player_button#misc#set_size_request ~width:100 ~height:100 ();
  two_player_button#misc#modify_bg [ (`NORMAL, `NAME "gray") ];
  two_player_button#set_border_width 10;
  ignore
    (let lbl = GMisc.label ~packing:two_player_button#set_image () in
     lbl#set_text "Two Player";
     lbl#set_justify `CENTER);
  ignore
    (two_player_button#connect#clicked ~callback:(fun () ->
         window#destroy ();
         create_chessboard_window ();
         Main.main ()));

  window#show ();
  ()

let main () =
  create_homescreen_window ();
  Main.main ()

let () = main ()
