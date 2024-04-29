open GMain
open Chess_game

(** Initialize GTK library. *)
let _ = GtkMain.Main.init ()

(** Static variables for the GUI Window width and height. *)
let width = 600

let height = 600

let state =
  [|
    [|
      Some "B_Rook"; Some "B_Knight"; Some "B_Bishop"; Some "B_Queen";
      Some "B_King"; Some "B_Bishop"; Some "B_Knight"; Some "B_Rook";
    |];
    [|
      Some "B_Pawn"; Some "B_Pawn"; Some "B_Pawn"; Some "B_Pawn"; Some "B_Pawn";
      Some "B_Pawn"; Some "B_Pawn"; Some "B_Pawn";
    |]; [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
    [|
      Some "W_Pawn"; Some "W_Pawn"; Some "W_Pawn"; Some "W_Pawn"; Some "W_Pawn";
      Some "W_Pawn"; Some "W_Pawn"; Some "W_Pawn";
    |];
    [|
      Some "W_Rook"; Some "W_Knight"; Some "W_Bishop"; Some "W_Queen";
      Some "W_King"; Some "W_Bishop"; Some "W_Knight"; Some "W_Rook";
    |];
  |]

let buttons = Array.make_matrix 8 8 (GButton.button ~label:"" ())

type f = {
  mutable row : int;
  mutable col : int;
}

let prev = { row = 4; col = 4 }

(**[piece_square r c] is the type of piece at row [r] and column [c] at the
   beginning of the game*)
let piece_square (row : int) (col : int) =
  match state.(row).(col) with
  | Some piece -> piece
  | None -> ""

(**[set_square_img r c] generates the image to be shown at row [r] and column
   [c] as specified by [piece_square]*)
let set_square_img row col =
  let dim = 65 in
  let img = GdkPixbuf.create ~width:dim ~height:dim ~has_alpha:true () in
  GdkPixbuf.scale ~dest:img ~width:dim ~height:dim
    (GdkPixbuf.from_file ("assets/" ^ piece_square row col ^ ".png"));
  img

let do_castle button c ke rs re =
  (* color, king end, rook start, rook end*)
  ignore
    (GMisc.image
       ~pixbuf:(set_square_img prev.row prev.col)
       ~packing:button#set_image ());
  ignore (GMisc.image ~packing:buttons.(prev.row).(prev.col)#set_image ());
  ignore
    (GMisc.image ~pixbuf:(set_square_img c rs)
       ~packing:buttons.(c).(re)#set_image ());
  ignore (GMisc.image ~packing:buttons.(c).(rs)#set_image ());
  state.(c).(ke) <- state.(c).(4);
  state.(c).(4) <- None;
  state.(c).(re) <- state.(c).(rs);
  state.(c).(rs) <- None

let rec update_tab (table : GPack.table) lst pos =
  match lst with
  | [] -> ()
  | h :: t ->
      let dim = 30 in
      let img = GdkPixbuf.create ~width:dim ~height:dim ~has_alpha:true () in
      GdkPixbuf.scale ~dest:img ~width:dim ~height:dim
        (GdkPixbuf.from_file ("assets/" ^ h ^ ".png"));
      table#attach ~left:pos ~top:0 ~expand:`BOTH ~fill:`BOTH
        (GMisc.image ~width:10 ~height:100 ~pixbuf:img ())#coerce;
      update_tab table t (pos + 1)

(* let turn () = !Movement.turn *)

(* let text_box () = let score = "+" ^ (Movement.material_advantage () |> snd |>
   string_of_int) in let buff = GText.buffer ~text:score () in GText.view
   ~buffer:buff () *)

(**[create_chessboard_window] creates a window with a standard chess board setup*)
let create_chessboard_window () =
  let window = GWindow.window ~width ~height ~title:"Board" () in
  ignore (window#connect#destroy ~callback:Main.quit);

  (* Vertical box for the Quit button + chessboard *)
  let vbox = GPack.vbox ~width ~height ~packing:window#add () in

  (* Quit button *)
  let quit_button = GButton.button ~label:"Quit" ~packing:vbox#pack () in
  ignore (quit_button#connect#clicked ~callback:Main.quit);

  (* Table for chessbaord *)
  let tableB =
    GPack.table ~rows:1 ~columns:15 ~homogeneous:true ~packing:vbox#add ()
  in
  tableB#attach ~left:0 ~top:0 ~expand:`BOTH ~fill:`BOTH
    (GMisc.image ~width:10 ~height:100 ())#coerce;

  let table =
    GPack.table ~rows:8 ~columns:8 ~homogeneous:true ~packing:vbox#add ()
  in

  let tableW =
    GPack.table ~rows:1 ~columns:15 ~homogeneous:true ~packing:vbox#add ()
  in
  tableW#attach ~left:0 ~top:0 ~expand:`BOTH ~fill:`BOTH
    (GMisc.image ~width:10 ~height:100 ())#coerce;

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
    ignore
      (button#connect#clicked ~callback:(fun () ->
           (* If prev square is a piece *)
           if
             piece_square prev.row prev.col <> ""
             && Movement.is_valid_move
                  (piece_square prev.row prev.col)
                  prev.row prev.col row col state
           then (
             (*Update captures, if necessary, and update the table*)
             Movement.update_captures row col state;
             update_tab tableB !Movement.captured_W 0;
             (* tableB#attach ~left:(List.length !Movement.captured_W) ~top:0
                (text_box ())#coerce; *)
             update_tab tableW !Movement.captured_B 0;
             (* Then set new piece at new square *)
             ignore
               (GMisc.image
                  ~pixbuf:(set_square_img prev.row prev.col)
                  ~packing:button#set_image ());
             (* Change prev piece to blank *)
             ignore
               (GMisc.image ~packing:buttons.(prev.row).(prev.col)#set_image ());
             (* move prev piece to new location *)
             state.(row).(col) <- state.(prev.row).(prev.col);
             state.(prev.row).(prev.col) <- None)
           else if
             (piece_square prev.row prev.col = "B_King"
             || piece_square prev.row prev.col = "W_King")
             && Movement.can_castle prev.row prev.col row col state
           then
             match Movement.castle prev.row prev.col row col with
             | "wksc" -> do_castle button 7 6 7 5
             | "wqsc" -> do_castle button 7 2 0 3
             | "bksc" -> do_castle button 0 6 7 5
             | "bqsc" -> do_castle button 0 2 0 3
             | _ -> failwith "Should never reach this case"
           else (
             prev.row <- row;
             prev.col <- col)));

    button
  in
  for row = 0 to 7 do
    for col = 0 to 7 do
      let square = create_square row col in
      table#attach ~left:col ~top:row ~expand:`BOTH ~fill:`BOTH square#coerce;
      buttons.(row).(col) <- square
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
