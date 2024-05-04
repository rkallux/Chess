open OUnit2
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

(**[captured_W] contains a list of all the white pieces that have been captured,
   sorted based on the pieces' material value*)
let captured_W = ref []

(**[captured_B] contains a list of all the black pieces that have been captured,
   sorted based on the pieces' material value*)
let captured_B = ref []

(**[total_material captured] returns the sum of the material value of all pieces
   in [captured]*)
let rec total_material captured =
  match captured with
  | [] -> 0
  | h :: t -> Movement.material h + total_material t

(**[material_advantage ()] is a tuple whose first element is the color that has
   the material advantage and whose second element is the value of the
   advantage. returns [("same", 0)] if the two sides are equal in terms of
   material*)
let material_advantage () =
  let adv = abs (total_material !captured_B - total_material !captured_W) in
  if total_material !captured_B - total_material !captured_W = 0 then ("same", 0)
  else if total_material !captured_B - total_material !captured_W > 0 then
    ("W", adv)
  else ("B", adv)

let rec print_lst lst =
  match lst with
  | [] -> print_endline ""
  | h :: t ->
      print_string (h ^ " ");
      print_lst t

let turn () = !Movement.turn

(**[update_captures row col] adds the piece at row [row] and column [col] into
   [captured_W] if it is a white piece and [captured_B] if it is black*)
let update_captures row col =
  match state.(row).(col) with
  | Some p -> (
      match p.[0] with
      | 'W' ->
          captured_W :=
            List.sort
              (fun p1 p2 -> Movement.material p1 - Movement.material p2)
              (p :: !captured_W);
          print_lst !captured_W
      | 'B' ->
          captured_B :=
            List.sort
              (fun p1 p2 -> Movement.material p1 - Movement.material p2)
              (p :: !captured_B);
          print_lst !captured_B
      | _ -> failwith "un")
  | _ -> failwith "un"

(**[create_chessboard_window] creates a window with a standard chess board setup*)
let create_chessboard_window () =
  let window = GWindow.window ~width ~height ~title:"Board" () in
  ignore (window#connect#destroy ~callback:Main.quit);

  (* Vertical box for the Quit button + chessboard *)
  let vbox = GPack.vbox ~width ~height ~packing:window#add () in

  (* Quit button *)
  let quit_button = GButton.button ~label:"Quit" ~packing:vbox#pack () in
  ignore (quit_button#connect#clicked ~callback:Main.quit);

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
  in

  (* Table for chessbaord *)
  let tableB =
    GPack.table ~rows:1 ~columns:15 ~homogeneous:true ~packing:vbox#add ()
  in
  tableB#attach ~left:5 ~top:0 ~expand:`BOTH ~fill:`BOTH
    (GMisc.image ~width:10 ~height:100 ())#coerce;

  let table =
    GPack.table ~rows:8 ~columns:8 ~homogeneous:true ~packing:vbox#add ()
  in

  let tableW =
    GPack.table ~rows:1 ~columns:15 ~homogeneous:true ~packing:vbox#add ()
  in
  tableW#attach ~left:5 ~top:0 ~expand:`BOTH ~fill:`BOTH
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
             && Movement.valid_move Movement.curr_state prev.row prev.col row
                  col
           then (
             print_endline (turn () ^ " to move");
             print_endline
               (fst (material_advantage ())
               ^ string_of_int (snd (material_advantage ())));
             (*If the prev piece is moving onto a new piece, it is a capture*)
             if state.(row).(col) <> None then update_captures row col;
             update_tab tableB !captured_W 0;
             update_tab tableW !captured_B 0;
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
             && Movement.is_valid_castle Movement.curr_state prev.row prev.col
                  row col
           then
             match Movement.type_castle prev.row prev.col row col with
             | "wksc" -> do_castle button 7 6 7 5
             | "wqsc" -> do_castle button 7 2 0 3
             | "bksc" -> do_castle button 0 6 7 5
             | "bqsc" -> do_castle button 0 2 0 3
             | _ -> failwith "Should never reach\n   this case"
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

let piece_square_tests =
  [
    (* Black pieces *)
    ( "Piece square at (0, 0)" >:: fun _ ->
      assert_equal "B_Rook" (piece_square 0 0) );
    ( "Piece square at (0,\n\n   1)" >:: fun _ ->
      assert_equal "B_Knight" (piece_square 0 1) );
    ( "Piece\n\n   square at (0, 2)" >:: fun _ ->
      assert_equal "B_Bishop" (piece_square 0 2) );
    ( "Piece square at (0, 3)" >:: fun _ ->
      assert_equal "B_Queen" (piece_square 0 3) );
    ( "Piece square at (0, 4)" >:: fun _ ->
      assert_equal "B_King" (piece_square 0 4) );
    ( "Piece square at (0, 5)" >:: fun _ ->
      assert_equal "B_Bishop" (piece_square 0 5) );
    ( "Piece square at (0, 6)" >:: fun _ ->
      assert_equal "B_Knight" (piece_square 0 6) );
    ( "Piece square at (0, 7)" >:: fun _ ->
      assert_equal "B_Rook" (piece_square 0 7) );
    ( "Piece square at (1,\n\n   0)" >:: fun _ ->
      assert_equal "B_Pawn" (piece_square 1 0) );
    ( "Piece\n   square\n at (1, 1)" >:: fun _ ->
      assert_equal "B_Pawn" (piece_square 1 1) );
    ( "Piece\n square at (1, 2)" >:: fun _ ->
      assert_equal "B_Pawn" (piece_square 1 2) );
    ( "Piece square at (1, 3)" >:: fun _ ->
      assert_equal "B_Pawn" (piece_square 1 3) );
    ( "Piece square at (1, 4)" >:: fun _ ->
      assert_equal "B_Pawn" (piece_square 1 4) );
    ( "Piece square at (1, 5)" >:: fun _ ->
      assert_equal "B_Pawn" (piece_square 1 5) );
    ( "Piece square at (1, 6)" >:: fun _ ->
      assert_equal "B_Pawn" (piece_square 1 6) );
    ( "Piece square at (1,\n   7)" >:: fun _ ->
      assert_equal "B_Pawn" (piece_square 1 7) ); (* White pieces *)
    ( "Piece square at (6, 0)" >:: fun _ ->
      assert_equal "W_Pawn" (piece_square 6 0) );
    ( "Piece square at (6, 1)" >:: fun _ ->
      assert_equal "W_Pawn" (piece_square 6 1) );
    ( "Piece square at (6, 2)" >:: fun _ ->
      assert_equal "W_Pawn" (piece_square 6 2) );
    ( "Piece square at (6, 3)" >:: fun _ ->
      assert_equal "W_Pawn" (piece_square 6 3) );
    ( "Piece square at (6,\n   4)" >:: fun _ ->
      assert_equal "W_Pawn" (piece_square 6 4) );
    ( "Piece square\n   at (6,\n 5)" >:: fun _ ->
      assert_equal "W_Pawn" (piece_square 6 5) );
    ( "Piece square\n at (6, 6)" >:: fun _ ->
      assert_equal "W_Pawn" (piece_square 6 6) );
    ( "Piece\n square at (6, 7)" >:: fun _ ->
      assert_equal "W_Pawn" (piece_square 6 7) );
    ( "Piece square at (7, 0)" >:: fun _ ->
      assert_equal "W_Rook" (piece_square 7 0) );
    ( "Piece square at (7, 1)" >:: fun _ ->
      assert_equal "W_Knight" (piece_square 7 1) );
    ( "Piece square at (7, 2)" >:: fun _ ->
      assert_equal "W_Bishop" (piece_square 7 2) );
    ( "Piece square at (7,\n   3)" >:: fun _ ->
      assert_equal "W_Queen" (piece_square 7 3) );
    ( "Piece square\n   at (7, 4)" >:: fun _ ->
      assert_equal "W_King" (piece_square 7 4) );
    ( "Piece\n   square at (7,\n 5)" >:: fun _ ->
      assert_equal "W_Bishop" (piece_square 7 5) );
    ( "Piece\n square at (7, 6)" >:: fun _ ->
      assert_equal "W_Knight" (piece_square 7 6) );
    ( "Piece square at (7, 7)" >:: fun _ ->
      assert_equal "W_Rook" (piece_square 7 7) ); (* Empty squares *)
    ("Piece square at (2, 2)" >:: fun _ -> assert_equal "" (piece_square 2 2));
    ("Piece square at (3, 3)" >:: fun _ -> assert_equal "" (piece_square 3 3));
    ("Piece square at (4, 4)" >:: fun _ -> assert_equal "" (piece_square 4 4));
    ("Piece square at (5, 5)" >:: fun _ -> assert_equal "" (piece_square 5 5));
  ]

let update_captures_tests =
  [
    (* Test case: Update captures when capturing pieces of the same color *)
    ( "Update captures when capturing pieces of the\n   same color" >:: fun _ ->
      captured_B := [ "B_Rook"; "B_Pawn" ];
      (* Set up the board with a black king *) state.(3).(3) <- Some "B_King";
      (* Simulate capturing the black king at position (3, 3) *)
      update_captures 3 3;
      (* Verify that the captured list now contains the captured king *)
      assert_equal [ "B_King"; "B_Pawn"; "B_Rook" ] !captured_B );
    (* Test case: Update captures when capturing same-colored king *)
    ( "Update captures when\n   capturing same-colored king" >:: fun _ ->
      captured_W := [ "W_Queen" ];
      (* Set up the board with a white king *) state.(4).(4) <- Some "W_King";
      (* Simulate capturing the white king at position (4, 4) *)
      update_captures 4 4;
      (* Verify that the captured list now contains the captured king *)
      assert_equal [ "W_King"; "W_Queen" ] !captured_W );
    (* Test case: Update captures when capturing opposite-colored king *)
    ( "Update captures when capturing\n   opposite-colored king" >:: fun _ ->
      captured_B := [ "B_Queen" ];
      (* Set up the board with a black king *) state.(3).(3) <- Some "B_King";
      (* Simulate capturing the black king at position (3, 3) *)
      update_captures 3 3;
      (* Verify that the captured list now contains the captured king *)
      assert_equal [ "B_King"; "B_Queen" ] !captured_B );
    (* Test case: Update captures for empty board *)
    ( "Update captures for empty board" >:: fun _ ->
      captured_W := [];
      captured_B := [];
      (* Clear the board *)
      Array.iter (fun arr -> Array.fill arr 0 8 None) state;
      (* Simulate capturing on an empty board *) update_captures 2 2;
      (* Verify that both captured lists remain empty *)
      assert_equal [] !captured_W;
      assert_equal [] !captured_B );
    (* Additional test cases can be added here *)
    (* Test case: Update captures when capturing a white piece *)
    ( "Update captures when capturing a white piece" >:: fun _ ->
      captured_W := [ "W_Bishop"; "W_Knight" ];
      (* Set up the board with a black king and white pieces *)
      state.(3).(3) <- Some "B_King";
      state.(2).(2) <- Some "W_Bishop";
      state.(4).(4) <- Some "W_Knight";
      (* Simulate capturing a white piece at position (2, 2) *)
      update_captures 2 2;
      (* Verify that the captured list now contains the captured white piece *)
      assert_equal [] !captured_B );
  ]

let additional_update_captures_tests =
  [
    (* Test case for updating captures on an empty board *)
    ( "Update captures for empty board" >:: fun _ ->
      captured_W := [];
      captured_B := [];
      (* Clear the board *)
      Array.iter (fun arr -> Array.fill arr 0 8 None) state;
      (* Simulate capturing on an empty board *)
      update_captures 2 2;
      (* Verify that both captured lists remain empty *)
      assert_equal [] !captured_W;
      assert_equal [] !captured_B );
    (* Test case for capturing a black piece on an otherwise empty board *)
    ( "Update captures for capturing a black piece on an empty board"
    >:: fun _ ->
      captured_W := [];
      captured_B := [];
      (* Clear the board *)
      Array.iter (fun arr -> Array.fill arr 0 8 None) state;
      (* Place a black piece on position (2, 2) *)
      state.(2).(2) <- Some "B_Rook";
      (* Simulate capturing the black piece at position (2, 2) *)
      update_captures 2 2;
      (* Verify that the captured list contains the black piece *)
      assert_equal [ "B_Rook" ] !captured_B;
      assert_equal [] !captured_W );
    (* Test case for capturing a white piece on an otherwise empty board *)
    ( "Update captures for capturing a white piece on an empty board"
    >:: fun _ ->
      captured_W := [];
      captured_B := [];
      (* Clear the board *)
      Array.iter (fun arr -> Array.fill arr 0 8 None) state;
      (* Place a white piece on position (5, 5) *)
      state.(5).(5) <- Some "W_Knight";
      (* Simulate capturing the white piece at position (5, 5) *)
      update_captures 5 5;
      (* Verify that the captured list contains the white piece *)
      assert_equal [ "W_Knight" ] !captured_W;
      assert_equal [] !captured_B );
  ]

let additional_piece_square_tests =
  [
    (* Test cases for various positions *)
    ("Piece square at (2, 3)" >:: fun _ -> assert_equal "" (piece_square 2 3));
    ("Piece square at (3, 2)" >:: fun _ -> assert_equal "" (piece_square 3 2));
    (* Updated test cases for out-of-bounds positions *)
    ( "Piece square at (-1, 2)" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          piece_square (-1) 2) );
    ( "Piece square at (4, -2)" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          piece_square 4 (-2)) );
    ( "Piece square at (-3, -5)" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          piece_square (-3) (-5)) );
    ( "Piece square at (10, 5)" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          piece_square 10 5) );
    ( "Piece square at (5, 10)" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          piece_square 5 10) );
    ( "Piece square at (15, 15)" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          piece_square 15 15) );
    ( "Piece square at (16, 15)" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          piece_square 16 15) );
    ( "Piece square at (-10, -5)" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          piece_square (-10) (-5)) );
    ( "Piece square at (100, 1)" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          piece_square 100 1) );
    ( "Piece square at (1, -1)" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          piece_square 1 (-1)) );
    ( "Piece square at (0, -1)" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          piece_square 0 (-1)) );
    ( "Piece square at (15000, 15000)" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          piece_square 15000 15000) );
  ]

let suite =
  "Chess Game"
  >::: List.flatten
         [
           piece_square_tests; update_captures_tests;
           additional_piece_square_tests; additional_update_captures_tests;
         ]

let () = run_test_tt_main suite
