open OUnit2
open Chess_game.Movement

(* create an empty board *)
let empty_board () = Array.make_matrix 8 8 None

let update_states board r c p =
  board.(r).(c) <- p;
  curr_state.(r).(c) <- p

let test_piece_at_piece _ =
  let board = empty_board () in
  board.(6).(0) <- Some "W_Pawn";
  assert_equal "W_Pawn" (piece_at board 6 0)

let test_piece_at_no_piece _ =
  let board = empty_board () in
  board.(6).(0) <- Some "W_Pawn";
  assert_equal "" (piece_at board 7 0)

let test_same_color_same _ =
  let board = empty_board () in
  board.(0).(0) <- Some "W_King";
  board.(0).(1) <- Some "W_Pawn";
  assert_equal true (same_color board "W_King" 0 1)

let test_same_color_different _ =
  let board = empty_board () in
  board.(0).(0) <- Some "W_King";
  board.(0).(1) <- Some "B_Pawn";
  assert_equal false (same_color board "W_King" 0 1)

let test_same_color_no_piece _ =
  let board = empty_board () in
  board.(0).(0) <- Some "W_King";
  assert_equal false (same_color board "W_King" 0 1)

let test_update_turn_white_to_black _ =
  turn := "W";
  update_turn ();
  assert_equal "B" !turn

let test_update_turn_black_to_white _ =
  turn := "B";
  update_turn ();
  assert_equal "W" !turn

let test_update_turn_invalid _ =
  turn := "Invalid";
  assert_raises (Failure "") update_turn

let test_enpassant_capture _ =
  make_currstate_empty ();
  let board = empty_board () in
  (* Set up the board state with pawns and simulate the last move *)
  update_states board 6 4 (Some "W_Pawn");
  update_states board 1 3 (Some "B_Pawn");
  update_currstate 6 4 4 4;
  update_state board 6 4 4 4;
  update_currstate 4 4 3 4;
  update_state board 4 4 3 4;
  update_currstate 1 3 3 3;
  update_state board 1 3 3 3;
  (* Set up the last move *)
  (* Test en passant capture scenario *)
  assert_bool "En passant capture should be detected"
    (is_enpassant board 3 4 2 3)

let test_no_enpassant_capture _ =
  make_currstate_empty ();
  let board = empty_board () in
  (* Set up the board state with pawns and simulate the last move *)
  update_states board 6 4 (Some "W_Pawn");
  update_states board 1 3 (Some "B_Pawn");
  update_currstate 6 4 4 4;
  update_state board 6 4 4 4;
  update_currstate 4 4 3 4;
  update_state board 4 4 3 4;
  update_currstate 1 3 2 3;
  update_state board 1 3 2 3;
  update_currstate 2 3 3 3;
  update_state board 2 3 3 3;
  (* Set up the last move *)
  (* Test scenario where there is no en passant capture *)
  assert_bool "No en passant capture should be detected"
    (not (is_enpassant board 3 4 2 3))

let test_bishop_valid_moves_se _ =
  let board = empty_board () in
  board.(4).(4) <- Some "B_Bishop";
  let moves = [ (5, 5); (6, 6); (3, 3) (* Southeast *) ] in
  List.iter
    (fun (er, ec) ->
      assert_bool "Valid move should be true"
        (is_valid_bishop_move board 4 4 er ec))
    moves

let test_bishop_valid_moves_sw _ =
  let board = empty_board () in
  board.(4).(4) <- Some "B_Bishop";
  let moves = [ (5, 3); (6, 2); (3, 5) (* Southwest *) ] in
  List.iter
    (fun (er, ec) ->
      assert_bool "Valid move should be true"
        (is_valid_bishop_move board 4 4 er ec))
    moves

let test_bishop_valid_moves_nw _ =
  let board = empty_board () in
  board.(4).(4) <- Some "B_Bishop";
  let moves = [ (2, 2); (1, 1) (* Northwest *) ] in
  List.iter
    (fun (er, ec) ->
      assert_bool "Valid move should be true"
        (is_valid_bishop_move board 4 4 er ec))
    moves

let test_bishop_valid_moves_ne _ =
  let board = empty_board () in
  board.(4).(4) <- Some "B_Bishop";
  let moves = [ (2, 6); (1, 7) (* Northeast *) ] in
  List.iter
    (fun (er, ec) ->
      assert_bool "Valid move should be true"
        (is_valid_bishop_move board 4 4 er ec))
    moves

let test_bishop_valid_moves _ =
  test_bishop_valid_moves_se ();
  test_bishop_valid_moves_sw ();
  test_bishop_valid_moves_nw ();
  test_bishop_valid_moves_ne ()

let test_bishop_blocked_moves _ =
  let board = empty_board () in
  board.(4).(4) <- Some "B_Bishop";
  board.(5).(5) <- Some "W_Pawn";
  assert_bool "Move should be blocked by piece"
    (not (is_valid_bishop_move board 4 4 6 6))

let test_bishop_invalid_moves _ =
  let board = empty_board () in
  board.(4).(4) <- Some "B_Bishop";
  assert_bool "Non-diagonal move should be false"
    (not (is_valid_bishop_move board 4 4 4 5))

let test_rook_valid_horizontal_moves _ =
  let board = empty_board () in
  board.(4).(4) <- Some "W_Rook";
  board.(4).(7) <- Some "B_Pawn";
  assert_bool "Rook should move right" (is_valid_rook_move board 4 4 4 6);
  assert_bool "Rook should capture on move right"
    (is_valid_rook_move board 4 4 4 7)

let test_rook_valid_vertical_moves _ =
  let board = empty_board () in
  board.(4).(4) <- Some "W_Rook";
  board.(1).(4) <- Some "B_Pawn";
  assert_bool "Rook should move up" (is_valid_rook_move board 4 4 2 4);
  assert_bool "Rook should capture on move up"
    (is_valid_rook_move board 4 4 1 4)

let test_rook_blocked_horizontal _ =
  let board = empty_board () in
  board.(4).(4) <- Some "W_Rook";
  board.(4).(5) <- Some "W_Pawn";
  assert_bool "Rook move should be blocked"
    (not (is_valid_rook_move board 4 4 4 6))

let test_rook_blocked_vertical _ =
  let board = empty_board () in
  board.(4).(4) <- Some "W_Rook";
  board.(3).(4) <- Some "W_Pawn";
  assert_bool "Rook move should be blocked"
    (not (is_valid_rook_move board 4 4 1 4))

let test_rook_invalid_diagonal _ =
  let board = empty_board () in
  board.(4).(4) <- Some "W_Rook";
  assert_bool "Rooks cannot move diagonally"
    (not (is_valid_rook_move board 4 4 5 5))

let test_knight_valid_moves _ =
  let start_row = 4 and start_col = 4 in
  let valid_moves =
    [
      (start_row + 2, start_col + 1); (start_row + 2, start_col - 1);
      (start_row - 2, start_col + 1); (start_row - 2, start_col - 1);
      (start_row + 1, start_col + 2); (start_row + 1, start_col - 2);
      (start_row - 1, start_col + 2); (start_row - 1, start_col - 2);
    ]
  in
  List.iter
    (fun (er, ec) ->
      assert_bool "Valid knight move"
        (is_valid_knight_move start_row start_col er ec))
    valid_moves

let test_knight_invalid_moves _ =
  let start_row = 4 and start_col = 4 in
  let invalid_moves =
    [
      (start_row, start_col + 1); (* Horizontal *) (start_row + 1, start_col);
      (* Vertical *) (start_row + 3, start_col + 3); (* Diagonal *)
      (start_row, start_col) (* No *);
    ]
  in
  List.iter
    (fun (er, ec) ->
      assert_bool "Invalid knight move"
        (not (is_valid_knight_move start_row start_col er ec)))
    invalid_moves

let test_knight_edge_moves _ =
  let start_positions = [ (0, 1); (1, 0); (7, 6); (6, 7) ] in
  let expected_valid_moves =
    [
      [ (2, 2); (2, 0) ]; (* From (0, 1) *) [ (3, 1); (2, 2) ];
      (* From (1, 0) *) [ (5, 7); (5, 5) ]; (* From (7, 6) *)
      [ (4, 6); (5, 5) ] (* From (6, 7) *);
    ]
  in
  List.iter2
    (fun (sr, sc) valid_moves ->
      List.iter
        (fun (er, ec) ->
          assert_bool "Valid knight edge move"
            (is_valid_knight_move sr sc er ec))
        valid_moves)
    start_positions expected_valid_moves

let test_queen_valid_horizontal_vertical_moves _ =
  let board = empty_board () in
  board.(4).(4) <- Some "W_Queen";
  let valid_moves = [ (4, 7); (4, 0); (0, 4); (7, 4) ] in
  List.iter
    (fun (er, ec) ->
      assert_bool "Queen should move correctly"
        (is_valid_queen_move board 4 4 er ec))
    valid_moves

let test_queen_valid_diagonal_moves _ =
  let board = empty_board () in
  board.(4).(4) <- Some "W_Queen";
  let valid_moves =
    [
      (1, 7); (7, 7); (* Northeast and southeast *) (7, 1); (1, 1);
      (* Southwest and northwest *)
    ]
  in
  List.iter
    (fun (er, ec) ->
      assert_bool "Queen should move diagonally"
        (is_valid_queen_move board 4 4 er ec))
    valid_moves

let test_queen_blocked_moves _ =
  let board = empty_board () in
  board.(4).(4) <- Some "W_Queen";
  board.(4).(5) <- Some "W_Pawn";
  board.(5).(5) <- Some "W_Pawn";
  let blocked_moves =
    [ (4, 7); (* Right, blocked *) (7, 7) (* Southeast, blocked *) ]
  in
  List.iter
    (fun (er, ec) ->
      assert_bool "Blocked move should fail"
        (not (is_valid_queen_move board 4 4 er ec)))
    blocked_moves

let test_queen_invalid_moves _ =
  let board = empty_board () in
  board.(4).(4) <- Some "W_Queen";
  let invalid_moves =
    [ (6, 5) (* Non-linear/non-diagonal move (e.g. like a knight's move *) ]
  in
  List.iter
    (fun (er, ec) ->
      assert_bool "Non-standard queen move should be invalid"
        (not (is_valid_queen_move board 4 4 er ec)))
    invalid_moves

let test_king_valid_moves _ =
  let board = empty_board () in
  let start_row = 4 and start_col = 4 in
  board.(start_row).(start_col) <- Some "W_King";
  let valid_moves =
    [
      (start_row - 1, start_col); (* Up *) (start_row + 1, start_col);
      (* Down *) (start_row, start_col - 1); (* Left *)
      (start_row, start_col + 1); (* Right *) (start_row - 1, start_col - 1);
      (* Up-Left Diagonal *) (start_row - 1, start_col + 1);
      (* Up-Right Diagonal *) (start_row + 1, start_col - 1);
      (* Down-Left Diagonal *) (start_row + 1, start_col + 1);
      (* Down-Right Diagonal *)
    ]
  in
  List.iter
    (fun (er, ec) ->
      assert_bool "Valid king move"
        (is_valid_king_move start_row start_col er ec))
    valid_moves

let test_king_invalid_moves _ =
  let board = empty_board () in
  let start_row = 4 and start_col = 4 in
  board.(start_row).(start_col) <- Some "W_King";
  let invalid_moves =
    [
      (start_row, start_col); (* No move *) (start_row - 2, start_col);
      (* Too far up *) (start_row + 2, start_col); (* Too far down *)
      (start_row, start_col - 2); (* Too far left *) (start_row, start_col + 2);
      (* Too far right *) (start_row - 2, start_col - 2); (* Too far diagonal *)
    ]
  in
  List.iter
    (fun (er, ec) ->
      assert_bool "Invalid king move"
        (not (is_valid_king_move start_row start_col er ec)))
    invalid_moves

let test_king_edge_moves _ =
  let board = empty_board () in
  let edge_positions = [ (0, 0); (0, 7); (7, 0); (7, 7) ] in
  List.iter
    (fun (sr, sc) ->
      board.(sr).(sc) <- Some "W_King";
      let expected_moves =
        [
          (max (sr - 1) 0, max (sc - 1) 0); (max (sr - 1) 0, min (sc + 1) 7);
          (min (sr + 1) 7, max (sc - 1) 0); (min (sr + 1) 7, min (sc + 1) 7);
        ]
      in
      List.iter
        (fun (er, ec) ->
          if (er <> sr || ec <> sc) && er >= 0 && er < 8 && ec >= 0 && ec < 8
          then
            assert_bool "Valid edge king move" (is_valid_king_move sr sc er ec))
        expected_moves;
      board.(sr).(sc) <- None
      (* Clear after test *))
    edge_positions

let test_castling_valid _ =
  reset_states ();
  let board = empty_board () in
  (* Setting up a board where castling is possible *)
  board.(7).(4) <- Some "W_King";
  board.(7).(7) <- Some "W_Rook";
  board.(7).(0) <- Some "W_Rook";
  assert_bool "Kingside castling should be valid"
    (is_valid_castle board 7 4 7 6);
  assert_bool "Queenside castling should be valid"
    (is_valid_castle board 7 4 7 2)

let test_castling_invalid_through_check _ =
  reset_states ();
  let board = empty_board () in
  (* King is passing through a square under attack *)
  board.(7).(4) <- Some "W_King";
  board.(7).(7) <- Some "W_Rook";
  board.(0).(5) <- Some "B_Rook";
  (* Attacking the path of the king *)
  assert_bool "Kingside castling should not be valid"
    (not (is_valid_castle board 7 4 7 6))

let test_pawn_normal_forward _ =
  let board = empty_board () in
  board.(6).(0) <- Some "W_Pawn";
  assert_bool "Pawn should move forward one square"
    (is_valid_pawn_move board "W_Pawn" 6 0 5 0);
  board.(5).(0) <- Some "B_Pawn";
  assert_bool "Pawn cannot move into occupied square"
    (not (is_valid_pawn_move board "W_Pawn" 6 0 5 0))

let test_pawn_initial_two_squares _ =
  let board = empty_board () in
  board.(6).(1) <- Some "W_Pawn";
  assert_bool "Pawn can move two squares initially"
    (is_valid_pawn_move board "W_Pawn" 6 1 4 1);
  board.(5).(1) <- Some "B_Pawn";
  assert_bool "Pawn cannot move two squares if path is blocked"
    (not (is_valid_pawn_move board "W_Pawn" 6 1 4 1))

let test_pawn_diagonal_capture _ =
  let board = empty_board () in
  board.(6).(2) <- Some "W_Pawn";
  board.(5).(3) <- Some "B_Pawn";
  assert_bool "Pawn should capture diagonally"
    (is_valid_pawn_move board "W_Pawn" 6 2 5 3);
  board.(5).(3) <- None;
  assert_bool "Pawn cannot move diagonally without capture"
    (not (is_valid_pawn_move board "W_Pawn" 6 2 5 3))

let test_pawn_backward_moves _ =
  let board = empty_board () in
  board.(4).(4) <- Some "W_Pawn";
  board.(5).(4) <- Some "B_Pawn";
  assert_bool "Pawns should not move backwards"
    (not (is_valid_pawn_move board "W_Pawn" 4 4 5 4))

let test_pawn_edge_moves _ =
  let board = empty_board () in
  board.(1).(0) <- Some "W_Pawn";
  assert_bool "Pawn should move to the last row for promotion"
    (is_valid_pawn_move board "W_Pawn" 1 0 0 0);

  assert_bool "Pawn should not move off the board horizontally"
    (not (is_valid_pawn_move board "W_Pawn" 1 0 1 1))

let test_pawn_invalid_diagonal_moves _ =
  let board = empty_board () in
  board.(4).(4) <- Some "W_Pawn";
  assert_bool "Pawn cannot move diagonally without capture - right up"
    (not (is_valid_pawn_move board "W_Pawn" 4 4 3 5));
  assert_bool "Pawn cannot move diagonally without capture - left up"
    (not (is_valid_pawn_move board "W_Pawn" 4 4 3 3))

let test_pawn_promotion_valid _ =
  make_currstate_empty ();
  (* Pawn reaches the promotion square *)
  curr_state.(1).(0) <- Some "W_Pawn";
  update_currstate 1 0 0 0;
  (* Move the pawn to the promotion square *)
  promote 0 0 "W_Queen";
  assert_equal "W_Queen" (piece_at curr_state 0 0)

let test_valid_piece_moves _ =
  let board = empty_board () in
  board.(1).(1) <- Some "W_Pawn";
  board.(2).(0) <- Some "B_King";
  board.(0).(2) <- Some "W_Bishop";
  board.(0).(0) <- Some "W_Rook";
  board.(4).(4) <- Some "W_Knight";
  board.(7).(7) <- Some "W_Queen";
  board.(3).(3) <- Some "W_King";
  assert_bool "Valid bishop move" (is_valid_move board "W_Bishop" 0 2 1 3);
  assert_bool "Valid rook move" (is_valid_move board "W_Rook" 0 0 0 1);
  assert_bool "Valid knight move" (is_valid_move board "W_Knight" 4 4 6 5);
  assert_bool "Valid queen move" (is_valid_move board "W_Queen" 7 7 7 6);
  assert_bool "Valid king move" (is_valid_move board "W_King" 3 3 4 3);
  assert_bool "Valid pawn move" (is_valid_move board "W_Pawn" 1 1 0 1)

let test_invalid_same_color_block _ =
  let board = empty_board () in
  board.(1).(1) <- Some "W_Pawn";
  board.(2).(0) <- Some "B_King";
  board.(0).(2) <- Some "W_Bishop";
  board.(0).(0) <- Some "W_Rook";
  board.(4).(4) <- Some "W_Knight";
  board.(7).(7) <- Some "W_Queen";
  board.(3).(3) <- Some "W_King";
  board.(1).(2) <- Some "W_Rook";
  assert_bool "Invalid bishop move due to block"
    (not (is_valid_move board "W_Bishop" 0 2 1 2));
  assert_bool "Invalid rook move due to block"
    (not (is_valid_move board "W_Rook" 0 0 0 3))

let test_invalid_piece_moves _ =
  let board = empty_board () in
  board.(1).(1) <- Some "W_Pawn";
  board.(2).(0) <- Some "B_King";
  board.(0).(2) <- Some "W_Bishop";
  board.(0).(0) <- Some "W_Rook";
  board.(4).(4) <- Some "W_Knight";
  board.(7).(7) <- Some "W_Queen";
  board.(3).(3) <- Some "W_King";
  assert_bool "Bishop invalid non-diagonal move"
    (not (is_valid_move board "W_Bishop" 0 2 0 3));
  assert_bool "Knight invalid move"
    (not (is_valid_move board "W_Knight" 4 4 4 6))

let test_unrecognized_piece _ =
  let board = empty_board () in
  board.(1).(1) <- Some "W_Pawn";
  board.(2).(0) <- Some "B_King";
  board.(0).(2) <- Some "W_Bishop";
  board.(0).(0) <- Some "W_Rook";
  board.(4).(4) <- Some "W_Knight";
  board.(7).(7) <- Some "W_Queen";
  board.(3).(3) <- Some "W_King";
  assert_bool "Unrecognized piece type should return false"
    (not (is_valid_move board "W_Zebra" 0 0 3 0))

let test_material_values _ =
  let pieces =
    [ "W_Pawn"; "W_Rook"; "W_Bishop"; "W_Knight"; "W_Queen"; "W_King" ]
  in
  let expected_values = [ 1; 5; 3; 3; 9; 0 ] in
  List.iter2
    (fun piece expected_value ->
      let result = material piece in
      assert_equal expected_value result ~printer:string_of_int)
    pieces expected_values

let test_material_pawn _ =
  assert_equal 1 (material "B_Pawn") ~printer:string_of_int;
  assert_equal 1 (material "W_Pawn") ~printer:string_of_int

let test_material_rook _ =
  assert_equal 5 (material "B_Rook") ~printer:string_of_int;
  assert_equal 5 (material "W_Rook") ~printer:string_of_int

let test_material_bishop _ =
  assert_equal 3 (material "B_Bishop") ~printer:string_of_int;
  assert_equal 3 (material "W_Bishop") ~printer:string_of_int

let test_material_knight _ =
  assert_equal 3 (material "B_Knight") ~printer:string_of_int;
  assert_equal 3 (material "W_Knight") ~printer:string_of_int

let test_material_queen _ =
  assert_equal 9 (material "B_Queen") ~printer:string_of_int;
  assert_equal 9 (material "W_Queen") ~printer:string_of_int

let test_material_unexpected _ =
  assert_equal 0 (material "B_King") ~printer:string_of_int;
  assert_equal 0 (material "W_King") ~printer:string_of_int

let test_material_unexpected_characters _ =
  assert_equal 0 (material "W_Zebra") ~printer:string_of_int

let test_material_partial_input _ =
  assert_equal 0 (material "Pawn") ~printer:string_of_int

let test_king_in_check _ =
  let board = empty_board () in
  board.(7).(4) <- Some "W_King";
  board.(7).(7) <- Some "B_Rook";
  board.(0).(4) <- Some "B_Rook";
  turn := "W";
  assert_bool "King should be in check" (in_check board)

let test_checkmate _ =
  reset_states ();
  make_currstate_empty ();
  let board = empty_board () in
  update_states board 0 4 (Some "B_King");
  update_states board 0 3 (Some "W_Queen");
  update_states board 1 2 (Some "W_Queen");
  turn := "B";
  assert_bool "Black should be in checkmate" (checkmated board)

let test_stalemate _ =
  let board = empty_board () in
  board.(7).(7) <- Some "W_King";
  board.(5).(6) <- Some "B_Queen";
  board.(6).(5) <- Some "B_King";
  turn := "W";
  assert_equal "Stalemate" (is_draw board)

let test_draw_insufficient_material _ =
  reset_states ();
  let board = empty_board () in
  board.(0).(0) <- Some "W_King";
  board.(7).(7) <- Some "B_King";
  assert_equal "Insufficient Material" (is_draw board)

let rec move_fifty board n =
  if n <> 0 then
    if valid_move board 2 3 2 2 then move_fifty board (n - 1)
    else if valid_move board 6 5 6 6 then move_fifty board (n - 1)

let test_fifty_move_rule _ =
  reset_states ();
  make_currstate_empty ();
  let board = empty_board () in
  board.(2).(3) <- Some "W_King";
  board.(6).(5) <- Some "B_King";
  move_fifty board 100;
  assert_equal "50 move rule" (is_draw board)

let suite =
  "Chess Game Tests"
  >::: [
         "test_piece_at_piece" >:: test_piece_at_piece;
         "test_piece_at_no_piece" >:: test_piece_at_no_piece;
         "test_same_color_same" >:: test_same_color_same;
         "test_same_color_different" >:: test_same_color_different;
         "test_same_color_no_piece" >:: test_same_color_no_piece;
         "test_update_turn_white_to_black" >:: test_update_turn_white_to_black;
         "test_update_turn_black_to_white" >:: test_update_turn_black_to_white;
         "test_update_turn_invalid" >:: test_update_turn_invalid;
         "test_enpassant_capture" >:: test_enpassant_capture;
         "test_no_enpassant_capture" >:: test_no_enpassant_capture;
         "test_castling_valid" >:: test_castling_valid;
         "test_castling_invalid_through_check"
         >:: test_castling_invalid_through_check;
         "test_pawn_promotion_valid" >:: test_pawn_promotion_valid;
         "test_bishop_valid_moves" >:: test_bishop_valid_moves;
         "test_king_in_check" >:: test_king_in_check;
         "test_checkmate" >:: test_checkmate;
         "test_stalemate" >:: test_stalemate;
         "test_draw_insufficient_material" >:: test_draw_insufficient_material;
         "test_fifty_move_rule" >:: test_fifty_move_rule;
         "test_bishop_blocked_moves" >:: test_bishop_blocked_moves;
         "test_bishop_invalid_moves" >:: test_bishop_invalid_moves;
         "test_rook_valid_horizontal_moves" >:: test_rook_valid_horizontal_moves;
         "test_rook_valid_vertical_moves" >:: test_rook_valid_vertical_moves;
         "test_rook_blocked_horizontal" >:: test_rook_blocked_horizontal;
         "test_rook_blocked_vertical" >:: test_rook_blocked_vertical;
         "test_rook_invalid_diagonal" >:: test_rook_invalid_diagonal;
         "test_knight_valid_moves" >:: test_knight_valid_moves;
         "test_knight_invalid_moves" >:: test_knight_invalid_moves;
         "test_knight_edge_moves" >:: test_knight_edge_moves;
         "test_queen_valid_horizontal_vertical_moves"
         >:: test_queen_valid_horizontal_vertical_moves;
         "test_queen_valid_diagonal_moves" >:: test_queen_valid_diagonal_moves;
         "test_queen_blocked_moves" >:: test_queen_blocked_moves;
         "test_queen_invalid_moves" >:: test_queen_invalid_moves;
         "test_king_valid_moves" >:: test_king_valid_moves;
         "test_king_invalid_moves" >:: test_king_invalid_moves;
         "test_king_edge_moves" >:: test_king_edge_moves;
         "test_pawn_normal_forward" >:: test_pawn_normal_forward;
         "test_pawn_initial_two_squares" >:: test_pawn_initial_two_squares;
         "test_pawn_diagonal_capture" >:: test_pawn_diagonal_capture;
         "test_pawn_edge_moves" >:: test_pawn_edge_moves;
         "test_pawn_backward_moves" >:: test_pawn_backward_moves;
         "test_pawn_invalid_diagonal_moves" >:: test_pawn_invalid_diagonal_moves;
         "test_valid_piece_moves" >:: test_valid_piece_moves;
         "test_invalid_same_color_block" >:: test_invalid_same_color_block;
         "test_invalid_piece_moves" >:: test_invalid_piece_moves;
         "test_unrecognized_piece" >:: test_unrecognized_piece;
         "test_material_values" >:: test_material_values;
         "test_material_pawn" >:: test_material_pawn;
         "test_material_rook" >:: test_material_rook;
         "test_material_bishop" >:: test_material_bishop;
         "test_material_knight" >:: test_material_knight;
         "test_material_queen" >:: test_material_queen;
         "test_material_unexpected" >:: test_material_unexpected;
         "test_material_unexpected_characters"
         >:: test_material_unexpected_characters;
         "test_material_partial_input" >:: test_material_partial_input;
       ]

let () = run_test_tt_main suite
