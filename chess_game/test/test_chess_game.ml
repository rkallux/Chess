open OUnit2
open Chess_game.Movement

(* create an empty board *)
let empty_board () = Array.make_matrix 8 8 None

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

let test_bishop_valid_moves _ =
  let board = empty_board () in
  board.(4).(4) <- Some "B_Bishop";
  let moves =
    [
      (5, 5); (6, 6); (3, 3); (* Southeast *) (5, 3); (6, 2); (3, 5);
      (* Southwest *) (2, 2); (1, 1); (* Northwest *) (2, 6);
      (1, 7) (* Northeast *);
    ]
  in
  List.iter
    (fun (er, ec) ->
      assert_bool "Valid move should be true"
        (is_valid_bishop_move board 4 4 er ec))
    moves

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

let setup_board () =
  let board = empty_board () in
  board.(1).(1) <- Some "W_Pawn";
  board.(2).(0) <- Some "B_King";
  board.(0).(2) <- Some "W_Bishop";
  board.(0).(0) <- Some "W_Rook";
  board.(4).(4) <- Some "W_Knight";
  board.(7).(7) <- Some "W_Queen";
  board.(3).(3) <- Some "W_King";
  board

let test_valid_piece_moves _ =
  let board = setup_board () in
  assert_bool "Valid bishop move" (is_valid_move board "W_Bishop" 0 2 1 3);
  assert_bool "Valid rook move" (is_valid_move board "W_Rook" 0 0 0 1);
  assert_bool "Valid knight move" (is_valid_move board "W_Knight" 4 4 6 5);
  assert_bool "Valid queen move" (is_valid_move board "W_Queen" 7 7 7 6);
  assert_bool "Valid king move" (is_valid_move board "W_King" 3 3 4 3);
  assert_bool "Valid pawn move" (is_valid_move board "W_Pawn" 1 1 0 1)

let test_invalid_same_color_block _ =
  let board = setup_board () in
  board.(1).(2) <- Some "W_Rook";
  assert_bool "Invalid bishop move due to block"
    (not (is_valid_move board "W_Bishop" 0 2 1 2));
  assert_bool "Invalid rook move due to block"
    (not (is_valid_move board "W_Rook" 0 0 0 3))

let test_invalid_piece_moves _ =
  let board = setup_board () in
  assert_bool "Bishop invalid non-diagonal move"
    (not (is_valid_move board "W_Bishop" 0 2 0 3));
  assert_bool "Knight invalid move"
    (not (is_valid_move board "W_Knight" 4 4 4 6))

let test_unrecognized_piece _ =
  let board = setup_board () in
  assert_bool "Unrecognized piece type should return false"
    (not (is_valid_move board "W_Zebra" 0 0 3 0))

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
         "test_bishop_valid_moves" >:: test_bishop_valid_moves;
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
       ]

let () = run_test_tt_main suite
