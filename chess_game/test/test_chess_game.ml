open OUnit2
open Chess_game.Movement

(* create an empty board *)
let empty_board () = Array.make_matrix 8 8 None

(* returns the piece at row col let piece_square state row col = match
   state.(row).(col) with | Some piece -> piece | None -> "" *)

let test_pawn_basic_move _ =
  let board = empty_board () in
  board.(6).(0) <- Some "W_Pawn";
  assert_equal true
    (is_valid_pawn_move board "W_Pawn" 6 0 5 0)
    ~msg:"Pawn should move forward one square"

let test_pawn_invalid_move _ =
  let board = empty_board () in
  board.(6).(0) <- Some "W_Pawn";
  assert_equal false
    (is_valid_pawn_move board "W_Pawn" 6 0 3 0)
    ~msg:"Pawn should move forward one square"

let test_pawn_capture_move _ =
  let board = empty_board () in
  board.(6).(0) <- Some "W_Pawn";
  board.(5).(1) <- Some "B_Pawn";
  assert_equal true
    (is_valid_pawn_move board "W_Pawn" 6 0 5 1)
    ~msg:"Pawn should capture the opposing piece diagonally"

let suite =
  "Chess Game Tests"
  >::: [
         "test_pawn_basic_move" >:: test_pawn_basic_move;
         "test_pawn_invalid_move" >:: test_pawn_invalid_move;
         "test_pawn_capture_move" >:: test_pawn_capture_move;
       ]

let () = run_test_tt_main suite
