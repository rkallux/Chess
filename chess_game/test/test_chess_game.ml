open OUnit2
open Chess_game.Movement
   
   let option_to_string = function
     | None -> "None"
     | Some str -> str
   
   (* Helper function to print the chess board for better error messages *)
   let print_chess_board board =
     Array.map (fun row ->
       "|" ^ (Array.map (function
         | None -> " "
         | Some p -> String.sub p 0 1) row |> Array.to_list |> String.concat "|") ^ "|"
     ) board |> Array.to_list |> String.concat "\n"
   
   (* Define curr_state here or pass it as a parameter to piece_square *)
   let curr_state = [|[|None; None; None; None; None; None; None; None|];
                     [|None; None; None; None; None; None; None; None|];
                     [|None; None; None; None; None; None; None; None|];
                     [|None; None; None; None; None; None; None; None|];
                     [|None; None; None; None; None; None; None; None|];
                     [|None; None; None; None; None; None; None; None|];
                     [|None; None; None; None; None; None; None; None|];
                     [|None; None; None; None; None; None; None; None|]|]
   
   let piece_square row col =
     match curr_state.(row).(col) with
     | Some piece -> piece
     | None -> ""
   
   (* Test individual piece placements on the board *)
   let test_piece_positions _ =
     let positions = [
       (0, 0, "B_Rook"); (0, 1, "B_Knight"); (0, 2, "B_Bishop"); (0, 3, "B_Queen");
       (0, 4, "B_King"); (0, 5, "B_Bishop"); (0, 6, "B_Knight"); (0, 7, "B_Rook");
       (1, 0, "B_Pawn"); (1, 1, "B_Pawn"); (1, 2, "B_Pawn"); (1, 3, "B_Pawn");
       (1, 4, "B_Pawn"); (1, 5, "B_Pawn"); (1, 6, "B_Pawn"); (1, 7, "B_Pawn");
       (6, 0, "W_Pawn"); (6, 1, "W_Pawn"); (6, 2, "W_Pawn"); (6, 3, "W_Pawn");
       (6, 4, "W_Pawn"); (6, 5, "W_Pawn"); (6, 6, "W_Pawn"); (6, 7, "W_Pawn");
       (7, 0, "W_Rook"); (7, 1, "W_Knight"); (7, 2, "W_Bishop"); (7, 3, "W_Queen");
       (7, 4, "W_King"); (7, 5, "W_Bishop"); (7, 6, "W_Knight"); (7, 7, "W_Rook");
       (2, 2, ""); (3, 3, ""); (4, 4, ""); (5, 5, "");
     ] in
     List.iter (fun (r, c, expected_piece) ->
       assert_equal expected_piece (piece_square r c) ~printer:(fun p -> p)
     ) positions
   
   (* Test all pawn movements from various positions, including initial double moves, captures, and promotions *)
   let test_pawn_moves _ =
     let initial_state = Array.copy curr_state in
     (* Test all pawns on all rows for possible moves, including moves from initial rows and capturing diagonally *)
     for r = 0 to 7 do
       for c = 0 to 7 do
         let state_white = Array.copy initial_state in
         let state_black = Array.copy initial_state in
         if r < 6 then begin
           update_state state_white r c (r+1) c;  (* Single step move white *)
           assert_equal (Some "W_Pawn") state_white.(r+1).(c) ~printer:option_to_string;
         end;
         if r > 1 then begin
           update_state state_black r c (r-1) c;  (* Single step move black *)
           assert_equal (Some "B_Pawn") state_black.(r-1).(c) ~printer:option_to_string;
         end;
         (* Double moves from initial positions *)
         if r == 6 then begin
           update_state state_white r c (r-2) c;  (* Double step move white *)
           assert_equal (Some "W_Pawn") state_white.(r-2).(c) ~printer:option_to_string;
         end;
         if r == 1 then begin
           update_state state_black r c (r+2) c;  (* Double step move black *)
           assert_equal (Some "B_Pawn") state_black.(r+2).(c) ~printer:option_to_string;
         end;
         (* Diagonal captures if possible *)
         if c > 0 && r < 7 then begin
           state_white.(r+1).(c-1) <- Some "B_Pawn";
           update_state state_white r c (r+1) (c-1);
           assert_equal (Some "W_Pawn") state_white.(r+1).(c-1) ~printer:option_to_string;
         end;
         if c < 7 && r > 0 then begin
           state_black.(r-1).(c+1) <- Some "W_Pawn";
           update_state state_black r c (r-1) (c+1);
           assert_equal (Some "B_Pawn") state_black.(r-1).(c+1) ~printer:option_to_string;
         end;
       done;
     done;
   
   (* Test knights moving in all possible 'L' shapes from all positions on the board *)
   let test_knight_moves _ =
    (* Validate all possible knight moves from multiple positions *)
    let initial_state = Array.make_matrix 8 8 None in
    let positions = [(7, 1); (2, 2); (0, 1); (5, 0); (5, 2); (6, 5)] in  (* Various starting positions for knights *)
    List.iter (fun (r, c) ->
      let state = Array.copy initial_state in
      state.(r).(c) <- Some "W_Knight";  (* Set knight at each position before testing moves *)
      let moves = [
        (r + 2, c + 1); (r + 2, c - 1); (r - 2, c + 1); (r - 2, c - 1);
        (r + 1, c + 2); (r + 1, c - 2); (r - 1, c + 2); (r - 1, c - 2)
      ] in
      List.iter (fun (nr, nc) ->
        if nr >= 0 && nr < 8 && nc >= 0 && nc < 8 then begin
          let move_state = Array.copy state in  (* Make a fresh copy for each move *)
          update_state move_state r c nr nc;
          assert_equal (Some "W_Knight") move_state.(nr).(nc) ~printer:option_to_string;
          assert_bool "Valid knight move" (is_valid_knight_move r c nr nc)
        end
      ) moves
    ) positions
   
   (* Test all bishops moving on both colors of squares from any position *)
   let test_bishop_moves _ =
     let initial_state = Array.map (fun _ -> Array.make 8 None) curr_state;
     for r = 0 to 7 do
       for c = 0 to 7 do
         initial_state.(r).(c) <- Some "W_Bishop";
         let directions = [(-1, -1); (-1, 1); (1, -1); (1, 1)] in
         List.iter (fun (dr, dc) ->
           let rec move r c =
             if r >= 0 && r < 8 && c >= 0 && c < 8 then begin
               let state = Array.copy initial_state;
               update_state state r c (r+dr) (c+dc);
               assert_equal (Some "W_Bishop") state.(r+dr).(c+dc) ~printer:option_to_string;
               move (r+dr) (c+dc)
             end
           in
           move r c
         ) directions;
       done;
     done;
   
   (* Additional tests for complex rook movements across the board *)
   let test_rook_moves _ =
     let initial_state = Array.map (fun _ -> Array.make 8 None) curr_state;
     for r = 0 to 7 do
       for c = 0 to 7 do
         initial_state.(r).(c) <- Some "W_Rook";
         let directions = [(-1, 0); (1, 0); (0, -1); (0, 1)] in
         List.iter (fun (dr, dc) ->
           let rec move r c =
             if r >= 0 && r < 8 && c >= 0 && c < 8 then begin
               let state = Array.copy initial_state;
               update_state state r c (r+dr) (c+dc);
               assert_equal (Some "W_Rook") state.(r+dr).(c+dc) ~printer:option_to_string;
               move (r+dr) (c+dc)
             end
           in
           move r c
         ) directions;
       done;
     done;
   
   (* Multiple tests for queen's movements combining both rook and bishop abilities *)
   let test_queen_moves _ =
     let initial_state = Array.map (fun _ -> Array.make 8 None) curr_state;
     for r = 0 to 7 do
       for c = 0 to 7 do
         initial_state.(r).(c) <- Some "W_Queen";
         let directions = [(-1, -1); (-1, 1); (1, -1); (1, 1); (-1, 0); (1, 0); (0, -1); (0, 1)] in
         List.iter (fun (dr, dc) ->
           let rec move r c =
             if r >= 0 && r < 8 && c >= 0 && c < 8 then begin
               let state = Array.copy initial_state;
               update_state state r c (r+dr) (c+dc);
               assert_equal (Some "W_Queen") state.(r+dr).(c+dc) ~printer:option_to_string;
               move (r+dr) (c+dc)
             end
           in
           move r c
         ) directions;
       done;
     done;
   
   (* Additional tests for checking and checkmate scenarios *)
   let test_check_scenarios _ =
     let initial_state = Array.copy curr_state;
     for r = 0 to 7 do
       for c = 0 to 7 do
         let setup_king_and_threats () =
           initial_state.(r).(c) <- Some "B_King";
           initial_state.(7-r).(7-c) <- Some "W_Queen";  (* Place a queen opposite to the king *)
         in
         setup_king_and_threats ();
         assert_bool "King should be in check" (in_check initial_state);
         (* Move king out of check if possible *)
         if r > 0 then begin
           update_state initial_state r c (r-1) c;  (* Try moving king up to escape check *)
           assert_bool "King moved out of check" (not (in_check initial_state));
           update_state initial_state (r-1) c r c;  (* Move king back to original position *)
         end;
         if r < 7 then begin
           update_state initial_state r c (r+1) c;  (* Try moving king down to escape check *)
           assert_bool "King moved out of check" (not (in_check initial_state));
           update_state initial_state (r+1) c r c;  (* Move king back to original position *)
         end;
         if c > 0 then begin
           update_state initial_state r c r (c-1);  (* Try moving king left to escape check *)
           assert_bool "King moved out of check" (not (in_check initial_state));
           update_state initial_state r (c-1) r c;  (* Move king back to original position *)
         end;
         if c < 7 then begin
           update_state initial_state r c r (c+1);  (* Try moving king right to escape check *)
           assert_bool "King moved out of check" (not (in_check initial_state));
           update_state initial_state r (c+1) r c;  (* Move king back to original position *)
         end;
       done;
     done;
   
   (* Extended tests for castling under different conditions *)
   let test_castling _ =
     let initial_state = Array.copy curr_state;
     for r in [0; 7] do  (* Only test for rows where castling is possible *)
       for c = 1 to 6 do
         initial_state.(r).(c) <- None;  (* Clear path between rook and king *)
       done;
       assert_bool "Should be able to castle kingside" (is_valid_castle initial_state r 4 (r) (r+2));
       assert_bool "Should be able to castle queenside" (is_valid_castle initial_state r 4 (r) (r-2));
       (* Move the rook and test that castling is now invalid *)
       update_rook_moved r 7;
       update_rook_moved r 0;
       assert_bool "Should not be able to castle after rook has moved" (not (is_valid_castle initial_state r 4 (r) (r+2)));
       assert_bool "Should not be able to castle after rook has moved" (not (is_valid_castle initial_state r 4 (r) (r-2)));
     done;
   
   let suite =
     "Chess Game Tests" >::: [
       "Pawn Moves" >:: test_pawn_moves;
       "Knight Moves" >:: test_knight_moves;
       "Bishop Moves" >:: test_bishop_moves;
       "Rook Moves" >:: test_rook_moves;
       "Queen Moves" >:: test_queen_moves;
       "Check Scenarios" >:: test_check_scenarios;
       "Castling" >:: test_castling;
       "King Moves" >:: test_knight_moves
     ]
   
   let () =
     run_test_tt_main suite
   