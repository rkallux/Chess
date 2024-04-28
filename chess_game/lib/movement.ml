let has_moved = [| false; false; false; false; false; false |]
(* Corresponds to [wksr, wqsr, bksr, bqsr, wk, bk] *)

let rec bishop_move_h start_row start_col end_row end_col acc =
  if start_row < end_row && start_col < end_col then
    if start_row = end_row - 1 && start_col = end_col - 1 then acc
    else
      bishop_move_h (start_row + 1) (start_col + 1) end_row end_col
        ((start_row + 1, start_col + 1) :: acc)
  else if start_row < end_row && start_col > end_col then
    if start_row = end_row - 1 && start_col = end_col + 1 then acc
    else
      bishop_move_h (start_row + 1) (start_col - 1) end_row end_col
        ((start_row + 1, start_col - 1) :: acc)
  else if start_row > end_row && start_col < end_col then
    if start_row = end_row + 1 && start_col = end_col - 1 then acc
    else
      bishop_move_h (start_row - 1) (start_col + 1) end_row end_col
        ((start_row - 1, start_col + 1) :: acc)
  else if start_row = end_row + 1 && start_col = end_col + 1 then acc
  else
    bishop_move_h (start_row - 1) (start_col - 1) end_row end_col
      ((start_row - 1, start_col - 1) :: acc)

(** [is_valid_bishop_move start_row start_col end_row end_col] checks if a move
    from [start_row, start_col] to [end_row, end_col] is a valid bishop move.
    Bishops move diagonally. *)
let is_valid_bishop_move start_row start_col end_row end_col state =
  (not (start_row = end_row))
  && abs (end_row - start_row) = abs (end_col - start_col)
  && List.for_all
       (fun ele -> state.(fst ele).(snd ele) = None)
       (bishop_move_h start_row start_col end_row end_col [])

let rec rook_move_h same start_n end_n acc =
  if start_n < end_n then
    if start_n = end_n - 1 then acc
      (*Allows for capture of pieces by having start_n = end_n +- 1*)
    else rook_move_h same (start_n + 1) end_n ((same, start_n + 1) :: acc)
  else if start_n = end_n + 1 then acc
  else rook_move_h same (start_n - 1) end_n ((same, start_n - 1) :: acc)

(** [is_valid_rook_move start_row start_col end_row end_col] checks if a move
    from [start_row, start_col] to [end_row, end_col] is a valid rook move.
    Rooks move horizontally or vertically. *)
let is_valid_rook_move start_row start_col end_row end_col state =
  (not (start_row = end_row && start_col = end_col))
  && (start_row = end_row
      && List.for_all
           (fun ele -> state.(fst ele).(snd ele) = None)
           (rook_move_h start_row start_col end_col [])
     || start_col = end_col
        && List.for_all
             (fun ele -> state.(snd ele).(fst ele) = None)
             (rook_move_h start_col start_row end_row []))

(** [is_valid_knight_move start_row start_col end_row end_col] checks if a move
    from [start_row, start_col] to [end_row, end_col] is a valid knight move.
    Knights move in an 'L' shape: two squares vertically and one horizontally or
    one vertically and two horizontally. *)
let is_valid_knight_move start_row start_col end_row end_col =
  let row_diff = abs (end_row - start_row) in
  let col_diff = abs (end_col - start_col) in
  (row_diff = 2 && col_diff = 1) || (row_diff = 1 && col_diff = 2)

(** [is_valid_queen_move start_row start_col end_row end_col] checks if a move
    from [start_row, start_col] to [end_row, end_col] is a valid queen move. The
    queen can move both horizontally/vertically and diagonally. *)
let is_valid_queen_move start_row start_col end_row end_col state =
  is_valid_rook_move start_row start_col end_row end_col state
  || is_valid_bishop_move start_row start_col end_row end_col state

(** [is_valid_king_move start_row start_col end_row end_col] checks if a move
    from [start_row, start_col] to [end_row, end_col] is a valid king move. The
    king moves one square in any direction. *)
let is_valid_king_move start_row start_col end_row end_col =
  let row_diff = abs (end_row - start_row) in
  let col_diff = abs (end_col - start_col) in
  (not (start_row = end_row && start_col = end_col))
  && row_diff <= 1 && col_diff <= 1

(** [is_valid_pawn_move piece start_row start_col end_row end_col state] checks
    if a move from [start_row, start_col] to [end_row, end_col] is a valid pawn
    move, based on the current [state] of the board. Pawns move forward one
    square, but can move two squares from their initial position, and capture
    diagonally. *)
let is_valid_pawn_move piece start_row start_col end_row end_col state =
  let forward = if piece = "W_Pawn" then -1 else 1 in
  (* white pawns move up, black pawns move down *)
  let start_rank = if piece = "W_Pawn" then 6 else 1 in
  (* initial row from which pawns can first move 2 steps *)
  match (end_row - start_row, end_col - start_col) with
  | r, 0 when r = forward ->
      state.(end_row).(end_col)
      = None (* move forward one square, end square must be empty *)
  | r, 0 when r = 2 * forward && start_row = start_rank ->
      state.(start_row + forward).(start_col)
      = None (* square in front of pawn is None *)
      && state.(end_row).(end_col) = None
      (* square 2 steps ahead is none *)
      (* initial two-square move, end square must be empty *)
      (* TODO: only should run once per pawn *)
  | r, c when r = forward && abs c = 1 ->
      state.(end_row).(end_col) <> None
      (* capture diagonally *)
      (* TODO: should check that end square is an opposing color *)
  | _ -> false

(**[same_color piece end_row end_col state] is true if the piece at the position
   of ([end_row], [end_column] in state has the same color as [piece] and false
   otherwise)*)
let same_color piece end_row end_col state =
  let end_piece =
    match state.(end_row).(end_col) with
    | Some p -> p
    | None -> "n"
  in
  if piece.[0] = end_piece.[0] then true else false

(**[turn] contains the color of the player to move. ["W"] means white to move
   and ["B"] means black to move.*)
let turn = ref "W"

(**[update_turn ()] allows the next player to move.*)
let update_turn () =
  match !turn with
  | "W" -> turn := "B"
  | "B" -> turn := "W"
  | _ -> failwith ""

let play_turn p =
  if p then (
    update_turn ();
    true)
  else false

let update_rook_moved r c =
  match (r, c) with
  | 7, 7 -> has_moved.(0) <- true (* White kingside rook has moved *)
  | 7, 0 -> has_moved.(1) <- true (* White queenside rook has moved *)
  | 0, 7 -> has_moved.(2) <- true (* Black kingside rook has moved *)
  | 0, 0 -> has_moved.(3) <- true (* Black queenside rook has moved *)
  | _ -> ()

let is_valid_move piece start_row start_col end_row end_col state =
  (*only a valid move if the correct player is moving a piece and is not landing
    on a square occupied by one of that player's pieces*)
  if String.sub piece 0 1 <> !turn || same_color piece end_row end_col state
  then false
  else
    match piece with
    (*if a player makes a valid move, it is now the next player's turn*)
    | "B_Bishop" | "W_Bishop" ->
        play_turn
          (is_valid_bishop_move start_row start_col end_row end_col state)
    | "B_Rook" | "W_Rook" ->
        if is_valid_rook_move start_row start_col end_row end_col state then
          update_rook_moved start_row start_col;
        play_turn (is_valid_rook_move start_row start_col end_row end_col state)
    | "B_Knight" | "W_Knight" ->
        play_turn (is_valid_knight_move start_row start_col end_row end_col)
    | "B_Queen" | "W_Queen" ->
        play_turn
          (is_valid_queen_move start_row start_col end_row end_col state)
    | "B_King" ->
        if is_valid_king_move start_row start_col end_row end_col then
          has_moved.(5) <- true;
        play_turn (is_valid_king_move start_row start_col end_row end_col)
    | "W_King" ->
        if is_valid_king_move start_row start_col end_row end_col then
          has_moved.(4) <- true;
        play_turn (is_valid_king_move start_row start_col end_row end_col)
    | "B_Pawn" | "W_Pawn" ->
        play_turn
          (is_valid_pawn_move piece start_row start_col end_row end_col state)
    | _ -> false

let checks_for_castle a b c d state =
  (* a, b, c, d are just factored out variables (not especially meaningful) *)
  state.(a).(b) = None
  && state.(a).(c) = None
  && (!turn = if a = 7 then "W" else "B")
  && (not has_moved.(if a = 7 then 4 else 5))
  && not has_moved.(d)

let can_castle start_row start_col end_row end_col state =
  match (start_row, start_col, end_row, end_col) with
  | 7, 4, 7, 6 -> checks_for_castle 7 5 6 0 state
  | 7, 4, 7, 2 -> state.(7).(1) = None && checks_for_castle 7 2 3 1 state
  | 0, 4, 0, 6 -> checks_for_castle 0 5 6 2 state
  | 0, 4, 0, 2 -> state.(0).(1) = None && checks_for_castle 0 2 3 3 state
  | _ -> false

let castle start_row start_col end_row end_col =
  match (start_row, start_col, end_row, end_col) with
  | 7, 4, 7, 6 ->
      update_turn ();
      has_moved.(4) <- true;
      "wksc" (* White kingside castle*)
  | 7, 4, 7, 2 ->
      update_turn ();
      has_moved.(4) <- true;
      "wqsc" (* White queenside castle*)
  | 0, 4, 0, 6 ->
      update_turn ();
      has_moved.(5) <- true;
      "bksc" (* Black kingside castle*)
  | 0, 4, 0, 2 ->
      update_turn ();
      has_moved.(5) <- true;
      "bqsc" (* Black queenside castle*)
  | _ -> ""

let board_list =
  [
    (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7); (1, 1); (1, 2);
    (1, 3); (1, 4); (1, 5); (1, 6); (1, 7); (2, 1); (2, 2); (2, 3); (2, 4);
    (2, 5); (2, 6); (2, 7); (3, 1); (3, 2); (3, 3); (3, 4); (3, 5); (3, 6);
    (3, 7); (4, 1); (4, 2); (4, 3); (4, 4); (4, 5); (4, 6); (4, 7); (5, 1);
    (5, 2); (5, 3); (5, 4); (5, 5); (5, 6); (5, 7); (6, 1); (6, 2); (6, 3);
    (6, 4); (6, 5); (6, 6); (6, 7); (7, 1); (7, 2); (7, 3); (7, 4); (7, 5);
    (7, 6); (7, 7);
  ]

let piece_valid_moves piece start_row start_col state =
  List.filter
    (fun e -> is_valid_move piece start_row start_col (fst e) (snd e) state)
    board_list

let is_capture piece sr sc er ec state =
  if is_valid_move piece sr sc er ec state && state.(er).(ec) <> None then true
  else false
