let has_moved = [| false; false; false; false; false |]

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

(**[turn] contains the color of the player to move. ['W'] means white to move
   and ['B'] means black to move.*)
let turn = ref 'W'

(**[update_turn ()] allows the next player to move.*)
let update_turn () =
  match !turn with
  | 'W' -> turn := 'B'
  | 'B' -> turn := 'W'
  | _ -> failwith ""

let play_turn p =
  if p then (
    update_turn ();
    true)
  else false

let update_rook_moved r c =
  match (r, c) with
  | 0, 0 -> has_moved.(0) <- true
  | 0, 7 -> has_moved.(1) <- true
  | 7, 0 -> has_moved.(2) <- true
  | 7, 7 -> has_moved.(3) <- true
  | _ -> ()

(** [is_valid_move piece start_row start_col end_row end_col state] checks if a
    move from [start_row, start_col] to [end_row, end_col] is valid based on the
    type of [piece] and the current [state] of the board. *)
let is_valid_move piece start_row start_col end_row end_col state =
  (*only a valid move if the correct player is moving a piece and is not landing
    on a square occupied by one of that player's pieces*)
  if piece.[0] <> !turn || same_color piece end_row end_col state then false
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
    | "B_King" | "W_King" ->
        if is_valid_king_move start_row start_col end_row end_col then
          has_moved.(4) <- true;
        play_turn (is_valid_king_move start_row start_col end_row end_col)
    | "B_Pawn" | "W_Pawn" ->
        play_turn
          (is_valid_pawn_move piece start_row start_col end_row end_col state)
    | _ -> false
