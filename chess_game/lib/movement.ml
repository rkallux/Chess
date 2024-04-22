let is_valid_bishop_move start_row start_col end_row end_col =
  abs (end_row - start_row) = abs (end_col - start_col)

let is_valid_rook_move start_row start_col end_row end_col =
  start_row = end_row || start_col = end_col

let is_valid_knight_move start_row start_col end_row end_col =
  let row_diff = abs (end_row - start_row) in
  let col_diff = abs (end_col - start_col) in
  (row_diff = 2 && col_diff = 1) || (row_diff = 1 && col_diff = 2)

let is_valid_queen_move start_row start_col end_row end_col =
  is_valid_rook_move start_row start_col end_row end_col
  || is_valid_bishop_move start_row start_col end_row end_col

let is_valid_king_move start_row start_col end_row end_col =
  let row_diff = abs (end_row - start_row) in
  let col_diff = abs (end_col - start_col) in
  row_diff <= 1 && col_diff <= 1

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

let is_valid_move piece start_row start_col end_row end_col state =
  match piece with
  | "B_Bishop" | "W_Bishop" ->
      is_valid_bishop_move start_row start_col end_row end_col
  | "B_Rook" | "W_Rook" ->
      is_valid_rook_move start_row start_col end_row end_col
  | "B_Knight" | "W_Knight" ->
      is_valid_knight_move start_row start_col end_row end_col
  | "B_Queen" | "W_Queen" ->
      is_valid_queen_move start_row start_col end_row end_col
  | "B_King" | "W_King" ->
      is_valid_king_move start_row start_col end_row end_col
  | "B_Pawn" | "W_Pawn" ->
      is_valid_pawn_move piece start_row start_col end_row end_col state
  | _ -> false
