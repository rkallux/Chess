(** The current board state of the game. *)
val curr_state : string option array array

(** The color of the player whose turn it is to move *)
val turn : string ref

(** A tuple whose first element is the color that has the material advantage (is
    "same" if equal) and whose second element is the value of the advantage. *)
val material_advantage : string * int

(** The list of all captured white pieces *)
val captured_W : string list ref

(** The list of all captured black pieces *)
val captured_B : string list ref

(** [piece_at state row col] is the piece at [row], [col] during [state] *)
val piece_at : string option array array -> int -> int -> string

(** [valid_move  piece start_row start_col end_row end_col state] is true if the
    move made by a player is legal. Does not account for castling. *)
val valid_move : string option array array -> int -> int -> int -> int -> bool

(** [is_enpassant s_row s_col e_row e_col] is true if the squares clicked is a
    legal en passant move. *)
val is_enpassant : int -> int -> int -> int -> bool

(** [has_piece state row col] is true if there is a piece on [row], [col] and
    false otherwise. *)
val has_piece : string option array array -> int -> int -> bool

(** [same_color state piece row col] is true if [piece] is the same color as the
    piece on [row], [col] and false otherwise. *)
val same_color : string option array array -> string -> int -> int -> bool

(** [is_valid_bishop_move state s_row s_col e_row e_col] is true if bishop on
    square [s_row], [s_col] can move to [e_row], [e_col] *)
val is_valid_bishop_move :
  string option array array -> int -> int -> int -> int -> bool

(** [is_valid_rook_move state s_row s_col e_row e_col] is true if rook on square
    [s_row], [s_col] can move to [e_row], [e_col] *)
val is_valid_rook_move :
  string option array array -> int -> int -> int -> int -> bool

(** [is_valid_knight_move state s_row s_col e_row e_col] is true if knight on
    square [s_row], [s_col] can move to [e_row], [e_col] *)
val is_valid_knight_move : int -> int -> int -> int -> bool

(** [is_valid_queen_move state s_row s_col e_row e_col] is true if queen on
    square [s_row], [s_col] can move to [e_row], [e_col] *)
val is_valid_queen_move :
  string option array array -> int -> int -> int -> int -> bool

(** [is_valid_king_move state s_row s_col e_row e_col] is true if king on square
    [s_row], [s_col] can move to [e_row], [e_col] *)
val is_valid_king_move : int -> int -> int -> int -> bool

(** [is_valid_pawn_move state s_row s_col e_row e_col] is true if pawn on square
    [s_row], [s_col] can move to [e_row], [e_col] *)
val is_valid_pawn_move :
  string option array array -> string -> int -> int -> int -> int -> bool

(** [valid_b_moves state] is the list of all valid black moves during [state].
    Where valid does not include checks. *)
val valid_b_moves : string option array array -> (int * int) list

(** [valid_w_moves state] is the list of all valid white moves during [state].
    Where valid does not include checks. *)
val valid_w_moves : string option array array -> (int * int) list

(** [in_check state] is true if a player is in check during [state] and false
    otherwise. *)
val in_check : string option array array -> bool

(** [checkmated state] is true if a player is checkmated during [state] and
    false otherwise. *)
val checkmated : string option array array -> bool

(** [is_valid_castle s_row s_col e_row e_col] is true if the player has clicked
    on two legal squares for castling. *)
val is_valid_castle : int -> int -> int -> int -> bool

(** [type_castle s_row s_col e_row e_col] is the string representation of what
    castle is being made. *)
val type_castle : int -> int -> int -> int -> string

(** Updates the state to reflect a castling move *)
val castle_state : string option array array -> int -> int -> int -> int -> unit

(** [material piece] is the material worth of [piece]. *)
val material : string -> int

(** [total_material p_list] is sum of the worth of all pieces in [p_list]. ]*)
val total_material : string list -> int

(** Updates the game state to reflect a move from one position to another *)
val update_state : string option array array -> int -> int -> int -> int -> unit

(** Captures a piece at a specific board position and updates the array of
    captured pieces *)
val update_captures : int -> int -> unit

(** Updates the turn to the next player *)
val update_turn : unit -> unit

(** Promotes a pawn to a new piece at a specific board position *)
val promote : int -> int -> string -> unit

(** Updates the state to reflect the captured pawn during an en passant *)
val update_enpassant_captured_state : int -> int -> unit
