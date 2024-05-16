(** The current board state of the game. *)
val curr_state : string option array array

(** The color of the player whose turn it is to move *)
val turn : string ref

(** A tuple whose first element is the color that has the material advantage (is
    "same" if equal) and whose second element is the value of the advantage. *)
val material_advantage : unit -> string * int

(** The list of all captured white pieces *)
val captured_W : string list ref

(** The list of all captured black pieces *)
val captured_B : string list ref

(** [piece_at state row col] is the piece at [row], [col] during [state] *)
val piece_at : string option array array -> int -> int -> string

(** [valid_move  piece start_row start_col end_row end_col state] is true if the
    move made by a player is legal. Does not account for castling. *)
val valid_move : string option array array -> int -> int -> int -> int -> bool

(** [is_enpassant state s_row s_col e_row e_col] is true if the squares clicked
    is a legal en passant move. *)
val is_enpassant : string option array array -> int -> int -> int -> int -> bool

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

(** [is_valid_move state piece start_row start_col end_row end_col] is true if
    [piece] on square [start_row], [start_col] can move to [end_row], [end_col] *)
val is_valid_move :
  string option array array -> string -> int -> int -> int -> int -> bool

(** [in_check state] is true if a player is in check during [state] and false
    otherwise. *)
val in_check : string option array array -> bool

(** [checkmated state] is true if a player is checkmated during [state] and
    false otherwise. *)
val checkmated : string option array array -> bool

(** [is_draw state] is true if the current position of [state] is a draw and
    false otherwise*)
val is_draw : string option array array -> string

(** [is_draw state] gives the reason of the draw if there is a draw and "no"
    otherwise*)
val is_draw_test : string option array array -> unit

(** [is_valid_castle state s_row s_col e_row e_col] is true if the player has
    clicked on two legal squares for castling. *)
val is_valid_castle :
  string option array array -> int -> int -> int -> int -> bool

(** [type_castle s_row s_col e_row e_col] is the string representation of what
    castle is being made. *)
val type_castle : int -> int -> int -> int -> string

(** Updates the state to reflect a castling move *)
val castle_state : string option array array -> int -> int -> int -> int -> unit

(** [material piece] is the material worth of [piece]. *)
val material : string -> int

(** [total_material p_list] is sum of the worth of all pieces in [p_list]. ]*)
val total_material : string list -> int

(** [update_state board x1 y1 x2 y2] updates the game state to reflect a move
    from position (x1, y1) to (x2, y2) on the [board]. *)
val update_state : string option array array -> int -> int -> int -> int -> unit

(** [update_captures board x y] captures a piece at position (x, y) on the
    [board] and updates the array of captured pieces. *)
val update_captures : string option array array -> int -> int -> unit

(** [update_turn ()] updates the turn to the next player. *)
val update_turn : unit -> unit

(** [promote x y piece] promotes a pawn at position (x, y) to a new [piece]. *)
val promote : int -> int -> string -> unit

(** [update_enpassant_captured_state board x y] updates the state to reflect the
    captured pawn during an en passant at position (x, y) on the [board]. *)
val update_enpassant_captured_state :
  string option array array -> int -> int -> unit

(** [fifty_move ()] checks whether it has been fifty moves since the last
    capture or pawn move, which would result in a draw under the fifty-move
    rule. *)
val fifty_move : unit -> bool

(** [insufficient_material board] checks for a draw by determining if there is
    insufficient material on the [board] to continue the game. *)
val insufficient_material : string option array array -> bool

(*************************** TESTING FUNCTIONS ****************************)

(** [update_currstate x1 y1 x2 y2] updates the current state for testing
    purposes to reflect a move from position (x1, y1) to (x2, y2). Used only for
    testing. *)
val update_currstate : int -> int -> int -> int -> unit

(** [make_currstate_empty ()] updates the current state to an empty board. Used
    only for testing. *)
val make_currstate_empty : unit -> unit

(** [reset_states ()] resets states of the board that cannot be modified outside
    of Movement. Used only for testing. *)
val reset_states : unit -> unit

(** [last_pawn_or_capture] holds the number of moves since the last pawn move or
    capture. Used only for testing. *)
val last_pawn_or_capture : int ref

(** [piece_valid_moves board piece x y] computes a list of valid moves (as
    coordinate pairs) for a [piece] at position (x, y) on the [board]. Used only
    for testing. *)
val piece_valid_moves :
  string option array array -> string -> int -> int -> (int * int) list

(** [add_valid_sqs func board piece moves x y] applies [func] to generate a list
    of valid moves for a [piece] at position (x, y) on the [board], adding these
    moves to the existing list [moves]. Used only for testing. *)
val add_valid_sqs :
  (string option array array -> string -> int -> int -> 'a list) ->
  string option array array ->
  string ->
  'a list ->
  int ->
  int ->
  'a list

(** [valid_b_moves board] calculates a list of valid moves for black pieces on
    the [board]. Used only for testing. *)
val valid_b_moves : string option array array -> (int * int) list

(** [valid_w_moves board] calculates a list of valid moves for white pieces on
    the [board]. Used only for testing. *)
val valid_w_moves : string option array array -> (int * int) list

(** [get_piece_square board piece x y] identifies the square (as a coordinate
    pair) of a specific [piece] located at (x, y) on the [board]. Used only for
    testing. *)
val get_piece_square :
  string option array array -> string -> int -> int -> int * int

(** [past_states] holds a reference to a list of past game states and their
    corresponding turn numbers. Each state is represented as a tuple containing
    the board state and the turn number. Used only for testing. *)
val past_states : (string option array array * int) list ref

(** [print_past_states ()] prints all past game states and their turn numbers
    stored in [past_states]. Used only for testing. *)
val print_past_states : unit -> unit

(*************************** TESTING FUNCTIONS ****************************)
