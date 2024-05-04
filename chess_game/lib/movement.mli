(* val curr_state : string option array array

   (** [valid_move piece start_row start_col end_row end_col state] is true if
   the move made by a player is allowed. Does not account for castling.
   Currently does not account for checks*) val valid_move : string option array
   array -> int -> int -> int -> int -> bool

   val update_enpassant_captured_state : int -> int -> unit

   (** [is_valid_castle start_row start_col end_row end_col state] is true if
   player inputs a valid castle. *) val is_valid_castle : string option array
   array -> int -> int -> int -> int -> bool

   (** [type_castle start_row start_col end_row end_col] is the string
   representation of what type of castle was made. For example "wksc" stands for
   a white king side castle. *) val type_castle : int -> int -> int -> int ->
   string

   (**[castle_state color k rook_start rook_end] performs a castle for [color]
   on the side matching the rook locations*) val castle_state : string option
   array array -> int -> int -> int -> int -> unit

   (**[turn] contains ["W"] if it is white to move and ["B"] if it is black to
   move*) val turn : string ref

   (**[material piece] is the material value of [piece]*) val material : string
   -> int

   (** [piece_valid moves piece start_row start_col state] is a list of all of
   the legal moves a piece can move to on a board. *) val piece_valid_moves :
   string option array array -> string -> int -> int -> (int * int) list

   (** [update_captures row col] adds the piece at [row, col] to the list of
   captured pieces*) val update_captures : int -> int -> unit

   (**[captured_W] contains the list of all captured white pieces*) val
   captured_W : string list ref

   (**[captured_B] contains the list of all captured white pieces*) val
   captured_B : string list ref

   (**[material_advantage ()] is a tuple with first element being the winning
   side and second element the material advantage*) val material_advantage :
   unit -> string * int

   (**[piece_at row col] is the name of the piece at [(row, col)] if there is a
   piece there and [""] otherwise*) val piece_at : string option array array ->
   int -> int -> string

   (**[promote row col piece] promotes the pawn at [(row,col)] to [piece]*) val
   promote : int -> int -> string -> unit

   (**[update_state row0 col0 row1 rol1] moves the piece at [(row0,col0)] to
   [(row1,col1)]*) val update_state : string option array array -> int -> int ->
   int -> int -> unit

   (**[has_piece row col] is true if there is a piece at [(row, col)] and false
   otherwise*) val has_piece : string option array array -> int -> int -> bool

   (** [is_enpassant start_row start_col end_row end_col state] is true if
   player inputs a valid enpassant. *) val is_enpassant : int -> int -> int ->
   int -> bool

   (**[update_enpassant_captured_state row col] removes the piece at [(row,col)]
   *) val update_enpassant_captured_state : int -> int -> unit *)

val curr_state : string option array array

(** Returns the name of the piece at given row and column in the state *)
val piece_at : string option array array -> int -> int -> string

(** Determines if a pawn can perform an en passant capture *)
val is_enpassant : int -> int -> int -> int -> bool

(** Updates the state to reflect the captured pawn during an en passant *)
val update_enpassant_captured_state : int -> int -> unit

(** Updates the game state to reflect a move from one position to another *)
val update_state : string option array array -> int -> int -> int -> int -> unit

(** Checks if there is a piece at the given row and column in the state *)
val has_piece : string option array array -> int -> int -> bool

(** Checks if a piece at one position has the same color as a piece at another
    position *)
val same_color : string option array array -> string -> int -> int -> bool

(** Contains the color of the player whose turn it is to move *)
val turn : string ref

(** Updates the turn to the next player *)
val update_turn : unit -> unit

(** Executes a turn if the move is valid and returns true if the turn was
    changed *)
val play_turn : bool -> bool

(** Checks if a bishop move from start to end position is valid *)
val is_valid_bishop_move :
  string option array array -> int -> int -> int -> int -> bool

(** Checks if a rook move from start to end position is valid *)
val is_valid_rook_move :
  string option array array -> int -> int -> int -> int -> bool

(** Checks if a knight move from start to end position is valid *)
val is_valid_knight_move : int -> int -> int -> int -> bool

(** Checks if a queen move from start to end position is valid *)
val is_valid_queen_move :
  string option array array -> int -> int -> int -> int -> bool

(** Checks if a king move from start to end position is valid *)
val is_valid_king_move : int -> int -> int -> int -> bool

(** Checks if a pawn move from start to end position is valid *)
val is_valid_pawn_move :
  string option array array -> string -> int -> int -> int -> int -> bool

(** Determines if a move is valid, not accounting for checks *)
val is_valid_move :
  string option array array -> string -> int -> int -> int -> int -> bool

(** Returns a list of all valid moves for a piece at a given position *)
val piece_valid_moves :
  string option array array -> string -> int -> int -> (int * int) list

(** Adds valid moves for pieces of a specific color to a list *)
val add_valid_sqs :
  string option array array ->
  string ->
  (int * int) list ->
  int ->
  int ->
  (int * int) list

(** Recursively collects all valid moves for a color *)
val valid_moves_aux :
  string option array array ->
  string ->
  (int * int) list ->
  int ->
  int ->
  (int * int) list

(** Returns a list of valid moves for black pieces *)
val valid_b_moves : string option array array -> (int * int) list

(** Returns a list of valid moves for white pieces *)
val valid_w_moves : string option array array -> (int * int) list

(** Finds the board position of a specific piece *)
val get_piece_square :
  string option array array -> string -> int -> int -> int * int

(** Determines if the king of the player whose turn it is, is in check *)
val in_check : string option array array -> bool

(** Checks if a castling move is valid *)
val is_valid_castle :
  string option array array -> int -> int -> int -> int -> bool

(** Returns the type of castling move as a string *)
val type_castle : int -> int -> int -> int -> string

(** Updates the state to reflect a castling move *)
val castle_state : string option array array -> int -> int -> int -> int -> unit

(** Promotes a pawn to a new piece at a specific board position *)
val promote : int -> int -> string -> unit

(** Captures a piece at a specific board position and updates the list of
    captured pieces *)
val update_captures : int -> int -> unit

(** Returns the material value of a captured piece *)
val material : string -> int

(** Returns the total material value of captured pieces *)
val total_material : string list -> int

(** Determines which side has a material advantage and the value of that
    advantage *)
val material_advantage : unit -> string * int

(** List of all captured white pieces *)
val captured_W : string list ref

(** List of all captured black pieces *)
val captured_B : string list ref
