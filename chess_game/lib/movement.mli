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
