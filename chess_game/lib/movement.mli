(** [valid_move  piece start_row start_col end_row end_col state] is true if the
    move made by a player is allowed. Does not account for castling. Currently
    does not account for checks*)
val valid_move : int -> int -> int -> int -> bool

(** [is_valid_castle start_row start_col end_row end_col state] is true if
    player inputs a valid castle. *)
val is_valid_castle : int -> int -> int -> int -> bool

(** [castle start_row start_col end_row end_col] is the string representation of
    what type of castle was made. For example "wksc" stands for a white king
    side castle. *)

(** [type_castle start_row start_col end_row end_col] is the string
    representation of what type of castle was made. For example "wksc" stands
    for a white king side castle. *)
val type_castle : int -> int -> int -> int -> string

(**[is_capture start_row start_col end_row end_col] is true if the move made
   captured a piece and false otherwise*)
val is_capture :
  string -> int -> int -> int -> int -> string option array array -> bool

(**[turn] contains ["W"] if it is white to move and ["B"] if it is black to move*)
val turn : string ref

(**[material piece] is the material value of [piece]*)
val material : string -> int

(** [piece_valid moves piece start_row start_col state] is a list of all of the
    legal moves a piece can move to on a board. *)
val piece_valid_moves :
  string -> int -> int -> string option array array -> (int * int) list

val update_captures : int -> int -> unit
val captured_W : string list ref
val captured_B : string list ref
val material_advantage : unit -> string * int
val state : string option array array
val piece_square_ind : int -> int -> string
val castle_state : int -> int -> int -> int -> unit
val promote : int -> int -> string -> unit
val update_state : int -> int -> int -> int -> unit
val has_piece : int -> int -> bool
