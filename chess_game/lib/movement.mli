(** [is_valid_move piece start_row start_col end_row end_col state] is true if
    the move made by a player is allowed. Does not account for castling.
    Currently does not account for checks*)
val is_valid_move :
  string -> int -> int -> int -> int -> string option array array -> bool

(** [can_castle start_row start_col end_row end_col state] is true if player
    inputs a valid castle. *)
val can_castle : int -> int -> int -> int -> string option array array -> bool

(** [castle start_row start_col end_row end_col] is the string representation of
    what type of castle was made. For example "wksc" stands for a white king
    side castle. *)

(** [castle start_row start_col end_row end_col] is the string representation of
    what type of castle was made. For example "wksc" stands for a white king
    side castle. *)
val castle : int -> int -> int -> int -> string

(**[is_capture start_row start_col end_row end_col] is true if the move made
   captured a piece and false otherwise*)
val is_capture :
  string -> int -> int -> int -> int -> string option array array -> bool

(**[turn] contains ["W"] if it is white to move and ["B"] if it is black to move*)
val turn : string ref

(** [piece_valid moves piece start_row start_col state] is a list of all of the
    legal moves a piece can move to on a board. *)
val piece_valid_moves :
  string -> int -> int -> string option array array -> (int * int) list
