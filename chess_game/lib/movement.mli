(** [valid_move  piece start_row start_col end_row end_col state] is true if the
    move made by a player is allowed. Does not account for castling. Currently
    does not account for checks*)
val valid_move : int -> int -> int -> int -> bool

(** [is_valid_castle start_row start_col end_row end_col state] is true if
    player inputs a valid castle. *)
val is_valid_castle : int -> int -> int -> int -> bool

(** [type_castle start_row start_col end_row end_col] is the string
    representation of what type of castle was made. For example "wksc" stands
    for a white king side castle. *)
val type_castle : int -> int -> int -> int -> string

(**[turn] contains ["W"] if it is white to move and ["B"] if it is black to move*)
val turn : string ref

(**[material piece] is the material value of [piece]*)
val material : string -> int

(** [piece_valid moves piece start_row start_col state] is a list of all of the
    legal moves a piece can move to on a board. *)
val piece_valid_moves : string -> int -> int -> (int * int) list

(** [update_captures row col] adds the piece at [row, col] to the list of
    captured pieces*)
val update_captures : int -> int -> unit

(**[captured_W] contains the list of all captured white pieces*)
val captured_W : string list ref

(**[captured_B] contains the list of all captured white pieces*)
val captured_B : string list ref

(**[material_advantage ()] is a tuple with first element being the winning side
   and second element the material advantage*)
val material_advantage : unit -> string * int

(**[piece_at row col] is the name of the piece at [(row, col)] if there is a
   piece there and [""] otherwise*)
val piece_at : int -> int -> string

(**[castle_state color k rook_start rook_end] performs a castle for [color] on
   the side matching the rook locations*)
val castle_state : int -> int -> int -> int -> unit

(**[promote row col piece] promotes the pawn at [(row,col)] to [piece]*)
val promote : int -> int -> string -> unit

(**[update_state row0 col0 row1 rol1] moves the piece at [(row0,col0)] to
   [(row1,col1)]*)
val update_state : int -> int -> int -> int -> unit

(**[has_piece row col] is true if there is a piece at [(row, col)] and false
   otherwise*)
val has_piece : int -> int -> bool

val is_enpassant : int -> int -> int -> int -> bool
