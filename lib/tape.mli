type t

val empty : t
val current_char : t -> char
val is_at_end : t -> bool
val move_forward : t -> t
val from_string : string -> t
val to_string : t -> string
val read_chars : t -> string
val unread_chars : t -> string
