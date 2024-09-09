(* Enake definicije kot na predavanjih. *)
type t = { string : string; index_of_current_char : int }

let from_string string = { string; index_of_current_char = 0 }
let empty = from_string ""
let current_char tape = String.get tape.string tape.index_of_current_char
let is_at_end tape = String.length tape.string = tape.index_of_current_char

let move_forward tape =
  { tape with index_of_current_char = succ tape.index_of_current_char }

let to_string tape = tape.string
let read_chars tape = String.sub tape.string 0 tape.index_of_current_char

let unread_chars tape =
  String.sub tape.string tape.index_of_current_char
    (String.length tape.string - tape.index_of_current_char)
