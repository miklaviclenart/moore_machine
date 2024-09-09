(* Enake definicije kot na predavanjih. *)
type t = { label : string }

let from_string label = { label }
let to_string { label } = label
