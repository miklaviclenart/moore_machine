type t

val run : Machine.t -> Tape.t -> t
val machine : t -> Machine.t
val tape : t -> Tape.t
val state : t -> State.t
val output : t -> string option
val step_forward : t -> t option
val is_in_accepting_state : t -> bool
