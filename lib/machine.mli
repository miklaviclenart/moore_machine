type t

val empty_machine : State.t -> t
val add_nonaccepting_state : State.t -> t -> t
val add_accepting_state : State.t -> t -> t
val add_transition : State.t -> char -> State.t -> t -> t
val add_output : State.t -> string -> t -> t
val transition_function : t -> State.t -> char -> State.t option
val output_function : t -> State.t -> string option
val initial_state : t -> State.t
val state_list : t -> State.t list
val transition_list : t -> (State.t * char * State.t) list
val accepting_state_list : t -> State.t list
val output_list : t -> (State.t * string) list
val is_accepting_state : t -> State.t -> bool
val read_string : t -> State.t -> string -> State.t option
