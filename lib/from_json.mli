val from_file : string -> Yojson.Basic.t
val name : Yojson.Basic.t -> string
val states : Yojson.Basic.t -> State.t list
val initial_state : Yojson.Basic.t -> State.t
val accepting_states : Yojson.Basic.t -> State.t list
val transitions : Yojson.Basic.t -> (State.t * char * State.t) list
val outputs : Yojson.Basic.t -> (State.t * string) list
val machine_from_json : string -> Machine.t