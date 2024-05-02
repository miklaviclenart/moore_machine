type state = State.t

type t = {
  states : state list;
  initial_state : state;
  accepting_states : state list;
  transitions : (state * char * state) list;
  outputs : (state * char) list;
}

let empty_machine initial_state output =
  {
    states = [ initial_state ];
    initial_state;
    accepting_states = [];
    transitions = [];
    outputs = [ (initial_state, output) ];
  }

let add_nonaccepting_state state machine =
  { machine with states = state :: machine.states }

let add_accepting_state state machine =
  {
    machine with
    states = state :: machine.states;
    accepting_states = state :: machine.accepting_states;
  }

let add_transition state1 char state2 machine =
  { machine with transitions = (state1, char, state2) :: machine.transitions }

let add_output state char machine =
  { machine with outputs = (state, char) :: machine.outputs }

let transition_function machine state char =
  match
    List.find_opt
      (fun (state1, char', _state2) -> state1 = state && char = char')
      machine.transitions
  with
  | None -> None
  | Some (_, _, stanje2) -> Some stanje2

let output_function machine state =
  match List.find_opt (fun (state', _) -> state' = state) machine.outputs with
  | None -> failwith "Output is not defined for this state"
  | Some (_, char) -> char

let initial_state machine = machine.initial_state
let state_list machine = machine.states
let transition_list machine = machine.transitions
let accepting_state_list machine = machine.accepting_states
let output_list machine = machine.outputs
let is_accepting_state machine state = List.mem state machine.accepting_states

let read_string machine q string =
  let aux acc char =
    match acc with None -> None | Some q -> transition_function machine q char
  in
  string |> String.to_seq |> Seq.fold_left aux (Some q)
