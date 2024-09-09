type state = State.t

(** 
   Tip predstavlja končni avtomat, ki vključuje:
   - [states]: seznam stanj avtomata,
   - [initial_state]: začetno stanje,
   - [accepting_states]: seznam sprejemnih stanj,
   - [transitions]: seznam prehodov,
   - [outputs]: seznam izhodov.
*)
type t = {
  states : state list;
  initial_state : state;
  accepting_states : state list;
  transitions : (state * char * state) list;
  outputs : (state * string) list;
}

(** Prazen avtomat. *)
let empty_machine initial_state =
  {
    states = [ initial_state ];
    initial_state;
    accepting_states = [];
    transitions = [];
    outputs = [];
  }

(** [add_nonaccepting_state state machine] doda stanje [state] na seznam nesprejemnih stanj avtomata [machine]. *)
let add_nonaccepting_state state machine =
  if List.mem state machine.states then machine
  else { machine with states = state :: machine.states }

(** [add_accepting_state state machine] doda stanje [state] na seznam sprejemnih stanj avtomata [machine]. *)
let add_accepting_state state machine =
  if List.mem state machine.states && List.mem state machine.accepting_states
  then machine
  else if List.mem state machine.states then
    { machine with accepting_states = state :: machine.accepting_states }
  else
    {
      machine with
      states = state :: machine.states;
      accepting_states = state :: machine.accepting_states;
    }

(** [add_transition state1 char state2 machine] doda prehod [(state1, char, state2)] na seznam prehodov avtomata [machine].
  Če ima avtomat za vnešeno stanje in vhod že dodan prehod, ga funkcija izbriše in nadomesti z novim.
*)
let add_transition state1 char state2 machine =
  match
    List.find_opt
      (fun (state1', char', _state2') -> state1 = state1' && char = char')
      machine.transitions
  with
  | None ->
      {
        machine with
        transitions = (state1, char, state2) :: machine.transitions;
      }
  | Some (state1', char', state2') ->
      let new_transitions =
        List.filter
          (fun (s, c, s0) -> (state1', char', state2') <> (s, c, s0))
          machine.transitions
      in
      { machine with transitions = (state1, char, state2) :: new_transitions }

(**
  [add_output state string machine] doda izhod [(state, string)] v seznam prehodov avtomata [machine].
  Če ima vnešeno stanje že izhod, ga izbriše in zapiše vnešenega.
*)
let add_output state string machine =
  match
    List.find_opt (fun (state', _string) -> state = state') machine.outputs
  with
  | None -> { machine with outputs = (state, string) :: machine.outputs }
  | Some (state', string') ->
      let new_outputs =
        List.filter (fun (s, o) -> (state', string') <> (s, o)) machine.outputs
      in
      { machine with outputs = (state, string) :: new_outputs }

(**
    [transition_function machine state char] vrne stanje [state option] avtomata [machine], 
    v katerega bi prišli, če bi pri stanju [state] vnesli vhod [char]. V primeru težav vrne [None].
*)
let transition_function machine state char =
  match
    List.find_opt
      (fun (state1, char', _state2) -> state1 = state && char = char')
      machine.transitions
  with
  | None -> None
  | Some (_, _, state2) -> Some state2

(** [output_function machine state] vrne izhod [string option] avtomata [machine] pri stanju [state]. V primeru težav vrne [None]. *)
let output_function machine state =
  match List.find_opt (fun (state', _) -> state' = state) machine.outputs with
  | None -> None
  | Some (_, str) -> Some str

(* Pomožne funkcije za dostop do polj zapisnega tipa avtomata. *)
let initial_state machine = machine.initial_state
let state_list machine = machine.states
let transition_list machine = machine.transitions
let accepting_state_list machine = machine.accepting_states
let output_list machine = machine.outputs
let is_accepting_state machine state = List.mem state machine.accepting_states

(** [read_string machine q string] sprejme niz [string], ga razbije na zaporedje znakov in vrne stanje
[state option] v katerega bi prišli, če kot vhod podamo vse znake zaporedja. V primeru težav vrne [None]. *)
let read_string machine q string =
  let aux acc char =
    match acc with None -> None | Some q -> transition_function machine q char
  in
  string |> String.to_seq |> Seq.fold_left aux (Some q)

(** [build_machine states initial_state accepting_states transitions outputs] zgradi avtomat s podanimi lastnostmi. *)
let build_machine states initial_state accepting_states transitions outputs =
  { states; initial_state; accepting_states; transitions; outputs }
