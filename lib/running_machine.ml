type t = { machine : Machine.t; tape : Tape.t; state : State.t }

let run machine tape = { machine; tape; state = Machine.initial_state machine }
let machine { machine; _ } = machine
let tape { tape; _ } = tape
let state { state; _ } = state

(** [output machine] vrne opcijo izhoda [string option] avtomata [machine] v trenutnem stanju. *)
let output machine = Machine.output_function machine.machine machine.state

let step_forward { machine; tape; state } =
  if Tape.is_at_end tape then None
  else
    let state' =
      Machine.transition_function machine state (Tape.current_char tape)
    in
    match state' with
    | None -> None
    | Some state' ->
        Some { machine; tape = Tape.move_forward tape; state = state' }

let is_in_accepting_state { machine; state; _ } =
  Machine.is_accepting_state machine state
