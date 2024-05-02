open Moore_machine

type ui_state =
  | OptionList
  | ShowMachine
  | ReadingString
  | ReadStringResult
  | IncorrectStringWarning

type model = {
  machine : Machine.t;
  machine_state : State.t;
  ui_state : ui_state;
}

type msg = ReadString of string | ChangeUI of ui_state | ReturnToInitialState

let read_string machine q string =
  let aux acc char =
    match acc with
    | None -> None
    | Some q -> Machine.transition_function machine q char
  in
  string |> String.to_seq |> Seq.fold_left aux (Some q)

let update model = function
  | ReadString str -> (
      match read_string model.machine model.machine_state str with
      | None -> { model with ui_state = IncorrectStringWarning }
      | Some machine_state ->
          { model with machine_state; ui_state = ReadStringResult })
  | ChangeUI ui_state -> { model with ui_state }
  | ReturnToInitialState ->
      {
        model with
        machine_state = Machine.initial_state model.machine;
        ui_state = OptionList;
      }

let rec print_options () =
  print_endline "1) izpiši avtomat";
  print_endline "2) beri znake";
  print_endline "3) nastavi na začetno stanje";
  print_string "> ";
  match read_line () with
  | "1" -> ChangeUI ShowMachine
  | "2" -> ChangeUI ReadingString
  | "3" -> ReturnToInitialState
  | _ ->
      print_endline "** VNESI 1, 2 ALI 3 **";
      print_options ()

let print_machine machine =
  let print_state state =
    let screen = State.to_string state in
    let screen =
      if state = Machine.initial_state machine then "-> " ^ screen else screen
    in
    let screen =
      if Machine.is_accepting_state machine state then screen ^ " +" else screen
    in
    print_endline screen
  in
  List.iter print_state (Machine.state_list machine)

let read_string _model =
  print_string "Vnesi niz > ";
  let str = read_line () in
  ReadString str

let print_result model =
  if Machine.is_accepting_state model.machine model.machine_state then
    print_endline "Niz je bil sprejet"
  else print_endline "Niz ni bil sprejet"

let view model =
  match model.ui_state with
  | OptionList -> print_options ()
  | ShowMachine ->
      print_machine model.machine;
      ChangeUI OptionList
  | ReadingString -> read_string model
  | ReadStringResult ->
      print_result model;
      ChangeUI OptionList
  | IncorrectStringWarning ->
      print_endline "Niz ni veljaven";
      ChangeUI OptionList

let init machine =
  {
    machine;
    machine_state = Machine.initial_state machine;
    ui_state = OptionList;
  }

let ones_1mod3 =
  let q0 = State.from_string "q0"
  and q1 = State.from_string "q1"
  and q2 = State.from_string "q2" in
  Machine.empty_machine q0 '0'
  |> Machine.add_accepting_state q1
  |> Machine.add_accepting_state q2
  |> Machine.add_transition q0 '0' q0
  |> Machine.add_transition q1 '0' q1
  |> Machine.add_transition q2 '0' q2
  |> Machine.add_transition q0 '1' q1
  |> Machine.add_transition q1 '1' q2
  |> Machine.add_transition q2 '1' q0

let rec loop model =
  let msg = view model in
  let model' = update model msg in
  loop model'

let _ = loop (init ones_1mod3)