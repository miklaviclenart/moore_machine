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

let print_state model =
  Printf.printf "Trenutno stanje: %s\n" (State.to_string model.machine_state);
  match Machine.output_function model.machine model.machine_state with
  | None -> ()
  | Some s -> Printf.printf "Izhod: %s\n\n" s

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

let print_machine m =
  let print_transitions_for_state state =
    let transitions =
      List.filter (fun (s, _, _) -> s = state) (Machine.transition_list m)
    in
    List.iter
      (fun (_, event, next_state) ->
        Printf.printf "        %c -> %s\n" event (State.to_string next_state))
      transitions
  in
  List.iter
    (fun state ->
      Printf.printf "Stanje: %s\n" (State.to_string state);
      if List.exists (fun s -> s = state) (Machine.accepting_state_list m) then
        Printf.printf "   - Je sprejemno stanje: DA\n"
      else Printf.printf "   - Je sprejemno stanje: NE\n";
      List.iter
        (fun (s, e) -> if s = state then Printf.printf "   - Izhod: %s\n" e)
        (Machine.output_list m);
      Printf.printf "   - Prehodi:\n";
      print_transitions_for_state state)
    (Machine.state_list m)

let read_string _model =
  print_string "Vnesi niz > ";
  let str = read_line () in
  ReadString str

let print_result model =
  if Machine.is_accepting_state model.machine model.machine_state then
    print_endline "Niz je bil sprejet\n"
  else print_endline "Niz ni bil sprejet\n"

let view model =
  match model.ui_state with
  | OptionList ->
      print_state model;
      print_options ()
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

let rec loop model =
  let msg = view model in
  let model' = update model msg in
  loop model'

let rec print_menu () =
  print_endline "1) zaženi avtomat";
  print_endline "2) pokaži shranjene avtomate";
  print_endline "3) izhod";
  print_string "> ";
  match read_line () with
  | "1" -> (
      print_endline "Vnesi ime avtomata";
      print_string "> ";
      let name = read_line () in
      try
        loop (init (From_json.machine_from_json ("examples/" ^ name ^ ".json")))
      with Sys_error _msg ->
        print_endline "Avtomat s tem imenom ne obstaja.";
        print_endline "Vnesite '2', da si ogledate že definirane intervale.";
        print_menu ())
  | "2" ->
      print_endline "";
      let files = Sys.readdir "examples" in
      Array.iter
        (fun file ->
          let filename = Filename.remove_extension file in
          Printf.printf "%s\n" filename)
        files;
      print_endline "";
      print_menu ()
  | "3" -> print_endline "Nasvidanje!"; exit 0
  | _ ->
      print_endline "** VNESI 1, 2 ALI 3 **";
      print_menu ()

let _ = print_menu ()
