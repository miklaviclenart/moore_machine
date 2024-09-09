(* Ta datoteka poskrbi za branje primerov iz `.json` datotek.
 * Kodo spodaj sem napisal s pomočjo https://dev.realworldocaml.org/json.html *)

(* Knjižnica Yojson za delo z `JSON` datotekami. *)
open Yojson.Basic.Util

let from_file name = Yojson.Basic.from_file name
let name json = json |> member "name" |> to_string

let states json =
  json |> member "states" |> to_list |> filter_string
  |> List.map State.from_string

let initial_state json =
  json |> member "initial_state" |> to_string |> State.from_string

let accepting_states json =
  json |> member "accepting_states" |> to_list |> filter_string
  |> List.map State.from_string

let transitions json =
  let transitions_json = json |> member "transitions" |> to_list in
  List.map
    (fun transition ->
      let from_state =
        transition |> member "from" |> to_string |> State.from_string
      in
      let input = transition |> member "input" |> to_string |> fun s -> s.[0] in
      let to_state =
        transition |> member "to" |> to_string |> State.from_string
      in
      (from_state, input, to_state))
    transitions_json

let outputs json =
  let outputs_json = json |> member "outputs" |> to_list in
  List.map
    (fun transition ->
      let state =
        transition |> member "state" |> to_string |> State.from_string
      in
      let output = transition |> member "output" |> to_string in
      (state, output))
    outputs_json

let machine_from_json file_name =
  let json = from_file file_name in
  let states = states json in
  let initial_state = initial_state json in
  let accepting_states = accepting_states json in
  let transitions = transitions json in
  let outputs = outputs json in
  Machine.build_machine states initial_state accepting_states transitions
    outputs
