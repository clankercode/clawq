type risk_level = Low | Medium | High

type invoke_context = {
  session_key : string option;
  send_progress : (string -> unit Lwt.t) option;
  interrupt_check : (unit -> string option) option;
  inject_system_messages : (string list -> unit) option;
  effective_cwd : string option;
  request_cwd_change : (string -> bool -> unit) option;
}

type invoke_stream =
  ?context:invoke_context ->
  on_output_chunk:(string -> unit Lwt.t) ->
  Yojson.Safe.t ->
  string Lwt.t

type t = {
  name : string;
  description : string;
  parameters_schema : Yojson.Safe.t;
  invoke : ?context:invoke_context -> Yojson.Safe.t -> string Lwt.t;
  invoke_stream : invoke_stream option;
  risk_level : risk_level;
  deferred : bool;
}

let default_context =
  {
    session_key = None;
    send_progress = None;
    interrupt_check = None;
    inject_system_messages = None;
    effective_cwd = None;
    request_cwd_change = None;
  }

let validate_required_params (tool : t) (args : Yojson.Safe.t) :
    (unit, string) result =
  let open Yojson.Safe.Util in
  let required =
    try
      tool.parameters_schema |> member "required" |> to_list
      |> List.map to_string
    with _ -> []
  in
  let missing =
    List.filter
      (fun name ->
        match args with
        | `Assoc fields -> (
            match List.assoc_opt name fields with
            | None | Some `Null -> true
            | Some _ -> false)
        | _ -> true)
      required
  in
  match missing with
  | [] -> Ok ()
  | _ ->
      let example_parts = List.map (fun name -> name ^ "=\"...\"") required in
      let example = tool.name ^ "(" ^ String.concat ", " example_parts ^ ")" in
      Error
        (Printf.sprintf
           "Error: missing required parameter%s %s for tool %s. Example: %s"
           (if List.length missing > 1 then "s" else "")
           (String.concat ", " (List.map (fun n -> "'" ^ n ^ "'") missing))
           tool.name example)
