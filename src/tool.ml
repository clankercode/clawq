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

let extract_required_params (schema : Yojson.Safe.t) : (string * string) list =
  let open Yojson.Safe.Util in
  let required =
    try schema |> member "required" |> to_list |> List.map to_string
    with _ -> []
  in
  let properties =
    try schema |> member "properties" |> to_assoc with _ -> []
  in
  List.map
    (fun name ->
      let typ =
        try List.assoc name properties |> member "type" |> to_string
        with _ -> "any"
      in
      (name, typ))
    required

let format_required_params (params : (string * string) list) : string =
  match params with
  | [] -> ""
  | _ ->
      "Required parameters: "
      ^ String.concat ", "
          (List.map (fun (n, t) -> Printf.sprintf "%s (%s)" n t) params)

let format_example (tool_name : string) (params : (string * string) list) :
    string =
  let parts =
    List.map (fun (name, _) -> Printf.sprintf "%s=\"...\"" name) params
  in
  tool_name ^ "(" ^ String.concat ", " parts ^ ")"

let make_param_error ~tool_name ~parameters_schema ~detail =
  let params = extract_required_params parameters_schema in
  let req_info = format_required_params params in
  let example = format_example tool_name params in
  match req_info with
  | "" -> Printf.sprintf "Error: %s for tool %s." detail tool_name
  | _ ->
      Printf.sprintf "Error: %s for tool %s. %s. Example: %s" detail tool_name
        req_info example

let find_missing_required_params (tool : t) (args : Yojson.Safe.t) : string list
    =
  let open Yojson.Safe.Util in
  let required =
    try
      tool.parameters_schema |> member "required" |> to_list
      |> List.map to_string
    with _ -> []
  in
  List.filter
    (fun name ->
      match args with
      | `Assoc fields -> (
          match List.assoc_opt name fields with
          | None | Some `Null -> true
          | Some _ -> false)
      | _ -> true)
    required

let format_missing_required_error (tool : t) ~(missing : string list)
    ?(escalation_level = 0) () =
  let missing_str =
    String.concat ", " (List.map (fun n -> "'" ^ n ^ "'") missing)
  in
  let detail =
    Printf.sprintf "missing required parameter%s %s"
      (if List.length missing > 1 then "s" else "")
      missing_str
  in
  let base =
    make_param_error ~tool_name:tool.name
      ~parameters_schema:tool.parameters_schema ~detail
  in
  (* B622: escalate the error message when the model has repeated the same
     missing-param mistake. Level 0 = first occurrence: plain error plus a
     clear warning that repeating the same invalid call will abort the turn.
     Level 1 = second consecutive: final warning. Level >= 2 = third+: hard
     STOP/ABORT message so the model breaks out of the loop. *)
  match escalation_level with
  | 0 ->
      Printf.sprintf
        "%s\n\n\
         ACTION REQUIRED: Re-emit '%s' WITH %s included. Repeating the same \
         call without the required parameter(s) will abort this turn."
        base tool.name missing_str
  | 1 ->
      Printf.sprintf
        "%s\n\n\
         FINAL WARNING: This is the SECOND consecutive call to '%s' with the \
         same missing parameter(s) %s. The next identical call will abort this \
         turn. Include the required argument(s) or switch to a different tool."
        base tool.name missing_str
  | _ ->
      Printf.sprintf
        "%s\n\n\
         STOP / ABORT: '%s' has been called %d times in a row with the same \
         missing parameter(s) %s. The turn is being aborted now. Do NOT repeat \
         this call again — either include the required argument(s) or respond \
         in text."
        base tool.name (escalation_level + 1) missing_str

let validate_required_params (tool : t) (args : Yojson.Safe.t) :
    (unit, string) result =
  match find_missing_required_params tool args with
  | [] -> Ok ()
  | missing -> Error (format_missing_required_error tool ~missing ())
