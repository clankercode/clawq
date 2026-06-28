(* Session list/show/events argument parsing and message/JSON formatting.
   Split from command_bridge_helpers.ml; re-exported there via [include].
   Note: distinct from command_bridge_session.ml (the session command handlers). *)

type session_list_args = {
  channel : string option;
  prefix : string option;
  activity : Memory.session_activity;
  only_main : bool option;
  include_postmortem : bool;
}

let parse_session_list_args args =
  let rec loop state = function
    | [] -> Ok state
    | "--channel" :: value :: rest ->
        loop { state with channel = Some value } rest
    | "--prefix" :: value :: rest ->
        loop { state with prefix = Some value } rest
    | "--active" :: rest -> loop { state with activity = Memory.Active } rest
    | "--inactive" :: rest ->
        loop { state with activity = Memory.Inactive } rest
    | "--main" :: rest -> loop { state with only_main = Some true } rest
    | "--non-main" :: rest -> loop { state with only_main = Some false } rest
    | "--include-postmortem" :: rest ->
        loop { state with include_postmortem = true } rest
    | flag :: _ when String.length flag > 0 && flag.[0] = '-' ->
        Error (Printf.sprintf "Unknown session list flag: %s" flag)
    | _ ->
        Error
          "Usage: clawq session list [--channel NAME] [--prefix PREFIX] \
           [--active|--inactive] [--main|--non-main] [--include-postmortem]"
  in
  loop
    {
      channel = None;
      prefix = None;
      activity = Memory.Any;
      only_main = None;
      include_postmortem = false;
    }
    args

type session_show_args = {
  epoch : Memory.epoch_selector option;
  offset : int;
  limit : int option;
}

let parse_session_show_args args =
  let rec loop epoch offset limit = function
    | [] -> Ok { epoch; offset; limit }
    | "--epoch" :: "current" :: rest ->
        loop (Some Memory.Current) offset limit rest
    | "--epoch" :: value :: rest -> (
        match int_of_string_opt value with
        | Some id when id > 0 ->
            loop (Some (Memory.Archived id)) offset limit rest
        | _ -> Error (Printf.sprintf "Invalid epoch value: %s" value))
    | "--offset" :: value :: rest -> (
        match int_of_string_opt value with
        | Some n when n >= 0 -> loop epoch n limit rest
        | _ -> Error (Printf.sprintf "Invalid offset value: %s" value))
    | "--limit" :: value :: rest -> (
        match int_of_string_opt value with
        | Some n when n > 0 -> loop epoch offset (Some n) rest
        | _ -> Error (Printf.sprintf "Invalid limit value: %s" value))
    | flag :: _ when String.length flag > 0 && flag.[0] = '-' ->
        Error (Printf.sprintf "Unknown session show flag: %s" flag)
    | _ ->
        Error
          "Usage: clawq session show SESSION [--epoch current|ID] [--offset N] \
           [--limit N]"
  in
  loop None 0 None args

(* B235: session events helpers *)
let string_contains = String_util.string_contains

type session_events_args = {
  ev_epoch : Memory.epoch_selector option;
  ev_type : string option;
}

let parse_session_events_args args =
  let rec loop epoch filter_type = function
    | [] -> Ok { ev_epoch = epoch; ev_type = filter_type }
    | "--epoch" :: "current" :: rest ->
        loop (Some Memory.Current) filter_type rest
    | "--epoch" :: value :: rest -> (
        match int_of_string_opt value with
        | Some id when id > 0 ->
            loop (Some (Memory.Archived id)) filter_type rest
        | _ -> Error (Printf.sprintf "Invalid epoch value: %s" value))
    | "--type" :: value :: rest -> loop epoch (Some value) rest
    | flag :: _ when String.length flag > 0 && flag.[0] = '-' ->
        Error (Printf.sprintf "Unknown session events flag: %s" flag)
    | _ ->
        Error
          "Usage: clawq session events SESSION [--epoch current|ID] [--type \
           TYPE]"
  in
  loop None None args

let classify_event_message (row : Memory.raw_message) =
  match row.role with
  | "event" ->
      if string_contains row.content "workspace context refreshed" then
        "workspace_refresh"
      else "unknown_event"
  | "system" ->
      if string_contains row.content "Relevant context from memory:" then
        "memory_context"
      else "attachment"
  | "assistant" ->
      if string_contains row.content "[Conversation history compacted]" then
        "compaction"
      else "other"
  | role -> role

let is_session_event_row (row : Memory.raw_message) =
  row.role = "event" || row.role = "system"
  || row.role = "assistant"
     && string_contains row.content "[Conversation history compacted]"

let string_or_null = function Some value -> `String value | None -> `Null

let session_show_system_prompt config =
  Prompt_builder.build ~config ~tool_registry:None ()

let session_show_active_workspace_file config filename =
  List.mem filename config.Runtime_config.prompt.workspace_files

let session_show_active_workspace_path config path =
  let workspace = Runtime_config.effective_workspace config in
  let resolved =
    if Filename.is_relative path then Filename.concat workspace path else path
  in
  let normalized = Path_util.normalize_path resolved in
  List.exists
    (fun filename ->
      Path_util.normalize_path (Filename.concat workspace filename) = normalized)
    config.Runtime_config.prompt.workspace_files

let session_show_shell_command_targets_active_workspace_file config command =
  let workspace = Runtime_config.effective_workspace config in
  List.exists
    (fun filename ->
      let abs_path =
        Path_util.normalize_path (Filename.concat workspace filename)
      in
      List.exists
        (fun needle ->
          let needle_len = String.length needle in
          needle_len > 0
          && String.length command >= needle_len
          &&
          let rec loop i =
            if i + needle_len > String.length command then false
            else if String.sub command i needle_len = needle then true
            else loop (i + 1)
          in
          loop 0)
        [ filename; Filename.basename filename; "./" ^ filename; abs_path ])
    config.Runtime_config.prompt.workspace_files

let redact_tool_call_arguments_for_session_show ~config ~function_name arguments
    =
  let open Yojson.Safe.Util in
  try
    let args = Yojson.Safe.from_string arguments in
    let redacted json = Some (Yojson.Safe.to_string json) in
    match function_name with
    | "doc_write" ->
        let filename = args |> member "filename" |> to_string in
        if session_show_active_workspace_file config filename then
          redacted
            (`Assoc
               [
                 ("filename", `String filename);
                 ("content", `String "[redacted]");
               ])
        else None
    | "file_write" | "file_append" ->
        let path = args |> member "path" |> to_string in
        if session_show_active_workspace_path config path then
          redacted
            (`Assoc
               [ ("path", `String path); ("content", `String "[redacted]") ])
        else None
    | "file_edit" ->
        let path = args |> member "path" |> to_string in
        if session_show_active_workspace_path config path then
          redacted
            (`Assoc
               [
                 ("path", `String path);
                 ("old_text", `String "[redacted]");
                 ("new_text", `String "[redacted]");
                 ( "replace_all",
                   match args |> member "replace_all" with
                   | `Bool b -> `Bool b
                   | _ -> `Bool false );
               ])
        else None
    | "file_edit_lines" ->
        let path = args |> member "path" |> to_string in
        if session_show_active_workspace_path config path then
          redacted
            (`Assoc
               [
                 ("path", `String path);
                 ( "start_line",
                   match args |> member "start_line" with
                   | `Int n -> `Int n
                   | _ -> `Null );
                 ( "end_line",
                   match args |> member "end_line" with
                   | `Int n -> `Int n
                   | _ -> `Null );
                 ("content", `String "[redacted]");
               ])
        else None
    | "shell_exec" ->
        let command = args |> member "command" |> to_string in
        if
          session_show_shell_command_targets_active_workspace_file config
            command
        then
          redacted
            (`Assoc
               [
                 ("command", `String "[redacted]");
                 ( "cwd",
                   match args |> member "cwd" with
                   | `String cwd -> `String cwd
                   | _ -> `Null );
               ])
        else None
    | _ -> None
  with _ -> None

let sanitize_tool_calls_json_for_session_show ~config = function
  | None -> None
  | Some tool_calls_json -> (
      try
        let json = Yojson.Safe.from_string tool_calls_json in
        let sanitized =
          match json with
          | `List calls ->
              `List
                (List.map
                   (function
                     | `Assoc fields as call ->
                         let function_name =
                           match List.assoc_opt "function_name" fields with
                           | Some (`String name) -> Some name
                           | _ -> None
                         in
                         let arguments =
                           match List.assoc_opt "arguments" fields with
                           | Some (`String args) -> Some args
                           | _ -> None
                         in
                         begin match (function_name, arguments) with
                         | Some name, Some args -> (
                             match
                               redact_tool_call_arguments_for_session_show
                                 ~config ~function_name:name args
                             with
                             | Some redacted_arguments ->
                                 `Assoc
                                   (("arguments", `String redacted_arguments)
                                   :: List.remove_assoc "arguments" fields)
                             | None -> call)
                         | _ -> call
                         end
                     | other -> other)
                   calls)
          | other -> other
        in
        Some (Yojson.Safe.to_string sanitized)
      with _ -> Some tool_calls_json)

let sanitize_provider_response_items_json_for_session_show ~config =
  let is_complete_json s =
    try
      ignore (Yojson.Safe.from_string s);
      true
    with _ -> false
  in
  let rec sanitize json =
    match json with
    | `List items -> `List (List.map sanitize items)
    | `Assoc fields ->
        let item_type =
          match List.assoc_opt "type" fields with
          | Some (`String value) -> Some value
          | _ -> None
        in
        let function_name =
          match List.assoc_opt "name" fields with
          | Some (`String value) -> Some value
          | _ -> None
        in
        let fields =
          match
            (item_type, function_name, List.assoc_opt "arguments" fields)
          with
          | Some ("function_call" | "tool_call"), Some name, Some (`String args)
            -> (
              match
                redact_tool_call_arguments_for_session_show ~config
                  ~function_name:name args
              with
              | Some redacted_arguments ->
                  ("arguments", `String redacted_arguments)
                  :: List.remove_assoc "arguments" fields
              | None ->
                  if is_complete_json args then fields
                  else
                    ("arguments", `String "[redacted]")
                    :: List.remove_assoc "arguments" fields)
          | _ -> fields
        in
        let fields =
          match (item_type, function_name, List.assoc_opt "input" fields) with
          | Some "tool_use", Some name, Some input -> (
              let args = Yojson.Safe.to_string input in
              match
                redact_tool_call_arguments_for_session_show ~config
                  ~function_name:name args
              with
              | Some redacted_args -> (
                  try
                    ("input", Yojson.Safe.from_string redacted_args)
                    :: List.remove_assoc "input" fields
                  with _ -> fields)
              | None -> fields)
          | _ -> fields
        in
        let fields =
          match List.assoc_opt "function" fields with
          | Some (`Assoc fn_fields) -> (
              let name =
                match List.assoc_opt "name" fn_fields with
                | Some (`String value) -> Some value
                | _ -> None
              in
              let arguments =
                match List.assoc_opt "arguments" fn_fields with
                | Some (`String value) -> Some value
                | _ -> None
              in
              match (name, arguments) with
              | Some name, Some args -> (
                  match
                    redact_tool_call_arguments_for_session_show ~config
                      ~function_name:name args
                  with
                  | Some redacted_args ->
                      let fn_fields =
                        ("arguments", `String redacted_args)
                        :: List.remove_assoc "arguments" fn_fields
                      in
                      ("function", `Assoc fn_fields)
                      :: List.remove_assoc "function" fields
                  | None ->
                      if is_complete_json args then fields
                      else
                        let fn_fields =
                          ("arguments", `String "[redacted]")
                          :: List.remove_assoc "arguments" fn_fields
                        in
                        ("function", `Assoc fn_fields)
                        :: List.remove_assoc "function" fields)
              | None, Some _args ->
                  let fn_fields =
                    ("arguments", `String "[redacted]")
                    :: List.remove_assoc "arguments" fn_fields
                  in
                  ("function", `Assoc fn_fields)
                  :: List.remove_assoc "function" fields
              | _ -> fields)
          | _ -> fields
        in
        let fields =
          List.map
            (fun (k, v) ->
              if k = "partial_json" then (k, `String "[redacted]")
              else if k = "data_raw" then
                match v with
                | `String raw -> (
                    try
                      let redacted = sanitize (Yojson.Safe.from_string raw) in
                      (k, `String (Yojson.Safe.to_string redacted))
                    with _ -> (k, v))
                | _ -> (k, sanitize v)
              else (k, sanitize v))
            fields
        in
        `Assoc fields
    | other -> other
  in
  function
  | None -> None
  | Some provider_response_items_json -> (
      try
        Some
          (Yojson.Safe.to_string
             (sanitize (Yojson.Safe.from_string provider_response_items_json)))
      with _ -> Some provider_response_items_json)

let raw_message_json config index (row : Memory.raw_message) =
  `Assoc
    [
      ("index", `Int index);
      ("id", `Int row.id);
      ("role", `String row.role);
      ("content", `String row.content);
      ("tool_call_id", string_or_null row.tool_call_id);
      ("tool_name", string_or_null row.tool_name);
      ( "tool_calls_json",
        string_or_null
          (sanitize_tool_calls_json_for_session_show ~config row.tool_calls_json)
      );
      ( "provider_response_items_json",
        string_or_null
          (sanitize_provider_response_items_json_for_session_show ~config
             row.provider_response_items_json) );
      ("created_at", `String row.created_at);
    ]

let session_epoch_json (epoch : Memory.session_epoch) =
  `Assoc
    [
      ( "epoch",
        match epoch.epoch_id with
        | Some id -> `Int id
        | None -> `String epoch.label );
      ("label", `String epoch.label);
      ("current", `Bool epoch.current);
      ("message_count", `Int epoch.message_count);
      ("first_message_at", string_or_null epoch.first_message_at);
      ("last_message_at", string_or_null epoch.last_message_at);
      ("recorded_at", string_or_null epoch.recorded_at);
    ]
