(* Lifecycle hooks system — broadly compatible with Claude Code and Codex hook
   conventions.

   Events: PreToolUse, PostToolUse, PostToolUseFailure, UserPromptSubmit, Stop,
   SessionStart, SessionEnd, PreCompact, PostCompact, OnError.

   Hook handlers: command (shell), http (POST to URL).

   Exit code semantics (matching Claude Code):
   - 0: success (stdout may contain JSON with decisions/context)
   - 2: block (stderr sent to agent, operation stopped)
   - other: error (stderr shown to user, execution continues)

   JSON payload on stdin follows Claude Code/Codex shape:
   {tool_name, tool_input, tool_response, session_id, cwd, workspace, ...} *)

(* ---- Event types ---- *)

type hook_event =
  | PreToolUse
  | PostToolUse
  | PostToolUseFailure
  | UserPromptSubmit
  | Stop
  | SessionStart
  | SessionEnd
  | PreCompact
  | PostCompact
  | OnError

let hook_event_to_string = function
  | PreToolUse -> "PreToolUse"
  | PostToolUse -> "PostToolUse"
  | PostToolUseFailure -> "PostToolUseFailure"
  | UserPromptSubmit -> "UserPromptSubmit"
  | Stop -> "Stop"
  | SessionStart -> "SessionStart"
  | SessionEnd -> "SessionEnd"
  | PreCompact -> "PreCompact"
  | PostCompact -> "PostCompact"
  | OnError -> "OnError"

let hook_event_of_string s =
  match s with
  | "PreToolUse" -> Some PreToolUse
  | "PostToolUse" -> Some PostToolUse
  | "PostToolUseFailure" -> Some PostToolUseFailure
  | "UserPromptSubmit" -> Some UserPromptSubmit
  | "Stop" -> Some Stop
  | "SessionStart" -> Some SessionStart
  | "SessionEnd" -> Some SessionEnd
  | "PreCompact" -> Some PreCompact
  | "PostCompact" -> Some PostCompact
  | "OnError" -> Some OnError
  | _ -> None

let all_hook_event_names =
  [
    "PreToolUse";
    "PostToolUse";
    "PostToolUseFailure";
    "UserPromptSubmit";
    "Stop";
    "SessionStart";
    "SessionEnd";
    "PreCompact";
    "PostCompact";
    "OnError";
  ]

(* Whether an event supports blocking (exit code 2 stops the operation) *)
let event_can_block = function
  | PreToolUse | UserPromptSubmit | Stop | PreCompact -> true
  | PostToolUse | PostToolUseFailure | SessionStart | SessionEnd | PostCompact
  | OnError ->
      false

(* ---- Handler types ---- *)

type handler_type = Command_handler | Http_handler

let handler_type_to_string = function
  | Command_handler -> "command"
  | Http_handler -> "http"

let handler_type_of_string s =
  match s with
  | "command" -> Some Command_handler
  | "http" -> Some Http_handler
  | _ -> None

(* ---- Hook configuration ---- *)

type hook_handler = {
  handler_type : handler_type;
  command : string;
  (* For command: shell command to execute. For http: URL to POST to. *)
  timeout : float;
  (* Maximum execution time in seconds. Default: 30.0 *)
  async_flag : bool;
  (* If true, fire-and-forget (don't block execution). *)
  headers : (string * string) list;
  (* For http handlers: headers to send. *)
  allowed_env_vars : string list;
      (* For http handlers: env vars allowed in header interpolation. *)
}
[@@warning "-69"]

type hook_entry = {
  matcher : string option;
  (* Regex or tool-name pattern to match (e.g. "Bash|Write|Edit"). None
     matches all tools. *)
  hooks : hook_handler list; (* Handlers to execute when matcher matches. *)
}
[@@warning "-69"]

type hook_config = { event : hook_event; entries : hook_entry list }
[@@warning "-69"]

(* ---- Hook execution result ---- *)

type hook_decision = Hook_allow | Hook_block of string | Hook_ask of string

type hook_json_output = {
  decision : hook_decision option;
  (* permissionDecision: allow/deny/ask *)
  decision_reason : string option;
  additional_context : string option;
  (* Text to inject into agent context *)
  updated_input : Yojson.Safe.t option;
      (* For PreToolUse: modified tool parameters *)
}

type hook_result = {
  exit_code : int;
  stdout_text : string;
  stderr_text : string;
  json_output : hook_json_output option;
  timed_out : bool;
}
[@@warning "-69"]

(* ---- Aggregated result from all hooks for an event ---- *)

type dispatch_result = {
  blocked : bool;
  block_reason : string option;
  additional_contexts : string list;
  updated_input : Yojson.Safe.t option;
  errors : string list;
}
[@@warning "-69"]

let empty_dispatch_result =
  {
    blocked = false;
    block_reason = None;
    additional_contexts = [];
    updated_input = None;
    errors = [];
  }

(* ---- Config parsing ---- *)

let default_timeout = 30.0
let max_timeout = 300.0

let parse_hook_handler (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let handler_type =
    let raw = try json |> member "type" |> to_string with _ -> "command" in
    match handler_type_of_string raw with
    | Some t -> t
    | None ->
        Logs.warn (fun m ->
            m "Hooks: unknown handler type '%s', defaulting to command" raw);
        Command_handler
  in
  let command =
    try json |> member "command" |> to_string
    with _ -> (
      match handler_type with
      | Http_handler -> ( try json |> member "url" |> to_string with _ -> "")
      | Command_handler -> "")
  in
  let timeout =
    try
      let t = json |> member "timeout" |> to_number in
      if t > max_timeout then begin
        Logs.warn (fun m ->
            m "Hooks: timeout %.0f exceeds max %.0f, clamping" t max_timeout);
        max_timeout
      end
      else if t <= 0.0 then default_timeout
      else t
    with _ -> default_timeout
  in
  let async_flag = try json |> member "async" |> to_bool with _ -> false in
  let headers =
    try
      json |> member "headers" |> to_assoc
      |> List.map (fun (k, v) -> (k, to_string v))
    with _ -> []
  in
  let allowed_env_vars =
    try json |> member "allowedEnvVars" |> to_list |> List.map to_string
    with _ -> []
  in
  { handler_type; command; timeout; async_flag; headers; allowed_env_vars }

let parse_hook_entry (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let matcher =
    try
      let m = json |> member "matcher" |> to_string in
      if m = "" || m = "*" then None else Some m
    with _ -> None
  in
  let hooks =
    try json |> member "hooks" |> to_list |> List.map parse_hook_handler
    with _ -> []
  in
  { matcher; hooks }

let parse_event_hooks (event_name : string) (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  match hook_event_of_string event_name with
  | None ->
      Logs.warn (fun m -> m "Hooks: unknown event '%s', skipping" event_name);
      None
  | Some event ->
      let entries =
        try json |> member event_name |> to_list |> List.map parse_hook_entry
        with _ -> []
      in
      if entries = [] then None else Some { event; entries }

(* Parse a config.json "hooks" section (Claude Code style):
   {"hooks": {"PreToolUse": [...], "PostToolUse": [...]}} *)
let parse_hooks_config_section (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  try
    let hooks_obj = json |> member "hooks" in
    hooks_obj |> to_assoc
    |> List.filter_map (fun (event_name, event_json) ->
        (* For each event key, the value is an array of hook entries *)
        let wrapped = `Assoc [ (event_name, event_json) ] in
        parse_event_hooks event_name wrapped)
  with _ -> []

(* Parse a .clawq/hooks.json file (Codex style):
   Events at root level: {"PreToolUse": [...], "PostToolUse": [...]} *)
let parse_hooks_file (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  try
    json |> to_assoc
    |> List.filter_map (fun (event_name, _event_json) ->
        let wrapped = `Assoc [ (event_name, json |> member event_name) ] in
        parse_event_hooks event_name wrapped)
  with _ -> []

(* ---- Validation ---- *)

let validate_hook_config (hc : hook_config) =
  let errors = ref [] in
  let add_err msg = errors := msg :: !errors in
  List.iteri
    (fun i (entry : hook_entry) ->
      List.iteri
        (fun j (handler : hook_handler) ->
          if handler.command = "" then
            add_err
              (Printf.sprintf "%s[%d].hooks[%d]: empty command/url"
                 (hook_event_to_string hc.event)
                 i j);
          if handler.timeout <= 0.0 then
            add_err
              (Printf.sprintf "%s[%d].hooks[%d]: invalid timeout %.1f"
                 (hook_event_to_string hc.event)
                 i j handler.timeout);
          if handler.timeout > max_timeout then
            add_err
              (Printf.sprintf "%s[%d].hooks[%d]: timeout exceeds max %.0f"
                 (hook_event_to_string hc.event)
                 i j max_timeout))
        entry.hooks)
    hc.entries;
  !errors

let validate_all_hooks (hooks : hook_config list) =
  List.concat_map validate_hook_config hooks

(* ---- Matching ---- *)

let matches_tool_name (matcher : string) tool_name =
  if matcher = "" || matcher = "*" then true
  else
    (* B805: Support pipe-separated alternatives and glob-style wildcards.
       Patterns may contain '*' as a wildcard suffix (e.g. "Bash*" matches
       "Bash" and "Bash_output"). Exact match takes priority. *)
    let patterns = String.split_on_char '|' matcher in
    List.exists
      (fun p ->
        let p = String.trim p in
        if p = "" || p = "*" then true
        else if String.ends_with ~suffix:"*" p then
          let prefix = String.sub p 0 (String.length p - 1) in
          String.starts_with ~prefix tool_name
        else p = tool_name)
      patterns

let hooks_for_event (all_hooks : hook_config list) (event : hook_event)
    ?tool_name () =
  let matching_configs =
    List.filter (fun (hc : hook_config) -> hc.event = event) all_hooks
  in
  match tool_name with
  | None -> matching_configs
  | Some name ->
      List.map
        (fun (hc : hook_config) ->
          {
            hc with
            entries =
              List.filter
                (fun (entry : hook_entry) ->
                  match entry.matcher with
                  | None -> true
                  | Some m -> matches_tool_name m name)
                hc.entries;
          })
        matching_configs

(* ---- JSON payload construction ---- *)

let build_tool_payload ~(tool_name : string) ~(tool_input : Yojson.Safe.t)
    ?(tool_response : Yojson.Safe.t option) ~session_id ~cwd ?workspace () =
  let fields =
    [
      ("tool_name", `String tool_name);
      ("tool_input", tool_input);
      ("session_id", `String session_id);
      ("cwd", `String cwd);
    ]
  in
  let fields =
    match tool_response with
    | Some resp -> ("tool_response", resp) :: fields
    | None -> fields
  in
  let fields =
    match workspace with
    | Some ws -> ("workspace", `String ws) :: fields
    | None -> fields
  in
  `Assoc fields

let build_prompt_payload ~(prompt : string) ~session_id ~cwd ?workspace () =
  let fields =
    [
      ("prompt", `String prompt);
      ("session_id", `String session_id);
      ("cwd", `String cwd);
    ]
  in
  let fields =
    match workspace with
    | Some ws -> ("workspace", `String ws) :: fields
    | None -> fields
  in
  `Assoc fields

let build_session_payload ~session_id ~cwd ?workspace ?reason () =
  let fields = [ ("session_id", `String session_id); ("cwd", `String cwd) ] in
  let fields =
    match workspace with
    | Some ws -> ("workspace", `String ws) :: fields
    | None -> fields
  in
  let fields =
    match reason with
    | Some r -> ("reason", `String r) :: fields
    | None -> fields
  in
  `Assoc fields

let build_error_payload ~(error_type : string) ~(error_message : string)
    ~session_id ~cwd ?workspace () =
  let fields =
    [
      ("error_type", `String error_type);
      ("error_message", `String error_message);
      ("session_id", `String session_id);
      ("cwd", `String cwd);
    ]
  in
  let fields =
    match workspace with
    | Some ws -> ("workspace", `String ws) :: fields
    | None -> fields
  in
  `Assoc fields

(* ---- Parse hook JSON output from stdout ---- *)

let parse_hook_json_output (stdout_text : string) =
  let open Yojson.Safe.Util in
  match stdout_text with
  | "" | "\n" -> None
  | _ -> (
      try
        let json = Yojson.Safe.from_string stdout_text in
        let decision =
          try
            let raw = json |> member "decision" |> to_string in
            match raw with
            | "block" ->
                Some
                  (Hook_block
                     (try json |> member "reason" |> to_string
                      with _ -> "blocked by hook"))
            | "ask" ->
                Some
                  (Hook_ask
                     (try json |> member "reason" |> to_string
                      with _ -> "hook requests user confirmation"))
            | "allow" -> Some Hook_allow
            | _ -> None
          with _ -> (
            (* Try hookSpecificOutput.permissionDecision *)
            try
              let hso = json |> member "hookSpecificOutput" in
              let raw = hso |> member "permissionDecision" |> to_string in
              match raw with
              | "deny" ->
                  Some
                    (Hook_block
                       (try
                          hso |> member "permissionDecisionReason" |> to_string
                        with _ -> "denied by hook"))
              | "ask" ->
                  Some
                    (Hook_ask
                       (try
                          hso |> member "permissionDecisionReason" |> to_string
                        with _ -> "hook requests confirmation"))
              | "allow" -> Some Hook_allow
              | _ -> None
            with _ -> None)
        in
        let decision_reason =
          try Some (json |> member "reason" |> to_string)
          with _ -> (
            try
              Some
                (json
                |> member "hookSpecificOutput"
                |> member "permissionDecisionReason"
                |> to_string)
            with _ -> None)
        in
        let additional_context =
          try Some (json |> member "additionalContext" |> to_string)
          with _ -> (
            try
              Some
                (json
                |> member "hookSpecificOutput"
                |> member "additionalContext" |> to_string)
            with _ -> None)
        in
        let updated_input =
          let value =
            let top_level = json |> member "updatedInput" in
            if top_level <> `Null then top_level
            else
              try json |> member "hookSpecificOutput" |> member "updatedInput"
              with _ -> `Null
          in
          if value = `Null then None else Some value
        in
        Some { decision; decision_reason; additional_context; updated_input }
      with _ -> None)

(* ---- Aggregate dispatch results ---- *)

let aggregate_results (results : hook_result list) =
  List.fold_left
    (fun (acc : dispatch_result) (r : hook_result) ->
      if r.timed_out then
        { acc with errors = ("hook timed out" : string) :: acc.errors }
      else begin
        let json = r.json_output in
        let blocked, block_reason =
          (* Exit code 2 = block *)
          if r.exit_code = 2 then
            ( true,
              if r.stderr_text <> "" then Some r.stderr_text
              else
                match json with
                | Some j -> (
                    match j.decision_reason with
                    | Some r -> Some r
                    | None -> Some "blocked by hook")
                | None -> Some "blocked by hook" )
          else
            (* Check JSON decision for block *)
            match json with
            | Some ({ decision = Some (Hook_block reason) } as j) ->
                ( true,
                  match j.decision_reason with
                  | Some r -> Some r
                  | None -> Some reason )
            | _ -> (acc.blocked, acc.block_reason)
        in
        let additional_contexts =
          match json with
          | Some { additional_context = Some ctx } ->
              ctx :: acc.additional_contexts
          | _ -> acc.additional_contexts
        in
        let updated_input =
          match json with
          | Some { updated_input = Some ui } -> Some ui
          | _ -> acc.updated_input
        in
        let errors =
          if r.exit_code <> 0 && r.exit_code <> 2 && r.stderr_text <> "" then
            r.stderr_text :: acc.errors
          else acc.errors
        in
        { blocked; block_reason; additional_contexts; updated_input; errors }
      end)
    empty_dispatch_result results
