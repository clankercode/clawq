open Tools_builtin_util
include Tools_builtin_fs
include Tools_builtin_memory

let is_path_allowed ~workspace ~workspace_only ~extra_allowed_paths path =
  if not workspace_only then true
  else is_path_within_allowed_roots ~workspace ~extra_allowed_paths path

(* B645: block direct writes into any `.backlog/` directory. The backlog is
   ID-allocated and indexed by the `bl` CLI; bypassing it via raw file_write
   risks ID collisions, format drift, and hallucinated entries (one teams
   agent did exactly this — wrote two versions of a phantom 'B605' bug with
   invented Anthropic policy text). Direct ad-hoc writes are not the right
   path; agents should use `shell_exec bl bug --simple ...` instead. *)
let backlog_write_error path =
  Printf.sprintf
    "Error: refusing to file_write into '%s'. The .backlog/ directory is \
     managed by the `bl` CLI which allocates IDs and keeps the index \
     consistent. Use `shell_exec` with one of:\n\
    \  - bl bug --simple \"<title>\" --body \"<body>\"  (for bug reports)\n\
    \  - bl idea \"<title>\"                            (for ideas/intake)\n\
    \  - bl edit <ID>                                  (to edit an existing \
     entry)\n\
     Run `bl --help` for the full command list."
    path

let path_targets_backlog path =
  (* Match any path that contains '/.backlog/' or starts with '.backlog/'. *)
  (* Normalize the path to catch relative path traversals like ../.backlog/ *)
  let normalized =
    let parts = String.split_on_char '/' path in
    let rec normalize acc = function
      | [] -> List.rev acc
      | "" :: rest -> normalize acc rest
      | "." :: rest -> normalize acc rest
      | ".." :: rest -> (
          match acc with
          | [] -> normalize [] rest
          | _ :: acc' -> normalize acc' rest)
      | x :: rest -> normalize (x :: acc) rest
    in
    String.concat "/" (normalize [] parts)
  in
  let check p =
    let needle = "/.backlog/" in
    let len_p = String.length p in
    let len_n = String.length needle in
    let rec scan i =
      if i + len_n > len_p then false
      else if String.sub p i len_n = needle then true
      else scan (i + 1)
    in
    scan 0 || (len_p >= 9 && String.sub p 0 9 = ".backlog/")
  in
  check path || check normalized

let file_read ~workspace ~workspace_only ~extra_allowed_paths =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "path",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Path to the file to read (required)" );
                  ] );
              ( "offset",
                `Assoc
                  [
                    ("type", `String "integer");
                    ( "description",
                      `String "Optional 1-indexed line offset for paged reads"
                    );
                  ] );
              ( "limit",
                `Assoc
                  [
                    ("type", `String "integer");
                    ( "description",
                      `String
                        "Optional max lines to read when using offset (default \
                         200, max 2000)" );
                  ] );
            ] );
        ("required", `List [ `String "path" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"file_read" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "file_read";
    description =
      "Read a file's text content. Full reads limited to 50,000 chars; for \
       larger files use offset and limit parameters to read in parts.";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path = try args |> member "path" |> to_string with _ -> "" in
        let offset_input = parse_optional_int_field args "offset" in
        let limit_input = parse_optional_int_field args "limit" in
        if path = "" then
          Lwt.return (param_err "parameter 'path' must be a non-empty string")
        else
          match (offset_input, limit_input) with
          | Error msg, _ | _, Error msg -> Lwt.return msg
          | Ok offset_opt, Ok limit_opt -> (
              let has_offset = offset_opt <> None in
              let has_limit = limit_opt <> None in
              let offset = Option.value offset_opt ~default:1 in
              let limit =
                Option.value limit_opt ~default:file_read_default_limit
              in
              match validate_file_read_window ~offset ~limit with
              | Error msg -> Lwt.return msg
              | Ok () ->
                  if
                    not
                      (is_path_allowed ~workspace ~workspace_only
                         ~extra_allowed_paths path)
                  then Lwt.return "Error: path is outside workspace"
                  else
                    let eff_ws =
                      effective_cwd_or_workspace ?context ~workspace ()
                    in
                    Lwt.catch
                      (fun () ->
                        let open Lwt.Syntax in
                        let path = resolve_path ~workspace:eff_ws path in
                        let canonical_path = canonicalize_for_read path in
                        match canonical_path with
                        | Error msg -> Lwt.return msg
                        | Ok canonical_path ->
                            if
                              not
                                (is_path_allowed ~workspace ~workspace_only
                                   ~extra_allowed_paths canonical_path)
                            then Lwt.return "Error: path is outside workspace"
                            else if
                              try Sys.is_directory canonical_path
                              with Sys_error _ -> false
                            then
                              let listing = format_dir_listing canonical_path in
                              Lwt.return
                                (Printf.sprintf
                                   "Note: '%s' is a directory, not a file. Use \
                                    `list_dir(path=\"%s\", show_hidden=false)` \
                                    to list directory contents.\n\n\
                                    Directory listing:\n\
                                    %s"
                                   path path listing)
                            else
                              let* content =
                                Lwt_io.with_file ~mode:Lwt_io.Input
                                  canonical_path Lwt_io.read
                              in
                              if has_offset || has_limit then
                                Lwt.return
                                  (format_lines_window ~content ~offset ~limit)
                              else if
                                String.length content > file_read_max_full_chars
                              then
                                Lwt.return
                                  (Printf.sprintf
                                     "File too large for full read (%d chars, \
                                      limit %d chars). Use file_read with \
                                      offset/limit to read in parts (for \
                                      example: offset=1, limit=200), or use \
                                      shell_exec with grep to search first."
                                     (String.length content)
                                     file_read_max_full_chars)
                              else Lwt.return content)
                      (fun exn ->
                        Lwt.return ("Error: " ^ Printexc.to_string exn))));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let file_append ~workspace ~workspace_only ~extra_allowed_paths =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "path",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Path to the file to append (required)" );
                  ] );
              ( "content",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Content to append to the file (required)" );
                  ] );
            ] );
        ("required", `List [ `String "path"; `String "content" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"file_append" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "file_append";
    description =
      "Append content to the end of a file, creating it if it does not exist. \
       Use this for log-like writes or accumulating notes. To overwrite the \
       whole file, use `file_write`. To insert/replace text at a specific \
       location, use `file_edit` (unique substring) or `file_edit_lines` (line \
       range).";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path = try args |> member "path" |> to_string with _ -> "" in
        let content =
          try args |> member "content" |> to_string with _ -> ""
        in
        if path = "" then
          Lwt.return (param_err "parameter 'path' must be a non-empty string")
        else if path_targets_backlog path then
          Lwt.return (backlog_write_error path)
        else if
          not
            (is_path_allowed ~workspace ~workspace_only ~extra_allowed_paths
               path)
        then Lwt.return "Error: path is outside workspace"
        else
          let eff_ws = effective_cwd_or_workspace ?context ~workspace () in
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let path = resolve_path ~workspace:eff_ws path in
              let* () =
                Lwt_io.with_file
                  ~flags:[ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ]
                  ~mode:Lwt_io.Output path (fun oc -> Lwt_io.write oc content)
              in
              Lwt.return
                (Printf.sprintf "Appended %d bytes to %s"
                   (String.length content) path))
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let shell_exec ~workspace ~workspace_only ~allowed_commands ~extra_allowed_paths
    ~sandbox =
  shell_exec_with_hooks ~workspace ~workspace_only ~allowed_commands
    ~extra_allowed_paths ~sandbox ()

let file_write ~workspace ~workspace_only ~extra_allowed_paths =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "path",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Path to the file to write (required)" );
                  ] );
              ( "content",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Content to write to the file (required)" );
                  ] );
            ] );
        ("required", `List [ `String "path"; `String "content" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"file_write" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "file_write";
    description =
      "Create or overwrite a file with the given content. Path must be \
       absolute and inside the workspace (workspace_only) or in an \
       extra_allowed_paths location. To MODIFY part of an existing file, \
       prefer `file_edit` (exact substring replace) or `file_edit_lines` \
       (line-range replace) — they fail loudly on a non-unique match instead \
       of silently clobbering. To ADD to the end of a file, use `file_append`. \
       Reserved for new files or full rewrites.";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path = try args |> member "path" |> to_string with _ -> "" in
        let content =
          try args |> member "content" |> to_string with _ -> ""
        in
        if path = "" then
          Lwt.return (param_err "parameter 'path' must be a non-empty string")
        else if path_targets_backlog path then
          Lwt.return (backlog_write_error path)
        else if
          not
            (is_path_allowed ~workspace ~workspace_only ~extra_allowed_paths
               path)
        then Lwt.return "Error: path is outside workspace"
        else
          let eff_ws = effective_cwd_or_workspace ?context ~workspace () in
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let path = resolve_path ~workspace:eff_ws path in
              let* () =
                Lwt_io.with_file ~mode:Lwt_io.Output path (fun oc ->
                    Lwt_io.write oc content)
              in
              Lwt.return
                (Printf.sprintf "Written %d bytes to %s" (String.length content)
                   path))
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let file_edit ~workspace ~workspace_only ~extra_allowed_paths =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "path",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Path to the file to edit (required)" );
                  ] );
              ( "old_text",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Text to find and replace (required)" );
                  ] );
              ( "new_text",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "Replacement text (required)");
                  ] );
              ( "replace_all",
                `Assoc
                  [
                    ("type", `String "boolean");
                    ( "description",
                      `String
                        "Optional: replace all occurrences (default false)" );
                  ] );
            ] );
        ( "required",
          `List [ `String "path"; `String "old_text"; `String "new_text" ] );
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"file_edit" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "file_edit";
    description =
      "Edit a file by replacing the first occurrence of old_text with new_text";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path = try args |> member "path" |> to_string with _ -> "" in
        let old_text =
          try args |> member "old_text" |> to_string with _ -> ""
        in
        let new_text =
          try args |> member "new_text" |> to_string with _ -> ""
        in
        let replace_all =
          try args |> member "replace_all" |> to_bool with _ -> false
        in
        if path = "" then
          Lwt.return (param_err "parameter 'path' must be a non-empty string")
        else if old_text = "" then
          Lwt.return
            (param_err "parameter 'old_text' must be a non-empty string")
        else if path_targets_backlog path then
          Lwt.return (backlog_write_error path)
        else if
          not
            (is_path_allowed ~workspace ~workspace_only ~extra_allowed_paths
               path)
        then Lwt.return "Error: path is outside workspace"
        else
          let eff_ws = effective_cwd_or_workspace ?context ~workspace () in
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let path = resolve_path ~workspace:eff_ws path in
              let* content =
                Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read
              in
              let idx =
                let ol = String.length old_text in
                let cl = String.length content in
                let rec find i =
                  if i + ol > cl then -1
                  else if String.sub content i ol = old_text then i
                  else find (i + 1)
                in
                find 0
              in
              if idx < 0 then Lwt.return "Error: old_text not found in file"
              else
                let replacements =
                  let rec count i acc =
                    if i + String.length old_text > String.length content then
                      acc
                    else if
                      String.sub content i (String.length old_text) = old_text
                    then count (i + String.length old_text) (acc + 1)
                    else count (i + 1) acc
                  in
                  count 0 0
                in
                let new_content =
                  if replace_all then (
                    let buf = Buffer.create (String.length content) in
                    let rec build i =
                      if i + String.length old_text > String.length content then
                        Buffer.add_string buf
                          (String.sub content i (String.length content - i))
                      else if
                        String.sub content i (String.length old_text) = old_text
                      then begin
                        Buffer.add_string buf new_text;
                        build (i + String.length old_text)
                      end
                      else begin
                        Buffer.add_char buf content.[i];
                        build (i + 1)
                      end
                    in
                    build 0;
                    Buffer.contents buf)
                  else
                    let before = String.sub content 0 idx in
                    let after =
                      String.sub content
                        (idx + String.length old_text)
                        (String.length content - idx - String.length old_text)
                    in
                    before ^ new_text ^ after
                in
                let* () =
                  Lwt_io.with_file ~mode:Lwt_io.Output path (fun oc ->
                      Lwt_io.write oc new_content)
                in
                Lwt.return
                  (Printf.sprintf
                     "Edited %s: replaced %d chars with %d chars (%d \
                      occurrence%s)"
                     path (String.length old_text) (String.length new_text)
                     (if replace_all then replacements else 1)
                     (if (if replace_all then replacements else 1) = 1 then ""
                      else "s")))
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let file_edit_lines ~workspace ~workspace_only ~extra_allowed_paths =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "path",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Path to the file to edit (required)" );
                  ] );
              ( "start_line",
                `Assoc
                  [
                    ("type", `String "integer");
                    ( "description",
                      `String "1-indexed start line, inclusive (required)" );
                  ] );
              ( "end_line",
                `Assoc
                  [
                    ("type", `String "integer");
                    ( "description",
                      `String "1-indexed end line, inclusive (required)" );
                  ] );
              ( "content",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "Replacement content (required)");
                  ] );
            ] );
        ( "required",
          `List
            [
              `String "path";
              `String "start_line";
              `String "end_line";
              `String "content";
            ] );
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"file_edit_lines" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "file_edit_lines";
    description =
      "Replace an inclusive 1-indexed line range [start_line, end_line] with \
       new content";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path = try args |> member "path" |> to_string with _ -> "" in
        let start_line =
          try args |> member "start_line" |> to_int with _ -> 0
        in
        let end_line = try args |> member "end_line" |> to_int with _ -> 0 in
        let replacement =
          try args |> member "content" |> to_string with _ -> ""
        in
        if path = "" then
          Lwt.return (param_err "parameter 'path' must be a non-empty string")
        else if start_line < 1 || end_line < 1 then
          Lwt.return
            (param_err
               "parameters 'start_line' and 'end_line' must be integers >= 1")
        else if end_line < start_line then
          Lwt.return (param_err "parameter 'end_line' must be >= 'start_line'")
        else if path_targets_backlog path then
          Lwt.return (backlog_write_error path)
        else if
          not
            (is_path_allowed ~workspace ~workspace_only ~extra_allowed_paths
               path)
        then Lwt.return "Error: path is outside workspace"
        else
          let eff_ws = effective_cwd_or_workspace ?context ~workspace () in
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let path = resolve_path ~workspace:eff_ws path in
              let* content =
                Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read
              in
              let lines = String.split_on_char '\n' content in
              let total = List.length lines in
              if start_line > total || end_line > total then
                Lwt.return
                  (Printf.sprintf
                     "Error: line range %d-%d out of bounds for %d-line file"
                     start_line end_line total)
              else
                let before =
                  lines |> List.filteri (fun i _ -> i < start_line - 1)
                in
                let after = lines |> List.filteri (fun i _ -> i >= end_line) in
                let replacement_lines = String.split_on_char '\n' replacement in
                let new_lines = before @ replacement_lines @ after in
                let new_content = String.concat "\n" new_lines in
                let* () =
                  Lwt_io.with_file ~mode:Lwt_io.Output path (fun oc ->
                      Lwt_io.write oc new_content)
                in
                Lwt.return
                  (Printf.sprintf "Edited %s: replaced lines %d-%d" path
                     start_line end_line))
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let is_localhost_url url =
  let has_http_scheme uri =
    match Uri.scheme uri with
    | Some scheme ->
        let scheme = String.lowercase_ascii scheme in
        scheme = "http" || scheme = "https"
    | None -> false
  in
  try
    let uri = Uri.of_string url in
    has_http_scheme uri
    && Uri.userinfo uri = None
    &&
    match Uri.host uri with
    | Some host -> String_util.is_loopback_host host
    | None -> false
  with _ -> false

let http_get ~workspace_only =
  let description =
    if workspace_only then
      "Fetch a localhost URL via GET and return the raw response body \
       (truncated at 10KB, workspace policy: external URLs restricted). For \
       HTML pages use web_fetch; for other methods use http_request."
    else
      "Fetch a URL via GET and return the raw response body (truncated at \
       10KB). For HTML pages use web_fetch; for other methods or custom \
       headers use http_request."
  in
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "url",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "URL to fetch (required)");
                  ] );
            ] );
        ("required", `List [ `String "url" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"http_get" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "http_get";
    description;
    parameters_schema = schema;
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let url = try args |> member "url" |> to_string with _ -> "" in
        if url = "" then
          Lwt.return (param_err "parameter 'url' must be a non-empty string")
        else if workspace_only && not (is_localhost_url url) then
          Lwt.return
            "Error: workspace policy restricts HTTP access to localhost only"
        else
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let* status, body = Http_client.get ~uri:url ~headers:[] in
              Lwt.return
                (Printf.sprintf "HTTP %d\n%s" status
                   (if String.length body > 10000 then
                      String.sub body 0 10000 ^ "\n... (truncated)"
                    else body)))
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let transcribe ~(config : Runtime_config.t) =
  let workspace = Runtime_config.effective_workspace config in
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "file_path",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Path to the audio file to transcribe (required)"
                    );
                  ] );
            ] );
        ("required", `List [ `String "file_path" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"transcribe" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "transcribe";
    description =
      "Transcribe an audio file to text via the configured speech-to-text \
       provider (Whisper, etc.). Supports common formats (mp3, wav, m4a, ogg). \
       Returns the plain-text transcription; no timestamps unless the provider \
       includes them. The file must already exist on disk — to download audio \
       first, use `http_get` or `web_fetch`.";
    parameters_schema = schema;
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let file_path =
          try args |> member "file_path" |> to_string with _ -> ""
        in
        if file_path = "" then
          Lwt.return
            (param_err "parameter 'file_path' must be a non-empty string")
        else if
          not
            (is_path_allowed ~workspace
               ~workspace_only:config.security.workspace_only
               ~extra_allowed_paths:config.security.extra_allowed_paths
               file_path)
        then Lwt.return "Error: file_path is outside workspace"
        else
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let file_path = resolve_path ~workspace file_path in
              let ic = open_in_bin file_path in
              let n = in_channel_length ic in
              let buf = Bytes.create n in
              really_input ic buf 0 n;
              close_in ic;
              let audio_data = Bytes.to_string buf in
              let filename = Filename.basename file_path in
              let content_type = Stt.content_type_of_ext filename in
              let* result =
                Stt.transcribe ~config ~audio_data ~filename ~content_type ()
              in
              Lwt.return result.text)
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let bg_shell_tools = Tools_bg_shell.tools

let inject_connector_history ~(config : Runtime_config.t) ~db ?session_mgr () =
  let connector_history_allowed ~key =
    match session_mgr with
    | Some mgr -> (
        match Session.find_connector_capabilities mgr ~key with
        | Some caps ->
            Connector_capabilities.should_capture_history
              ~enabled:config.connector_history.enabled caps
        | None -> false)
    | None -> false
  in
  {
    Tool.name = "inject_connector_history";
    description =
      "Retrieve recent messages from the group chat/channel that were not \
       addressed to the bot. Only available when connector_history.enabled is \
       true. Returns channel messages for context. Use this when the user asks \
       about recent chat activity or you need context about what others said.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "count",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Number of messages to retrieve (default 20, max 128)"
                      );
                    ] );
              ] );
          ("required", `List []);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let count =
          try
            let c = args |> member "count" |> to_int in
            max 1 (min 128 c)
          with _ -> 20
        in
        match context with
        | Some { session_key = Some key; send_progress; _ } ->
            if not config.connector_history.enabled then
              Lwt.return
                "Error: connector_history.enabled is false. Enable it in \
                 config to capture unaddressed group messages."
            else if not (connector_history_allowed ~key) then
              Lwt.return
                "Error: this connector does not support connector history \
                 capture. Use /inject_connector_history in Teams or Discord \
                 group chats."
            else
              let db_opt =
                if config.connector_history.persist_to_db then Some db else None
              in
              let entries = Connector_history.get ?db:db_opt ~key ~count () in
              if entries = [] then
                Lwt.return "No connector history available for this session."
              else begin
                let n = List.length entries in
                (match send_progress with
                | Some send ->
                    Lwt.async (fun () ->
                        send
                          (Printf.sprintf
                             "Last %d chat msgs loaded into context" n))
                | None -> ());
                Lwt.return (Connector_history.format_for_context entries)
              end
        | _ ->
            Lwt.return
              "Error: inject_connector_history requires a session context \
               (session_key). This tool is only available in connector \
               sessions (Teams, Discord).");
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }
