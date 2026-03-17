open Tools_builtin_util

(* Directories to skip during recursive walks — these are commonly huge and
   rarely contain user-authored content worth searching. *)
let skip_dirs =
  let s = Hashtbl.create 32 in
  List.iter
    (fun d -> Hashtbl.replace s d ())
    [
      ".git";
      "node_modules";
      ".cache";
      "_build";
      ".opam";
      "__pycache__";
      ".tox";
      ".mypy_cache";
      ".pytest_cache";
      "target";
      "dist";
      "build";
      ".next";
      ".nuxt";
      "vendor";
      "_opam";
      ".yarn";
      ".pnpm-store";
    ];
  s

let is_skip_dir name = Hashtbl.mem skip_dirs name

(* Recursively walk a directory tree in a thread-pool thread, skipping
   common huge directories, with a timeout.  [on_entry] is called for every
   file-system entry; it may call [~add] to accumulate a result string and
   check [~at_limit] to stop early.  Returns [Ok results] or [Error msg]. *)
let walk_collect ?(timeout = 30.0) ?(max_files = 1000) ~root ~max_results
    ~on_entry () =
  Lwt.pick
    [
      Lwt_preemptive.detach
        (fun () ->
          let results = ref [] in
          let count = ref 0 in
          let files_visited = ref 0 in
          let hit_file_limit = ref false in
          let add s =
            if !count < max_results then begin
              results := s :: !results;
              incr count
            end
          in
          let at_limit () =
            if !count >= max_results then true
            else if !files_visited >= max_files then begin
              hit_file_limit := true;
              true
            end
            else false
          in
          let rec walk dir =
            if at_limit () then ()
            else
              match Sys.readdir dir with
              | entries ->
                  Array.iter
                    (fun entry ->
                      if not (at_limit ()) then begin
                        let full = Filename.concat dir entry in
                        let is_dir =
                          try Sys.is_directory full with Sys_error _ -> false
                        in
                        incr files_visited;
                        on_entry ~full ~entry ~is_dir ~at_limit ~add;
                        if is_dir && not (is_skip_dir entry) then walk full
                      end)
                    entries
              | exception Sys_error _ -> ()
          in
          (try
             if Sys.is_directory root then walk root
             else begin
               incr files_visited;
               on_entry ~full:root ~entry:(Filename.basename root) ~is_dir:false
                 ~at_limit ~add
             end
           with Sys_error _ -> ());
          let results = List.rev !results in
          if !hit_file_limit then
            Ok
              ( results,
                Some
                  (Printf.sprintf
                     "\n\n\
                      (warning: stopped after visiting %d files — use a more \
                      specific path or increase max_files to search more)"
                     max_files) )
          else Ok (results, None))
        ();
      Lwt.bind (Lwt_unix.sleep timeout) (fun () ->
          Lwt.return
            (Error
               (Printf.sprintf
                  "Error: operation timed out after %.0fs (search path may be \
                   too broad — try a more specific path or narrower glob)"
                  timeout)));
    ]

let is_path_allowed ~workspace ~workspace_only ~extra_allowed_paths path =
  if not workspace_only then true
  else is_path_within_allowed_roots ~workspace ~extra_allowed_paths path

let file_read ~workspace ~workspace_only ~extra_allowed_paths =
  {
    Tool.name = "file_read";
    description =
      "Read a file's text content. Full reads limited to 50,000 chars; for \
       larger files use offset and limit parameters to read in parts.";
    parameters_schema =
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
                      ("description", `String "Path to the file to read");
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
                          "Optional max lines to read when using offset \
                           (default 200, max 2000)" );
                    ] );
              ] );
          ("required", `List [ `String "path" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path = try args |> member "path" |> to_string with _ -> "" in
        let offset_input = parse_optional_int_field args "offset" in
        let limit_input = parse_optional_int_field args "limit" in
        if path = "" then Lwt.return "Error: path is required"
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
                            else
                              let* content =
                                Lwt_io.with_file ~mode:Lwt_io.Input path
                                  Lwt_io.read
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
  {
    Tool.name = "file_append";
    description =
      "Append content to the end of a file, creating it if it does not exist";
    parameters_schema =
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
                      ("description", `String "Path to the file to append");
                    ] );
                ( "content",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Content to append to the file");
                    ] );
              ] );
          ("required", `List [ `String "path"; `String "content" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path = try args |> member "path" |> to_string with _ -> "" in
        let content =
          try args |> member "content" |> to_string with _ -> ""
        in
        if path = "" then Lwt.return "Error: path is required"
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
              let* existing =
                Lwt.catch
                  (fun () ->
                    Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read)
                  (fun _ -> Lwt.return "")
              in
              let* () =
                Lwt_io.with_file ~mode:Lwt_io.Output path (fun oc ->
                    Lwt_io.write oc (existing ^ content))
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
  {
    Tool.name = "file_write";
    description = "Create or overwrite a file with the given content";
    parameters_schema =
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
                      ("description", `String "Path to the file to write");
                    ] );
                ( "content",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Content to write to the file");
                    ] );
              ] );
          ("required", `List [ `String "path"; `String "content" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path = try args |> member "path" |> to_string with _ -> "" in
        let content =
          try args |> member "content" |> to_string with _ -> ""
        in
        if path = "" then Lwt.return "Error: path is required"
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
  {
    Tool.name = "file_edit";
    description =
      "Edit a file by replacing the first occurrence of old_text with new_text";
    parameters_schema =
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
                      ("description", `String "Path to the file to edit");
                    ] );
                ( "old_text",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Text to find and replace");
                    ] );
                ( "new_text",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Replacement text");
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
        ];
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
        if path = "" then Lwt.return "Error: path is required"
        else if old_text = "" then Lwt.return "Error: old_text is required"
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
                  if replace_all then
                    let rec build i acc =
                      if i + String.length old_text > String.length content then
                        acc ^ String.sub content i (String.length content - i)
                      else if
                        String.sub content i (String.length old_text) = old_text
                      then build (i + String.length old_text) (acc ^ new_text)
                      else build (i + 1) (acc ^ String.make 1 content.[i])
                    in
                    build 0 ""
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
  {
    Tool.name = "file_edit_lines";
    description =
      "Replace an inclusive 1-indexed line range [start_line, end_line] with \
       new content";
    parameters_schema =
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
                      ("description", `String "Path to the file to edit");
                    ] );
                ( "start_line",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "1-indexed start line (inclusive)");
                    ] );
                ( "end_line",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "1-indexed end line (inclusive)");
                    ] );
                ( "content",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Replacement content");
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
        ];
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
        if path = "" then Lwt.return "Error: path is required"
        else if start_line < 1 || end_line < 1 then
          Lwt.return "Error: start_line and end_line must be >= 1"
        else if end_line < start_line then
          Lwt.return "Error: end_line must be >= start_line"
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
  let is_loopback_host host =
    let host = String.lowercase_ascii host in
    host = "localhost" || host = "127.0.0.1" || host = "::1"
  in
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
    | Some host -> is_loopback_host host
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
  {
    Tool.name = "http_get";
    description;
    parameters_schema =
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
                      ("description", `String "URL to fetch");
                    ] );
              ] );
          ("required", `List [ `String "url" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let url = try args |> member "url" |> to_string with _ -> "" in
        if url = "" then Lwt.return "Error: url is required"
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
  {
    Tool.name = "transcribe";
    description = "Transcribe an audio file to text using speech-to-text";
    parameters_schema =
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
                        `String "Path to the audio file to transcribe" );
                    ] );
              ] );
          ("required", `List [ `String "file_path" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let file_path =
          try args |> member "file_path" |> to_string with _ -> ""
        in
        if file_path = "" then Lwt.return "Error: file_path is required"
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

let memory_store ~db =
  {
    Tool.name = "memory_store";
    description =
      "Store a persistent key-value memory that survives across sessions. \
       Overwrites if the key already exists.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "key",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Unique key for the memory");
                    ] );
                ( "content",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Content to store");
                    ] );
                ( "category",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Category for the memory (default: general)" );
                    ] );
              ] );
          ("required", `List [ `String "key"; `String "content" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let key = try args |> member "key" |> to_string with _ -> "" in
        let content =
          try args |> member "content" |> to_string with _ -> ""
        in
        let category =
          try args |> member "category" |> to_string with _ -> "general"
        in
        if key = "" then Lwt.return "Error: key is required"
        else if content = "" then Lwt.return "Error: content is required"
        else begin
          Memory.store_core ~db ~key ~content ~category ();
          Lwt.return (Printf.sprintf "Stored memory: %s" key)
        end);
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let memory_recall ~db =
  {
    Tool.name = "memory_recall";
    description =
      "Search persistent memories by full-text query and return matching \
       key-content pairs";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "query",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Search query");
                    ] );
                ( "limit",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "Maximum number of results (default: 5)" );
                    ] );
              ] );
          ("required", `List [ `String "query" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let query = try args |> member "query" |> to_string with _ -> "" in
        let limit = try args |> member "limit" |> to_int with _ -> 5 in
        if query = "" then Lwt.return "Error: query is required"
        else
          let results = Memory.recall_core ~db ~query ~limit in
          if results = [] then Lwt.return "No matching memories found"
          else
            let lines =
              List.map
                (fun (key, content, category) ->
                  Printf.sprintf "[%s] (%s): %s" key category content)
                results
            in
            Lwt.return (String.concat "\n" lines));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let memory_forget ~db =
  {
    Tool.name = "memory_forget";
    description = "Delete a persistent memory by its exact key";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "key",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Key of the memory to remove");
                    ] );
              ] );
          ("required", `List [ `String "key" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let key = try args |> member "key" |> to_string with _ -> "" in
        if key = "" then Lwt.return "Error: key is required"
        else
          let deleted = Memory.forget_core ~db ~key in
          if deleted then Lwt.return (Printf.sprintf "Deleted memory: %s" key)
          else Lwt.return (Printf.sprintf "No memory found with key: %s" key));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let memory_list ~db =
  {
    Tool.name = "memory_list";
    description =
      "List all persistent memories, optionally filtered by category";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "category",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Optional category filter (omit for all)" );
                    ] );
              ] );
          ("required", `List []);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let category =
          try args |> member "category" |> to_string with _ -> ""
        in
        let results = Memory.list_core ~db ~category () in
        if results = [] then Lwt.return "No memories found"
        else
          let lines =
            List.map
              (fun (key, content, cat) ->
                Printf.sprintf "[%s] (%s): %s" key cat content)
              results
          in
          Lwt.return (String.concat "\n" lines));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let history_search ~db =
  {
    Tool.name = "history_search";
    description =
      "Search your own chat/session message history across current and \
       archived epochs. Returns matching messages with role, content snippet, \
       timestamp, and source epoch.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "query",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Text to search for in message history" );
                    ] );
                ( "limit",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "Maximum number of results (default: 10)" );
                    ] );
              ] );
          ("required", `List [ `String "query" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let query = try args |> member "query" |> to_string with _ -> "" in
        let limit = try args |> member "limit" |> to_int with _ -> 10 in
        if query = "" then Lwt.return "Error: query is required"
        else
          let session_key =
            match context with Some ctx -> ctx.Tool.session_key | None -> None
          in
          match session_key with
          | None -> Lwt.return "Error: no session context available"
          | Some sk ->
              let results =
                Memory.search_session_history ~db ~session_key:sk ~query ~limit
                  ()
              in
              if results = [] then Lwt.return "No matching messages found"
              else
                let lines =
                  List.map
                    (fun (r : Memory.history_search_result) ->
                      let snippet =
                        if String.length r.content > 200 then
                          String.sub r.content 0 200 ^ "..."
                        else r.content
                      in
                      Printf.sprintf "[%s] (%s) [%s]: %s" r.source r.role
                        r.created_at snippet)
                    results
                in
                Lwt.return (String.concat "\n" lines));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

(* ───── Filesystem navigation tools ───── *)

(* Glob pattern matching helpers *)
let glob_match_segment pat s =
  let pl = String.length pat and sl = String.length s in
  let rec go pi si =
    if pi = pl then si = sl
    else
      match pat.[pi] with
      | '*' ->
          let rec try_star j =
            if j > sl then false
            else if go (pi + 1) j then true
            else try_star (j + 1)
          in
          try_star si
      | '?' -> si < sl && go (pi + 1) (si + 1)
      | c -> si < sl && c = s.[si] && go (pi + 1) (si + 1)
  in
  go 0 0

let rec glob_match_segs pats parts =
  match (pats, parts) with
  | [], [] -> true
  | [ "**" ], _ -> true
  | [], _ -> false
  | "**" :: rest_pats, parts -> (
      glob_match_segs rest_pats parts
      ||
      match parts with
      | [] -> false
      | _ :: rest_parts -> glob_match_segs ("**" :: rest_pats) rest_parts)
  | _, [] -> false
  | pat :: rest_pats, part :: rest_parts ->
      glob_match_segment pat part && glob_match_segs rest_pats rest_parts

let glob_matches_path ~pattern path =
  let split s = String.split_on_char '/' s |> List.filter (fun x -> x <> "") in
  glob_match_segs (split pattern) (split path)

let glob ~workspace ~workspace_only ~extra_allowed_paths =
  {
    Tool.name = "glob";
    description =
      "Find files matching a glob pattern (supports * and ** wildcards). \
       Returns absolute file paths.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "pattern",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Glob pattern, e.g. \"**/*.ml\" or \"src/*.json\"" );
                    ] );
                ( "root",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Root directory to search from (defaults to \
                           workspace)" );
                    ] );
                ( "max_results",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "Max results to return (default 200)" );
                    ] );
                ( "max_files",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Max files to visit during search (default 1000). \
                           Increase for broad searches or reduce search scope \
                           with a more specific root/pattern." );
                    ] );
              ] );
          ("required", `List [ `String "pattern" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let pattern =
          try args |> member "pattern" |> to_string with _ -> ""
        in
        let root_arg = try args |> member "root" |> to_string with _ -> "" in
        let max_results =
          try args |> member "max_results" |> to_int with _ -> 200
        in
        let max_files =
          try args |> member "max_files" |> to_int with _ -> 1000
        in
        if pattern = "" then
          Lwt.return
            "Error: glob requires a non-empty 'pattern' parameter. Example: \
             glob(pattern=\"**/*.ml\"). The 'pattern' field must be a \
             non-empty string."
        else
          let eff_ws = effective_cwd_or_workspace ?context ~workspace () in
          let root =
            if root_arg = "" then eff_ws
            else resolve_path ~workspace:eff_ws root_arg
          in
          if root_arg <> "" && not (Sys.file_exists root) then
            Lwt.return
              (Printf.sprintf
                 "Error: root directory does not exist: %s. Provide an \
                  absolute path or a path relative to the workspace root."
                 root_arg)
          else if
            root_arg <> ""
            && try not (Sys.is_directory root) with Sys_error _ -> true
          then
            Lwt.return
              (Printf.sprintf
                 "Error: root is not a directory: %s. Provide a directory \
                  path, not a file. Use list_dir to discover available \
                  directories."
                 root_arg)
          else if
            workspace_only
            && not
                 (is_path_within_allowed_roots ~workspace ~extra_allowed_paths
                    root)
          then
            Lwt.return
              "Error: root path is outside the workspace in workspace_only mode"
          else
            let rlen = String.length root in
            Lwt.bind
              (walk_collect ~root ~max_results ~max_files
                 ~on_entry:(fun ~full ~entry:_ ~is_dir:_ ~at_limit:_ ~add ->
                   let flen = String.length full in
                   let rel =
                     if
                       flen > rlen + 1
                       && String.sub full 0 rlen = root
                       && full.[rlen] = '/'
                     then String.sub full (rlen + 1) (flen - rlen - 1)
                     else full
                   in
                   if glob_matches_path ~pattern rel then add full)
                 ())
              (fun result ->
                match result with
                | Error msg -> Lwt.return msg
                | Ok (results, warning) ->
                    let sorted = List.sort String.compare results in
                    if sorted = [] then
                      Lwt.return
                        ("No files matched" ^ Option.value ~default:"" warning)
                    else
                      Lwt.return
                        (String.concat "\n" sorted
                        ^ Printf.sprintf "\n\n(%d files matched)"
                            (List.length sorted)
                        ^ Option.value ~default:"" warning)));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let list_dir ~workspace ~workspace_only ~extra_allowed_paths =
  {
    Tool.name = "list_dir";
    description =
      "List directory contents with type labels (file/dir) for each entry";
    parameters_schema =
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
                        `String "Directory path (defaults to workspace)" );
                    ] );
                ( "show_hidden",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String "Show hidden files (default false)" );
                    ] );
              ] );
          ("required", `List []);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path_arg = try args |> member "path" |> to_string with _ -> "" in
        let show_hidden =
          try args |> member "show_hidden" |> to_bool with _ -> false
        in
        let eff_ws = effective_cwd_or_workspace ?context ~workspace () in
        let path =
          if path_arg = "" then eff_ws
          else resolve_path ~workspace:eff_ws path_arg
        in
        if path_arg <> "" && not (Sys.file_exists path) then
          Lwt.return
            (Printf.sprintf
               "Error: path does not exist: %s. Provide an absolute path or a \
                path relative to the workspace root."
               path_arg)
        else if
          path_arg <> ""
          && try not (Sys.is_directory path) with Sys_error _ -> true
        then
          Lwt.return
            (Printf.sprintf
               "Error: path is not a directory: %s. list_dir only works on \
                directories. Use file_read to read a file's contents."
               path_arg)
        else if
          workspace_only
          && not
               (is_path_within_allowed_roots ~workspace ~extra_allowed_paths
                  path)
        then
          Lwt.return
            "Error: path is outside the workspace in workspace_only mode"
        else
          match Sys.readdir path with
          | entries ->
              let entries = Array.to_list entries in
              let entries =
                if show_hidden then entries
                else List.filter (fun e -> e = "" || e.[0] <> '.') entries
              in
              let entries = List.sort String.compare entries in
              let lines =
                List.map
                  (fun entry ->
                    let full = Filename.concat path entry in
                    let kind =
                      try if Sys.is_directory full then "dir " else "file"
                      with Sys_error _ -> "?   "
                    in
                    Printf.sprintf "%s  %s" kind entry)
                  entries
              in
              Lwt.return
                (if lines = [] then "(empty directory)"
                 else
                   String.concat "\n" lines
                   ^ Printf.sprintf "\n\n(%d entries)" (List.length lines))
          | exception Sys_error msg ->
              Lwt.return (Printf.sprintf "Error: %s" msg));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let contains_substr ~haystack ~needle ~case_sensitive =
  let h =
    if case_sensitive then haystack else String.lowercase_ascii haystack
  in
  let n = if case_sensitive then needle else String.lowercase_ascii needle in
  let hl = String.length h and nl = String.length n in
  if nl = 0 then true
  else if nl > hl then false
  else begin
    let found = ref false in
    let i = ref 0 in
    while (not !found) && !i + nl <= hl do
      if String.sub h !i nl = n then found := true;
      incr i
    done;
    !found
  end

let grep ~workspace ~workspace_only ~extra_allowed_paths =
  {
    Tool.name = "grep";
    description =
      "Search files for a regex pattern (OCaml Str syntax) and return matching \
       lines with file path and line number. Supports | to match multiple \
       alternative patterns.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "pattern",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Regex pattern (e.g. \"let.*=\" or \"TODO|FIXME\"). \
                           Use | to separate alternatives." );
                    ] );
                ( "path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "File or directory to search (defaults to workspace)"
                      );
                    ] );
                ( "file_glob",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Glob filter for filenames, e.g. \"*.ml\" (optional)"
                      );
                    ] );
                ( "include",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Alias for file_glob; glob filter for filenames \
                           (optional)" );
                    ] );
                ( "case_sensitive",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String "Case-sensitive search (default true)" );
                    ] );
                ( "max_results",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "Max matching lines (default 50)");
                    ] );
                ( "max_files",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Max files to visit during search (default 1000). \
                           Increase for broad searches or reduce search scope \
                           with a more specific path/file_glob." );
                    ] );
              ] );
          ("required", `List [ `String "pattern" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let pattern =
          try args |> member "pattern" |> to_string with _ -> ""
        in
        let path_arg = try args |> member "path" |> to_string with _ -> "" in
        let file_glob =
          try args |> member "include" |> to_string
          with _ -> (
            try args |> member "file_glob" |> to_string with _ -> "")
        in
        let case_sensitive =
          try args |> member "case_sensitive" |> to_bool with _ -> true
        in
        let max_results =
          try args |> member "max_results" |> to_int with _ -> 50
        in
        let max_files =
          try args |> member "max_files" |> to_int with _ -> 1000
        in
        if pattern = "" then
          Lwt.return
            "Error: grep requires a non-empty 'pattern' parameter. Example: \
             grep(pattern=\"TODO|FIXME\"). The 'pattern' field must be a \
             non-empty string."
        else
          let split_unescaped_pipes s =
            let parts = ref [] in
            let buf = Buffer.create (String.length s) in
            let flush () =
              parts := Buffer.contents buf :: !parts;
              Buffer.clear buf
            in
            let rec loop i escaped =
              if i >= String.length s then flush ()
              else
                let ch = s.[i] in
                if escaped then begin
                  Buffer.add_char buf ch;
                  loop (i + 1) false
                end
                else
                  match ch with
                  | '\\' ->
                      Buffer.add_char buf ch;
                      loop (i + 1) true
                  | '|' ->
                      flush ();
                      loop (i + 1) false
                  | _ ->
                      Buffer.add_char buf ch;
                      loop (i + 1) false
            in
            loop 0 false;
            List.rev !parts
          in
          match
            try
              Ok
                (List.map
                   (fun part ->
                     if case_sensitive then Str.regexp part
                     else Str.regexp_case_fold part)
                   (split_unescaped_pipes pattern))
            with Failure msg -> Error msg
          with
          | Error msg -> Lwt.return ("Error: invalid regex pattern: " ^ msg)
          | Ok regexes ->
              let file_matches_glob file_path =
                file_glob = ""
                || glob_match_segment file_glob (Filename.basename file_path)
              in
              let line_matches line =
                List.exists
                  (fun regex ->
                    try
                      ignore (Str.search_forward regex line 0);
                      true
                    with Not_found -> false)
                  regexes
              in
              let eff_ws = effective_cwd_or_workspace ?context ~workspace () in
              let root =
                if path_arg = "" then eff_ws
                else resolve_path ~workspace:eff_ws path_arg
              in
              if path_arg <> "" && not (Sys.file_exists root) then
                Lwt.return
                  (Printf.sprintf
                     "Error: path does not exist: %s. Provide an absolute path \
                      or a path relative to the workspace root."
                     path_arg)
              else if
                workspace_only
                && not
                     (is_path_within_allowed_roots ~workspace
                        ~extra_allowed_paths root)
              then
                Lwt.return
                  "Error: path is outside the workspace in workspace_only mode"
              else
                Lwt.bind
                  (walk_collect ~root ~max_results ~max_files
                     ~on_entry:(fun ~full ~entry:_ ~is_dir ~at_limit ~add ->
                       if is_dir || at_limit () || not (file_matches_glob full)
                       then ()
                       else begin
                         try
                           let ic = open_in full in
                           let lnum = ref 0 in
                           (try
                              while not (at_limit ()) do
                                let line = input_line ic in
                                incr lnum;
                                if line_matches line then
                                  add
                                    (Printf.sprintf "%s:%d: %s" full !lnum line)
                              done
                            with End_of_file -> ());
                           close_in ic
                         with Sys_error _ -> ()
                       end)
                     ())
                  (fun result ->
                    match result with
                    | Error msg -> Lwt.return msg
                    | Ok (sorted, warning) ->
                        if sorted = [] then
                          Lwt.return
                            (Printf.sprintf "No matches found for '%s'" pattern
                            ^ Option.value ~default:"" warning)
                        else
                          Lwt.return
                            (String.concat "\n" sorted
                            ^ Printf.sprintf "\n\n(%d matches)"
                                (List.length sorted)
                            ^ Option.value ~default:"" warning)));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let change_working_dir ~(config : Runtime_config.t) ~workspace ~workspace_only
    ~extra_allowed_paths =
  {
    Tool.name = "change_working_dir";
    description =
      "Change the effective working directory for subsequent tool operations. \
       Relative paths resolve from the current effective CWD. Optionally wipe \
       conversation history to reduce context noise after initial navigation.";
    parameters_schema =
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
                        `String
                          "Directory path to change to (absolute or relative \
                           to current effective CWD)" );
                    ] );
                ( "wipe_history",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "If true, wipe intermediate history keeping only the \
                           first user message and a summary (default false)" );
                    ] );
              ] );
          ("required", `List [ `String "path" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path_arg = try args |> member "path" |> to_string with _ -> "" in
        let wipe_history =
          try args |> member "wipe_history" |> to_bool with _ -> false
        in
        if path_arg = "" then
          Lwt.return
            "Error: path is required. Provide the directory to change to, e.g. \
             change_working_dir(path=\"src\")."
        else
          match context with
          | None ->
              Lwt.return
                "Error: change_working_dir requires an agent context. This \
                 tool can only be used within an agent session."
          | Some ctx -> (
              match ctx.Tool.request_cwd_change with
              | None ->
                  Lwt.return
                    "Error: change_working_dir callback not available. This \
                     tool can only be used within an agent session."
              | Some request_cwd_change ->
                  let eff_ws =
                    effective_cwd_or_workspace ?context ~workspace ()
                  in
                  let resolved =
                    if Filename.is_relative path_arg then
                      Filename.concat eff_ws path_arg
                    else path_arg
                  in
                  let resolved = normalize_path resolved in
                  if not (Sys.file_exists resolved) then
                    Lwt.return
                      (Printf.sprintf
                         "Error: directory does not exist: %s. Check the path \
                          or use list_dir to discover available directories."
                         path_arg)
                  else if
                    try not (Sys.is_directory resolved)
                    with Sys_error _ -> true
                  then
                    Lwt.return
                      (Printf.sprintf
                         "Error: path is not a directory: %s. \
                          change_working_dir only works on directories."
                         path_arg)
                  else if
                    workspace_only
                    && not
                         (is_path_within_allowed_roots ~workspace
                            ~extra_allowed_paths resolved)
                  then
                    Lwt.return
                      "Error: path is outside the workspace in workspace_only \
                       mode"
                  else
                    let patterns = config.security.allowed_cwd_patterns in
                    let expanded_patterns =
                      List.map
                        (Runtime_config.expand_cwd_pattern ~config)
                        patterns
                    in
                    let matches =
                      List.exists
                        (fun pat -> glob_matches_path ~pattern:pat resolved)
                        expanded_patterns
                    in
                    if not matches then
                      Lwt.return
                        (Printf.sprintf
                           "Error: directory %s does not match any \
                            allowed_cwd_patterns. Configured patterns: %s. \
                            Update security.allowed_cwd_patterns in %s to \
                            allow this directory."
                           resolved
                           (String.concat ", "
                              (List.map (fun p -> "\"" ^ p ^ "\"") patterns))
                           (Dot_dir.config_path ()))
                    else begin
                      request_cwd_change resolved wipe_history;
                      let entries =
                        try
                          let items =
                            Sys.readdir resolved |> Array.to_list
                            |> List.filter (fun e -> e = "" || e.[0] <> '.')
                            |> List.sort String.compare
                          in
                          let classified =
                            List.map
                              (fun name ->
                                let full = Filename.concat resolved name in
                                if try Sys.is_directory full with _ -> false
                                then name ^ "/"
                                else name)
                              items
                          in
                          let max_show = 30 in
                          let len = List.length classified in
                          if len = 0 then "(empty directory)"
                          else if len <= max_show then
                            String.concat "  " classified
                          else
                            String.concat "  "
                              (List.filteri
                                 (fun i _ -> i < max_show)
                                 classified)
                            ^ Printf.sprintf "  ...(%d more)" (len - max_show)
                        with _ -> "(unable to list)"
                      in
                      Lwt.return
                        (Printf.sprintf
                           "Changed working directory to: %s\nContents: %s%s"
                           resolved entries
                           (if wipe_history then
                              "\n\
                               (History wiped — only first user message and \
                               summary retained)"
                            else ""))
                    end));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let bg_shell_tools = Tools_bg_shell.tools

(* ───── HTTP tools ───── *)
