let normalize_path path =
  let parts = String.split_on_char '/' path in
  let is_abs = String.length path > 0 && path.[0] = '/' in
  let rec resolve acc = function
    | [] -> List.rev acc
    | "." :: rest -> resolve acc rest
    | ".." :: rest -> (
        match acc with _ :: tl -> resolve tl rest | [] -> resolve [] rest)
    | "" :: rest -> resolve acc rest
    | part :: rest -> resolve (part :: acc) rest
  in
  let resolved = resolve [] parts in
  let joined = String.concat "/" resolved in
  if is_abs then "/" ^ joined else joined

(* Coq-extracted pure path safety check (F2).
   Uses the formally verified normalize + is_prefix logic from PathSafety.v. *)
let is_path_safe_coq ~workspace resolved_path =
  let ws_segs = String.split_on_char '/' workspace in
  let path_segs = String.split_on_char '/' resolved_path in
  Clawq_core.is_path_safe_segs ws_segs path_segs

(* OCaml path safety check using realpath (handles symlinks).
   Kept as defense-in-depth alongside the Coq-extracted check. *)
let is_path_safe_ocaml ~workspace path =
  let real_workspace =
    try Unix.realpath workspace
    with Unix.Unix_error _ -> normalize_path workspace
  in
  let resolved =
    if Filename.is_relative path then Filename.concat workspace path else path
  in
  (* Try realpath first (works for existing files/dirs), fall back to normalization *)
  let real_path =
    try Unix.realpath resolved
    with Unix.Unix_error _ ->
      (* For non-existent files, try resolving the parent directory *)
      let dir = Filename.dirname resolved in
      let base = Filename.basename resolved in
      let real_dir =
        try Unix.realpath dir with Unix.Unix_error _ -> normalize_path dir
      in
      Filename.concat real_dir base
  in
  let wlen = String.length real_workspace in
  String.length real_path >= wlen
  && String.sub real_path 0 wlen = real_workspace
  && (String.length real_path = wlen || real_path.[wlen] = '/')

(* Primary gate: require BOTH Coq-verified pure check AND OCaml realpath check.
   - Coq check: formally verified immunity to ".." traversal in segment space
   - OCaml check: resolves symlinks (defense-in-depth, beyond Coq scope)
   Log warnings when they disagree to surface model/implementation drift. *)
let is_path_safe ~workspace path =
  let resolved_for_coq =
    if Filename.is_relative path then Filename.concat workspace path else path
  in
  let coq_ok = is_path_safe_coq ~workspace resolved_for_coq in
  let ocaml_ok = is_path_safe_ocaml ~workspace path in
  if coq_ok && not ocaml_ok then
    Logs.warn (fun m ->
        m
          "PathSafety: Coq=safe, OCaml=unsafe for '%s' (symlink or realpath \
           edge case)"
          path);
  if (not coq_ok) && ocaml_ok then
    Logs.debug (fun m ->
        m "PathSafety: Coq=unsafe, OCaml=safe for '%s' (conservative Coq model)"
          path);
  coq_ok && ocaml_ok

let default_shell_allowlist =
  [
    "ls";
    "cat";
    "head";
    "tail";
    "grep";
    "find";
    "wc";
    "sort";
    "uniq";
    "echo";
    "pwd";
    "date";
    "whoami";
    "which";
    "file";
    "stat";
    "diff";
    "patch";
    "mkdir";
    "touch";
    "git";
    "make";
    "dune";
    "opam";
    "npm";
    "yarn";
    "jq";
    "sed";
    "awk";
    "tr";
    "cut";
    "tee";
    "tar";
    "zip";
    "unzip";
    "gzip";
    "gunzip";
  ]

let extract_command cmd =
  let trimmed = String.trim cmd in
  (* Skip leading env vars like VAR=value *)
  let parts = String.split_on_char ' ' trimmed in
  let rec find_cmd = function
    | [] -> ""
    | part :: rest ->
        if String.contains part '=' && not (String.contains part '/') then
          find_cmd rest
        else
          (* Handle paths like /usr/bin/ls -> ls *)
          Filename.basename part
  in
  find_cmd parts

let is_command_allowed ~allowed_commands cmd =
  let base_cmd = extract_command cmd in
  base_cmd <> "" && List.mem base_cmd allowed_commands

let has_unsafe_shell_syntax_ocaml cmd =
  let has_char c = String.contains cmd c in
  has_char ';' || has_char '|' || has_char '&' || has_char '>' || has_char '<'
  || has_char '`' || has_char '\n' || has_char '\r'
  ||
  let len = String.length cmd in
  let rec has_dollar_paren i =
    if i + 1 >= len then false
    else if cmd.[i] = '$' && cmd.[i + 1] = '(' then true
    else has_dollar_paren (i + 1)
  in
  has_dollar_paren 0

let split_command_words_ocaml cmd =
  let len = String.length cmd in
  let buf = Buffer.create len in
  let words = ref [] in
  let push_word () =
    if Buffer.length buf > 0 then begin
      words := Buffer.contents buf :: !words;
      Buffer.clear buf
    end
  in
  let rec parse i quote =
    if i >= len then (
      match quote with
      | Some _ -> Error "unterminated quote in command"
      | None ->
          push_word ();
          Ok (List.rev !words))
    else
      let c = cmd.[i] in
      match quote with
      | None ->
          if c = ' ' || c = '\t' then begin
            push_word ();
            parse (i + 1) None
          end
          else if c = '\'' || c = '"' then parse (i + 1) (Some c)
          else if c = '\\' && i + 1 < len then begin
            Buffer.add_char buf cmd.[i + 1];
            parse (i + 2) None
          end
          else begin
            Buffer.add_char buf c;
            parse (i + 1) None
          end
      | Some q ->
          if c = q then parse (i + 1) None
          else if c = '\\' && q = '"' && i + 1 < len then begin
            Buffer.add_char buf cmd.[i + 1];
            parse (i + 2) (Some q)
          end
          else begin
            Buffer.add_char buf c;
            parse (i + 1) (Some q)
          end
  in
  parse 0 None

let has_unsafe_shell_syntax cmd =
  let coq_unsafe = not (Clawq_core.is_shell_safe cmd) in
  let ocaml_unsafe = has_unsafe_shell_syntax_ocaml cmd in
  if coq_unsafe <> ocaml_unsafe then
    Logs.warn (fun m ->
        m "ShellSafety drift: Coq=%b OCaml=%b for command %S" coq_unsafe
          ocaml_unsafe cmd);
  coq_unsafe

let split_command_words cmd =
  let coq_result =
    match Clawq_core.split_words cmd with
    | Some words -> Ok words
    | None -> Error "unterminated quote in command"
  in
  let ocaml_result = split_command_words_ocaml cmd in
  if coq_result <> ocaml_result then
    Logs.warn (fun m ->
        m "ShellSafety tokenizer drift between Coq and OCaml for command %S" cmd);
  coq_result

let contains_substr s sub =
  let slen = String.length s in
  let nlen = String.length sub in
  let rec loop i =
    if i + nlen > slen then false
    else if String.sub s i nlen = sub then true
    else loop (i + 1)
  in
  nlen = 0 || loop 0

let expand_home_in_arg arg =
  let a = String.trim arg in
  if String.length a > 0 && a.[0] = '-' then
    match String.index_opt a '=' with
    | Some i when i + 1 < String.length a ->
        let key = String.sub a 0 (i + 1) in
        let value = String.sub a (i + 1) (String.length a - i - 1) in
        key ^ Runtime_config.expand_home value
    | _ -> a
  else Runtime_config.expand_home a

let is_path_within_allowed_roots ~workspace ~extra_allowed_paths path =
  is_path_safe ~workspace path
  || List.exists
       (fun extra ->
         let extra = Runtime_config.expand_home extra |> normalize_path in
         is_path_safe ~workspace:extra path)
       extra_allowed_paths

let is_workspace_safe_arg ~workspace ~extra_allowed_paths arg =
  let a = String.trim arg in
  let a =
    if String.length a > 0 && a.[0] = '-' then
      match String.index_opt a '=' with
      | Some i when i + 1 < String.length a ->
          String.sub a (i + 1) (String.length a - i - 1)
      | _ -> ""
    else a
  in
  if a = "" then true
  else
    let len = String.length a in
    (not (contains_substr a ".." || contains_substr a "://"))
    &&
    if len > 0 && (a.[0] = '/' || a.[0] = '~') then
      let expanded = Runtime_config.expand_home a in
      is_path_within_allowed_roots ~workspace ~extra_allowed_paths expanded
    else true

let has_workspace_unsafe_args ~workspace ~extra_allowed_paths argv =
  match argv with
  | [] -> true
  | cmd :: args ->
      let unsafe_args =
        List.exists
          (fun arg ->
            not (is_workspace_safe_arg ~workspace ~extra_allowed_paths arg))
          args
      in
      let git_unsafe =
        if cmd = "git" then
          match args with
          | subcmd :: _ ->
              let allowed_subcommands =
                [
                  "status";
                  "log";
                  "diff";
                  "show";
                  "branch";
                  "rev-parse";
                  "ls-files";
                ]
              in
              (not (List.mem subcmd allowed_subcommands))
              || List.exists
                   (fun a -> contains_substr a "://" || contains_substr a "@")
                   args
          | [] -> true
        else false
      in
      unsafe_args || git_unsafe

let is_workspace_safe_command_token token =
  let t = String.trim token in
  t <> "" && (not (String.contains t '/')) && not (contains_substr t "..")

let resolve_path ~workspace path =
  if Filename.is_relative path then Filename.concat workspace path else path

let file_read_default_limit = 200
let file_read_max_limit = 2000
let file_read_max_full_chars = 50000
let file_read_max_line_chars = 2000

let parse_optional_int_field args field_name =
  let open Yojson.Safe.Util in
  match args |> member field_name with
  | `Null -> Ok None
  | _ -> (
      try Ok (Some (args |> member field_name |> to_int))
      with _ ->
        Error (Printf.sprintf "Error: %s must be an integer" field_name))

let validate_file_read_window ~offset ~limit =
  if offset < 1 then Error "Error: offset must be >= 1"
  else if limit < 1 then Error "Error: limit must be >= 1"
  else if limit > file_read_max_limit then
    Error (Printf.sprintf "Error: limit must be <= %d" file_read_max_limit)
  else Ok ()

let canonicalize_for_read path =
  try Ok (Unix.realpath path)
  with Unix.Unix_error (err, _, _) ->
    Error (Printf.sprintf "Error: %s" (Unix.error_message err))

let truncate_line_for_paging line =
  if String.length line <= file_read_max_line_chars then (line, false)
  else
    ( String.sub line 0 file_read_max_line_chars
      ^ Printf.sprintf " ...(truncated %d chars)"
          (String.length line - file_read_max_line_chars),
      true )

let format_lines_window ~content ~offset ~limit =
  let lines = String.split_on_char '\n' content in
  let total = List.length lines in
  let start = offset - 1 in
  let indexed = List.mapi (fun i line -> (i + 1, line)) lines in
  let selected =
    indexed |> List.filter (fun (n, _) -> n >= offset && n < offset + limit)
  in
  if selected = [] then
    Printf.sprintf
      "No lines in requested range. File has %d lines. Try a smaller offset."
      total
  else
    let truncated_any = ref false in
    let rendered =
      selected
      |> List.map (fun (n, line) ->
          let line, truncated = truncate_line_for_paging line in
          if truncated then truncated_any := true;
          Printf.sprintf "%d: %s" n line)
      |> String.concat "\n"
    in
    let last_line = fst (List.hd (List.rev selected)) in
    let suffix =
      if start + List.length selected < total then
        Printf.sprintf
          "\n\n(Showing lines %d-%d of %d. Use offset=%d to continue.)" offset
          last_line total (last_line + 1)
      else Printf.sprintf "\n\n(End of file - total %d lines)" total
    in
    let trunc_suffix =
      if !truncated_any then
        Printf.sprintf
          "\n\n(Note: long lines are truncated to %d chars in paged mode.)"
          file_read_max_line_chars
      else ""
    in
    rendered ^ suffix ^ trunc_suffix

let shell_exec ~workspace ~workspace_only ~allowed_commands ~extra_allowed_paths
    ~sandbox =
  let description =
    if workspace_only then
      "Execute a shell command from the workspace directory and return stdout \
       and stderr"
    else "Execute a shell command and return stdout and stderr"
  in
  {
    Tool.name = "shell_exec";
    description;
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "command",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Shell command to execute");
                    ] );
              ] );
          ("required", `List [ `String "command" ]);
        ];
    invoke =
      (fun args ->
        let open Yojson.Safe.Util in
        let command =
          try args |> member "command" |> to_string with _ -> ""
        in
        if command = "" then Lwt.return "Error: command is required"
        else if workspace_only && has_unsafe_shell_syntax command then
          Lwt.return
            "Error: command contains unsafe shell syntax in workspace_only mode"
        else if
          workspace_only && not (is_command_allowed ~allowed_commands command)
        then
          Lwt.return
            (Printf.sprintf
               "Error: command '%s' is not in the allowlist. Allowed: %s"
               (extract_command command)
               (String.concat ", " allowed_commands))
        else
          let open Lwt.Syntax in
          let env =
            if workspace_only then
              [|
                ("HOME=" ^ try Sys.getenv "HOME" with Not_found -> "/tmp");
                ("PATH="
                ^ try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin");
              |]
            else Unix.environment ()
          in
          let cwd = if workspace_only then Some workspace else None in
          let command = Sandbox.wrap_command sandbox command in
          match split_command_words command with
          | Error msg -> Lwt.return ("Error: " ^ msg)
          | Ok argv -> (
              let argv =
                if workspace_only then List.map expand_home_in_arg argv
                else argv
              in
              match argv with
              | [] -> Lwt.return "Error: command is required"
              | cmd :: _
                when workspace_only && not (is_workspace_safe_command_token cmd)
                ->
                  Lwt.return
                    "Error: command binary path is disallowed in \
                     workspace_only mode"
              | _
                when workspace_only
                     && has_workspace_unsafe_args ~workspace
                          ~extra_allowed_paths argv ->
                  Lwt.return
                    "Error: command contains paths/targets disallowed in \
                     workspace_only mode"
              | _ ->
                  let cmd = ("", Array.of_list argv) in
                  let proc = Lwt_process.open_process_full ?cwd ~env cmd in
                  let timeout = Lwt_unix.sleep 30.0 in
                  let* result =
                    Lwt.pick
                      [
                        (let* stdout = Lwt_io.read proc#stdout in
                         let* stderr = Lwt_io.read proc#stderr in
                         let* status = proc#close in
                         let exit_code =
                           match status with
                           | Unix.WEXITED n -> n
                           | Unix.WSIGNALED n -> 128 + n
                           | Unix.WSTOPPED n -> 128 + n
                         in
                         Lwt.return
                           (Printf.sprintf
                              "exit_code: %d\nstdout:\n%s\nstderr:\n%s"
                              exit_code stdout stderr));
                        (let* () = timeout in
                         proc#kill Sys.sigkill;
                         Lwt.return "Error: command timed out after 30 seconds");
                      ]
                  in
                  Lwt.return result));
    risk_level = High;
  }

let is_path_allowed ~workspace ~workspace_only ~extra_allowed_paths path =
  if not workspace_only then true
  else is_path_within_allowed_roots ~workspace ~extra_allowed_paths path

let file_read ~workspace ~workspace_only ~extra_allowed_paths =
  {
    Tool.name = "file_read";
    description = "Read the contents of a file";
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
      (fun args ->
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
                    Lwt.catch
                      (fun () ->
                        let open Lwt.Syntax in
                        let path = resolve_path ~workspace path in
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
    risk_level = Low;
  }

let file_append ~workspace ~workspace_only ~extra_allowed_paths =
  {
    Tool.name = "file_append";
    description = "Append content to the end of a file";
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
      (fun args ->
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
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let path = resolve_path ~workspace path in
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
    risk_level = Medium;
  }

let file_write ~workspace ~workspace_only ~extra_allowed_paths =
  {
    Tool.name = "file_write";
    description = "Write content to a file";
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
      (fun args ->
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
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let path = resolve_path ~workspace path in
              let* () =
                Lwt_io.with_file ~mode:Lwt_io.Output path (fun oc ->
                    Lwt_io.write oc content)
              in
              Lwt.return
                (Printf.sprintf "Written %d bytes to %s" (String.length content)
                   path))
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    risk_level = Medium;
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
      (fun args ->
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
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let path = resolve_path ~workspace path in
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
    risk_level = Medium;
  }

let file_edit_lines ~workspace ~workspace_only ~extra_allowed_paths =
  {
    Tool.name = "file_edit_lines";
    description =
      "Replace an inclusive line range [start_line, end_line] with new content";
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
      (fun args ->
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
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let path = resolve_path ~workspace path in
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
    risk_level = Medium;
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
      "Fetch a localhost URL and return the response body (workspace policy: \
       external URLs restricted)"
    else "Fetch a URL and return the response body"
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
      (fun args ->
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
    risk_level = Medium;
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
      (fun args ->
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
    risk_level = Low;
  }

let memory_store ~db =
  {
    Tool.name = "memory_store";
    description =
      "Store a core memory with a key, content, and optional category";
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
      (fun args ->
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
    risk_level = Low;
  }

let memory_recall ~db =
  {
    Tool.name = "memory_recall";
    description = "Search core memories using full-text search";
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
      (fun args ->
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
    risk_level = Low;
  }

let memory_forget ~db =
  {
    Tool.name = "memory_forget";
    description = "Remove a core memory by key";
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
      (fun args ->
        let open Yojson.Safe.Util in
        let key = try args |> member "key" |> to_string with _ -> "" in
        if key = "" then Lwt.return "Error: key is required"
        else
          let deleted = Memory.forget_core ~db ~key in
          if deleted then Lwt.return (Printf.sprintf "Deleted memory: %s" key)
          else Lwt.return (Printf.sprintf "No memory found with key: %s" key));
    risk_level = Low;
  }

let memory_list ~db =
  {
    Tool.name = "memory_list";
    description = "List core memories, optionally filtered by category";
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
      (fun args ->
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
    risk_level = Low;
  }

let register_all ~(config : Runtime_config.t) ~sandbox ?(db = None) registry =
  let workspace_only = config.security.workspace_only in
  let workspace = Runtime_config.effective_workspace config in
  let extra_allowed_paths = config.security.extra_allowed_paths in
  Tool_registry.register registry
    (shell_exec ~workspace ~workspace_only
       ~allowed_commands:default_shell_allowlist ~extra_allowed_paths ~sandbox);
  Tool_registry.register registry
    (file_read ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (file_write ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (file_append ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (file_edit ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (file_edit_lines ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry (http_get ~workspace_only);
  if config.stt <> None then
    Tool_registry.register registry (transcribe ~config);
  match db with
  | Some db ->
      Tool_registry.register registry (memory_store ~db);
      Tool_registry.register registry (memory_recall ~db);
      Tool_registry.register registry (memory_forget ~db);
      Tool_registry.register registry (memory_list ~db)
  | None -> ()
