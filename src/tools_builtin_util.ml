let normalize_path = Path_util.normalize_path
let path_drift_count = ref 0
let shell_syntax_drift_count = ref 0
let shell_tokenizer_drift_count = ref 0

let reset_drift_counters () =
  path_drift_count := 0;
  shell_syntax_drift_count := 0;
  shell_tokenizer_drift_count := 0

let get_drift_counters () =
  (!path_drift_count, !shell_syntax_drift_count, !shell_tokenizer_drift_count)

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
  if coq_ok && not ocaml_ok then begin
    incr path_drift_count;
    Logs.warn (fun m ->
        m
          "PathSafety: Coq=safe, OCaml=unsafe for '%s' (symlink or realpath \
           edge case)"
          path)
  end;
  if (not coq_ok) && ocaml_ok then begin
    incr path_drift_count;
    Logs.debug (fun m ->
        m "PathSafety: Coq=unsafe, OCaml=safe for '%s' (conservative Coq model)"
          path)
  end;
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
  || has_char '`' || has_char '$' || has_char '!' || has_char '\n'
  || has_char '\r'
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
  if coq_unsafe <> ocaml_unsafe then begin
    incr shell_syntax_drift_count;
    Logs.warn (fun m ->
        m "ShellSafety drift: Coq=%b OCaml=%b for command %S" coq_unsafe
          ocaml_unsafe cmd)
  end;
  coq_unsafe

let split_command_words cmd =
  let coq_result =
    match Clawq_core.split_words cmd with
    | Some words -> Ok words
    | None -> Error "unterminated quote in command"
  in
  let ocaml_result = split_command_words_ocaml cmd in
  if coq_result <> ocaml_result then begin
    incr shell_tokenizer_drift_count;
    Logs.warn (fun m ->
        m "ShellSafety tokenizer drift between Coq and OCaml for command %S" cmd)
  end;
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

let effective_cwd_or_workspace ?context ~workspace () =
  match context with
  | Some c -> (
      match c.Tool.effective_cwd with Some cwd -> cwd | None -> workspace)
  | None -> workspace

(* Detect "cd <path> && <rest>" command pattern for cwd optimization.
   Returns Some (dir, rest_command) when the command starts with a simple
   cd to an absolute path followed by &&. *)
let extract_cd_prefix command =
  let cmd = String.trim command in
  let len = String.length cmd in
  if len > 3 && String.sub cmd 0 3 = "cd " then
    (* Find the && separator *)
    let rec find_amp i =
      if i + 1 >= len then None
      else if cmd.[i] = '&' && cmd.[i + 1] = '&' then
        let dir = String.trim (String.sub cmd 3 (i - 3)) in
        let rest = String.trim (String.sub cmd (i + 2) (len - i - 2)) in
        if dir <> "" && rest <> "" && dir.[0] = '/' then Some (dir, rest)
        else None
      else find_amp (i + 1)
    in
    find_amp 3
  else None

let resolve_shell_cwd ~workspace ~workspace_only ~extra_allowed_paths cwd_arg =
  let cwd_arg = String.trim cwd_arg in
  if cwd_arg = "" then Error "Error: cwd must not be empty"
  else
    let expanded = Runtime_config.expand_home cwd_arg in
    let resolved = resolve_path ~workspace expanded in
    if not (Sys.file_exists resolved) then
      Error (Printf.sprintf "Error: cwd does not exist: %s" cwd_arg)
    else if not (Sys.is_directory resolved) then
      Error (Printf.sprintf "Error: cwd is not a directory: %s" cwd_arg)
    else if
      workspace_only
      && not
           (is_path_within_allowed_roots ~workspace ~extra_allowed_paths
              resolved)
    then Error "Error: cwd is disallowed in workspace_only mode"
    else Ok resolved

let is_env_assignment_token token =
  match String.index_opt token '=' with
  | None -> false
  | Some idx when idx = 0 -> false
  | Some idx ->
      let name = String.sub token 0 idx in
      let valid_start =
        let c = name.[0] in
        (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_'
      in
      valid_start
      &&
      let rec loop i =
        if i >= String.length name then true
        else
          let c = name.[i] in
          if
            (c >= 'A' && c <= 'Z')
            || (c >= 'a' && c <= 'z')
            || (c >= '0' && c <= '9')
            || c = '_'
          then loop (i + 1)
          else false
      in
      loop 1

let drop_env_assignment_tokens argv =
  let rec loop = function
    | token :: rest when is_env_assignment_token token -> loop rest
    | rest -> rest
  in
  loop argv

let git_push_command_info ~command =
  let candidate =
    match extract_cd_prefix command with
    | Some (_dir, rest) -> rest
    | None -> command
  in
  match split_command_words candidate with
  | Error _ -> None
  | Ok argv -> (
      match drop_env_assignment_tokens argv with
      | git_cmd :: subcommand :: _
        when Filename.basename git_cmd = "git" && subcommand = "push" ->
          Some ()
      | _ -> None)

(* Process runner / output-rendering helpers and CI-watch group. *)
include Tools_builtin_proc

(* Session/summary agent-tool definitions. *)
include Tools_builtin_session

let shell_exec_with_hooks ~workspace ~workspace_only ~allowed_commands
    ~extra_allowed_paths ~sandbox ?session_mgr ?(spawn_background = Lwt.async)
    ?(watch_ci_after_push = watch_ci_after_push) () =
  let description =
    if workspace_only then
      "Execute a shell command and return stdout+stderr. IMPORTANT: You MUST \
       provide the 'command' argument — calls without 'command' will fail. \
       Workspace policy: only allowlisted commands (ls, cat, head, tail, grep, \
       find, wc, sort, uniq, echo, pwd, date, whoami, which, file, stat, diff, \
       patch, mkdir, touch, git, make, dune, opam, npm, yarn, jq, sed, awk, \
       tr, cut, tee, tar, zip, unzip, gzip, gunzip). No pipes, semicolons, \
       redirects, or subshells. Default timeout 30s, max 600s. Examples: \
       shell_exec(command=\"ls -la\", head=100, tail=100), \
       shell_exec(command=\"motd\") to check the message of the day"
    else
      "Execute a shell command and return stdout+stderr. IMPORTANT: You MUST \
       provide the 'command' argument — calls without 'command' will fail. \
       Example: shell_exec(command=\"ls -la\", head=100, tail=100)"
  in
  let max_timeout = 600.0 in
  let default_timeout = 30.0 in
  let run_command ?context ?on_output_chunk args =
    let open Yojson.Safe.Util in
    let interrupt_check =
      match context with Some c -> c.Tool.interrupt_check | None -> None
    in
    let eff_ws = effective_cwd_or_workspace ?context ~workspace () in
    let command = try args |> member "command" |> to_string with _ -> "" in
    if String.trim command = "" then
      Lwt.return
        "Error: shell_exec requires a non-empty 'command' parameter. Example: \
         shell_exec(command=\"ls -la\", head=100, tail=50). The 'command' \
         field must be a non-empty string."
    else
      let cwd_arg =
        try
          match args |> member "cwd" with
          | `Null -> None
          | json -> Some (to_string json)
        with _ -> Some ""
      in
      let timeout_secs =
        try
          let v = args |> member "timeout" |> to_number in
          if v <= 0.0 then default_timeout else Float.min v max_timeout
        with _ -> default_timeout
      in
      match
        ( parse_optional_int_field args "head",
          parse_optional_int_field args "tail" )
      with
      | Error msg, _ | _, Error msg -> Lwt.return msg
      | Ok head_lines, Ok tail_lines -> (
          match
            ( validate_positive_optional_int ~field_name:"head" head_lines,
              validate_positive_optional_int ~field_name:"tail" tail_lines )
          with
          | Error msg, _ | _, Error msg -> Lwt.return msg
          | Ok (), Ok () -> (
              if workspace_only && has_unsafe_shell_syntax command then
                Lwt.return
                  "Error: command contains unsafe shell syntax in \
                   workspace_only mode"
              else if
                workspace_only
                && not (is_command_allowed ~allowed_commands command)
              then
                Lwt.return
                  (Printf.sprintf
                     "Error: command '%s' is not in the allowlist. Allowed: %s"
                     (extract_command command)
                     (String.concat ", " allowed_commands))
              else
                let open Lwt.Syntax in
                let cwd_result =
                  match cwd_arg with
                  | None ->
                      Ok
                        (if eff_ws <> workspace then Some eff_ws
                         else if workspace_only then Some workspace
                         else None)
                  | Some cwd ->
                      Result.map
                        (fun resolved -> Some resolved)
                        (resolve_shell_cwd ~workspace ~workspace_only
                           ~extra_allowed_paths cwd)
                in
                match cwd_result with
                | Error msg -> Lwt.return msg
                | Ok cwd -> (
                    let base_env =
                      if workspace_only then
                        Runtime_config.workspace_only_env ()
                      else Runtime_config.augment_env_path (Unix.environment ())
                    in
                    let env =
                      match context with
                      | Some c -> (
                          match c.Tool.session_key with
                          | Some sk ->
                              let prefix = "CLAWQ_SESSION_ID=" in
                              let var = prefix ^ sk in
                              let replaced = ref false in
                              let updated =
                                Array.map
                                  (fun entry ->
                                    if String.starts_with ~prefix entry then begin
                                      replaced := true;
                                      var
                                    end
                                    else entry)
                                  base_env
                              in
                              if !replaced then updated
                              else Array.append base_env [| var |]
                          | None -> base_env)
                      | None -> base_env
                    in
                    let session_key =
                      match context with
                      | Some c -> c.Tool.session_key
                      | None -> None
                    in
                    let original_command = command in
                    let should_watch_ci_after_push =
                      Option.is_some
                        (git_push_command_info ~command:original_command)
                    in
                    let optimized_cd_prefix =
                      if workspace_only then None
                      else
                        match extract_cd_prefix original_command with
                        | Some (dir, rest)
                          when Sys.file_exists dir && Sys.is_directory dir ->
                            Some (dir, rest)
                        | _ -> None
                    in
                    let repo_path =
                      match (optimized_cd_prefix, cwd) with
                      | Some (dir, _), _ -> dir
                      | None, Some dir -> dir
                      | None, None -> workspace
                    in
                    let* captured_head_sha =
                      match
                        (session_mgr, session_key, should_watch_ci_after_push)
                      with
                      | Some _mgr, Some _sk, true -> (
                          let* result =
                            exec_command ~cwd:repo_path "git"
                              [ "rev-parse"; "HEAD" ]
                          in
                          match result with
                          | Ok sha -> Lwt.return (Some (String.trim sha))
                          | Error err ->
                              Logs.info (fun m ->
                                  m
                                    "CI watch could not capture pre-push HEAD \
                                     in %s: %s"
                                    repo_path err);
                              Lwt.return_none)
                      | _ -> Lwt.return_none
                    in
                    let command =
                      Sandbox.wrap_command sandbox original_command
                    in
                    let run_proc cmd =
                      run_process_with_timeout ?interrupt_check ?on_output_chunk
                        ~cwd ~env ~cmd ~timeout_secs ~head_lines ~tail_lines ()
                    in
                    let maybe_watch_ci_after_push result =
                      match
                        (session_mgr, session_key, should_watch_ci_after_push)
                      with
                      | Some mgr, Some sk, true
                        when String.starts_with ~prefix:"exit_code: 0\n" result
                        ->
                          spawn_background (fun () ->
                              match captured_head_sha with
                              | Some head_sha when head_sha <> "" ->
                                  watch_ci_after_push
                                    ~resolve_head_sha:(fun ~repo_path:_ ->
                                      Lwt.return (Ok head_sha))
                                    ~session_mgr:mgr ~session_key:sk ~repo_path
                                    ()
                              | _ ->
                                  watch_ci_after_push ~session_mgr:mgr
                                    ~session_key:sk ~repo_path ())
                      | _ -> ()
                    in
                    let run_and_maybe_watch cmd =
                      let* result = run_proc cmd in
                      maybe_watch_ci_after_push result;
                      Lwt.return result
                    in
                    if workspace_only then
                      match split_command_words command with
                      | Error msg -> Lwt.return ("Error: " ^ msg)
                      | Ok argv -> (
                          let argv = List.map expand_home_in_arg argv in
                          match argv with
                          | [] -> Lwt.return "Error: command is required"
                          | cmd :: _
                            when not (is_workspace_safe_command_token cmd) ->
                              Lwt.return
                                "Error: command binary path is disallowed in \
                                 workspace_only mode"
                          | _
                            when has_workspace_unsafe_args ~workspace
                                   ~extra_allowed_paths argv ->
                              Lwt.return
                                "Error: command contains paths/targets \
                                 disallowed in workspace_only mode"
                          | _ -> run_and_maybe_watch ("", Array.of_list argv))
                    else
                      match optimized_cd_prefix with
                      | Some (dir, rest) ->
                          let cwd = Some dir in
                          let* result =
                            run_process_with_timeout ?interrupt_check
                              ?on_output_chunk ~cwd ~env
                              ~cmd:("", [| "/bin/sh"; "-c"; rest |])
                              ~timeout_secs ~head_lines ~tail_lines ()
                          in
                          maybe_watch_ci_after_push result;
                          Lwt.return result
                      | _ ->
                          run_and_maybe_watch
                            ("", [| "/bin/sh"; "-c"; command |]))))
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
                      ( "description",
                        `String "The shell command to execute (required)" );
                    ] );
                ( "cwd",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional working directory. Relative paths are \
                           resolved from the workspace root." );
                    ] );
                ( "timeout",
                  `Assoc
                    [
                      ("type", `String "number");
                      ( "description",
                        `String "Timeout in seconds (default 30, max 600)" );
                    ] );
                ( "head",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Optional number of first lines to show for stdout \
                           and stderr" );
                    ] );
                ( "tail",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Optional number of last lines to show for stdout \
                           and stderr" );
                    ] );
              ] );
          ("required", `List [ `String "command" ]);
        ];
    invoke = (fun ?context args -> run_command ?context args);
    invoke_stream =
      Some
        (fun ?context ~on_output_chunk args ->
          run_command ?context ~on_output_chunk args);
    risk_level = High;
    deferred = false;
  }

(** [resolve_credential_handle ~config ~handle_id ~header_name] resolves a
    credential handle through the credential lease API. Returns [Ok value] with
    the resolved credential value, or [Error msg] if the handle is missing or
    unresolvable. When [handle_id] is [None], returns [Ok ""] (legacy path). *)
let resolve_credential_handle ~(config : Runtime_config.t)
    ~(handle_id : string option) ~(header_name : string) :
    (string, string) result =
  match handle_id with
  | None -> Ok ""
  | Some hid -> (
      match
        Credential_lease.resolve_lease ~config ~handle_id:hid ~header_name
      with
      | Error err ->
          let msg = Credential_lease.resolution_error_to_string err in
          Error
            (Printf.sprintf "credential lease denied for handle '%s': %s" hid
               msg)
      | Ok lease ->
          let result = ref "" in
          Credential_lease.apply_headers lease (fun headers ->
              result :=
                List.fold_left
                  (fun acc (name, value) ->
                    if name = header_name then value else acc)
                  "" headers);
          if !result = "" then
            Error
              (Printf.sprintf
                 "credential lease for handle '%s' resolved but produced no %s \
                  header"
                 hid header_name)
          else Ok !result)
