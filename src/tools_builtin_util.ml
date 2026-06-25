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

let validate_positive_optional_int ~field_name = function
  | None -> Ok ()
  | Some n when n >= 1 -> Ok ()
  | Some _ -> Error (Printf.sprintf "Error: %s must be >= 1" field_name)

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

let logical_lines text =
  let lines = String.split_on_char '\n' text in
  match List.rev lines with
  | [] -> []
  | [ "" ] -> []
  | "" :: rest -> List.rev rest
  | _ -> lines

let last_n_lines ~n lines =
  let len = List.length lines in
  if len <= n then lines
  else
    let to_drop = len - n in
    let rec drop k = function
      | [] -> []
      | _ :: rest when k > 0 -> drop (k - 1) rest
      | l -> l
    in
    drop to_drop lines

let take_n_lines ~n lines =
  let rec loop acc remaining = function
    | _ when remaining <= 0 -> List.rev acc
    | [] -> List.rev acc
    | line :: rest -> loop (line :: acc) (remaining - 1) rest
  in
  loop [] n lines

let apply_shell_output_window ~head_lines ~tail_lines text =
  match (head_lines, tail_lines) with
  | None, None -> (text, false)
  | _ ->
      let lines = logical_lines text in
      let total = List.length lines in
      let head_count = Option.value head_lines ~default:0 in
      let tail_count = Option.value tail_lines ~default:0 in
      if total <= head_count + tail_count then (text, false)
      else
        let head =
          if head_count > 0 then take_n_lines ~n:head_count lines else []
        in
        let tail =
          if tail_count > 0 then last_n_lines ~n:tail_count lines else []
        in
        let omitted = total - List.length head - List.length tail in
        let marker =
          match (head_lines, tail_lines) with
          | Some h, Some t ->
              Printf.sprintf
                "... (omitted %d middle lines; showing first %d and last %d of \
                 %d)"
                omitted h t total
          | Some h, None ->
              Printf.sprintf
                "... (omitted %d trailing lines; showing first %d of %d)"
                omitted h total
          | None, Some t ->
              Printf.sprintf
                "... (omitted %d leading lines; showing last %d of %d)" omitted
                t total
          | None, None -> assert false
        in
        (String.concat "\n" (head @ [ marker ] @ tail), true)

type ci_run = {
  run_id : int;
  status : string;
  conclusion : string option;
  url : string option;
  workflow_name : string option;
}

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

let ci_conclusion_is_failure = function
  | Some ("success" | "neutral" | "skipped") -> false
  | Some _ -> true
  | None -> false

let exec_command ?cwd program argv =
  let open Lwt.Syntax in
  let proc =
    Lwt_process.open_process_full
      (program, Array.of_list (program :: argv))
      ?cwd
  in
  Lwt.finalize
    (fun () ->
      let* stdout, stderr =
        Lwt.both (Lwt_io.read proc#stdout) (Lwt_io.read proc#stderr)
      in
      let* status = proc#status in
      match status with
      | Unix.WEXITED 0 -> Lwt.return (Ok stdout)
      | Unix.WEXITED code ->
          let detail = String.trim (if stderr <> "" then stderr else stdout) in
          Lwt.return
            (Error
               (Printf.sprintf "%s exited %d%s" program code
                  (if detail = "" then "" else ": " ^ detail)))
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
          Lwt.return
            (Error (Printf.sprintf "%s terminated by signal %d" program signal)))
    (fun () ->
      let open Lwt.Syntax in
      let* _ = proc#close in
      Lwt.return_unit)

let gh_json_command ?cwd argv = exec_command ?cwd "gh" argv

let parse_ci_run_json json ~id_field =
  let open Yojson.Safe.Util in
  let opt_string field =
    match json |> member field with
    | `Null -> None
    | value -> (
        try
          let s = to_string value |> String.trim in
          if s = "" then None else Some s
        with _ -> None)
  in
  try
    Some
      {
        run_id = json |> member id_field |> to_int;
        status = json |> member "status" |> to_string;
        conclusion = opt_string "conclusion";
        url = opt_string "url";
        workflow_name = opt_string "workflowName";
      }
  with _ -> None

let inject_session_message_async ?turn_override ~(session_mgr : Session.t)
    ~session_key ~message () =
  let open Lwt.Syntax in
  let turn mgr ~key ~message ?channel ?channel_id () =
    match turn_override with
    | Some custom -> custom mgr ~key ~message ?channel ?channel_id ()
    | None -> Session.turn mgr ~key ~message ?channel ?channel_id ()
  in
  let channel, channel_id =
    match Restart_notify.parse_channel_from_key session_key with
    | Some (channel, channel_id) -> (Some channel, Some channel_id)
    | None -> (None, None)
  in
  Lwt.catch
    (fun () ->
      let* response =
        turn session_mgr ~key:session_key ~message ?channel ?channel_id ()
      in
      if Session.should_suppress_response response then
        Logs.info (fun m ->
            m "CI watch queued follow-up for busy session %s" session_key)
      else
        Logs.info (fun m ->
            m "CI watch injected follow-up into session %s" session_key);
      Lwt.return_unit)
    (fun exn ->
      Logs.warn (fun m ->
          m "CI watch session injection failed for %s: %s" session_key
            (Printexc.to_string exn));
      Lwt.return_unit)

let watch_ci_after_push
    ?(resolve_head_sha =
      fun ~repo_path ->
        exec_command ~cwd:repo_path "git" [ "rev-parse"; "HEAD" ])
    ?(gh_command = gh_json_command) ?(sleep = Lwt_unix.sleep)
    ?(poll_interval = 10.0) ?(startup_timeout = 120.0)
    ?(completion_timeout = 1800.0) ~(session_mgr : Session.t) ~session_key
    ~repo_path () =
  let open Lwt.Syntax in
  let rec wait_for_run ~head_sha ~deadline () =
    let* result =
      gh_command ~cwd:repo_path
        [
          "run";
          "list";
          "--json";
          "databaseId,headSha,status,conclusion,url,workflowName";
          "--limit";
          "20";
        ]
    in
    match result with
    | Error err -> Lwt.return (Error err)
    | Ok body -> (
        try
          let open Yojson.Safe.Util in
          let json = Yojson.Safe.from_string body |> to_list in
          let matching =
            List.filter_map
              (fun item ->
                let sha =
                  try item |> member "headSha" |> to_string with _ -> ""
                in
                if sha = head_sha then
                  parse_ci_run_json item ~id_field:"databaseId"
                else None)
              json
          in
          match matching with
          | run :: _ -> Lwt.return (Ok (Some run))
          | [] when Unix.gettimeofday () < deadline ->
              let* () = sleep poll_interval in
              wait_for_run ~head_sha ~deadline ()
          | [] -> Lwt.return (Ok None)
        with exn ->
          Lwt.return
            (Error
               (Printf.sprintf "failed to parse gh run list JSON: %s"
                  (Printexc.to_string exn))))
  in
  let rec wait_for_completion ~run_id ~deadline () =
    let* result =
      gh_command ~cwd:repo_path
        [
          "run";
          "view";
          string_of_int run_id;
          "--json";
          "databaseId,status,conclusion,url,workflowName";
        ]
    in
    match result with
    | Error err -> Lwt.return (Error err)
    | Ok body -> (
        try
          let json = Yojson.Safe.from_string body in
          match parse_ci_run_json json ~id_field:"databaseId" with
          | Some run when run.status = "completed" -> Lwt.return (Ok run)
          | Some _ when Unix.gettimeofday () < deadline ->
              let* () = sleep poll_interval in
              wait_for_completion ~run_id ~deadline ()
          | Some run -> Lwt.return (Ok run)
          | None ->
              Lwt.return
                (Error "failed to parse gh run view JSON: missing run fields")
        with exn ->
          Lwt.return
            (Error
               (Printf.sprintf "failed to parse gh run view JSON: %s"
                  (Printexc.to_string exn))))
  in
  Lwt.catch
    (fun () ->
      let* git_head = resolve_head_sha ~repo_path in
      let head_sha =
        match git_head with
        | Ok sha -> String.trim sha
        | Error err ->
            Logs.info (fun m ->
                m "CI watch skipped for %s: unable to resolve HEAD: %s"
                  repo_path err);
            ""
      in
      if head_sha = "" then Lwt.return_unit
      else
        let startup_deadline = Unix.gettimeofday () +. startup_timeout in
        let* run_result =
          wait_for_run ~head_sha ~deadline:startup_deadline ()
        in
        match run_result with
        | Error err ->
            Logs.info (fun m -> m "CI watch stopped for %s: %s" repo_path err);
            Lwt.return_unit
        | Ok None ->
            Logs.info (fun m ->
                m "CI watch found no workflow run for HEAD %s in %s" head_sha
                  repo_path);
            Lwt.return_unit
        | Ok (Some run) -> (
            let completion_deadline =
              Unix.gettimeofday () +. completion_timeout
            in
            let* final_run_result =
              if run.status = "completed" then Lwt.return (Ok run)
              else
                wait_for_completion ~run_id:run.run_id
                  ~deadline:completion_deadline ()
            in
            match final_run_result with
            | Error err ->
                Logs.info (fun m ->
                    m "CI watch stopped for %s run %d: %s" repo_path run.run_id
                      err);
                Lwt.return_unit
            | Ok final_run when ci_conclusion_is_failure final_run.conclusion ->
                let workflow =
                  Option.value final_run.workflow_name ~default:"GitHub Actions"
                in
                let conclusion =
                  Option.value final_run.conclusion ~default:"failed"
                in
                let url_suffix =
                  match final_run.url with
                  | Some url -> "\nRun: " ^ url
                  | None -> ""
                in
                let message =
                  Printf.sprintf
                    "[async CI watch]\n\
                     The recent `git push` for HEAD `%s` in `%s` completed \
                     with CI conclusion `%s` (%s). Investigate the failure and \
                     continue from there.%s"
                    head_sha
                    (Filename.basename repo_path)
                    conclusion workflow url_suffix
                in
                let* () =
                  inject_session_message_async ~session_mgr ~session_key
                    ~message ()
                in
                Lwt.return_unit
            | Ok _ -> Lwt.return_unit))
    (fun exn ->
      Logs.warn (fun m ->
          m "CI watch failed for session %s in %s: %s" session_key repo_path
            (Printexc.to_string exn));
      Lwt.return_unit)

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
  let read_channel ?on_chunk ic buf =
    let open Lwt.Syntax in
    let rec loop () =
      let* chunk = Lwt_io.read ~count:4096 ic in
      if chunk = "" then Lwt.return_unit
      else begin
        Buffer.add_string buf chunk;
        let* () =
          match on_chunk with
          | Some emit -> emit chunk
          | None -> Lwt.return_unit
        in
        loop ()
      end
    in
    loop ()
  in
  let max_timeout = 600.0 in
  let default_timeout = 30.0 in
  let shell_output_dir () = Dot_dir.sub "tool-output" in
  let ensure_dir path =
    if Sys.file_exists path then () else Unix.mkdir path 0o755
  in
  let ensure_parent_dirs path =
    let rec loop p =
      let parent = Filename.dirname p in
      if parent <> p && not (Sys.file_exists parent) then begin
        loop parent;
        ensure_dir parent
      end
    in
    loop path
  in
  let write_tool_output_if_needed ~save_full text =
    let max_chars = 20000 in
    if (not save_full) && String.length text <= max_chars then None
    else
      let dir = shell_output_dir () in
      (try ensure_dir (Dot_dir.path ()) with _ -> ());
      (try ensure_dir dir with _ -> ());
      let path =
        Filename.concat dir
          (Printf.sprintf "shell-exec-%Ld.txt"
             (Int64.of_float (Unix.gettimeofday () *. 1000000.)))
      in
      try
        ensure_parent_dirs path;
        let oc = open_out_bin path in
        output_string oc text;
        close_out oc;
        Some path
      with _ -> None
  in
  let render_output_stream ~label ~text ~head_lines ~tail_lines =
    let max_chars = 20000 in
    let windowed, omitted_by_window =
      apply_shell_output_window ~head_lines ~tail_lines text
    in
    let full_output_path =
      write_tool_output_if_needed
        ~save_full:(omitted_by_window || String.length text > max_chars)
        text
    in
    let rendered, truncated_for_chars =
      if String.length windowed <= max_chars then (windowed, false)
      else (String.sub windowed 0 max_chars ^ "\n... (truncated)", true)
    in
    let total_lines_note =
      match (head_lines, tail_lines) with
      | None, None -> ""
      | _ ->
          let total = List.length (logical_lines text) in
          Printf.sprintf "\n[%s: %d total lines]" label total
    in
    let note =
      match (truncated_for_chars, full_output_path) with
      | true, Some path ->
          Printf.sprintf
            "\n[%s truncated; full %s saved to %s for later inspection]" label
            label path
      | false, Some path ->
          Printf.sprintf "\n[full %s saved to %s for later inspection]" label
            path
      | true, None -> Printf.sprintf "\n[%s truncated]" label
      | false, None -> ""
    in
    (rendered, note ^ total_lines_note)
  in
  let render_command_result ~exit_code ~stdout ~stderr ~head_lines ~tail_lines =
    let stdout_rendered, stdout_note =
      render_output_stream ~label:"stdout" ~text:stdout ~head_lines ~tail_lines
    in
    let stderr_rendered, stderr_note =
      render_output_stream ~label:"stderr" ~text:stderr ~head_lines ~tail_lines
    in
    Printf.sprintf "exit_code: %d\nstdout:\n%s%s\nstderr:\n%s%s" exit_code
      stdout_rendered stdout_note stderr_rendered stderr_note
  in
  let should_interrupt interrupt_check =
    match interrupt_check with
    | Some check -> (
        match check () with
        | Some reason when reason <> Agent.queued_message_interrupt_token ->
            true
        | _ -> false)
    | None -> false
  in
  let wait_for_interrupt interrupt_check =
    let open Lwt.Syntax in
    let rec loop () =
      if should_interrupt interrupt_check then Lwt.return_unit
      else
        let* () = Lwt_unix.sleep 0.05 in
        loop ()
    in
    loop ()
  in
  let shell_command_display (cmd : string * string array) =
    match cmd with
    | "", argv -> String.concat " " (Array.to_list argv)
    | command, [||] -> command
    | command, argv -> command ^ " " ^ String.concat " " (Array.to_list argv)
  in
  let run_process_with_timeout ?interrupt_check ?on_output_chunk ~cwd ~env ~cmd
      ~timeout_secs ~head_lines ~tail_lines () =
    let open Lwt.Syntax in
    let proc =
      match cmd with
      | "", argv -> Process_group.start ?cwd ~env (Process_group.Exec argv)
      | command, [||] ->
          Process_group.start ?cwd ~env (Process_group.Shell command)
      | command, argv ->
          Process_group.start ?cwd ~env
            (Process_group.Exec (Array.append [| command |] argv))
    in
    let stdout_buf = Buffer.create 1024 in
    let stderr_buf = Buffer.create 256 in
    let runner_result, runner_wakener = Lwt.wait () in
    let forced_result = ref None in
    let bg_job : Bg_shell.job option ref = ref None in
    let finish_runner result =
      if Lwt.is_sleeping runner_result then
        Lwt.wakeup_later runner_wakener result
    in
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            Lwt.finalize
              (fun () ->
                let* _ =
                  Lwt.both
                    (read_channel ?on_chunk:on_output_chunk
                       proc.Process_group.stdout stdout_buf)
                    (read_channel ?on_chunk:on_output_chunk
                       proc.Process_group.stderr stderr_buf)
                in
                let* status = Process_group.wait proc.pid in
                let exit_code =
                  match status with
                  | Unix.WEXITED n -> n
                  | Unix.WSIGNALED n -> 128 + n
                  | Unix.WSTOPPED n -> 128 + n
                in
                (match !bg_job with
                | Some job ->
                    Bg_shell.complete job ~exit_code
                      ~stdout:(Buffer.contents stdout_buf)
                      ~stderr:(Buffer.contents stderr_buf)
                | None -> ());
                finish_runner
                  (Ok
                     (render_command_result ~exit_code
                        ~stdout:(Buffer.contents stdout_buf)
                        ~stderr:(Buffer.contents stderr_buf)
                        ~head_lines ~tail_lines));
                Lwt.return_unit)
              (fun () ->
                match !bg_job with
                | Some _ -> Lwt.return_unit
                | None -> Process_group.close proc))
          (fun exn ->
            (match !bg_job with
            | Some job -> Bg_shell.fail_job job ~msg:(Printexc.to_string exn)
            | None -> ());
            finish_runner (Error exn);
            Lwt.return_unit));
    let timeout =
      let* () = Lwt_unix.sleep timeout_secs in
      let output =
        Printf.sprintf "Error: command timed out after %.0f seconds"
          timeout_secs
      in
      forced_result := Some output;
      let* () = Process_group.terminate proc.pid in
      let* _ = runner_result in
      Lwt.return (`Done output)
    in
    let interrupt =
      match interrupt_check with
      | None -> fst (Lwt.wait ())
      | Some _ ->
          let* () = wait_for_interrupt interrupt_check in
          let job =
            Bg_shell.create ~pid:proc.pid
              ~command:(shell_command_display cmd)
              ~cwd
          in
          bg_job := Some job;
          let msg = Bg_shell.format_job_info job in
          forced_result := Some msg;
          Lwt.return (`Done msg)
    in
    let* outcome =
      Lwt.pick
        [
          (let* result = runner_result in
           match !forced_result with
           | Some output -> Lwt.return (`Done output)
           | None -> Lwt.return (`Runner result));
          timeout;
          interrupt;
        ]
    in
    match outcome with
    | `Runner (Ok result) -> Lwt.return result
    | `Runner (Error exn) -> Lwt.fail exn
    | `Done result -> Lwt.return result
  in
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

let thread_summary ~db ~(config : Runtime_config.t) =
  {
    Tool.name = "thread_summary";
    description =
      "Get a concise dot-point summary of what a session is working on. \
       Focuses on recent activity. Useful for understanding a thread at a \
       glance.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "session_id",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Session key to summarize (required)" );
                    ] );
              ] );
          ("required", `List [ `String "session_id" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Lwt.Syntax in
        let open Yojson.Safe.Util in
        let session_id =
          try args |> member "session_id" |> to_string with _ -> ""
        in
        if session_id = "" then
          Lwt.return
            "Error: session_id is required. Provide the session key to \
             summarize (e.g., \"default\" or a channel-specific session key)."
        else
          let all_msgs = Memory.load_history ~db ~session_key:session_id in
          if all_msgs = [] then
            Lwt.return
              (Printf.sprintf
                 "No messages found for session '%s'. Check the session key or \
                  use 'session list' to see active sessions."
                 session_id)
          else
            let n = List.length all_msgs in
            let window = min 40 n in
            let recent = List.filteri (fun i _ -> i >= n - window) all_msgs in
            let conversation =
              List.map
                (fun (m : Provider.message) ->
                  let snippet =
                    if String.length m.content > 800 then
                      String.sub m.content 0 800 ^ "..."
                    else m.content
                  in
                  Printf.sprintf "[%s]: %s" m.role snippet)
                recent
              |> String.concat "\n"
            in
            let prompt =
              "Summarize what this session is working on. Be concise. Use \
               dot-point form. Focus on the most recent activity and current \
               state. Max 10 bullet points.\n\n" ^ conversation
            in
            let obs_config = Session_observer.observer_config_for ~config in
            let messages =
              [
                Provider.make_message ~role:"system"
                  ~content:
                    "You are a session summarizer. Output a concise dot-point \
                     summary of the session's current focus and recent \
                     actions. No preamble.";
                Provider.make_message ~role:"user" ~content:prompt;
              ]
            in
            Lwt.catch
              (fun () ->
                let* response =
                  Provider.complete ~config:obs_config ~messages ()
                in
                match response with
                | Provider.Text { content; _ } ->
                    Lwt.return (String.trim content)
                | Provider.ToolCalls _ ->
                    Lwt.return
                      "Summary unavailable (unexpected tool call response)")
              (fun exn ->
                Lwt.return
                  ("Error generating summary: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let unsummarize ~db =
  {
    Tool.name = "unsummarize";
    description =
      "Retrieve the original (unsummarized) content of a previously summarized \
       tool result. Use this when you need the full output that was \
       automatically summarized. Usually the summary is sufficient — only call \
       this when you need exact text, specific line ranges, or data the \
       summary explicitly notes was omitted.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "summary_id",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "The summary ID, e.g. sum_abc123def456 (required)" );
                    ] );
                ( "lines",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "Max lines to return (default: 100)" );
                    ] );
                ( "offset",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Line offset to start from (default: 0). Ignored \
                           when head_and_tail=true." );
                    ] );
                ( "with_context",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Include context available during summarization \
                           (default: false)" );
                    ] );
                ( "head_and_tail",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Return first and last N lines instead of contiguous \
                           slice (default: false)" );
                    ] );
              ] );
          ("required", `List [ `String "summary_id" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let summary_id =
          try args |> member "summary_id" |> to_string with _ -> ""
        in
        if summary_id = "" then
          Lwt.return
            "Error: parameter \"summary_id\" is required. Provide the summary \
             ID from the [Auto-summarized] header (e.g., sum_abc123def456)."
        else
          let lines = try args |> member "lines" |> to_int with _ -> 100 in
          let offset = try args |> member "offset" |> to_int with _ -> 0 in
          let with_context =
            try args |> member "with_context" |> to_bool with _ -> false
          in
          let head_and_tail =
            try args |> member "head_and_tail" |> to_bool with _ -> false
          in
          match Summary_store.find ~db ~summary_id with
          | None ->
              Lwt.return
                (Printf.sprintf
                   "Error: summary ID %S not found. The original content may \
                    have been purged (TTL expired) or the ID may be incorrect. \
                    Check the summary_id from the [Auto-summarized] header."
                   summary_id)
          | Some record ->
              let all_lines =
                String.split_on_char '\n' record.original_content
                |> Array.of_list
              in
              let total_lines = Array.length all_lines in
              let lines = max 1 (min lines total_lines) in
              let result_text =
                if head_and_tail then
                  if total_lines <= lines * 2 then
                    (* Content fits — return all *)
                    record.original_content
                  else
                    let head =
                      Array.sub all_lines 0 lines
                      |> Array.to_list |> String.concat "\n"
                    in
                    let tail =
                      Array.sub all_lines (total_lines - lines) lines
                      |> Array.to_list |> String.concat "\n"
                    in
                    let skipped = total_lines - (lines * 2) in
                    Printf.sprintf "%s\n--- (skipped %d lines) ---\n%s" head
                      skipped tail
                else
                  let offset = max 0 (min offset (total_lines - 1)) in
                  let avail = total_lines - offset in
                  let n = min lines avail in
                  Array.sub all_lines offset n
                  |> Array.to_list |> String.concat "\n"
              in
              let from_line, to_line =
                if head_and_tail then
                  if total_lines <= lines * 2 then (0, total_lines - 1)
                  else (0, total_lines - 1) (* head + skipped + tail *)
                else
                  let offset = max 0 (min offset (total_lines - 1)) in
                  let avail = total_lines - offset in
                  let n = min lines avail in
                  (offset, offset + n - 1)
              in
              let header =
                if head_and_tail && total_lines > lines * 2 then
                  Printf.sprintf
                    "[Original for %s: %d lines, %d bytes, showing lines 0-%d \
                     and %d-%d]"
                    summary_id total_lines record.original_bytes (lines - 1)
                    (total_lines - lines) (total_lines - 1)
                else
                  Printf.sprintf
                    "[Original for %s: %d lines, %d bytes, showing lines %d-%d]"
                    summary_id total_lines record.original_bytes from_line
                    to_line
              in
              let context_section =
                if with_context && record.context_snippet <> "" then
                  Printf.sprintf "\n\n[Context at summarization time:]\n%s"
                    record.context_snippet
                else ""
              in
              Lwt.return
                (Printf.sprintf "%s\n%s%s" header result_text context_section));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

(* B679: send_to_session — cross-session messaging agent tool.
   Allows an agent in one session to send a message to another session's
   channel without waking the target agent (by default). Used for cron-
   driven briefings to deliver results to a DM session. *)
let send_to_session ~(session_mgr : Session.t option) ?(db = None) () =
  (* Rate limit state: session_key -> (count, window_start). Max 20 per hour. *)
  let rate_limit_state : (string, int * float) Hashtbl.t = Hashtbl.create 32 in
  let max_sends_per_hour = 20 in
  let window_seconds = 3600.0 in
  let check_rate_limit ~caller_key =
    let now = Unix.gettimeofday () in
    match Hashtbl.find_opt rate_limit_state caller_key with
    | None ->
        Hashtbl.replace rate_limit_state caller_key (1, now);
        true
    | Some (count, window_start) ->
        if now -. window_start > window_seconds then (
          Hashtbl.replace rate_limit_state caller_key (1, now);
          true)
        else if count >= max_sends_per_hour then false
        else (
          Hashtbl.replace rate_limit_state caller_key (count + 1, window_start);
          true)
  in
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "session_id",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String
                        "Target session key to send the message to (required)"
                    );
                  ] );
              ( "message",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "Message text to send (required)");
                  ] );
              ( "wake_agent",
                `Assoc
                  [
                    ("type", `String "boolean");
                    ( "description",
                      `String
                        "If true, trigger the target session's agent loop. \
                         Default: false (silent delivery)." );
                  ] );
              ( "store_in_history",
                `Assoc
                  [
                    ("type", `String "boolean");
                    ( "description",
                      `String
                        "If true, persist the message in the target session's \
                         chat history so the agent sees it on next wake. \
                         Default: true." );
                  ] );
            ] );
        ("required", `List [ `String "session_id"; `String "message" ]);
        ("additionalProperties", `Bool false);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"send_to_session" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "send_to_session";
    description =
      "Send a message to another session's channel. By default the message \
       arrives silently without waking the target session's agent (like a \
       notification). Set wake_agent=true to trigger the agent loop. Use \
       store_in_history=true (default) to persist the message so the target \
       agent sees it on next wake. Rate limited to 20 sends per hour per \
       caller session.";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        let open Lwt.Syntax in
        let open Yojson.Safe.Util in
        let caller_key =
          match context with Some ctx -> ctx.Tool.session_key | None -> None
        in
        (* Rate limit check *)
        let* rate_err =
          match caller_key with
          | Some key ->
              if not (check_rate_limit ~caller_key:key) then
                Lwt.return
                  (Printf.sprintf
                     "Error: rate limit exceeded (max %d sends per hour). Wait \
                      before sending more cross-session messages."
                     max_sends_per_hour)
              else Lwt.return ""
          | None -> Lwt.return ""
        in
        if rate_err <> "" then Lwt.return rate_err
        else
          let session_id =
            try args |> member "session_id" |> to_string with _ -> ""
          in
          let message =
            try args |> member "message" |> to_string with _ -> ""
          in
          if session_id = "" then
            Lwt.return
              (param_err "parameter 'session_id' must be a non-empty string")
          else if message = "" then
            Lwt.return
              (param_err "parameter 'message' must be a non-empty string")
          else
            let wake_agent =
              try args |> member "wake_agent" |> to_bool with _ -> false
            in
            let store_in_history =
              try args |> member "store_in_history" |> to_bool with _ -> true
            in
            match session_mgr with
            | None ->
                Lwt.return
                  "Error: no session manager available (send_to_session \
                   requires a live daemon)."
            | Some mgr -> (
                let sanitized_id = Session.sanitize_session_key session_id in
                (* Check target session channel routing *)
                let channel_info =
                  match db with
                  | Some db ->
                      Memory.get_session_channel ~db ~session_key:sanitized_id
                  | None -> None
                in
                (match channel_info with
                | Some _ -> ()
                | None ->
                    (* Also try parsing from key format directly *)
                    ());
                let channel, channel_id =
                  match Restart_notify.parse_channel_from_key sanitized_id with
                  | Some (ch, ch_id) -> (Some ch, Some ch_id)
                  | None -> (None, None)
                in
                if wake_agent then
                  (* Full Session.turn — wakes agent in target session *)
                  Lwt.catch
                    (fun () ->
                      let open Lwt.Syntax in
                      let* response =
                        Session.turn mgr ~key:sanitized_id ~message ?channel
                          ?channel_id ()
                      in
                      if Session.should_suppress_response response then
                        Lwt.return "Message queued for busy target session."
                      else
                        Lwt.return
                          "Message sent and target session agent triggered.")
                    (fun exn ->
                      Lwt.return
                        (Printf.sprintf "Error sending to session %s: %s"
                           sanitized_id (Printexc.to_string exn)))
                else
                  (* Silent delivery via notifier (does not wake agent) *)
                  let notify_opt =
                    match
                      Session_core.find_silent_channel_notifier mgr
                        ~key:sanitized_id
                    with
                    | Some _ as s -> s
                    | None ->
                        Session_core.find_registered_notifier mgr
                          ~key:sanitized_id
                  in
                  match notify_opt with
                  | Some notify ->
                      Lwt.catch
                        (fun () ->
                          let open Lwt.Syntax in
                          let* () = notify message in
                          (* Optionally persist to message history *)
                          let* () =
                            if store_in_history then
                              match db with
                              | Some db ->
                                  let msg =
                                    Provider.make_message ~role:"event"
                                      ~content:
                                        ("[cross-session message]\n" ^ message)
                                  in
                                  Memory.store_message ~db
                                    ~session_key:sanitized_id msg;
                                  Lwt.return_unit
                              | None -> Lwt.return_unit
                            else Lwt.return_unit
                          in
                          Lwt.return
                            (Printf.sprintf
                               "Message sent silently to session %s. Agent in \
                                target session will see this on next wake \
                                (store_in_history=%b)."
                               sanitized_id store_in_history))
                        (fun exn ->
                          Lwt.return
                            (Printf.sprintf "Error sending to session %s: %s"
                               sanitized_id (Printexc.to_string exn)))
                  | None ->
                      (* No notifier registered — try Session.turn which will
                       use channel routing if available *)
                      Lwt.catch
                        (fun () ->
                          let open Lwt.Syntax in
                          let* response =
                            Session.turn mgr ~key:sanitized_id ~message ?channel
                              ?channel_id ()
                          in
                          let* () =
                            if store_in_history then
                              match db with
                              | Some db ->
                                  let msg =
                                    Provider.make_message ~role:"event"
                                      ~content:
                                        ("[cross-session message]\n" ^ message)
                                  in
                                  Memory.store_message ~db
                                    ~session_key:sanitized_id msg;
                                  Lwt.return_unit
                              | None -> Lwt.return_unit
                            else Lwt.return_unit
                          in
                          Lwt.return
                            (Printf.sprintf
                               "Message sent to session %s via direct turn (no \
                                channel notifier registered)."
                               sanitized_id))
                        (fun exn ->
                          Lwt.return
                            (Printf.sprintf
                               "Error: could not send to session %s — no \
                                channel notifier registered and Session.turn \
                                failed: %s"
                               sanitized_id (Printexc.to_string exn)))));
    invoke_stream = None;
    risk_level = Tool.Medium;
    deferred = false;
  }
