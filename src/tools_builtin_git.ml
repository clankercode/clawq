(* ───── Git operations tool ───── *)

let sanitize_git_arg arg =
  let arg_low = String.lowercase_ascii arg in
  let dangerous_prefixes =
    [
      "--exec=";
      "--upload-pack=";
      "--receive-pack=";
      "--pager=";
      "--editor=";
      "--config=";
    ]
  in
  let ok_prefixes =
    not
      (List.exists
         (fun p ->
           String.length arg >= String.length p
           && String.sub arg_low 0 (String.length p) = p)
         dangerous_prefixes)
  in
  ok_prefixes
  && String.lowercase_ascii arg <> "--no-verify"
  && (not
        (Tools_builtin_fs.contains_substr ~haystack:arg ~needle:"$("
           ~case_sensitive:true))
  && (not
        (Tools_builtin_fs.contains_substr ~haystack:arg ~needle:"`"
           ~case_sensitive:true))
  && (not (String.contains arg '|'))
  && (not (String.contains arg ';'))
  && (not (String.contains arg '>'))
  && not
       (arg = "-c" || arg = "-C"
       || String.length arg > 2
          && arg.[0] = '-'
          && (arg.[1] = 'c' || arg.[1] = 'C')
          && arg.[2] = '=')

let git_operations ~workspace =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "operation",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "enum",
                      `List
                        [
                          `String "status";
                          `String "diff";
                          `String "log";
                          `String "branch";
                          `String "add";
                          `String "commit";
                          `String "checkout";
                          `String "stash";
                          `String "show";
                        ] );
                    ( "description",
                      `String "Git operation to perform (required)" );
                  ] );
              ( "paths",
                `Assoc
                  [
                    ( "oneOf",
                      `List
                        [
                          `Assoc [ ("type", `String "string") ];
                          `Assoc
                            [
                              ("type", `String "array");
                              ("items", `Assoc [ ("type", `String "string") ]);
                            ];
                        ] );
                    ( "description",
                      `String "File paths (for add/diff/show; string or array)"
                    );
                  ] );
              ( "message",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "Commit message (for commit)");
                  ] );
              ( "branch",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "Branch name (for checkout/branch)");
                  ] );
              ( "cached",
                `Assoc
                  [
                    ("type", `String "boolean");
                    ("description", `String "Show staged changes (for diff)");
                  ] );
              ( "limit",
                `Assoc
                  [
                    ("type", `String "integer");
                    ("description", `String "Log entry count (default 10)");
                  ] );
              ( "repo_path",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String
                        "Absolute path to the git repo or worktree root. When \
                         omitted, defaults to the workspace directory. Use \
                         this when operating on a repo outside the workspace \
                         (e.g. ~/src/myproject or a git worktree)." );
                  ] );
            ] );
        ("required", `List [ `String "operation" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"git_operations" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "git_operations";
    description =
      "Perform structured Git operations: status, diff, log, branch, add, \
       commit, checkout, stash, show";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let interrupt_check =
          match context with Some c -> c.Tool.interrupt_check | None -> None
        in
        let operation =
          try args |> member "operation" |> to_string with _ -> ""
        in
        let message =
          try args |> member "message" |> to_string with _ -> ""
        in
        let branch = try args |> member "branch" |> to_string with _ -> "" in
        let cached = try args |> member "cached" |> to_bool with _ -> false in
        let limit = try args |> member "limit" |> to_int with _ -> 10 in
        let repo_path =
          try
            match args |> member "repo_path" with
            | `String s when s <> "" -> Some s
            | _ -> None
          with _ -> None
        in
        let paths =
          try
            match args |> member "paths" with
            | `String s -> [ s ]
            | `List items ->
                List.filter_map
                  (fun v -> try Some (to_string v) with _ -> None)
                  items
            | _ -> []
          with _ -> []
        in
        if operation = "" then
          Lwt.return
            (param_err
               "parameter 'operation' must be a non-empty string \
                (status|diff|log|branch|add|commit|checkout|stash|show)")
        else
          let cwd_result =
            match repo_path with
            | None -> Ok workspace
            | Some p when not (Filename.is_relative p) -> Ok p
            | Some p ->
                Error
                  (Printf.sprintf
                     "Error: repo_path must be an absolute path starting with \
                      \"/\". Received: %S. Provide an absolute path like \
                      \"/home/user/src/myproject\" or omit repo_path to use \
                      the default workspace."
                     p)
          in
          match cwd_result with
          | Error err -> Lwt.return err
          | Ok cwd -> (
              let build_argv () =
                match operation with
                | "status" -> Ok [ "git"; "status"; "--short" ]
                | "diff" ->
                    let argv = [ "git"; "diff" ] in
                    let argv = if cached then argv @ [ "--cached" ] else argv in
                    let argv = if paths <> [] then argv @ paths else argv in
                    Ok argv
                | "log" ->
                    Ok
                      [ "git"; "log"; "--oneline"; Printf.sprintf "-n%d" limit ]
                | "branch" ->
                    if branch <> "" then Ok [ "git"; "branch"; branch ]
                    else Ok [ "git"; "branch"; "-a" ]
                | "add" ->
                    if paths = [] then Error "Error: paths required for add"
                    else Ok ("git" :: "add" :: paths)
                | "commit" ->
                    if message = "" then
                      Error "Error: message required for commit"
                    else Ok [ "git"; "commit"; "-m"; message ]
                | "checkout" ->
                    if branch = "" && paths = [] then
                      Error "Error: branch or paths required for checkout"
                    else if branch <> "" then Ok [ "git"; "checkout"; branch ]
                    else Ok ("git" :: "checkout" :: "--" :: paths)
                | "stash" -> Ok [ "git"; "stash" ]
                | "show" ->
                    let argv = [ "git"; "show"; "--stat" ] in
                    let argv =
                      if paths <> [] then argv @ [ "--" ] @ paths else argv
                    in
                    Ok argv
                | op ->
                    Error (Printf.sprintf "Error: unknown operation '%s'" op)
              in
              match build_argv () with
              | Error msg -> Lwt.return msg
              | Ok argv -> (
                  (* Sanitize user-supplied inputs: paths and branch name.
                 The commit message is intentionally excluded — it is passed
                 as an execve argument, not interpreted by a shell, so shell
                 metacharacters in a commit message are safe. *)
                  let user_inputs =
                    paths @ if branch <> "" then [ branch ] else []
                  in
                  let safe = List.for_all sanitize_git_arg user_inputs in
                  if not safe then
                    Lwt.return
                      "Error: git arguments contain disallowed patterns"
                  else
                    let open Lwt.Syntax in
                    let env =
                      Array.append
                        (Runtime_config.workspace_only_env ())
                        [| "GIT_TERMINAL_PROMPT=0" |]
                    in
                    let proc =
                      Process_group.start ~cwd ~env
                        (Process_group.Exec (Array.of_list argv))
                    in
                    let runner_result, runner_wakener = Lwt.wait () in
                    let forced_result = ref None in
                    let finish_runner result =
                      if Lwt.is_sleeping runner_result then
                        Lwt.wakeup_later runner_wakener result
                    in
                    Lwt.async (fun () ->
                        Lwt.catch
                          (fun () ->
                            Lwt.finalize
                              (fun () ->
                                let* stdout, stderr =
                                  Lwt.both
                                    (Lwt_io.read proc.Process_group.stdout)
                                    (Lwt_io.read proc.Process_group.stderr)
                                in
                                let* status = Process_group.wait proc.pid in
                                let exit_code =
                                  match status with
                                  | Unix.WEXITED n -> n
                                  | Unix.WSIGNALED n -> 128 + n
                                  | Unix.WSTOPPED n -> 128 + n
                                in
                                let output =
                                  (if stdout <> "" then stdout else "")
                                  ^ if stderr <> "" then stderr else ""
                                in
                                finish_runner
                                  (Ok
                                     (if exit_code = 0 then
                                        if output = "" then "(no output)"
                                        else output
                                      else
                                        Printf.sprintf "exit_code: %d\n%s"
                                          exit_code output));
                                Lwt.return_unit)
                              (fun () -> Process_group.close proc))
                          (fun exn ->
                            finish_runner (Error exn);
                            Lwt.return_unit));
                    let timeout =
                      let* () = Lwt_unix.sleep 30.0 in
                      forced_result :=
                        Some "Error: git timed out after 30 seconds";
                      let* () = Process_group.terminate proc.pid in
                      let* _ = runner_result in
                      Lwt.return (`Done "Error: git timed out after 30 seconds")
                    in
                    let interrupt =
                      match interrupt_check with
                      | None -> fst (Lwt.wait ())
                      | Some check ->
                          let rec wait () =
                            match check () with
                            | Some reason
                              when reason
                                   <> Agent.queued_message_interrupt_token ->
                                Lwt.return_unit
                            | _ ->
                                let* () = Lwt_unix.sleep 0.05 in
                                wait ()
                          in
                          let* () = wait () in
                          forced_result :=
                            Some "Git command interrupted by user.";
                          let* () =
                            Process_group.terminate_immediately proc.pid
                          in
                          let* _ = runner_result in
                          Lwt.return (`Done "Git command interrupted by user.")
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
                    | `Done result -> Lwt.return result)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }
