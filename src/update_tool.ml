let is_update_command text =
  String.lowercase_ascii (String.trim text) = "/update"

type update_mode = Auto | Git | Binary

let update_mode_of_string = function
  | "auto" -> Some Auto
  | "git" -> Some Git
  | "binary" -> Some Binary
  | _ -> None

let string_of_update_mode = function
  | Auto -> "auto"
  | Git -> "git"
  | Binary -> "binary"

let normalize_executable_path ?start_path () =
  let path = Option.value start_path ~default:Sys.executable_name in
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let normalize_start_dir ?start_path () =
  let path = normalize_executable_path ?start_path () in
  Filename.dirname path

let rec find_repo_root_from ?exists dir =
  let exists = Option.value exists ~default:Sys.file_exists in
  if
    exists (Filename.concat dir "dune-project")
    || exists (Filename.concat dir ".git")
  then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_repo_root_from ~exists parent

let find_repo_root ?start_path ?exists () =
  find_repo_root_from ?exists (normalize_start_dir ?start_path ())

let exit_code_of_status = function
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n -> 128 + n
  | Unix.WSTOPPED n -> 128 + n

exception Interrupted_by_user

let should_interrupt interrupt_check =
  match interrupt_check with
  | Some check -> (
      match check () with
      | Some reason when reason <> Agent.queued_message_interrupt_token -> true
      | _ -> false)
  | None -> false

let wait_for_interrupt interrupt_check =
  let open Lwt.Syntax in
  let rec loop () =
    if should_interrupt interrupt_check then Lwt.return_unit
    else
      let* () = Lwt_unix.sleep 0.05 in
      loop ()
  in
  loop ()

let stream_process ~cwd ~argv ~send_progress ~interrupt_check =
  let proc =
    Process_group.start ~cwd ~env:(Unix.environment ())
      (Process_group.Exec argv)
  in
  let read_channel ch =
    let rec loop () =
      let open Lwt.Syntax in
      let* line = Lwt_io.read_line_opt ch in
      match line with
      | None -> Lwt.return_unit
      | Some text ->
          let* () = send_progress text in
          loop ()
    in
    loop ()
  in
  let open Lwt.Syntax in
  let runner_result, runner_wakener = Lwt.wait () in
  let finish_runner result =
    if Lwt.is_sleeping runner_result then Lwt.wakeup_later runner_wakener result
  in
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          Lwt.finalize
            (fun () ->
              let* () =
                Lwt.join
                  [
                    read_channel proc.Process_group.stdout;
                    read_channel proc.Process_group.stderr;
                  ]
              in
              let* status = Process_group.wait proc.pid in
              finish_runner (Ok (exit_code_of_status status));
              Lwt.return_unit)
            (fun () -> Process_group.close proc))
        (fun exn ->
          finish_runner (Error exn);
          Lwt.return_unit));
  match interrupt_check with
  | None -> (
      let* result = runner_result in
      match result with
      | Ok exit_code -> Lwt.return exit_code
      | Error exn -> Lwt.fail exn)
  | Some _ -> (
      let* outcome =
        Lwt.pick
          [
            (let* result = runner_result in
             Lwt.return (`Finished result));
            (let* () = wait_for_interrupt interrupt_check in
             Lwt.return `Interrupted);
          ]
      in
      match outcome with
      | `Finished (Ok exit_code) -> Lwt.return exit_code
      | `Finished (Error exn) -> Lwt.fail exn
      | `Interrupted ->
          let* () = Process_group.terminate_immediately proc.pid in
          let* _ = runner_result in
          Lwt.fail Interrupted_by_user)

let trim s = String.trim s

let contains_sub s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else loop (i + 1)
  in
  if len_sub = 0 then true else loop 0

let summarize_failure_lines lines =
  let cleaned =
    lines |> List.map trim |> List.filter (fun s -> s <> "") |> List.rev
  in
  let dune_lock =
    List.find_opt
      (fun line ->
        contains_sub line "Dune build lock present"
        || contains_sub line "_build/.lock")
      cleaned
  in
  match dune_lock with
  | Some line ->
      "Build blocked by Dune lock contention. Another build may still \
       be        running, or a stale _build/.lock may need cleanup. Detail: "
      ^ line
  | None -> (
      match cleaned with line :: _ -> line | [] -> "no build output captured")

let binary_url_of_env () =
  match Sys.getenv_opt "CLAWQ_UPDATE_BINARY_URL" with
  | Some url when String.trim url <> "" -> Some (String.trim url)
  | _ -> None

let git_build_output_path ~repo_root =
  Filename.concat repo_root "_build/default/src/main.exe"

let run_git_update ~repo_root ~run_command ~send_signal ~send_progress
    ~prepare_restart =
  let progress_lines = ref [] in
  let send_progress text =
    progress_lines := text :: !progress_lines;
    send_progress text
  in
  let open Lwt.Syntax in
  let* () = send_progress "Starting update..." in
  let* () = send_progress "Mode: git" in
  let* () = send_progress "Running: git pull" in
  let* git_exit =
    run_command ~cwd:repo_root ~argv:[| "git"; "pull" |] ~send_progress
  in
  let* () =
    if git_exit = 0 then Lwt.return_unit
    else
      send_progress
        (Printf.sprintf "git pull failed (exit %d), continuing to build."
           git_exit)
  in
  let* () = send_progress "Running: make build" in
  let* build_exit =
    run_command ~cwd:repo_root ~argv:[| "make"; "build" |] ~send_progress
  in
  if build_exit <> 0 then begin
    let detail = summarize_failure_lines !progress_lines in
    let message =
      Printf.sprintf
        "Build failed (exit %d). Restart aborted. Most relevant detail: %s"
        build_exit detail
    in
    let* () = send_progress message in
    Lwt.return message
  end
  else begin
    let fresh_binary = git_build_output_path ~repo_root in
    if Sys.file_exists fresh_binary then
      Unix.putenv Restart_exec.reexec_path_env fresh_binary;
    let message = "Build complete. Sending restart signal..." in
    let* prepared = prepare_restart ~send_progress in
    match prepared with
    | Some err -> Lwt.return err
    | None ->
        send_signal (Unix.getpid ()) Sys.sigusr1;
        Lwt.return message
  end

let run_prepare_restart prepare_restart ~send_progress =
  let open Lwt.Syntax in
  match prepare_restart with
  | None -> Lwt.return_none
  | Some prepare_restart -> (
      let* () =
        send_progress "Restart requested. Finishing this turn before handoff..."
      in
      let* result = prepare_restart () in
      match result with
      | Ok () -> Lwt.return_none
      | Error err ->
          let* () = send_progress err in
          Lwt.return_some err)

let run_binary_update ~binary_url ~target_path ~run_command ~send_signal
    ~send_progress ~prepare_restart =
  let open Lwt.Syntax in
  let cwd = Filename.dirname target_path in
  let tmp_path = target_path ^ ".download" in
  let* () = send_progress "Starting update..." in
  let* () = send_progress "Mode: binary" in
  let* () = send_progress "Running: curl -fL -o <temp> <binary_url>" in
  let* download_exit =
    run_command ~cwd
      ~argv:[| "curl"; "-fL"; "-o"; tmp_path; binary_url |]
      ~send_progress
  in
  if download_exit <> 0 then begin
    let message =
      Printf.sprintf "Binary download failed (exit %d). Restart aborted."
        download_exit
    in
    let* () = send_progress message in
    Lwt.return message
  end
  else begin
    let* () = send_progress "Running: chmod 755 <temp>" in
    let* chmod_exit =
      run_command ~cwd ~argv:[| "chmod"; "755"; tmp_path |] ~send_progress
    in
    if chmod_exit <> 0 then begin
      let message =
        Printf.sprintf
          "Downloaded binary setup failed (exit %d). Restart aborted."
          chmod_exit
      in
      let* () = send_progress message in
      Lwt.return message
    end
    else begin
      let* () = send_progress "Running: mv <temp> <clawq>" in
      let* move_exit =
        run_command ~cwd ~argv:[| "mv"; tmp_path; target_path |] ~send_progress
      in
      if move_exit <> 0 then begin
        let message =
          Printf.sprintf
            "Replacing executable failed (exit %d). Restart aborted." move_exit
        in
        let* () = send_progress message in
        Lwt.return message
      end
      else begin
        let message = "Binary update complete. Sending restart signal..." in
        let* prepared = prepare_restart ~send_progress in
        match prepared with
        | Some err -> Lwt.return err
        | None ->
            send_signal (Unix.getpid ()) Sys.sigusr1;
            Lwt.return message
      end
    end
  end

let run_update ?(find_repo_root = find_repo_root)
    ?(run_command = stream_process) ?(send_signal = Unix.kill) ?claim_update
    ?(finish_update = fun () -> Lwt.return_unit) ?prepare_restart
    ?(binary_url = binary_url_of_env ()) ?start_path ?(mode = Auto) ~is_draining
    ~send_progress ?(interrupt_check = None) () =
  let open Lwt.Syntax in
  let claim_update =
    match claim_update with
    | Some claim_update -> claim_update
    | None -> fun () -> Lwt.return (not (is_draining ()))
  in
  let message = "Restart already in progress, please wait." in
  let* claimed = claim_update () in
  if not claimed then begin
    let* () = send_progress message in
    Lwt.return message
  end
  else
    Lwt.finalize
      (fun () ->
        Lwt.catch
          (fun () ->
            let repo_root = find_repo_root ?start_path () in
            let target_path = normalize_executable_path ?start_path () in
            match (mode, repo_root, binary_url) with
            | Git, None, _ ->
                let message =
                  "Cannot find repository root, git update mode is unavailable."
                in
                let* () = send_progress message in
                Lwt.return message
            | (Auto | Git), Some repo_root, _ ->
                run_git_update ~repo_root
                  ~run_command:(fun ~cwd ~argv ~send_progress ->
                    run_command ~cwd ~argv ~send_progress ~interrupt_check)
                  ~send_signal ~send_progress
                  ~prepare_restart:(run_prepare_restart prepare_restart)
            | Binary, _, Some binary_url | Auto, None, Some binary_url ->
                run_binary_update ~binary_url ~target_path
                  ~run_command:(fun ~cwd ~argv ~send_progress ->
                    run_command ~cwd ~argv ~send_progress ~interrupt_check)
                  ~send_signal ~send_progress
                  ~prepare_restart:(run_prepare_restart prepare_restart)
            | Binary, _, None ->
                let message =
                  "Binary update mode requires CLAWQ_UPDATE_BINARY_URL to be \
                   set."
                in
                let* () = send_progress message in
                Lwt.return message
            | Auto, None, None ->
                let message =
                  "Cannot find repository root, and binary update mode is not \
                   configured."
                in
                let* () = send_progress message in
                Lwt.return message)
          (function
            | Interrupted_by_user -> Lwt.return "Update interrupted by user."
            | exn -> Lwt.fail exn))
      finish_update

let adjust_offline_result result =
  if result = "Build complete. Sending restart signal..." then
    "Build complete. Next `clawq` invocation will use the updated version."
  else if result = "Binary update complete. Sending restart signal..." then
    "Binary replaced. Next `clawq` invocation will use the updated version."
  else result

let run_offline_update ?(find_repo_root = find_repo_root)
    ?(run_command = stream_process) ?(binary_url = binary_url_of_env ())
    ?start_path ?(mode = Auto) ~send_progress () =
  let open Lwt.Syntax in
  let* () =
    send_progress "Note: no live daemon detected. Running update offline."
  in
  let* result =
    run_update ~find_repo_root ~run_command
      ~send_signal:(fun _ _ -> ())
      ~is_draining:(fun () -> false)
      ~binary_url ?start_path ~mode ~send_progress ()
  in
  Lwt.return (adjust_offline_result result)

(** A step in the update progress checklist. *)
type step_state = Pending | Running | Done | Failed of string

type step = { label : string; mutable state : step_state }

(** Render the progress checklist as a tree-like ASCII display. *)
let render_progress ~mode steps output_tail =
  let buf = Buffer.create 256 in
  let mode_str =
    match mode with Auto -> "auto" | Git -> "git" | Binary -> "binary"
  in
  Buffer.add_string buf
    (Printf.sprintf "Updating clawq (mode: %s)...\n" mode_str);
  let n = List.length steps in
  List.iteri
    (fun i step ->
      let is_last = i = n - 1 in
      let connector = if is_last then "└─ " else "├─ " in
      let icon =
        match step.state with
        | Done -> "✅"
        | Running -> "⏳"
        | Pending -> "⬜"
        | Failed _ -> "❌"
      in
      Buffer.add_string buf
        (Printf.sprintf "%s%s %s\n" connector icon step.label);
      match step.state with
      | Failed detail ->
          let indent = if is_last then "   " else "│  " in
          Buffer.add_string buf (Printf.sprintf "%s └─ %s\n" indent detail)
      | Running -> (
          match output_tail with
          | Some tail when tail <> "" ->
              let indent = if is_last then "   " else "│  " in
              let lines = String.split_on_char '\n' tail in
              let lines =
                let len = List.length lines in
                if len > 4 then
                  let rec drop k = function
                    | [] -> []
                    | _ :: rest when k > 0 -> drop (k - 1) rest
                    | l -> l
                  in
                  drop (len - 4) lines
                else lines
              in
              List.iter
                (fun line ->
                  let truncated =
                    if String.length line > 72 then String.sub line 0 72 ^ "..."
                    else line
                  in
                  Buffer.add_string buf
                    (Printf.sprintf "%s   %s\n" indent truncated))
                lines
          | _ -> ())
      | _ -> ())
    steps;
  let result = Buffer.contents buf in
  let len = String.length result in
  if len > 0 && result.[len - 1] = '\n' then String.sub result 0 (len - 1)
  else result

(** Create a [send_progress] callback that maintains a single editable message
    with an ASCII checklist. Returns [(send_progress, get_final_text)].

    [send_first text] sends an initial message and returns its id.
    [edit msg_id text] edits the message in place. *)
let make_progress_sender ~send_first ~edit ?(throttle = 0.5) ~mode () =
  let msg_id = ref None in
  let steps = ref [] in
  let output_tail = ref None in
  let last_edit = ref 0.0 in
  let flush () =
    let text = render_progress ~mode (List.rev !steps) !output_tail in
    let open Lwt.Syntax in
    match !msg_id with
    | None ->
        let* id = send_first text in
        msg_id := Some id;
        last_edit := Unix.gettimeofday ();
        Lwt.return_unit
    | Some id ->
        let now = Unix.gettimeofday () in
        let elapsed = now -. !last_edit in
        let* () =
          if throttle > 0.0 && elapsed < throttle then
            Lwt_unix.sleep (throttle -. elapsed)
          else Lwt.return_unit
        in
        let* () = edit id text in
        last_edit := Unix.gettimeofday ();
        Lwt.return_unit
  in
  let add_step label state =
    steps := { label; state } :: !steps;
    flush ()
  in
  let update_current state =
    match !steps with
    | step :: _ ->
        step.state <- state;
        flush ()
    | [] -> Lwt.return_unit
  in
  let append_output text =
    output_tail := Some text;
    let now = Unix.gettimeofday () in
    if now -. !last_edit >= 1.0 then flush () else Lwt.return_unit
  in
  let send_progress text =
    let open Lwt.Syntax in
    let trimmed = String.trim text in
    if trimmed = "" then Lwt.return_unit
    else
      let starts_with prefix s =
        String.length s >= String.length prefix
        && String.sub s 0 (String.length prefix) = prefix
      in
      if starts_with "Starting update" trimmed then
        let* () = add_step "Starting" Running in
        Lwt.return_unit
      else if starts_with "Mode:" trimmed then
        let* () = update_current Done in
        Lwt.return_unit
      else if starts_with "Running: git pull" trimmed then
        let* () = add_step "git pull" Running in
        Lwt.return_unit
      else if starts_with "git pull failed" trimmed then
        let* () = update_current (Failed trimmed) in
        Lwt.return_unit
      else if starts_with "Running: make build" trimmed then
        let* () = update_current Done in
        let* () = add_step "make build" Running in
        Lwt.return_unit
      else if starts_with "Running: curl" trimmed then
        let* () = add_step "download binary" Running in
        Lwt.return_unit
      else if starts_with "Running: chmod" trimmed then
        let* () = update_current Done in
        let* () = add_step "set permissions" Running in
        Lwt.return_unit
      else if starts_with "Running: mv" trimmed then
        let* () = update_current Done in
        let* () = add_step "replace executable" Running in
        Lwt.return_unit
      else if
        starts_with "Build complete" trimmed
        || starts_with "Binary update complete" trimmed
      then
        let* () = update_current Done in
        let* () = add_step "restart" Running in
        Lwt.return_unit
      else if starts_with "Build failed" trimmed then
        let* () = update_current (Failed trimmed) in
        Lwt.return_unit
      else if
        starts_with "Binary download failed" trimmed
        || starts_with "Downloaded binary setup failed" trimmed
        || starts_with "Replacing executable failed" trimmed
      then
        let* () = update_current (Failed trimmed) in
        Lwt.return_unit
      else if starts_with "Restart requested" trimmed then
        let* () = update_current Done in
        let* () = add_step "finishing turn" Running in
        Lwt.return_unit
      else if
        starts_with "Restart already" trimmed
        || starts_with "Cannot find" trimmed
        || starts_with "Binary update mode requires" trimmed
      then
        let* () = add_step trimmed (Failed "") in
        Lwt.return_unit
      else
        (* Stream output from subprocess — append to tail *)
        append_output trimmed
  in
  let get_final_text () =
    render_progress ~mode (List.rev !steps) !output_tail
  in
  (send_progress, get_final_text)

let tool ~is_draining ?claim_update ?finish_update ?find_repo_root ?run_command
    ?send_signal () =
  let invoke_common ?context ?on_output_chunk args =
    let open Yojson.Safe.Util in
    let mode =
      try args |> member "mode" |> to_string
      with _ -> string_of_update_mode Auto
    in
    let send_progress =
      match on_output_chunk with
      | Some f -> fun text -> f (text ^ "\n")
      | None -> fun _ -> Lwt.return_unit
    in
    let mode =
      match
        update_mode_of_string (String.lowercase_ascii (String.trim mode))
      with
      | Some mode -> mode
      | None -> Auto
    in
    let prepare_restart =
      match context with
      | Some { Tool.session_key = Some key; _ } ->
          fun () ->
            (match Restart_notify.parse_channel_from_key key with
            | Some (channel, channel_id) ->
                Restart_notify.write ~channel ~channel_id
            | None -> ());
            Lwt.return (Ok ())
      | _ -> fun () -> Lwt.return (Ok ())
    in
    run_update ?find_repo_root ?run_command ?send_signal ?claim_update
      ?finish_update ~mode ~is_draining ~prepare_restart ~send_progress
      ~interrupt_check:
        (match context with Some c -> c.Tool.interrupt_check | None -> None)
      ()
  in
  {
    Tool.name = "update_clawq";
    description =
      "Update clawq by rebuilding from git when available, or by downloading a \
       replacement binary when configured, then trigger a graceful restart.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "mode",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "enum",
                        `List
                          [ `String "auto"; `String "git"; `String "binary" ] );
                      ( "description",
                        `String
                          "Update mode. 'auto' prefers git rebuild when a repo \
                           is present, otherwise binary download if \
                           configured." );
                    ] );
              ] );
          ("additionalProperties", `Bool false);
        ];
    invoke = (fun ?context args -> invoke_common ?context args);
    invoke_stream =
      Some
        (fun ?context ~on_output_chunk args ->
          invoke_common ?context ~on_output_chunk args);
    risk_level = Medium;
    deferred = false;
  }
