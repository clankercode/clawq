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

let stream_process ~cwd ~argv ~send_progress =
  let proc = Lwt_process.open_process_full ~cwd ("", argv) in
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
  let* () = Lwt.join [ read_channel proc#stdout; read_channel proc#stderr ] in
  let* status = proc#close in
  Lwt.return (exit_code_of_status status)

let binary_url_of_env () =
  match Sys.getenv_opt "CLAWQ_UPDATE_BINARY_URL" with
  | Some url when String.trim url <> "" -> Some (String.trim url)
  | _ -> None

let run_git_update ~repo_root ~run_command ~send_signal ~send_progress
    ~prepare_restart =
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
    let message =
      Printf.sprintf "Build failed (exit %d). Restart aborted." build_exit
    in
    let* () = send_progress message in
    Lwt.return message
  end
  else begin
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
    ~send_progress () =
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
            run_git_update ~repo_root ~run_command ~send_signal ~send_progress
              ~prepare_restart:(run_prepare_restart prepare_restart)
        | Binary, _, Some binary_url | Auto, None, Some binary_url ->
            run_binary_update ~binary_url ~target_path ~run_command ~send_signal
              ~send_progress
              ~prepare_restart:(run_prepare_restart prepare_restart)
        | Binary, _, None ->
            let message =
              "Binary update mode requires CLAWQ_UPDATE_BINARY_URL to be set."
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
      finish_update

let tool ~is_draining ?claim_update ?finish_update () =
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
          (fun () ->
            (match Restart_notify.parse_channel_from_key key with
            | Some (channel, channel_id) ->
                Restart_notify.write ~channel ~channel_id
            | None -> ());
            Lwt.return (Ok ()))
      | _ -> fun () -> Lwt.return (Ok ())
    in
    run_update ?claim_update ?finish_update ~mode ~is_draining
      ~prepare_restart ~send_progress ()
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
