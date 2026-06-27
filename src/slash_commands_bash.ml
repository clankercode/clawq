let shell_exec_tool_name = "shell_exec"

let room_profile_tool_denial ?(config = Runtime_config.default) ?session_key ()
    =
  match session_key with
  | None -> None
  | Some session_key ->
      Runtime_config.room_profile_tool_denial_for_session config ~session_key
        ~tool_name:shell_exec_tool_name

let run_bash_command ?(timeout_secs = 60.0) ?config ?session_key cmd =
  match room_profile_tool_denial ?config ?session_key () with
  | Some msg -> Lwt.return (Error msg)
  | None ->
      let env = Unix.environment () in
      let proc = Process_group.start ~env (Exec [| "/bin/bash"; "-c"; cmd |]) in
      let read_all ch =
        Lwt.catch (fun () -> Lwt_io.read ch) (fun _ -> Lwt.return "")
      in
      let forced_result = ref None in
      let closed = ref false in
      let safe_close () =
        if not !closed then begin
          closed := true;
          Process_group.close proc
        end
        else Lwt.return_unit
      in
      let runner () =
        let open Lwt.Syntax in
        let* stdout, stderr =
          Lwt.both (read_all proc.stdout) (read_all proc.stderr)
        in
        let* status = Process_group.wait proc.pid in
        let* () = safe_close () in
        match !forced_result with
        | Some msg -> Lwt.return (Error msg)
        | None ->
            let exit_code =
              match status with
              | Unix.WEXITED n -> n
              | Unix.WSIGNALED n -> 128 + n
              | Unix.WSTOPPED n -> 128 + n
            in
            Lwt.return (Ok (exit_code, stdout, stderr))
      in
      let timeout () =
        let open Lwt.Syntax in
        let* () = Lwt_unix.sleep timeout_secs in
        forced_result := Some "command timed out";
        let* () = Process_group.terminate proc.pid in
        let* _ = Process_group.wait proc.pid in
        let* () = safe_close () in
        Lwt.return (Error "command timed out")
      in
      Lwt.pick [ runner (); timeout () ]

let format_result cmd result =
  match result with
  | Error msg -> Printf.sprintf "$ %s\n%s" cmd msg
  | Ok (exit_code, stdout, stderr) ->
      let buf = Buffer.create 256 in
      Buffer.add_string buf (Printf.sprintf "$ %s\n" cmd);
      if stdout <> "" then Buffer.add_string buf stdout;
      if stderr <> "" then (
        if stdout <> "" && not (String.ends_with ~suffix:"\n" stdout) then
          Buffer.add_char buf '\n';
        Buffer.add_string buf (Printf.sprintf "[stderr]\n%s" stderr));
      if exit_code <> 0 then (
        let contents = Buffer.contents buf in
        if not (String.ends_with ~suffix:"\n" contents) then
          Buffer.add_char buf '\n';
        Buffer.add_string buf (Printf.sprintf "[exit code: %d]" exit_code));
      Buffer.contents buf
