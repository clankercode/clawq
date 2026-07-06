(** Periodic HEARTBEAT.md processing for opted-in channel sessions. *)

let in_quiet_hours ~(quiet_start : int) ~(quiet_end : int) ~hour =
  if quiet_start > quiet_end then hour >= quiet_start || hour < quiet_end
  else hour >= quiet_start && hour < quiet_end

let read_heartbeat_file ~workspace =
  let hb_path = Filename.concat workspace "HEARTBEAT.md" in
  if Sys.file_exists hb_path then
    try
      let ic = open_in hb_path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let n = in_channel_length ic in
          let buf = Bytes.create n in
          really_input ic buf 0 n;
          String.trim (Bytes.to_string buf))
    with _ -> ""
  else ""

let process_heartbeat_for_session ~session_manager ~content key =
  let open Lwt.Syntax in
  let* result =
    Lwt.catch
      (fun () ->
        Logs.info (fun m ->
            m "Heartbeat: processing HEARTBEAT.md (%d chars) on %s"
              (String.length content) key);
        let* result =
          Session.with_suppressed_channel_output session_manager ~key (fun () ->
              Session.try_turn session_manager ~key ~message:content ())
        in
        match result with
        | Some response -> Lwt.return_some response
        | None ->
            Logs.info (fun m ->
                m "Heartbeat: session %s busy, skipping this tick" key);
            Lwt.return_none)
      (fun exn ->
        Logs.err (fun m ->
            m "Heartbeat error for %s: %s" key (Printexc.to_string exn));
        Lwt.return_none)
  in
  match result with
  | None -> Lwt.return_unit
  | Some response ->
      Daemon_resume.handle_heartbeat_response ~session_manager ~key ~response ()

let tick ~workspace ~session_manager =
  let open Lwt.Syntax in
  let cur_hb = (Session.get_config session_manager).heartbeat in
  let* () = Lwt_unix.sleep (float_of_int cur_hb.interval_seconds) in
  let cur_hb = (Session.get_config session_manager).heartbeat in
  if not cur_hb.enabled then begin
    Logs.debug (fun m -> m "Heartbeat: disabled, skipping tick");
    Lwt.return_unit
  end
  else
    let tm = Unix.localtime (Unix.gettimeofday ()) in
    let hour = tm.Unix.tm_hour in
    if
      in_quiet_hours ~quiet_start:cur_hb.quiet_start ~quiet_end:cur_hb.quiet_end
        ~hour
    then begin
      Logs.debug (fun m -> m "Heartbeat: quiet hours, skipping");
      Lwt.return_unit
    end
    else
      let content = read_heartbeat_file ~workspace in
      if content = "" then Lwt.return_unit
      else if Session.is_draining session_manager then begin
        Logs.info (fun m -> m "Heartbeat: daemon draining, skipping turn");
        Lwt.return_unit
      end
      else
        let keys = Session.list_heartbeat_session_keys session_manager in
        if keys = [] then begin
          Logs.debug (fun m ->
              m "Heartbeat: no opted-in sessions, skipping tick");
          Lwt.return_unit
        end
        else
          Lwt_list.iter_s
            (process_heartbeat_for_session ~session_manager ~content)
            keys

let start ~(config : Runtime_config.t) ~workspace ~session_manager =
  let hb = config.heartbeat in
  Logs.info (fun m ->
      m "Heartbeat loop started: enabled=%b interval=%ds quiet=%d:00-%d:00"
        hb.enabled hb.interval_seconds hb.quiet_start hb.quiet_end);
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          let rec hb_loop () =
            let open Lwt.Syntax in
            let* () = tick ~workspace ~session_manager in
            hb_loop ()
          in
          hb_loop ())
        (fun exn ->
          Logs.err (fun m ->
              m "Heartbeat loop error: %s" (Printexc.to_string exn));
          Lwt.return_unit))
