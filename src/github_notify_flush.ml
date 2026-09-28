(** Flush queued GitHub room notifications as joined batches. *)

let started = ref false

let send_now ~session_manager ?config ~room_id ~text () =
  let open Lwt.Syntax in
  match Session.find_registered_notifier session_manager ~key:room_id with
  | Some notifier ->
      let* () = notifier text in
      Lwt.return ""
  | None -> (
      match Restart_notify.parse_channel_from_key room_id with
      | Some (channel, channel_id) when channel = "teams" -> (
          match config with
          | Some cfg -> (
              let* result =
                Daemon_resume.dispatch_resumed_message
                  ~senders:Daemon_resume.default_resume_senders ~config:cfg
                  ~channel ~channel_id ~text ()
              in
              match result with
              | Ok () -> Lwt.return ""
              | Error err ->
                  Logs.warn (fun m ->
                      m "GitHub notify batch: fallback send failed for %s: %s"
                        room_id err);
                  Lwt.fail (Failure err))
          | None ->
              Logs.debug (fun m ->
                  m "GitHub notify batch: no config for fallback send to %s"
                    room_id);
              Lwt.return "")
      | _ ->
          Logs.debug (fun m ->
              m "GitHub notify batch: no notifier for room %s" room_id);
          Lwt.return "")

let enqueue_or_send ~session_manager ?config ~room_id ~text () =
  match Session.get_db session_manager with
  | Some db ->
      Github_notify_batch.enqueue ~db ~room_id ~body:text;
      Lwt.return "queued"
  | None -> send_now ~session_manager ?config ~room_id ~text ()

let flush_room ~db ~session_manager ~config ~last_sent ~now room_id =
  let open Lwt.Syntax in
  let last = Option.value (Hashtbl.find_opt last_sent room_id) ~default:0. in
  match Github_notify_batch.oldest_created ~db ~room_id with
  | None -> Lwt.return_unit
  | Some oldest
    when Github_notify_batch.should_flush ~now ~last_sent:last ~oldest
           ~window_s:Github_notify_batch.window_s -> (
      let batch =
        Github_notify_batch.take_oldest ~db ~room_id
          ~limit:Github_notify_batch.max_batch
      in
      match batch with
      | [] -> Lwt.return_unit
      | _ ->
          let text = Github_notify_batch.join_bodies (List.map snd batch) in
          Lwt.catch
            (fun () ->
              let* _id = send_now ~session_manager ~config ~room_id ~text () in
              Github_notify_batch.delete_ids ~db (List.map fst batch);
              Hashtbl.replace last_sent room_id now;
              Logs.info (fun m ->
                  m "GitHub notify batch: sent %d updates to %s"
                    (List.length batch) room_id);
              Lwt.return_unit)
            (fun exn ->
              Hashtbl.replace last_sent room_id now;
              Logs.warn (fun m ->
                  m "GitHub notify batch: send failed for %s: %s" room_id
                    (Printexc.to_string exn));
              Lwt.return_unit))
  | Some _ -> Lwt.return_unit

(* One pass over the rooms with pending batches. Every database or send error
   is absorbed here: the flush loop runs for the life of the daemon, so a
   transient BUSY must not kill the async thread (the uncaught BUSY observed in
   daemon.log killed the previous instance's notify flush). *)
let flush_tick ~db ~session_manager ~config ~last_sent () =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let now = Unix.gettimeofday () in
      let rooms = Github_notify_batch.pending_rooms ~db in
      Lwt_list.iter_s
        (flush_room ~db ~session_manager ~config ~last_sent ~now)
        rooms)
    (fun exn ->
      (try Github_notify_batch.ensure_schema db with _ -> ());
      let error = Printexc.to_string exn in
      (* The tick runs every second; without dedup a sustained lock storm would
         log one warn line per tick. *)
      Log_dedup.warn
        ~key:("github notify batch flush tick failed: " ^ error)
        (Printf.sprintf "GitHub notify batch: flush tick failed: %s" error);
      Lwt.return_unit)

let start ~session_manager ~(config : Runtime_config.t) ~db =
  if !started then ()
  else begin
    started := true;
    (try Github_notify_batch.ensure_schema db
     with exn ->
       Logs.warn (fun m ->
           m "GitHub notify batch: schema init failed: %s"
             (Printexc.to_string exn)));
    let last_sent = Hashtbl.create 8 in
    Lwt.async (fun () ->
        let rec loop () =
          let open Lwt.Syntax in
          let* () = Lwt_unix.sleep 1.0 in
          let* () = flush_tick ~db ~session_manager ~config ~last_sent () in
          loop ()
        in
        loop ())
  end
