type boot_replay_summary = {
  reclaimed_stale_count : int;
  reclaimed_failed_count : int;
  session_count : int;
  total_rows : int;
  replayed_count : int;
  failed_count : int;
}

type replay_payload = {
  message : string;
  is_bang : bool;
  deferred_followup : bool;
  cwd : string option;
}

let boot_replay_summary_message (summary : boot_replay_summary) =
  Printf.sprintf
    "sessions=%d rows=%d reclaimed_stale=%d reclaimed_failed=%d replayed=%d \
     failed=%d"
    summary.session_count summary.total_rows summary.reclaimed_stale_count
    summary.reclaimed_failed_count summary.replayed_count summary.failed_count

let empty_summary =
  {
    reclaimed_stale_count = 0;
    reclaimed_failed_count = 0;
    session_count = 0;
    total_rows = 0;
    replayed_count = 0;
    failed_count = 0;
  }

let parse_replay_payload payload_json =
  try
    let json = Yojson.Safe.from_string payload_json in
    let open Yojson.Safe.Util in
    let message =
      json |> member "message" |> to_string_option |> Option.value ~default:""
    in
    let is_bang =
      json |> member "bang" |> to_bool_option |> Option.value ~default:false
    in
    let deferred_followup =
      json |> member "deferred_followup" |> to_bool_option
      |> Option.value ~default:false
    in
    let cwd =
      try
        match json |> member "cwd" with
        | `String s when String.trim s <> "" -> Some s
        | _ -> None
      with _ -> None
    in
    { message; is_bang; deferred_followup; cwd }
  with _ ->
    {
      message = payload_json;
      is_bang = false;
      deferred_followup = false;
      cwd = None;
    }

let replay_message payload =
  if
    payload.is_bang
    && String.length payload.message > 0
    && payload.message.[0] <> '!'
  then "!" ^ payload.message
  else payload.message

let default_replay_turn mgr ~key ~message ?(deferred_if_busy = false) ?cwd () =
  Session.turn mgr ~key ~message ?cwd ~deferred_if_busy ()

let replay_durable_inbound_queue ?replay_turn ~(session_manager : Session.t)
    ~(config : Runtime_config.t) () =
  ignore config;
  match session_manager.Session.db with
  | None ->
      Logs.info (fun m ->
          m "Boot: durable inbound replay summary %s"
            (boot_replay_summary_message empty_summary));
      Lwt.return empty_summary
  | Some db ->
      let open Lwt.Syntax in
      let turn_fn =
        match replay_turn with Some f -> f | None -> default_replay_turn
      in
      let reclaimed = Memory.queue_reclaim_stale ~db ~older_than_seconds:3600 in
      if reclaimed > 0 then
        Logs.info (fun m ->
            m "Boot: reclaimed %d stale inbound queue claims" reclaimed);
      let reclaimed_failed = Memory.queue_reclaim_failed ~db () in
      if reclaimed_failed > 0 then
        Logs.info (fun m ->
            m "Boot: reclaimed %d failed inbound queue rows for retry"
              reclaimed_failed);
      let pending_sessions = Memory.queue_list_pending_sessions ~db in
      let total = Memory.queue_count_all ~db in
      let summary =
        {
          reclaimed_stale_count = reclaimed;
          reclaimed_failed_count = reclaimed_failed;
          session_count = List.length pending_sessions;
          total_rows = total;
          replayed_count = 0;
          failed_count = 0;
        }
        |> ref
      in
      if pending_sessions = [] then begin
        Logs.info (fun m -> m "Boot: no durable inbound queue rows to replay");
        Logs.info (fun m ->
            m "Boot: durable inbound replay summary %s"
              (boot_replay_summary_message !summary));
        Lwt.return !summary
      end
      else begin
        Logs.info (fun m ->
            m "Boot: replaying %d durable inbound queue rows across %d sessions"
              total
              (List.length pending_sessions));
        let* () =
          Lwt_list.iter_s
            (fun session_key ->
              let rec drain_session () =
                match Memory.queue_claim ~db ~session_key with
                | Memory.Claim_empty -> Lwt.return_unit
                | Memory.Claim_ok row ->
                    Logs.info (fun m ->
                        m
                          "Replay: claimed queue_id=%d session=%s source=%s \
                           attempt=%d"
                          row.queue_id session_key row.source row.attempt_count);
                    let payload = parse_replay_payload row.payload_json in
                    if String.trim payload.message = "" then begin
                      Logs.warn (fun m ->
                          m
                            "Replay: skipping queue_id=%d session=%s \
                             reason=empty-message"
                            row.queue_id session_key);
                      Memory.queue_record_failure ~db ~queue_id:row.queue_id
                        ~error:"empty message";
                      summary :=
                        {
                          !summary with
                          failed_count = !summary.failed_count + 1;
                        };
                      drain_session ()
                    end
                    else
                      Lwt.catch
                        (fun () ->
                          Logs.info (fun m ->
                              m
                                "Replay: processing queue_id=%d session=%s \
                                 bang=%b msg_len=%d"
                                row.queue_id session_key payload.is_bang
                                (String.length payload.message));
                          let* _response =
                            turn_fn session_manager ~key:session_key
                              ~message:(replay_message payload)
                              ~deferred_if_busy:payload.deferred_followup
                              ?cwd:payload.cwd ()
                          in
                          let deleted =
                            Memory.queue_delete ~db ~queue_id:row.queue_id
                          in
                          if deleted then
                            Logs.info (fun m ->
                                m
                                  "Replay: success queue_id=%d session=%s \
                                   deleted=true"
                                  row.queue_id session_key)
                          else
                            Logs.warn (fun m ->
                                m
                                  "Replay: success queue_id=%d session=%s \
                                   deleted=false (already removed)"
                                  row.queue_id session_key);
                          summary :=
                            {
                              !summary with
                              replayed_count = !summary.replayed_count + 1;
                            };
                          drain_session ())
                        (fun exn ->
                          let error = Printexc.to_string exn in
                          Logs.err (fun m ->
                              m "Replay: failed queue_id=%d session=%s error=%s"
                                row.queue_id session_key error);
                          Memory.queue_record_failure ~db ~queue_id:row.queue_id
                            ~error;
                          summary :=
                            {
                              !summary with
                              failed_count = !summary.failed_count + 1;
                            };
                          drain_session ())
              in
              drain_session ())
            pending_sessions
        in
        Logs.info (fun m ->
            m "Boot: durable inbound replay complete replayed=%d failed=%d"
              !summary.replayed_count !summary.failed_count);
        Logs.info (fun m ->
            m "Boot: durable inbound replay summary %s"
              (boot_replay_summary_message !summary));
        Lwt.return !summary
      end
