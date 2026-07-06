open Session_postmortem

let install ~turn () =
  spawn_postmortem_agent_fn :=
    fun mgr ~stuck_history ~session_key ~reason ?db () ->
      let open Lwt.Syntax in
      let pm_cfg = mgr.Session_core.config.postmortem in
      if not pm_cfg.enabled then begin
        Logs.info (fun m ->
            m "postmortem disabled in config; suppressing launch (session=%s)"
              session_key);
        Lwt.return_unit
      end
      else
        let postmortem_session_key =
          Printf.sprintf "__postmortem_%s@%d" session_key
            (int_of_float (Unix.gettimeofday ()))
        in
        let evidence_summary = Postmortem.format_history_text stuck_history in
        let correction = "(postmortem agent will determine correction)" in
        (* B612 round 2: outer set the breaker before calling us. If the doc
           write fails below, clear it so a later same-pattern stuck event
           still gets a chance. *)
        let root_key = Session_core.root_postmortem_session_key session_key in
        let breaker_key = (root_key, pattern_key_for_reason reason) in
        let* doc_path =
          Lwt.catch
            (fun () ->
              Postmortem.write_doc ~session_key ~pattern:reason
                ~evidence_summary ~correction)
            (fun exn ->
              Hashtbl.remove mgr.Session_core.postmortem_circuit_breakers
                breaker_key;
              Logs.warn (fun m ->
                  m
                    "postmortem: write_doc failed; cleared breaker so retry \
                     can fire (session=%s err=%s)"
                    session_key (Printexc.to_string exn));
              Lwt.fail exn)
        in
        let postmortem_id =
          match db with
          | Some db -> (
              try
                let id =
                  Memory.insert_postmortem ~db ~session_key ~pattern:reason
                    ~evidence_json:
                      (Yojson.Safe.to_string (`String evidence_summary))
                    ~correction_injected:correction ~doc_path
                in
                (* Apply configured postmortem model override (if any) to the
                   dedicated postmortem session before the agent runs. *)
                (match pm_cfg.model with
                | Some m ->
                    Memory.set_session_model_override ~db
                      ~session_key:postmortem_session_key ~model:m
                | None -> ());
                Some id
              with exn ->
                Logs.warn (fun m ->
                    m "postmortem: failed to insert DB record: %s"
                      (Printexc.to_string exn));
                None)
          | None -> None
        in
        let prompt =
          Postmortem.make_postmortem_prompt ~session_key ~reason ~doc_path
            ~history_text:evidence_summary ()
        in
        Lwt.async (fun () ->
            Lwt.catch
              (fun () ->
                let* () =
                  if pm_cfg.delay_s > 0.0 then Lwt_unix.sleep pm_cfg.delay_s
                  else Lwt.return_unit
                in
                let* response =
                  turn mgr ~key:postmortem_session_key ~message:prompt ()
                in
                (* B610: now that the postmortem turn finished, close the DB
                   record with the agent's outcome summary, and attempt to
                   auto-lodge a backlog bug for any structured finding. *)
                (match (db, postmortem_id) with
                | Some db, Some id -> (
                    try
                      Memory.update_postmortem_outcome ~db ~id ~outcome:response
                    with exn ->
                      Logs.warn (fun m ->
                          m "postmortem: update_postmortem_outcome failed: %s"
                            (Printexc.to_string exn)))
                | _ -> ());
                let* () =
                  Postmortem_followup.try_lodge_bug ~doc_path ~response
                    ~session_key ~reason
                in
                Lwt.return_unit)
              (fun exn ->
                Logs.warn (fun m ->
                    m "postmortem agent error for session %s: %s" session_key
                      (Printexc.to_string exn));
                Lwt.return_unit));
        Lwt.return_unit
