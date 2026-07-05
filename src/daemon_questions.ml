type ask_fn =
  session_key:string ->
  questions:Tools_builtin.question_item list ->
  Tools_builtin.question_result list Lwt.t

let qtype_supports_notes = function
  | Tools_builtin.Text _ | Tools_builtin.File_upload _ | Tools_builtin.Confirm
  | Tools_builtin.Rating _ ->
      false
  | _ -> true

let notes_eligible qi =
  qtype_supports_notes qi.Tools_builtin.qtype && qi.request_notes

let make_ask_fn ~notes_enabled ~session_manager ~db () : ask_fn =
 fun ~session_key ~questions ->
  let open Lwt.Syntax in
  let notify =
    match
      Session.find_alert_channel_notifier session_manager ~key:session_key
    with
    | Some n -> n
    | None -> (
        match
          Session.find_registered_notifier session_manager ~key:session_key
        with
        | Some n -> n
        | None ->
            fun _text ->
              Lwt.fail_with
                (Printf.sprintf "No channel notifier for session %s" session_key)
        )
  in
  let caps =
    Session.find_connector_capabilities session_manager ~key:session_key
  in
  let rich_notify =
    Session.find_rich_notifier session_manager ~key:session_key
  in
  let has_rich = Option.is_some rich_notify in
  let connector =
    match caps with
    | Some c -> c.Connector_capabilities.connector
    | None -> Format_adapter.Plain
  in
  let total = List.length questions in
  let cleanup_db () =
    (match db with
    | Some db -> (
        try Memory.pending_question_delete ~db ~session_key
        with exn ->
          Logs.warn (fun m ->
              m "[%s] Failed to clean pending question from DB: %s" session_key
                (Printexc.to_string exn)))
    | None -> ());
    Lwt.return_unit
  in
  Lwt.finalize
    (fun () ->
      let* results =
        Lwt_list.mapi_s
          (fun i qi ->
            (match db with
            | Some db -> (
                try
                  Memory.pending_question_upsert ~db ~session_key
                    ~questions_json:
                      (Tools_builtin.question_items_to_json questions)
                    ~question_index:i
                with exn ->
                  Logs.warn (fun m ->
                      m "[%s] Failed to persist pending question: %s"
                        session_key (Printexc.to_string exn)))
            | None -> ());
            let strategy =
              Question_presenter.select_strategy ~capabilities:caps
                ~has_rich_notifier:has_rich qi.Tools_builtin.qtype
            in
            let rendered =
              Question_presenter.render_question ~strategy ~connector
                ~session_key ~index:i ~total qi
            in
            let callback_ids = ref [] in
            let* () =
              match rendered with
              | Question_presenter.RichMessage msg -> (
                  match rich_notify with
                  | Some rn ->
                      Logs.info (fun m ->
                          m "[%s] Sending rich question %d/%d" session_key
                            (i + 1) total);
                      let cbs =
                        Question_presenter.extract_callback_answers msg
                      in
                      Session.register_question_callbacks session_manager
                        ~key:session_key ~callbacks:cbs;
                      callback_ids := List.map (fun (id, _) -> id) cbs;
                      let* _result = rn msg in
                      Lwt.return_unit
                  | None ->
                      Logs.info (fun m ->
                          m
                            "[%s] Rich notifier unavailable, falling back to \
                             text for question %d/%d"
                            session_key (i + 1) total);
                      notify (Rich_message.to_fallback_text msg))
              | Question_presenter.TextMessage text ->
                  Logs.info (fun m ->
                      m "[%s] Sending text question %d/%d" session_key (i + 1)
                        total);
                  notify text
            in
            let promise, _resolver =
              Session.register_pending_question session_manager ~key:session_key
            in
            let* raw = promise in
            Session.clear_question_callbacks session_manager ~key:session_key
              ~callback_ids:!callback_ids;
            if raw = Session.question_cancelled_sentinel then
              Lwt.fail (Failure "Question cancelled by user interrupt")
            else
              let* notes =
                if notes_enabled && notes_eligible qi then begin
                  let* () = notify "Add notes? (reply or 'skip')" in
                  let notes_promise, _resolver =
                    Session.register_pending_question session_manager
                      ~key:session_key
                  in
                  let* notes_raw = notes_promise in
                  if
                    notes_raw = Session.question_cancelled_sentinel
                    || String.lowercase_ascii (String.trim notes_raw) = "skip"
                  then Lwt.return_none
                  else Lwt.return_some notes_raw
                end
                else Lwt.return_none
              in
              Lwt.return
                Tools_builtin.{ question = qi.question; answer = raw; notes })
          questions
      in
      Lwt.return results)
    cleanup_db

let register_tool ~config ~session_manager ~db registry =
  let ask_fn =
    make_ask_fn
      ~notes_enabled:config.Runtime_config.interactive.enable_question_notes
      ~session_manager ~db ()
  in
  Tool_registry.register registry
    (Tools_builtin.ask_user_question ~ask_fn:(Some ask_fn));
  ask_fn
