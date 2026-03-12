let consolidated_status_on_chunk
    ~(agent_defaults : Runtime_config.agent_defaults) ~thinking_buf sm =
  function
  | Provider.ToolStart { id; name; arguments } ->
      let summary =
        Stream_visibility.summarize_tool_arguments ~name arguments
      in
      Status_message.tool_start sm ~id ~name ~summary
  | Provider.ToolResult { id; name; result; is_error } ->
      Status_message.tool_result sm ~id ~name ~result ~is_error
  | Provider.ThinkingDelta text ->
      if agent_defaults.show_thinking then begin
        Buffer.add_string thinking_buf text;
        Status_message.update_thinking sm text
      end
      else Lwt.return_unit
  | Provider.Delta _ | Provider.ToolCallDelta _ | Provider.ToolOutputDelta _
  | Provider.Done ->
      Lwt.return_unit

let stream_turn_with_visibility mgr ~notify agent ~key ~effective_message
    ~persisted_up_to ~interrupt_check ~inject_messages ~runtime_context
    ~on_history_update ?on_stuck () =
  let open Lwt.Syntax in
  let agent_defaults = mgr.Session_core.config.agent_defaults in
  let use_consolidated =
    agent_defaults.show_tool_calls
    && agent_defaults.tool_status_mode = "consolidated"
  in
  let status_factory =
    if use_consolidated then
      Hashtbl.find_opt mgr.Session_core.status_message_factories key
    else None
  in
  match status_factory with
  | Some factory ->
      let sm = factory () in
      let thinking_buf = Buffer.create 256 in
      let on_chunk =
        consolidated_status_on_chunk ~agent_defaults ~thinking_buf sm
      in
      let* response =
        Agent.turn_stream agent ~user_message:effective_message ?db:mgr.db
          ~session_key:key ~interrupt_check ~inject_messages ?runtime_context
          ~history_prepared:true ~on_history_update ?on_stuck ~on_chunk ()
      in
      let* () = Status_message.finalize sm in
      let thinking = Buffer.contents thinking_buf in
      let* () =
        if agent_defaults.show_thinking && thinking <> "" then
          notify (Stream_visibility.thinking_message thinking)
        else Lwt.return_unit
      in
      if agent.Agent.compacted_mid_turn then begin
        Session_core.persist_compacted_history mgr ~key agent;
        agent.Agent.compacted_mid_turn <- false
      end
      else
        Session_core.persist_new_messages mgr ~key
          ~history_before:!persisted_up_to agent;
      (match mgr.db with
      | Some db when mgr.config.security.audit_enabled ->
          Audit.log ~db
            (ChatMessage
               {
                 session_key = key;
                 role = "assistant";
                 content_preview = response;
               })
      | _ -> ());
      Lwt.return response
  | None ->
      let visibility = Stream_visibility.create () in
      let settings : Stream_visibility.settings =
        {
          show_thinking = agent_defaults.show_thinking;
          show_tool_calls = agent_defaults.show_tool_calls;
          notify_tool_starts = false;
          notify_tool_successes = true;
        }
      in
      let* response =
        Agent.turn_stream agent ~user_message:effective_message ?db:mgr.db
          ~session_key:key ~interrupt_check ~inject_messages ?runtime_context
          ~history_prepared:true ~on_history_update ?on_stuck
          ~on_chunk:(Stream_visibility.on_chunk visibility ~settings ~notify)
          ()
      in
      let thinking = Stream_visibility.thinking_text visibility in
      let* () =
        if settings.show_thinking && thinking <> "" then
          notify (Stream_visibility.thinking_message thinking)
        else Lwt.return_unit
      in
      if agent.Agent.compacted_mid_turn then begin
        Session_core.persist_compacted_history mgr ~key agent;
        agent.Agent.compacted_mid_turn <- false
      end
      else
        Session_core.persist_new_messages mgr ~key
          ~history_before:!persisted_up_to agent;
      (match mgr.db with
      | Some db when mgr.config.security.audit_enabled ->
          Audit.log ~db
            (ChatMessage
               {
                 session_key = key;
                 role = "assistant";
                 content_preview = response;
               })
      | _ -> ());
      Lwt.return response

let normalize_incoming_message mgr ~key ~message =
  let open Lwt.Syntax in
  if String.length message > 0 && message.[0] = '!' then begin
    let raw = String.sub message 1 (String.length message - 1) in
    let normalized = if String.trim raw = "" then "[interrupted]" else raw in
    let session_exists = Hashtbl.mem mgr.Session_core.sessions key in
    let session_busy =
      match Hashtbl.find_opt mgr.Session_core.sessions key with
      | Some (_, mutex, _) -> Lwt_mutex.is_locked mutex
      | None -> false
    in
    Logs.info (fun m ->
        m
          "Bang message received for session %s: raw=%S normalized=%S \
           session_exists=%b session_busy=%b"
          key raw normalized session_exists session_busy);
    let* () = Session_core.set_interrupt_if_present mgr ~key normalized in
    Lwt.return normalized
  end
  else Lwt.return message

(* Forward reference: filled in after [turn] is defined below *)
let spawn_postmortem_agent_fn :
    (Session_core.t ->
    stuck_history:Provider.message list ->
    session_key:string ->
    reason:string ->
    ?db:Sqlite3.db ->
    unit ->
    unit Lwt.t)
    ref =
  ref (fun _mgr ~stuck_history:_ ~session_key:_ ~reason:_ ?db:_ () ->
      Lwt.return_unit)

let spawn_postmortem_agent mgr ~stuck_history ~session_key ~reason ?db () =
  let root_key = Session_core.root_postmortem_session_key session_key in
  if root_key <> session_key then begin
    Logs.warn (fun m ->
        m
          "Suppressing recursive postmortem launch for session %s (root=%s, \
           reason=%s)"
          session_key root_key reason);
    Lwt.return_unit
  end
  else if Hashtbl.mem mgr.Session_core.postmortem_circuit_breakers root_key then begin
    Logs.warn (fun m ->
        m
          "Postmortem circuit breaker open for session %s; suppressing \
           additional launch (reason=%s)"
          root_key reason);
    Lwt.return_unit
  end
  else begin
    Hashtbl.replace mgr.Session_core.postmortem_circuit_breakers root_key ();
    !spawn_postmortem_agent_fn mgr ~stuck_history ~session_key ~reason ?db ()
  end

let run_locked_turn mgr ~key agent interrupt ~message ?(content_parts = [])
    ?(attachments = []) ?channel_name ?channel_type ?sender_id ?sender_name
    ?channel ?channel_id () =
  let open Lwt.Syntax in
  let interrupt_check () = !interrupt in
  interrupt := None;
  (match mgr.Session_core.db with
  | Some db when mgr.config.security.audit_enabled ->
      Audit.log ~db
        (ChatMessage
           { session_key = key; role = "user"; content_preview = message })
  | _ -> ());
  Session_core.inject_attachment_context agent attachments;
  let effective_message =
    Session_core.effective_message_for_turn ~message ?channel_name ?channel_type
      ?sender_id ?sender_name ()
  in
  let history_before = List.length agent.history in
  let notify = Session_core.find_registered_notifier mgr ~key in
  let refresh_messages =
    match Agent.note_external_workspace_refresh_if_needed agent with
    | Some msg -> [ msg ]
    | None -> []
  in
  let* () = Session_core.notify_event_messages ?notify refresh_messages in
  let* compaction_info =
    Agent.prepare_turn_history agent ~user_message:effective_message
      ~content_parts ~workspace_refresh_checked:true ?db:mgr.db ()
  in
  let compacted = Option.is_some compaction_info in
  let* () = Session_core.notify_compaction_if_needed ?notify compaction_info in
  if compacted then Session_core.persist_compacted_history mgr ~key agent
  else Session_core.persist_new_messages mgr ~key ~history_before agent;
  let runtime_context =
    Prompt_builder.build_runtime_context ~config:mgr.config
      ~details:
        (Session_core.runtime_context_details mgr ~agent ~key
           ~compacted_before_turn:compacted)
      ()
  in
  let prepared_history_len = List.length agent.history in
  Session_core.record_agent_turn mgr ~key ?channel ?channel_id ();
  let persisted_up_to = ref prepared_history_len in
  let on_history_update new_msgs =
    (match mgr.db with
    | Some db ->
        List.iter
          (fun msg -> Memory.store_message ~db ~session_key:key msg)
          new_msgs;
        persisted_up_to := List.length agent.Agent.history
    | None -> ());
    Session_core.notify_event_messages ?notify new_msgs
  in
  let inject_messages () =
    let msgs = Session_core.take_all_queued_messages_for_injection mgr ~key in
    List.map
      (fun (qm : Session_core.queued_message) ->
        Session_core.queued_message_prompt
          (Session_core.effective_message_for_turn ~message:qm.message
             ?channel_name:qm.channel_name ?channel_type:qm.channel_type
             ?sender_id:qm.sender_id ?sender_name:qm.sender_name ()))
      msgs
  in
  let on_stuck signals =
    let open Lwt.Syntax in
    let signal_desc = Stuck_detector.signals_to_string signals in
    Logs.warn (fun m ->
        m "[observer] stuck detected session=%s: %s" key signal_desc);
    let correction =
      Printf.sprintf
        "[Observer] Stuck pattern detected: %s\n\n\
         A postmortem agent has been launched to analyze this failure and look \
         for solutions. While it works, you can:\n\
         1. Ask a subagent to help find an alternative approach\n\
         2. Work on a different part of the task\n\
         3. Wait for the postmortem agent to write its findings to \
         POSTMORTEM.md"
        signal_desc
    in
    let correction_msg =
      Provider.make_message ~role:"user" ~content:correction
    in
    agent.Agent.history <- correction_msg :: agent.Agent.history;
    let* () = on_history_update [ correction_msg ] in
    Lwt.async (fun () ->
        spawn_postmortem_agent mgr ~stuck_history:agent.Agent.history
          ~session_key:key ~reason:signal_desc ?db:mgr.db ());
    Lwt.return_unit
  in
  let* response =
    Lwt.catch
      (fun () ->
        let* draining_response = Session_core.respond_if_draining mgr in
        match draining_response with
        | Some response -> Lwt.return response
        | None -> (
            match notify with
            | Some send
              when mgr.config.agent_defaults.show_thinking
                   || mgr.config.agent_defaults.show_tool_calls ->
                stream_turn_with_visibility mgr ~notify:send agent ~key
                  ~effective_message ~persisted_up_to ~interrupt_check
                  ~inject_messages ~runtime_context ~on_history_update ~on_stuck
                  ()
            | _ ->
                Agent.turn agent ~user_message:effective_message ?db:mgr.db
                  ~session_key:key ~interrupt_check ~inject_messages
                  ?runtime_context ~history_prepared:true ~on_history_update
                  ~on_stuck ()))
      (function
        | Agent.Restart_requested ->
            if agent.Agent.compacted_mid_turn then begin
              Session_core.persist_compacted_history mgr ~key agent;
              agent.Agent.compacted_mid_turn <- false
            end
            else
              Session_core.persist_new_messages mgr ~key
                ~history_before:!persisted_up_to agent;
            Session_core.set_response_deferred mgr ~key;
            Lwt.return Session_core.draining_message
        | exn ->
            if agent.Agent.compacted_mid_turn then begin
              Session_core.persist_compacted_history mgr ~key agent;
              agent.Agent.compacted_mid_turn <- false
            end
            else
              Session_core.persist_new_messages mgr ~key
                ~history_before:!persisted_up_to agent;
            Lwt.fail exn)
  in
  (match notify with
  | Some _
    when mgr.config.agent_defaults.show_thinking
         || mgr.config.agent_defaults.show_tool_calls ->
      ()
  | _ ->
      if not (Session_core.response_deferred mgr ~key) then begin
        if agent.Agent.compacted_mid_turn then begin
          Session_core.persist_compacted_history mgr ~key agent;
          agent.Agent.compacted_mid_turn <- false
        end
        else
          Session_core.persist_new_messages mgr ~key
            ~history_before:!persisted_up_to agent;
        match mgr.db with
        | Some db when mgr.config.security.audit_enabled ->
            Audit.log ~db
              (ChatMessage
                 {
                   session_key = key;
                   role = "assistant";
                   content_preview = response;
                 })
        | _ -> ()
      end);
  (* Message-count observer: trigger LLM stuck check every N new messages *)
  if mgr.config.observer.enabled then begin
    let cur_len = List.length agent.Agent.history in
    let last_checked =
      Option.value ~default:0
        (Hashtbl.find_opt mgr.Session_core.observer_last_checked key)
    in
    let n = mgr.config.observer.check_every_n_messages in
    if cur_len - last_checked >= n then begin
      Hashtbl.replace mgr.Session_core.observer_last_checked key cur_len;
      let history_snapshot = agent.Agent.history in
      let stats : Session_observer.session_stats =
        {
          session_key = key;
          turn_count = cur_len / 2;
          total_tool_calls = 0;
          error_count = 0;
          session_age_s = 0.0;
        }
      in
      Lwt.async (fun () ->
          let open Lwt.Syntax in
          let* verdict =
            Session_observer.check_stuck ~config:mgr.config
              ~history:history_snapshot ~stats ()
          in
          match verdict with
          | Session_observer.Ok | Session_observer.Error _ -> Lwt.return_unit
          | Session_observer.Stuck { reason; _ } ->
              Logs.warn (fun m ->
                  m "[observer] message-count check: stuck session=%s: %s" key
                    reason);
              spawn_postmortem_agent mgr ~stuck_history:history_snapshot
                ~session_key:key ~reason ?db:mgr.db ())
    end
  end;
  Lwt.return response

let rec drain_queued_messages_loop mgr ~key agent interrupt ?on_drain_progress
    ~drained_any () =
  match
    ( Session_core.take_next_queued_message mgr ~key,
      Session_core.find_registered_notifier mgr ~key )
  with
  | Some queued, Some notify ->
      let open Lwt.Syntax in
      Logs.info (fun m -> m "Sending queued message to LLM for session %s" key);
      let* () =
        match on_drain_progress with
        | Some dp -> dp.Session_core.before_turn queued.message_id
        | None -> Lwt.return_unit
      in
      let injected_message =
        Session_core.queued_message_prompt
          (Session_core.effective_message_for_turn ~message:queued.message
             ?channel_name:queued.channel_name ?channel_type:queued.channel_type
             ?sender_id:queued.sender_id ?sender_name:queued.sender_name ())
      in
      let* response =
        run_locked_turn mgr ~key agent interrupt ~message:injected_message
          ~content_parts:queued.content_parts ?channel_name:queued.channel_name
          ?channel_type:queued.channel_type ?sender_id:queued.sender_id
          ?sender_name:queued.sender_name ?channel:queued.channel
          ?channel_id:queued.channel_id ()
      in
      let* () = notify response in
      (match (queued.inbound_queue_id, mgr.Session_core.db) with
      | Some qid, Some db -> ignore (Memory.queue_delete ~db ~queue_id:qid)
      | _ -> ());
      let* () =
        match on_drain_progress with
        | Some dp -> dp.after_turn queued.message_id
        | None -> Lwt.return_unit
      in
      if not (Session_core.take_response_deferred mgr ~key) then
        Session_core.mark_response_sent mgr ~key;
      drain_queued_messages_loop mgr ~key agent interrupt ?on_drain_progress
        ~drained_any:true ()
  | Some queued, None ->
      Logs.warn (fun m ->
          m
            "Dropping queued message for session %s: no notifier registered \
             (message: %s)"
            key
            (if String.length queued.message > 80 then
               String.sub queued.message 0 80 ^ "..."
             else queued.message));
      (match (queued.inbound_queue_id, mgr.Session_core.db) with
      | Some qid, Some db -> ignore (Memory.queue_delete ~db ~queue_id:qid)
      | _ -> ());
      Lwt.return_unit
  | None, _ ->
      if drained_any then
        let open Lwt.Syntax in
        let* () =
          match on_drain_progress with
          | Some dp -> dp.after_all ()
          | None -> Lwt.return_unit
        in
        Lwt.return_unit
      else Lwt.return_unit

let drain_queued_messages mgr ~key agent interrupt ?on_drain_progress () =
  Session_core.with_live_activity mgr ~key (fun () ->
      drain_queued_messages_loop mgr ~key agent interrupt ?on_drain_progress
        ~drained_any:false ())

let rec turn mgr ~key ~message ?(content_parts = []) ?(attachments = [])
    ?channel_name ?channel_type ?sender_id ?sender_name ?channel ?channel_id
    ?message_id ?before_drain () =
  Session_core.with_live_activity mgr ~key (fun () ->
      let open Lwt.Syntax in
      let* () = Session_core.mark_autonomous_activity_started mgr ~key in
      let* message = normalize_incoming_message mgr ~key ~message in
      let* handled =
        Session_core.handle_special_command mgr ~key ~message
          ?send_progress:(Session_core.find_registered_notifier mgr ~key)
          ~interrupt_check:(Session_core.interrupt_check_if_present mgr ~key)
          ()
      in
      match handled with
      | Some response -> Lwt.return response
      | None ->
          let queued_message : Session_core.queued_message =
            {
              message;
              content_parts;
              attachments;
              channel_name;
              channel_type;
              sender_id;
              sender_name;
              channel;
              channel_id;
              message_id;
              inbound_queue_id = None;
            }
          in
          let* queued =
            Session_core.enqueue_message_if_busy mgr ~key queued_message
          in
          if queued then Lwt.return Session_core.queued_message_response
          else
            Session_core.with_session_lock_unless_draining mgr ~key
              ~on_draining:(fun () ->
                let* draining_response = Session_core.respond_if_draining mgr in
                match draining_response with
                | Some response -> Lwt.return response
                | None -> Lwt.return Session_core.draining_message)
              (fun agent interrupt ->
                Session_core.with_in_flight mgr (fun () ->
                    let* response =
                      run_locked_turn mgr ~key agent interrupt ~message
                        ~content_parts ~attachments ?channel_name ?channel_type
                        ?sender_id ?sender_name ?channel ?channel_id ()
                    in
                    let* () =
                      match before_drain with
                      | Some f -> f response
                      | None -> Lwt.return_unit
                    in
                    let* () =
                      drain_queued_messages mgr ~key agent interrupt ()
                    in
                    Lwt.return response)))

let () =
  spawn_postmortem_agent_fn :=
    fun mgr ~stuck_history ~session_key ~reason ?db () ->
      let open Lwt.Syntax in
      let postmortem_session_key = "__postmortem_" ^ session_key in
      let evidence_summary = Postmortem.format_history_text stuck_history in
      let correction = "(postmortem agent will determine correction)" in
      let* doc_path =
        Postmortem.write_doc ~session_key ~pattern:reason ~evidence_summary
          ~correction
      in
      (match db with
      | Some db -> (
          try
            ignore
              (Memory.insert_postmortem ~db ~session_key ~pattern:reason
                 ~evidence_json:
                   (Yojson.Safe.to_string (`String evidence_summary))
                 ~correction_injected:correction ~doc_path)
          with exn ->
            Logs.warn (fun m ->
                m "postmortem: failed to insert DB record: %s"
                  (Printexc.to_string exn)))
      | None -> ());
      let prompt =
        Postmortem.make_postmortem_prompt ~session_key ~reason ~doc_path
          ~history_text:evidence_summary ()
      in
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              let* _response =
                turn mgr ~key:postmortem_session_key ~message:prompt ()
              in
              Lwt.return_unit)
            (fun exn ->
              Logs.warn (fun m ->
                  m "postmortem agent error for session %s: %s" session_key
                    (Printexc.to_string exn));
              Lwt.return_unit));
      Lwt.return_unit

let delegate_turn mgr ~prompt ~send_reply =
  if mgr.Session_core.draining then
    Lwt.async (fun () ->
        Lwt.catch
          (fun () -> send_reply Session_core.draining_message)
          (fun _ -> Lwt.return_unit))
  else
    Lwt.async (fun () ->
        Session_core.with_in_flight mgr (fun () ->
            let agent =
              Agent.create ~config:mgr.config ?tool_registry:mgr.tool_registry
                ()
            in
            Lwt.catch
              (fun () ->
                let open Lwt.Syntax in
                let* response = Agent.turn agent ~user_message:prompt () in
                send_reply response)
              (fun exn ->
                Logs.err (fun m ->
                    m "Delegation failed: %s" (Printexc.to_string exn));
                Lwt.catch
                  (fun () ->
                    send_reply
                      (Printf.sprintf "Delegation failed: %s"
                         (Printexc.to_string exn)))
                  (fun _ -> Lwt.return_unit))))

let fork_and_run mgr ~parent_key ~prompt ~send_reply =
  if mgr.Session_core.draining then
    Lwt.async (fun () ->
        Lwt.catch
          (fun () -> send_reply Session_core.draining_message)
          (fun _ -> Lwt.return_unit))
  else
    Lwt.async (fun () ->
        Session_core.with_in_flight mgr (fun () ->
            let open Lwt.Syntax in
            let* parent_history =
              Session_core.snapshot_history mgr ~key:parent_key
            in
            let agent =
              Agent.create ~config:mgr.config ?tool_registry:mgr.tool_registry
                ()
            in
            agent.Agent.history <- List.rev parent_history;
            Lwt.catch
              (fun () ->
                let* response = Agent.turn agent ~user_message:prompt () in
                send_reply response)
              (fun exn ->
                Logs.err (fun m ->
                    m "Fork failed for parent=%s: %s" parent_key
                      (Printexc.to_string exn));
                Lwt.catch
                  (fun () ->
                    send_reply
                      (Printf.sprintf "Fork failed: %s" (Printexc.to_string exn)))
                  (fun _ -> Lwt.return_unit))))

let turn_stream mgr ~key ~message ?(content_parts = []) ?(attachments = [])
    ?channel_name ?channel_type ?sender_id ?sender_name ?channel ?channel_id
    ?message_id ?on_drain_progress ?before_drain ~on_chunk () =
  Session_core.with_live_activity mgr ~key (fun () ->
      let open Lwt.Syntax in
      let* () = Session_core.mark_autonomous_activity_started mgr ~key in
      let* message = normalize_incoming_message mgr ~key ~message in
      let send_progress text = on_chunk (Provider.Delta (text ^ "\n")) in
      let* handled =
        Session_core.handle_special_command mgr ~key ~message ~send_progress
          ~interrupt_check:(Session_core.interrupt_check_if_present mgr ~key)
          ()
      in
      match handled with
      | Some response ->
          let* () = on_chunk (Provider.Delta response) in
          let* () = on_chunk Provider.Done in
          Lwt.return response
      | None ->
          let queued_message : Session_core.queued_message =
            {
              message;
              content_parts;
              attachments;
              channel_name;
              channel_type;
              sender_id;
              sender_name;
              channel;
              channel_id;
              message_id;
              inbound_queue_id = None;
            }
          in
          let* queued =
            Session_core.enqueue_message_if_busy mgr ~key queued_message
          in
          if queued then Lwt.return Session_core.queued_message_response
          else
            Session_core.with_session_lock_unless_draining mgr ~key
              ~on_draining:(fun () ->
                let* draining_response =
                  Session_core.respond_if_draining ~on_chunk mgr
                in
                match draining_response with
                | Some response -> Lwt.return response
                | None -> Lwt.return Session_core.draining_message)
              (fun agent interrupt ->
                Session_core.with_in_flight mgr (fun () ->
                    let interrupt_check () = !interrupt in
                    interrupt := None;
                    (match mgr.db with
                    | Some db when mgr.config.security.audit_enabled ->
                        Audit.log ~db
                          (ChatMessage
                             {
                               session_key = key;
                               role = "user";
                               content_preview = message;
                             })
                    | _ -> ());
                    Session_core.inject_attachment_context agent attachments;
                    let effective_message =
                      match
                        (channel_name, channel_type, sender_id, sender_name)
                      with
                      | None, None, None, None -> message
                      | _ ->
                          let ctx =
                            Session_core.format_context_block ?channel_name
                              ?channel_type ?sender_id ?sender_name ()
                          in
                          ctx ^ "\n" ^ message
                    in
                    let history_before = List.length agent.history in
                    let notify =
                      Session_core.find_registered_notifier mgr ~key
                    in
                    let refresh_messages =
                      match
                        Agent.note_external_workspace_refresh_if_needed agent
                      with
                      | Some msg -> [ msg ]
                      | None -> []
                    in
                    let* () =
                      Session_core.notify_event_messages ?notify ~on_chunk
                        refresh_messages
                    in
                    let* compaction_info =
                      Agent.prepare_turn_history agent
                        ~user_message:effective_message ~content_parts
                        ~workspace_refresh_checked:true ?db:mgr.db ()
                    in
                    let compacted = Option.is_some compaction_info in
                    let* () =
                      Session_core.notify_compaction_if_needed
                        ~notify:(fun text ->
                          on_chunk (Provider.Delta (text ^ "\n")))
                        compaction_info
                    in
                    if compacted then
                      Session_core.persist_compacted_history mgr ~key agent
                    else
                      Session_core.persist_new_messages mgr ~key ~history_before
                        agent;
                    let runtime_context =
                      Prompt_builder.build_runtime_context ~config:mgr.config
                        ~details:
                          (Session_core.runtime_context_details mgr ~agent ~key
                             ~compacted_before_turn:compacted)
                        ()
                    in
                    let prepared_history_len = List.length agent.history in
                    Session_core.record_agent_turn mgr ~key ?channel ?channel_id
                      ();
                    let persisted_up_to = ref prepared_history_len in
                    let on_history_update new_msgs =
                      (match mgr.db with
                      | Some db ->
                          List.iter
                            (fun msg ->
                              Memory.store_message ~db ~session_key:key msg)
                            new_msgs;
                          persisted_up_to := List.length agent.Agent.history
                      | None -> ());
                      Session_core.notify_event_messages ?notify ~on_chunk
                        new_msgs
                    in
                    let inject_messages () =
                      let msgs =
                        Session_core.take_all_queued_messages_for_injection mgr
                          ~key
                      in
                      List.map
                        (fun (qm : Session_core.queued_message) ->
                          Session_core.queued_message_prompt
                            (Session_core.effective_message_for_turn
                               ~message:qm.message ?channel_name:qm.channel_name
                               ?channel_type:qm.channel_type
                               ?sender_id:qm.sender_id
                               ?sender_name:qm.sender_name ()))
                        msgs
                    in
                    let* response =
                      Lwt.catch
                        (fun () ->
                          let* draining_response =
                            Session_core.respond_if_draining ~on_chunk mgr
                          in
                          match draining_response with
                          | Some response -> Lwt.return response
                          | None ->
                              Agent.turn_stream agent
                                ~user_message:effective_message ?db:mgr.db
                                ~session_key:key ~interrupt_check
                                ~inject_messages ?runtime_context
                                ~history_prepared:true ~on_history_update
                                ~on_chunk ())
                        (function
                          | Agent.Restart_requested ->
                              if agent.Agent.compacted_mid_turn then begin
                                Session_core.persist_compacted_history mgr ~key
                                  agent;
                                agent.Agent.compacted_mid_turn <- false
                              end
                              else
                                Session_core.persist_new_messages mgr ~key
                                  ~history_before:!persisted_up_to agent;
                              Session_core.set_response_deferred mgr ~key;
                              let* () =
                                on_chunk
                                  (Provider.Delta Session_core.draining_message)
                              in
                              let* () = on_chunk Provider.Done in
                              Lwt.return Session_core.draining_message
                          | exn ->
                              if agent.Agent.compacted_mid_turn then begin
                                Session_core.persist_compacted_history mgr ~key
                                  agent;
                                agent.Agent.compacted_mid_turn <- false
                              end
                              else
                                Session_core.persist_new_messages mgr ~key
                                  ~history_before:!persisted_up_to agent;
                              Lwt.fail exn)
                    in
                    if not (Session_core.response_deferred mgr ~key) then begin
                      if agent.Agent.compacted_mid_turn then begin
                        Session_core.persist_compacted_history mgr ~key agent;
                        agent.Agent.compacted_mid_turn <- false
                      end
                      else
                        Session_core.persist_new_messages mgr ~key
                          ~history_before:!persisted_up_to agent;
                      match mgr.db with
                      | Some db when mgr.config.security.audit_enabled ->
                          Audit.log ~db
                            (ChatMessage
                               {
                                 session_key = key;
                                 role = "assistant";
                                 content_preview = response;
                               })
                      | _ -> ()
                    end;
                    let* () =
                      match before_drain with
                      | Some f -> f response
                      | None -> Lwt.return_unit
                    in
                    let* () =
                      drain_queued_messages mgr ~key agent interrupt
                        ?on_drain_progress ()
                    in
                    Lwt.return response)))
