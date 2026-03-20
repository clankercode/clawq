let stream_turn_with_visibility mgr ~notify agent ~key ~effective_message
    ~persisted_up_to ~interrupt_check ~inject_messages ?on_tool_round_complete
    ~runtime_context ~on_history_update ?on_stuck () =
  let open Lwt.Syntax in
  let agent_defaults = mgr.Session_core.config.agent_defaults in
  let capabilities = Session_core.find_connector_capabilities mgr ~key in
  let strategy = Status_update.select_strategy ~agent_defaults ~capabilities in
  let notifier_factory =
    Hashtbl.find_opt mgr.Session_core.status_message_factories key
  in
  let parse_mode =
    match capabilities with Some c -> c.parse_mode | None -> "Markdown"
  in
  let handler =
    Status_update.make_handler ~strategy ~notifier_factory ~notify
      ~agent_defaults ~parse_mode
  in
  let* response =
    Agent.turn_stream agent ~user_message:effective_message ?db:mgr.db
      ~session_key:key ~interrupt_check ~inject_messages ?on_tool_round_complete
      ?runtime_context ~history_prepared:true ~on_history_update ?on_stuck
      ~on_chunk:handler.on_chunk ()
  in
  let* () = handler.finalize () in
  let thinking = handler.get_thinking () in
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
    Session_core.persist_new_messages mgr ~key ~history_before:!persisted_up_to
      agent;
  (match mgr.db with
  | Some db when mgr.config.security.audit_enabled ->
      Audit.log ~db
        (ChatMessage
           { session_key = key; role = "assistant"; content_preview = response })
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

let expand_skill_refs_fn :
    (string -> (string * string list * (string * string) list) Lwt.t) ref =
  ref (fun message -> Lwt.return (message, [], []))

let extract_skill_names_from_injections injections =
  List.filter_map
    (fun inj ->
      if String.length inj > 8 && String.sub inj 0 8 = "[Skill: " then
        match String.index_opt inj ']' with
        | Some i -> Some (String.sub inj 8 (i - 8))
        | None -> None
      else None)
    injections

let notify_skill_loads ~send injections =
  let names = extract_skill_names_from_injections injections in
  List.iter
    (fun name ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> send (Printf.sprintf "Loaded skill: %s" name))
            (fun _ -> Lwt.return_unit)))
    names

let dedup_skill_injections = Skill_dedup.dedup_skill_injections

let resolve_agent_template_registry mgr (tmpl : Agent_template.t) =
  match mgr.Session_core.tool_registry with
  | Some base_reg -> Some (Agent_template.filter_tool_registry base_reg tmpl)
  | None -> None

let handle_agent_mention mgr ?notify message =
  let open Lwt.Syntax in
  let available_agents =
    List.map
      (fun (t : Agent_template.t) -> t.name)
      (Agent_template.available_templates ())
  in
  let stripped = Group_chat_filter.strip_leading_platform_mention message in
  match Group_chat_filter.parse_agent_mention ~available_agents stripped with
  | Some (agent_name, prompt) when prompt <> "" -> (
      (match notify with
      | Some send ->
          Lwt.async (fun () ->
              Lwt.catch
                (fun () ->
                  send (Printf.sprintf "Invoking agent '%s'..." agent_name))
                (fun _ -> Lwt.return_unit))
      | None -> ());
      match Agent_template.resolve agent_name with
      | None ->
          Lwt.return_some
            (Printf.sprintf
               "Agent template '%s' not found. Use /agent list to see \
                available templates."
               agent_name)
      | Some tmpl ->
          if mgr.Session_core.draining then
            Lwt.return_some Session_core.draining_message
          else
            let tool_registry = resolve_agent_template_registry mgr tmpl in
            let agent =
              Agent.create ~config:mgr.config ?tool_registry
                ~agent_template:tmpl ()
            in
            (match notify with
            | Some send ->
                agent.Agent.on_project_doc_loaded <-
                  Some
                    (fun msg ->
                      Lwt.catch (fun () -> send msg) (fun _ -> Lwt.return_unit))
            | None -> ());
            let* response =
              Lwt.catch
                (fun () -> Agent.turn agent ~user_message:prompt ())
                (fun exn ->
                  Lwt.return
                    (Printf.sprintf "Agent invoke failed (%s): %s" agent_name
                       (Printexc.to_string exn)))
            in
            Lwt.return_some response)
  | Some (agent_name, _) ->
      Lwt.return_some
        (Printf.sprintf "Usage: @%s <prompt> — provide a prompt for the agent."
           agent_name)
  | None -> Lwt.return_none

let run_locked_turn mgr ~key agent interrupt ~message ?(content_parts = [])
    ?(attachments = []) ?(skill_injections = [])
    ?(md_skills : (string * string) list = []) ?channel_name ?channel_type
    ?sender_id ?sender_name ?user_group ?channel ?channel_id
    ?on_tool_round_complete () =
  let open Lwt.Syntax in
  let interrupt_check () = !interrupt in
  interrupt := None;
  (* Check for @agent mention at start of message *)
  let notify = Session_core.find_registered_notifier mgr ~key in
  let* agent_response = handle_agent_mention mgr ?notify message in
  match agent_response with
  | Some response -> Lwt.return response
  | None ->
      let* message, auto_injections, auto_md_skills =
        !expand_skill_refs_fn message
      in
      let skill_injections = skill_injections @ auto_injections in
      let md_skills =
        match (md_skills, auto_md_skills) with
        | [], auto -> auto
        | explicit, [] -> explicit
        | explicit, auto ->
            let seen = Hashtbl.create 16 in
            List.iter (fun (n, _) -> Hashtbl.replace seen n ()) explicit;
            let deduped_auto =
              List.filter (fun (n, _) -> not (Hashtbl.mem seen n)) auto
            in
            explicit @ deduped_auto
      in
      let md_skills =
        if user_group = Some "admin" then md_skills
        else
          List.filter
            (fun (name, _) -> not (Builtin_skills.is_test_skill_name name))
            md_skills
      in
      (* Send "Loaded skill: X" notification for @mention skills *)
      (if auto_injections <> [] then
         match Session_core.find_registered_notifier mgr ~key with
         | Some send -> notify_skill_loads ~send auto_injections
         | None -> ());
      (match mgr.Session_core.db with
      | Some db when mgr.config.security.audit_enabled ->
          Audit.log ~db
            (ChatMessage
               { session_key = key; role = "user"; content_preview = message })
      | _ -> ());
      Session_core.inject_attachment_context agent attachments;
      let skill_injections =
        dedup_skill_injections ~history:agent.Agent.history skill_injections
      in
      List.iter
        (fun content ->
          agent.Agent.history <-
            Provider.make_message ~role:"system" ~content :: agent.Agent.history)
        skill_injections;
      let effective_message =
        Session_core.effective_message_for_turn ~message ?channel_name
          ?channel_type ?sender_id ?sender_name ?user_group ()
      in
      let history_before = List.length agent.history in
      let notify = Session_core.find_registered_notifier mgr ~key in
      (* Wire project doc notification callback *)
      (match notify with
      | Some send ->
          agent.Agent.on_project_doc_loaded <-
            Some
              (fun msg ->
                Lwt.catch (fun () -> send msg) (fun _ -> Lwt.return_unit))
      | None -> ());
      let refresh_messages =
        let ws_msgs =
          match Agent.note_external_workspace_refresh_if_needed agent with
          | Some msg -> [ msg ]
          | None -> []
        in
        let pd_msgs =
          match Agent.refresh_project_docs_if_changed agent with
          | Some msg -> [ msg ]
          | None -> []
        in
        ws_msgs @ pd_msgs
      in
      let* () = Session_core.notify_event_messages ?notify refresh_messages in
      let* compaction_info =
        Agent.prepare_turn_history agent ~user_message:effective_message
          ~content_parts ~workspace_refresh_checked:true ?db:mgr.db ()
      in
      let compacted = Option.is_some compaction_info in
      let* () =
        Session_core.notify_compaction_if_needed ?notify compaction_info
      in
      if compacted then Session_core.persist_compacted_history mgr ~key agent
      else Session_core.persist_new_messages mgr ~key ~history_before agent;
      let runtime_context =
        Prompt_builder.build_runtime_context ~config:mgr.config ~md_skills
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
        let msgs =
          Session_core.take_all_queued_messages_for_injection mgr ~key
        in
        List.map
          (fun (qm : Session_core.queued_message) ->
            Session_core.queued_message_prompt
              (Session_core.effective_message_for_turn ~message:qm.message
                 ?channel_name:qm.channel_name ?channel_type:qm.channel_type
                 ?sender_id:qm.sender_id ?sender_name:qm.sender_name
                 ?user_group:qm.user_group ()))
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
             A postmortem agent has been launched to analyze this failure and \
             look for solutions. While it works, you can:\n\
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
                      ~inject_messages ?on_tool_round_complete ~runtime_context
                      ~on_history_update ~on_stuck ()
                | _ ->
                    Agent.turn agent ~user_message:effective_message ?db:mgr.db
                      ~session_key:key ~interrupt_check ~inject_messages
                      ?on_tool_round_complete ?runtime_context
                      ~history_prepared:true ~on_history_update ~on_stuck ()))
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
              | Session_observer.Ok | Session_observer.Error _ ->
                  Lwt.return_unit
              | Session_observer.Stuck { reason; _ } ->
                  Logs.warn (fun m ->
                      m "[observer] message-count check: stuck session=%s: %s"
                        key reason);
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
             ?sender_id:queued.sender_id ?sender_name:queued.sender_name
             ?user_group:queued.user_group ())
      in
      let* response =
        run_locked_turn mgr ~key agent interrupt ~message:injected_message
          ~content_parts:queued.content_parts ?channel_name:queued.channel_name
          ?channel_type:queued.channel_type ?sender_id:queued.sender_id
          ?sender_name:queued.sender_name ?user_group:queued.user_group
          ?channel:queued.channel ?channel_id:queued.channel_id ()
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
      Logs.info (fun m ->
          m
            "Pausing drain for session %s: no notifier registered; message \
             preserved in queue"
            key);
      let existing =
        match Hashtbl.find_opt mgr.Session_core.queued_messages key with
        | Some msgs -> msgs
        | None -> []
      in
      Hashtbl.replace mgr.Session_core.queued_messages key (queued :: existing);
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
    ?(skill_injections = []) ?channel_name ?channel_type ?sender_id ?sender_name
    ?user_group ?channel ?channel_id ?message_id ?cwd ?before_drain () =
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
              user_group;
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
                    (match cwd with
                    | Some c -> (
                        let old_cwd = agent.Agent.effective_cwd in
                        agent.Agent.effective_cwd <- Some c;
                        (match old_cwd with
                        | Some prev when prev <> c ->
                            let event_msg =
                              Provider.make_message ~role:"event"
                                ~content:
                                  (Printf.sprintf
                                     "[system] Working directory changed from \
                                      %s to %s"
                                     prev c)
                            in
                            agent.Agent.history <-
                              agent.Agent.history @ [ event_msg ]
                        | _ -> ());
                        match mgr.Session_core.db with
                        | Some db ->
                            Memory.set_session_cwd ~db ~session_key:key
                              ~cwd:(Some c)
                        | None -> ())
                    | None -> ());
                    let* response =
                      run_locked_turn mgr ~key agent interrupt ~message
                        ~content_parts ~attachments ~skill_injections
                        ?channel_name ?channel_type ?sender_id ?sender_name
                        ?user_group ?channel ?channel_id ()
                    in
                    (* Persist effective_cwd after turn (may have changed via
                       change_working_dir tool) *)
                    (match mgr.Session_core.db with
                    | Some db ->
                        Memory.set_session_cwd ~db ~session_key:key
                          ~cwd:agent.Agent.effective_cwd
                    | None -> ());
                    let* () =
                      match before_drain with
                      | Some f -> f response
                      | None -> Lwt.return_unit
                    in
                    let* () =
                      drain_queued_messages mgr ~key agent interrupt ()
                    in
                    Lwt.return response)))

let try_turn mgr ~key ~message ?(content_parts = []) ?(attachments = [])
    ?(skill_injections = []) ?channel_name ?channel_type ?sender_id ?sender_name
    ?user_group ?channel ?channel_id ?message_id ?on_tool_round_complete
    ?before_drain () =
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
      | Some response -> Lwt.return_some response
      | None ->
          Session_core.try_session_lock mgr ~key (fun agent interrupt ->
              Session_core.with_in_flight mgr (fun () ->
                  let* response =
                    run_locked_turn mgr ~key agent interrupt ~message
                      ~content_parts ~attachments ~skill_injections
                      ?channel_name ?channel_type ?sender_id ?sender_name
                      ?user_group ?channel ?channel_id ?on_tool_round_complete
                      ()
                  in
                  let* () =
                    match before_drain with
                    | Some f -> f response
                    | None -> Lwt.return_unit
                  in
                  let* () = drain_queued_messages mgr ~key agent interrupt () in
                  Lwt.return response)))

let () =
  spawn_postmortem_agent_fn :=
    fun mgr ~stuck_history ~session_key ~reason ?db () ->
      let open Lwt.Syntax in
      let postmortem_session_key =
        Printf.sprintf "__postmortem_%s@%d" session_key
          (int_of_float (Unix.gettimeofday ()))
      in
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

let apply_template_tool_restrictions = Agent_template.filter_tool_registry

let agent_invoke_turn mgr ~agent_name ~prompt ~send_reply =
  match Agent_template.resolve agent_name with
  | None ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              send_reply
                (Printf.sprintf
                   "Agent template '%s' not found. Use /agent list to see \
                    available templates."
                   agent_name))
            (fun _ -> Lwt.return_unit))
  | Some tmpl ->
      if mgr.Session_core.draining then
        Lwt.async (fun () ->
            Lwt.catch
              (fun () -> send_reply Session_core.draining_message)
              (fun _ -> Lwt.return_unit))
      else
        Lwt.async (fun () ->
            Session_core.with_in_flight mgr (fun () ->
                let tool_registry = resolve_agent_template_registry mgr tmpl in
                let agent =
                  Agent.create ~config:mgr.config ?tool_registry
                    ~agent_template:tmpl ()
                in
                agent.Agent.on_project_doc_loaded <-
                  Some
                    (fun msg ->
                      Lwt.catch
                        (fun () -> send_reply msg)
                        (fun _ -> Lwt.return_unit));
                Lwt.catch
                  (fun () ->
                    let open Lwt.Syntax in
                    let* response = Agent.turn agent ~user_message:prompt () in
                    send_reply response)
                  (fun exn ->
                    Logs.err (fun m ->
                        m "Agent invoke failed (%s): %s" agent_name
                          (Printexc.to_string exn));
                    Lwt.catch
                      (fun () ->
                        send_reply
                          (Printf.sprintf "Agent invoke failed (%s): %s"
                             agent_name (Printexc.to_string exn)))
                      (fun _ -> Lwt.return_unit))))

let delegate_turn mgr ?agent_name ~prompt ~send_reply () =
  if mgr.Session_core.draining then
    Lwt.async (fun () ->
        Lwt.catch
          (fun () -> send_reply Session_core.draining_message)
          (fun _ -> Lwt.return_unit))
  else
    let resolve_template () =
      match agent_name with
      | None -> Some (None, mgr.Session_core.tool_registry)
      | Some name -> (
          match Agent_template.resolve name with
          | None -> None
          | Some tmpl ->
              let tool_registry = resolve_agent_template_registry mgr tmpl in
              Some (Some tmpl, tool_registry))
    in
    match resolve_template () with
    | None ->
        Lwt.async (fun () ->
            Lwt.catch
              (fun () ->
                send_reply
                  (Printf.sprintf
                     "Agent template '%s' not found. Use /agent list to see \
                      available templates."
                     (Option.value ~default:"" agent_name)))
              (fun _ -> Lwt.return_unit))
    | Some (agent_template, tool_registry) ->
        Lwt.async (fun () ->
            Session_core.with_in_flight mgr (fun () ->
                let agent =
                  Agent.create ~config:mgr.config ?tool_registry ?agent_template
                    ()
                in
                agent.Agent.on_project_doc_loaded <-
                  Some
                    (fun msg ->
                      Lwt.catch
                        (fun () -> send_reply msg)
                        (fun _ -> Lwt.return_unit));
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

let fork_and_run mgr ~parent_key ?agent_name ~prompt ~send_reply () =
  if mgr.Session_core.draining then
    Lwt.async (fun () ->
        Lwt.catch
          (fun () -> send_reply Session_core.draining_message)
          (fun _ -> Lwt.return_unit))
  else
    let resolve_fork_template () =
      match agent_name with
      | None -> Some (None, mgr.Session_core.tool_registry, prompt)
      | Some name -> (
          match Agent_template.resolve name with
          | None -> None
          | Some tmpl ->
              let tool_registry = resolve_agent_template_registry mgr tmpl in
              let wrapped_prompt =
                Printf.sprintf
                  "Adopt this agent profile and follow the user's prompt:\n\n\
                   %s\n\n\
                   User Prompt: %s"
                  tmpl.system_prompt prompt
              in
              Some (None, tool_registry, wrapped_prompt))
    in
    match resolve_fork_template () with
    | None ->
        Lwt.async (fun () ->
            Lwt.catch
              (fun () ->
                send_reply
                  (Printf.sprintf
                     "Agent template '%s' not found. Use /agent list to see \
                      available templates."
                     (Option.value ~default:"" agent_name)))
              (fun _ -> Lwt.return_unit))
    | Some (_agent_template, tool_registry, effective_prompt) ->
        Lwt.async (fun () ->
            Session_core.with_in_flight mgr (fun () ->
                let open Lwt.Syntax in
                let* parent_history =
                  Session_core.snapshot_history mgr ~key:parent_key
                in
                let agent = Agent.create ~config:mgr.config ?tool_registry () in
                agent.Agent.on_project_doc_loaded <-
                  Some
                    (fun msg ->
                      Lwt.catch
                        (fun () -> send_reply msg)
                        (fun _ -> Lwt.return_unit));
                agent.Agent.history <- List.rev parent_history;
                Lwt.catch
                  (fun () ->
                    let* response =
                      Agent.turn agent ~user_message:effective_prompt ()
                    in
                    send_reply response)
                  (fun exn ->
                    Logs.err (fun m ->
                        m "Fork failed for parent=%s: %s" parent_key
                          (Printexc.to_string exn));
                    Lwt.catch
                      (fun () ->
                        send_reply
                          (Printf.sprintf "Fork failed: %s"
                             (Printexc.to_string exn)))
                      (fun _ -> Lwt.return_unit))))

let turn_stream mgr ~key ~message ?(content_parts = []) ?(attachments = [])
    ?(skill_injections = []) ?channel_name ?channel_type ?sender_id ?sender_name
    ?user_group ?channel ?channel_id ?message_id ?cwd ?on_tool_round_complete
    ?on_drain_progress ?before_drain ~on_chunk () =
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
              user_group;
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
                    (match cwd with
                    | Some c -> (
                        let old_cwd = agent.Agent.effective_cwd in
                        agent.Agent.effective_cwd <- Some c;
                        (match old_cwd with
                        | Some prev when prev <> c ->
                            let event_msg =
                              Provider.make_message ~role:"event"
                                ~content:
                                  (Printf.sprintf
                                     "[system] Working directory changed from \
                                      %s to %s"
                                     prev c)
                            in
                            agent.Agent.history <-
                              agent.Agent.history @ [ event_msg ]
                        | _ -> ());
                        match mgr.Session_core.db with
                        | Some db ->
                            Memory.set_session_cwd ~db ~session_key:key
                              ~cwd:(Some c)
                        | None -> ())
                    | None -> ());
                    let interrupt_check () = !interrupt in
                    interrupt := None;
                    (* Check for @agent mention at start of message *)
                    let notify_fn text =
                      on_chunk (Provider.Delta (text ^ "\n"))
                    in
                    let* agent_response =
                      handle_agent_mention mgr ~notify:notify_fn message
                    in
                    match agent_response with
                    | Some response ->
                        let* () = on_chunk (Provider.Delta response) in
                        let* () = on_chunk Provider.Done in
                        Lwt.return response
                    | None ->
                        let* message, auto_injections, auto_md_skills =
                          !expand_skill_refs_fn message
                        in
                        let all_injections =
                          skill_injections @ auto_injections
                        in
                        let md_skills =
                          if user_group = Some "admin" then auto_md_skills
                          else
                            List.filter
                              (fun (name, _) ->
                                not (Builtin_skills.is_test_skill_name name))
                              auto_md_skills
                        in
                        (* Send "Loaded skill: X" notification for @mention skills *)
                        if auto_injections <> [] then
                          notify_skill_loads
                            ~send:(fun text ->
                              on_chunk (Provider.Delta (text ^ "\n")))
                            auto_injections;
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
                        let all_injections =
                          dedup_skill_injections ~history:agent.Agent.history
                            all_injections
                        in
                        List.iter
                          (fun content ->
                            agent.Agent.history <-
                              Provider.make_message ~role:"system" ~content
                              :: agent.Agent.history)
                          all_injections;
                        let effective_message =
                          Session_core.effective_message_for_turn ~message
                            ?channel_name ?channel_type ?sender_id ?sender_name
                            ?user_group ()
                        in
                        let history_before = List.length agent.history in
                        let notify =
                          Session_core.find_registered_notifier mgr ~key
                        in
                        (match notify with
                        | Some send ->
                            agent.Agent.on_project_doc_loaded <-
                              Some
                                (fun msg ->
                                  Lwt.catch
                                    (fun () -> send msg)
                                    (fun _ -> Lwt.return_unit))
                        | None -> ());
                        let refresh_messages =
                          let ws_msgs =
                            match
                              Agent.note_external_workspace_refresh_if_needed
                                agent
                            with
                            | Some msg -> [ msg ]
                            | None -> []
                          in
                          let pd_msgs =
                            match
                              Agent.refresh_project_docs_if_changed agent
                            with
                            | Some msg -> [ msg ]
                            | None -> []
                          in
                          ws_msgs @ pd_msgs
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
                          Session_core.persist_new_messages mgr ~key
                            ~history_before agent;
                        let runtime_context =
                          Prompt_builder.build_runtime_context
                            ~config:mgr.config ~md_skills
                            ~details:
                              (Session_core.runtime_context_details mgr ~agent
                                 ~key ~compacted_before_turn:compacted)
                            ()
                        in
                        let prepared_history_len = List.length agent.history in
                        Session_core.record_agent_turn mgr ~key ?channel
                          ?channel_id ();
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
                            Session_core.take_all_queued_messages_for_injection
                              mgr ~key
                          in
                          List.map
                            (fun (qm : Session_core.queued_message) ->
                              Session_core.queued_message_prompt
                                (Session_core.effective_message_for_turn
                                   ~message:qm.message
                                   ?channel_name:qm.channel_name
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
                                    ~inject_messages ?on_tool_round_complete
                                    ?runtime_context ~history_prepared:true
                                    ~on_history_update ~on_chunk ())
                            (function
                              | Agent.Restart_requested ->
                                  if agent.Agent.compacted_mid_turn then begin
                                    Session_core.persist_compacted_history mgr
                                      ~key agent;
                                    agent.Agent.compacted_mid_turn <- false
                                  end
                                  else
                                    Session_core.persist_new_messages mgr ~key
                                      ~history_before:!persisted_up_to agent;
                                  Session_core.set_response_deferred mgr ~key;
                                  let* () =
                                    on_chunk
                                      (Provider.Delta
                                         Session_core.draining_message)
                                  in
                                  let* () = on_chunk Provider.Done in
                                  Lwt.return Session_core.draining_message
                              | exn ->
                                  if agent.Agent.compacted_mid_turn then begin
                                    Session_core.persist_compacted_history mgr
                                      ~key agent;
                                    agent.Agent.compacted_mid_turn <- false
                                  end
                                  else
                                    Session_core.persist_new_messages mgr ~key
                                      ~history_before:!persisted_up_to agent;
                                  Lwt.fail exn)
                        in
                        if not (Session_core.response_deferred mgr ~key) then begin
                          if agent.Agent.compacted_mid_turn then begin
                            Session_core.persist_compacted_history mgr ~key
                              agent;
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
                        (* Persist effective_cwd after turn (may have changed
                           via change_working_dir tool) *)
                        (match mgr.Session_core.db with
                        | Some db ->
                            Memory.set_session_cwd ~db ~session_key:key
                              ~cwd:agent.Agent.effective_cwd
                        | None -> ());
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
