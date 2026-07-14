open Session_postmortem
include Session_agents

let consume_pending_github_catalog_refresh ?db ?room_id () =
  match (db, room_id) with
  | Some db, Some room_id -> (
      match Github_route_ops.consume_catalog_refresh ~db ~room_id () with
      | None -> ()
      | Some request ->
          Logs.info (fun message ->
              message
                "Applying pending GitHub catalog refresh for room=%s plan=%s \
                 before freezing this turn's Tool catalog"
                request.room_id request.setup_plan_id))
  | _ -> ()

let persist_compacted_if_dirty mgr ~key agent =
  if Agent.take_compaction_dirty agent then (
    try
      Session_core.persist_compacted_history mgr ~key agent;
      true
    with exn ->
      Agent.mark_compacted agent;
      raise exn)
  else false

(* Persist history after a turn. If compaction happened (this turn, or
   out-of-band since the last flush) write the full compacted history; else
   append only the messages added since [history_before]. Reads-and-clears the
   compaction signal via Agent.take_compaction_dirty. *)
let persist_after_turn mgr ~key ~history_before agent =
  if persist_compacted_if_dirty mgr ~key agent then ()
  else Session_core.persist_new_messages mgr ~key ~history_before agent

(* Like [persist_after_turn] but for streaming paths that may have already
   flushed new messages mid-turn: if nothing was appended since
   [history_before], still persist workspace state (effective_cwd may have
   changed). *)
let persist_after_turn_or_workspace mgr ~key ~history_before agent =
  if persist_compacted_if_dirty mgr ~key agent then ()
  else if List.length agent.Agent.history > history_before then
    Session_core.persist_new_messages mgr ~key ~history_before agent
  else Session_core.persist_session_workspace_state mgr ~key agent

type turn_io = { emit : Provider.stream_event -> unit Lwt.t; streaming : bool }

let buffered_turn_io = { emit = (fun _ -> Lwt.return_unit); streaming = false }
let streaming_turn_io emit = { emit; streaming = true }
let emit_progress io text = io.emit (Provider.Delta (text ^ "\n"))

let finish_response io response =
  let open Lwt.Syntax in
  if io.streaming then
    let* () = io.emit (Provider.Delta response) in
    let* () = io.emit Provider.Done in
    Lwt.return response
  else Lwt.return response

let stream_turn_with_visibility mgr ~notify agent ~key ~effective_message
    ~persisted_up_to ~interrupt_check ~inject_messages ?on_tool_round_complete
    ~runtime_context ~on_history_update ?on_stuck ?on_llm_call_debug () =
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
      ~agent_defaults ~parse_mode ()
  in
  let* response =
    Agent.turn_stream agent ~user_message:effective_message ?db:mgr.db
      ~session_key:key ~interrupt_check ~inject_messages
      ~on_inject_messages:handler.reset ?on_tool_round_complete ?runtime_context
      ~history_prepared:true ~on_history_update ?on_stuck ?on_llm_call_debug
      ~on_chunk:handler.on_chunk ()
  in
  let* () = handler.finalize () in
  let thinking = handler.get_thinking () in
  let* () =
    if agent_defaults.show_thinking && thinking <> "" then
      notify (Stream_visibility.thinking_message thinking)
    else Lwt.return_unit
  in
  persist_after_turn mgr ~key ~history_before:!persisted_up_to agent;
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

let expand_skill_refs_fn :
    (?workspace_only:bool ->
    ?skip_loaded:string list ->
    string ->
    (string * string list * (string * string) list) Lwt.t)
    ref =
  ref (fun ?workspace_only:_ ?skip_loaded:_ message ->
      Lwt.return (message, [], []))

let extract_skill_names_from_injections injections =
  List.filter_map Skill_dedup.loaded_skill_name_from_injection injections

let notify_skill_loads ~send injections =
  let names = extract_skill_names_from_injections injections in
  List.iter
    (fun name ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> send (Printf.sprintf "Loaded skill: %s" name))
            (fun exn ->
              Logs.warn (fun m ->
                  m "Skill load notification failed: %s"
                    (Printexc.to_string exn));
              Lwt.return_unit)))
    names

let dedup_skill_injections = Skill_dedup.dedup_skill_injections

let run_locked_turn mgr ~key agent interrupt ~message ?(content_parts = [])
    ?(attachments = []) ?(skill_injections = [])
    ?(md_skills : (string * string) list = []) ?channel_name ?channel_type
    ?sender_id ?sender_name ?user_group ?channel ?channel_id ?message_id
    ?on_tool_round_complete ?(io = buffered_turn_io) () =
  let open Lwt.Syntax in
  let interrupt_check () = !interrupt in
  interrupt := None;
  (* Check for @agent mention at start of message *)
  let notify = Session_core.find_registered_notifier mgr ~key in
  let mention_notify =
    if io.streaming then Some (emit_progress io) else notify
  in
  let* agent_response =
    handle_agent_mention mgr ~key ?notify:mention_notify message
  in
  match agent_response with
  | Some response -> finish_response io response
  | None ->
      let skip_loaded =
        Skill_dedup.loaded_skill_names_in_history agent.Agent.history
      in
      let* message, auto_injections, auto_md_skills =
        !expand_skill_refs_fn ~skip_loaded message
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
      (match Session_core.find_registered_notifier mgr ~key with
      | Some send -> notify_skill_loads ~send skill_injections
      | None when io.streaming ->
          notify_skill_loads ~send:(emit_progress io) skill_injections
      | None -> ());
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
      let on_llm_call_debug =
        Session_heartbeat.debug_callback_for mgr ~key notify
      in
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
      let on_chunk = if io.streaming then Some io.emit else None in
      let* () =
        Session_core.notify_event_messages ?notify ?on_chunk refresh_messages
      in
      let* compaction_info =
        Agent.prepare_turn_history agent ~user_message:effective_message
          ~content_parts ~workspace_refresh_checked:true ?db:mgr.db
          ~session_key:key ?room_id:channel_id ?on_llm_call_debug ()
      in
      let compacted = Option.is_some compaction_info in
      let compaction_notify =
        if io.streaming then Some (emit_progress io) else notify
      in
      let* () =
        Session_core.notify_compaction_if_needed ?notify:compaction_notify
          compaction_info
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
      (* Grounding is deliberately runtime-only: [Agent_turn_core] injects this
         into the outgoing provider request without adding it to history.  A
         Room's GitHub journal must therefore never bleed into a later turn in
         another Room (or into a resumed direct session). *)
      let github_grounding =
        match (mgr.Session_core.db, channel_id) with
        | Some db, Some room_id when String.trim room_id <> "" -> (
            let source =
              match message_id with
              | Some thread_ref when String.trim thread_ref <> "" ->
                  Github_item_context_resolve.Thread_reply
                    {
                      room_id = String.trim room_id;
                      thread_ref = Some (String.trim thread_ref);
                      text = message;
                    }
              | None | Some _ ->
                  Github_item_context_resolve.Room_mention
                    {
                      room_id = String.trim room_id;
                      text = message;
                      item_key_hint = None;
                    }
            in
            match Github_collab_grounding.ground ~db ~source () with
            | Ok grounding -> Some grounding.prompt_block
            | Error err ->
                Logs.warn (fun m ->
                    m "GitHub Room grounding unavailable for this turn: %s" err);
                None)
        | _ -> None
      in
      let runtime_context =
        match (runtime_context, github_grounding) with
        | context, None -> context
        | Some context, Some grounding -> Some (context ^ "\n\n" ^ grounding)
        | None, Some grounding -> Some grounding
      in
      let prepared_history_len = List.length agent.history in
      Session_core.record_agent_turn mgr ~key ?channel ?channel_id ();
      let persisted_up_to = ref prepared_history_len in
      let on_history_update new_msgs =
        (match mgr.db with
        | Some db ->
            List.iter
              (fun msg ->
                Memory.store_message ~db ~session_key:key msg;
                (* B734: fire-and-forget embedding for scoped messages *)
                let scope_info =
                  match channel_id with
                  | Some room_id -> (
                      match Memory.get_room_profile_binding ~db ~room_id with
                      | Some _binding ->
                          Some (Sqlite3.last_insert_rowid db, "room", room_id)
                      | None -> None)
                  | None -> None
                in
                match scope_info with
                | Some (message_id, scope_kind, scope_key) ->
                    Lwt.async (fun () ->
                        Vector.embed_and_store_message ~config:mgr.config ~db
                          ~session_key:key ~message_id ~content:msg.content
                          ~scope_kind ~scope_key ())
                | None -> ())
              new_msgs;
            persisted_up_to := List.length agent.Agent.history
        | None -> ());
        Session_core.notify_event_messages ?notify ?on_chunk new_msgs
      in
      let inject_messages () =
        let msgs =
          Session_core.take_all_queued_messages_for_injection ~interrupt mgr
            ~key
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
        if not mgr.Session_core.config.postmortem.enabled then begin
          Logs.info (fun m ->
              m
                "[observer] postmortem disabled; not injecting correction \
                 message (session=%s)"
                key);
          Lwt.return_unit
        end
        else
          let correction =
            Printf.sprintf
              "[Observer] Stuck pattern detected: %s\n\n\
               A postmortem agent has been launched to analyze this failure \
               and look for solutions. While it works, you can:\n\
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
            let* draining_response =
              Session_core.respond_if_draining ?on_chunk mgr
            in
            match draining_response with
            | Some response -> Lwt.return response
            | None when io.streaming ->
                (* Direct streaming historically does not inject observer
                   corrections or launch postmortems mid-turn. *)
                Agent.turn_stream agent ~user_message:effective_message
                  ?db:mgr.db ~session_key:key ~interrupt_check ~inject_messages
                  ?on_tool_round_complete ?runtime_context
                  ~history_prepared:true ~on_history_update ?on_llm_call_debug
                  ~on_chunk:io.emit ()
            | None -> (
                match notify with
                | Some send
                  when mgr.config.agent_defaults.show_thinking
                       || mgr.config.agent_defaults.show_tool_calls ->
                    stream_turn_with_visibility mgr ~notify:send agent ~key
                      ~effective_message ~persisted_up_to ~interrupt_check
                      ~inject_messages ?on_tool_round_complete ~runtime_context
                      ~on_history_update ~on_stuck ?on_llm_call_debug ()
                | _ ->
                    Agent.turn agent ~user_message:effective_message ?db:mgr.db
                      ~session_key:key ~interrupt_check ~inject_messages
                      ?on_tool_round_complete ?runtime_context
                      ~history_prepared:true ~on_history_update ~on_stuck
                      ?on_llm_call_debug ()))
          (function
            | Agent.Restart_requested ->
                persist_after_turn mgr ~key ~history_before:!persisted_up_to
                  agent;
                Session_core.set_response_deferred mgr ~key;
                finish_response io Session_core.draining_message
            | exn ->
                persist_after_turn mgr ~key ~history_before:!persisted_up_to
                  agent;
                Lwt.fail exn)
      in
      (match notify with
      | Some _
        when (not io.streaming)
             && (mgr.config.agent_defaults.show_thinking
               || mgr.config.agent_defaults.show_tool_calls) ->
          ()
      | _ ->
          if not (Session_core.response_deferred mgr ~key) then begin
            persist_after_turn_or_workspace mgr ~key
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

(** [evaluate_room_policy config ~key ~channel ~channel_id ~user_group
     ?has_external_users ()] evaluates the external room policy for the current
    turn. Returns [Ok (classification, decision_string)] if work should proceed,
    or [Error msg] if work should be denied.

    This function now also enforces invocation restrictions by scope, checking
    role/member/admin rules before allowing work to proceed. *)
let evaluate_room_policy (config : Runtime_config.t) ~key ~channel ~channel_id
    ~user_group ?(has_external_users = false) () =
  Invocation_restrict.check_room_policy_and_role ~config ~key ~channel
    ~channel_id ~user_group ~has_external_users ~work_kind:Room_work ()

let rec drain_queued_messages_loop mgr ~key agent interrupt ?on_drain_progress
    ~drained_any () =
  match Session_core.take_next_queued_message_for_drain mgr ~key with
  | Some queued when Session_core.is_queued_admin_stop_message queued ->
      Session_core.handle_queued_admin_stop mgr ~key interrupt queued;
      Lwt.return_unit
  | Some queued -> (
      match Session_core.find_registered_notifier mgr ~key with
      | Some notify ->
          let open Lwt.Syntax in
          Logs.info (fun m ->
              m "Sending queued message to LLM for session %s" key);
          let* () =
            match on_drain_progress with
            | Some dp -> dp.Session_core.before_turn queued.message_id
            | None -> Lwt.return_unit
          in
          let turn_message =
            if queued.deferred_followup then queued.message
            else
              Session_core.queued_message_prompt
                (Session_core.effective_message_for_turn ~message:queued.message
                   ?channel_name:queued.channel_name
                   ?channel_type:queued.channel_type ?sender_id:queued.sender_id
                   ?sender_name:queued.sender_name ?user_group:queued.user_group
                   ())
          in
          (* Record effective-access snapshot when queued work begins and
             store the snapshot on the agent. Clear any stale snapshot from
             a previous turn when no snapshot_work_type is set.
             Also record room classification for the snapshot. *)
          let room_cls, room_dec =
            match
              evaluate_room_policy mgr.Session_core.config ~key
                ~channel:queued.channel ~channel_id:queued.channel_id
                ~user_group:queued.user_group
                ~has_external_users:queued.has_external_users ()
            with
            | Ok (cls, dec) -> (cls.scope, dec)
            | Error msg ->
                Logs.warn (fun m -> m "Room policy denied: %s" msg);
                (Runtime_config_types.Rm_unknown, "deny: " ^ msg)
          in
          consume_pending_github_catalog_refresh ?db:mgr.Session_core.db
            ?room_id:queued.channel_id ();
          (match queued.snapshot_work_type with
          | Some work_type -> (
              match mgr.Session_core.db with
              | Some db ->
                  let snap =
                    Access_snapshot.create_and_persist ~db
                      ~config:mgr.Session_core.config ~work_type
                      ~session_key:key ?room_id:queued.channel_id
                      ~room_classification:room_cls
                      ~room_policy_decision:room_dec ()
                  in
                  agent.Agent.access_snapshot_id <- Some snap.id;
                  agent.Agent.access_snapshot <- Some snap
              | None -> ())
          | None ->
              agent.Agent.access_snapshot_id <- None;
              agent.Agent.access_snapshot <- None);
          let* response =
            run_locked_turn mgr ~key agent interrupt ~message:turn_message
              ~content_parts:queued.content_parts
              ?channel_name:queued.channel_name
              ?channel_type:queued.channel_type ?sender_id:queued.sender_id
              ?sender_name:queued.sender_name ?user_group:queued.user_group
              ?channel:queued.channel ?channel_id:queued.channel_id
              ?message_id:queued.message_id ()
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
      | None ->
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
          Hashtbl.replace mgr.Session_core.queued_messages key
            (existing @ [ queued ]);
          Lwt.return_unit)
  | None ->
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

let run_session_turn mgr ~io ~key ~message ?(content_parts = [])
    ?(attachments = []) ?(skill_injections = []) ?channel_name ?channel_type
    ?sender_id ?sender_name ?user_group ?channel ?channel_id ?message_id ?cwd
    ?(deferred_if_busy = false) ?on_tool_round_complete ?on_drain_progress
    ?before_drain ?snapshot_work_type ?(has_external_users = false) () =
  Session_core.with_live_activity mgr ~key (fun () ->
      let open Lwt.Syntax in
      let* () = Session_core.mark_autonomous_activity_started mgr ~key in
      let raw_message = message in
      let* message = normalize_incoming_message mgr ~key ~message in
      let send_progress =
        if io.streaming then Some (emit_progress io)
        else Session_core.find_registered_notifier mgr ~key
      in
      let* handled =
        Session_core.handle_special_command mgr ~key ~message ?send_progress
          ~interrupt_check:(Session_core.interrupt_check_if_present mgr ~key)
          ()
      in
      match handled with
      | Some response -> finish_response io response
      | None -> (
          (* Check external room policy before proceeding with work *)
          let policy_denial =
            (* The public streaming path historically records policy in its
               access snapshot but does not short-circuit the turn. *)
            if io.streaming then None
            else
              match
                evaluate_room_policy mgr.Session_core.config ~key ~channel
                  ~channel_id ~user_group ~has_external_users ()
              with
              | Ok _ -> None
              | Error msg -> Some msg
          in
          match policy_denial with
          | Some deny_msg -> finish_response io deny_msg
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
                  bang = false;
                  deferred_followup = deferred_if_busy;
                  snapshot_work_type;
                  has_external_users;
                }
              in
              let on_draining () =
                let on_chunk = if io.streaming then Some io.emit else None in
                let* draining_response =
                  Session_core.respond_if_draining ?on_chunk mgr
                in
                match draining_response with
                | Some response -> Lwt.return response
                | None -> Lwt.return Session_core.draining_message
              in
              let run_with_lock agent interrupt =
                Session_core.with_in_flight mgr (fun () ->
                    (* Record effective-access snapshot when work begins and
                   store the snapshot on the agent so tools can use
                   snapshot-scoped access instead of re-resolving from
                   the live config. Clear any stale snapshot from a
                   previous turn when no snapshot_work_type is set.
                   Also evaluate the external room policy. *)
                    let room_classification_for_snap, room_decision_for_snap =
                      match
                        evaluate_room_policy mgr.Session_core.config ~key
                          ~channel ~channel_id ~user_group ~has_external_users
                          ()
                      with
                      | Ok (cls, dec) -> (cls.scope, dec)
                      | Error _msg -> (Runtime_config_types.Rm_unknown, "denied")
                    in
                    consume_pending_github_catalog_refresh
                      ?db:mgr.Session_core.db ?room_id:channel_id ();
                    (match snapshot_work_type with
                    | Some work_type -> (
                        match mgr.Session_core.db with
                        | Some db ->
                            let snap =
                              Access_snapshot.create_and_persist ~db
                                ~config:mgr.Session_core.config ~work_type
                                ~session_key:key ?room_id:channel_id
                                ~room_classification:
                                  room_classification_for_snap
                                ~room_policy_decision:room_decision_for_snap ()
                            in
                            agent.Agent.access_snapshot_id <- Some snap.id;
                            agent.Agent.access_snapshot <- Some snap
                        | None -> ())
                    | None ->
                        agent.Agent.access_snapshot_id <- None;
                        agent.Agent.access_snapshot <- None);
                    (match cwd with
                    | Some c ->
                        Session_room_profile.apply_cwd_change_for_turn mgr ~key
                          agent ~cwd:c
                    | None -> ());
                    let* response =
                      run_locked_turn mgr ~key agent interrupt ~message
                        ~content_parts ~attachments ~skill_injections
                        ?channel_name ?channel_type ?sender_id ?sender_name
                        ?user_group ?channel ?channel_id ?message_id
                        ?on_tool_round_complete ~io ()
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
                      drain_queued_messages mgr ~key agent interrupt
                        ?on_drain_progress ()
                    in
                    Lwt.return response)
              in
              if deferred_if_busy then
                let rec send_now_or_queue () =
                  if mgr.Session_core.draining then on_draining ()
                  else
                    let* locked =
                      Session_core.try_session_lock mgr ~key run_with_lock
                    in
                    match locked with
                    | Some response -> Lwt.return response
                    | None -> (
                        let* outcome =
                          Session_core.enqueue_followup_if_busy mgr ~key
                            queued_message
                        in
                        match outcome with
                        | `Queued | `Appended ->
                            Lwt.return Session_core.queued_message_response
                        | `Idle ->
                            let* () = Lwt.pause () in
                            send_now_or_queue ())
                in
                send_now_or_queue ()
              else
                let* queued =
                  Session_core.enqueue_message_if_busy mgr ~key ~raw_message
                    queued_message
                in
                if queued then Lwt.return Session_core.queued_message_response
                else
                  Session_core.with_session_lock_unless_draining mgr ~key
                    ~on_draining run_with_lock))

let turn mgr ~key ~message ?(content_parts = []) ?(attachments = [])
    ?(skill_injections = []) ?channel_name ?channel_type ?sender_id ?sender_name
    ?user_group ?channel ?channel_id ?message_id ?cwd
    ?(deferred_if_busy = false) ?before_drain ?snapshot_work_type
    ?(has_external_users = false) () =
  run_session_turn mgr ~io:buffered_turn_io ~key ~message ~content_parts
    ~attachments ~skill_injections ?channel_name ?channel_type ?sender_id
    ?sender_name ?user_group ?channel ?channel_id ?message_id ?cwd
    ~deferred_if_busy ?before_drain ?snapshot_work_type ~has_external_users ()

let try_turn mgr ~key ~message ?(content_parts = []) ?(attachments = [])
    ?(skill_injections = []) ?channel_name ?channel_type ?sender_id ?sender_name
    ?user_group ?channel ?channel_id ?message_id ?on_tool_round_complete
    ?before_drain () =
  Session_core.with_live_activity mgr ~key (fun () ->
      let open Lwt.Syntax in
      let* () = Session_core.mark_autonomous_activity_started mgr ~key in
      let raw_message = message in
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
          let* stopped =
            Session_core.stop_busy_session_if_admin_stop mgr ~key
              ~message:raw_message ?user_group ()
          in
          if stopped then Lwt.return_some Agent.stopped_by_admin_message
          else
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
                    let* () =
                      drain_queued_messages mgr ~key agent interrupt ()
                    in
                    Lwt.return response)))

let () =
  Session_postmortem_launcher.install
    ~turn:(fun mgr ~key ~message () -> turn mgr ~key ~message ())
    ()

let turn_stream mgr ~key ~message ?(content_parts = []) ?(attachments = [])
    ?(skill_injections = []) ?channel_name ?channel_type ?sender_id ?sender_name
    ?user_group ?channel ?channel_id ?message_id ?cwd ?on_tool_round_complete
    ?on_drain_progress ?before_drain ?snapshot_work_type ~on_chunk () =
  run_session_turn mgr
    ~io:(streaming_turn_io on_chunk)
    ~key ~message ~content_parts ~attachments ~skill_injections ?channel_name
    ?channel_type ?sender_id ?sender_name ?user_group ?channel ?channel_id
    ?message_id ?cwd ?on_tool_round_complete ?on_drain_progress ?before_drain
    ?snapshot_work_type ()
