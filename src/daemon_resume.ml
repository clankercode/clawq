type resume_senders = {
  send_telegram :
    bot_token:string -> chat_id:string -> text:string -> unit Lwt.t;
  send_discord :
    bot_token:string -> channel_id:string -> text:string -> unit Lwt.t;
  send_slack :
    bot_token:string -> channel_id:string -> text:string -> unit Lwt.t;
  send_teams :
    config:Runtime_config.teams_config ->
    channel_id:string ->
    text:string ->
    string Lwt.t;
}

let default_resume_senders =
  {
    send_telegram =
      (fun ~bot_token ~chat_id ~text ->
        Telegram.send_message ~disable_notification:true ~bot_token ~chat_id
          ~text ());
    send_discord = Discord.send_message;
    send_slack = Slack.send_message;
    send_teams = Teams.send_message;
  }

type boot_resume_summary = {
  pending_count : int;
  resumed_count : int;
  missing_channel_count : int;
  failed_count : int;
}

let boot_resume_summary_message (summary : boot_resume_summary) =
  Printf.sprintf "pending=%d resumed=%d missing_channel=%d failed=%d"
    summary.pending_count summary.resumed_count summary.missing_channel_count
    summary.failed_count

let dispatch_resumed_message ?(senders = default_resume_senders)
    ~(config : Runtime_config.t) ~channel ~channel_id ~text () =
  let open Lwt.Syntax in
  match channel with
  | "telegram" -> (
      match config.channels.telegram with
      | Some { accounts = (_, account) :: _ } ->
          let* () =
            senders.send_telegram ~bot_token:account.bot_token
              ~chat_id:channel_id ~text
          in
          Lwt.return (Ok ())
      | Some { accounts = [] } | None ->
          Lwt.return (Error "telegram channel is not configured"))
  | "discord" -> (
      match config.channels.discord with
      | Some discord ->
          let* () =
            senders.send_discord ~bot_token:discord.bot_token ~channel_id ~text
          in
          Lwt.return (Ok ())
      | None -> Lwt.return (Error "discord channel is not configured"))
  | "slack" -> (
      match config.channels.slack with
      | Some slack ->
          let* () =
            senders.send_slack ~bot_token:slack.bot_token ~channel_id ~text
          in
          Lwt.return (Ok ())
      | None -> Lwt.return (Error "slack channel is not configured"))
  | "teams" -> (
      match config.channels.teams with
      | Some tc ->
          let* activity_id = senders.send_teams ~config:tc ~channel_id ~text in
          (* B666: Teams.send_message returns the new activity_id on success, or
             "" when send_reply gave up (missing service_url, no OAuth token, HTTP
             failure). Surface that as Error so callers don't log "delivery
             succeeded" after a Teams ERROR. *)
          if activity_id = "" then
            Lwt.return
              (Error
                 (Printf.sprintf
                    "teams send returned empty activity_id (check daemon.log \
                     for the specific Teams error — usually missing/invalid \
                     service_url in channel_id=%S or expired OAuth token)"
                    channel_id))
          else Lwt.return (Ok ())
      | None -> Lwt.return (Error "teams channel is not configured"))
  | "github" ->
      (* GitHub sessions are fire-and-forget: the agent posts back to GitHub
         via tool calls; there is no channel to route the resumed response to. *)
      Logs.info (fun m ->
          m "Resumed github session repo=%s response=%S" channel_id text);
      Lwt.return (Ok ())
  | _ -> Lwt.return (Error (Printf.sprintf "unsupported channel %s" channel))

let resumed_dispatch_target ~session_key ~channel ~channel_id =
  if session_key = "__main__" then
    Printf.sprintf "session=%s route=main-via-%s:%s" session_key channel
      channel_id
  else Printf.sprintf "session=%s route=%s:%s" session_key channel channel_id

let notify_resumed_session ?(senders = default_resume_senders)
    ~(session_manager : Session.t) ~(config : Runtime_config.t) ~session_key
    ~channel ~channel_id text =
  let open Lwt.Syntax in
  match Session.find_registered_notifier session_manager ~key:session_key with
  | Some notify ->
      Logs.info (fun m ->
          m
            "notify_resumed_session: using registered notifier for session=%s \
             text_len=%d"
            session_key (String.length text));
      Lwt.catch
        (fun () -> notify text)
        (fun exn ->
          Logs.warn (fun m ->
              m "Resumed session notifier failed for %s: %s" session_key
                (Printexc.to_string exn));
          Lwt.return_unit)
  | None ->
      Logs.info (fun m ->
          m
            "notify_resumed_session: dispatching to %s:%s for session=%s \
             text_len=%d"
            channel channel_id session_key (String.length text));
      Lwt.catch
        (fun () ->
          let* result =
            dispatch_resumed_message ~senders ~config ~channel ~channel_id ~text
              ()
          in
          match result with
          | Ok () ->
              Logs.info (fun m ->
                  m "notify_resumed_session: dispatch ok for %s via %s:%s"
                    session_key channel channel_id);
              Lwt.return_unit
          | Error err ->
              Logs.warn (fun m ->
                  m "Failed to send resumed session notice for %s via %s:%s: %s"
                    session_key channel channel_id err);
              Lwt.return_unit)
        (fun exn ->
          Logs.warn (fun m ->
              m "Resumed session notice dispatch failed for %s: %s" session_key
                (Printexc.to_string exn));
          Lwt.return_unit)

let post_dispatch_resumed_session_response
    ?(continuation_delay = Session.default_autonomous_continuation_delay)
    ?(senders = default_resume_senders) ~(session_manager : Session.t)
    ~(config : Runtime_config.t) ~session_key ~channel ~channel_id ~response ()
    =
  let trimmed = String.trim response in
  let target = resumed_dispatch_target ~session_key ~channel ~channel_id in
  Logs.info (fun m ->
      m "Resume continuation evaluation starting %s response_len=%d" target
        (String.length trimmed));
  if trimmed = "" || trimmed = "HEARTBEAT_OK" then begin
    Logs.info (fun m ->
        m
          "Resume continuation stayed idle %s reason=no-follow-up-response \
           after_restart_resume=true"
          target);
    Lwt.return_unit
  end
  else if trimmed = Session.autonomous_stay_idle_message then begin
    let open Lwt.Syntax in
    let* () =
      Session.process_autonomous_turn_result ~delay:continuation_delay
        session_manager ~key:session_key ~response:trimmed
    in
    Logs.info (fun m ->
        m
          "Resume continuation disarmed %s reason=agent-requested-idle \
           after_restart_resume=true"
          target);
    Lwt.return_unit
  end
  else begin
    let on_response follow_up =
      let open Lwt.Syntax in
      Logs.info (fun m ->
          m "Resume continuation sending follow-up %s follow_up_len=%d" target
            (String.length (String.trim follow_up)));
      let* () =
        if session_key = "__main__" then Lwt.return_unit
        else
          notify_resumed_session ~senders ~session_manager ~config ~session_key
            ~channel ~channel_id follow_up
      in
      Logs.info (fun m -> m "Resume continuation follow-up sent %s" target);
      Lwt.return_unit
    in
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            Session.process_autonomous_turn_result ~delay:continuation_delay
              ~on_response session_manager ~key:session_key ~response:trimmed)
          (fun exn ->
            Logs.err (fun m ->
                m "Resume continuation failed %s: %s" target
                  (Printexc.to_string exn));
            Lwt.return_unit));
    Logs.info (fun m -> m "Resume continuation armed %s" target);
    Lwt.return_unit
  end

let handle_heartbeat_response
    ?(continuation_delay = Session.default_autonomous_continuation_delay)
    ~(session_manager : Session.t) ~key ~response () =
  let open Lwt.Syntax in
  let trimmed = String.trim response in
  let notify_heartbeat text =
    match Restart_notify.parse_channel_from_key key with
    | Some (channel, channel_id) ->
        notify_resumed_session ~session_manager
          ~config:(Session.get_config session_manager)
          ~session_key:key ~channel ~channel_id text
    | None ->
        Logs.warn (fun m ->
            m "Heartbeat: session %s has no routable channel target" key);
        Lwt.return_unit
  in
  let on_response follow_up =
    let follow_up = String.trim follow_up in
    if follow_up = "" || follow_up = "HEARTBEAT_OK" then Lwt.return_unit
    else notify_heartbeat follow_up
  in
  let* () =
    if
      trimmed = "" || trimmed = "HEARTBEAT_OK"
      || trimmed = Session.autonomous_stay_idle_message
    then
      Session.process_autonomous_turn_result ~delay:continuation_delay
        session_manager ~key ~response:trimmed
    else begin
      let* () = notify_heartbeat trimmed in
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              Session.process_autonomous_turn_result ~delay:continuation_delay
                ~on_response session_manager ~key ~response:trimmed)
            (fun exn ->
              Logs.err (fun m ->
                  m "Heartbeat continuation error: %s" (Printexc.to_string exn));
              Lwt.return_unit));
      Lwt.return_unit
    end
  in
  if trimmed = "HEARTBEAT_OK" then
    Logs.info (fun m -> m "Heartbeat: agent replied HEARTBEAT_OK, no outbound")
  else begin
    Logs.info (fun m ->
        m "Heartbeat: agent response (%d chars)" (String.length trimmed));
    if Restart_notify.parse_channel_from_key key = None then
      Logs.warn (fun m ->
          m
            "Heartbeat: agent wants to send a message but session %s is not \
             routable"
            key)
  end;
  Lwt.return_unit

let resume_user_notice =
  "[automatic restart-resume] The daemon restarted while this session had \
   active work in progress. Resuming now..."

let resume_turn_prompt =
  "Automatic restart-resume: the daemon restarted while autonomous work was "
  ^ "actively in progress in this session. Resume the interrupted work now "
  ^ "— review the conversation history to identify where you left off, pick "
  ^ "up the highest-priority unfinished task, and continue executing from "
  ^ "that point. Limit yourself to at most 3 tool-call iterations. Produce a "
  ^ "text response in this same turn — do not wait for a follow-up message. "
  ^ "If you cannot reach a conclusion in 3 iterations, summarise what is in "
  ^ "progress and stop. "
  ^ "IMPORTANT: Do not call update_clawq during a restart-resume turn — the "
  ^ "daemon has just restarted and triggering another restart would cause a "
  ^ "boot loop."

(* Agent says STAY_IDLE too much so let's pretend it doesn't exist and see what happens *)
(*^ "(If, after checking the full conversation state, you have confirmed that "
  ^ "there is absolutely no way to continue, reply exactly STAY_IDLE.)"*)

(* B673: stuck/watchdog/postmortem messages from a previous session epoch are
   injected into history by the observer + agent watchdog when a session loops.
   On restart-resume they get replayed verbatim to the LLM, which biases the
   model toward repeating the failure mode. Strip them so the post-restart turn
   starts from clean conversational state.

   Conservative heuristic: only drop messages whose content starts with a
   well-known noise prefix. Keep everything else (user messages, normal
   assistant turns, tool calls + results). *)
let is_resume_noise_message (m : Provider.message) =
  let starts_with prefix s =
    let pl = String.length prefix in
    String.length s >= pl && String.sub s 0 pl = prefix
  in
  let c = m.Provider.content in
  (m.role = "user" && starts_with "[Observer] Stuck pattern detected:" c)
  || (m.role = "assistant" && starts_with "[Watchdog] Pausing this session" c)
  || m.role = "assistant"
     && starts_with "Aborted turn after " c
     &&
       try
         ignore (String.index c '\'');
         true
       with Not_found -> false

let sanitize_history_for_resume (history : Provider.message list) =
  let filtered =
    List.filter (fun m -> not (is_resume_noise_message m)) history
  in
  let dropped = List.length history - List.length filtered in
  (filtered, dropped)

let default_resume_turn ?on_history_persisted ~(session_manager : Session.t)
    ~notify ~session_key agent interrupt =
  let open Lwt.Syntax in
  let sanitized, dropped_count =
    sanitize_history_for_resume agent.Agent.history
  in
  let on_llm_call_debug =
    Session.debug_callback_for session_manager ~key:session_key (Some notify)
  in
  if dropped_count > 0 then begin
    Logs.info (fun m ->
        m
          "B673: restart-resume sanitize dropped %d noise message(s) from \
           history for session=%s"
          dropped_count session_key);
    agent.Agent.history <- sanitized
  end;
  Agent.refresh_profiled_room_flag agent ?db:session_manager.db ~session_key ();
  let* compaction_info =
    Agent.compact_history_if_needed agent ?db:session_manager.db
      ?on_llm_call_debug ()
  in
  let compacted = Option.is_some compaction_info in
  let* () = Session.notify_compaction_if_needed ~notify compaction_info in
  if compacted then
    Session.persist_compacted_history session_manager ~key:session_key agent;
  Logs.info (fun m ->
      m "Firing automatic restart-resume prompt for session=%s prompt_len=%d"
        session_key
        (String.length resume_turn_prompt));
  let history_before_resume_prompt = List.length agent.Agent.history in
  (* The resume prompt must be a `user` message, not `system`. With history
     newest-first, build_messages reverses it so a prepended `system` message
     becomes the *last* message in the request — and for a freshly-resumed
     session (e.g. a github workflow_run with near-empty history) the payload
     ends up all-system with no user turn. OpenAI-compatible providers reject
     that: z.ai returns HTTP 400 code 1214 ("messages parameter is illegal").
     Using `user` also lets inject_runtime_context attach the resume runtime
     context (it only augments the last user message), which a `system` role
     silently dropped. *)
  agent.Agent.history <-
    Provider.make_message ~role:"user" ~content:resume_turn_prompt
    :: agent.Agent.history;
  Session.persist_new_messages session_manager ~key:session_key
    ~history_before:history_before_resume_prompt agent;
  Option.iter
    (fun f -> f (List.length agent.Agent.history))
    on_history_persisted;
  let runtime_context =
    Prompt_builder.build_runtime_context ~config:session_manager.config
      ~details:
        (Session.runtime_context_details session_manager ~agent ~key:session_key
           ~compacted_before_turn:compacted)
      ()
  in
  (* Cap tool iterations for restart-resume to prevent indefinite blocking. *)
  let resume_max_iters = 3 in
  let saved_config = agent.Agent.config in
  agent.Agent.config <-
    {
      saved_config with
      agent_defaults =
        {
          saved_config.agent_defaults with
          max_tool_iterations =
            min resume_max_iters saved_config.agent_defaults.max_tool_iterations;
        };
    };
  (* Treat queued-message interrupt as a stop signal during restart-resume so
     that a new inbound message terminates the resume turn immediately rather
     than letting the loop continue with no benefit. *)
  let restart_resume_interrupt_check () =
    match !interrupt with
    | Some s when s = Agent.queued_message_interrupt_token ->
        Some "restart_resume_queued_stop"
    | v -> v
  in
  Lwt.finalize
    (fun () ->
      Agent.turn agent ~user_message:resume_turn_prompt ?db:session_manager.db
        ~session_key ~interrupt_check:restart_resume_interrupt_check
        ?runtime_context ~history_prepared:true ?on_llm_call_debug ())
    (fun () ->
      agent.Agent.config <- saved_config;
      Lwt.return_unit)

let resume_agent_session ?(senders = default_resume_senders) ?run_turn
    ?(after_dispatch = fun ~response:_ -> Lwt.return_unit)
    ~(session_manager : Session.t) ~(config : Runtime_config.t) ~session_key
    ~channel ~channel_id () =
  let notify text =
    notify_resumed_session ~senders ~session_manager ~config ~session_key
      ~channel ~channel_id text
  in
  let open Lwt.Syntax in
  Logs.info (fun m ->
      m "Automatic restart-resume: beginning resume sequence for %s"
        (resumed_dispatch_target ~session_key ~channel ~channel_id));
  let* () =
    notify_resumed_session ~senders ~session_manager ~config ~session_key
      ~channel ~channel_id resume_user_notice
  in
  Session.with_session_lock session_manager ~key:session_key
    (fun agent interrupt ->
      let history_before = List.length agent.Agent.history in
      let persisted_up_to = ref history_before in
      let run_turn =
        match run_turn with
        | Some f -> f
        | None ->
            default_resume_turn ~session_manager ~notify ~session_key
              ~on_history_persisted:(fun len -> persisted_up_to := len)
      in
      let* response =
        Session.with_in_flight session_manager (fun () ->
            run_turn agent interrupt)
      in
      Session.persist_new_messages session_manager ~key:session_key
        ~history_before:!persisted_up_to agent;
      let* dispatch_result =
        dispatch_resumed_message ~senders ~config ~channel ~channel_id
          ~text:response ()
      in
      let* () =
        match dispatch_result with
        | Ok () ->
            Logs.info (fun m ->
                m "Resumed session response dispatched %s response_len=%d"
                  (resumed_dispatch_target ~session_key ~channel ~channel_id)
                  (String.length (String.trim response)));
            let* () = after_dispatch ~response in
            Session.clear_response_deferred session_manager ~key:session_key;
            Session.mark_response_sent session_manager ~key:session_key;
            Lwt.return_unit
        | Error msg ->
            Logs.warn (fun m ->
                m "Failed to deliver resumed session %s via %s:%s: %s"
                  session_key channel channel_id msg);
            (* Mark responded even on dispatch failure to prevent boot-loop: the
               turn completed and retrying on next boot would re-run the same
               agent turn indefinitely. *)
            Session.mark_response_sent session_manager ~key:session_key;
            Lwt.return_unit
      in
      (* Drain any messages that were queued while the resume turn held the
         lock. *)
      interrupt := None;
      Session.drain_queued_messages session_manager ~key:session_key agent
        interrupt ())

let resume_pending_agent_sessions ?(senders = default_resume_senders)
    ?resume_one ~(session_manager : Session.t) ~(config : Runtime_config.t) () =
  let resume_one =
    match resume_one with
    | Some f -> f
    | None ->
        fun ~session_key ~channel ~channel_id ->
          let after_dispatch ~response =
            post_dispatch_resumed_session_response ~senders ~session_manager
              ~config ~session_key ~channel ~channel_id ~response ()
          in
          resume_agent_session ~senders ~after_dispatch ~session_manager ~config
            ~session_key ~channel ~channel_id ()
  in
  let pending =
    Session.load_pending_agent_sessions session_manager ~max_age_seconds:3600
  in
  let open Lwt.Syntax in
  let summary =
    {
      pending_count = List.length pending;
      resumed_count = 0;
      missing_channel_count = 0;
      failed_count = 0;
    }
    |> ref
  in
  if pending <> [] then
    Logs.info (fun m ->
        m "Resuming %d pending agent sessions" (List.length pending));
  let* () =
    Lwt_list.iter_s
      (fun (session_key, channel_opt, channel_id_opt) ->
        match (channel_opt, channel_id_opt) with
        | Some channel, Some channel_id ->
            Lwt.catch
              (fun () ->
                let* () = resume_one ~session_key ~channel ~channel_id in
                summary :=
                  { !summary with resumed_count = !summary.resumed_count + 1 };
                Lwt.return_unit)
              (fun exn ->
                summary :=
                  { !summary with failed_count = !summary.failed_count + 1 };
                Logs.err (fun m ->
                    m "Failed to resume session %s: %s" session_key
                      (Printexc.to_string exn));
                Lwt.return_unit)
        | _ ->
            summary :=
              {
                !summary with
                missing_channel_count = !summary.missing_channel_count + 1;
              };
            Logs.info (fun m ->
                m
                  "Marking session %s as responded: no channel routing info \
                   (expected for cron/CLI sessions)"
                  session_key);
            Session.mark_response_sent session_manager ~key:session_key;
            Lwt.return_unit)
      pending
  in
  Logs.info (fun m ->
      m "Boot: pending-session resume summary %s"
        (boot_resume_summary_message !summary));
  Lwt.return !summary

let resume_sessions_after_channels ?(senders = default_resume_senders)
    ?resume_one ~(session_manager : Session.t) ~(config : Runtime_config.t) () =
  Logs.info (fun m ->
      m
        "Boot: resuming pending routed sessions after channel listeners \
         spawned; outbound resume delivery uses direct Telegram/Discord/Slack \
         send APIs and does not wait for polling or gateway readiness");
  resume_pending_agent_sessions ~senders ?resume_one ~session_manager ~config ()
