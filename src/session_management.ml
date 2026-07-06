(** Public session management helpers layered on Session_core. *)

include Session_core

let get_config mgr = mgr.config
let get_tool_registry mgr = mgr.tool_registry
let get_db mgr = mgr.db
let set_sandbox mgr sandbox = mgr.sandbox <- Some sandbox
let session_count mgr = Hashtbl.length mgr.sessions

let active_session_count mgr =
  Hashtbl.fold
    (fun _key state acc -> if state.active_scopes > 0 then acc + 1 else acc)
    mgr.live_activity 0

let update_config ?(source = "") mgr config =
  let old_model = mgr.config.Runtime_config.agent_defaults.primary_model in
  let new_model = config.Runtime_config.agent_defaults.primary_model in
  mgr.config <- config;
  if old_model <> new_model then
    if source = "" then
      Logs.info (fun m ->
          m "Primary model changed from '%s' to '%s'" old_model new_model)
    else
      Logs.info (fun m ->
          m "Primary model changed from '%s' to '%s' [source: %s]" old_model
            new_model source);
  List.iter
    (fun (key, (agent, _, _)) ->
      agent.Agent.config <- config;
      (* Re-apply session model override, room profile, or channel default *)
      (match Session_room_profile.resolve_model_for_session mgr ~key with
      | Some model ->
          let cfg = agent.Agent.config in
          let ad = { cfg.agent_defaults with primary_model = model } in
          agent.Agent.config <- { cfg with agent_defaults = ad }
      | None -> ());
      (* Re-apply room profile template fields *)
      Session_room_profile.apply_room_profile_template_fields mgr ~key agent;
      (* Recompute layered instructions from access scopes *)
      agent.Agent.instruction_items <-
        Session_room_profile.resolve_instruction_items_for_session mgr ~key;
      Agent.sync_observed_active_workspace_files agent;
      persist_session_workspace_state mgr ~key agent)
    (Hashtbl.to_seq mgr.sessions |> List.of_seq)

let set_session_model mgr ~key ~model =
  (match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) ->
      let cfg = agent.Agent.config in
      let agent_defaults = { cfg.agent_defaults with primary_model = model } in
      agent.Agent.config <- { cfg with agent_defaults }
  | None -> ());
  match mgr.db with
  | Some db -> Memory.set_session_model_override ~db ~session_key:key ~model
  | None -> ()

(* B710: Pre-switch context check. Before applying a model switch, check if
   the new model's context window is smaller than current history usage. If so,
   force-compact history to fit. Returns [Some compaction_info] if compaction
   happened, [None] otherwise. *)
let set_session_model_with_compact mgr ~key ~model =
  let open Lwt.Syntax in
  let apply_model agent =
    let cfg = agent.Agent.config in
    let agent_defaults = { cfg.agent_defaults with primary_model = model } in
    agent.Agent.config <- { cfg with agent_defaults };
    match mgr.db with
    | Some db -> Memory.set_session_model_override ~db ~session_key:key ~model
    | None -> ()
  in
  (* No mutex: callers are either inside the turn loop (already holds mutex)
     or in connector handlers where set_session_model is a fast in-memory op.
     Adding Lwt_mutex.with_lock here would deadlock when called from
     tools_builtin during an active turn. *)
  match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) ->
      let* compaction_info =
        Agent.pre_switch_compact_if_needed agent ~new_model:model ?db:mgr.db ()
      in
      (match compaction_info with
      | Some _ ->
          (* Compaction happened — mark mid-turn so persistence includes the
             compacted history, not just new messages. *)
          agent.Agent.compacted_mid_turn <- true
      | None -> (
          let current_tokens =
            Agent.estimate_history_tokens agent.Agent.history
          in
          match
            Runtime_config.context_window_for_model
              ~configured_limits:agent.Agent.config.model_context_limits model
          with
          | Some new_cw when current_tokens > new_cw ->
              Logs.warn (fun m ->
                  m
                    "B710: Model switch to '%s' may cause context overflow: \
                     current history is %d tokens, exceeding target context \
                     window %d, and pre-switch compaction did not run or could \
                     not compact anything"
                    model current_tokens new_cw)
          | _ -> ()));
      apply_model agent;
      Lwt.return compaction_info
  | None ->
      set_session_model mgr ~key ~model;
      Lwt.return_none

let get_session_model_override mgr ~key =
  match mgr.db with
  | Some db -> Memory.get_session_model_override ~db ~session_key:key
  | None -> None

let get_session_effective_model mgr ~key =
  match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) -> agent.Agent.config.agent_defaults.primary_model
  | None -> (
      match Session_room_profile.resolve_model_for_session mgr ~key with
      | Some model -> model
      | None -> mgr.config.agent_defaults.primary_model)

(** [get_session_agent_defaults mgr ~key] returns the effective agent_defaults
    for the given session, reading from the in-memory agent if the session is
    loaded, or from the resolved config otherwise. Exposed for testing room
    profile template field application. *)
let get_session_agent_defaults mgr ~key : Runtime_config.agent_defaults =
  match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) -> agent.Agent.config.agent_defaults
  | None -> mgr.config.agent_defaults

(** [get_session_system_prompt mgr ~key] returns the effective built
    system_prompt for the given session, triggering a prompt rebuild with
    current room profile overrides. Returns "" if session not loaded. *)
let get_session_system_prompt mgr ~key : string =
  match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) ->
      let prompt =
        Prompt_builder.build ~config:agent.Agent.config
          ~tool_registry:agent.Agent.tool_registry
          ?agent_template:agent.Agent.agent_template
          ?room_profile_system_prompt:agent.Agent.room_profile_system_prompt
          ~instruction_items:agent.Agent.instruction_items ()
      in
      agent.Agent.system_prompt <- prompt;
      prompt
  | None -> ""

let get_session_profiled_room mgr ~key =
  match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) -> Agent.profiled_room_active agent
  | None -> false

let get_session_effective_cwd mgr ~key =
  match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) -> agent.Agent.effective_cwd
  | None -> None

let clear_session_model mgr ~key =
  (* Clear DB override first so Session_room_profile.resolve_model_for_session
     sees the correct fallback chain (room profile > channel default > global). *)
  (match mgr.db with
  | Some db -> Memory.clear_session_model_override ~db ~session_key:key
  | None -> ());
  match Hashtbl.find_opt mgr.sessions key with
  | Some (agent, _, _) ->
      (match Session_room_profile.resolve_model_for_session mgr ~key with
      | Some model ->
          let cfg = agent.Agent.config in
          let agent_defaults =
            { cfg.agent_defaults with primary_model = model }
          in
          agent.Agent.config <- { cfg with agent_defaults }
      | None ->
          let cfg = agent.Agent.config in
          let agent_defaults =
            {
              cfg.agent_defaults with
              primary_model = mgr.config.agent_defaults.primary_model;
            }
          in
          agent.Agent.config <- { cfg with agent_defaults });
      Session_room_profile.apply_room_profile_template_fields mgr ~key agent
  | None -> ()

let reset mgr ~key =
  let open Lwt.Syntax in
  (* Two-phase reset to avoid deadlock.  Phase 1 holds sessions_lock (quick,
     no blocking on per-session mutex) to clean DB and remove the session from
     all hashtables.  Phase 2 releases sessions_lock first, then waits for an
     in-progress turn by acquiring the per-session mutex, and does a second
     DB cleanup to catch any writes the old turn persisted between phases. *)
  let clear_db () =
    match mgr.db with
    | Some db ->
        let pending_cleared = Memory.queue_clear ~db ~session_key:key in
        if pending_cleared > 0 then
          Logs.info (fun m ->
              m "Session reset cleared %d pending inbound queue rows for %s"
                pending_cleared key);
        Memory.archive_session ~db ~session_key:key;
        Memory.clear_session ~db ~session_key:key
    | None -> ()
  in
  (* Phase 1: under sessions_lock — get mutex ref, clean DB *)
  let* mutex_opt =
    Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
      ~label:(Printf.sprintf "sessions_lock/reset[%s]" key) mgr.sessions_lock
      (fun () ->
        let m =
          match Hashtbl.find_opt mgr.sessions key with
          | Some (_, mutex, _) -> Some mutex
          | None -> None
        in
        (* F2: clear DB in Phase 1 but keep the session in the hashtable.
           Phase 2 will remove it after waiting for the in-progress turn.
           This prevents new session creation during the gap between phases
           because get_or_create_locked will find the existing session. *)
        clear_db ();
        Hashtbl.remove mgr.deferred_responses key;
        Hashtbl.remove mgr.queued_messages key;
        Hashtbl.remove mgr.continuation_checks key;
        Hashtbl.remove mgr.observer_last_checked key;
        (* Clean up question callbacks for the session being reset *)
        (match Hashtbl.find_opt mgr.session_callbacks key with
        | Some cb_ids ->
            List.iter
              (fun cb_id -> Hashtbl.remove mgr.question_callbacks cb_id)
              cb_ids;
            Hashtbl.remove mgr.session_callbacks key
        | None -> ());
        cancel_pending_question mgr ~key;
        let target_root = root_postmortem_session_key key in
        Hashtbl.filter_map_inplace
          (fun (rk, _) v -> if rk = target_root then None else Some v)
          mgr.postmortem_circuit_breakers;
        unregister_channel_notifier mgr ~key;
        unregister_rich_notifier mgr ~key;
        (* Do NOT remove from mgr.sessions yet — keep it to prevent new
           session creation during Phase 2 wait. *)
        Lwt.return m)
  in
  (* Phase 2: outside sessions_lock — wait for in-progress turn, re-clear DB *)
  let* () =
    match mutex_opt with
    | Some mutex ->
        let* () =
          Lwt_util.lock_with_timeout
            ~label:(Printf.sprintf "session_mutex/reset[%s]" key)
            mutex
        in
        (* F2: now that we hold the old session's mutex, remove it from
           the hashtable and re-clear DB. Since the session was kept in the
           hashtable during Phase 1, no new session could have been created
           for this key — get_or_create_locked would have found the existing
           one. So the re-clear is safe. *)
        let* () =
          Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
            ~label:(Printf.sprintf "sessions_lock/reset_phase2[%s]" key)
            mgr.sessions_lock (fun () ->
              Hashtbl.remove mgr.sessions key;
              (match mgr.db with
              | Some db -> Memory.clear_session ~db ~session_key:key
              | None -> ());
              Lwt.return_unit)
        in
        Lwt_mutex.unlock mutex;
        Lwt.return_unit
    | None -> Lwt.return_unit
  in
  let active_bg_tasks =
    match mgr.db with
    | Some db -> (
        try
          Background_task.init_schema db;
          Background_task.count_active_for_session ~db ~session_key:key
        with _ -> 0)
    | None -> 0
  in
  Lwt.return active_bg_tasks

(* Step tuple: (name, emoji, started_at option, done_at option)
   None/None = Pending, Some t0/None = Running, Some t0/Some t1 = Done *)
let compact_progress_render ~steps ~overall_start ~finished =
  let buf = Buffer.create 256 in
  if finished then begin
    let dur =
      match
        Status_message.format_duration_opt
          (Unix.gettimeofday () -. overall_start)
      with
      | Some s -> " \xe2\x80\x94 " ^ s
      | None -> ""
    in
    Buffer.add_string buf
      (Printf.sprintf "\xe2\x9c\x85 Session history compacted%s\n" dur)
  end
  else
    Buffer.add_string buf
      "\xf0\x9f\x97\x9c\xef\xb8\x8f Compacting session history\xe2\x80\xa6\n";
  List.iter
    (fun (name, emoji, started_at_opt, done_at_opt) ->
      match (started_at_opt, done_at_opt) with
      | None, _ ->
          (* Pending -- not yet started *)
          Buffer.add_string buf
            (Printf.sprintf "\xe2\x97\x8c %s %s\n" emoji name)
      | Some _, None ->
          (* Running *)
          Buffer.add_string buf
            (Printf.sprintf "\xe2\x8f\xb3 %s %s\n" emoji name)
      | Some started_at, Some done_at ->
          (* Done *)
          let dur_part =
            match
              Status_message.format_duration_opt (done_at -. started_at)
            with
            | Some s -> " \xe2\x80\x94 " ^ s
            | None -> ""
          in
          Buffer.add_string buf
            (Printf.sprintf "\xe2\x9c\x93 %s %s%s\n" emoji name dur_part))
    !steps;
  let s = Buffer.contents buf in
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\n' then String.sub s 0 (n - 1) else s

let compact mgr ~key ?notifier () =
  let open Lwt.Syntax in
  Logs.info (fun m ->
      m "/compact requested for session %s — starting compaction" key);
  (* Mutable progress state, shared between callbacks and finalization. *)
  let steps : (string * string * float option * float option) list ref =
    ref []
  in
  let msg_id : string option ref = ref None in
  let overall_start = ref (Unix.gettimeofday ()) in
  let send_or_edit_opt =
    match notifier with
    | None -> None
    | Some (n : Status_message.notifier) ->
        let f text =
          Lwt.catch
            (fun () ->
              match !msg_id with
              | None ->
                  let* id = n.send text in
                  msg_id := Some id;
                  Lwt.return_unit
              | Some id ->
                  let* new_id_opt = n.edit id text in
                  (match new_id_opt with
                  | Some new_id -> msg_id := Some new_id
                  | None -> ());
                  Lwt.return_unit)
            (fun exn ->
              Logs.warn (fun m ->
                  m "Compact progress update failed: %s"
                    (Printexc.to_string exn));
              Lwt.return_unit)
        in
        Some f
  in
  let debug_notify =
    match find_registered_notifier mgr ~key with
    | Some send -> Some send
    | None -> (
        match notifier with
        | None -> None
        | Some (n : Status_message.notifier) ->
            Some
              (fun text ->
                let* _id = n.send text in
                Lwt.return_unit))
  in
  let on_llm_call_debug =
    Session_heartbeat.debug_callback_for mgr ~key debug_notify
  in
  let render ~finished =
    compact_progress_render ~steps ~overall_start:!overall_start ~finished
  in
  let compact_cbs =
    match send_or_edit_opt with
    | None -> None
    | Some soe ->
        let on_step_start name _emoji =
          (* Transition the matching Pending step to Running *)
          steps :=
            List.map
              (fun (n', e, s, d) ->
                if n' = name && s = None then
                  (n', e, Some (Unix.gettimeofday ()), d)
                else (n', e, s, d))
              !steps;
          soe (render ~finished:false)
        in
        let on_step_done name dur =
          steps :=
            List.map
              (fun (n', e, s, d) ->
                match s with
                | Some t0 when n' = name && d = None ->
                    (n', e, s, Some (t0 +. dur))
                | _ -> (n', e, s, d))
              !steps;
          soe (render ~finished:false)
        in
        Some Agent.{ on_step_start; on_step_done }
  in
  (* Three-phase compaction: plan (lock), execute (no lock), apply (lock).
     This avoids holding the session mutex during LLM calls which can take
     70+ seconds and cause the fatal timeout in lock_with_timeout. *)
  let* result =
    Lwt.catch
      (fun () ->
        (* Phase 1: Plan -- acquire lock, snapshot state, release lock. *)
        let* plan_opt =
          with_session_lock mgr ~key (fun agent _interrupt ->
              Agent.refresh_profiled_room_flag agent ?db:mgr.db ~session_key:key
                ();
              let plan = Agent.plan_force_compact agent in
              (* Set up progress steps while we have the agent *)
              let* () =
                match (plan, send_or_edit_opt) with
                | Some plan, Some soe ->
                    let has_mem_flush =
                      agent.Agent.config.memory.pre_compaction_flush
                      && Option.is_some mgr.db
                      && not plan.Agent.cp_profiled_room
                    in
                    overall_start := Unix.gettimeofday ();
                    steps :=
                      (if has_mem_flush then
                         [ ("Save memories", "\xf0\x9f\xa7\xa0", None, None) ]
                       else [])
                      @ [
                          ( "Summarize (part 1)",
                            "\xe2\x9c\x82\xef\xb8\x8f",
                            None,
                            None );
                          ( "Summarize (part 2)",
                            "\xe2\x9c\x82\xef\xb8\x8f",
                            None,
                            None );
                        ];
                    soe (render ~finished:false)
                | _ -> Lwt.return_unit
              in
              Lwt.return plan)
        in
        match plan_opt with
        | None -> Lwt.return (Ok false)
        | Some plan ->
            (* Phase 2: Execute -- LLM calls with NO lock held. *)
            let* summary =
              Agent.execute_compact_plan plan ?db:mgr.db ?compact_cbs
                ?on_llm_call_debug ()
            in
            (* Phase 3: Apply -- re-acquire lock, reconcile, persist. *)
            with_session_lock mgr ~key (fun agent _interrupt ->
                match Agent.apply_compact_result agent plan ~summary with
                | Some _ ->
                    persist_compacted_history mgr ~key agent;
                    Lwt.return (Ok true)
                | None ->
                    Logs.warn (fun m ->
                        m
                          "Compact apply skipped for session %s — history \
                           changed during execution"
                          key);
                    Lwt.return (Ok false)))
      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
  in
  let* () =
    match (result, send_or_edit_opt) with
    | Ok true, Some soe -> soe (render ~finished:true)
    | Ok false, Some soe ->
        soe
          "Nothing to compact \xe2\x80\x94 session history is already short \
           enough."
    | Error _, Some soe when !msg_id <> None ->
        Lwt.catch
          (fun () -> soe "\xe2\x9d\x8c Compaction failed")
          (fun _ -> Lwt.return_unit)
    | _ -> Lwt.return_unit
  in
  Lwt.return result

(* Produce a full JSON dump of the current epoch for a session.
   Used by /debug_dump_chat to send session state as a file attachment. *)
let dump_json mgr ~key =
  match mgr.db with
  | None ->
      Yojson.Safe.pretty_to_string
        (`Assoc
           [ ("session_key", `String key); ("error", `String "no database") ])
  | Some db -> (
      let config = mgr.config in
      let msg_json (row : Memory.raw_message) =
        let content_field =
          match Yojson.Safe.from_string row.content with
          | json -> json
          | exception _ -> `String row.content
        in
        `Assoc
          [
            ("role", `String row.role);
            ("content", content_field);
            ("created_at", `String row.created_at);
          ]
      in
      match
        Memory.load_epoch_messages ~db ~session_key:key ~epoch:Memory.Current
      with
      | None ->
          Yojson.Safe.pretty_to_string
            (`Assoc
               [
                 ("session_key", `String key);
                 ("epoch", `String "current");
                 ("error", `String "no messages found");
               ])
      | Some rows ->
          let epochs = Memory.list_session_epochs ~db ~session_key:key in
          let archived =
            List.filter (fun (e : Memory.session_epoch) -> not e.current) epochs
          in
          let archived_epoch_count = List.length archived in
          let total_archived_messages =
            List.fold_left
              (fun acc (e : Memory.session_epoch) -> acc + e.message_count)
              0 archived
          in
          let system_prompt =
            Prompt_builder.build ~config ~tool_registry:None ()
          in
          Yojson.Safe.pretty_to_string
            (`Assoc
               [
                 ("session_key", `String key);
                 ("epoch", `String "current");
                 ("system_prompt", `String system_prompt);
                 ("archived_epoch_count", `Int archived_epoch_count);
                 ("total_archived_messages", `Int total_archived_messages);
                 ("total_messages", `Int (List.length rows));
                 ("messages", `List (List.map msg_json rows));
               ]))
