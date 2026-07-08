include Agent_turn_setup

(* The one turn loop.

   [turn] (buffered) and [turn_stream] (streaming) used to be near-identical
   ~500-line twins; every fix had to land twice (see the B6xx comments). They
   differ on exactly three things, captured by [turn_io]:

   - how a Provider response is obtained for a [messages] list
     (Provider.complete vs Provider.complete_stream + on_chunk),
   - whether synthesized/terminal text is surfaced live ([emit]; a no-op in the
     buffered mode), and
   - whether the tool round surfaces per-tool events ([streaming]).

   Everything else — cost tracking, the context-exhaustion retry, the B652
   watchdog, B677 circuit breaker, B603 mid-turn compaction, stuck detection,
   interrupt handling — is shared in [run_turn]. *)

type turn_io = {
  get_response :
    Provider.message list ->
    (Provider.completion_response * (unit -> unit)) Lwt.t;
      (** Obtain a response and a [release_budget] thunk for a messages list. *)
  emit : Provider.stream_event -> unit Lwt.t;
      (** Surface a synthesized/terminal chunk live. No-op in buffered mode. *)
  streaming : bool;
      (** Whether the tool round surfaces per-tool ToolStart/ToolResult events.
      *)
}

(* ---- shared cost tracking (was duplicated byte-for-byte in both twins) ---- *)
let track_cost agent ~db ~session_key ~current_request_history_len ~latency_ms
    response =
  let usage, model =
    match response with
    | Provider.Text { usage; model; _ } -> (usage, model)
    | Provider.ToolCalls { usage; model; _ } -> (usage, model)
  in
  match (usage, session_key) with
  | Some (pt, ct, api_cached), Some sid -> (
      if api_cached > pt && pt > 0 then
        Logs.warn (fun m ->
            m
              "[cache] provider reported cached=%d > prompt_tokens=%d; \
               provider is not normalizing usage. Cache %% will be wrong."
              api_cached pt);
      if pt > 0 then
        Logs.info (fun m ->
            m "[cache] cached=%d/%d (%.0f%%) prompt tokens" api_cached pt
              (100.0 *. float_of_int api_cached /. float_of_int pt));
      Cost_tracker.record_turn ~model ~prompt_tokens:pt ~completion_tokens:ct
        ~session_id:sid ~cache_hit:(api_cached > 0)
        ~api_cached_tokens:api_cached ();
      Model_preferences.increment_usage model |> ignore;
      match db with
      | Some db ->
          let pname, _, _ = Provider.select_provider ~config:agent.config () in
          let prev = Request_stats.get_prev_totals ~db ~session_key:sid in
          let added =
            match prev with
            | Some (prev_pt, prev_ct, _) ->
                let history_delta_tokens =
                  estimate_history_delta_tokens ~history:agent.history
                    ~previous_request_history_len:agent.last_request_history_len
                    ~current_request_history_len
                  |> Option.value ~default:prev_ct
                in
                max 0 (pt - (prev_pt + history_delta_tokens))
            | None -> pt
          in
          let cache_hit =
            if api_cached > 0 then true
            else
              match prev with
              | Some (_, _, ts) when ts <> "" -> (
                  try
                    let stmt =
                      Sqlite3.prepare db
                        "SELECT (strftime('%s', 'now') - strftime('%s', ?1)) < \
                         300"
                    in
                    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT ts));
                    Fun.protect
                      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                      (fun () ->
                        match Sqlite3.step stmt with
                        | Sqlite3.Rc.ROW -> (
                            match Sqlite3.column stmt 0 with
                            | Sqlite3.Data.INT 1L -> true
                            | _ -> false)
                        | _ -> false)
                  with _ -> false)
              | _ -> false
          in
          let cost_usd_opt =
            match Cost_tracker.lookup_pricing model with
            | None -> None
            | Some _ ->
                Some
                  (Cost_tracker.calculate_cost_with_cache ~model
                     ~prompt_tokens:pt ~completion_tokens:ct
                     ~added_prompt_tokens:added ~cache_hit
                     ~api_cached_tokens:api_cached ())
          in
          let cached_tokens =
            if api_cached > 0 then Some api_cached else None
          in
          Request_stats.record ~db ~session_key:sid ~provider:pname ~model
            ~prompt_tokens:pt ~completion_tokens:ct ?cost_usd:cost_usd_opt
            ~added_prompt_tokens:added ?cached_tokens ~latency_ms ();
          record_provider_response_event ~db ?session_key ~provider:pname ~model
            ~usage:(pt, ct, api_cached) ?cost_usd:cost_usd_opt ~latency_ms ()
      | None -> ())
  | None, Some _ -> (
      match db with
      | Some db ->
          let pname, _, _ = Provider.select_provider ~config:agent.config () in
          record_provider_response_event ~db ?session_key ~provider:pname ~model
            ~latency_ms ()
      | None -> ())
  | _ -> ()

let fire_history_update agent ~on_history_update len_before =
  match on_history_update with
  | Some cb ->
      let len_after = List.length agent.history in
      if len_after > len_before then begin
        let reversed = List.rev agent.history in
        let new_msgs = List.filteri (fun i _ -> i >= len_before) reversed in
        cb new_msgs
      end
      else Lwt.return_unit
  | None -> Lwt.return_unit

(* ---- response acquisition: the buffered adapter ---- *)
let complete_with_ledger agent ?db ?session_key ~quota_states_opt ~tools
    messages =
  let open Lwt.Syntax in
  let resilient_complete config messages tools =
    let res = config.Runtime_config.resilience in
    let primary () =
      Provider.complete ~config ~messages ?tools ?session_key
        ?quota_states:quota_states_opt ()
    in
    let with_optional_fallback () =
      match res.fallback_provider with
      | Some fb_name ->
          let primary_name, _, _ =
            Provider.select_provider ~config ?quota_states:quota_states_opt ()
          in
          let fallback_name, _, _ =
            Provider.select_provider ~config ~preferred_provider:fb_name ()
          in
          if fallback_name = primary_name then primary ()
          else
            Resilience.with_fallback ~primary ~fallback:(fun () ->
                Provider.complete ~config ~messages ?tools ?session_key
                  ~preferred_provider:fb_name ())
      | None -> primary ()
    in
    let* release_budget =
      reserve_room_budget_before_provider_call ?db ?session_key ~messages ()
    in
    Lwt.catch
      (fun () ->
        let* timed =
          Resilience.with_timeout_retry ~timeout_s:res.timeout_s
            ~max_retries:res.max_retries ~base_delay_s:res.base_delay_s
            with_optional_fallback
        in
        match timed with
        | Ok v -> Lwt.return (v, release_budget)
        | Error e ->
            release_budget ();
            Lwt.fail_with e)
      (fun exn ->
        release_budget ();
        Lwt.fail exn)
  in
  let provider_name, _, model =
    Provider.select_provider ~config:agent.config ?quota_states:quota_states_opt
      ()
  in
  record_provider_request_event ?db ?session_key ~provider:provider_name ~model
    ~messages ();
  Lwt.catch
    (fun () -> resilient_complete agent.config messages tools)
    (fun exn ->
      record_provider_error_event ?db ?session_key ~provider:provider_name
        ~model exn;
      Lwt.fail exn)

(* ---- response acquisition: the streaming adapter ---- *)
let stream_with_ledger agent ?db ?session_key ~quota_states_opt ~tools ~on_chunk
    ~partial_buf messages =
  let open Lwt.Syntax in
  let resilient_stream config messages tools on_chunk =
    let res = config.Runtime_config.resilience in
    Buffer.clear partial_buf;
    let primary () =
      Provider.complete_stream ~config ~messages ?tools ?session_key
        ?quota_states:quota_states_opt ~on_chunk ()
    in
    let with_optional_fallback () =
      match res.fallback_provider with
      | Some fb_name ->
          let primary_name, _, _ =
            Provider.select_provider ~config ?quota_states:quota_states_opt ()
          in
          let fallback_name, _, _ =
            Provider.select_provider ~config ~preferred_provider:fb_name ()
          in
          if fallback_name = primary_name then primary ()
          else
            Resilience.with_fallback ~primary ~fallback:(fun () ->
                Provider.complete_stream ~config ~messages ?tools ?session_key
                  ~preferred_provider:fb_name ~on_chunk ())
      | None -> primary ()
    in
    let* release_budget =
      reserve_room_budget_before_provider_call ?db ?session_key ~messages ()
    in
    Lwt.catch
      (fun () ->
        let* timed =
          Resilience.with_timeout_retry ~timeout_s:res.timeout_s
            ~max_retries:res.max_retries ~base_delay_s:res.base_delay_s
            with_optional_fallback
        in
        match timed with
        | Ok v -> Lwt.return (v, release_budget)
        | Error e ->
            release_budget ();
            Lwt.fail_with e)
      (fun exn ->
        release_budget ();
        Lwt.fail exn)
  in
  let provider_name, _, model =
    Provider.select_provider ~config:agent.config ?quota_states:quota_states_opt
      ()
  in
  record_provider_request_event ?db ?session_key ~provider:provider_name ~model
    ~messages ();
  Lwt.catch
    (fun () -> resilient_stream agent.config messages tools on_chunk)
    (fun exn ->
      record_provider_error_event ?db ?session_key ~provider:provider_name
        ~model exn;
      Lwt.fail exn)

(* ---- the two adapters at the turn_io seam ---- *)
let buffered_io agent ?db ?session_key ~quota_states_opt ~tools () =
  {
    get_response =
      (fun messages ->
        complete_with_ledger agent ?db ?session_key ~quota_states_opt ~tools
          messages);
    emit = (fun _ -> Lwt.return_unit);
    streaming = false;
  }

let streaming_io agent ?db ?session_key ~quota_states_opt ~tools ~on_chunk
    ~interrupt_check () =
  let open Lwt.Syntax in
  (* Buffer accumulates streamed content so a mid-stream interrupt can annotate
     and return the partial output. *)
  let partial_buf = Buffer.create 256 in
  let is_restart_interrupt = function
    | Some reason when reason = restart_interrupt_token -> true
    | _ -> false
  in
  let wrapped_on_chunk chunk =
    match chunk with
    | Provider.Delta text -> (
        Buffer.add_string partial_buf text;
        let* () = on_chunk chunk in
        match interrupt_check with
        | Some check -> (
            match check () with
            | interrupt when is_restart_interrupt interrupt ->
                Lwt.fail Restart_requested
            | interrupt when interrupt = Some queued_message_interrupt_token ->
                Lwt.return_unit
            | Some _ ->
                let note = " --- [NOTE: interrupted by user]" in
                let* () = on_chunk (Provider.Delta note) in
                Lwt.fail (Interrupted (Buffer.contents partial_buf))
            | None -> Lwt.return_unit)
        | None -> Lwt.return_unit)
    | Provider.Done | Provider.ThinkingDelta _ | Provider.ToolCallDelta _
    | Provider.ToolStart _ | Provider.ToolOutputDelta _ | Provider.ToolResult _
      ->
        on_chunk chunk
  in
  {
    get_response =
      (fun messages ->
        stream_with_ledger agent ?db ?session_key ~quota_states_opt ~tools
          ~on_chunk:wrapped_on_chunk ~partial_buf messages);
    emit = on_chunk;
    streaming = true;
  }

(* ---- the shared loop ---- *)
let run_turn agent ~mk_io ~user_message ?db ?session_key ?interrupt_check
    ?inject_messages ?on_inject_messages ?on_tool_round_complete
    ?runtime_context ?(history_prepared = false) ?on_history_update ?on_stuck
    ?on_llm_call_debug () =
  let is_restart_interrupt = function
    | Some reason when reason = restart_interrupt_token -> true
    | _ -> false
  in
  let is_queued_message_interrupt = function
    | Some reason when reason = queued_message_interrupt_token -> true
    | _ -> false
  in
  let open Lwt.Syntax in
  refresh_profiled_room_flag agent ?db ?session_key ();
  let* _compaction_info =
    if history_prepared then Lwt.return_none
    else prepare_turn_history agent ~user_message ?db ?session_key ()
  in
  let audit_enabled = agent.config.security.audit_enabled in
  let max_iters = agent.config.agent_defaults.max_tool_iterations in
  let tools = tools_json agent in
  (* Quota state: fetch from cache once per turn (non-blocking). *)
  let quota_states_opt =
    let qs = Provider_quota.get_all_cached () in
    if qs = [] then None else Some qs
  in
  (* Inject a quota notice into system prompt context when provider is >= 70%
     used.  runtime_context is shadowed here so the notice is visible to the
     LLM without altering the caller-provided context string. *)
  let runtime_context =
    match quota_states_opt with
    | None -> runtime_context
    | Some qs -> (
        let pn, _, _ =
          Provider.select_provider ~config:agent.config
            ?quota_states:quota_states_opt ()
        in
        let notice =
          match List.assoc_opt pn qs with
          | Some pq -> Provider_quota.quota_notice pq
          | None -> None
        in
        match notice with
        | None -> runtime_context
        | Some n ->
            Some
              (match runtime_context with
              | None -> n
              | Some rc -> rc ^ "\n\n" ^ n))
  in
  let io = mk_io ~quota_states_opt ~tools in
  (* B652: agent health watchdog. Stuck_detector + on_stuck already log + spawn
     a postmortem when the model loops, but the main agent loop kept iterating
     regardless. After N consecutive Definite stuck detections in a single turn
     we hard-stop, append a user-facing pause message to history, and return
     early. Override threshold via CLAWQ_WATCHDOG_THRESHOLD (default 2). *)
  let consecutive_stuck = ref 0 in
  let watchdog_threshold =
    match Sys.getenv_opt "CLAWQ_WATCHDOG_THRESHOLD" with
    | Some v -> ( try max 1 (int_of_string v) with _ -> 2)
    | None -> 2
  in
  let rec loop iteration =
    Lwt.catch
      (fun () ->
        let runtime_context =
          match agent.effective_cwd with
          | Some cwd ->
              Prompt_builder.build_runtime_context ~config:agent.config
                ~effective_cwd:cwd ()
          | None -> runtime_context
        in
        let current_request_history_len = List.length agent.history in
        let llm_start = Unix.gettimeofday () in
        let* response, release_budget =
          Lwt.catch
            (fun () ->
              let messages = build_messages ?runtime_context agent in
              io.get_response messages)
            (fun exn ->
              if
                is_context_exhaustion_error (Printexc.to_string exn)
                && force_compress_history agent
              then begin
                Logs.warn (fun m ->
                    m
                      "Context exhaustion detected; force-compressed history, \
                       retrying turn");
                let messages = build_messages ?runtime_context agent in
                io.get_response messages
              end
              else Lwt.fail exn)
        in
        let latency_ms =
          int_of_float ((Unix.gettimeofday () -. llm_start) *. 1000.0)
        in
        (try
           track_cost agent ~db ~session_key ~current_request_history_len
             ~latency_ms response
         with exn ->
           release_budget ();
           raise exn);
        release_budget ();
        agent.last_request_history_len <- Some current_request_history_len;
        (* Invoke debug callback if provided *)
        (match on_llm_call_debug with
        | Some cb ->
            let llm_duration = Unix.gettimeofday () -. llm_start in
            let model, usage, tool_call_count =
              match response with
              | Provider.Text { model; usage; _ } -> (model, usage, 0)
              | Provider.ToolCalls { model; usage; calls; _ } ->
                  (model, usage, List.length calls)
            in
            let pname, _, _ =
              Provider.select_provider ~config:agent.config
                ?quota_states:quota_states_opt ()
            in
            let call =
              {
                Session_debug.provider = pname;
                model;
                duration_s = llm_duration;
                usage;
                tool_call_count;
              }
            in
            Lwt.async (fun () ->
                Lwt.catch
                  (fun () -> cb call)
                  (fun exn ->
                    Logs.warn (fun m ->
                        m "Debug callback failed: %s" (Printexc.to_string exn));
                    Lwt.return_unit))
        | None -> ());
        match response with
        | Provider.Text { content; provider_response_items_json; thinking; _ }
          ->
            let thinking =
              if agent.config.agent_defaults.drop_thinking then None
              else thinking
            in
            agent.history <-
              Provider.make_message_full ~provider_response_items_json
                ~role:"assistant" ~content ~thinking ()
              :: agent.history;
            trim_history agent;
            Lwt.return content
        | Provider.ToolCalls { calls; _ } when tools = None ->
            let content =
              "I attempted to use tools ("
              ^ String.concat ", "
                  (List.map
                     (fun (tc : Provider.tool_call) -> tc.function_name)
                     calls)
              ^ ") but tools are disabled. Set security.tools_enabled to true \
                 in " ^ Dot_dir.config_path () ^ " to enable them."
            in
            agent.history <-
              Provider.make_message ~role:"assistant" ~content :: agent.history;
            trim_history agent;
            let* () = io.emit (Provider.Delta content) in
            Lwt.return content
        | Provider.ToolCalls { calls; _ } when iteration >= max_iters ->
            let content =
              "I've reached the maximum number of tool iterations. Here's what \
               I was trying to do: "
              ^ String.concat ", "
                  (List.map
                     (fun (tc : Provider.tool_call) -> tc.function_name)
                     calls)
            in
            agent.history <-
              Provider.make_message ~role:"assistant" ~content :: agent.history;
            trim_history agent;
            let* () = io.emit (Provider.Delta content) in
            Lwt.return content
        | Provider.ToolCalls
            { calls; provider_response_items_json; thinking; _ } -> (
            let len_before_tool_loop = List.length agent.history in
            let thinking =
              if agent.config.agent_defaults.drop_thinking then None
              else thinking
            in
            let assistant_msg =
              {
                Provider.role = "assistant";
                content = "";
                content_parts = [];
                tool_calls = calls;
                tool_call_id = None;
                name = None;
                provider_response_items_json;
                thinking;
                is_error = false;
              }
            in
            agent.history <- assistant_msg :: agent.history;
            let* () =
              execute_tools agent ~db ~audit_enabled ~session_key
                ?raw_tool_calls_json:provider_response_items_json
                ?interrupt_check ?on_tool_round_complete ?on_llm_call_debug
                ~emit:io.emit ~streaming:io.streaming calls
            in
            (match inject_messages with
            | Some get_msgs ->
                let msgs = get_msgs () in
                List.iter
                  (fun msg ->
                    agent.history <-
                      Provider.make_message ~role:"user" ~content:msg
                      :: agent.history)
                  msgs
            | None -> ());
            let* () =
              match on_inject_messages with
              | Some cb -> cb ()
              | None -> Lwt.return_unit
            in
            let* () =
              fire_history_update agent ~on_history_update len_before_tool_loop
            in
            (* B603: proactive mid-turn compaction. Within a single turn the
               model may emit many tool batches (file reads, shell outputs) that
               grow history without bound. compact_history_if_needed checks
               token/message thresholds and compacts only when needed — cheap
               when not at the threshold. *)
            let* mid_turn_compaction = compact_history_if_needed agent ?db () in
            (match mid_turn_compaction with
            | Some _ ->
                agent.compacted_mid_turn <- true;
                Logs.info (fun m ->
                    m "Mid-turn compaction triggered at iteration %d" iteration)
            | None -> ());
            (* B677: hard turn-abort on repeated identical parameter-validation
               failures. validate_required_with_escalation sets this when the
               model has emitted the same invalid call shape >= threshold times.
               Append a user-facing assistant message and end the turn before
               any further LLM cost accrues. *)
            match agent.hard_abort_reason with
            | Some reason ->
                Logs.warn (fun m -> m "B677 circuit breaker: %s" reason);
                agent.history <-
                  Provider.make_message ~role:"assistant" ~content:reason
                  :: agent.history;
                trim_history agent;
                agent.hard_abort_reason <- None;
                let* () = io.emit (Provider.Delta reason) in
                Lwt.return reason
            | None -> (
                (* Check for stuck patterns after each tool call batch *)
                let stuck_signals =
                  let result =
                    Stuck_detector.check ~history:agent.history ~iteration
                      ~max_iters
                  in
                  match result with
                  | Stuck_detector.Definite signals -> Some signals
                  | _ -> None
                in
                let* () =
                  match (stuck_signals, on_stuck) with
                  | Some signals, Some cb -> cb signals
                  | _ -> Lwt.return_unit
                in
                (* B652: watchdog — count consecutive Definite stuck detections
                   and hard-stop after `watchdog_threshold` so a wedged session
                   does not keep burning cost. The observer/postmortem already
                   fired by here. Reset the counter on any non-stuck
                   iteration. *)
                (match stuck_signals with
                | Some _ -> incr consecutive_stuck
                | None -> consecutive_stuck := 0);
                if !consecutive_stuck >= watchdog_threshold then begin
                  let signal_desc =
                    match stuck_signals with
                    | Some s -> Stuck_detector.signals_to_string s
                    | None -> "(unknown)"
                  in
                  let pause_msg =
                    Printf.sprintf
                      "[Watchdog] Pausing this session after %d consecutive \
                       stuck detections (threshold=%d). Last signal: %s\n\n\
                       The model appears to be looping on the same failure \
                       mode. I'm stopping the turn so cost doesn't keep \
                       accruing. Reply with new context or `/reset` to start \
                       over."
                      !consecutive_stuck watchdog_threshold signal_desc
                  in
                  Logs.warn (fun m ->
                      m
                        "B652 watchdog: pausing after %d consecutive Definite \
                         stuck detections (threshold=%d)"
                        !consecutive_stuck watchdog_threshold);
                  agent.history <-
                    Provider.make_message ~role:"assistant" ~content:pause_msg
                    :: agent.history;
                  trim_history agent;
                  let* () = io.emit (Provider.Delta pause_msg) in
                  Lwt.return pause_msg
                end
                else
                  match interrupt_check with
                  | Some check -> (
                      match check () with
                      | interrupt when is_restart_interrupt interrupt ->
                          Lwt.fail Restart_requested
                      | interrupt when is_queued_message_interrupt interrupt ->
                          (* Not a real interrupt: continue looping. Queued
                             messages are picked up via inject_messages between
                             tool batches. Restart-resume turns remap this token
                             to a real stop signal in daemon_util.ml. *)
                          loop (iteration + 1)
                      | interrupt when is_stop_interrupt interrupt ->
                          let stopped = record_stopped_by_admin agent in
                          let* () = io.emit (Provider.Delta stopped) in
                          Lwt.return stopped
                      | Some _ ->
                          let partial =
                            "[Agent was interrupted mid-task] --- [NOTE: \
                             interrupted by user]"
                          in
                          agent.history <-
                            Provider.make_message ~role:"assistant"
                              ~content:partial
                            :: agent.history;
                          trim_history agent;
                          let* () = io.emit (Provider.Delta partial) in
                          Lwt.return partial
                      | None -> loop (iteration + 1))
                  | None -> loop (iteration + 1))))
      (fun exn ->
        match exn with
        | Restart_requested -> Lwt.fail Restart_requested
        | Interrupted partial ->
            let annotated = partial ^ " --- [NOTE: interrupted by user]" in
            agent.history <-
              Provider.make_message ~role:"assistant" ~content:annotated
              :: agent.history;
            trim_history agent;
            Lwt.return annotated
        | exn -> Lwt.fail exn)
  in
  loop 0
