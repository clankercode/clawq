(* B830: idle-session eviction tests. Sessions idle beyond
   memory.session_idle_eviction_s are dropped from the in-memory table (with
   their side-table rows) and must reload identically from the DB on next
   touch. Guards must pin sessions that are active, have pending questions,
   queued messages, or an in-flight turn. *)

let idle_now () = Unix.gettimeofday () +. 3600.

let make_manager ?(config = Runtime_config.default) () =
  let db = Memory.init ~db_path:":memory:" () in
  (Session.create ~config ~db (), db)

let touch mgr key =
  Lwt_main.run
    (Session.with_session_lock mgr ~key (fun _agent _interrupt ->
         Lwt.return_unit))

let queued_msg : Session.queued_message =
  {
    message = "later";
    content_parts = [];
    attachments = [];
    channel_name = None;
    channel_type = None;
    sender_id = None;
    sender_name = None;
    user_group = None;
    channel = None;
    channel_id = None;
    message_id = None;
    inbound_queue_id = None;
    bang = false;
    deferred_followup = false;
    snapshot_work_type = None;
    has_external_users = false;
  }

let test_idle_session_evicted_with_side_tables () =
  let mgr, db = make_manager () in
  let key = "github:owner_repo:pr:1" in
  touch mgr key;
  (* Populate the side tables that eviction must clean up. *)
  Session.register_channel_notifier mgr ~key (fun _ -> Lwt.return_unit);
  Session.register_rich_notifier mgr ~key (fun _ -> failwith "not called");
  Hashtbl.replace mgr.observer_last_checked key 12;
  Hashtbl.replace mgr.live_activity key (Session.create_live_activity_state ());
  Hashtbl.replace mgr.postmortem_circuit_breakers
    (Session.root_postmortem_session_key key, "stuck-pattern")
    ();
  Hashtbl.replace mgr.session_callbacks key [ "cb1" ];
  Hashtbl.replace mgr.question_callbacks "cb1" "answer";
  Hashtbl.replace mgr.deferred_responses key ();
  Memory.store_message ~db ~session_key:key
    (Provider.make_message ~role:"user" ~content:"persisted");
  let evicted =
    Lwt_main.run
      (Session.evict_idle_sessions ~now:(idle_now ()) mgr
         ~idle_threshold_s:1800. ())
  in
  Alcotest.(check int) "one session evicted" 1 evicted;
  Alcotest.(check int) "session table empty" 0 (Session.session_count mgr);
  Alcotest.(check bool)
    "channel notifier removed" true
    (Option.is_none (Session.find_registered_notifier mgr ~key));
  Alcotest.(check bool)
    "rich notifier removed" true
    (Option.is_none (Session.find_rich_notifier mgr ~key));
  Alcotest.(check bool)
    "observer_last_checked removed" true
    (not (Hashtbl.mem mgr.observer_last_checked key));
  Alcotest.(check bool)
    "live_activity removed" true
    (not (Hashtbl.mem mgr.live_activity key));
  Alcotest.(check bool)
    "circuit breaker removed" true
    (not
       (Hashtbl.mem mgr.postmortem_circuit_breakers
          (Session.root_postmortem_session_key key, "stuck-pattern")));
  Alcotest.(check bool)
    "session_callbacks removed" true
    (not (Hashtbl.mem mgr.session_callbacks key));
  Alcotest.(check bool)
    "question_callbacks removed" true
    (not (Hashtbl.mem mgr.question_callbacks "cb1"));
  Alcotest.(check bool)
    "deferred_responses removed" true
    (not (Session.response_deferred mgr ~key));
  (* Eviction must never touch the DB: reload still restores the history. *)
  Alcotest.(check int)
    "db history survives eviction" 1
    (List.length (Memory.load_history ~db ~session_key:key))

let test_recent_session_kept () =
  let mgr, _ = make_manager () in
  let key = "slack:c1:u1" in
  touch mgr key;
  let evicted =
    Lwt_main.run (Session.evict_idle_sessions mgr ~idle_threshold_s:1800. ())
  in
  Alcotest.(check int) "nothing evicted" 0 evicted;
  Alcotest.(check bool)
    "session still present" true
    (Hashtbl.mem mgr.sessions key)

let test_missing_timestamp_session_kept () =
  (* Sessions inserted outside get_or_create_locked (e.g. pair-coding) have no
     recorded last-active time; treat unknown age as fresh. *)
  let mgr, _ = make_manager () in
  let key = "pc:session" in
  Hashtbl.replace mgr.sessions key
    ( Agent.create ~config:Runtime_config.default (),
      Lwt_mutex.create (),
      ref None );
  let evicted =
    Lwt_main.run
      (Session.evict_idle_sessions ~now:(idle_now ()) mgr
         ~idle_threshold_s:1800. ())
  in
  Alcotest.(check int) "nothing evicted" 0 evicted;
  Alcotest.(check bool)
    "session still present" true
    (Hashtbl.mem mgr.sessions key)

let test_disabled_threshold_keeps_idle_sessions () =
  let mgr, _ = make_manager () in
  let key = "telegram:42:user" in
  touch mgr key;
  let evicted =
    Lwt_main.run
      (Session.evict_idle_sessions ~now:(idle_now ()) mgr ~idle_threshold_s:0.
         ())
  in
  Alcotest.(check int) "nothing evicted" 0 evicted;
  Alcotest.(check bool)
    "session still present" true
    (Hashtbl.mem mgr.sessions key)

let test_live_activity_pins_session () =
  let mgr, _ = make_manager () in
  let key = "github:owner_repo:pr:2" in
  touch mgr key;
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Session.start_live_activity mgr ~key in
     let* evicted =
       Session.evict_idle_sessions ~now:(idle_now ()) mgr
         ~idle_threshold_s:1800. ()
     in
     Alcotest.(check int) "busy session kept" 0 evicted;
     Alcotest.(check bool)
       "session still present" true
       (Hashtbl.mem mgr.sessions key);
     let* () = Session.stop_live_activity mgr ~key in
     let* evicted =
       Session.evict_idle_sessions ~now:(idle_now ()) mgr
         ~idle_threshold_s:1800. ()
     in
     Alcotest.(check int) "evicted once idle again" 1 evicted;
     Lwt.return_unit)

let test_pending_question_pins_session () =
  let mgr, _ = make_manager () in
  let key = "web:s1" in
  touch mgr key;
  let _promise, _resolver = Session.register_pending_question mgr ~key in
  let evicted =
    Lwt_main.run
      (Session.evict_idle_sessions ~now:(idle_now ()) mgr
         ~idle_threshold_s:1800. ())
  in
  Alcotest.(check int) "question pending session kept" 0 evicted;
  Alcotest.(check bool)
    "session still present" true
    (Hashtbl.mem mgr.sessions key);
  Session.cancel_pending_question mgr ~key;
  let evicted =
    Lwt_main.run
      (Session.evict_idle_sessions ~now:(idle_now ()) mgr
         ~idle_threshold_s:1800. ())
  in
  Alcotest.(check int) "evicted after cancel" 1 evicted

let test_queued_messages_pin_session () =
  let mgr, _ = make_manager () in
  let key = "slack:c2:u2" in
  touch mgr key;
  Hashtbl.replace mgr.queued_messages key [ queued_msg ];
  let evicted =
    Lwt_main.run
      (Session.evict_idle_sessions ~now:(idle_now ()) mgr
         ~idle_threshold_s:1800. ())
  in
  Alcotest.(check int) "queued message session kept" 0 evicted;
  Alcotest.(check bool)
    "session still present" true
    (Hashtbl.mem mgr.sessions key);
  Hashtbl.remove mgr.queued_messages key;
  let evicted =
    Lwt_main.run
      (Session.evict_idle_sessions ~now:(idle_now ()) mgr
         ~idle_threshold_s:1800. ())
  in
  Alcotest.(check int) "evicted after drain" 1 evicted

let test_locked_mutex_pins_session () =
  let mgr, _ = make_manager () in
  let key = "github:owner_repo:pr:3" in
  touch mgr key;
  Lwt_main.run
    (let open Lwt.Syntax in
     let _, mutex, _ = Hashtbl.find mgr.sessions key in
     let* () = Lwt_mutex.lock mutex in
     let* evicted =
       Session.evict_idle_sessions ~now:(idle_now ()) mgr
         ~idle_threshold_s:1800. ()
     in
     Alcotest.(check int) "in-flight turn session kept" 0 evicted;
     Alcotest.(check bool)
       "session still present" true
       (Hashtbl.mem mgr.sessions key);
     Lwt_mutex.unlock mutex;
     let* evicted =
       Session.evict_idle_sessions ~now:(idle_now ()) mgr
         ~idle_threshold_s:1800. ()
     in
     Alcotest.(check int) "evicted after unlock" 1 evicted;
     Lwt.return_unit)

(* A fake OpenAI-compatible native provider so turns run without network. *)
let with_native_provider f =
  let provider =
    {
      Runtime_config.default_provider_config with
      api_key = "test-key";
      kind = Some "openai";
      default_model = Some "gpt-test";
    }
  in
  let config =
    {
      Runtime_config.default with
      providers = [ ("openai", provider) ];
      prompt = { Runtime_config.default.prompt with dynamic_enabled = false };
      security = { Runtime_config.default.security with tools_enabled = false };
      agent_defaults =
        {
          Runtime_config.default.agent_defaults with
          primary_model = "openai:gpt-test";
          show_thinking = false;
          show_tool_calls = false;
        };
    }
  in
  let requests : string list list ref = ref [] in
  let prev_complete = !Provider.native_complete in
  Fun.protect
    (fun () ->
      Provider.register_native_complete Provider.OpenAICompat
        (fun
          ~config:_ ~provider:_ ~model:_ ~messages ?tools:_ ?session_key:_ () ->
          requests :=
            List.map (fun (m : Provider.message) -> m.content) messages
            :: !requests;
          Lwt.return
            (Provider.Text
               {
                 content = "ok";
                 model = "openai:gpt-test";
                 usage = None;
                 provider_response_items_json = None;
                 thinking = None;
               }));
      f config requests)
    ~finally:(fun () -> Provider.native_complete := prev_complete)

let test_evicted_session_reloads_with_identical_history () =
  with_native_provider (fun config requests ->
      let mgr, db = make_manager ~config () in
      (* Canonical-key coverage; raw GitHub turn/restart/eviction regressions
         live in test_session_key_canonicalization.ml. *)
      let key = "github:owner_repo:pr:9" in
      let response1 =
        Lwt_main.run (Session.turn mgr ~key ~message:"first" ())
      in
      Alcotest.(check string) "first turn replied" "ok" response1;
      let evicted =
        Lwt_main.run
          (Session.evict_idle_sessions ~now:(idle_now ()) mgr
             ~idle_threshold_s:1800. ())
      in
      Alcotest.(check int) "session evicted after first turn" 1 evicted;
      Alcotest.(check bool)
        "session gone" true
        (not (Hashtbl.mem mgr.sessions key));
      let response2 =
        Lwt_main.run (Session.turn mgr ~key ~message:"second" ())
      in
      Alcotest.(check string) "second turn replied" "ok" response2;
      Lwt_main.run
        (Session.with_session_lock mgr ~key (fun agent _interrupt ->
             Alcotest.(check int)
               "reloaded history keeps both turns" 4
               (List.length agent.Agent.history);
             Lwt.return_unit));
      Alcotest.(check int)
        "db has both turns" 4
        (List.length (Memory.load_history ~db ~session_key:key));
      (* The reloaded context must still contain the first turn's exchange. *)
      let second_request = List.hd !requests in
      let joined = String.concat "\n" second_request in
      Alcotest.(check bool)
        "first turn visible after reload" true
        (Test_helpers.string_contains joined "first");
      Alcotest.(check bool)
        "first reply visible after reload" true
        (Test_helpers.string_contains joined "ok"))

let test_session_idle_eviction_s_default () =
  Alcotest.(check int)
    "default threshold is 30 minutes" 1800
    Runtime_config.default.memory.session_idle_eviction_s

let test_session_idle_eviction_s_parsed () =
  let json =
    Yojson.Safe.from_string {|{ "memory": {"session_idle_eviction_s": 900} }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check int)
    "threshold parsed from config" 900
    cfg.Runtime_config.memory.session_idle_eviction_s

let test_session_idle_eviction_s_settable_leaf () =
  Alcotest.(check bool)
    "memory.session_idle_eviction_s is a settable leaf" true
    (Config_set.validate_path
       [ "memory"; "session_idle_eviction_s" ]
       Config_set.config_schema)

let suite =
  [
    Alcotest.test_case "idle session evicted with side tables" `Quick
      test_idle_session_evicted_with_side_tables;
    Alcotest.test_case "recent session kept" `Quick test_recent_session_kept;
    Alcotest.test_case "session without timestamp kept" `Quick
      test_missing_timestamp_session_kept;
    Alcotest.test_case "disabled threshold keeps idle sessions" `Quick
      test_disabled_threshold_keeps_idle_sessions;
    Alcotest.test_case "live activity pins session" `Quick
      test_live_activity_pins_session;
    Alcotest.test_case "pending question pins session" `Quick
      test_pending_question_pins_session;
    Alcotest.test_case "queued messages pin session" `Quick
      test_queued_messages_pin_session;
    Alcotest.test_case "locked mutex pins session" `Quick
      test_locked_mutex_pins_session;
    Alcotest.test_case "evicted session reloads with identical history" `Quick
      test_evicted_session_reloads_with_identical_history;
    Alcotest.test_case "session_idle_eviction_s default" `Quick
      test_session_idle_eviction_s_default;
    Alcotest.test_case "session_idle_eviction_s parsed" `Quick
      test_session_idle_eviction_s_parsed;
    Alcotest.test_case "session_idle_eviction_s settable leaf" `Quick
      test_session_idle_eviction_s_settable_leaf;
  ]
