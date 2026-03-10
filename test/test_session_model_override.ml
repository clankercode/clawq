let make_db () = Memory.init ~db_path:":memory:" ()

(* --- DB persistence layer --- *)

let test_set_and_get_model_override () =
  let db = make_db () in
  Memory.set_session_model_override ~db ~session_key:"telegram:123:456"
    ~model:"openai:gpt-5.4";
  match
    Memory.get_session_model_override ~db ~session_key:"telegram:123:456"
  with
  | None -> Alcotest.fail "expected model override to be set"
  | Some m ->
      Alcotest.(check string) "model override persisted" "openai:gpt-5.4" m

let test_get_model_override_none_when_not_set () =
  let db = make_db () in
  let result =
    Memory.get_session_model_override ~db ~session_key:"telegram:123:456"
  in
  Alcotest.(check (option string)) "no override when not set" None result

let test_model_override_can_be_updated () =
  let db = make_db () in
  Memory.set_session_model_override ~db ~session_key:"s1" ~model:"openai:gpt-4";
  Memory.set_session_model_override ~db ~session_key:"s1"
    ~model:"anthropic:claude-sonnet-4-6";
  match Memory.get_session_model_override ~db ~session_key:"s1" with
  | None -> Alcotest.fail "expected model override after update"
  | Some m ->
      Alcotest.(check string)
        "model override updated" "anthropic:claude-sonnet-4-6" m

let test_model_override_independent_per_session () =
  let db = make_db () in
  Memory.set_session_model_override ~db ~session_key:"s1" ~model:"openai:gpt-4";
  Memory.set_session_model_override ~db ~session_key:"s2"
    ~model:"anthropic:claude-sonnet-4-6";
  (match Memory.get_session_model_override ~db ~session_key:"s1" with
  | None -> Alcotest.fail "s1 override missing"
  | Some m -> Alcotest.(check string) "s1 model" "openai:gpt-4" m);
  match Memory.get_session_model_override ~db ~session_key:"s2" with
  | None -> Alcotest.fail "s2 override missing"
  | Some m -> Alcotest.(check string) "s2 model" "anthropic:claude-sonnet-4-6" m

let test_model_override_survives_upsert_session_state () =
  let db = make_db () in
  Memory.set_session_model_override ~db ~session_key:"sk" ~model:"openai:gpt-4";
  Memory.upsert_session_state ~db ~session_key:"sk" ~turn:"user" ();
  Memory.upsert_session_state ~db ~session_key:"sk" ~turn:"agent" ();
  match Memory.get_session_model_override ~db ~session_key:"sk" with
  | None -> Alcotest.fail "model override should survive upsert_session_state"
  | Some m ->
      Alcotest.(check string) "model override persisted" "openai:gpt-4" m

let test_model_override_cleared_on_session_reset () =
  let db = make_db () in
  Memory.upsert_session_state ~db ~session_key:"sk" ~turn:"user" ();
  Memory.set_session_model_override ~db ~session_key:"sk" ~model:"openai:gpt-4";
  Memory.clear_session ~db ~session_key:"sk";
  let result = Memory.get_session_model_override ~db ~session_key:"sk" in
  Alcotest.(check (option string))
    "model override cleared after session reset" None result

(* --- Session-level integration --- *)

let make_config () : Runtime_config.t =
  {
    Runtime_config.default with
    agent_defaults =
      {
        Runtime_config.default.agent_defaults with
        primary_model = "openai:gpt-5.4";
      };
  }

let test_set_session_model_updates_agent () =
  let db = make_db () in
  let config = make_config () in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  Session.set_session_model mgr ~key ~model:"anthropic:claude-sonnet-4-6";
  let effective = Session.get_session_effective_model mgr ~key in
  Alcotest.(check string)
    "effective model updated" "anthropic:claude-sonnet-4-6" effective

let test_get_session_effective_model_falls_back_to_global () =
  let config = make_config () in
  let mgr = Session.create ~config () in
  let effective =
    Session.get_session_effective_model mgr ~key:"telegram:999:888"
  in
  Alcotest.(check string)
    "falls back to global config model" "openai:gpt-5.4" effective

let test_model_override_restored_on_session_recreation () =
  let db = make_db () in
  let config = make_config () in
  (* Set model override via first session manager *)
  let mgr1 = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  Session.set_session_model mgr1 ~key ~model:"anthropic:claude-sonnet-4-6";
  (* Simulate restart: create fresh session manager with same DB *)
  let mgr2 = Session.create ~config ~db () in
  (* Before any message, check effective model from DB *)
  let effective = Session.get_session_effective_model mgr2 ~key in
  Alcotest.(check string)
    "model restored from DB after restart" "anthropic:claude-sonnet-4-6"
    effective

let test_model_override_persisted_to_db () =
  let db = make_db () in
  let config = make_config () in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  Session.set_session_model mgr ~key ~model:"openai:gpt-4";
  match Memory.get_session_model_override ~db ~session_key:key with
  | None -> Alcotest.fail "model override should be persisted to DB"
  | Some m ->
      Alcotest.(check string) "DB has correct model override" "openai:gpt-4" m

(* Exercises the get_or_create_locked path: model override is written to DB
   before the session exists in memory, then runtime_context_block forces
   get_or_create_locked to run (loading the session into memory and applying
   the override to the agent config).  get_session_effective_model should
   then read the in-memory agent.config, not fall back to the DB lookup. *)
let test_model_override_applied_on_session_load () =
  let db = make_db () in
  let config = make_config () in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  (* Write override to DB without creating the session in memory *)
  Memory.set_session_model_override ~db ~session_key:key
    ~model:"anthropic:claude-sonnet-4-6";
  (* Force get_or_create_locked via runtime_context_block (uses
     with_session_lock internally), which loads the session from DB and
     applies the stored model override to the agent's config. *)
  ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
  (* Session is now in memory; get_session_effective_model reads agent.config *)
  let effective = Session.get_session_effective_model mgr ~key in
  Alcotest.(check string)
    "model override applied when session loaded from DB"
    "anthropic:claude-sonnet-4-6" effective

let suite =
  [
    Alcotest.test_case "set and get model override" `Quick
      test_set_and_get_model_override;
    Alcotest.test_case "get model override none when not set" `Quick
      test_get_model_override_none_when_not_set;
    Alcotest.test_case "model override can be updated" `Quick
      test_model_override_can_be_updated;
    Alcotest.test_case "model override independent per session" `Quick
      test_model_override_independent_per_session;
    Alcotest.test_case "model override survives upsert_session_state" `Quick
      test_model_override_survives_upsert_session_state;
    Alcotest.test_case "model override cleared on session reset" `Quick
      test_model_override_cleared_on_session_reset;
    Alcotest.test_case "set_session_model updates agent config" `Quick
      test_set_session_model_updates_agent;
    Alcotest.test_case "get_session_effective_model falls back to global" `Quick
      test_get_session_effective_model_falls_back_to_global;
    Alcotest.test_case "model override restored on session recreation" `Quick
      test_model_override_restored_on_session_recreation;
    Alcotest.test_case "model override persisted to DB" `Quick
      test_model_override_persisted_to_db;
    Alcotest.test_case "model override applied on session load" `Quick
      test_model_override_applied_on_session_load;
  ]
