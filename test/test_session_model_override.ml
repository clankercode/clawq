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

(* --- clear_session_model_override --- *)

let test_clear_session_model_override () =
  let db = make_db () in
  Memory.set_session_model_override ~db ~session_key:"s1" ~model:"openai:gpt-4";
  Memory.clear_session_model_override ~db ~session_key:"s1";
  let result = Memory.get_session_model_override ~db ~session_key:"s1" in
  Alcotest.(check (option string))
    "override cleared after clear call" None result

let test_clear_session_model_override_noop_when_no_override () =
  let db = make_db () in
  Memory.clear_session_model_override ~db ~session_key:"s1";
  let result = Memory.get_session_model_override ~db ~session_key:"s1" in
  Alcotest.(check (option string))
    "still none after clear on absent" None result

let test_clear_session_model_restores_global () =
  let db = make_db () in
  let config = make_config () in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  Session.set_session_model mgr ~key ~model:"anthropic:claude-sonnet-4-6";
  Session.clear_session_model mgr ~key;
  let effective = Session.get_session_effective_model mgr ~key in
  Alcotest.(check string)
    "effective model returns to global after clear" "openai:gpt-5.4" effective

let test_clear_session_model_clears_db () =
  let db = make_db () in
  let config = make_config () in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  Session.set_session_model mgr ~key ~model:"openai:gpt-4";
  Session.clear_session_model mgr ~key;
  let result = Memory.get_session_model_override ~db ~session_key:key in
  Alcotest.(check (option string)) "DB override cleared" None result

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

let make_config_with_room_profile ?(global_model = "openai:gpt-5.4")
    ?(channel_default = (None : string option))
    ?(room_profiles : Runtime_config.room_profile list = [])
    ?(room_profile_bindings : Runtime_config.room_profile_binding list = []) ()
    : Runtime_config.t =
  let channels = Runtime_config.default.channels in
  let channels =
    match channel_default with
    | Some m ->
        let tg : Runtime_config.telegram_config =
          { accounts = []; text_coalesce_ms = 100; default_model = Some m }
        in
        { channels with telegram = Some tg }
    | None -> channels
  in
  {
    Runtime_config.default with
    agent_defaults =
      {
        Runtime_config.default.agent_defaults with
        primary_model = global_model;
      };
    channels;
    room_profiles;
    room_profile_bindings;
  }

let test_room_profile_model_overrides_channel_default () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  let config =
    make_config_with_room_profile ~channel_default:(Some "openai:gpt-4")
      ~room_profiles:profiles ~room_profile_bindings:bindings ()
  in
  let result =
    Runtime_config.resolve_room_profile_model config
      ~session_key:"telegram:123:456"
  in
  Alcotest.(check (option string))
    "room profile model overrides channel default"
    (Some "anthropic:claude-sonnet-4-6") result

let test_room_profile_model_overrides_global () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  let config =
    make_config_with_room_profile ~global_model:"openai:gpt-5.4"
      ~room_profiles:profiles ~room_profile_bindings:bindings ()
  in
  let result =
    Runtime_config.resolve_room_profile_model config
      ~session_key:"telegram:123:456"
  in
  Alcotest.(check (option string))
    "room profile model overrides global" (Some "anthropic:claude-sonnet-4-6")
    result

let test_session_override_takes_precedence_over_room_profile () =
  let db = make_db () in
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  let config =
    make_config_with_room_profile ~room_profiles:profiles
      ~room_profile_bindings:bindings ()
  in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  Session.set_session_model mgr ~key ~model:"openai:gpt-4";
  let effective = Session.get_session_effective_model mgr ~key in
  Alcotest.(check string)
    "session override beats room profile" "openai:gpt-4" effective

let test_inactive_room_profile_binding_skipped () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = false } ]
  in
  let config =
    make_config_with_room_profile ~room_profiles:profiles
      ~room_profile_bindings:bindings ()
  in
  let result =
    Runtime_config.resolve_room_profile_model config
      ~session_key:"telegram:123:456"
  in
  Alcotest.(check (option string)) "inactive binding skipped" None result

let test_missing_room_profile_returns_none () =
  let bindings =
    [
      {
        Runtime_config.profile_id = "nonexistent";
        room = "123:456";
        active = true;
      };
    ]
  in
  let config =
    make_config_with_room_profile ~room_profile_bindings:bindings ()
  in
  let result =
    Runtime_config.resolve_room_profile_model config
      ~session_key:"telegram:123:456"
  in
  Alcotest.(check (option string)) "missing profile returns None" None result

let test_room_profile_full_precedence_chain () =
  (* session override > room profile > channel default > global *)
  let db = make_db () in
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  (* 1. Room profile > channel default > global *)
  let config =
    make_config_with_room_profile ~global_model:"openai:gpt-5.4"
      ~channel_default:(Some "openai:gpt-4") ~room_profiles:profiles
      ~room_profile_bindings:bindings ()
  in
  (* Enable Anthropic OAuth so room profile model passes the security gate *)
  let config =
    {
      config with
      security = { config.security with allow_anthropic_oauth_inference = true };
    }
  in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  let effective_no_override = Session.get_session_effective_model mgr ~key in
  Alcotest.(check string)
    "room profile beats channel default and global"
    "anthropic:claude-sonnet-4-6" effective_no_override;
  (* 2. Session override > room profile *)
  Session.set_session_model mgr ~key ~model:"openai:gpt-4";
  let effective_with_override = Session.get_session_effective_model mgr ~key in
  Alcotest.(check string)
    "session override beats room profile" "openai:gpt-4" effective_with_override;
  (* 3. After clearing override, room profile resumes *)
  Session.clear_session_model mgr ~key;
  let effective_after_clear = Session.get_session_effective_model mgr ~key in
  Alcotest.(check string)
    "room profile resumes after clear" "anthropic:claude-sonnet-4-6"
    effective_after_clear

let test_clear_session_model_restores_room_profile_on_active_session () =
  (* Regression test: clear_session_model must follow the precedence chain
     (room profile > channel default > global) when the session is already
     loaded into memory, not reset directly to the global default. *)
  let db = make_db () in
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  let config =
    make_config_with_room_profile ~global_model:"openai:gpt-5.4"
      ~room_profiles:profiles ~room_profile_bindings:bindings ()
  in
  (* Enable Anthropic OAuth so room profile model passes the security gate *)
  let config =
    {
      config with
      security = { config.security with allow_anthropic_oauth_inference = true };
    }
  in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  (* Force session creation so it is loaded into memory *)
  ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
  (* Verify room profile model is in effect *)
  let before = Session.get_session_effective_model mgr ~key in
  Alcotest.(check string)
    "room profile active before override" "anthropic:claude-sonnet-4-6" before;
  (* Set session override, then clear it *)
  Session.set_session_model mgr ~key ~model:"openai:gpt-4";
  Session.clear_session_model mgr ~key;
  let after = Session.get_session_effective_model mgr ~key in
  Alcotest.(check string)
    "room profile resumes after clear on active session"
    "anthropic:claude-sonnet-4-6" after

let test_room_profile_model_applied_on_session_load () =
  let db = make_db () in
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  let config =
    make_config_with_room_profile ~room_profiles:profiles
      ~room_profile_bindings:bindings ()
  in
  (* Enable Anthropic OAuth so room profile model passes the security gate *)
  let config =
    {
      config with
      security = { config.security with allow_anthropic_oauth_inference = true };
    }
  in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  (* Force session creation via runtime_context_block *)
  ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
  let effective = Session.get_session_effective_model mgr ~key in
  Alcotest.(check string)
    "room profile model applied on session load" "anthropic:claude-sonnet-4-6"
    effective

let test_room_profile_binding_matches_full_session_key () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [
      {
        Runtime_config.profile_id = "vip";
        room = "telegram:123:456";
        active = true;
      };
    ]
  in
  let config =
    make_config_with_room_profile ~room_profiles:profiles
      ~room_profile_bindings:bindings ()
  in
  let result =
    Runtime_config.resolve_room_profile_model config
      ~session_key:"telegram:123:456"
  in
  Alcotest.(check (option string))
    "full session key match works" (Some "anthropic:claude-sonnet-4-6") result

let test_no_room_profile_returns_none () =
  let config = make_config () in
  let result =
    Runtime_config.resolve_room_profile_model config
      ~session_key:"telegram:123:456"
  in
  Alcotest.(check (option string)) "no room profiles returns None" None result

(* B606: Even when a room profile sets an Anthropic model, the OAuth security
   gate must still block the 'claude' runner when the opt-in flag is off. *)
let test_oauth_gate_denies_claude_even_with_room_profile () =
  match
    Background_task.resolve_runner ~check_available:false ~allow_claude:false
      ~preferred:Background_task.Claude ()
  with
  | Ok _ -> Alcotest.fail "expected claude runner to be denied"
  | Error msg ->
      Alcotest.(check bool)
        "denial mentions opt-in flag" true
        (String_util.contains msg "allow_anthropic_oauth_inference")

(* --- Agent template model precedence --- *)

let make_template_config ?(global_model = "openai:gpt-5.4")
    ?(allow_anthropic_oauth = false)
    ?(room_profiles : Runtime_config.room_profile list = [])
    ?(room_profile_bindings : Runtime_config.room_profile_binding list = []) ()
    : Runtime_config.t =
  {
    Runtime_config.default with
    agent_defaults =
      {
        Runtime_config.default.agent_defaults with
        primary_model = global_model;
      };
    security =
      {
        Runtime_config.default.security with
        allow_anthropic_oauth_inference = allow_anthropic_oauth;
      };
    room_profiles;
    room_profile_bindings;
  }

let with_test_template ?(system_prompt = "") name model f =
  let tmpl : Agent_template.t =
    {
      name;
      description = "test template";
      role = Agent_template.Coder;
      goal = "";
      backstory = "";
      system_prompt;
      model = Some model;
      max_tool_iterations = None;
      allowed_tools = [];
      disallowed_tools = [];
      tool_search_enabled = None;
      reasoning_effort = None;
      cwd = None;
      source = Agent_template.Builtin;
      metadata = [];
    }
  in
  let prev = !Agent_template.builtins_ref in
  Agent_template.builtins_ref := [ tmpl ];
  Fun.protect ~finally:(fun () -> Agent_template.builtins_ref := prev) f

let test_channel_default_used_when_no_room_profile () =
  with_test_template "test-agent" "openai:gpt-4" (fun () ->
      let config = make_template_config ~allow_anthropic_oauth:true () in
      let config =
        {
          config with
          agent_bindings =
            [
              {
                Agent_router.pattern = "default";
                agent_name = "test-agent";
                priority = 0;
              };
            ];
          channels =
            (let tg =
               match Runtime_config.default.channels.telegram with
               | Some tg ->
                   {
                     tg with
                     Runtime_config.default_model = Some "openai:gpt-3.5-turbo";
                   }
               | None ->
                   {
                     Runtime_config.accounts = [];
                     text_coalesce_ms = 100;
                     default_model = Some "openai:gpt-3.5-turbo";
                   }
             in
             { Runtime_config.default.channels with telegram = Some tg });
        }
      in
      let mgr = Session.create ~config () in
      let effective =
        Session.get_session_effective_model mgr ~key:"telegram:999:888"
      in
      Alcotest.(check string)
        "channel default used when no room profile" "openai:gpt-3.5-turbo"
        effective)

let test_room_profile_beats_template () =
  with_test_template "test-agent" "openai:gpt-4" (fun () ->
      let profiles =
        [
          {
            Runtime_config.id = "vip";
            display_name = None;
            model = "openai:gpt-3.5-turbo";
            system_prompt = "";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ]
      in
      let room_bindings =
        [
          { Runtime_config.profile_id = "vip"; room = "123:456"; active = true };
        ]
      in
      let config =
        make_template_config ~allow_anthropic_oauth:true ~room_profiles:profiles
          ~room_profile_bindings:room_bindings ()
      in
      let config =
        {
          config with
          agent_bindings =
            [
              {
                Agent_router.pattern = "default";
                agent_name = "test-agent";
                priority = 0;
              };
            ];
        }
      in
      let mgr = Session.create ~config () in
      let effective =
        Session.get_session_effective_model mgr ~key:"telegram:123:456"
      in
      Alcotest.(check string)
        "room profile beats template" "openai:gpt-3.5-turbo" effective)

let test_security_gate_denies_anthropic_from_profile () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let room_bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  (* allow_anthropic_oauth = false (default) *)
  let config =
    make_template_config ~allow_anthropic_oauth:false ~room_profiles:profiles
      ~room_profile_bindings:room_bindings ()
  in
  let mgr = Session.create ~config () in
  let effective =
    Session.get_session_effective_model mgr ~key:"telegram:123:456"
  in
  (* Gate denies the Anthropic model; falls through to global *)
  Alcotest.(check string)
    "security gate denies anthropic model, falls to global" "openai:gpt-5.4"
    effective

let test_security_gate_allows_anthropic_when_enabled () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let room_bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  let config =
    make_template_config ~allow_anthropic_oauth:true ~room_profiles:profiles
      ~room_profile_bindings:room_bindings ()
  in
  let mgr = Session.create ~config () in
  let effective =
    Session.get_session_effective_model mgr ~key:"telegram:123:456"
  in
  Alcotest.(check string)
    "oauth enabled allows anthropic model" "anthropic:claude-sonnet-4-6"
    effective

let test_security_gate_allows_non_anthropic () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "openai:gpt-4";
        system_prompt = "";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let room_bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  (* OAuth off, but model is not Anthropic — should pass *)
  let config =
    make_template_config ~allow_anthropic_oauth:false ~room_profiles:profiles
      ~room_profile_bindings:room_bindings ()
  in
  let mgr = Session.create ~config () in
  let effective =
    Session.get_session_effective_model mgr ~key:"telegram:123:456"
  in
  Alcotest.(check string)
    "non-anthropic model unaffected by oauth gate" "openai:gpt-4" effective

let test_security_gate_denial_message_format () =
  let result =
    Agent_template.check_model_security_gates
      ~config:(make_template_config ~allow_anthropic_oauth:false ())
      ~model:"anthropic:claude-sonnet-4-6"
  in
  match result with
  | Ok () -> Alcotest.fail "expected security gate to deny anthropic model"
  | Error msg ->
      Alcotest.(check bool)
        "denial mentions model name" true
        (String_util.contains msg "anthropic:claude-sonnet-4-6");
      Alcotest.(check bool)
        "denial mentions opt-in flag" true
        (String_util.contains msg "allow_anthropic_oauth_inference")

let test_session_override_bypasses_security_gate () =
  (* Session override sets an Anthropic model. Even with OAuth off, the
     explicit user choice bypasses the security gate. *)
  let db = make_db () in
  let config = make_template_config ~allow_anthropic_oauth:false () in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  Session.set_session_model mgr ~key ~model:"anthropic:claude-sonnet-4-6";
  let effective = Session.get_session_effective_model mgr ~key in
  Alcotest.(check string)
    "session override bypasses security gate" "anthropic:claude-sonnet-4-6"
    effective

(* --- Template field precedence tests --- *)

let test_room_profile_system_prompt_applied () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "room profile prompt";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  let db = make_db () in
  let config =
    make_config_with_room_profile ~room_profiles:profiles
      ~room_profile_bindings:bindings ()
  in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  (* Force session creation so room profile template fields are applied *)
  ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
  let ad = Session.get_session_agent_defaults mgr ~key in
  Alcotest.(check string)
    "room profile system_prompt applied" "room profile prompt" ad.system_prompt

let test_room_profile_max_tool_iterations_applied () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "";
        max_tool_iterations = 25;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  let db = make_db () in
  let config =
    make_config_with_room_profile ~room_profiles:profiles
      ~room_profile_bindings:bindings ()
  in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
  let ad = Session.get_session_agent_defaults mgr ~key in
  Alcotest.(check int)
    "room profile max_tool_iterations applied" 25 ad.max_tool_iterations

let test_room_profile_template_overrides_global () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "room profile prompt";
        max_tool_iterations = 30;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  let db = make_db () in
  let config =
    make_config_with_room_profile ~room_profiles:profiles
      ~room_profile_bindings:bindings ()
  in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
  let ad = Session.get_session_agent_defaults mgr ~key in
  Alcotest.(check string)
    "system_prompt overrides global" "room profile prompt" ad.system_prompt;
  Alcotest.(check int)
    "max_tool_iterations overrides global" 30 ad.max_tool_iterations

let test_session_model_override_does_not_affect_template () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "room profile prompt";
        max_tool_iterations = 20;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = true } ]
  in
  let db = make_db () in
  let config =
    make_config_with_room_profile ~room_profiles:profiles
      ~room_profile_bindings:bindings ()
  in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
  Session.set_session_model mgr ~key ~model:"openai:gpt-4";
  let ad = Session.get_session_agent_defaults mgr ~key in
  Alcotest.(check string)
    "system_prompt preserved" "room profile prompt" ad.system_prompt;
  Alcotest.(check int) "max_tool_iterations preserved" 20 ad.max_tool_iterations

let test_inactive_room_profile_skips_template () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "anthropic:claude-sonnet-4-6";
        system_prompt = "inactive prompt";
        max_tool_iterations = 99;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "123:456"; active = false } ]
  in
  let db = make_db () in
  let config =
    make_config_with_room_profile ~room_profiles:profiles
      ~room_profile_bindings:bindings ()
  in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:123:456" in
  ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
  let ad = Session.get_session_agent_defaults mgr ~key in
  Alcotest.(check string) "system_prompt not applied" "" ad.system_prompt;
  Alcotest.(check int)
    "max_tool_iterations unchanged"
    Runtime_config.default.agent_defaults.max_tool_iterations
    ad.max_tool_iterations

(* --- Test: room profile system_prompt reaches actual built prompt --- *)

let test_room_profile_prompt_out_ranks_template_in_built_prompt () =
  (* When a room profile has a system_prompt and an agent template also has a
     system_prompt, the room profile's prompt must appear in the actual built
     system_prompt (agent.system_prompt), not only in agent_defaults. *)
  with_test_template ~system_prompt:"template system prompt" "test-agent"
    "openai:gpt-4" (fun () ->
      let profiles =
        [
          {
            Runtime_config.id = "vip";
            display_name = None;
            model = "openai:gpt-3.5-turbo";
            system_prompt = "room profile system prompt";
            max_tool_iterations = 10;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ]
      in
      let room_bindings =
        [
          { Runtime_config.profile_id = "vip"; room = "123:456"; active = true };
        ]
      in
      let config =
        make_template_config ~allow_anthropic_oauth:true ~room_profiles:profiles
          ~room_profile_bindings:room_bindings ()
      in
      let config =
        {
          config with
          agent_bindings =
            [
              {
                Agent_router.pattern = "default";
                agent_name = "test-agent";
                priority = 0;
              };
            ];
        }
      in
      let mgr = Session.create ~config () in
      let key = "telegram:123:456" in
      (* Force session creation so room profile template fields are applied *)
      ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
      (* The agent_defaults should have the room profile prompt *)
      let ad = Session.get_session_agent_defaults mgr ~key in
      Alcotest.(check string)
        "agent_defaults has room profile prompt" "room profile system prompt"
        ad.system_prompt;
      (* The actual built prompt must contain the room profile prompt, NOT
         the agent template's system_prompt. *)
      let built = Session.get_session_system_prompt mgr ~key in
      Alcotest.(check bool)
        "built prompt contains room profile prompt" true
        (String_util.contains built "room profile system prompt");
      Alcotest.(check bool)
        "built prompt does NOT contain template prompt" false
        (String_util.contains built "template system prompt"))

let test_child_thread_session_inherits_profile_model_template_and_privacy () =
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "openai:gpt-4";
        system_prompt = "child room profile prompt";
        max_tool_iterations = 17;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "C01"; active = true } ]
  in
  let config =
    make_template_config ~allow_anthropic_oauth:true ~room_profiles:profiles
      ~room_profile_bindings:bindings ()
  in
  let mgr = Session.create ~config () in
  let key =
    Room_session.child_thread_key ~profile_id:"vip" ~connector:"slack"
      ~room_id:"C01" ~thread_id:"1719000000.000100" ()
  in
  ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
  Alcotest.(check string)
    "child inherits profile model" "openai:gpt-4"
    (Session.get_session_effective_model mgr ~key);
  let defaults = Session.get_session_agent_defaults mgr ~key in
  Alcotest.(check string)
    "child inherits profile template" "child room profile prompt"
    defaults.system_prompt;
  Alcotest.(check int)
    "child inherits max iterations" 17 defaults.max_tool_iterations;
  Alcotest.(check bool)
    "child privacy guard active" true
    (Session.get_session_profiled_room mgr ~key)

let test_child_thread_session_inherits_parent_room_cwd () =
  let db = make_db () in
  let cwd = Filename.concat (Filename.get_temp_dir_name ()) "clawq-child-cwd" in
  if not (Sys.file_exists cwd) then Unix.mkdir cwd 0o755;
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "openai:gpt-4";
        system_prompt = "";
        max_tool_iterations = 10;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "C01"; active = true } ]
  in
  let config =
    make_template_config ~room_profiles:profiles ~room_profile_bindings:bindings
      ()
  in
  let config =
    {
      config with
      security =
        {
          config.security with
          workspace_only = false;
          allowed_cwd_patterns = [];
        };
    }
  in
  Memory.set_session_cwd ~db ~session_key:"slack:C01" ~cwd:(Some cwd);
  let mgr = Session.create ~config ~db () in
  let key =
    Room_session.child_thread_key ~profile_id:"vip" ~connector:"slack"
      ~room_id:"C01" ~source_message_id:"1719000000.000100" ()
  in
  ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
  Alcotest.(check (option string))
    "child cwd" (Some cwd)
    (Session.get_session_effective_cwd mgr ~key)

let test_threadless_child_fallback_inherits_profile_and_parent_room_cwd () =
  let db = make_db () in
  let cwd =
    Filename.concat (Filename.get_temp_dir_name ()) "clawq-child-fallback-cwd"
  in
  if not (Sys.file_exists cwd) then Unix.mkdir cwd 0o755;
  let profiles =
    [
      {
        Runtime_config.id = "vip";
        display_name = None;
        model = "openai:gpt-4";
        system_prompt = "thread-less child room profile prompt";
        max_tool_iterations = 23;
        status = "active";
        allowed_tools = [];
        denied_tools = [];
        ambient_enabled = false;
        ambient_quiet_start = 23;
        ambient_quiet_end = 8;
        ambient_rate_limit_rph = 0;
      };
    ]
  in
  let bindings =
    [ { Runtime_config.profile_id = "vip"; room = "C01"; active = true } ]
  in
  let config =
    make_template_config ~allow_anthropic_oauth:true ~room_profiles:profiles
      ~room_profile_bindings:bindings ()
  in
  let config =
    {
      config with
      security =
        {
          config.security with
          workspace_only = false;
          allowed_cwd_patterns = [];
        };
    }
  in
  Memory.set_session_cwd ~db ~session_key:"slack:C01" ~cwd:(Some cwd);
  let mgr = Session.create ~config ~db () in
  let key =
    Room_session.child_thread_key ~profile_id:"vip" ~connector:"slack"
      ~room_id:"C01" ()
  in
  ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
  Alcotest.(check string)
    "fallback inherits profile model" "openai:gpt-4"
    (Session.get_session_effective_model mgr ~key);
  let defaults = Session.get_session_agent_defaults mgr ~key in
  Alcotest.(check string)
    "fallback inherits profile template" "thread-less child room profile prompt"
    defaults.system_prompt;
  Alcotest.(check int)
    "fallback inherits max iterations" 23 defaults.max_tool_iterations;
  Alcotest.(check bool)
    "fallback privacy guard active" true
    (Session.get_session_profiled_room mgr ~key);
  Alcotest.(check (option string))
    "fallback inherits parent room cwd" (Some cwd)
    (Session.get_session_effective_cwd mgr ~key)

let test_routine_session_inherits_profile_and_uses_routine_cwd () =
  let old_home = try Some (Sys.getenv "CLAWQ_HOME") with Not_found -> None in
  let home =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "clawq-routine-home-%d" (Unix.getpid ()))
  in
  let rec rmrf path =
    if Sys.file_exists path then
      match (Unix.lstat path).Unix.st_kind with
      | Unix.S_DIR ->
          Array.iter
            (fun name -> rmrf (Filename.concat path name))
            (Sys.readdir path);
          Unix.rmdir path
      | _ -> Unix.unlink path
  in
  (try Unix.mkdir home 0o755 with _ -> ());
  Unix.putenv "CLAWQ_HOME" home;
  Fun.protect
    ~finally:(fun () ->
      (match old_home with
      | Some value -> Unix.putenv "CLAWQ_HOME" value
      | None -> Unix.putenv "CLAWQ_HOME" "");
      rmrf home)
    (fun () ->
      let profiles =
        [
          {
            Runtime_config.id = "vip";
            display_name = None;
            model = "openai:gpt-4";
            system_prompt = "routine room profile prompt";
            max_tool_iterations = 19;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ]
      in
      let bindings =
        [ { Runtime_config.profile_id = "vip"; room = "C01"; active = true } ]
      in
      let config =
        make_template_config ~room_profiles:profiles
          ~room_profile_bindings:bindings ()
      in
      let config =
        {
          config with
          security =
            {
              config.security with
              workspace_only = false;
              allowed_cwd_patterns = [];
            };
        }
      in
      let key =
        Room_session.routine_key ~profile_id:"vip" ~routine_id:"daily-briefing"
          ()
      in
      let mgr = Session.create ~config () in
      ignore (Lwt_main.run (Session.runtime_context_block mgr ~key));
      Alcotest.(check string)
        "routine inherits profile model" "openai:gpt-4"
        (Session.get_session_effective_model mgr ~key);
      let defaults = Session.get_session_agent_defaults mgr ~key in
      Alcotest.(check string)
        "routine inherits profile template" "routine room profile prompt"
        defaults.system_prompt;
      Alcotest.(check int)
        "routine inherits max iterations" 19 defaults.max_tool_iterations;
      Alcotest.(check (option string))
        "routine cwd"
        (Some
           (Room_workspace.routine_workspace_path ~create:false
              ~profile_id:"vip" ~routine_id:"daily-briefing"))
        (Session.get_session_effective_cwd mgr ~key))

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
    Alcotest.test_case "clear_session_model_override clears DB" `Quick
      test_clear_session_model_override;
    Alcotest.test_case "clear_session_model_override noop when absent" `Quick
      test_clear_session_model_override_noop_when_no_override;
    Alcotest.test_case "clear_session_model restores global default" `Quick
      test_clear_session_model_restores_global;
    Alcotest.test_case "clear_session_model clears DB" `Quick
      test_clear_session_model_clears_db;
    Alcotest.test_case "room profile model overrides channel default" `Quick
      test_room_profile_model_overrides_channel_default;
    Alcotest.test_case "room profile model overrides global" `Quick
      test_room_profile_model_overrides_global;
    Alcotest.test_case "session override takes precedence over room profile"
      `Quick test_session_override_takes_precedence_over_room_profile;
    Alcotest.test_case "inactive room profile binding skipped" `Quick
      test_inactive_room_profile_binding_skipped;
    Alcotest.test_case "missing room profile returns none" `Quick
      test_missing_room_profile_returns_none;
    Alcotest.test_case
      "full precedence chain: session > room > channel > global" `Quick
      test_room_profile_full_precedence_chain;
    Alcotest.test_case
      "clear_session_model restores room profile on active session" `Quick
      test_clear_session_model_restores_room_profile_on_active_session;
    Alcotest.test_case "room profile model applied on session load" `Quick
      test_room_profile_model_applied_on_session_load;
    Alcotest.test_case "room profile binding matches full session key" `Quick
      test_room_profile_binding_matches_full_session_key;
    Alcotest.test_case "no room profiles returns none" `Quick
      test_no_room_profile_returns_none;
    Alcotest.test_case "oauth gate denies claude even with room profile" `Quick
      test_oauth_gate_denies_claude_even_with_room_profile;
    Alcotest.test_case "room profile system_prompt applied" `Quick
      test_room_profile_system_prompt_applied;
    Alcotest.test_case "room profile max_tool_iterations applied" `Quick
      test_room_profile_max_tool_iterations_applied;
    Alcotest.test_case "room profile template overrides global" `Quick
      test_room_profile_template_overrides_global;
    Alcotest.test_case "session model override does not affect template" `Quick
      test_session_model_override_does_not_affect_template;
    Alcotest.test_case "inactive room profile skips template" `Quick
      test_inactive_room_profile_skips_template;
    Alcotest.test_case "channel default used when no room profile" `Quick
      test_channel_default_used_when_no_room_profile;
    Alcotest.test_case "room profile beats template model" `Quick
      test_room_profile_beats_template;
    Alcotest.test_case "security gate denies anthropic model from profile"
      `Quick test_security_gate_denies_anthropic_from_profile;
    Alcotest.test_case "security gate allows anthropic when oauth enabled"
      `Quick test_security_gate_allows_anthropic_when_enabled;
    Alcotest.test_case "security gate allows non-anthropic model" `Quick
      test_security_gate_allows_non_anthropic;
    Alcotest.test_case "security gate denial message format" `Quick
      test_security_gate_denial_message_format;
    Alcotest.test_case "session override bypasses security gate" `Quick
      test_session_override_bypasses_security_gate;
    Alcotest.test_case "room profile prompt outranks template in built prompt"
      `Quick test_room_profile_prompt_out_ranks_template_in_built_prompt;
    Alcotest.test_case
      "child thread inherits profile model template and privacy" `Quick
      test_child_thread_session_inherits_profile_model_template_and_privacy;
    Alcotest.test_case "child thread inherits parent room cwd" `Quick
      test_child_thread_session_inherits_parent_room_cwd;
    Alcotest.test_case "threadless child fallback inherits profile and cwd"
      `Quick test_threadless_child_fallback_inherits_profile_and_parent_room_cwd;
    Alcotest.test_case "routine session inherits profile and cwd" `Quick
      test_routine_session_inherits_profile_and_uses_routine_cwd;
  ]
