(** B778: room-scoped cron pre-flight + configuration-error stuck detection. *)

let make_db () = Memory.init ~db_path:":memory:" ()
let default_config = Runtime_config.default

let room_profile id : Runtime_config.room_profile =
  {
    id;
    display_name = None;
    model = "openai:gpt-4o";
    system_prompt = "";
    max_tool_iterations = 10;
    status = "active";
    allowed_tools = [];
    denied_tools = [];
    access_bundle_ids = [];
    ambient_enabled = false;
    ambient_quiet_start = 23;
    ambient_quiet_end = 8;
    ambient_rate_limit_rph = 0;
  }

let with_binding room profile_id =
  {
    default_config with
    room_profiles = [ room_profile profile_id ];
    room_profile_bindings =
      [ { Runtime_config.profile_id; room; active = true } ];
  }

(* ── is_room_scoped_session_key ────────────────────────────────────────── *)

let test_room_scoped_detection () =
  Alcotest.(check bool)
    "teams room" true
    (Cron_room_preflight.is_room_scoped_session_key
       "teams:personal:19:f42c673471c643cdbbc2cb32a9aa6029@thread.v2");
  Alcotest.(check bool)
    "slack room" true
    (Cron_room_preflight.is_room_scoped_session_key "slack:C123:U456");
  Alcotest.(check bool)
    "discord room" true
    (Cron_room_preflight.is_room_scoped_session_key "discord:ch:u");
  Alcotest.(check bool)
    "cron worker not room" false
    (Cron_room_preflight.is_room_scoped_session_key "cron:briefing");
  Alcotest.(check bool)
    "chat not room" false
    (Cron_room_preflight.is_room_scoped_session_key "chat");
  Alcotest.(check bool)
    "default not room" false
    (Cron_room_preflight.is_room_scoped_session_key "default")

(* ── validate: reject unbound room ─────────────────────────────────────── *)

let test_reject_unbound_room () =
  let db = make_db () in
  let session_key = "teams:personal:19:abc@thread.v2" in
  match
    Cron_room_preflight.validate ~config:default_config ~db ~session_key
      ~name:"github-repo-monitor" ~message:"check PRs" ()
  with
  | Ok _ -> Alcotest.fail "expected unbound room to be rejected"
  | Error msg ->
      Alcotest.(check bool)
        "mentions profile binding" true
        (Test_helpers.string_contains msg "profile binding");
      Alcotest.(check bool)
        "actionable bind command" true
        (Test_helpers.string_contains msg "clawq rooms bind");
      Alcotest.(check bool)
        "mentions --force" true
        (Test_helpers.string_contains msg "--force")

let test_allow_non_room_session () =
  let db = make_db () in
  match
    Cron_room_preflight.validate ~config:default_config ~db
      ~session_key:"cron:briefing" ~name:"briefing" ~message:"run briefing" ()
  with
  | Error e -> Alcotest.fail ("non-room should pass: " ^ e)
  | Ok v -> Alcotest.(check int) "no warnings" 0 (List.length v.warnings)

let test_allow_bound_room_via_config () =
  let db = make_db () in
  let session_key = "slack:C123:U456" in
  (* Config binding on channel id (post-connector segment), matching
     Runtime_config.resolve_room_profile. *)
  let cfg = with_binding "C123:U456" "ops" in
  match
    Cron_room_preflight.validate ~config:cfg ~db ~session_key
      ~name:"daily-standup" ~message:"summarize channel" ()
  with
  | Error e -> Alcotest.fail ("bound room should pass: " ^ e)
  | Ok _ -> ()

let test_allow_bound_room_via_db () =
  let db = make_db () in
  let session_key = "discord:guild:channel" in
  let profile_id = Memory.insert_room_profile ~db ~name:"ops-profile" in
  (* Binding key is the post-connector segment used by room memory tools. *)
  Memory.upsert_room_profile_binding ~db ~room_id:"guild:channel" ~profile_id;
  match
    Cron_room_preflight.validate ~config:default_config ~db ~session_key
      ~name:"room-job" ~message:"hello room" ()
  with
  | Error e -> Alcotest.fail ("DB-bound room should pass: " ^ e)
  | Ok _ -> ()

let test_force_allows_unbound_with_warning () =
  let db = make_db () in
  let session_key = "teams:team:conv" in
  match
    Cron_room_preflight.validate ~config:default_config ~db ~force:true
      ~session_key ~name:"forced-job" ~message:"run anyway" ()
  with
  | Error e -> Alcotest.fail ("force should allow: " ^ e)
  | Ok v ->
      Alcotest.(check bool)
        "force warning present" true
        (List.exists
           (fun w -> Test_helpers.string_contains w "--force")
           v.warnings)

let test_github_oriented_principal_warning () =
  let db = make_db () in
  let session_key = "slack:C1:U1" in
  let cfg = with_binding "C1:U1" "ops" in
  (* Ensure env is unset for this process for the duration of the check. *)
  let prev = Sys.getenv_opt "CLAWQ_PRINCIPAL_ID" in
  (try Unix.putenv "CLAWQ_PRINCIPAL_ID" "" with _ -> ());
  let result =
    Cron_room_preflight.validate ~config:cfg ~db ~session_key
      ~name:"github-repo-monitor" ~message:"scan github PRs" ()
  in
  (match prev with
  | Some v -> Unix.putenv "CLAWQ_PRINCIPAL_ID" v
  | None ->
      (* Cannot truly unset in OCaml; leave empty which principal_id_set treats
         as unset. *)
      ());
  match result with
  | Error e -> Alcotest.fail ("should schedule with warning: " ^ e)
  | Ok v ->
      Alcotest.(check bool)
        "principal warning" true
        (List.exists
           (fun w -> Test_helpers.string_contains w "CLAWQ_PRINCIPAL_ID")
           v.warnings)

(* ── Stuck detector: configuration errors ──────────────────────────────── *)

let tool_error ~name ~content =
  {
    (Provider.make_tool_result ~tool_call_id:"c1" ~name ~content) with
    Provider.is_error = true;
  }

let test_config_error_classification () =
  Alcotest.(check bool)
    "binding error" true
    (Stuck_detector.is_configuration_error
       "Error: No memory scope or profile binding found for room 'x'. Bind a \
        room profile first.");
  Alcotest.(check bool)
    "github empty room" true
    (Stuck_detector.is_configuration_error
       "Error: Room \"x\" has no GitHub item access: empty journal and \
        projections");
  Alcotest.(check bool)
    "principal env" true
    (Stuck_detector.is_configuration_error
       "Error: github_account tool: CLAWQ_PRINCIPAL_ID must be set so the tool \
        can scope inspection to the current Principal.");
  Alcotest.(check bool)
    "generic error not config" false
    (Stuck_detector.is_configuration_error "Error: file not found: /tmp/x")

let test_config_error_definite_at_two () =
  let err =
    "Error: No memory scope or profile binding found for room 'abc'. Bind a \
     room profile first."
  in
  let history =
    [
      tool_error ~name:"room_memory_list" ~content:err;
      tool_error ~name:"room_memory_search" ~content:err;
    ]
  in
  match Stuck_detector.check ~history ~iteration:2 ~max_iters:20 with
  | Stuck_detector.Definite signals ->
      Alcotest.(check bool)
        "config error detected" true
        (Stuck_detector.has_configuration_error signals);
      let abort = Stuck_detector.configuration_abort_message signals in
      Alcotest.(check bool)
        "abort says room not configured" true
        (Test_helpers.string_contains abort "Room not configured");
      Alcotest.(check bool)
        "abort has bind fix" true
        (Test_helpers.string_contains abort "clawq rooms bind")
  | Stuck_detector.Clear | Stuck_detector.Suspicious _ ->
      Alcotest.fail "expected Definite for 2 identical configuration errors"

let test_generic_error_not_definite_at_two () =
  let err = "Error: temporary network glitch" in
  let history =
    [
      tool_error ~name:"web_fetch" ~content:err;
      tool_error ~name:"web_fetch" ~content:err;
    ]
  in
  match Stuck_detector.check ~history ~iteration:2 ~max_iters:20 with
  | Stuck_detector.Suspicious _ -> ()
  | Stuck_detector.Definite signals
    when Stuck_detector.has_configuration_error signals ->
      Alcotest.fail "generic errors must not be classified as configuration"
  | Stuck_detector.Definite _ ->
      Alcotest.fail "generic SameErrorString should be Suspicious at count=2"
  | Stuck_detector.Clear ->
      Alcotest.fail "expected Suspicious for 2 identical generic errors"

let suite =
  [
    Alcotest.test_case "room-scoped session key detection" `Quick
      test_room_scoped_detection;
    Alcotest.test_case "reject unbound room cron" `Quick
      test_reject_unbound_room;
    Alcotest.test_case "allow non-room session" `Quick
      test_allow_non_room_session;
    Alcotest.test_case "allow bound room via config" `Quick
      test_allow_bound_room_via_config;
    Alcotest.test_case "allow bound room via DB" `Quick
      test_allow_bound_room_via_db;
    Alcotest.test_case "force allows unbound with warning" `Quick
      test_force_allows_unbound_with_warning;
    Alcotest.test_case "github-oriented principal warning" `Quick
      test_github_oriented_principal_warning;
    Alcotest.test_case "configuration error classification" `Quick
      test_config_error_classification;
    Alcotest.test_case "config errors Definite at 2" `Quick
      test_config_error_definite_at_two;
    Alcotest.test_case "generic errors not Definite at 2" `Quick
      test_generic_error_not_definite_at_two;
  ]
