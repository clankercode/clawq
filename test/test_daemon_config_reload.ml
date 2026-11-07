module S = Test_slack_standing_order_support

let test_publication () =
  let old = !Session_management.on_config_published in
  let manager = Session.create ~config:Runtime_config.default () in
  let next =
    {
      Runtime_config.default with
      security = { Runtime_config.default.security with tools_enabled = false };
    }
  in
  let observed = ref false in
  Fun.protect
    ~finally:(fun () -> Session_management.on_config_published := old)
    (fun () ->
      (Session_management.on_config_published :=
         fun () -> observed := Session.get_config manager == next);
      Session.update_config manager next;
      Alcotest.(check bool)
        "publication hook observes assigned config synchronously" true !observed)

let test_direct_publication_failure () =
  let old = !Session_management.on_config_published in
  let original = Runtime_config.default in
  let manager = Session.create ~config:original () in
  let agent, _, _ = Session.get_or_create_locked manager ~key:"slack:C123" in
  let next =
    {
      original with
      security = { original.security with tools_enabled = false };
    }
  in
  Fun.protect
    ~finally:(fun () -> Session_management.on_config_published := old)
    (fun () ->
      (Session_management.on_config_published :=
         fun () -> failwith "injected publication failure");
      let failed =
        try
          Session.update_config manager next;
          false
        with Failure message -> message = "injected publication failure"
      in
      Alcotest.(check bool) "publication failure remains observable" true failed;
      Alcotest.(check bool)
        "manager retains published policy" false
        (Session.get_config manager).security.tools_enabled;
      Alcotest.(check bool)
        "existing agent follows published policy despite hook failure" false
        agent.Agent.config.security.tools_enabled)

let test_rollback () =
  let old = !Session_management.on_config_published in
  let original = Runtime_config.default in
  let manager = Session.create ~config:original () in
  let next =
    {
      original with
      security = { original.security with tools_enabled = false };
    }
  in
  let current_config = ref original
  and sandbox = ref (Daemon_util.make_sandbox original) in
  let published = ref [] in
  Fun.protect
    ~finally:(fun () -> Session_management.on_config_published := old)
    (fun () ->
      (Session_management.on_config_published :=
         fun () ->
           published := !published @ [ Session.get_config manager ];
           if List.length !published = 1 then
             failwith "injected publication failure");
      let result =
        Daemon_config_reload.apply_runtime_config_reload
          ~reconcile_room_profiles:(fun ~db:_ ~config:_ -> ())
          ~source:"test" ~current_config ~session_manager:manager ~sandbox
          ~db:None ~tool_registry:None ~new_config:next ()
      in
      Alcotest.(check bool)
        "reload reports failure" true (Result.is_error result);
      Alcotest.(check bool)
        "rollback publication observed in order" true
        (match !published with
        | [ a; b ] -> a == next && b == original
        | _ -> false);
      Alcotest.(check bool)
        "manager rolled back" true
        (Session.get_config manager == original);
      Alcotest.(check bool)
        "published ref rolled back" true
        (!current_config == original))

let suite =
  [
    S.case "direct publication is synchronous" test_publication;
    S.case "direct publication failure refreshes agents"
      test_direct_publication_failure;
    S.case "publication exception also publishes rollback" test_rollback;
  ]
