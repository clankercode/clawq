open Lwt.Syntax
open Test_slack_fix1_fixtures

let test_routing ~marker () =
  with_recording (fun base calls _ _ _ _ _ ->
      let* result =
        if marker then
          Daemon_resume.dispatch_update_marker ~session_manager:base.manager
            ~config:base.manager.config
            ~marker:
              Restart_notify.
                {
                  channel = "slack";
                  channel_id = "C123";
                  session_key = Some key;
                  model = None;
                }
            ~text:"updated" ()
        else
          Daemon_resume.dispatch_session_fallback ~session_manager:base.manager
            ~config:base.manager.config ~session_key:key ~text:"rich fallback"
            ()
      in
      check_bool "actual daemon fallback reports success" true (result = Ok ());
      check_bool "actual daemon seam preserves root and real room" true
        (List.exists
           (fun (m, p) ->
             m = "chat.postMessage"
             && get "channel" p = str "C123"
             && get "thread_ts" p = str "100.000001")
           !calls);
      Lwt.return_unit)

let test_missing () =
  with_recording (fun base calls _ _ _ _ _ ->
      let* result =
        Daemon_resume.dispatch_update_marker ~session_manager:base.manager
          ~config:base.manager.config
          ~marker:
            Restart_notify.
              {
                channel = "slack";
                channel_id = "C123";
                session_key = None;
                model = None;
              }
          ~text:"missing locator" ()
      in
      check_bool "missing Slack marker locator cannot post channel completion"
        true
        ((match result with Error _ -> true | _ -> false) && writes calls = []);
      Lwt.return_unit)

let suite =
  [
    ( "daemon rich fallback carries session route",
      `Quick,
      test_routing ~marker:false );
    ( "daemon update marker carries session route",
      `Quick,
      test_routing ~marker:true );
    ("missing marker locator refuses channel fallback", `Quick, test_missing);
  ]
