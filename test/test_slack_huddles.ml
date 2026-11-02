open Lwt.Syntax
open Test_slack_huddle_fixtures

let test_discovery () =
  with_fixture (fun f ->
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          let json = parse result in
          check_bool "all reads reached injected HTTP" true
            (List.exists (fun (_, m, _) -> m = "auth.test") !(f.calls));
          Alcotest.(check int) "huddle found" 1 (count "huddles" json);
          let _, _, params =
            List.find (fun (_, m, _) -> m = "conversations.history") !(f.calls)
          in
          Alcotest.(check string)
            "bounded page" "15"
            (List.assoc "limit" params |> Yojson.Safe.Util.to_string);
          check_bool "complete" true (get "complete" json = `Bool true);
          Lwt.return_unit))

let test_content () =
  with_fixture (fun f ->
      f.turn (fun () ->
          let* json = call_content f in
          Alcotest.(check int) "inline transcript" 1 (count "transcripts" json);
          Alcotest.(check int) "notes separate" 1 (count "thread_notes" json);
          check_bool "does not assert complete transcript" true
            (contains (Yojson.Safe.to_string json) "not asserted");
          Lwt.return_unit))

let test_cursor () =
  with_fixture (fun f ->
      override f "conversations.history" (fun json _ ->
          update json
            [
              ("has_more", `Bool true);
              ("response_metadata", obj [ ("next_cursor", str "next-page") ]);
            ]);
      f.turn (fun () ->
          let args =
            update discovery
              [ ("cursor", str "previous-page"); ("limit", `Int 2) ]
          in
          let* result = f.invoke ~args "slack_huddles" in
          let json = parse result in
          check_bool "partial" true (get "complete" json = `Bool false);
          Alcotest.(check string)
            "cursor returned" "next-page"
            (get "next_cursor" json |> Yojson.Safe.Util.to_string);
          let calls =
            List.filter
              (fun (_, m, _) -> m = "conversations.history")
              !(f.calls)
          in
          Alcotest.(check int) "one page only" 1 (List.length calls);
          let _, _, p = List.hd calls in
          check_bool "cursor and limit passed" true
            (List.assoc "cursor" p = str "previous-page"
            && List.assoc "limit" p = str "2");
          Lwt.return_unit))

let test_invalid_window () =
  with_fixture (fun f ->
      f.turn (fun () ->
          let* results =
            Lwt_list.map_s
              (fun fields ->
                f.invoke ~args:(update discovery fields) "slack_huddles")
              [
                [ ("latest", str "NaN") ];
                [ ("latest", str "89") ];
                [ ("latest", str "9999999") ];
                [ ("limit", `Int 16) ];
                [ ("limit", `String "2") ];
              ]
          in
          List.iter expect_error results;
          check_bool "no API calls" true (!(f.calls) = []);
          Lwt.return_unit))

let test_no_identity () =
  with_fixture (fun f ->
      let* result = f.invoke "slack_huddles" in
      expect_error result;
      check_bool "no API calls" true (!(f.calls) = []);
      Lwt.return_unit)

let test_arbitrary_file () =
  with_fixture (fun f ->
      f.turn (fun () ->
          let* result =
            f.invoke
              ~args:(update content [ ("file_id", str "FSECRET") ])
              "slack_huddle_content"
          in
          expect_error result;
          check_bool "no API calls" true (!(f.calls) = []);
          Lwt.return_unit))

let test_member_denied () =
  with_fixture (fun f ->
      override f "conversations.members" (fun _ _ ->
          ok [ ("members", arr [ str "UBOT" ]) ]);
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          no_content f;
          Lwt.return_unit))

let test_member_pagination () =
  with_fixture (fun f ->
      override f "conversations.members" (fun json p ->
          if List.mem_assoc "cursor" p then json
          else
            ok
              [
                ("members", arr [ str "UBOT" ]);
                ( "response_metadata",
                  obj [ ("next_cursor", str "members-next") ] );
              ]);
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          ignore (parse result);
          Alcotest.(check int)
            "membership proved on second page" 2
            (List.filter
               (fun (_, m, _) -> m = "conversations.members")
               !(f.calls)
            |> List.length);
          Lwt.return_unit))

let test_member_bound () =
  with_fixture (fun f ->
      let page = ref 0 in
      override f "conversations.members" (fun _ _ ->
          incr page;
          ok
            [
              ("members", arr [ str "UBOT" ]);
              ( "response_metadata",
                obj [ ("next_cursor", str (string_of_int !page)) ] );
            ]);
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          no_content f;
          Alcotest.(check int) "bounded membership lookup" 5 !page;
          Lwt.return_unit))

let test_cross_non_admin () =
  with_fixture (fun f ->
      f.turn (fun () ->
          let* result = f.invoke ~args:(at_target discovery) "slack_huddles" in
          expect_error result;
          no_content f;
          Lwt.return_unit))

let test_cross_guest_admin () =
  with_fixture (fun f ->
      admin f;
      override f "users.info" (fun json _ ->
          update json
            [
              ( "user",
                update (get "user" json) [ ("is_restricted", `Bool true) ] );
            ]);
      f.turn (fun () ->
          let* result = f.invoke ~args:(at_target discovery) "slack_huddles" in
          expect_error result;
          no_content f;
          Lwt.return_unit))

let test_cross_admin_membership () =
  with_fixture (fun f ->
      admin f;
      override f "conversations.members" (fun json p ->
          if List.assoc "channel" p = str "C999" then
            ok [ ("members", arr [ str "UBOT" ]) ]
          else json);
      f.turn (fun () ->
          let* result = f.invoke ~args:(at_target discovery) "slack_huddles" in
          expect_error result;
          no_content f;
          let channels =
            List.filter_map
              (fun (_, m, p) ->
                if m = "conversations.members" then
                  Some (List.assoc "channel" p)
                else None)
              !(f.calls)
          in
          check_bool "origin and target checked" true
            (List.mem (str "C123") channels && List.mem (str "C999") channels);
          Lwt.return_unit))

let test_cross_admin () =
  with_fixture (fun f ->
      admin f;
      f.turn (fun () ->
          let* result = f.invoke ~args:(at_target discovery) "slack_huddles" in
          let json = parse result in
          check_bool "target provenance" true
            (get "channel_id"
               (List.hd (get "huddles" json |> Yojson.Safe.Util.to_list))
            = str "C999");
          Lwt.return_unit))

let test_private_denied () =
  with_fixture (fun f ->
      update_slack f (fun s ->
          { s with private_channel_policy = Runtime_config.Pc_deny });
      override f "conversations.info" (fun json _ ->
          update json
            [
              ( "channel",
                update (get "channel" json) [ ("is_private", `Bool true) ] );
            ]);
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          no_content f;
          Lwt.return_unit))

let test_private_opt_in () =
  with_fixture (fun f ->
      update_slack f (fun s ->
          {
            s with
            private_channel_policy = Runtime_config.Pc_deny;
            allow_private_channels = [ "C123" ];
          });
      override f "conversations.info" (fun json _ ->
          update json
            [
              ( "channel",
                update (get "channel" json) [ ("is_private", `Bool true) ] );
            ]);
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          ignore (parse result);
          Lwt.return_unit))

let test_allowlists () =
  with_fixture (fun f ->
      update_slack f (fun s -> { s with allow_users = [ "UOTHER" ] });
      let* () =
        f.turn (fun () ->
            let* result = f.invoke "slack_huddles" in
            expect_error result;
            check_bool "no calls" true (!(f.calls) = []);
            Lwt.return_unit)
      in
      update_slack f (fun s ->
          { s with allow_users = [ "*" ]; allow_channels = [ "COTHER" ] });
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          check_bool "no calls" true (!(f.calls) = []);
          Lwt.return_unit))

let test_identity_team () =
  with_fixture (fun f ->
      override f "users.info" (fun json _ ->
          update json
            [ ("user", update (get "user" json) [ ("team_id", str "TOTHER") ]) ]);
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          no_content f;
          Lwt.return_unit))

let test_notes_only () =
  with_fixture (fun f ->
      override f "files.info" (fun json _ ->
          update json
            [
              ( "file",
                obj
                  [
                    ("id", str "F123");
                    ("preview_plain_text", str "Only notes");
                    ( "transcription_hl",
                      obj [ ("lines", arr [ str "Highlighted excerpt" ]) ] );
                    ("huddle_summary_id", str "SUM123");
                  ] );
            ]);
      f.turn (fun () ->
          let* json = call_content f in
          Alcotest.(check int)
            "no fabricated transcript" 0 (count "transcripts" json);
          check_bool "explicit unavailable" true
            (get "transcript_status" json = str "unavailable_in_returned_data");
          Alcotest.(check int)
            "thread discussion retained" 1
            (count "thread_notes" json);
          Lwt.return_unit))

let test_follow_transcript () =
  with_fixture (fun f ->
      override f "files.info" (fun json p ->
          if List.assoc "file" p = str "F123" then
            ok
              [
                ( "file",
                  obj
                    [
                      ("id", str "F123");
                      ("file_access", str "check_file_info");
                      ("huddle_transcript_file_id", str "F456");
                    ] );
              ]
          else json);
      f.turn (fun () ->
          let* json = call_content f in
          Alcotest.(check int) "two hydrated files" 2 (count "files" json);
          Alcotest.(check int)
            "followed actual transcript" 1 (count "transcripts" json);
          Lwt.return_unit))

let test_hydration_bound () =
  with_fixture (fun f ->
      let n = ref 0 in
      override f "files.info" (fun json _ ->
          incr n;
          update json
            [
              ( "file",
                update (get "file" json)
                  [
                    ( "huddle_transcript_file_id",
                      str ("F" ^ string_of_int (1000 + !n)) );
                  ] );
            ]);
      f.turn (fun () ->
          let* json = call_content f in
          Alcotest.(check int) "eight file maximum" 8 !n;
          check_bool "explicit partial" true
            (get "content_complete" json = `Bool false
            && get "files_truncated" json = `Bool true);
          Lwt.return_unit))

let test_file_wrong_huddle () =
  with_fixture (fun f ->
      override f "files.info" (fun json _ ->
          update json
            [
              ( "file",
                update (get "file" json)
                  [
                    ( "canvas_metadata",
                      obj [ ("originating_huddle_id", str "ROTHER") ] );
                  ] );
            ]);
      f.turn (fun () ->
          let* json = call_content f in
          Alcotest.(check int) "content withheld" 0 (count "transcripts" json);
          Alcotest.(check int) "provenance error" 1 (count "file_errors" json);
          Lwt.return_unit))

let test_file_wrong_channel () =
  with_fixture (fun f ->
      override f "files.info" (fun json _ ->
          update json
            [
              ( "file",
                update (get "file" json)
                  [
                    ( "huddle_transcription",
                      obj
                        [
                          ("channel_id", str "CSECRET");
                          ("lines", arr [ str "secret" ]);
                        ] );
                  ] );
            ]);
      f.turn (fun () ->
          let* json = call_content f in
          Alcotest.(check int) "content withheld" 0 (count "transcripts" json);
          check_bool "secret content absent" false
            (contains (Yojson.Safe.to_string json) "secret");
          Lwt.return_unit))

let test_no_source () =
  with_fixture (fun f ->
      override f "conversations.history" (fun _ _ ->
          ok
            [
              ( "messages",
                arr
                  [
                    obj
                      [
                        ("ts", str "100.000001");
                        ("text", str "ordinary message");
                      ];
                  ] );
            ]);
      f.turn (fun () ->
          let* result = f.invoke ~args:content "slack_huddle_content" in
          expect_error result;
          check_bool "no thread/files read" true
            (List.for_all
               (fun (_, m, _) ->
                 m <> "conversations.replies" && m <> "files.info")
               !(f.calls));
          Lwt.return_unit))

let test_reply_page () =
  with_fixture (fun f ->
      override f "conversations.replies" (fun json _ ->
          update json
            [
              ("has_more", `Bool true);
              ("response_metadata", obj [ ("next_cursor", str "notes-next") ]);
            ]);
      f.turn (fun () ->
          let* json = call_content f in
          check_bool "thread partial" true
            (get "complete" json = `Bool false
            && get "next_cursor" json = str "notes-next");
          Lwt.return_unit))

let test_missing_scope () =
  with_fixture (fun f ->
      override f "files.info" (fun _ _ ->
          obj [ ("ok", `Bool false); ("error", str "missing_scope") ]);
      f.turn (fun () ->
          let* json = call_content f in
          check_bool "partial with repair" true
            (get "content_complete" json = `Bool false
            && contains (Yojson.Safe.to_string json) "files:read");
          Lwt.return_unit))

let test_rate_limit () =
  with_fixture (fun f ->
      override f "conversations.history" (fun _ _ ->
          obj [ ("ok", `Bool false); ("error", str "ratelimited") ]);
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          check_bool "actionable rate limit" true
            (contains result "no automatic retry");
          Alcotest.(check int)
            "one request, no retries" 1
            (List.filter
               (fun (_, m, _) -> m = "conversations.history")
               !(f.calls)
            |> List.length);
          Lwt.return_unit))

let test_http_error () =
  with_fixture (fun f ->
      f.status := 429;
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          check_bool "429 recognized" true (contains result "rate_limited");
          Alcotest.(check int) "no retry" 1 (List.length !(f.calls));
          Lwt.return_unit))

let test_exception_redacted () =
  with_fixture (fun f ->
      (f.handle := fun _ _ -> failwith "xoxb-do-not-expose");
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          check_bool "credential absent" false
            (contains result "xoxb-do-not-expose");
          Lwt.return_unit))

let test_frozen_reload () =
  with_fixture (fun f ->
      let* () =
        f.turn (fun () ->
            let* first = f.invoke "slack_huddles" in
            ignore (parse first);
            update_slack f (fun s ->
                {
                  s with
                  bot_token = "xoxb-reloaded";
                  allow_channels = [ "COTHER" ];
                });
            let* second = f.invoke "slack_huddles" in
            ignore (parse second);
            check_bool "same turn frozen credential" true
              (List.for_all (fun (t, _, _) -> t = "xoxb-test") !(f.calls));
            Lwt.return_unit)
      in
      f.calls := [];
      let* () =
        f.turn (fun () ->
            let* result = f.invoke "slack_huddles" in
            expect_error result;
            check_bool "next turn refreshed allowlist" true (!(f.calls) = []);
            Lwt.return_unit)
      in
      update_slack f (fun s -> { s with allow_channels = [ "*" ] });
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          ignore (parse result);
          check_bool "stable registry uses new token" true
            (List.for_all (fun (t, _, _) -> t = "xoxb-reloaded") !(f.calls));
          Lwt.return_unit))

let test_disable () =
  with_fixture (fun f ->
      f.turn (fun () ->
          let config = !(f.cfg) in
          f.cfg :=
            { config with channels = { config.channels with slack = None } };
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          check_bool "disabled without calls" true (!(f.calls) = []);
          Lwt.return_unit))

let test_stale_snapshot () =
  with_fixture (fun f ->
      f.turn (fun () ->
          let stale = f.agent.access_snapshot_id in
          f.snapshot ();
          let tool =
            Option.get
              (Tool_registry.find
                 (Option.get f.manager.tool_registry)
                 "slack_huddles")
          in
          let* result =
            tool.invoke
              ~context:
                {
                  Tool.default_context with
                  session_key = Some f.key;
                  snapshot_id = stale;
                }
              discovery
          in
          expect_error result;
          check_bool "stale denied before network" true (!(f.calls) = []);
          Lwt.return_unit))

let test_clear_boundary () =
  with_fixture (fun f ->
      f.turn (fun () ->
          Slack_huddle_context.clear ();
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          check_bool "no authority after turn" true (!(f.calls) = []);
          Lwt.return_unit))

let test_denied_tool () =
  with_fixture (fun f ->
      f.turn (fun () ->
          ignore
            (Sqlite3.exec f.db
               "UPDATE access_snapshots SET \
                denied_tools_json='[\"slack_huddles\"]'");
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          check_bool "frozen tool denial" true (!(f.calls) = []);
          Lwt.return_unit))

let test_empty_transcript () =
  with_fixture (fun f ->
      override f "files.info" (fun json _ ->
          update json
            [
              ( "file",
                update (get "file" json)
                  [
                    ( "huddle_transcription",
                      obj
                        [
                          ("channel_id", str "C123");
                          ("lines", arr [ obj [ ("speaker", str "U123") ] ]);
                          ("blocks", arr []);
                        ] );
                  ] );
            ]);
      f.turn (fun () ->
          let* json = call_content f in
          Alcotest.(check int)
            "metadata alone is not transcript content" 0
            (count "transcripts" json);
          Lwt.return_unit))

let test_malformed_page () =
  with_fixture (fun f ->
      override f "conversations.history" (fun _ _ -> ok []);
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          check_bool "malformed page explicit" true
            (contains result "malformed");
          Lwt.return_unit))

let test_canvas_thread () =
  with_fixture (fun f ->
      override f "conversations.history" (fun json _ ->
          let source =
            update root
              [
                ( "room",
                  update (get "room" root) [ ("canvas_thread_ts", str "102.1") ]
                );
              ]
          in
          update json [ ("messages", arr [ source ]) ]);
      f.turn (fun () ->
          let* result =
            f.invoke
              ~args:
                (update content
                   [ ("thread", str "canvas"); ("cursor", str "canvas-next") ])
              "slack_huddle_content"
          in
          let json = parse result in
          check_bool "canvas provenance" true
            (get "thread_ts" json = str "102.1");
          let _, _, params =
            List.find (fun (_, m, _) -> m = "conversations.replies") !(f.calls)
          in
          check_bool "only documented thread and cursor used" true
            (List.assoc "ts" params = str "102.1"
            && List.assoc "cursor" params = str "canvas-next");
          Lwt.return_unit))

let test_queue_identity () =
  with_fixture (fun f ->
      f.turn (fun () ->
          (* Simulate the verified second ingress returning its queued response, then
     run the same before/after callbacks as Slack's production drainer. *)
          let queued : Session.queued_message =
            {
              message = "next";
              content_parts = [];
              attachments = [];
              channel_name = Some "C123";
              channel_type = Some "group";
              sender_id = Some "U456";
              sender_name = None;
              user_group = Some "guest";
              channel = Some "slack";
              channel_id = Some "C123";
              message_id = Some "99.2";
              inbound_queue_id = None;
              bang = false;
              deferred_followup = false;
              snapshot_work_type = Some Access_snapshot.Room_turn;
              has_external_users = false;
            }
          in
          let* () =
            Slack_huddle_context.with_request ~session_manager:f.manager
              ~key:f.key ~channel_id:"C123" ~user_id:"U456" ~message_id:"99.2"
              (fun () ->
                Hashtbl.replace f.manager.queued_messages f.key [ queued ];
                Lwt.return_unit)
          in
          Slack_huddle_context.clear ();
          Hashtbl.remove f.manager.queued_messages f.key;
          Slack_huddle_context.before_queued ~key:f.key (Some "99.2");
          f.snapshot ();
          override f "users.info" (fun json p ->
              update json
                [
                  ( "user",
                    update (get "user" json) [ ("id", List.assoc "user" p) ] );
                ]);
          override f "conversations.members" (fun _ _ ->
              ok [ ("members", arr [ str "U456"; str "UBOT" ]) ]);
          let* result = f.invoke "slack_huddles" in
          ignore (parse result);
          let _, _, params =
            List.find (fun (_, m, _) -> m = "users.info") !(f.calls)
          in
          check_bool "queued requester used, never prior owner" true
            (List.assoc "user" params = str "U456");
          Slack_huddle_context.clear ();
          let* denied = f.invoke "slack_huddles" in
          expect_error denied;
          Lwt.return_unit))

let test_missing_queue_identity () =
  with_fixture (fun f ->
      f.turn (fun () ->
          Slack_huddle_context.before_queued ~key:f.key (Some "never-ingressed");
          f.snapshot ();
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          check_bool "replay cannot inherit owner" true (!(f.calls) = []);
          Lwt.return_unit))

let test_admin_no_escalation () =
  with_fixture (fun f ->
      f.turn (fun () ->
          admin f;
          let* result = f.invoke ~args:(at_target discovery) "slack_huddles" in
          expect_error result;
          no_content f;
          Lwt.return_unit))

let test_admin_revocation () =
  with_fixture (fun f ->
      admin f;
      f.turn (fun () ->
          ignore (Sqlite3.exec f.db "DELETE FROM channel_admins");
          let* result = f.invoke ~args:(at_target discovery) "slack_huddles" in
          expect_error result;
          no_content f;
          Lwt.return_unit))

let test_egress_denied () =
  with_fixture (fun f ->
      let cfg = !(f.cfg) in
      f.cfg := { cfg with egress = { cfg.egress with default_allowlist = [] } };
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          check_bool "egress repair guidance" true
            (contains result "egress denied");
          check_bool "no HTTP before allow" true (!(f.calls) = []);
          Lwt.return_unit))

let test_egress_files_denied () =
  with_fixture (fun f ->
      let cfg = !(f.cfg) in
      let deny : Runtime_config_types.egress_rule =
        {
          host = "slack.com";
          path = Some "/api/files.info";
          method_ = Some "GET";
          action = Deny;
          log_policy = No_log;
        }
      in
      f.cfg :=
        {
          cfg with
          egress =
            {
              cfg.egress with
              default_allowlist = deny :: cfg.egress.default_allowlist;
            };
        };
      f.turn (fun () ->
          let* json = call_content f in
          check_bool "files read denied with partial result" true
            (get "content_complete" json = `Bool false
            && contains (Yojson.Safe.to_string json) "egress denied");
          check_bool "denied files never sent" true
            (List.for_all (fun (_, m, _) -> m <> "files.info") !(f.calls));
          Lwt.return_unit))

let test_files_rate_limit_stops_page () =
  with_fixture (fun f ->
      override f "conversations.history" (fun json _ ->
          update json
            [
              ( "messages",
                arr
                  [
                    update root
                      [
                        ( "room",
                          update (get "room" root)
                            [
                              ( "attached_file_ids",
                                arr [ str "F123"; str "F456" ] );
                            ] );
                      ];
                  ] );
            ]);
      override f "files.info" (fun _ _ ->
          obj [ ("ok", `Bool false); ("error", str "ratelimited") ]);
      f.turn (fun () ->
          let* json = call_content f in
          check_bool "partial files explicit" true
            (get "content_complete" json = `Bool false
            && get "files_truncated" json = `Bool true);
          Alcotest.(check int)
            "stop method after rate limit" 1
            (List.filter (fun (_, m, _) -> m = "files.info") !(f.calls)
            |> List.length);
          Lwt.return_unit))

let cases =
  [
    ("file rate limit stops further hydration", test_files_rate_limit_stops_page);
    ("egress policy denies before any HTTP", test_egress_denied);
    ("egress policy checked for file hydration", test_egress_files_denied);
    ("empty transcription metadata stays unavailable", test_empty_transcript);
    ("malformed message page is an error", test_malformed_page);
    ("documented canvas thread selection", test_canvas_thread);
    ("queued turn selects its own requesting human", test_queue_identity);
    ("missing queue evidence cannot inherit owner", test_missing_queue_identity);
    ("admin grant does not escalate frozen turn", test_admin_no_escalation);
    ("admin revocation respected at execution", test_admin_revocation);
    ("authorized discovery through registry", test_discovery);
    ("inline transcript and thread notes separate", test_content);
    ("bounded discovery and opaque cursor", test_cursor);
    ("invalid window and limits make no requests", test_invalid_window);
    ("session name without ingress is not authority", test_no_identity);
    ("arbitrary file IDs rejected", test_arbitrary_file);
    ("bot membership cannot authorize human", test_member_denied);
    ("human membership cursor followed", test_member_pagination);
    ("membership lookup bounded", test_member_bound);
    ("cross-channel non-admin denied", test_cross_non_admin);
    ("guest admin cross-channel denied", test_cross_guest_admin);
    ("cross-channel checks origin and target", test_cross_admin_membership);
    ("admin cross-channel requires membership", test_cross_admin);
    ("private channel denied", test_private_denied);
    ("private channel explicit opt-in", test_private_opt_in);
    ("current user and channel allowlists", test_allowlists);
    ("requester workspace identity checked", test_identity_team);
    ("notes and highlighted previews are not transcripts", test_notes_only);
    ("conditional transcript followup hydrated", test_follow_transcript);
    ("file traversal bounded and partial", test_hydration_bound);
    ("file huddle provenance mismatch withheld", test_file_wrong_huddle);
    ("transcript channel mismatch withheld", test_file_wrong_channel);
    ("ordinary message cannot grant file access", test_no_source);
    ("thread cursor marks partial result", test_reply_page);
    ("files scope errors actionable and partial", test_missing_scope);
    ("Slack rate limit no eager retry", test_rate_limit);
    ("HTTP rate limit no eager retry", test_http_error);
    ("network exceptions redact credentials", test_exception_redacted);
    ("frozen turn and live next-turn reload", test_frozen_reload);
    ("runtime disable blocks stable tool", test_disable);
    ("stale turn snapshot denied", test_stale_snapshot);
    ("completed turn authority cleared", test_clear_boundary);
    ("frozen tool denial enforced", test_denied_tool);
  ]

let suite =
  List.map (fun (name, test) -> Alcotest.test_case name `Quick test) cases
