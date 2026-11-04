open Lwt.Syntax
module P = Slack_peer_thread
open Test_slack_huddle_fixtures

let allowed : Slack_config_types.standing_order_target_config list =
  [ { app_id = "ACLAUDE"; label = "Claude" } ]

let peer ?(profile = true) () =
  obj
    ([ ("bot_id", str "BCLAUDE"); ("user", str "UCLAUDE") ]
    @
    if profile then [ ("bot_profile", obj [ ("app_id", str "ACLAUDE") ]) ]
    else [ ("app_id", str "ACLAUDE") ])

let test_mentions () =
  Alcotest.(check (list string))
    "labels, W ids, no duplicates" [ "U1"; "W2" ]
    (P.mentioned_users "<@U1|name> <@W2> <@U1> <#C1> <!channel>");
  Alcotest.(check (list string))
    "ignore malformed mentions" []
    (P.mentioned_users "<@> <@B1> <@U bad> <@U1");
  Lwt.return_unit

let test_peer_message () =
  List.iter
    (fun p -> check_bool "peer" true (P.is_peer_message ~allowed p))
    [ peer (); peer ~profile:false () ];
  List.iter
    (fun p -> check_bool "not peer" false (P.is_peer_message ~allowed p))
    [
      obj [ ("app_id", str "ACLAUDE") ];
      obj [ ("bot_id", str "BOTHER"); ("app_id", str "AOTHER") ];
      obj [ ("bot_id", `Null); ("app_id", str "ACLAUDE") ];
    ];
  check_bool "empty allowlist" false (P.is_peer_message ~allowed:[] (peer ()));
  Lwt.return_unit

let test_decision () =
  let lookups = ref [] in
  let lookup id =
    lookups := id :: !lookups;
    Lwt.return (id = "UCLAUDE")
  in
  let silent ?(allow = allowed) ?(bot = "UBOT") ?(top_level = false)
      ?(root = None) ?(thread = []) text =
    P.silent ~allowed:allow ~bot ~lookup ~text ~top_level ~root ~thread
  in
  let* answer = silent ~top_level:true "<@UCLAUDE> help" in
  check_bool "top-level peer mention" true answer;
  lookups := [];
  let* answer = silent ~top_level:true "<@UCLAUDE> <@UBOT|clawq> help" in
  check_bool "explicit clawq mention" false answer;
  let* answer = silent ~allow:[] ~top_level:true "<@UCLAUDE> help" in
  check_bool "empty allowlist" false answer;
  let* answer = silent ~top_level:true "hello" in
  check_bool "no mentions" false answer;
  let* answer = silent "<@UCLAUDE> unreadable reply" in
  check_bool "unknown thread fails open" false answer;
  Alcotest.(check (list string)) "no lookups" [] !lookups;
  let* answer =
    silent
      ~root:(Some (obj [ ("text", str "<@UCLAUDE|Claude> help") ]))
      ~thread:[ peer () ]
      "answer"
  in
  check_bool "peer posted" true answer;
  Alcotest.(check (list string)) "thread evidence avoids lookup" [] !lookups;
  let* answer =
    silent ~root:(Some (obj [ ("text", str "<@UCLAUDE> help") ])) "answer"
  in
  check_bool "root mention" true answer;
  Alcotest.(check (list string)) "root lookup" [ "UCLAUDE" ] !lookups;
  Lwt.return_unit

let test_cache ~positive () =
  P.reset_cache ();
  let now = ref 100. and count = ref 0 in
  let call _ _ =
    incr count;
    Lwt.return
      (Ok
         (obj
            [
              ( "user",
                if positive then Test_slack_threads.peer_user
                else obj [ ("id", str "UCLAUDE"); ("is_bot", `Bool false) ] );
            ]))
  in
  let lookup = P.cached_lookup ~now:(fun () -> !now) ~call ~allowed in
  let* first = lookup "UCLAUDE" in
  check_bool "first result" positive first;
  now := 100. +. (if positive then 21600. else 600.) -. 1.;
  let* cached = lookup "UCLAUDE" in
  check_bool "cached result" positive cached;
  Alcotest.(check int) "cached before expiry" 1 !count;
  now := !now +. 1.;
  let* _ = lookup "UCLAUDE" in
  Alcotest.(check int) "refreshed at expiry" 2 !count;
  Lwt.return_unit

let test_failures () =
  let errors =
    [
      Slack_standing_order_slack.Transport;
      Slack_standing_order_slack.Slack_error "internal_error";
    ]
  in
  Lwt_list.iter_s
    (fun error ->
      P.reset_cache ();
      let count = ref 0 in
      let call _ _ =
        incr count;
        Lwt.return (Error error)
      in
      let lookup = P.cached_lookup ~call ~allowed in
      let* first = lookup "UCLAUDE" in
      let* second = lookup "UCLAUDE" in
      check_bool "failure fails open" false (first || second);
      Alcotest.(check int) "failures are retried" 2 !count;
      Lwt.return_unit)
    errors

let test_cache_config () =
  P.reset_cache ();
  let count = ref 0 in
  let call _ _ =
    incr count;
    Lwt.return (Ok (obj [ ("user", Test_slack_threads.peer_user) ]))
  in
  let* _ = P.cached_lookup ~call ~allowed "UCLAUDE" in
  let* empty = P.cached_lookup ~call ~allowed:[] "UCLAUDE" in
  check_bool "removed target never silences" false empty;
  Alcotest.(check int) "empty allowlist makes no call" 1 !count;
  let other : Slack_config_types.standing_order_target_config list =
    [ { app_id = "AOTHER"; label = "Other" } ]
  in
  let* changed = P.cached_lookup ~call ~allowed:other "UCLAUDE" in
  check_bool "changed allowlist cannot reuse positive" false changed;
  let* restored = P.cached_lookup ~call ~allowed "UCLAUDE" in
  check_bool "changed allowlist cannot reuse negative" true restored;
  Lwt.return_unit

let suite =
  [
    ("mentioned_users", `Quick, test_mentions);
    ("is_peer_message", `Quick, test_peer_message);
    ("peer decision and lookup shortcuts", `Quick, test_decision);
    ("positive cache lasts six hours", `Quick, test_cache ~positive:true);
    ("negative cache lasts ten minutes", `Quick, test_cache ~positive:false);
    ("lookup errors fail open and are never cached", `Quick, test_failures);
    ("cache follows allowlist changes", `Quick, test_cache_config);
  ]
  |> List.map (fun (name, speed, test) ->
      (name, speed, fun () -> Lwt_main.run (test ())))
