(* B773: canonical GitHub trigger tests — /clawq, leading at-mention, and
   assignment normalize into one parser; negative cases must not trigger. *)

let issue_comment_json body =
  Printf.sprintf
    {|{"action":"created",
       "repository":{"full_name":"o/r","owner":{"login":"o"},"name":"r"},
       "issue":{"number":7,"title":"Widget breaks","html_url":"https://x"},
       "comment":{"id":991,"body":%s,"user":{"login":"alice"},"html_url":"https://x/c"},
       "sender":{"login":"alice"}}|}
    (Yojson.Safe.to_string (`String body))

let parse_comment body =
  Github_webhook.parse_event ~event_type:"issue_comment"
    ~body:(issue_comment_json body)

let extract ?trigger_login body =
  Github_webhook.extract_trigger ?trigger_login ~event:(parse_comment body)
    ~pr_files:[] ()

let msg_of = function
  | Some (user_message, _preamble) -> Some user_message
  | None -> None

let test_mention_leading_triggers () =
  (match
     msg_of (extract ~trigger_login:"clawq-bot" "@clawq-bot fix the widget")
   with
  | Some msg -> Alcotest.(check string) "prefix stripped" "fix the widget" msg
  | None -> Alcotest.fail "leading mention must trigger");
  (* case-insensitive + colon form *)
  (match msg_of (extract ~trigger_login:"clawq-bot" "@Clawq-Bot: fix it") with
  | Some msg -> Alcotest.(check string) "colon form" "fix it" msg
  | None -> Alcotest.fail "case-insensitive mention must trigger");
  (* following lines kept *)
  match
    msg_of
      (extract ~trigger_login:"clawq-bot" "@clawq-bot do this\nand also that")
  with
  | Some msg ->
      Alcotest.(check string) "multiline kept" "do this\nand also that" msg
  | None -> Alcotest.fail "multiline mention must trigger"

let test_mention_negative_cases () =
  let no_trigger label body =
    Alcotest.(check bool)
      label true
      (extract ~trigger_login:"clawq-bot" body = None)
  in
  no_trigger "quoted mention ignored"
    "> @clawq-bot fix it\nreplying to the above";
  no_trigger "mid-text mention ignored"
    "someone should ask @clawq-bot about this";
  no_trigger "longer login not matched" "@clawq-bot2 fix it";
  no_trigger "unaddressed comment ignored" "just a normal comment";
  Alcotest.(check bool)
    "no trigger_login configured" true
    (extract "@clawq-bot fix it" = None)

let test_clawq_still_works_via_trigger () =
  match msg_of (extract ~trigger_login:"clawq-bot" "/clawq explain this") with
  | Some msg -> Alcotest.(check string) "legacy /clawq" "explain this" msg
  | None -> Alcotest.fail "/clawq must keep working through extract_trigger"

let assigned_json ~assignee ~action =
  Printf.sprintf
    {|{"action":%s,
       "repository":{"full_name":"o/r","owner":{"login":"o"},"name":"r"},
       "issue":{"number":9,"title":"Do the thing","body":"details here","html_url":"https://x/9"},
       "assignee":{"login":%s},
       "sender":{"login":"maintainer"}}|}
    (Yojson.Safe.to_string (`String action))
    (Yojson.Safe.to_string (`String assignee))

let test_assignment_parses () =
  match
    Github_webhook.parse_event ~event_type:"issues"
      ~body:(assigned_json ~assignee:"clawq-bot" ~action:"assigned")
  with
  | Github_webhook.IssueAssigned a ->
      Alcotest.(check int) "issue number" 9 a.issue_number;
      Alcotest.(check string) "assignee" "clawq-bot" a.assignee;
      Alcotest.(check string) "actor" "maintainer" a.actor;
      Alcotest.(check string)
        "stable session key" "github:o/r:issue:9"
        (Github_webhook.session_key (Github_webhook.IssueAssigned a));
      Alcotest.(check string)
        "event type" "issues"
        (Github_webhook.event_type_string (Github_webhook.IssueAssigned a))
  | _ -> Alcotest.fail "issues.assigned must parse to IssueAssigned"

let test_non_assigned_actions_ignored () =
  List.iter
    (fun action ->
      match
        Github_webhook.parse_event ~event_type:"issues"
          ~body:(assigned_json ~assignee:"clawq-bot" ~action)
      with
      | Github_webhook.Ignored -> ()
      | _ -> Alcotest.failf "issues.%s must be ignored" action)
    [ "unassigned"; "opened"; "edited" ]

let test_labeled_parses_as_fallback_trigger () =
  let body =
    {|{"action":"labeled",
       "repository":{"full_name":"o/r","owner":{"login":"o"},"name":"r"},
       "issue":{"number":9,"title":"Do the thing","body":"d","html_url":"https://x/9"},
       "label":{"name":"clawq"},
       "sender":{"login":"maintainer"}}|}
  in
  match Github_webhook.parse_event ~event_type:"issues" ~body with
  | Github_webhook.IssueAssigned a ->
      Alcotest.(check (option string)) "label captured" (Some "clawq") a.label;
      Alcotest.(check string) "no assignee for labels" "" a.assignee
  | _ -> Alcotest.fail "issues.labeled must parse for the label fallback"

let suite =
  [
    Alcotest.test_case "leading at-mention triggers and strips prefix" `Quick
      test_mention_leading_triggers;
    Alcotest.test_case "quoted/mid-text/foreign mentions do not trigger" `Quick
      test_mention_negative_cases;
    Alcotest.test_case "/clawq unchanged through canonical parser" `Quick
      test_clawq_still_works_via_trigger;
    Alcotest.test_case "issues.assigned parses with stable session" `Quick
      test_assignment_parses;
    Alcotest.test_case "other issues actions are ignored" `Quick
      test_non_assigned_actions_ignored;
    Alcotest.test_case "labeled action parses as fallback trigger" `Quick
      test_labeled_parses_as_fallback_trigger;
  ]
