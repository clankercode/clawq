(** Tests for filter preview / structured explain (P20.M1.E2.T001). *)

module S = Github_route_store
module E = Github_event_envelope
module F = Github_route_filter
module En = Github_filter_enrichment
module P = Github_route_filter_preview
module Admin = Github_route_admin

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let room = S.Room "room-teams-1"
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let create ~db ?(id = "route-1") ?(enabled = true)
    ?(selector = S.Repo "acme/widget") ?(destination = room)
    ?(filter = S.default_filter) () =
  assert_ok
    (S.create ~db ~id ~destination ~selector ~filter ~enabled ~now:fixed_now ())

let make_envelope ?(event = "pull_request") ?(action = Some "opened")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(kind = Some E.Pull_request)
    ?(number = Some 42) ?(family = E.Lifecycle) ?(delivery_id = Some "deliv-1")
    ?(actor_login = Some "alice") ?(labels = [ "bug" ]) ?(draft = Some false)
    ?(base_ref = Some "main") ?(assignees = []) ?(milestone = None) () : E.t =
  {
    version = E.envelope_version;
    delivery_id;
    installation_id = Some 99;
    event;
    action;
    repo_full_name = repo;
    org;
    item_kind = kind;
    item_number = number;
    item_node_id = None;
    item_url = None;
    html_url = None;
    family;
    actor = { E.empty_actor with login = actor_login };
    before = None;
    after =
      Some
        {
          E.empty_safe_state with
          labels;
          draft;
          base_ref;
          assignees;
          milestone;
          state = Some "open";
        };
    transfer = None;
    received_at = Some "2024-01-01T00:00:00Z";
    event_at = None;
    head_sha = Some "abc123";
    unsupported = false;
    skip_reason = None;
  }

let find_pred name (p : P.preview) =
  List.find_opt (fun (pr : P.predicate_result) -> pr.name = name) p.predicates

let require_pred name p =
  match find_pred name p with
  | Some pr -> pr
  | None -> Alcotest.failf "missing predicate %s" name

let check_decision expected (p : P.preview) =
  Alcotest.(check string) "decision" expected p.decision

(* 1. Matched: enabled route, baseline + advanced PR labels pass *)
let test_matched () =
  with_db @@ fun db ->
  let filter =
    assert_ok
      (F.validate
         {
           F.default with
           pr =
             {
               F.empty_pr with
               labels = Some { op = `In; values = [ "bug"; "enhancement" ] };
               author = Some { op = `Eq; values = [ "alice" ] };
             };
         })
  in
  ignore (create ~db ~id:"rt_repo" ~selector:(S.Repo "acme/widget") ~filter ());
  let env = make_envelope () in
  let enrichment : En.enrichment =
    { paths = None; teams = None; reasons = []; complete = true }
  in
  let p = P.preview ~db ~destination:room ~envelope:env ~enrichment () in
  check_decision "Matched" p;
  Alcotest.(check (option string))
    "winning selector" (Some "repo:acme/widget") p.winning_selector;
  Alcotest.(check string) "destination" "room:room-teams-1" p.destination;
  Alcotest.(check bool) "no_fallthrough" true p.no_fallthrough;
  Alcotest.(check bool)
    "final_reason mentions accepted" true
    (Test_helpers.string_contains
       (String.lowercase_ascii p.final_reason)
       "accepted");
  let labels = require_pred "pr.labels" p in
  Alcotest.(check bool) "pr.labels pass" true labels.passed;
  let author = require_pred "pr.author" p in
  Alcotest.(check bool) "pr.author pass" true author.passed;
  Alcotest.(check bool)
    "enrichment not required" true
    (List.exists
       (fun s -> s = "enrichment:not_required" || s = "paths:not_demanded")
       p.enrichment_status);
  (* JSON is stable and redacted path works *)
  let j = P.to_json p in
  (match j with
  | `Assoc fields ->
      let keys = List.map fst fields in
      Alcotest.(check bool)
        "decision key present" true (List.mem "decision" keys);
      Alcotest.(check bool)
        "keys sorted" true
        (keys = List.sort String.compare keys)
  | _ -> Alcotest.fail "expected object");
  (* Admin surface *)
  let p2 =
    Admin.preview_filter ~db ~destination:room ~envelope:env ~enrichment ()
  in
  check_decision "Matched" p2

(* 2. Muted: advanced PR label filter rejects (no fallthrough to Org) *)
let test_muted_advanced_filter () =
  with_db @@ fun db ->
  let filter =
    assert_ok
      (F.validate
         {
           F.default with
           pr =
             {
               F.empty_pr with
               labels = Some { op = `In; values = [ "security" ] };
             };
         })
  in
  ignore (create ~db ~id:"rt_org" ~selector:(S.Org "acme") ());
  ignore (create ~db ~id:"rt_repo" ~selector:(S.Repo "acme/widget") ~filter ());
  let env = make_envelope ~labels:[ "bug" ] () in
  let p = P.preview ~db ~destination:room ~envelope:env () in
  check_decision "Muted" p;
  Alcotest.(check (option string))
    "winning is repo (not org)" (Some "repo:acme/widget") p.winning_selector;
  Alcotest.(check bool) "no_fallthrough" true p.no_fallthrough;
  let labels = require_pred "pr.labels" p in
  Alcotest.(check bool) "pr.labels fail" false labels.passed;
  Alcotest.(check bool)
    "final_reason mentions pr.labels" true
    (Test_helpers.string_contains p.final_reason "pr.labels");
  Alcotest.(check bool)
    "org is shadowed" true
    (List.exists (fun s -> Test_helpers.string_contains s "rt_org") p.shadowed)

(* 3. Muted: disabled most-specific route (no fallthrough) *)
let test_muted_disabled () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_org" ~selector:(S.Org "acme") ~enabled:true ());
  ignore
    (create ~db ~id:"rt_item"
       ~selector:
         (S.Item
            {
              repo_full_name = "acme/widget";
              kind = `Pull_request;
              number = 42;
            })
       ~enabled:false ());
  let env = make_envelope () in
  let p = P.preview ~db ~destination:room ~envelope:env () in
  check_decision "Muted" p;
  Alcotest.(check (option string))
    "winning item selector" (Some "item:acme/widget:pr:42") p.winning_selector;
  let en = require_pred "enabled" p in
  Alcotest.(check bool) "enabled fails" false en.passed;
  Alcotest.(check bool)
    "reason mentions disabled" true
    (Test_helpers.string_contains
       (String.lowercase_ascii p.final_reason)
       "disabled")

(* 4. No_route: nothing applies *)
let test_no_route () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_other" ~selector:(S.Repo "other/repo") ());
  let env = make_envelope ~repo:"acme/widget" () in
  let p = P.preview ~db ~destination:room ~envelope:env () in
  check_decision "No_route" p;
  Alcotest.(check (option string)) "no winner" None p.winning_selector;
  Alcotest.(check bool) "no_fallthrough false" false p.no_fallthrough;
  Alcotest.(check (list string)) "no shadowed" [] p.shadowed;
  Alcotest.(check bool)
    "reason mentions no route" true
    (Test_helpers.string_contains
       (String.lowercase_ascii p.final_reason)
       "no item")

(* 5. Shadowed listing: Item wins over Repo and Org *)
let test_shadowed_listing () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_org" ~selector:(S.Org "acme") ());
  ignore (create ~db ~id:"rt_repo" ~selector:(S.Repo "acme/widget") ());
  ignore
    (create ~db ~id:"rt_item"
       ~selector:
         (S.Item
            {
              repo_full_name = "acme/widget";
              kind = `Pull_request;
              number = 42;
            })
       ());
  let env = make_envelope () in
  let p = P.preview ~db ~destination:room ~envelope:env () in
  check_decision "Matched" p;
  Alcotest.(check (option string))
    "item wins" (Some "item:acme/widget:pr:42") p.winning_selector;
  Alcotest.(check int) "two shadowed" 2 (List.length p.shadowed);
  Alcotest.(check bool)
    "shadowed sorted" true
    (p.shadowed = List.sort String.compare p.shadowed);
  Alcotest.(check bool)
    "shadowed includes org" true
    (List.exists (fun s -> Test_helpers.string_contains s "rt_org") p.shadowed);
  Alcotest.(check bool)
    "shadowed includes repo" true
    (List.exists (fun s -> Test_helpers.string_contains s "rt_repo") p.shadowed);
  let rule = require_pred "rule.no_fallthrough" p in
  Alcotest.(check bool) "rule passes" true rule.passed;
  Alcotest.(check bool)
    "rule detail mentions shadowed" true
    (Test_helpers.string_contains
       (String.lowercase_ascii rule.detail)
       "shadowed")

(* 6. Enrichment status: demanded paths missing → mute + status *)
let test_enrichment_status_missing_paths () =
  with_db @@ fun db ->
  let filter =
    assert_ok
      (F.validate
         {
           F.default with
           pr =
             {
               F.empty_pr with
               changed_path = Some { op = `Glob; values = [ "src/**" ] };
             };
         })
  in
  ignore (create ~db ~id:"rt_repo" ~selector:(S.Repo "acme/widget") ~filter ());
  let env = make_envelope () in
  (* No enrichment provided while paths demanded *)
  let p = P.preview ~db ~destination:room ~envelope:env () in
  check_decision "Muted" p;
  Alcotest.(check bool)
    "paths missing status" true
    (List.mem "paths:missing" p.enrichment_status);
  Alcotest.(check bool)
    "enrichment not_provided" true
    (List.mem "enrichment:not_provided" p.enrichment_status);
  let path_pred = require_pred "pr.changed_path" p in
  Alcotest.(check bool) "changed_path fails closed" false path_pred.passed;
  (* With successful enrichment, accept *)
  let enrichment : En.enrichment =
    {
      paths = Some (Ok [ "src/lib/a.ml"; "README.md" ]);
      teams = None;
      reasons = [];
      complete = true;
    }
  in
  let p2 = P.preview ~db ~destination:room ~envelope:env ~enrichment () in
  check_decision "Matched" p2;
  Alcotest.(check bool)
    "paths ok status" true
    (List.exists
       (fun s -> String.starts_with ~prefix:"paths:ok:" s)
       p2.enrichment_status);
  let path_pred2 = require_pred "pr.changed_path" p2 in
  Alcotest.(check bool) "changed_path passes" true path_pred2.passed;
  (* Rate-limited enrichment fails closed *)
  let enrichment_rl : En.enrichment =
    {
      paths = Some (Error "rate_limited");
      teams = None;
      reasons = [ "rate_limited" ];
      complete = false;
    }
  in
  let p3 =
    P.preview ~db ~destination:room ~envelope:env ~enrichment:enrichment_rl ()
  in
  check_decision "Muted" p3;
  Alcotest.(check bool)
    "paths error status" true
    (List.mem "paths:error:rate_limited" p3.enrichment_status)

(* 7. Issue advanced mute + format_lines stable *)
let test_issue_muted_and_format () =
  with_db @@ fun db ->
  let filter =
    assert_ok
      (F.validate
         {
           F.default with
           issue =
             {
               F.empty_issue with
               assignee = Some { op = `In; values = [ "bob" ] };
               milestone = Some { op = `Eq; values = [ "v1" ] };
             };
         })
  in
  ignore (create ~db ~id:"rt_repo" ~selector:(S.Repo "acme/widget") ~filter ());
  let env =
    make_envelope ~event:"issues" ~action:(Some "opened") ~kind:(Some E.Issue)
      ~number:(Some 7) ~labels:[ "bug" ] ~assignees:[ "alice" ]
      ~milestone:(Some "v2") ()
  in
  let p = P.preview ~db ~destination:room ~envelope:env () in
  check_decision "Muted" p;
  let asg = require_pred "issue.assignee" p in
  Alcotest.(check bool) "assignee fails" false asg.passed;
  let lines = P.format_lines p in
  Alcotest.(check bool)
    "has decision line" true
    (List.exists (fun l -> String.starts_with ~prefix:"decision=" l) lines);
  Alcotest.(check bool)
    "has predicates section" true
    (List.mem "predicates:" lines)

let suite =
  [
    ("matched with advanced pr predicates", `Quick, test_matched);
    ("muted advanced filter no fallthrough", `Quick, test_muted_advanced_filter);
    ("muted disabled no fallthrough", `Quick, test_muted_disabled);
    ("no_route", `Quick, test_no_route);
    ("shadowed listing item>repo>org", `Quick, test_shadowed_listing);
    ( "enrichment status missing and rate_limited",
      `Quick,
      test_enrichment_status_missing_paths );
    ("issue muted and format_lines", `Quick, test_issue_muted_and_format);
  ]
