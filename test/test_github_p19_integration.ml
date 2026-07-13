(** P19 cross-family integration and failure-path coverage (P19.M4.E3.T001).

    Integration-style suite (in-memory SQLite) covering Teams vs generic
    fallback, route migration collisions, every action family, access
    revocation, stale confirmations, App/PAT selection, pilot default-off /
    enable / backout, receipt↔webhook reconciliation, delivery failure / restart
    / dead-letter + catch-up, and minimal-build module surface notes.

    Fixtures stay compatible with a later P21 state matrix: all high-risk paths
    run with [user_auth_available=false] and never claim user attribution exists
    in P19. Canonical contract:
    docs/plans/2026-07-12-github-item-room-routing.md. *)

module Store = Github_route_store
module Migrate = Github_route_migrate
module Auth = Github_auth_selection
module Scope = Github_app_installation_scope
module Plain = Github_plain_delivery_render
module Intent = Github_delivery_intent
module Proj = Github_item_projection
module Env = Github_event_envelope
module Outbox = Github_delivery_outbox
module Deliv_reconcile = Github_delivery_reconcile
module Action_reconcile = Github_action_reconcile
module Journal = Github_room_event_journal
module History = Github_event_history_index
module Collab = Github_collab_actions
module Review = Github_pr_review_actions
module Merge = Github_merge_action
module Issue = Github_issue_actions
module Workflow = Github_workflow_dispatch
module Code = Github_code_change_action
module Workflow_shared = Github_action_workflow
module Ground = Github_collab_grounding
module Context = Github_item_context_resolve
module Route_match = Github_route_match

let fixed_now = 1_700_000_000.0
let base_revision = "rev-config-1"
let room_id = "room-teams-1"
let room = Store.Room room_id
let item_key = "pr:acme/widget:42"
let collab_item_key = "item:acme/widget:pr:42"
let head_sha = "abc123def4567890abcdef1234567890abcdef12"
let repo = "acme/widget"

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let contains hay needle =
  let hay = String.lowercase_ascii hay in
  let needle = String.lowercase_ascii needle in
  let n = String.length needle in
  let h = String.length hay in
  if n = 0 then true
  else if n > h then false
  else
    let rec loop i =
      if i > h - n then false
      else if String.sub hay i n = needle then true
      else loop (i + 1)
    in
    loop 0

let with_db ~schemas f =
  let db = Sqlite3.db_open ":memory:" in
  List.iter (fun ensure -> ensure db) schemas;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let with_action_db f =
  with_db
    ~schemas:
      [
        Store.ensure_schema;
        Setup_plan_apply.init_schema;
        History.ensure_schema;
        Outbox.ensure_schema;
        Action_reconcile.ensure_schema;
      ]
    f

let with_delivery_db f =
  with_db
    ~schemas:
      [
        History.ensure_schema;
        Journal.ensure_schema;
        Proj.ensure_schema;
        Outbox.ensure_schema;
        Action_reconcile.ensure_schema;
      ]
    f

let with_route_db f = with_db ~schemas:[ Store.ensure_schema ] f

let caps ?(reply = false) ?(label = false) ?(assign = false) ?(review = false)
    ?(merge = false) ?(close = false) ?(extra = []) () : Store.capability_policy
    =
  {
    allow_reply = reply;
    allow_label = label;
    allow_assign = assign;
    allow_review = review;
    allow_merge = merge;
    allow_close = close;
    extra;
  }

let make_route ~id ~policy : Store.t =
  {
    id;
    destination = room;
    selector = Store.Repo repo;
    filter = Store.default_filter;
    comment_mode = Store.default_comment_mode;
    capability_policy = policy;
    enabled = true;
    revision = "1";
    managed_bundle_id = None;
    managed_feature_id = None;
    provenance =
      {
        created_by = Some "test";
        created_via = Some "integration";
        setup_plan_id = None;
        notes = None;
      };
    created_at = "2024-01-01T00:00:00Z";
    updated_at = "2024-01-01T00:00:00Z";
  }

let sample_projection ?(item_key = item_key) ?(title = Some "Add feature")
    ?(state = Some "open") ?(revision = 1) () : Proj.projection =
  {
    room_id;
    item_key;
    title;
    state;
    draft = Some false;
    merged = None;
    labels = [ "enhancement" ];
    assignees = [ "alice" ];
    head_sha = Some "abc123";
    html_url = Some "https://github.com/acme/widget/pull/42";
    last_event_at = Some "2024-01-01T00:00:00Z";
    last_family = Some Env.Lifecycle;
    comment_count = 0;
    revision;
    card_kind = Proj.Lifecycle;
  }

let sample_intent ?(id = "ghdi_int_1") ?(item_key = item_key) ?(now = fixed_now)
    () : Intent.intent =
  let proj = sample_projection ~item_key () in
  let intent = Intent.of_projection ~room_id ~projection:proj ~now () in
  { intent with id }

let sample_app ?(app_id = 42) ?(installation_id = 1001) () :
    Runtime_config.github_app_config =
  {
    app_id;
    private_key_path = "/tmp/github-app.pem";
    webhook_secret = "whsec";
    installations = [ { installation_id; repos = [ "acme/widget" ] } ];
  }

let sample_scope ?(installation_id = 1001) ?(login = "acme")
    ?(status = Scope.Active) () : Scope.t =
  Scope.with_revision
    {
      installation_id;
      app_id = Some 42;
      account = { login; id = 99; account_type = "Organization" };
      selection = Scope.All_repos;
      repositories = [];
      revoked_repositories = [];
      permissions = [ ("issues", "write"); ("metadata", "read") ];
      status;
      revision = "";
      updated_at = Time_util.iso8601_utc ~t:fixed_now ();
    }

let legacy ?(id = "leg-1") ?(room = room_id) ?(repo = "Acme/Widget") ?(pr = 42)
    ?(enabled = true) ?(events = [ "pull_request" ])
    ?(created_at = Some "2024-01-01T00:00:00Z") () : Migrate.legacy_subscription
    =
  {
    id;
    room_id = room;
    repo_full_name = repo;
    pr_number = pr;
    enabled;
    events;
    profile_id = None;
    backlink_ref = None;
    audit_ref = None;
    created_at;
  }

let make_envelope ?(event = "pull_request") ?(action = Some "closed")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(kind = Some Env.Pull_request)
    ?(number = Some 42) ?(family = Env.Lifecycle)
    ?(delivery_id = Some "deliv-self-1") ?(actor_login = Some "clawq-bot")
    ?(actor_type = Some "Bot") ?(title = Some "Add feature")
    ?(state = Some "closed") ?(draft = Some false) ?(merged = Some true)
    ?(labels = [ "enhancement" ]) ?(assignees = []) ?(head_sha = Some "abc123")
    ?(html_url = Some "https://github.com/acme/widget/pull/42")
    ?(event_at = Some "2024-01-01T00:00:00Z") () : Env.t =
  {
    version = Env.envelope_version;
    delivery_id;
    installation_id = Some 99;
    event;
    action;
    repo_full_name = repo;
    org;
    item_kind = kind;
    item_number = number;
    item_node_id = Some "PR_kwDOABC";
    item_url = Some "https://api.github.com/repos/acme/widget/pulls/42";
    html_url;
    family;
    actor = { Env.empty_actor with login = actor_login; type_ = actor_type };
    before =
      Some
        {
          Env.empty_safe_state with
          title;
          state = Some "open";
          draft;
          merged = Some false;
          labels;
          assignees;
          head_sha;
        };
    after =
      Some
        {
          Env.empty_safe_state with
          title;
          state;
          draft;
          merged;
          labels;
          assignees;
          head_sha;
        };
    transfer = None;
    received_at = Some "2024-01-01T00:00:00Z";
    event_at;
    head_sha;
    unsupported = false;
    skip_reason = None;
  }

let outbox_status_name = function
  | Outbox.Pending -> "pending"
  | Outbox.In_flight -> "in_flight"
  | Outbox.Succeeded -> "succeeded"
  | Outbox.Dead_letter -> "dead_letter"
  | Outbox.Superseded -> "superseded"

let count_outbox ~db =
  let sql = {|SELECT COUNT(*) FROM github_delivery_outbox|} in
  let stmt = Sqlite3.prepare db sql in
  let n =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.INT i -> Int64.to_int i
        | _ -> 0)
    | _ -> 0
  in
  ignore (Sqlite3.finalize stmt);
  n

let count_correlations ~db ~status =
  let sql =
    {|SELECT COUNT(*) FROM github_action_correlations WHERE status = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT status));
  let n =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.INT i -> Int64.to_int i
        | _ -> 0)
    | _ -> 0
  in
  ignore (Sqlite3.finalize stmt);
  n

let seed_lifecycle_projection ~db ~room_id ~number ~title ~state ~delivery_id
    ~now =
  let env =
    make_envelope ~action:(Some "opened") ~number:(Some number)
      ~title:(Some title) ~state:(Some state) ~merged:None
      ~delivery_id:(Some delivery_id)
      ~html_url:
        (Some (Printf.sprintf "https://github.com/acme/widget/pull/%d" number))
      ~actor_login:(Some "alice") ~actor_type:(Some "User") ()
  in
  let entry = assert_ok (Journal.append ~db ~room_id ~envelope:env ~now ()) in
  assert_ok (Proj.reduce_entry ~db ~entry ())

(* --------------------------------------------------------------------------- *)
(* 1. Teams Adaptive Card vs generic plain/editless fallback                   *)
(* --------------------------------------------------------------------------- *)

let test_teams_adaptive_vs_plain_fallback () =
  let intent =
    Intent.of_projection ~room_id ~projection:(sample_projection ())
      ~now:fixed_now ()
  in
  (* Teams-capable: adaptive cards preferred. *)
  let teams =
    Plain.select_renderer ~supports_adaptive_cards:true ~supports_edit:true
      intent
  in
  Alcotest.(check bool)
    "Teams selects Adaptive_card" true
    (match teams with `Adaptive_card -> true | _ -> false);
  (* Generic Connector with edit → plain. *)
  let plain =
    Plain.select_renderer ~supports_adaptive_cards:false ~supports_edit:true
      intent
  in
  Alcotest.(check bool)
    "generic edit selects Plain" true
    (match plain with `Plain -> true | _ -> false);
  (* Editless fallback (Direct Session / no edit). *)
  let editless =
    Plain.select_renderer ~supports_adaptive_cards:false ~supports_edit:false
      intent
  in
  Alcotest.(check bool)
    "no-edit selects Editless_plain" true
    (match editless with `Editless_plain -> true | _ -> false);
  let plain_text = Plain.render_plain intent in
  let editless_text = Plain.render_editless intent in
  Alcotest.(check bool) "plain non-empty" true (String.trim plain_text <> "");
  Alcotest.(check bool)
    "editless longer/equal" true
    (String.length editless_text >= String.length plain_text);
  Alcotest.(check bool)
    "editless notes continuity" true
    (contains editless_text "full replacement"
    || contains editless_text "weaker continuity"
    || contains editless_text "no in-place edit")

(* --------------------------------------------------------------------------- *)
(* 2. Route migrate collision Prefer_existing                                  *)
(* --------------------------------------------------------------------------- *)

let test_route_migrate_prefer_existing_collision () =
  with_route_db @@ fun db ->
  let dest = Store.Room room_id in
  let sel =
    Store.Item
      { repo_full_name = "Acme/Widget"; kind = `Pull_request; number = 42 }
  in
  let existing =
    assert_ok
      (Store.create ~db ~id:"rt_preexisting" ~destination:dest ~selector:sel
         ~filter:
           {
             Store.include_events = [ "issues" ];
             exclude_events = [];
             include_repos = [];
             exclude_repos = [];
           }
         ~now:fixed_now ())
  in
  let leg = legacy ~id:"leg-collide" ~events:[ "pull_request" ] () in
  let report =
    assert_ok
      (Migrate.migrate_subscriptions ~db ~legacy:[ leg ]
         ~policy:Migrate.Prefer_existing_route ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check int) "one active route" 1 report.active_routes;
  (match report.resolutions with
  | [ (_, Migrate.Skipped { winner_route_id = Some wid; reason }) ] ->
      Alcotest.(check string) "kept existing" existing.id wid;
      Alcotest.(check bool)
        "reason mentions prefer" true (contains reason "prefer")
  | _ -> Alcotest.fail "expected Skipped Prefer_existing");
  (* Idempotent re-run: still one active, no duplicate. *)
  let report2 =
    assert_ok
      (Migrate.migrate_subscriptions ~db ~legacy:[ leg ]
         ~policy:Migrate.Prefer_existing_route ~now:(fixed_now +. 2.) ())
  in
  Alcotest.(check int) "still one active" 1 report2.active_routes;
  match Store.find_active ~db ~destination:dest ~selector:sel with
  | Ok (Some a) ->
      Alcotest.(check string) "winner still preexisting" "rt_preexisting" a.id;
      Alcotest.(check (list string))
        "filter untouched" [ "issues" ] a.filter.include_events
  | Ok None -> Alcotest.fail "missing active"
  | Error e -> Alcotest.fail e

(* --------------------------------------------------------------------------- *)
(* 3. Action families: collab success + high-risk pilot-off denies             *)
(* --------------------------------------------------------------------------- *)

let deny_mentions_pilot msg =
  contains msg "pilot"
  && (contains msg "production"
     || contains msg "not available"
     || contains msg "default")

let test_action_families_collab_and_pilot_denies () =
  with_action_db @@ fun db ->
  (* Collab plan succeeds (ordinary capability, no pilot). *)
  let collab_route = make_route ~id:"rt_collab" ~policy:(caps ~reply:true ()) in
  let comment =
    Collab.Comment
      { item_key = collab_item_key; body = "Looks good after CI is green." }
  in
  let plan =
    assert_ok
      (Workflow_shared.preview ~db ~principal ~room_id
         ~action:(Workflow_shared.Collab comment) ~base_revision
         ~route:collab_route ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "collab is github action plan" true
    (Workflow_shared.is_github_action_plan plan);
  Alcotest.(check string)
    "collab label" "collab"
    (Workflow_shared.action_kind_label (Workflow_shared.Collab comment));
  (match
     assert_ok
       (Workflow_shared.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
          ~principal ~current_base_revision:base_revision ~now:fixed_now ())
   with
  | Setup_plan_apply.Applied { first_time = true; receipt_id } -> (
      Alcotest.(check bool)
        "receipt non-empty" true
        (String.length receipt_id > 0);
      (* Idempotent re-apply. *)
      match
        assert_ok
          (Workflow_shared.apply_confirmed ~db ~plan_id:plan.id
             ~digest:plan.digest ~principal ~current_base_revision:base_revision
             ~now:(fixed_now +. 1.) ())
      with
      | Setup_plan_apply.Applied { first_time = false; receipt_id = r2 } ->
          Alcotest.(check string) "same receipt" receipt_id r2
      | Setup_plan_apply.Applied { first_time = true; _ } ->
          Alcotest.fail "retry must be idempotent"
      | Setup_plan_apply.Rejected { message; _ } ->
          Alcotest.fail ("retry rejected: " ^ message))
  | Setup_plan_apply.Applied { first_time = false; _ } ->
      Alcotest.fail "first apply should be first_time"
  | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message);

  (* Review submit denied when pilot off (default gate). *)
  let review_route =
    make_route ~id:"rt_review" ~policy:(caps ~review:true ())
  in
  let review_req =
    {
      Review.item_key = collab_item_key;
      kind = Review.Approve;
      head_sha;
      body = Some "LGTM";
      actor_login = Some "alice";
    }
  in
  (match
     Workflow_shared.preview ~db ~principal ~room_id
       ~action:(Workflow_shared.Submit_review review_req) ~base_revision
       ~route:review_route ~pilot:Review.default_pilot_gate
       ~user_auth_available:false ~now:fixed_now ()
   with
  | Ok _ -> Alcotest.fail "review pilot-off should deny"
  | Error msg ->
      Alcotest.(check bool)
        "review mentions pilot" true (deny_mentions_pilot msg);
      Alcotest.(check bool)
        "review no App/PAT fallback claim" true
        (contains msg "p21" || contains msg "no app/pat"
        || contains msg "user_required"
        || contains msg "attribution"));

  (* Merge denied when pilot off. *)
  let merge_route = make_route ~id:"rt_merge" ~policy:(caps ~merge:true ()) in
  let merge_req : Merge.merge_request =
    {
      item_key = collab_item_key;
      method_ = Merge.Squash;
      head_sha;
      commit_title = Some "Merge PR #42";
      commit_message = None;
    }
  in
  let merge_policy : Merge.live_policy =
    {
      head_sha;
      is_draft = false;
      mergeable = true;
      required_checks_ok = true;
      required_reviews_ok = true;
      branch_policy_ok = true;
      allowed_methods = [ Merge.Merge; Merge.Squash; Merge.Rebase ];
      actor_mode = Merge.App;
      authority_ok = true;
    }
  in
  (match
     Workflow_shared.preview ~db ~principal ~room_id
       ~action:
         (Workflow_shared.Merge { req = merge_req; policy = merge_policy })
       ~base_revision ~route:merge_route ~merge_pilot:Merge.default_pilot_gate
       ~user_auth_available:false ~now:fixed_now ()
   with
  | Ok _ -> Alcotest.fail "merge pilot-off should deny"
  | Error msg ->
      Alcotest.(check bool)
        "merge mentions pilot" true (deny_mentions_pilot msg));

  (* Issue create denied when pilot off. *)
  let issue_route =
    make_route ~id:"rt_issue"
      ~policy:(caps ~close:true ~extra:[ ("allow_create", true) ] ())
  in
  let issue_action =
    Issue.Create
      {
        repo_full_name = repo;
        title = "Flaky test";
        body = Some "repro steps";
        labels = [ "bug" ];
      }
  in
  (match
     Workflow_shared.preview ~db ~principal ~room_id
       ~action:(Workflow_shared.Issue issue_action) ~base_revision
       ~route:issue_route ~issue_pilot:Issue.default_pilot_gate
       ~user_auth_available:false ~now:fixed_now ()
   with
  | Ok _ -> Alcotest.fail "issue pilot-off should deny"
  | Error msg ->
      Alcotest.(check bool)
        "issue mentions pilot" true (deny_mentions_pilot msg));

  (* Workflow dispatch denied when pilot off. *)
  let wd_route =
    make_route ~id:"rt_wd"
      ~policy:(caps ~extra:[ (Workflow.capability_key, true) ] ())
  in
  let wd_req : Workflow.request =
    {
      repo_full_name = repo;
      workflow_id = "deploy.yml";
      ref_ = "main";
      inputs = [ ("environment", "staging") ];
      item_key = Some collab_item_key;
      allowed_input_names = Some [ "environment" ];
    }
  in
  (match
     Workflow_shared.preview ~db ~principal ~room_id
       ~action:(Workflow_shared.Workflow_dispatch wd_req) ~base_revision
       ~route:wd_route ~workflow_pilot:Workflow.default_pilot_gate
       ~user_auth_available:false ~now:fixed_now ()
   with
  | Ok _ -> Alcotest.fail "workflow pilot-off should deny"
  | Error msg ->
      Alcotest.(check bool)
        "workflow mentions pilot" true (deny_mentions_pilot msg));

  (* Code-change denied when pilot off (authorize path; not in shared workflow). *)
  let code_route =
    make_route ~id:"rt_code"
      ~policy:(caps ~extra:[ (Code.capability_key, true) ] ())
  in
  let code_req : Code.code_work_request =
    {
      repo_full_name = repo;
      base_branch = "main";
      scope = "fix SSO redirect";
      runner = "codex";
      output_authority = "room:" ^ room_id;
      branch_prefix = Code.default_branch_prefix;
      head_branch = Some "clawq/wi-1-fix";
      item_key = Some collab_item_key;
      related_issue = None;
    }
  in
  match
    Code.authorize_code_work ~route:(Some code_route)
      ~pilot:Code.default_pilot_gate ~user_auth_available:false ~req:code_req
      ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "code_change pilot-off should deny"
  | Error msg ->
      Alcotest.(check bool)
        "code_change mentions pilot" true (deny_mentions_pilot msg)

(* --------------------------------------------------------------------------- *)
(* 4. PAT org deny                                                             *)
(* --------------------------------------------------------------------------- *)

let test_pat_org_deny () =
  let auth = Auth.snapshot_of_parts ~pat:"ghp_test_token_value" () in
  let sel = Auth.select_for_org_route ~auth ~org:"acme" () in
  Alcotest.(check string)
    "reason" "rejected_org_requires_app"
    (Auth.selection_reason_to_string sel.reason);
  Alcotest.(check bool) "chosen none" true (sel.chosen = `None);
  Alcotest.(check bool) "explains App" true (contains sel.explanation "app");
  Alcotest.(check bool) "explains PAT" true (contains sel.explanation "pat");
  Alcotest.(check bool)
    "cannot claim org with PAT" false
    (Auth.can_claim_org_scope ~auth ~installation:(Some (sample_scope ())))

(* --------------------------------------------------------------------------- *)
(* 5. Stale plan apply reject                                                  *)
(* --------------------------------------------------------------------------- *)

let test_stale_plan_apply_reject () =
  with_action_db @@ fun db ->
  let route = make_route ~id:"rt_stale" ~policy:(caps ~reply:true ()) in
  let plan =
    assert_ok
      (Workflow_shared.preview ~db ~principal ~room_id
         ~action:
           (Workflow_shared.Collab
              (Collab.Comment
                 { item_key = collab_item_key; body = "stale check" }))
         ~base_revision ~route ~now:fixed_now ())
  in
  match
    assert_ok
      (Workflow_shared.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal ~current_base_revision:"rev-stale-other" ~now:fixed_now ())
  with
  | Setup_plan_apply.Rejected { reason; message } ->
      Alcotest.(check string)
        "stale_revision" "stale_revision"
        (Setup_plan_apply.string_of_reject_reason reason);
      Alcotest.(check bool)
        "message actionable" true
        (contains message "revision" || contains message "stale")
  | Setup_plan_apply.Applied _ -> Alcotest.fail "expected stale revision reject"

(* --------------------------------------------------------------------------- *)
(* 6. Reconcile webhook closes once; no outbox                                 *)
(* --------------------------------------------------------------------------- *)

let test_reconcile_webhook_closes_once_no_outbox () =
  with_delivery_db @@ fun db ->
  let corr : Action_reconcile.correlation =
    {
      room_id;
      item_key = Some item_key;
      action = "merge";
      plan_id = Some "plan-merge-1";
      receipt_id = Some "receipt-merge-1";
      delivery_id = None;
      github_ref = Some "abc123";
      actor_mode = "pilot";
    }
  in
  assert_ok
    (Action_reconcile.record_correlation ~db ~correlation:corr ~now:fixed_now ());
  Alcotest.(check int) "one open" 1 (count_correlations ~db ~status:"open");
  let env = make_envelope ~action:(Some "closed") ~merged:(Some true) () in
  Alcotest.(check string)
    "item key" item_key
    (Route_match.canonical_item_key env);
  let outbox_before = count_outbox ~db in
  let r1 =
    Action_reconcile.reconcile_webhook ~db ~room_id ~envelope:env
      ~now:(fixed_now +. 1.) ()
  in
  (match r1 with
  | Action_reconcile.Closed { first_time = true; correlation = c } ->
      Alcotest.(check string) "action" "merge" c.action;
      Alcotest.(check (option string))
        "receipt" (Some "receipt-merge-1") c.receipt_id
  | _ -> Alcotest.fail "expected Closed first_time");
  Alcotest.(check int) "no open left" 0 (count_correlations ~db ~status:"open");
  Alcotest.(check int) "one closed" 1 (count_correlations ~db ~status:"closed");
  Alcotest.(check int) "outbox unchanged" outbox_before (count_outbox ~db);
  (* Second matching webhook → Already_closed; still no outbox growth. *)
  let r2 =
    Action_reconcile.reconcile_webhook ~db ~room_id ~envelope:env
      ~now:(fixed_now +. 2.) ()
  in
  (match r2 with
  | Action_reconcile.Already_closed -> ()
  | Action_reconcile.Closed { first_time = false; _ } -> ()
  | other ->
      Alcotest.fail
        (match other with
        | Action_reconcile.Closed _ -> "closed again unexpectedly"
        | Action_reconcile.No_matching_receipt -> "no_matching_receipt"
        | Action_reconcile.Ignored_human_event -> "ignored_human"
        | Action_reconcile.Already_closed -> "already_closed"));
  Alcotest.(check int) "outbox still unchanged" outbox_before (count_outbox ~db);
  Alcotest.(check int)
    "still one closed correlation" 1
    (count_correlations ~db ~status:"closed")

(* --------------------------------------------------------------------------- *)
(* 7. Delivery 24h dead letter + catch-up one per item + restart reclaim       *)
(* --------------------------------------------------------------------------- *)

let test_delivery_dead_letter_catchup_restart () =
  with_delivery_db @@ fun db ->
  let room = "room-delivery" in
  let proj =
    seed_lifecycle_projection ~db ~room_id:room ~number:7 ~title:"Catch me"
      ~state:"open" ~delivery_id:"open-7" ~now:fixed_now
  in
  let item = proj.item_key in
  (* Multiple historical pending intents for same item. *)
  List.iter
    (fun (id, rev) ->
      let proj' = sample_projection ~item_key:item ~revision:rev () in
      let intent =
        let i =
          Intent.of_projection ~room_id:room ~projection:proj' ~now:fixed_now ()
        in
        { i with id }
      in
      ignore
        (assert_ok
           (Outbox.enqueue ~db ~room_id:room ~item_key:item ~intent
              ~now:fixed_now ())))
    [ ("ghdi_hist_a", 1); ("ghdi_hist_b", 2) ];
  Alcotest.(check int)
    "two open before catchup" 2
    (assert_ok (Outbox.count_open_for_item ~db ~room_id:room ~item_key:item));

  (* Catch-up: collapse to one current-state intent per item. *)
  let enqueued =
    assert_ok
      (Deliv_reconcile.reconcile_room ~db ~room_id:room ~now:fixed_now ())
  in
  Alcotest.(check int) "one catchup enqueued" 1 enqueued;
  Alcotest.(check int)
    "one open after catchup" 1
    (assert_ok (Outbox.count_open_for_item ~db ~room_id:room ~item_key:item));

  (* Restart recovery: claim, leave in-flight, reclaim. *)
  let claimed = assert_ok (Outbox.claim_due ~db ~now:fixed_now ~limit:10 ()) in
  Alcotest.(check int) "one claimable" 1 (List.length claimed);
  let entry =
    match claimed with
    | [ e ] -> e
    | _ -> Alcotest.fail "expected single catchup claim"
  in
  Alcotest.(check string)
    "in_flight" "in_flight"
    (outbox_status_name entry.status);
  let reclaimed =
    assert_ok (Outbox.claim_due ~db ~now:fixed_now ~limit:10 ())
  in
  Alcotest.(check int) "restart reclaim in_flight" 1 (List.length reclaimed);

  (* Delivery failure after 24h → dead letter. *)
  let past_24h = fixed_now +. Outbox.default_max_age_seconds +. 1. in
  let dead =
    assert_ok
      (Outbox.mark_failure ~db ~id:entry.id ~error:"connector 503" ~now:past_24h
         ())
  in
  Alcotest.(check string)
    "dead_letter" "dead_letter"
    (outbox_status_name dead.status);
  (match dead.dead_lettered_at with
  | Some _ -> ()
  | None -> Alcotest.fail "expected dead_lettered_at");
  let still_claimable = assert_ok (Outbox.claim_due ~db ~now:past_24h ()) in
  Alcotest.(check int) "dead not claimable" 0 (List.length still_claimable);
  let dead_list = assert_ok (Outbox.list_dead_letters ~db ~room_id:room ()) in
  Alcotest.(check bool)
    "listed dead letter" true
    (List.exists (fun (e : Outbox.entry) -> e.id = entry.id) dead_list)

(* --------------------------------------------------------------------------- *)
(* 8. Auth selection App vs PAT                                                *)
(* --------------------------------------------------------------------------- *)

let test_auth_app_vs_pat_selection () =
  (* PAT-only exact repo. *)
  let pat_auth = Auth.snapshot_of_parts ~pat:"ghp_test_token_value" () in
  let pat_sel =
    Auth.select_for_repo ~auth:pat_auth ~repo_full_name:"acme/widget" ()
  in
  Alcotest.(check string)
    "pat mode" "pat_only"
    (Auth.auth_mode_to_string pat_sel.mode);
  Alcotest.(check bool) "pat chosen" true (pat_sel.chosen = `Pat);
  (* App-only authorized installation. *)
  let app_auth = Auth.snapshot_of_parts ~app:(sample_app ()) () in
  let installation = sample_scope ~login:"acme" () in
  let app_sel =
    Auth.select_for_repo ~auth:app_auth ~installation
      ~repo_full_name:"acme/widget" ()
  in
  Alcotest.(check string)
    "app mode" "app_only"
    (Auth.auth_mode_to_string app_sel.mode);
  (match app_sel.chosen with
  | `App id -> Alcotest.(check int) "installation" 1001 id
  | _ -> Alcotest.fail "expected App");
  (* Mixed prefers App. *)
  let mixed =
    Auth.snapshot_of_parts ~pat:"ghp_test_token_value" ~app:(sample_app ()) ()
  in
  let mixed_sel =
    Auth.select_for_repo ~auth:mixed ~installation ~repo_full_name:"acme/widget"
      ()
  in
  Alcotest.(check string)
    "mixed mode" "mixed"
    (Auth.auth_mode_to_string mixed_sel.mode);
  Alcotest.(check string)
    "prefer app reason" "app_preferred_when_mixed"
    (Auth.selection_reason_to_string mixed_sel.reason);
  (match mixed_sel.chosen with
  | `App _ -> ()
  | _ -> Alcotest.fail "mixed must prefer App");
  Alcotest.(check bool)
    "explains prefer" true
    (contains mixed_sel.explanation "prefer"
    || contains mixed_sel.explanation "app");
  (* Migration preserves PAT without confirmed apply. *)
  let before = Auth.snapshot_of_parts ~pat:"ghp_old" () in
  let after_keep =
    Auth.snapshot_of_parts ~pat:"ghp_old" ~app:(sample_app ()) ()
  in
  match
    Auth.migration_safe ~before ~after:after_keep ~confirmed_apply:false
  with
  | Ok () ->
      Alcotest.(check bool)
        "pat preserved" true
        (Auth.migration_preserves_pat ~before ~after:after_keep)
  | Error e -> Alcotest.fail e

(* --------------------------------------------------------------------------- *)
(* 9. Access revocation: live GitHub fetch soft-fails without secrets          *)
(* --------------------------------------------------------------------------- *)

let test_access_revocation_soft_fail () =
  with_delivery_db @@ fun db ->
  let room = "room-revoked" in
  let _proj =
    seed_lifecycle_projection ~db ~room_id:room ~number:42 ~title:"Still known"
      ~state:"open" ~delivery_id:"deliv-revoked" ~now:fixed_now
  in
  let live_fetch ~item_key:_ =
    Error "GitHub 401: Authorization: Bearer ghp_should_not_appear"
  in
  let grounded =
    assert_ok
      (Ground.ground ~db
         ~source:
           (Context.Card_action { action = "ask"; item_key; room_id = room })
         ~live_fetch ())
  in
  Alcotest.(check (option string))
    "item still grounded" (Some item_key) grounded.item_key;
  Alcotest.(check bool) "live absent after revoke" true (grounded.live = None);
  Alcotest.(check bool)
    "no secret leak" false
    (contains grounded.prompt_block "ghp_should_not_appear"
    || contains grounded.prompt_block "bearer");
  Alcotest.(check bool)
    "journal context remains" true
    (String.length grounded.prompt_block > 0)

(* --------------------------------------------------------------------------- *)
(* 10. Pilot gate default-off, explicit enable, backout                         *)
(* --------------------------------------------------------------------------- *)

let test_pilot_default_off_enable_backout () =
  (* Default gates are off across high-risk families (P19 experimental). *)
  Alcotest.(check bool)
    "review default off" false Review.default_pilot_gate.enabled;
  Alcotest.(check bool)
    "merge default off" false Merge.default_pilot_gate.enabled;
  Alcotest.(check bool)
    "issue default off" false Issue.default_pilot_gate.enabled;
  Alcotest.(check bool)
    "workflow default off" false Workflow.default_pilot_gate.enabled;
  Alcotest.(check bool)
    "code_change default off" false Code.default_pilot_gate.enabled;

  with_action_db @@ fun db ->
  let route = make_route ~id:"rt_pilot" ~policy:(caps ~review:true ()) in
  let req =
    {
      Review.item_key = collab_item_key;
      kind = Review.Comment;
      head_sha;
      body = Some "nit: rename";
      actor_login = Some "alice";
    }
  in
  (* Explicit enable → preview ok (P19 pilot only; user_auth still false). *)
  let pilot_on =
    {
      Review.enabled = true;
      pilot_name = "p19-pr-review-pilot";
      expires_at = Some "2099-01-01T00:00:00Z";
    }
  in
  let plan =
    assert_ok
      (Workflow_shared.preview ~db ~principal ~room_id
         ~action:(Workflow_shared.Submit_review req) ~base_revision ~route
         ~pilot:pilot_on ~user_auth_available:false ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "enabled pilot allows plan" true
    (Workflow_shared.is_github_action_plan plan);
  (* No claim that P19 provides user attribution. *)
  let planned = Yojson.Safe.to_string plan.planned_state in
  Alcotest.(check bool)
    "does not claim user attribution production" false
    (contains planned "user_required enabled"
    || contains planned "production user attribution");

  (* Backout: disable pilot → deny again. *)
  let pilot_backout =
    {
      Review.enabled = false;
      pilot_name = "p19-pr-review-pilot";
      expires_at = None;
    }
  in
  (match
     Workflow_shared.preview ~db ~principal ~room_id
       ~action:(Workflow_shared.Submit_review req) ~base_revision ~route
       ~pilot:pilot_backout ~user_auth_available:false ~now:(fixed_now +. 1.) ()
   with
  | Ok _ -> Alcotest.fail "backout should deny"
  | Error msg ->
      Alcotest.(check bool) "backout pilot deny" true (deny_mentions_pilot msg));

  (* Expired enable is also a backout. *)
  let pilot_expired =
    {
      Review.enabled = true;
      pilot_name = "p19-pr-review-pilot";
      expires_at = Some "2020-01-01T00:00:00Z";
    }
  in
  match
    Workflow_shared.preview ~db ~principal ~room_id
      ~action:(Workflow_shared.Submit_review req) ~base_revision ~route
      ~pilot:pilot_expired ~user_auth_available:false ~now:fixed_now ()
  with
  | Ok _ -> Alcotest.fail "expired pilot should deny"
  | Error msg ->
      Alcotest.(check bool)
        "expired mentions pilot/expir" true
        (deny_mentions_pilot msg || contains msg "expir")

(* --------------------------------------------------------------------------- *)
(* 11. Minimal-build surface note (modules present in full runtime)            *)
(* --------------------------------------------------------------------------- *)

let test_minimal_build_module_surface () =
  (* Full-build integration suite documents that P19 GitHub modules are present
     and callable. Minimal binary (command_bridge_min / clawq-min) disables
     service/runtime integrations with "disabled in minimal build"; this suite
     asserts the full-runtime modules that pilot/ops paths depend on exist. *)
  Alcotest.(check bool)
    "default_max_age is 24h" true
    (Outbox.default_max_age_seconds = 86400.);
  Alcotest.(check string)
    "workflow capability key" "workflow_dispatch" Workflow.capability_key;
  Alcotest.(check string)
    "code_change capability key" "code_change" Code.capability_key;
  Alcotest.(check string) "branch prefix" "clawq/" Code.default_branch_prefix;
  Alcotest.(check bool)
    "review pilot name set" true
    (String.length Review.default_pilot_gate.pilot_name > 0);
  Alcotest.(check bool)
    "merge pilot name set" true
    (String.length Merge.default_pilot_gate.pilot_name > 0);
  Alcotest.(check bool)
    "issue pilot name set" true
    (String.length Issue.default_pilot_gate.pilot_name > 0);
  (* Compatibility aliases document route cutover (no dual-write). *)
  let aliases = Migrate.compatibility_cli_aliases () in
  Alcotest.(check bool) "migrate aliases non-empty" true (aliases <> []);
  Alcotest.(check bool)
    "aliases mention route" true
    (List.exists (fun (_from, to_) -> contains to_ "route") aliases)

let suite =
  [
    ( "Teams adaptive vs plain/editless fallback",
      `Quick,
      test_teams_adaptive_vs_plain_fallback );
    ( "route migrate Prefer_existing collision + idempotent",
      `Quick,
      test_route_migrate_prefer_existing_collision );
    ( "action families collab success + pilot-off denies",
      `Quick,
      test_action_families_collab_and_pilot_denies );
    ("PAT org deny", `Quick, test_pat_org_deny);
    ("stale plan apply reject", `Quick, test_stale_plan_apply_reject);
    ( "reconcile webhook closes once no outbox",
      `Quick,
      test_reconcile_webhook_closes_once_no_outbox );
    ( "delivery dead letter + catchup + restart reclaim",
      `Quick,
      test_delivery_dead_letter_catchup_restart );
    ("auth App vs PAT selection", `Quick, test_auth_app_vs_pat_selection);
    ( "access revocation live soft-fail",
      `Quick,
      test_access_revocation_soft_fail );
    ( "pilot default-off enable backout",
      `Quick,
      test_pilot_default_off_enable_backout );
    ( "minimal-build module surface present",
      `Quick,
      test_minimal_build_module_surface );
  ]
