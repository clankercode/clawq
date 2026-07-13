(** Tests for P21.M3.E3.T003: preserve pinned attribution through delayed and
    background work.

    Covers: pin construction (snapshot + allow, no credentials), generation
    advance within lineage, lineage break fail-closed, outbox/work-item
    preservation across retry, plan attach, and conflicting pin rejection. *)

module Auth = Github_attribution_authorize
module Delayed = Github_delayed_attribution
module Job = Github_durable_job_actor_attribution
module O = Github_delivery_outbox
module D = Github_delivery_intent
module Proj = Github_item_projection
module E = Github_event_envelope
module Bg = Github_room_background_work
module RS = Github_route_store
module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding
module A = Actor_snapshot
module Audit = Github_attribution_audit
module Policy = Github_attribution_policy
module V = Github_user_token_vault
module Token_store = Github_user_token_store
module Token_lease = Github_user_token_lease

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-delayed-attr-test" ()

let sample_tokens =
  {
    Token_store.access_token = "ghu_access_DELAYED_ATTR_PLAINTEXT_never_export";
    refresh_token = Some "ghr_refresh_DELAYED_ATTR_PLAINTEXT_never_export";
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let contains ~needle s =
  let n = String.length needle in
  let len = String.length s in
  if n = 0 then true
  else if n > len then false
  else
    let rec loop i =
      if i + n > len then false
      else if String.sub s i n = needle then true
      else loop (i + 1)
    in
    loop 0

let secrets_absent blob =
  List.iter
    (fun needle ->
      Alcotest.(check bool) ("no " ^ needle) false (contains ~needle blob))
    [
      sample_tokens.access_token;
      Option.get sample_tokens.refresh_token;
      "ghu_access_DELAYED";
      "ghr_refresh_DELAYED";
      aes_key;
    ]

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  B.ensure_schema db;
  O.ensure_schema db;
  Github_work_item.init_schema db;
  RS.ensure_schema db;
  Setup_plan_apply.init_schema db;
  V.ensure_schema db;
  Audit.ensure_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (Token_lease.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let fixed_now = 1_785_300_000.0
let room_id = "room-teams-shared"
let item_key = "pr:acme/widget:42"
let pid s = assert_ok (P.principal_id_of_string s)

let sample_key ?(connector = P.Teams) ?(tenant = "tenant-acme")
    ?(user = "user-ada") () =
  assert_ok
    (P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
       ~immutable_user_id:user)

let seed_principal ~db ~id ?(revision = 1) () =
  let p =
    P.make_principal ~id:(pid id) ~revision ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p))

let seed_actor_and_link ~db ~principal_id ~key ?(link_id = "idlink_ada") () =
  let actor =
    P.make_connector_actor ~key ~principal_id
      ~display:
        {
          display_name = Some "Ada";
          avatar_url = None;
          email = None;
          extra = [];
        }
      ~verified_at:"2026-07-01T00:00:00Z" ~created_at:"2026-07-01T00:00:00Z"
      ~updated_at:"2026-07-01T00:00:00Z" ()
  in
  let actor = assert_ok (S.insert_connector_actor ~db ~now:fixed_now actor) in
  let link =
    P.make_identity_link ~id:link_id ~principal_id ~actor_key:key
      ~linked_at:"2026-07-01T00:00:00Z" ()
  in
  let link = assert_ok (S.insert_identity_link ~db ~now:fixed_now link) in
  (actor, link)

let seed_binding ~db ~principal_id ?(id = "ghbind_ada")
    ?(lineage_id = "lineage_ada") ?(github_user_id = 9001L) () =
  let identity =
    assert_ok (B.make_account_identity ~app_id:42 ~github_user_id ())
  in
  let b =
    B.make_binding ~id ~principal_id ~identity ~lineage_id
      ~authorization_status:B.Authorized
      ~display:{ B.login = Some "ada"; avatar_url = None }
      ~vault_ref:(assert_ok (B.make_vault_ref "vault_opaque_only"))
      ~created_at:"2026-07-01T00:00:00Z" ~updated_at:"2026-07-01T00:00:00Z" ()
  in
  assert_ok (B.insert ~db ~now:fixed_now b)

let seed_ada ~db =
  let principal_id = pid "prin_ada" in
  seed_principal ~db ~id:"prin_ada" ();
  let key = sample_key () in
  let _actor, link = seed_actor_and_link ~db ~principal_id ~key () in
  let binding = seed_binding ~db ~principal_id () in
  (principal_id, key, link, binding)

let selected ?(binding_id = "ghbind_ada") ?(lineage_id = "lineage_ada")
    ?(authorized = true) ?(vault_active = true) ?(vault_generation = 1)
    ?(lineage_matches_pin = true) () =
  assert_ok
    (Auth.make_selected_binding ~binding_id ~lineage_id ~authorized
       ~vault_active ~vault_generation ~lineage_matches_pin ())

let base_request ?(action = "comment") ?(tool_authorized = true)
    ?(repo_granted = true) ?(repo_blocked = false) ?(principal_current = true)
    ?(confirmation_required = false) ?(confirmation_satisfied = true)
    ?(confirmation_id = None) ?(binding = Auth.Selected (selected ()))
    ?(installation_active = true) ?(installation_repo_ok = true)
    ?(permissions_ok = true) ?(user_authority_ok = true) ?(org_policy_ok = true)
    ?(sso_ok = true) ?(live_ok = true) ?(live_detail = None)
    ?(live_revision = Some "meta_rev_1") ?(pin = Auth.empty_revision_pin)
    ?(actor_snapshot_id = Some "snap_1") ?(catalog_revision = "cat_rev_1")
    ?(access_revision = "acc_rev_1") ?(principal_revision = 1)
    ?(installation_revision = Some "inst_rev_1")
    ?(fallback = Auth.default_fallback_context) ?(principal_id = "prin_ada") ()
    : Auth.request =
  {
    action;
    tool_catalog =
      {
        revision = catalog_revision;
        access_revision;
        tool_authorized;
        room_id = Some room_id;
        session_key = Some "sess_1";
      };
    repo_grant =
      {
        repo_full_name = "acme/widget";
        granted = repo_granted;
        blocked = repo_blocked;
        access_revision = Some access_revision;
      };
    principal =
      {
        principal_id;
        principal_revision;
        principal_current_active = principal_current;
        actor_revision = Some 1;
        identity_link_revision = Some 1;
        confirmation_id;
        confirmation_required;
        confirmation_satisfied;
      };
    binding = { resolution = binding };
    installation =
      {
        installation_id = Some 99;
        revision = installation_revision;
        active = installation_active;
        repo_authorized = installation_repo_ok;
        permissions_ok;
      };
    user_org_sso = { user_authority_ok; org_policy_ok; sso_ok };
    live_action =
      { ok = live_ok; revision = live_revision; detail = live_detail };
    pin;
    actor_snapshot_id;
    fallback;
  }

let authorize_allow ?(vault_generation = 1)
    ?(actor_snapshot_id : string option = None) () =
  let binding = Auth.Selected (selected ~vault_generation ()) in
  let req =
    base_request ~binding ~actor_snapshot_id
      ~pin:
        {
          Auth.empty_revision_pin with
          binding_lineage_id = Some "lineage_ada";
          vault_generation = Some vault_generation;
          actor_snapshot_id;
          principal_revision = Some 1;
        }
      ()
  in
  match Auth.authorize req with
  | Auth.Allow a -> a
  | Auth.Deny d ->
      Alcotest.fail
        (Printf.sprintf "expected allow: %s %s" d.failed_check d.repair.message)

let capture_snap ~db ~key ~(binding : B.binding) ~job_id =
  assert_ok
    (Job.capture_for_delayed_job ~db ~actor_key:key ~delayed_job_id:job_id
       ~account_binding_id:binding.id ~room_id ~now:fixed_now ())

let sample_intent ?(id = "ghdi_delayed_1") () : D.intent =
  let proj : Proj.projection =
    {
      room_id;
      item_key;
      title = Some "Add feature";
      state = Some "open";
      draft = Some false;
      merged = None;
      labels = [ "enhancement" ];
      assignees = [ "alice" ];
      head_sha = Some "abc123";
      html_url = Some "https://github.com/acme/widget/pull/42";
      last_event_at = Some "2024-01-01T00:00:00Z";
      last_family = Some E.Lifecycle;
      comment_count = 0;
      revision = 1;
      card_kind = Proj.Lifecycle;
    }
  in
  let intent = D.of_projection ~room_id ~projection:proj ~now:fixed_now () in
  { intent with id }

let caps ~background : RS.capability_policy =
  {
    allow_reply = false;
    allow_label = false;
    allow_assign = false;
    allow_review = false;
    allow_merge = false;
    allow_close = false;
    extra = (if background then [ (Bg.capability_key, true) ] else []);
  }

let make_route ~id ~policy : RS.t =
  {
    id;
    destination = RS.Room room_id;
    selector = RS.Repo "acme/widget";
    filter = RS.default_filter;
    comment_mode = RS.default_comment_mode;
    capability_policy = policy;
    enabled = true;
    revision = "1";
    managed_bundle_id = None;
    managed_feature_id = None;
    provenance =
      {
        created_by = Some "test";
        created_via = Some "test";
        setup_plan_id = None;
        notes = None;
      };
    created_at = "2024-01-01T00:00:00Z";
    updated_at = "2024-01-01T00:00:00Z";
  }

let pilot_on =
  {
    Bg.enabled = true;
    pilot_name = "p19-room-background-work-pilot";
    expires_at = None;
  }

let bg_req ?(dedup = "room:room-teams-shared:bg:delayed-1") () : Bg.request =
  {
    room_id;
    item_key = Some "issue:acme/widget:9";
    prompt = "summarize discussion";
    runner_pref = None;
    thread_ref = Some "thread:msg-1";
    dedup_key = dedup;
  }

let principal_plan =
  Setup_plan.{ id = "principal:ada"; kind = Principal; label = Some "Ada" }

(* -------------------------------------------------------------------------- *)
(* Pin construction                                                            *)
(* -------------------------------------------------------------------------- *)

let test_make_pin_token_free () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap = capture_snap ~db ~key ~binding ~job_id:"job_pin_1" in
  let allow = authorize_allow ~actor_snapshot_id:(Some snap.id) () in
  let pin =
    assert_ok
      (Delayed.make_pin ~job_id:"job_pin_1" ~snapshot:snap ~allow
         ~expected_github_actor:
           (Audit.Numeric_user
              { host = "github.com"; app_id = 42; github_user_id = 9001L })
         ~confirmation_id:"conf_delayed_1" ())
  in
  Alcotest.(check string) "job" "job_pin_1" pin.job_id;
  Alcotest.(check bool) "never authority" false (A.is_authority pin.snapshot);
  let j = assert_ok (Delayed.pin_to_storage_json pin) in
  secrets_absent (Yojson.Safe.to_string j);
  Alcotest.(check bool) "no token material" false (A.contains_token_material j);
  let round = assert_ok (Delayed.pin_of_storage_json j) in
  Alcotest.(check string) "roundtrip job" pin.job_id round.job_id;
  Alcotest.(check string) "roundtrip snap" snap.id round.snapshot.id;
  Alcotest.(check string)
    "mode" "user"
    (Auth.resolved_mode_to_string round.allow.mode)

let test_pin_for_delayed_clears_generation () =
  let allow = authorize_allow ~vault_generation:3 () in
  Alcotest.(check (option int))
    "prior has gen" (Some 3) allow.revisions.vault_generation;
  let pin = Delayed.pin_for_delayed_revalidate allow in
  Alcotest.(check (option int))
    "gen cleared for delay" None pin.vault_generation;
  Alcotest.(check (option string))
    "lineage kept" (Some "lineage_ada") pin.binding_lineage_id

(* -------------------------------------------------------------------------- *)
(* Generation advance within lineage                                           *)
(* -------------------------------------------------------------------------- *)

let test_generation_advance_within_lineage () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap = capture_snap ~db ~key ~binding ~job_id:"job_gen_1" in
  let prior =
    authorize_allow ~vault_generation:1 ~actor_snapshot_id:(Some snap.id) ()
  in
  let pin =
    assert_ok
      (Delayed.make_pin ~job_id:"job_gen_1" ~snapshot:snap ~allow:prior
         ~expected_github_actor:
           (Audit.Numeric_user
              { host = "github.com"; app_id = 42; github_user_id = 9001L })
         ())
  in
  (* Live evidence after ordinary refresh: same lineage, generation advanced. *)
  let live =
    base_request
      ~binding:(Auth.Selected (selected ~vault_generation:4 ()))
      ~actor_snapshot_id:(Some snap.id) ~pin:Auth.empty_revision_pin ()
  in
  let env =
    assert_ok
      (match
         Delayed.prepare_execution ~db ~job_id:"job_gen_1" ~pin ~live ()
       with
      | Ok e -> Ok e
      | Error inv -> Error (Delayed.string_of_exec_invalidation inv))
  in
  Alcotest.(check bool)
    "usable snap" true env.snapshot_env.live_authority.usable;
  Alcotest.(check bool) "gen advanced" true env.generation_advanced;
  Alcotest.(check (option int))
    "fresh gen" (Some 4) env.fresh_allow.revisions.vault_generation;
  Alcotest.(check (option string))
    "same lineage" (Some "lineage_ada")
    env.fresh_allow.revisions.binding_lineage_id;
  Alcotest.(check string)
    "mode continuous" "user"
    (Auth.resolved_mode_to_string env.fresh_allow.mode)

let test_tight_generation_pin_would_deny_but_delayed_allows () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap = capture_snap ~db ~key ~binding ~job_id:"job_gen_2" in
  let prior =
    authorize_allow ~vault_generation:1 ~actor_snapshot_id:(Some snap.id) ()
  in
  let live =
    base_request
      ~binding:(Auth.Selected (selected ~vault_generation:5 ()))
      ~actor_snapshot_id:(Some snap.id)
      ~pin:
        {
          Auth.empty_revision_pin with
          binding_lineage_id = Some "lineage_ada";
          vault_generation = Some 1;
          actor_snapshot_id = Some snap.id;
          principal_revision = Some 1;
        }
      ()
  in
  (* Immediate dispatch revalidation pins generation tightly → deny. *)
  (match Github_attribution_dispatch_lease.revalidate ~live ~prior () with
  | Error (Github_attribution_dispatch_lease.Authorization d) ->
      Alcotest.(check bool)
        "stale gen" true
        (contains ~needle:"generation" d.repair.code
        || contains ~needle:"generation" d.repair.message
        || contains ~needle:"generation" d.failed_check)
  | Error e ->
      Alcotest.fail
        ("expected authorization deny on tight pin: "
        ^ Github_attribution_dispatch_lease.string_of_denial e)
  | Ok _ -> Alcotest.fail "tight pin should deny on generation advance");
  (* Delayed path permits advance. *)
  let pin =
    assert_ok
      (Delayed.make_pin ~job_id:"job_gen_2" ~snapshot:snap ~allow:prior ())
  in
  match Delayed.prepare_execution ~db ~job_id:"job_gen_2" ~pin ~live () with
  | Ok env -> Alcotest.(check bool) "advanced" true env.generation_advanced
  | Error inv -> Alcotest.fail (Delayed.string_of_exec_invalidation inv)

(* -------------------------------------------------------------------------- *)
(* Lineage break fails closed                                                  *)
(* -------------------------------------------------------------------------- *)

let test_lineage_break_on_relink () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap = capture_snap ~db ~key ~binding ~job_id:"job_lin_1" in
  let prior = authorize_allow ~actor_snapshot_id:(Some snap.id) () in
  let pin =
    assert_ok
      (Delayed.make_pin ~job_id:"job_lin_1" ~snapshot:snap ~allow:prior ())
  in
  let live =
    base_request
      ~binding:
        (Auth.Selected
           (selected ~lineage_id:"lineage_RELINKED" ~vault_generation:1
              ~lineage_matches_pin:false ()))
      ~actor_snapshot_id:(Some snap.id) ()
  in
  match Delayed.prepare_execution ~db ~job_id:"job_lin_1" ~pin ~live () with
  | Ok _ -> Alcotest.fail "expected lineage break"
  | Error inv ->
      let msg = Delayed.string_of_exec_invalidation inv in
      Alcotest.(check bool)
        "lineage language" true
        (contains ~needle:"lineage" msg || contains ~needle:"binding" msg)

let test_lineage_break_on_binding_id_change () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap = capture_snap ~db ~key ~binding ~job_id:"job_lin_2" in
  let prior = authorize_allow ~actor_snapshot_id:(Some snap.id) () in
  let pin =
    assert_ok
      (Delayed.make_pin ~job_id:"job_lin_2" ~snapshot:snap ~allow:prior ())
  in
  let live =
    base_request
      ~binding:
        (Auth.Selected
           (selected ~binding_id:"ghbind_OTHER" ~lineage_id:"lineage_ada"
              ~vault_generation:2 ()))
      ~actor_snapshot_id:(Some snap.id) ()
  in
  match Delayed.prepare_execution ~db ~job_id:"job_lin_2" ~pin ~live () with
  | Ok _ -> Alcotest.fail "expected binding change fail closed"
  | Error inv ->
      let msg = Delayed.string_of_exec_invalidation inv in
      Alcotest.(check bool)
        "binding language" true
        (contains ~needle:"binding" msg || contains ~needle:"lineage" msg)

let test_cancelled_job_not_executed () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap = capture_snap ~db ~key ~binding ~job_id:"job_cancel" in
  let prior = authorize_allow ~actor_snapshot_id:(Some snap.id) () in
  let pin =
    assert_ok
      (Delayed.make_pin ~job_id:"job_cancel" ~snapshot:snap ~allow:prior ())
  in
  let live = base_request ~actor_snapshot_id:(Some snap.id) () in
  match
    Delayed.prepare_execution ~db ~job_id:"job_cancel" ~pin ~live
      ~cancelled:true ()
  with
  | Ok _ -> Alcotest.fail "cancelled must not execute"
  | Error inv ->
      Alcotest.(check bool)
        "cancelled" true
        (contains ~needle:"cancel" (Delayed.string_of_exec_invalidation inv))

(* -------------------------------------------------------------------------- *)
(* Outbox + work item preservation                                             *)
(* -------------------------------------------------------------------------- *)

let test_outbox_preserves_full_pin_across_retry () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let intent = sample_intent ~id:"ghdi_pin_outbox" () in
  let snap = capture_snap ~db ~key ~binding ~job_id:intent.id in
  let allow = authorize_allow ~actor_snapshot_id:(Some snap.id) () in
  let entry =
    assert_ok
      (O.enqueue ~db ~room_id ~item_key:intent.item_key ~intent
         ~actor_snapshot:snap ~attribution_allow:allow ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "has snap" true
    (Option.is_some entry.actor_snapshot_json);
  Alcotest.(check bool)
    "has allow" true
    (Option.is_some entry.attribution_allow_json);
  let pin =
    match assert_ok (O.delayed_pin_of_entry entry) with
    | Some p -> p
    | None -> Alcotest.fail "expected delayed pin"
  in
  Alcotest.(check string) "snap id" snap.id pin.snapshot.id;
  let claimed = assert_ok (O.claim_due ~db ~now:fixed_now ~limit:10 ()) in
  Alcotest.(check int) "claimed" 1 (List.length claimed);
  let c0 = List.hd claimed in
  (match assert_ok (O.delayed_pin_of_entry c0) with
  | Some p -> Alcotest.(check string) "claim keeps" snap.id p.snapshot.id
  | None -> Alcotest.fail "pin lost on claim");
  let after_fail =
    assert_ok
      (O.mark_failure ~db ~id:entry.id ~error:"connector timeout"
         ~now:(fixed_now +. 1.) ())
  in
  (match assert_ok (O.delayed_pin_of_entry after_fail) with
  | Some p -> Alcotest.(check string) "retry keeps" snap.id p.snapshot.id
  | None -> Alcotest.fail "pin lost on retry");
  secrets_absent
    (Yojson.Safe.to_string
       (`Assoc
          [
            ("snap", Option.value entry.actor_snapshot_json ~default:`Null);
            ("allow", Option.value entry.attribution_allow_json ~default:`Null);
          ]))

let test_work_item_preserves_pin_on_cancel_retry () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap = capture_snap ~db ~key ~binding ~job_id:"wi_1" in
  let allow = authorize_allow ~actor_snapshot_id:(Some snap.id) () in
  let outcome =
    assert_ok
      (Github_work_item.create_if_new ~db ~dedup_key:"dedup-delayed-wi-1"
         ~repo_full_name:"acme/widget" ~issue_number:9 ~requester:"ada"
         ~trigger:"room_background" ~prompt:"do work" ~actor_snapshot:snap
         ~attribution_allow:allow ())
  in
  let item =
    match outcome with
    | Github_work_item.Created i | Github_work_item.Duplicate i -> i
  in
  let pin =
    match assert_ok (Github_work_item.delayed_pin_of_item item) with
    | Some p -> p
    | None -> Alcotest.fail "expected pin on work item"
  in
  Alcotest.(check string) "snap" snap.id pin.snapshot.id;
  Github_work_item.record_result ~db ~id:item.id
    ~status:Github_work_item.Cancelled
    ~result_kind:Github_work_item.Result_failed ~result_summary:"cancelled";
  let after =
    match Github_work_item.get ~db ~id:item.id with
    | Some i -> i
    | None -> Alcotest.fail "missing after cancel"
  in
  (match assert_ok (Github_work_item.delayed_pin_of_item after) with
  | Some p -> Alcotest.(check string) "cancel keeps pin" snap.id p.snapshot.id
  | None -> Alcotest.fail "pin lost on cancel");
  (* Re-queue path: status back to queued must keep pin. *)
  Github_work_item.set_status ~db ~id:item.id ~status:Github_work_item.Queued;
  let retried =
    match Github_work_item.get ~db ~id:item.id with
    | Some i -> i
    | None -> Alcotest.fail "missing after requeue"
  in
  match assert_ok (Github_work_item.delayed_pin_of_item retried) with
  | Some p -> Alcotest.(check string) "retry keeps pin" snap.id p.snapshot.id
  | None -> Alcotest.fail "pin lost on requeue"

let test_outbox_rejects_conflicting_pin () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let intent = sample_intent ~id:"ghdi_conflict" () in
  let snap = capture_snap ~db ~key ~binding ~job_id:intent.id in
  let allow = authorize_allow ~actor_snapshot_id:(Some snap.id) () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id ~item_key:intent.item_key ~intent
          ~actor_snapshot:snap ~attribution_allow:allow ~now:fixed_now ()));
  (* Different actor cannot replace initiating pin. *)
  let other_key = sample_key ~user:"user-bob" () in
  seed_principal ~db ~id:"prin_bob" ();
  let bob_pid = pid "prin_bob" in
  ignore
    (seed_actor_and_link ~db ~principal_id:bob_pid ~key:other_key
       ~link_id:"idlink_bob" ());
  let bob_binding =
    seed_binding ~db ~principal_id:bob_pid ~id:"ghbind_bob"
      ~lineage_id:"lineage_bob" ~github_user_id:9002L ()
  in
  let snap_bob =
    capture_snap ~db ~key:other_key ~binding:bob_binding ~job_id:intent.id
  in
  let allow_bob =
    match
      Auth.authorize
        (base_request ~principal_id:"prin_bob"
           ~binding:
             (Auth.Selected
                (selected ~binding_id:"ghbind_bob" ~lineage_id:"lineage_bob" ()))
           ~actor_snapshot_id:(Some snap_bob.id)
           ~pin:
             {
               Auth.empty_revision_pin with
               binding_lineage_id = Some "lineage_bob";
               principal_revision = Some 1;
               actor_snapshot_id = Some snap_bob.id;
             }
           ())
    with
    | Auth.Allow a -> a
    | Auth.Deny d -> Alcotest.fail d.repair.message
  in
  match
    O.enqueue ~db ~room_id ~item_key:intent.item_key ~intent
      ~actor_snapshot:snap_bob ~attribution_allow:allow_bob
      ~now:(fixed_now +. 1.) ()
  with
  | Error e ->
      Alcotest.(check bool)
        "conflict" true
        (contains ~needle:"conflict" e
        || contains ~needle:"borrow" e
        || contains ~needle:"refuses" e)
  | Ok _ -> Alcotest.fail "expected conflicting pin rejection"

(* -------------------------------------------------------------------------- *)
(* Background plan attach                                                      *)
(* -------------------------------------------------------------------------- *)

let test_background_plan_attaches_delayed_pin () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let route = make_route ~id:"rt_bg" ~policy:(caps ~background:true) in
  let snap = capture_snap ~db ~key ~binding ~job_id:"plan_will_replace" in
  let allow = authorize_allow ~actor_snapshot_id:(Some snap.id) () in
  let plan =
    assert_ok
      (Bg.plan_background ~db ~principal:principal_plan ~req:(bg_req ())
         ~base_revision:"rev-1" ~route ~pilot:pilot_on ~actor_snapshot:snap
         ~attribution_allow:allow
         ~expected_github_actor:
           (Audit.Numeric_user
              { host = "github.com"; app_id = 42; github_user_id = 9001L })
         ~now:fixed_now ())
  in
  Alcotest.(check bool) "has allow" true (Delayed.has_attribution_allow plan);
  let pin =
    match assert_ok (Delayed.pin_of_plan plan) with
    | Some p -> p
    | None -> Alcotest.fail "expected delayed pin on plan"
  in
  Alcotest.(check string) "snap" snap.id pin.snapshot.id;
  Alcotest.(check string)
    "mode" "user"
    (Auth.resolved_mode_to_string pin.allow.mode);
  secrets_absent (Yojson.Safe.to_string plan.apply_payload.data);
  let item =
    assert_ok
      (Bg.enqueue_work_item ~db ~req:(bg_req ()) ~actor_snapshot:snap
         ~attribution_allow:allow ())
  in
  match assert_ok (Github_work_item.delayed_pin_of_item item) with
  | Some p -> Alcotest.(check string) "wi snap" snap.id p.snapshot.id
  | None -> Alcotest.fail "work item missing pin"

let test_issue_delayed_dispatch_user_lease () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap = capture_snap ~db ~key ~binding ~job_id:"job_lease" in
  let prior =
    authorize_allow ~vault_generation:1 ~actor_snapshot_id:(Some snap.id) ()
  in
  let pin =
    assert_ok
      (Delayed.make_pin ~job_id:"job_lease" ~snapshot:snap ~allow:prior ())
  in
  let keys =
    assert_ok
      (V.make_single_key_provider ~key_id:"mk-delayed" ~key_version:1 ~aes_key
         ())
  in
  let account =
    assert_ok
      (V.make_account_key ~principal_id:"prin_ada" ~github_user_id:9001L
         ~app_id:42 ())
  in
  let vault =
    match
      V.create ~db ~keys ~id:"ghvault_delayed_1" ~now:fixed_now ~account
        ~tokens:sample_tokens ~scopes:[ "repo" ]
        ~expires_at:"2026-12-01T00:00:00Z" ()
    with
    | Ok r -> r
    | Error d -> Alcotest.fail (V.string_of_denial d)
  in
  (* Live evidence matches vault generation (refresh advance is covered by
     pure revalidation tests; lease path uses real vault meta generation). *)
  let live =
    base_request
      ~binding:(Auth.Selected (selected ~vault_generation:1 ()))
      ~actor_snapshot_id:(Some snap.id) ()
  in
  match
    Delayed.issue_for_delayed_dispatch ~db ~job_id:"job_lease" ~pin ~live
      ~vault_id:vault.id ~now:fixed_now ()
  with
  | Error e -> Alcotest.fail e
  | Ok { envelope; issued } ->
      Alcotest.(check bool)
        "usable" true envelope.snapshot_env.live_authority.usable;
      Alcotest.(check bool)
        "has lease" true
        (match issued.lease with Some _ -> true | None -> false);
      Delayed.revoke_issued_lease issued;
      secrets_absent (Github_attribution_dispatch_lease.string_of_issued issued)

let test_legacy_snapshot_only_not_full_pin () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let intent = sample_intent ~id:"ghdi_legacy_snap" () in
  let snap = capture_snap ~db ~key ~binding ~job_id:intent.id in
  let entry =
    assert_ok
      (O.enqueue ~db ~room_id ~item_key:intent.item_key ~intent
         ~actor_snapshot:snap ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "snap only" true
    (Option.is_some entry.actor_snapshot_json
    && Option.is_none entry.attribution_allow_json);
  match assert_ok (O.delayed_pin_of_entry entry) with
  | None -> ()
  | Some _ -> Alcotest.fail "snapshot-only should not form full delayed pin"

(* -------------------------------------------------------------------------- *)
(* Token isolation (P21.M3.E3.T004)                                             *)
(* -------------------------------------------------------------------------- *)

let test_delayed_dispatch_isolates_personal_token () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap = capture_snap ~db ~key ~binding ~job_id:"job_iso" in
  let prior =
    authorize_allow ~vault_generation:1 ~actor_snapshot_id:(Some snap.id) ()
  in
  let pin =
    assert_ok
      (Delayed.make_pin ~job_id:"job_iso" ~snapshot:snap ~allow:prior ())
  in
  let keys =
    assert_ok
      (V.make_single_key_provider ~key_id:"mk-delayed-iso" ~key_version:1
         ~aes_key ())
  in
  let account =
    assert_ok
      (V.make_account_key ~principal_id:"prin_ada" ~github_user_id:9001L
         ~app_id:42 ())
  in
  let vault =
    match
      V.create ~db ~keys ~id:"ghvault_delayed_iso" ~now:fixed_now ~account
        ~tokens:sample_tokens ~scopes:[ "repo" ]
        ~expires_at:"2026-12-01T00:00:00Z" ()
    with
    | Ok r -> r
    | Error d -> Alcotest.fail (V.string_of_denial d)
  in
  let live =
    base_request
      ~binding:(Auth.Selected (selected ~vault_generation:1 ()))
      ~actor_snapshot_id:(Some snap.id) ()
  in
  match
    Delayed.issue_for_delayed_dispatch ~db ~job_id:"job_iso" ~pin ~live
      ~vault_id:vault.id ~now:fixed_now ()
  with
  | Error e -> Alcotest.fail e
  | Ok { envelope = _; issued } ->
      (match issued.lease with
      | None -> Alcotest.fail "expected user lease"
      | Some lease -> (
          match Token_lease.assert_non_http_refused lease with
          | Ok () -> ()
          | Error e -> Alcotest.fail e));
      let materials = Delayed.isolation_materials_of_pin ~pin ~issued () in
      (match Token_lease.assert_materials_token_free ~materials with
      | Ok () -> ()
      | Error d -> Alcotest.fail (Token_lease.string_of_denial d));
      secrets_absent (Github_attribution_dispatch_lease.string_of_issued issued);
      secrets_absent
        (Yojson.Safe.to_string
           (Github_attribution_dispatch_lease.issued_to_json issued));
      (match
         Delayed.enforce_token_isolation ~db ~lease:(Option.get issued.lease)
           ~materials:
             [
               ( Token_lease.Runner_env,
                 "GITHUB_TOKEN=" ^ sample_tokens.access_token );
             ]
           ~job_id:"job_iso" ~now:fixed_now ()
       with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "runner env with token shape must be denied");
      (* Isolation audit present. *)
      let audits =
        Audit.list_by_action ~db ~action:"delayed_work" ~limit:20 ()
      in
      Alcotest.(check bool)
        "isolation audit" true
        (List.exists
           (fun (r : Audit.t) -> contains ~needle:"token_isolation" r.reason)
           audits);
      Delayed.revoke_issued_lease issued

let test_pin_storage_token_free_and_ambient_refuse () =
  with_db @@ fun db ->
  let _pid, key, _link, binding = seed_ada ~db in
  let snap = capture_snap ~db ~key ~binding ~job_id:"job_ambient" in
  let allow = authorize_allow ~actor_snapshot_id:(Some snap.id) () in
  let pin =
    assert_ok (Delayed.make_pin ~job_id:"job_ambient" ~snapshot:snap ~allow ())
  in
  let materials = Delayed.isolation_materials_of_pin ~pin () in
  (match Token_lease.assert_materials_token_free ~materials with
  | Ok () -> ()
  | Error d -> Alcotest.fail (Token_lease.string_of_denial d));
  (match Delayed.pin_to_storage_json pin with
  | Error e -> Alcotest.fail e
  | Ok j -> secrets_absent (Yojson.Safe.to_string j));
  (* Scheduled ambient surface refuses any user lease (App identity only). *)
  let keys =
    assert_ok
      (V.make_single_key_provider ~key_id:"mk-ambient" ~key_version:1 ~aes_key
         ())
  in
  let account =
    assert_ok
      (V.make_account_key ~principal_id:"prin_ada" ~github_user_id:9001L
         ~app_id:42 ())
  in
  let vault =
    match
      V.create ~db ~keys ~id:"ghvault_ambient" ~now:fixed_now ~account
        ~tokens:sample_tokens ~scopes:[] ~expires_at:"2026-12-01T00:00:00Z" ()
    with
    | Ok r -> r
    | Error d -> Alcotest.fail (V.string_of_denial d)
  in
  match Token_lease.issue ~db ~now:fixed_now ~vault_id:vault.id () with
  | Error d -> Alcotest.fail (Token_lease.string_of_denial d)
  | Ok lease -> (
      match Token_lease.refuse_scheduled_ambient lease with
      | Error (Token_lease.Forbidden_surface s) ->
          Alcotest.(check bool)
            "ambient msg" true
            (contains ~needle:"ambient" s || contains ~needle:"scheduled" s)
      | _ -> Alcotest.fail "scheduled ambient must refuse user lease")

let suite =
  [
    ("make pin token-free roundtrip", `Quick, test_make_pin_token_free);
    ( "delayed pin clears vault generation",
      `Quick,
      test_pin_for_delayed_clears_generation );
    ( "generation advance within lineage succeeds",
      `Quick,
      test_generation_advance_within_lineage );
    ( "tight pin denies gen advance; delayed allows",
      `Quick,
      test_tight_generation_pin_would_deny_but_delayed_allows );
    ( "lineage break on relink fails closed",
      `Quick,
      test_lineage_break_on_relink );
    ( "binding id change fails closed",
      `Quick,
      test_lineage_break_on_binding_id_change );
    ("cancelled job not executed", `Quick, test_cancelled_job_not_executed);
    ( "outbox preserves full pin across retry",
      `Quick,
      test_outbox_preserves_full_pin_across_retry );
    ( "work item preserves pin on cancel/retry",
      `Quick,
      test_work_item_preserves_pin_on_cancel_retry );
    ( "outbox rejects conflicting pin",
      `Quick,
      test_outbox_rejects_conflicting_pin );
    ( "background plan attaches delayed pin",
      `Quick,
      test_background_plan_attaches_delayed_pin );
    ( "issue delayed dispatch with gen-advanced lease",
      `Quick,
      test_issue_delayed_dispatch_user_lease );
    ( "legacy snapshot-only is not full delayed pin",
      `Quick,
      test_legacy_snapshot_only_not_full_pin );
    ( "delayed dispatch isolates personal token",
      `Quick,
      test_delayed_dispatch_isolates_personal_token );
    ( "pin storage token-free and ambient refuses user lease",
      `Quick,
      test_pin_storage_token_free_and_ambient_refuse );
  ]
