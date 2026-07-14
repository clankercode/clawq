(** P21 cross-Connector and shared-Room integration coverage (P21.M4.E2.T001).

    Integration-style suite (in-memory SQLite) covering:
    - two linked Principals + one unlinked participant in a shared Room
    - every verified actor / trust adapter namespace
    - merge/split/relink/revoke lineage breaks vs ordinary refresh
    - every action family (User_preferred + User_required)
    - safe App fallback vs User_required denial
    - delayed attribution pin lineage
    - personal-token exclusion from non-HTTP surfaces
    - native attribution receipts + webhook reconciliation isolation
    - no cross-Principal borrowing

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding
module A = Actor_snapshot
module Merge = Principal_merge
module Unlink = Principal_unlink_split
module Pref = Github_account_preference
module Job = Github_durable_job_actor_attribution
module Delayed = Github_delayed_attribution
module Auth = Github_attribution_authorize
module Policy = Github_attribution_policy
module Collab = Github_collab_actions
module Collab_attr = Github_collab_attribution
module Issue = Github_issue_actions
module Issue_attr = Github_issue_attribution
module Code = Github_code_change_action
module Code_attr = Github_code_change_attribution
module Store = Github_route_store
module Rec = Github_action_reconcile
module E = Github_event_envelope
module O = Github_delivery_outbox
module D = Github_delivery_intent
module Proj = Github_item_projection
module Audit = Github_attribution_audit
module V = Github_user_token_vault
module Token_store = Github_user_token_store
module Token_lease = Github_user_token_lease
module Lease = Github_attribution_dispatch_lease

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-p21-integration-test" ()

let sample_tokens =
  {
    Token_store.access_token = "ghu_access_P21_INT_PLAINTEXT_never_export";
    refresh_token = Some "ghr_refresh_P21_INT_PLAINTEXT_never_export";
  }

let fixed_now = 1_785_500_000.0
let shared_room = "room-shared-p21"
let item_key = "pr:acme/widget:42"
let collab_item_key = "item:acme/widget:pr:42"
let repo = "acme/widget"
let base_revision = "rev-config-1"
let far_expires = "2026-12-01T00:00:00Z"

let principal_ada =
  Setup_plan.{ id = "principal:ada"; kind = Principal; label = Some "Ada" }

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

let secrets_absent blob =
  List.iter
    (fun needle ->
      Alcotest.(check bool) ("no " ^ needle) false (contains blob needle))
    [
      sample_tokens.access_token;
      Option.get sample_tokens.refresh_token;
      "ghu_access_P21";
      "ghr_refresh_P21";
      aes_key;
    ]

let pid s = assert_ok (P.principal_id_of_string s)

let actor_key ?(connector = P.Teams) ?(tenant = "tenant-acme") ~user () =
  assert_ok
    (P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
       ~immutable_user_id:user)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  B.ensure_schema db;
  Merge.ensure_schema db;
  Unlink.ensure_schema db;
  Pref.ensure_schema db;
  O.ensure_schema db;
  Proj.ensure_schema db;
  Rec.ensure_schema db;
  Store.ensure_schema db;
  Setup_plan_apply.init_schema db;
  V.ensure_schema db;
  Audit.ensure_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (Token_lease.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let seed_principal ~db ~id ?(revision = 1)
    ?(created_at = "2026-01-01T00:00:00Z") () =
  let p =
    P.make_principal ~id:(pid id) ~revision ~created_at ~updated_at:created_at
      ()
  in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p));
  pid id

let seed_actor_and_link ~db ~principal_id ~key ~link_id
    ?(display_name = "User") () =
  let actor =
    P.make_connector_actor ~key ~principal_id
      ~display:
        {
          display_name = Some display_name;
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

let seed_binding ~db ~principal_id ~id ~github_user_id ~lineage_id
    ?(login = "user") ?(app_id = 42) () =
  let identity =
    assert_ok (B.make_account_identity ~app_id ~github_user_id ())
  in
  let b =
    B.make_binding ~id ~principal_id ~identity ~lineage_id
      ~authorization_status:B.Authorized
      ~display:{ B.login = Some login; avatar_url = None }
      ~vault_ref:(assert_ok (B.make_vault_ref ("vault_" ^ id)))
      ~created_at:"2026-07-01T00:00:00Z" ~updated_at:"2026-07-01T00:00:00Z" ()
  in
  assert_ok (B.insert ~db ~now:fixed_now b)

(** Shared-Room fixture: Ada (Teams+Slack linked, GitHub bound), Bob (Teams
    linked + GitHub), Carol (Teams actor only — unlinked, no GitHub). *)
type shared_fixture = {
  ada : P.principal_id;
  bob : P.principal_id;
  carol : P.principal_id;
  key_ada_teams : P.connector_actor_key;
  key_ada_slack : P.connector_actor_key;
  key_bob : P.connector_actor_key;
  key_carol : P.connector_actor_key;
  bind_ada : B.binding;
  bind_bob : B.binding;
}

let seed_shared_room ~db : shared_fixture =
  let ada =
    seed_principal ~db ~id:"prin_ada" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let bob =
    seed_principal ~db ~id:"prin_bob" ~created_at:"2026-02-01T00:00:00Z" ()
  in
  let carol =
    seed_principal ~db ~id:"prin_carol" ~created_at:"2026-03-01T00:00:00Z" ()
  in
  let key_ada_teams = actor_key ~user:"aad-ada" () in
  let key_ada_slack =
    actor_key ~connector:P.Slack ~tenant:"ws-acme" ~user:"UADA" ()
  in
  let key_bob = actor_key ~user:"aad-bob" () in
  let key_carol = actor_key ~user:"aad-carol" () in
  ignore
    (seed_actor_and_link ~db ~principal_id:ada ~key:key_ada_teams
       ~link_id:"link_ada_teams" ~display_name:"Ada" ());
  ignore
    (seed_actor_and_link ~db ~principal_id:ada ~key:key_ada_slack
       ~link_id:"link_ada_slack" ~display_name:"Ada Slack" ());
  ignore
    (seed_actor_and_link ~db ~principal_id:bob ~key:key_bob ~link_id:"link_bob"
       ~display_name:"Bob" ());
  ignore
    (seed_actor_and_link ~db ~principal_id:carol ~key:key_carol
       ~link_id:"link_carol" ~display_name:"Carol" ());
  let bind_ada =
    seed_binding ~db ~principal_id:ada ~id:"gh_ada" ~github_user_id:1001L
      ~lineage_id:"lin_ada" ~login:"ada" ()
  in
  let bind_bob =
    seed_binding ~db ~principal_id:bob ~id:"gh_bob" ~github_user_id:1002L
      ~lineage_id:"lin_bob" ~login:"bob" ()
  in
  {
    ada;
    bob;
    carol;
    key_ada_teams;
    key_ada_slack;
    key_bob;
    key_carol;
    bind_ada;
    bind_bob;
  }

let sample_intent ?(id = "ghdi_p21_1") ?(room_id = shared_room)
    ?(item_key = item_key) () : D.intent =
  let proj : Proj.projection =
    {
      room_id;
      item_key;
      title = Some "P21 Integration PR";
      state = Some "open";
      draft = Some false;
      merged = None;
      labels = [];
      assignees = [];
      head_sha = Some "deadbeef";
      html_url = Some "https://github.com/acme/widget/pull/42";
      last_event_at = Some "2026-07-01T00:00:00Z";
      last_family = Some E.Lifecycle;
      comment_count = 0;
      revision = 1;
      card_kind = Proj.Lifecycle;
    }
  in
  let intent = D.of_projection ~room_id ~projection:proj ~now:fixed_now () in
  { intent with id }

let make_envelope ?(event = "pull_request") ?(action = Some "closed")
    ?(family = E.Lifecycle) ?(delivery_id = Some "deliv-1")
    ?(actor_login = Some "clawq-bot") ?(actor_type = Some "Bot")
    ?(kind = Some E.Pull_request) ?(number = Some 42) ?(merged = Some true)
    ?(state = Some "closed") ?(head_sha = Some "deadbeef")
    ?(actor_id = None) () : E.t =
  let actor : E.actor =
    { login = actor_login; type_ = actor_type; id = actor_id }
  in
  {
    version = E.envelope_version;
    delivery_id;
    installation_id = Some 99;
    event;
    action;
    repo_full_name = repo;
    org = Some "acme";
    item_kind = kind;
    item_number = number;
    item_node_id = Some "PR_kwDOP21";
    item_url = Some "https://api.github.com/repos/acme/widget/pulls/42";
    html_url = Some "https://github.com/acme/widget/pull/42";
    family;
    actor;
    item_author = actor.login;
    before =
      Some
        {
          E.empty_safe_state with
          title = Some "P21 Integration PR";
          state = Some "open";
          draft = Some false;
          merged = Some false;
          head_sha;
        };
    after =
      Some
        {
          E.empty_safe_state with
          title = Some "P21 Integration PR";
          state;
          draft = Some false;
          merged;
          head_sha;
        };
    transfer = None;
    received_at = Some "2026-07-01T00:00:00Z";
    event_at = Some "2026-07-01T00:00:00Z";
    head_sha;
    unsupported = false;
    skip_reason = None;
  }

let result_tag = function
  | Rec.Closed { first_time; _ } ->
      if first_time then "closed_first" else "closed_again"
  | Rec.No_matching_receipt -> "no_matching_receipt"
  | Rec.Already_closed -> "already_closed"
  | Rec.Ignored_human_event -> "ignored_human_event"

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
    destination = Store.Room shared_room;
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
        created_via = Some "p21-integration";
        setup_plan_id = None;
        notes = None;
      };
    created_at = "2026-01-01T00:00:00Z";
    updated_at = "2026-01-01T00:00:00Z";
  }

let selected ?(binding_id = "gh_ada") ?(lineage_id = "lin_ada")
    ?(authorized = true) ?(vault_active = true) ?(vault_generation = 1)
    ?(lineage_matches_pin = true) () =
  assert_ok
    (Auth.make_selected_binding ~binding_id ~lineage_id ~authorized
       ~vault_active ~vault_generation ~lineage_matches_pin ())

let base_request ?(action = "comment") ?(principal_id = "prin_ada")
    ?(tool_authorized = true) ?(repo_granted = true) ?(repo_blocked = false)
    ?(principal_current = true) ?(confirmation_required = false)
    ?(confirmation_satisfied = true) ?(confirmation_id = None)
    ?(binding = Auth.Selected (selected ())) ?(installation_active = true)
    ?(installation_repo_ok = true) ?(permissions_ok = true)
    ?(user_authority_ok = true) ?(org_policy_ok = true) ?(sso_ok = true)
    ?(live_ok = true) ?(live_detail = None) ?(live_revision = Some "meta_rev_1")
    ?(pin = Auth.empty_revision_pin) ?(actor_snapshot_id = Some "snap_ada")
    ?(catalog_revision = "cat_rev_1") ?(access_revision = "acc_rev_1")
    ?(principal_revision = 1) ?(installation_revision = Some "inst_rev_1")
    ?(fallback = Auth.default_fallback_context) () : Auth.request =
  {
    action;
    tool_catalog =
      {
        revision = catalog_revision;
        access_revision;
        tool_authorized;
        room_id = Some shared_room;
        session_key = Some "sess_shared";
      };
    repo_grant =
      {
        repo_full_name = repo;
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

let authorize_allow ?(principal_id = "prin_ada") ?(binding_id = "gh_ada")
    ?(lineage_id = "lin_ada") ?(vault_generation = 1)
    ?(actor_snapshot_id : string option = None) ?(action = "comment") () =
  let binding =
    Auth.Selected (selected ~binding_id ~lineage_id ~vault_generation ())
  in
  let req =
    base_request ~action ~principal_id ~binding ~actor_snapshot_id
      ~pin:
        {
          Auth.empty_revision_pin with
          binding_lineage_id = Some lineage_id;
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

let account ?(principal_id = "prin_ada") ?(github_user_id = 1001L)
    ?(app_id = 42) () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ())

let make_keys ?(key_id = "mk-p21-int-1") ?(key_version = 1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let create_vault ~db ?(keys = make_keys ()) ?(account = account ())
    ?(tokens = sample_tokens) ?(id = "ghvault_p21_1")
    ?(expires_at = far_expires) () =
  match
    V.create ~db ~keys ~id ~now:fixed_now ~account ~tokens
      ~scopes:[ "repo"; "read:user" ] ~expires_at ()
  with
  | Ok r ->
      let identity =
        assert_ok
          (B.make_account_identity ~host:r.account.host ~app_id:r.account.app_id
             ~github_user_id:r.account.github_user_id ())
      in
      let binding =
        B.make_binding ~id:"gh_ada"
          ~principal_id:
            (assert_ok (P.principal_id_of_string r.account.principal_id))
          ~identity ~authorization_status:B.Authorized ~lineage_id:"lin_ada"
          ~vault_ref:(assert_ok (B.make_vault_ref r.id))
          ()
      in
      ignore (assert_ok (B.insert ~db ~now:fixed_now binding));
      r
  | Error d -> Alcotest.fail ("create vault: " ^ V.string_of_denial d)

let make_snapshot ?(snapshot_id = "actorsnap_p21_ada")
    ?(principal = "prin_ada") ?(user = "aad-ada") ?(display_name = "Ada")
    ?(binding_id = "gh_ada") ?(lineage_id = "lin_ada")
    ?(github_user_id = 1001L) () =
  assert_ok
    (A.create ~id:snapshot_id ~now:fixed_now ~reason:"p21-integration"
       ~principal_id:(pid principal) ~principal_revision:1
       ~actor_key:(actor_key ~user ())
       ~actor_revision:1 ~identity_link_id:("link_" ^ principal)
       ~identity_link_revision:1
       ~display:
         {
           display_name = Some display_name;
           avatar_url = None;
           email = None;
           extra = [];
         }
       ~source:
         {
           room_id = Some shared_room;
           session_id = None;
           message_id = None;
         }
       ~account_binding:
         (assert_ok
            (A.make_account_binding_evidence ~binding_id ~lineage_id
               ~identity:
                 (assert_ok
                    (B.make_account_identity ~app_id:42 ~github_user_id ()))
               ()))
       ())

(* -------------------------------------------------------------------------- *)
(* 1. Shared Room: two linked + one unlinked, no borrowing                     *)
(* -------------------------------------------------------------------------- *)

let test_shared_room_linked_and_unlinked_isolation () =
  with_db @@ fun db ->
  let fx = seed_shared_room ~db in
  (* Cross-Connector link: Ada Teams + Slack share one Principal. *)
  let principal_of key =
    match assert_ok (S.get_connector_actor ~db ~key) with
    | Some a -> a.principal_id
    | None -> Alcotest.fail "connector actor missing"
  in
  Alcotest.(check string)
    "teams actor → ada"
    (P.principal_id_to_string fx.ada)
    (P.principal_id_to_string (principal_of fx.key_ada_teams));
  Alcotest.(check string)
    "slack actor → same ada"
    (P.principal_id_to_string fx.ada)
    (P.principal_id_to_string (principal_of fx.key_ada_slack));
  (* Capture delayed jobs for linked users; unlinked Carol has no binding. *)
  let snap_ada =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:fx.key_ada_teams
         ~delayed_job_id:"job_ada" ~account_binding_id:fx.bind_ada.id
         ~room_id:shared_room ~now:fixed_now ())
  in
  let snap_bob =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:fx.key_bob
         ~delayed_job_id:"job_bob" ~account_binding_id:fx.bind_bob.id
         ~room_id:shared_room ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check (option string))
    "room is context only" (Some shared_room) snap_ada.source.room_id;
  Alcotest.(check bool)
    "snapshots never authority" false
    (A.is_authority snap_ada || A.is_authority snap_bob);
  Alcotest.(check string)
    "ada principal" "prin_ada"
    (P.principal_id_to_string snap_ada.lineage.principal_id);
  Alcotest.(check string)
    "bob principal" "prin_bob"
    (P.principal_id_to_string snap_bob.lineage.principal_id);
  (* Carol has no GitHub binding; preference is none (below). Capturing with
     Ada's binding id freezes Carol as initiating Principal evidence, but live
     authority fails closed on account owner mismatch. *)
  let snap_carol =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:fx.key_carol
         ~delayed_job_id:"job_carol" ~account_binding_id:fx.bind_ada.id
         ~room_id:shared_room ~now:fixed_now ())
  in
  Alcotest.(check string)
    "carol evidence principal" "prin_carol"
    (P.principal_id_to_string snap_carol.lineage.principal_id);
  let auth_carol = assert_ok (A.re_resolve_current_authority ~db snap_carol) in
  Alcotest.(check bool) "carol cannot use ada binding" false auth_carol.usable;
  Alcotest.(check bool)
    "owner mismatch" true
    (List.exists
       (function A.Account_owner_mismatch _ -> true | _ -> false)
       auth_carol.breaks);
  (* Borrowed identity refused. *)
  (match
     Job.assert_not_borrowed_identity ~initiating:fx.key_ada_teams
       ~claimed:fx.key_bob
   with
  | Ok () -> Alcotest.fail "must refuse borrowed identity"
  | Error e ->
      Alcotest.(check bool)
        "borrow language" true
        (contains e "another participant" || contains e "claimed"
       || contains e "borrow"));
  (* Outbox: Ada enqueue then Bob conflict. *)
  let intent = sample_intent ~id:"ghdi_shared_p21" () in
  let entry =
    assert_ok
      (O.enqueue ~db ~room_id:shared_room ~item_key:intent.item_key ~intent
         ~actor_snapshot:snap_ada ~now:fixed_now ())
  in
  (match
     O.enqueue ~db ~room_id:shared_room ~item_key:intent.item_key ~intent
       ~actor_snapshot:snap_bob ~now:(fixed_now +. 1.) ()
   with
  | Ok _ -> Alcotest.fail "bob must not overwrite ada initiating lineage"
  | Error e ->
      Alcotest.(check bool)
        "conflict" true
        (contains e "conflict" || contains e "borrow" || contains e "refuses"
       || contains e "actor"));
  (* Exec with claimed_actor = Bob fails closed. *)
  (match
     Job.prepare_execution ~db ~job_id:entry.id ~snapshot:snap_ada
       ~claimed_actor:fx.key_bob ()
   with
  | Ok _ -> Alcotest.fail "claimed_actor borrow must fail"
  | Error inv ->
      let msg = Job.string_of_exec_invalidation inv in
      Alcotest.(check bool)
        "borrow at exec" true
        (contains msg "another participant" || contains msg "claimed"
       || contains msg "borrow"));
  (* Preference: Bob room pref never selects for Ada; Carol has none. *)
  let room_scope = assert_ok (Pref.make_room_scope ~room_id:shared_room ()) in
  let bob_pref =
    assert_ok (Pref.make_preference_value ~binding_id:fx.bind_bob.id ())
  in
  ignore
    (assert_ok
       (Pref.set_preference ~db ~now:fixed_now ~principal_id:fx.bob
          ~scope:room_scope ~value:bob_pref ()));
  let ctx_ada =
    Pref.make_resolve_context ~principal_id:fx.ada ~room_id:shared_room
      ~repo_full_name:repo ()
  in
  (match assert_ok (Pref.resolve ~db ~context:ctx_ada ()) with
  | Pref.Resolved { binding; _ } ->
      Alcotest.(check string) "ada own binding" fx.bind_ada.id binding.id
  | Pref.Ambiguous _ -> Alcotest.fail "sole ada account should resolve"
  | Pref.None_eligible _ -> Alcotest.fail "ada has account");
  let ctx_carol =
    Pref.make_resolve_context ~principal_id:fx.carol ~room_id:shared_room
      ~repo_full_name:repo ()
  in
  match assert_ok (Pref.resolve ~db ~context:ctx_carol ()) with
  | Pref.None_eligible _ -> ()
  | Pref.Resolved { binding; _ } ->
      Alcotest.fail
        (Printf.sprintf "unlinked carol must not resolve to %s" binding.id)
  | Pref.Ambiguous _ -> Alcotest.fail "carol has no accounts"

(* -------------------------------------------------------------------------- *)
(* 2. Every trust adapter namespace isolation                                   *)
(* -------------------------------------------------------------------------- *)

let test_every_verified_actor_namespace () =
  with_db @@ fun db ->
  let adapters =
    [
      (P.Teams, "tenant-a", "user-same");
      (P.Slack, "ws-a", "user-same");
      (P.Discord, "guild-a", "user-same");
      (P.Telegram, "bot-a", "user-same");
      (P.Web, "https://issuer.example", "sub-same");
      (P.Cli, "cli-issuer", "cli-same");
      (P.Direct, "direct-issuer", "direct-same");
    ]
  in
  List.iteri
    (fun i (connector, tenant, user) ->
      let id = Printf.sprintf "prin_ns_%d" i in
      let prin = seed_principal ~db ~id () in
      let key = actor_key ~connector ~tenant ~user () in
      ignore
        (seed_actor_and_link ~db ~principal_id:prin ~key
           ~link_id:(Printf.sprintf "link_ns_%d" i)
           ~display_name:(Printf.sprintf "User %d" i)
           ());
      match assert_ok (S.get_connector_actor ~db ~key) with
      | Some actor ->
          Alcotest.(check string)
            (Printf.sprintf "%s isolated principal"
               (P.string_of_connector connector))
            id
            (P.principal_id_to_string actor.principal_id)
      | None -> Alcotest.fail "actor missing")
    adapters;
  (* Same immutable user id across connectors never collides. *)
  let keys =
    List.mapi
      (fun i (connector, tenant, user) ->
        (actor_key ~connector ~tenant ~user (), Printf.sprintf "prin_ns_%d" i))
      adapters
  in
  let principals =
    List.map
      (fun (key, expected) ->
        match assert_ok (S.get_connector_actor ~db ~key) with
        | Some actor ->
            Alcotest.(check string)
              "expected principal" expected
              (P.principal_id_to_string actor.principal_id);
            P.principal_id_to_string actor.principal_id
        | None -> Alcotest.fail "actor missing")
      keys
  in
  let uniq = List.sort_uniq String.compare principals in
  Alcotest.(check int)
    "one principal per connector namespace" (List.length adapters)
    (List.length uniq)

(* -------------------------------------------------------------------------- *)
(* 3. Action family policy matrix                                              *)
(* -------------------------------------------------------------------------- *)

let preferred_actions = [ "comment"; "label"; "assign"; "review_request" ]

let required_actions =
  [
    "review_submit";
    "issue_create";
    "issue_close";
    "issue_reopen";
    "code_change";
    "workflow_dispatch";
    "merge";
  ]

let test_every_action_family_policy () =
  List.iter
    (fun action ->
      let r = Policy.lookup ~action in
      Alcotest.(check string)
        (action ^ " preferred")
        "user_preferred"
        (Policy.attribution_to_string r.attribution);
      Alcotest.(check bool)
        (action ^ " permits app fallback")
        true
        (Policy.permits_app_fallback r.attribution))
    preferred_actions;
  List.iter
    (fun action ->
      let r = Policy.lookup ~action in
      Alcotest.(check string)
        (action ^ " required")
        "user_required"
        (Policy.attribution_to_string r.attribution);
      Alcotest.(check bool)
        (action ^ " no app fallback")
        false
        (Policy.permits_app_fallback r.attribution))
    required_actions;
  (* Aliases canonicalize into known families. *)
  Alcotest.(check string)
    "submit_review alias" "review_submit"
    (Policy.lookup ~action:"submit_review").action;
  Alcotest.(check string)
    "code_work alias" "code_change"
    (Policy.lookup ~action:"code_work").action;
  Alcotest.(check string)
    "pr_create alias" "code_change"
    (Policy.lookup ~action:"pr_create").action;
  (* Unknown fail closed as User_required / Critical. *)
  let unk = Policy.lookup ~action:"totally_unknown_mutation" in
  Alcotest.(check string)
    "unknown required" "user_required"
    (Policy.attribution_to_string unk.attribution);
  Alcotest.(check string)
    "unknown critical" "critical" (Policy.risk_tier_to_string unk.tier);
  Alcotest.(check bool) "unknown no pilot" false unk.pilot_allowed

(* -------------------------------------------------------------------------- *)
(* 4. User_preferred: user success + safe App fallback                         *)
(* -------------------------------------------------------------------------- *)

let test_user_preferred_user_and_app_fallback () =
  with_db @@ fun db ->
  let _fx = seed_shared_room ~db in
  let route =
    make_route ~id:"rt_collab"
      ~policy:(caps ~reply:true ~label:true ~assign:true ())
  in
  (* User path for comment. *)
  let comment = Collab.Comment { item_key = collab_item_key; body = "LGTM" } in
  (match
     Collab_attr.gate ~route:(Some route) ~action:comment
       ~evidence:(base_request ()) ()
   with
  | Collab_attr.Capability_denied { reason } -> Alcotest.fail reason
  | Collab_attr.Attribution { decision = Auth.Allow a; _ } ->
      Alcotest.(check string)
        "user mode" "user"
        (Auth.resolved_mode_to_string a.mode);
      Alcotest.(check bool) "not fallback" false a.used_app_fallback
  | Collab_attr.Attribution { decision = Auth.Deny d; _ } ->
      Alcotest.fail (Printf.sprintf "deny %s" d.repair.code));
  (* Visible App fallback for label when no user binding. *)
  let label =
    Collab.Label { item_key = collab_item_key; add = [ "needs-triage" ]; remove = [] }
  in
  let evidence_app =
    base_request ~action:"label" ~binding:Auth.Not_required
      ~user_authority_ok:false
      ~fallback:(Auth.fallback_context ~preview_actor:Auth.Fallback.Names_app ())
      ()
  in
  (match
     Collab_attr.gate ~route:(Some route) ~action:label ~evidence:evidence_app ()
   with
  | Collab_attr.Attribution { decision = Auth.Allow a; _ } ->
      Alcotest.(check string)
        "app mode" "app"
        (Auth.resolved_mode_to_string a.mode);
      Alcotest.(check bool) "fallback used" true a.used_app_fallback
  | Collab_attr.Attribution { decision = Auth.Deny d; _ } ->
      Alcotest.fail (Printf.sprintf "app fallback deny: %s" d.repair.code)
  | Collab_attr.Capability_denied { reason } -> Alcotest.fail reason);
  (* End-to-end plan + app dispatch without lease. *)
  let planned =
    assert_ok
      (Collab_attr.plan_with_attribution ~db ~principal:principal_ada
         ~room_id:shared_room ~action:label ~base_revision
         ~evidence:evidence_app ~route ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "plan has allow" true
    (Collab_attr.has_attribution_allow planned.plan);
  Alcotest.(check bool) "fallback on plan" true planned.staged.allow.used_app_fallback;
  let dispatched =
    match
      Collab_attr.prepare_dispatch ~db ~live:evidence_app
        ~prior:planned.staged.allow ~item_key:collab_item_key
        ~room_id:shared_room ~plan_id:planned.plan.id ~now:fixed_now ()
    with
    | Ok d -> d
    | Error e -> Alcotest.fail (Lease.string_of_denial e)
  in
  Alcotest.(check string)
    "dispatch app" "app"
    (Auth.resolved_mode_to_string dispatched.issued.mode);
  Alcotest.(check bool) "no lease on app" true (Option.is_none dispatched.issued.lease);
  secrets_absent
    (Yojson.Safe.to_string (Lease.issued_to_json dispatched.issued)
    ^ Yojson.Safe.to_string (Audit.to_json dispatched.receipt))

(* -------------------------------------------------------------------------- *)
(* 5. User_required: success with user auth; App fallback forbidden            *)
(* -------------------------------------------------------------------------- *)

let test_user_required_denial_and_user_success () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_issue"
      ~policy:(caps ~close:true ~extra:[ ("allow_create", true) ] ())
  in
  let create =
    Issue.Create
      {
        repo_full_name = repo;
        title = "Flaky CI";
        body = Some "repro";
        labels = [ "bug" ];
      }
  in
  let live : Issue_attr.live_revalidation =
    {
      item_present = true;
      repo_present = true;
      current_state = Some "open";
      already_applied = false;
      target_revision = Some "state-rev-1";
      planned_target_revision = Some "state-rev-1";
    }
  in
  (* Without user auth and pilot off → deny (no App/PAT fallback). *)
  (match
     Issue_attr.authorize_preview ~db ~action:create ~route:(Some route)
       ~pilot:Issue.default_pilot_gate ~user_auth_available:false
       ~auth:(base_request ~action:"issue_create" ())
       ~live ~room_id:shared_room ~now:fixed_now ()
   with
  | Ok _ -> Alcotest.fail "user_required without auth must deny"
  | Error d ->
      let msg = Issue_attr.string_of_preview_deny d in
      Alcotest.(check bool)
        "denial language" true
        (contains msg "user" || contains msg "pilot" || contains msg "required"
       || contains msg "auth" || contains msg "p21"));
  (* App-mode evidence is forbidden on User_required production path. *)
  let app_evidence =
    base_request ~action:"issue_create" ~binding:Auth.Not_required
      ~user_authority_ok:false
      ~fallback:(Auth.fallback_context ~preview_actor:Auth.Fallback.Names_app ())
      ~confirmation_required:true ~confirmation_satisfied:true
      ~confirmation_id:(Some "conf_1") ()
  in
  (match
     Issue_attr.authorize_preview ~db ~action:create ~route:(Some route)
       ~pilot:Issue.default_pilot_gate ~user_auth_available:true ~auth:app_evidence
       ~live ~room_id:shared_room ~now:(fixed_now +. 1.) ()
   with
  | Ok o when o.used_app_fallback || Auth.resolved_mode_to_string o.mode = "app"
    ->
      Alcotest.fail "User_required must never resolve to App fallback"
  | Ok _ ->
      (* Some paths deny at authorize before returning allow — either is fine
         so long as mode is not app fallback. *)
      ()
  | Error d ->
      let msg = Issue_attr.string_of_preview_deny d in
      Alcotest.(check bool)
        "app forbidden language" true
        (contains msg "user_required" || contains msg "forbidden"
       || contains msg "binding" || contains msg "user" || contains msg "app"));
  (* User path succeeds. *)
  let user_auth =
    base_request ~action:"issue_create" ~confirmation_required:true
      ~confirmation_satisfied:true ~confirmation_id:(Some "conf_1") ()
  in
  match
    Issue_attr.authorize_preview ~db ~action:create ~route:(Some route)
      ~pilot:Issue.default_pilot_gate ~user_auth_available:true ~auth:user_auth
      ~live ~room_id:shared_room ~now:(fixed_now +. 2.) ()
  with
  | Ok ok ->
      Alcotest.(check string)
        "user mode" "user"
        (Auth.resolved_mode_to_string ok.mode);
      Alcotest.(check bool) "no app fallback" false ok.used_app_fallback;
      Alcotest.(check string) "action" "issue_create" ok.policy_action
  | Error d ->
      Alcotest.fail ("user preview: " ^ Issue_attr.string_of_preview_deny d)

(* -------------------------------------------------------------------------- *)
(* 6. Delayed lineage: ordinary refresh vs merge/split/relink/revoke           *)
(* -------------------------------------------------------------------------- *)

let test_delayed_lineage_refresh_and_breaks () =
  with_db @@ fun db ->
  let fx = seed_shared_room ~db in
  let snap =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:fx.key_ada_teams
         ~delayed_job_id:"job_lineage" ~account_binding_id:fx.bind_ada.id
         ~room_id:shared_room ~now:fixed_now ())
  in
  let prior =
    authorize_allow ~actor_snapshot_id:(Some snap.id) ~vault_generation:1 ()
  in
  let pin =
    assert_ok
      (Delayed.make_pin ~job_id:"job_lineage" ~snapshot:snap ~allow:prior
         ~expected_github_actor:
           (Audit.Numeric_user
              { host = "github.com"; app_id = 42; github_user_id = 1001L })
         ~confirmation_id:"conf_delayed" ())
  in
  (* Ordinary refresh: generation advance within same lineage is allowed. *)
  let live_refresh =
    base_request
      ~binding:(Auth.Selected (selected ~vault_generation:4 ()))
      ~actor_snapshot_id:(Some snap.id) ~pin:Auth.empty_revision_pin ()
  in
  (match Delayed.prepare_execution ~db ~job_id:"job_lineage" ~pin ~live:live_refresh ()
   with
  | Ok env ->
      Alcotest.(check bool) "gen advanced" true env.generation_advanced;
      Alcotest.(check (option int))
        "fresh gen" (Some 4) env.fresh_allow.revisions.vault_generation;
      Alcotest.(check string)
        "mode continuous" "user"
        (Auth.resolved_mode_to_string env.fresh_allow.mode)
  | Error inv -> Alcotest.fail (Delayed.string_of_exec_invalidation inv));
  (* Relink (lineage id change) fails closed. *)
  let live_relink =
    base_request
      ~binding:
        (Auth.Selected
           (selected ~lineage_id:"lin_ada_RELINKED" ~vault_generation:1
              ~lineage_matches_pin:false ()))
      ~actor_snapshot_id:(Some snap.id) ()
  in
  (match
     Delayed.prepare_execution ~db ~job_id:"job_lineage" ~pin ~live:live_relink
       ()
   with
  | Ok _ -> Alcotest.fail "relink must break lineage"
  | Error inv ->
      let msg = Delayed.string_of_exec_invalidation inv in
      Alcotest.(check bool)
        "lineage language" true
        (contains msg "lineage" || contains msg "binding"));
  (* Merge adoption: survivor without exclusive GitHub slot; loser has one
     binding (deterministic adoption, no hard account collision). *)
  let older =
    seed_principal ~db ~id:"prin_merge_old" ~created_at:"2026-01-01T00:00:00Z"
      ()
  in
  let newer =
    seed_principal ~db ~id:"prin_merge_new" ~created_at:"2026-06-01T00:00:00Z"
      ()
  in
  let key_old = actor_key ~user:"merge-old" () in
  let key_new =
    actor_key ~connector:P.Slack ~tenant:"ws-merge" ~user:"merge-new" ()
  in
  ignore
    (seed_actor_and_link ~db ~principal_id:older ~key:key_old
       ~link_id:"link_merge_old" ~display_name:"Old" ());
  ignore
    (seed_actor_and_link ~db ~principal_id:newer ~key:key_new
       ~link_id:"link_merge_new" ~display_name:"New" ());
  let bind_loser =
    seed_binding ~db ~principal_id:newer ~id:"gh_merge_loser"
      ~github_user_id:5555L ~lineage_id:"lin_merge_loser" ~login:"mergeloser" ()
  in
  let snap_pre_merge =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key_new
         ~delayed_job_id:"job_pre_merge" ~account_binding_id:bind_loser.id
         ~room_id:shared_room ~now:fixed_now ())
  in
  Alcotest.(check string)
    "pre-merge principal newer" "prin_merge_new"
    (P.principal_id_to_string snap_pre_merge.lineage.principal_id);
  (match
     Merge.apply_merge ~db ~left_id:older ~right_id:newer
       ~link_tx_id:"ltx_p21" ~merge_id:"pmerge_p21" ~now:fixed_now ()
   with
  | Merge.Applied receipt ->
      Alcotest.(check string)
        "survivor older" "prin_merge_old"
        (P.principal_id_to_string receipt.survivor_id)
  | Merge.Idempotent _ -> Alcotest.fail "unexpected idempotent"
  | Merge.Refused { reason; _ } -> Alcotest.fail ("merge refused: " ^ reason)
  | Merge.Stale_revision s -> Alcotest.fail ("stale: " ^ s));
  Alcotest.(check string)
    "historical newer frozen" "prin_merge_new"
    (P.principal_id_to_string snap_pre_merge.lineage.principal_id);
  let auth_merge =
    assert_ok (A.re_resolve_current_authority ~db snap_pre_merge)
  in
  Alcotest.(check bool)
    "followed merge alias" true auth_merge.followed_merge_alias;
  Alcotest.(check bool) "usable after merge" true auth_merge.usable;
  (match
     Job.prepare_execution ~db ~job_id:"job_pre_merge" ~snapshot:snap_pre_merge
       ()
   with
  | Ok env ->
      Alcotest.(check bool)
        "exec usable after merge" true env.live_authority.usable
  | Error inv ->
      Alcotest.fail
        ("exec after merge: " ^ Job.string_of_exec_invalidation inv));
  (* Split: break authority; evidence stays on source. *)
  let snap_split =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:fx.key_ada_slack
         ~delayed_job_id:"job_split" ~account_binding_id:fx.bind_ada.id
         ~room_id:shared_room ~now:(fixed_now +. 2.) ())
  in
  (match
     Unlink.unlink_actor ~db ~source_principal_id:fx.ada
       ~actor_key:fx.key_ada_slack ~plan_id:"psplit_p21" ~unlink_id:"punlink_p21"
       ~now:(fixed_now +. 2.) ()
   with
  | Unlink.Applied _ -> ()
  | Unlink.Idempotent _ -> ()
  | Unlink.Refused { reason; _ } -> Alcotest.fail ("split refused: " ^ reason)
  | Unlink.Stale_revision s -> Alcotest.fail ("stale: " ^ s));
  let auth_split = assert_ok (A.re_resolve_current_authority ~db snap_split) in
  Alcotest.(check bool) "unusable after split" false auth_split.usable;
  Alcotest.(check string)
    "split evidence frozen" "prin_ada"
    (P.principal_id_to_string snap_split.lineage.principal_id);
  (match
     Job.prepare_execution ~db ~job_id:"job_split" ~snapshot:snap_split ()
   with
  | Ok _ -> Alcotest.fail "split must fail closed at exec"
  | Error inv ->
      let msg = Job.string_of_exec_invalidation inv in
      Alcotest.(check bool)
        "split exec deny" true
        (contains msg "unusable" || contains msg "authority"
       || contains msg "principal" || contains msg "refused"));
  (* Revoke binding: authority breaks, evidence frozen. *)
  let snap_rev =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:fx.key_ada_teams
         ~delayed_job_id:"job_rev" ~account_binding_id:fx.bind_ada.id
         ~room_id:shared_room ~now:(fixed_now +. 3.) ())
  in
  ignore
    (assert_ok
       (B.update_authorization_status ~db ~id:fx.bind_ada.id ~status:B.Revoked
          ~now:(fixed_now +. 4.) ()));
  let auth_rev = assert_ok (A.re_resolve_current_authority ~db snap_rev) in
  Alcotest.(check bool) "unusable after revoke" false auth_rev.usable;
  Alcotest.(check string)
    "revoke evidence frozen" "prin_ada"
    (P.principal_id_to_string snap_rev.lineage.principal_id)

(* -------------------------------------------------------------------------- *)
(* 7. Personal token exclusion from runners / shell / Git                      *)
(* -------------------------------------------------------------------------- *)

let test_personal_token_exclusion () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let route =
    make_route ~id:"rt_code" ~policy:(caps ~extra:[ (Code.capability_key, true) ] ())
  in
  let head_sha = "abc123def4567890abcdef1234567890abcdef12" in
  let head_branch = "clawq/p21-int-fix" in
  let family =
    Code_attr.Code_work
      {
        repo_full_name = repo;
        base_branch = "main";
        scope = "fix attribution isolation";
        runner = "codex";
        output_authority = "principal:ada";
        branch_prefix = Code.default_branch_prefix;
        head_branch = Some head_branch;
        item_key = Some collab_item_key;
        related_issue = None;
      }
  in
  let auth =
    base_request ~action:"code_change" ~confirmation_required:true
      ~confirmation_satisfied:true ~confirmation_id:(Some "conf_code_1") ()
  in
  let live : Code_attr.live_revalidation =
    {
      repo_present = true;
      base_present = true;
      already_applied = false;
      current_refs = None;
      code_work_result_status = None;
      code_work_finished_at = None;
      max_age_seconds = None;
      target_revision = Some head_sha;
      planned_target_revision = Some head_sha;
    }
  in
  let preview =
    match
      Code_attr.authorize_preview ~db ~family ~route:(Some route)
        ~pilot:Code.default_pilot_gate ~user_auth_available:true ~auth ~live
        ~room_id:shared_room ~now:fixed_now ()
    with
    | Ok o -> o
    | Error d ->
        Alcotest.fail ("code preview: " ^ Code_attr.string_of_preview_deny d)
  in
  Alcotest.(check string)
    "user mode" "user"
    (Auth.resolved_mode_to_string preview.mode);
  let disp =
    match
      Code_attr.dispatch ~db ~family ~live_auth:auth ~prior:preview.allow ~live
        ~vault_id:vault.id ~expected:acct ~room_id:shared_room
        ~receipt_id:"rcpt_code_p21" ~now:fixed_now ()
    with
    | Ok d -> d
    | Error d ->
        Alcotest.fail ("code dispatch: " ^ Code_attr.string_of_dispatch_deny d)
  in
  Alcotest.(check bool) "has lease" true disp.has_user_lease;
  (match disp.issued.lease with
  | None -> Alcotest.fail "expected lease"
  | Some lease -> (
      match Token_lease.assert_non_http_refused lease with
      | Ok () -> ()
      | Error e -> Alcotest.fail e));
  let materials =
    Code_attr.isolation_materials_of_issued ~issued:disp.issued ()
  in
  (match Token_lease.assert_materials_token_free ~materials with
  | Ok () -> ()
  | Error d -> Alcotest.fail (Token_lease.string_of_denial d));
  let issued_json = Yojson.Safe.to_string (Lease.issued_to_json disp.issued) in
  secrets_absent issued_json;
  (* Dirty Git transport / shell / runner env materials are denied. *)
  List.iter
    (fun (surface, payload) ->
      match
        Code_attr.enforce_token_isolation ~db
          ~lease:(Option.get disp.issued.lease)
          ~materials:[ (surface, payload) ]
          ~room_id:shared_room ~now:fixed_now ()
      with
      | Error reason ->
          Alcotest.(check bool)
            "scan deny" true
            (contains reason "token" || contains reason "forbidden"
           || contains reason "refuse")
      | Ok _ ->
          Alcotest.fail
            (Printf.sprintf "dirty %s material must be denied"
               (match surface with
               | Token_lease.Git_transport -> "git"
               | Token_lease.Shell -> "shell"
               | Token_lease.Runner_env -> "runner"
               | _ -> "surface")))
    [
      ( Token_lease.Git_transport,
        "git push https://x-access-token:" ^ sample_tokens.access_token
        ^ "@github.com/acme/widget.git" );
      ( Token_lease.Shell,
        "export GITHUB_TOKEN=" ^ sample_tokens.access_token );
      ( Token_lease.Runner_env,
        "GH_TOKEN=" ^ sample_tokens.access_token );
    ];
  Code_attr.revoke_issued_lease disp.issued

(* -------------------------------------------------------------------------- *)
(* 8. Every action family native receipt + isolation                           *)
(* -------------------------------------------------------------------------- *)

type family_case = {
  action : string;
  event : string;
  env_action : string option;
  family : E.family;
  kind : E.item_kind option;
  number : int option;
  merged : bool option;
  state : string option;
  github_user_id : int64;
  login : string;
  principal : string;
}

let all_family_cases =
  let ada = (1001L, "ada", "prin_ada") in
  let bob = (1002L, "bob", "prin_bob") in
  let mk action event env_action family kind number merged state who =
    let github_user_id, login, principal = who in
    {
      action;
      event;
      env_action;
      family;
      kind;
      number;
      merged;
      state;
      github_user_id;
      login;
      principal;
    }
  in
  [
    mk "comment" "issue_comment" (Some "created") E.Comment
      (Some E.Pull_request) (Some 42) None (Some "open") ada;
    mk "label" "pull_request" (Some "labeled") E.State_update
      (Some E.Pull_request) (Some 42) None (Some "open") ada;
    mk "assign" "pull_request" (Some "assigned") E.State_update
      (Some E.Pull_request) (Some 42) None (Some "open") bob;
    mk "request_reviewers" "pull_request" (Some "review_requested") E.Review
      (Some E.Pull_request) (Some 42) None (Some "open") ada;
    mk "submit_review" "pull_request_review" (Some "submitted") E.Review
      (Some E.Pull_request) (Some 42) None (Some "open") bob;
    mk "issue_create" "issues" (Some "opened") E.Lifecycle (Some E.Issue)
      (Some 7) None (Some "open") ada;
    mk "issue_close" "issues" (Some "closed") E.Lifecycle (Some E.Issue)
      (Some 7) None (Some "closed") ada;
    mk "issue_reopen" "issues" (Some "reopened") E.Lifecycle (Some E.Issue)
      (Some 7) None (Some "open") bob;
    mk "workflow_dispatch" "workflow_run" (Some "requested") E.Ci None None None
      None ada;
    mk "code_work" "pull_request" (Some "opened") E.Lifecycle
      (Some E.Pull_request) (Some 42) None (Some "open") ada;
    mk "pr_create" "pull_request" (Some "opened") E.Lifecycle
      (Some E.Pull_request) (Some 42) None (Some "open") bob;
    mk "merge" "pull_request" (Some "closed") E.Lifecycle (Some E.Pull_request)
      (Some 42) (Some true) (Some "closed") ada;
    mk "room_background_work" "issue_comment" (Some "created") E.Comment
      (Some E.Pull_request) (Some 42) None (Some "open") bob;
  ]

let item_key_for_case (c : family_case) =
  match (c.kind, c.number) with
  | Some E.Issue, Some n -> Some (Printf.sprintf "issue:acme/widget:%d" n)
  | Some E.Pull_request, Some n -> Some (Printf.sprintf "pr:acme/widget:%d" n)
  | _ -> None

let test_every_family_receipt_and_isolation () =
  with_db @@ fun db ->
  (* Process one family at a time so overlapping webhook fingerprints
     (comment vs room_background_work, code_work vs pr_create) cannot steal
     another open correlation mid-suite. *)
  List.iteri
    (fun i (fc : family_case) ->
      let ik = item_key_for_case fc in
      let snap =
        make_snapshot
          ~snapshot_id:(Printf.sprintf "snap_fam_%d" i)
          ~principal:fc.principal ~user:("aad-" ^ fc.login)
          ~display_name:(String.capitalize_ascii fc.login)
          ~binding_id:("gh_" ^ fc.login) ~lineage_id:("lin_" ^ fc.login)
          ~github_user_id:fc.github_user_id ()
      in
      let attr_id = Printf.sprintf "ghattr_p21_%s_%d" fc.action i in
      let receipt_id = Printf.sprintf "receipt_p21_%s_%d" fc.action i in
      let corr =
        assert_ok
          (Rec.record_from_native_receipt ~db ~room_id:shared_room
             ~action:fc.action ~actor_mode:"user" ?item_key:ik
             ~plan_id:(Printf.sprintf "plan_%s_%d" fc.action i)
             ~receipt_id ~attribution_receipt_id:attr_id
             ~requested_mode:"user" ~resolved_mode:"user" ~actor_snapshot:snap
             ~expected_github_login:fc.login ~github_user_id:fc.github_user_id
             ~native_actor_kind:"user"
             ~now:(fixed_now +. float_of_int i)
             ())
      in
      Alcotest.(check string)
        ("canonical " ^ fc.action)
        (Rec.canonicalize_action fc.action)
        corr.action;
      Alcotest.(check bool)
        "receipt never authority" false (Rec.snapshot_is_authority corr);
      (match corr.actor_snapshot with
      | Some s -> secrets_absent (Yojson.Safe.to_string (A.to_json s))
      | None -> ());
      Alcotest.(check (option string))
        "attr receipt linked" (Some attr_id) corr.attribution_receipt_id;
      (* Unrelated human cannot close this open receipt. *)
      let stranger =
        make_envelope ~event:fc.event ~action:fc.env_action ~family:fc.family
          ~kind:fc.kind ~number:fc.number ~merged:fc.merged ~state:fc.state
          ~delivery_id:(Some (Printf.sprintf "deliv_stranger_%d" i))
          ~actor_login:(Some "carol") ~actor_type:(Some "User")
          ~actor_id:(Some 9999) ()
      in
      let r0 =
        Rec.reconcile_webhook ~db ~room_id:shared_room ~envelope:stranger
          ~now:(fixed_now +. 50. +. float_of_int i)
          ()
      in
      Alcotest.(check string)
        ("stranger ignored " ^ fc.action)
        "ignored_human_event" (result_tag r0);
      let env =
        make_envelope ~event:fc.event ~action:fc.env_action ~family:fc.family
          ~kind:fc.kind ~number:fc.number ~merged:fc.merged ~state:fc.state
          ~delivery_id:(Some (Printf.sprintf "deliv_p21_%s_%d" fc.action i))
          ~actor_login:(Some (String.capitalize_ascii fc.login))
          ~actor_type:(Some "User")
          ~actor_id:(Some (Int64.to_int fc.github_user_id))
          ()
      in
      let r1 =
        Rec.reconcile_webhook ~db ~room_id:shared_room ~envelope:env
          ~now:(fixed_now +. 100. +. float_of_int i)
          ()
      in
      Alcotest.(check string)
        ("family " ^ fc.action ^ " closes")
        "closed_first" (result_tag r1);
      (match r1 with
      | Rec.Closed { correlation = c; _ } -> (
          Alcotest.(check string)
            "resolved user" "user" (Rec.resolved_attribution c);
          Alcotest.(check (option string))
            "closed own receipt" (Some receipt_id) c.receipt_id;
          match c.actor_snapshot with
          | Some s ->
              Alcotest.(check string)
                "principal frozen" fc.principal
                (P.principal_id_to_string s.lineage.principal_id);
              Alcotest.(check bool) "never authority" false (A.is_authority s)
          | None -> Alcotest.fail ("missing snap for " ^ fc.action))
      | _ -> Alcotest.fail ("expected close for " ^ fc.action));
      let r2 =
        Rec.reconcile_webhook ~db ~room_id:shared_room ~envelope:env
          ~now:(fixed_now +. 200. +. float_of_int i)
          ()
      in
      Alcotest.(check string)
        ("family " ^ fc.action ^ " once")
        "already_closed" (result_tag r2);
      match
        Rec.get_by_attribution_receipt_id ~db ~attribution_receipt_id:attr_id
      with
      | None -> Alcotest.fail "lookup by attribution receipt"
      | Some loaded ->
          Alcotest.(check (option string))
            "loaded receipt" (Some receipt_id) loaded.receipt_id)
    all_family_cases

(* -------------------------------------------------------------------------- *)
(* 9. Cross-Principal receipt isolation in shared Room                         *)
(* -------------------------------------------------------------------------- *)

let test_cross_principal_receipt_isolation () =
  with_db @@ fun db ->
  let snap_ada =
    make_snapshot ~snapshot_id:"actorsnap_rcpt_ada" ~principal:"prin_ada"
      ~user:"aad-ada" ~display_name:"Ada" ~binding_id:"gh_ada"
      ~lineage_id:"lin_ada" ~github_user_id:1001L ()
  in
  let snap_bob =
    make_snapshot ~snapshot_id:"actorsnap_rcpt_bob" ~principal:"prin_bob"
      ~user:"aad-bob" ~display_name:"Bob" ~binding_id:"gh_bob"
      ~lineage_id:"lin_bob" ~github_user_id:1002L ()
  in
  let corr_ada =
    Rec.make_correlation ~room_id:shared_room ~action:"comment"
      ~actor_mode:"user" ~item_key ~plan_id:"plan_ada" ~receipt_id:"rcpt_ada"
      ~requested_mode:"user" ~resolved_mode:"user" ~actor_snapshot:snap_ada
      ~expected_github_login:"ada" ~github_user_id:1001L
      ~native_actor_kind:"user" ()
  in
  let corr_bob =
    Rec.make_correlation ~room_id:shared_room ~action:"comment"
      ~actor_mode:"user" ~item_key ~plan_id:"plan_bob" ~receipt_id:"rcpt_bob"
      ~requested_mode:"user" ~resolved_mode:"user" ~actor_snapshot:snap_bob
      ~expected_github_login:"bob" ~github_user_id:1002L
      ~native_actor_kind:"user" ()
  in
  assert_ok (Rec.record_correlation ~db ~correlation:corr_ada ~now:fixed_now ());
  assert_ok
    (Rec.record_correlation ~db ~correlation:corr_bob ~now:(fixed_now +. 0.1) ());
  let bob_evt =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "deliv-bob-iso")
      ~actor_login:(Some "bob") ~actor_type:(Some "User") ~merged:None
      ~state:(Some "open") ~actor_id:(Some 1002) ()
  in
  let r =
    Rec.reconcile_webhook ~db ~room_id:shared_room ~envelope:bob_evt
      ~now:(fixed_now +. 1.) ()
  in
  (match r with
  | Rec.Closed { correlation = c; _ } -> (
      Alcotest.(check (option string))
        "bob receipt" (Some "rcpt_bob") c.receipt_id;
      match c.actor_snapshot with
      | Some s ->
          Alcotest.(check string)
            "bob principal" "prin_bob"
            (P.principal_id_to_string s.lineage.principal_id);
          Alcotest.(check bool) "not ada snap" true (s.id <> snap_ada.id)
      | None -> Alcotest.fail "bob snapshot missing")
  | _ -> Alcotest.fail "expected bob closed");
  match Rec.get_by_receipt_id ~db ~receipt_id:"rcpt_ada" with
  | None -> Alcotest.fail "ada still present"
  | Some open_ada -> (
      match open_ada.actor_snapshot with
      | Some s ->
          Alcotest.(check string)
            "ada intact" "prin_ada"
            (P.principal_id_to_string s.lineage.principal_id)
      | None -> Alcotest.fail "ada snapshot missing")

(* -------------------------------------------------------------------------- *)
(* 10. Restart / retry preserves snapshot and re-resolves                      *)
(* -------------------------------------------------------------------------- *)

let test_restart_retry_preserves_snapshot () =
  with_db @@ fun db ->
  let fx = seed_shared_room ~db in
  let intent = sample_intent ~id:"ghdi_restart_p21" () in
  let snap =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:fx.key_ada_teams
         ~delayed_job_id:intent.id ~account_binding_id:fx.bind_ada.id
         ~room_id:shared_room ~now:fixed_now ())
  in
  let entry =
    assert_ok
      (O.enqueue ~db ~room_id:shared_room ~item_key:intent.item_key ~intent
         ~actor_snapshot:snap ~now:fixed_now ())
  in
  let claimed = assert_ok (O.claim_due ~db ~now:fixed_now ~limit:10 ()) in
  Alcotest.(check int) "claimed 1" 1 (List.length claimed);
  (match assert_ok (O.snapshot_of_entry (List.hd claimed)) with
  | Some s ->
      Alcotest.(check string) "claim keeps snap" snap.id s.id;
      Alcotest.(check string)
        "principal" "prin_ada"
        (P.principal_id_to_string s.lineage.principal_id)
  | None -> Alcotest.fail "snapshot lost on claim");
  let after_fail =
    assert_ok
      (O.mark_failure ~db ~id:entry.id ~error:"timeout" ~now:(fixed_now +. 1.)
         ())
  in
  (match assert_ok (O.snapshot_of_entry after_fail) with
  | Some s -> Alcotest.(check string) "retry keeps snap" snap.id s.id
  | None -> Alcotest.fail "snapshot lost on failure");
  match
    Job.prepare_execution_of_json ~db ~job_id:entry.id
      ~snapshot_json:after_fail.actor_snapshot_json ~require_snapshot:true ()
  with
  | Ok (Some env) ->
      Alcotest.(check bool) "usable" true env.live_authority.usable;
      Alcotest.(check string) "job" entry.id env.job_id
  | Ok None -> Alcotest.fail "expected envelope"
  | Error e -> Alcotest.fail e

(* -------------------------------------------------------------------------- *)
(* 11. User collab dispatch with vault lease stays secret-free                 *)
(* -------------------------------------------------------------------------- *)

let test_user_collab_dispatch_secret_free () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let route =
    make_route ~id:"rt_reply" ~policy:(caps ~reply:true ())
  in
  let action =
    Collab.Comment { item_key = collab_item_key; body = "native user comment" }
  in
  let evidence = base_request () in
  let planned =
    assert_ok
      (Collab_attr.plan_with_attribution ~db ~principal:principal_ada
         ~room_id:shared_room ~action ~base_revision ~evidence ~route
         ~now:fixed_now ())
  in
  let dispatched =
    match
      Collab_attr.prepare_dispatch ~db ~live:evidence
        ~prior:planned.staged.allow ~vault_id:vault.id ~expected:acct
        ~item_key:collab_item_key ~room_id:shared_room
        ~plan_id:planned.plan.id ~github_user_id:1001L ~now:fixed_now ()
    with
    | Ok d -> d
    | Error e -> Alcotest.fail (Lease.string_of_denial e)
  in
  Alcotest.(check string)
    "mode" "user"
    (Auth.resolved_mode_to_string dispatched.issued.mode);
  Alcotest.(check bool) "has lease" true (Option.is_some dispatched.issued.lease);
  secrets_absent
    (Yojson.Safe.to_string (Lease.issued_to_json dispatched.issued)
    ^ Yojson.Safe.to_string (Audit.to_json dispatched.receipt)
    ^ Yojson.Safe.to_string planned.plan.apply_payload.data);
  match
    Token_lease.with_token ~db ~keys ~now:fixed_now
      ~lease:(Option.get dispatched.issued.lease)
      ~f:(fun ~access_token -> access_token = sample_tokens.access_token)
      ()
  with
  | Ok true -> ()
  | Ok false -> Alcotest.fail "token mismatch"
  | Error d -> Alcotest.fail (Token_lease.string_of_denial d)

(* -------------------------------------------------------------------------- *)
(* 12. Minimal-build / full-runtime surface notes                              *)
(* -------------------------------------------------------------------------- *)

let test_minimal_build_and_module_surface () =
  (* Full-build integration suite documents modules required for P21 pilot
     paths. Minimal binary disables integrations with "disabled in minimal
     build"; these assertions pin the full-runtime contract. *)
  Alcotest.(check int) "policy families" 11 (List.length (Policy.defaults ()));
  Alcotest.(check bool)
    "default outbox max age 24h" true
    (O.default_max_age_seconds = 86400.);
  Alcotest.(check string)
    "code_change capability" "code_change" Code.capability_key;
  Alcotest.(check string)
    "branch prefix" "clawq/" Code.default_branch_prefix;
  Alcotest.(check bool)
    "issue pilot name set" true
    (String.length Issue.default_pilot_gate.pilot_name > 0);
  Alcotest.(check bool)
    "code pilot name set" true
    (String.length Code.default_pilot_gate.pilot_name > 0);
  (* User_required families never permit App as fallback. *)
  List.iter
    (fun action ->
      Alcotest.(check bool)
        (action ^ " no fallback")
        false
        (Policy.permits_app_fallback (Policy.lookup ~action).attribution))
    required_actions;
  Alcotest.(check bool)
    "preferred do permit" true
    (List.for_all
       (fun a -> Policy.permits_app_fallback (Policy.lookup ~action:a).attribution)
       preferred_actions)

(* -------------------------------------------------------------------------- *)
(* Suite                                                                       *)
(* -------------------------------------------------------------------------- *)

let suite =
  [
    ( "shared room linked and unlinked isolation",
      `Quick,
      test_shared_room_linked_and_unlinked_isolation );
    ( "every verified actor namespace",
      `Quick,
      test_every_verified_actor_namespace );
    ("every action family policy", `Quick, test_every_action_family_policy);
    ( "user preferred user and app fallback",
      `Quick,
      test_user_preferred_user_and_app_fallback );
    ( "user required denial and user success",
      `Quick,
      test_user_required_denial_and_user_success );
    ( "delayed lineage refresh and breaks",
      `Quick,
      test_delayed_lineage_refresh_and_breaks );
    ("personal token exclusion", `Quick, test_personal_token_exclusion);
    ( "every family receipt and isolation",
      `Quick,
      test_every_family_receipt_and_isolation );
    ( "cross principal receipt isolation",
      `Quick,
      test_cross_principal_receipt_isolation );
    ( "restart retry preserves snapshot",
      `Quick,
      test_restart_retry_preserves_snapshot );
    ( "user collab dispatch secret free",
      `Quick,
      test_user_collab_dispatch_secret_free );
    ( "minimal build and module surface",
      `Quick,
      test_minimal_build_and_module_surface );
  ]
