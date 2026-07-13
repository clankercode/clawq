(** Cross-Principal isolation and delayed-work attribution (P21.M1.E3.T004).

    Integration-style pure/sqlite suite covering every trust adapter, shared
    Rooms, tenant namespaces, rename, verified link / deterministic adoption,
    duplicate-account conflict, preference ambiguity, unlink/split/revoke,
    restart/retry, stale snapshots, receipts/webhooks, and legacy rows.

    Invariants under test:
    - No selection uses Room history or another credential/participant.
    - Historical Actor evidence remains stable across lifecycle events.
    - Current authority follows only valid live lineage (merge alias ok;
      split/unlink/revoke break pinned authority). *)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding
module A = Actor_snapshot
module M = Principal_merge
module U = Principal_unlink_split
module R = Principal_resolve
module Op = Github_account_ownership_policy
module Pref = Github_account_preference
module Job = Github_durable_job_actor_attribution
module O = Github_delivery_outbox
module D = Github_delivery_intent
module Proj = Github_item_projection
module E = Github_event_envelope
module Rec = Github_action_reconcile
module L = Principal_legacy_migrate
module J = Github_room_event_journal

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  B.ensure_schema db;
  M.ensure_schema db;
  U.ensure_schema db;
  Pref.ensure_schema db;
  O.ensure_schema db;
  L.ensure_schema db;
  J.ensure_schema db;
  Proj.ensure_schema db;
  Rec.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_400_000.0
let shared_room = "room-shared-acme"
let item_key = "pr:acme/widget:7"
let pid s = assert_ok (P.principal_id_of_string s)

let actor_key ?(connector = P.Teams) ?(tenant = "tenant-acme") ~user () =
  assert_ok
    (P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
       ~immutable_user_id:user)

let contains hay needle =
  let hay = String.lowercase_ascii hay in
  let needle = String.lowercase_ascii needle in
  try
    let _ = Str.search_forward (Str.regexp_string needle) hay 0 in
    true
  with Not_found -> false

let seed_principal ~db ~id ?(revision = 1)
    ?(created_at = "2026-01-01T00:00:00Z") () =
  let p =
    P.make_principal ~id:(pid id) ~revision ~created_at ~updated_at:created_at
      ()
  in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p));
  pid id

let seed_actor_and_link ~db ~principal_id ~key ~link_id ?(display_name = "User")
    () =
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

let sample_intent ?(id = "ghdi_iso_1") ?(room_id = shared_room)
    ?(item_key = item_key) () : D.intent =
  let proj : Proj.projection =
    {
      room_id;
      item_key;
      title = Some "Isolation PR";
      state = Some "open";
      draft = Some false;
      merged = None;
      labels = [];
      assignees = [];
      head_sha = Some "deadbeef";
      html_url = Some "https://github.com/acme/widget/pull/7";
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
    ?(merged = Some true) () : E.t =
  {
    version = E.envelope_version;
    delivery_id;
    installation_id = Some 99;
    event;
    action;
    repo_full_name = "acme/widget";
    org = Some "acme";
    item_kind = Some E.Pull_request;
    item_number = Some 7;
    item_node_id = Some "PR_kwDOISO";
    item_url = Some "https://api.github.com/repos/acme/widget/pulls/7";
    html_url = Some "https://github.com/acme/widget/pull/7";
    family;
    actor = { E.empty_actor with login = actor_login; type_ = actor_type };
    before =
      Some
        {
          E.empty_safe_state with
          title = Some "Isolation PR";
          state = Some "open";
          draft = Some false;
          merged = Some false;
          head_sha = Some "deadbeef";
        };
    after =
      Some
        {
          E.empty_safe_state with
          title = Some "Isolation PR";
          state = Some "closed";
          draft = Some false;
          merged;
          head_sha = Some "deadbeef";
        };
    transfer = None;
    received_at = Some "2026-07-01T00:00:00Z";
    event_at = Some "2026-07-01T00:00:00Z";
    head_sha = Some "deadbeef";
    unsupported = false;
    skip_reason = None;
  }

let result_tag = function
  | Rec.Closed { first_time; _ } ->
      if first_time then "closed_first" else "closed_again"
  | Rec.No_matching_receipt -> "no_matching_receipt"
  | Rec.Already_closed -> "already_closed"
  | Rec.Ignored_human_event -> "ignored_human_event"

(* -------------------------------------------------------------------------- *)
(* 1. Every trust adapter resolves in its own namespace                        *)
(* -------------------------------------------------------------------------- *)

let test_every_trust_adapter_isolated () =
  with_db @@ fun db ->
  let adapters =
    [
      (P.Teams, "tenant-a", "user-same");
      (P.Slack, "ws-a", "user-same");
      (P.Discord, "guild-a", "user-same");
      (P.Telegram, "bot-a", "user-same");
      (P.Web, "https://issuer.example", "sub-same");
    ]
  in
  let principals =
    List.map
      (fun (connector, tenant, user) ->
        let key = actor_key ~connector ~tenant ~user () in
        let principal_id =
          assert_ok (R.resolve_or_create ~db ~actor_key:key ~now:fixed_now ())
        in
        (* Second-seen is stable. *)
        let again =
          assert_ok (R.resolve_or_create ~db ~actor_key:key ~now:fixed_now ())
        in
        Alcotest.(check string)
          "stable second-seen"
          (P.principal_id_to_string principal_id)
          (P.principal_id_to_string again);
        (connector, principal_id))
      adapters
  in
  (* Each adapter-namespace yields a distinct Principal even with same user id. *)
  let ids =
    List.map (fun (_, p) -> P.principal_id_to_string p) principals
    |> List.sort_uniq String.compare
  in
  Alcotest.(check int) "five distinct principals" 5 (List.length ids);
  (* Cli bootstrap without enrolment lookup cannot invent a Principal. *)
  match
    R.resolve_bootstrap ~db
      ~provenance:
        (Principal_bootstrap.Cli_enrolled
           {
             device_id = "dev-1";
             principal_id = "prin_anyone";
             exp = fixed_now +. 3600.;
           })
      ~now:fixed_now
      ~enrolled:(fun ~device_id:_ -> None)
      ()
  with
  | R.Rejected _ -> ()
  | R.Principal _ ->
      Alcotest.fail "unenrolled CLI must not resolve as human Principal"

(* -------------------------------------------------------------------------- *)
(* 2. Same immutable user id across tenants                                   *)
(* -------------------------------------------------------------------------- *)

let test_same_ids_across_tenants () =
  with_db @@ fun db ->
  let k1 = actor_key ~connector:P.Teams ~tenant:"tenant-east" ~user:"u-42" () in
  let k2 = actor_key ~connector:P.Teams ~tenant:"tenant-west" ~user:"u-42" () in
  let p1 =
    assert_ok (R.resolve_or_create ~db ~actor_key:k1 ~now:fixed_now ())
  in
  let p2 =
    assert_ok (R.resolve_or_create ~db ~actor_key:k2 ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "different principals" true
    (not (P.principal_id_equal p1 p2));
  (* Snapshots / delayed work pin the correct namespace, not the raw user id.
     resolve_or_create already inserted actor+link rows. *)
  let snap1 =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:k1 ~delayed_job_id:"job_east"
         ~room_id:shared_room ~now:fixed_now ())
  in
  let snap2 =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:k2 ~delayed_job_id:"job_west"
         ~room_id:shared_room ~now:fixed_now ())
  in
  Alcotest.(check string)
    "east principal"
    (P.principal_id_to_string p1)
    (P.principal_id_to_string snap1.lineage.principal_id);
  Alcotest.(check string)
    "west principal"
    (P.principal_id_to_string p2)
    (P.principal_id_to_string snap2.lineage.principal_id);
  Alcotest.(check bool)
    "cannot treat as same lineage" false
    (Job.snapshots_same_initiating_lineage snap1 snap2)

(* -------------------------------------------------------------------------- *)
(* 3. Two users in one Room never borrow                                      *)
(* -------------------------------------------------------------------------- *)

let test_two_users_one_room_never_borrow () =
  with_db @@ fun db ->
  let ada = seed_principal ~db ~id:"prin_ada" () in
  let bob = seed_principal ~db ~id:"prin_bob" () in
  let key_ada = actor_key ~user:"user-ada" () in
  let key_bob = actor_key ~user:"user-bob" () in
  ignore
    (seed_actor_and_link ~db ~principal_id:ada ~key:key_ada ~link_id:"link_ada"
       ~display_name:"Ada" ());
  ignore
    (seed_actor_and_link ~db ~principal_id:bob ~key:key_bob ~link_id:"link_bob"
       ~display_name:"Bob" ());
  let bind_ada =
    seed_binding ~db ~principal_id:ada ~id:"gh_ada" ~github_user_id:1001L
      ~lineage_id:"lin_ada" ~login:"ada" ()
  in
  let bind_bob =
    seed_binding ~db ~principal_id:bob ~id:"gh_bob" ~github_user_id:1002L
      ~lineage_id:"lin_bob" ~login:"bob" ()
  in
  (* Both act in the same Room. *)
  let snap_ada =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key_ada
         ~delayed_job_id:"job_ada" ~account_binding_id:bind_ada.id
         ~room_id:shared_room ~now:fixed_now ())
  in
  let snap_bob =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key_bob
         ~delayed_job_id:"job_bob" ~account_binding_id:bind_bob.id
         ~room_id:shared_room ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check (option string))
    "room is source context only" (Some shared_room) snap_ada.source.room_id;
  Alcotest.(check bool)
    "never authority" false
    (A.is_authority snap_ada || A.is_authority snap_bob);
  (* Room history / identity rejection helpers. *)
  let room_reject =
    Job.reject_identity_from_room_history ~room_id:shared_room
  in
  Alcotest.(check bool)
    "rejects room history" true
    (contains room_reject "room");
  (match
     Job.assert_not_borrowed_identity ~initiating:key_ada ~claimed:key_bob
   with
  | Ok () -> Alcotest.fail "must refuse borrowed identity"
  | Error e ->
      Alcotest.(check bool)
        "borrow message" true
        (contains e "another participant" || contains e "claimed"));
  (* Outbox: Ada's job cannot be re-enqueued under Bob's snapshot. *)
  let intent = sample_intent ~id:"ghdi_shared_1" () in
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
        (contains e "conflict" || contains e "borrow" || contains e "refuses"));
  (* Exec with claimed_actor = Bob fails closed. *)
  (match
     Job.prepare_execution ~db ~job_id:entry.id ~snapshot:snap_ada
       ~claimed_actor:key_bob ()
   with
  | Ok _ -> Alcotest.fail "claimed_actor borrow must fail"
  | Error inv ->
      let msg = Job.string_of_exec_invalidation inv in
      Alcotest.(check bool)
        "borrow at exec" true
        (contains msg "another participant" || contains msg "claimed"));
  (* Preference: Bob's Room pref never selects for Ada. *)
  let room_scope = assert_ok (Pref.make_room_scope ~room_id:shared_room ()) in
  let bob_pref =
    assert_ok (Pref.make_preference_value ~binding_id:bind_bob.id ())
  in
  ignore
    (assert_ok
       (Pref.set_preference ~db ~now:fixed_now ~principal_id:bob
          ~scope:room_scope ~value:bob_pref ()));
  (* Ada has only one account → sole eligible, not Bob's. *)
  let ctx =
    Pref.make_resolve_context ~principal_id:ada ~room_id:shared_room
      ~repo_full_name:"acme/widget" ()
  in
  match assert_ok (Pref.resolve ~db ~context:ctx ()) with
  | Pref.Resolved { binding; _ } ->
      Alcotest.(check string) "ada own binding" bind_ada.id binding.id
  | Pref.Ambiguous _ -> Alcotest.fail "sole account should resolve"
  | Pref.None_eligible _ -> Alcotest.fail "ada has account"

(* -------------------------------------------------------------------------- *)
(* 4. Display rename freezes evidence; authority stays usable                 *)
(* -------------------------------------------------------------------------- *)

let test_rename_preserves_evidence () =
  with_db @@ fun db ->
  let prin = seed_principal ~db ~id:"prin_ren" () in
  let key = actor_key ~user:"user-ren" () in
  ignore
    (seed_actor_and_link ~db ~principal_id:prin ~key ~link_id:"link_ren"
       ~display_name:"Ada Lovelace" ());
  let snap =
    assert_ok
      (A.create_from_live ~db ~now:fixed_now ~actor_key:key
         ~source:
           { room_id = Some shared_room; session_id = None; message_id = None }
         ())
  in
  let frozen = snap.display.display_name in
  ignore
    (assert_ok
       (S.update_connector_actor ~db ~key ~now:fixed_now
          ~display:
            {
              display_name = Some "Augusta Ada King";
              avatar_url = None;
              email = None;
              extra = [];
            }
          ()));
  Alcotest.(check (option string))
    "snapshot display frozen" (Some "Ada Lovelace") frozen;
  Alcotest.(check (option string))
    "still frozen after rename" frozen snap.display.display_name;
  let auth = assert_ok (A.re_resolve_current_authority ~db snap) in
  Alcotest.(check bool) "usable after rename" true auth.usable;
  Alcotest.(check bool) "not authority" false (A.is_authority snap)

(* -------------------------------------------------------------------------- *)
(* 5. Verified link + deterministic adoption                                  *)
(* -------------------------------------------------------------------------- *)

let test_verified_link_deterministic_adoption () =
  with_db @@ fun db ->
  (* Older survivor (Teams), newer loser (Slack) — creation-order survivor. *)
  let older =
    seed_principal ~db ~id:"prin_old" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let newer =
    seed_principal ~db ~id:"prin_new" ~created_at:"2026-06-01T00:00:00Z" ()
  in
  let key_old = actor_key ~connector:P.Teams ~user:"u-old" () in
  let key_new = actor_key ~connector:P.Slack ~tenant:"ws-b" ~user:"u-new" () in
  ignore
    (seed_actor_and_link ~db ~principal_id:older ~key:key_old
       ~link_id:"link_old" ~display_name:"Old" ());
  ignore
    (seed_actor_and_link ~db ~principal_id:newer ~key:key_new
       ~link_id:"link_new" ~display_name:"New" ());
  let bind_loser =
    seed_binding ~db ~principal_id:newer ~id:"gh_loser" ~github_user_id:2001L
      ~lineage_id:"lin_loser" ~login:"loser" ()
  in
  (* Capture delayed work under the loser before merge. *)
  let snap =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key_new
         ~delayed_job_id:"job_pre_merge" ~account_binding_id:bind_loser.id
         ~room_id:shared_room ~now:fixed_now ())
  in
  Alcotest.(check string)
    "evidence principal is loser" "prin_new"
    (P.principal_id_to_string snap.lineage.principal_id);
  (* Deterministic survivor selection (preview). *)
  let preview =
    assert_ok (M.preview_merge ~db ~left_id:older ~right_id:newer ())
  in
  Alcotest.(check string)
    "survivor by created_at" "prin_old"
    (P.principal_id_to_string preview.survivor_id);
  Alcotest.(check string)
    "loser" "prin_new"
    (P.principal_id_to_string preview.loser_id);
  (match
     M.apply_merge ~db ~left_id:older ~right_id:newer ~link_tx_id:"ltx_iso"
       ~merge_id:"pmerge_iso" ~now:fixed_now ()
   with
  | M.Applied receipt ->
      Alcotest.(check string)
        "applied survivor" "prin_old"
        (P.principal_id_to_string receipt.survivor_id);
      Alcotest.(check bool)
        "adopted slack actor" true
        (List.exists
           (String.equal (P.actor_identity_key key_new))
           receipt.adopted_actor_keys)
  | M.Idempotent _ -> Alcotest.fail "unexpected idempotent"
  | M.Refused { reason; _ } -> Alcotest.fail ("merge refused: " ^ reason)
  | M.Stale_revision s -> Alcotest.fail ("stale: " ^ s));
  (* Evidence unchanged; live authority follows survivor + adopted binding. *)
  Alcotest.(check string)
    "historical principal frozen" "prin_new"
    (P.principal_id_to_string snap.lineage.principal_id);
  let auth = assert_ok (A.re_resolve_current_authority ~db snap) in
  Alcotest.(check bool) "followed merge alias" true auth.followed_merge_alias;
  Alcotest.(check bool) "usable after adoption" true auth.usable;
  (match auth.live_principal_id with
  | Some p ->
      Alcotest.(check string)
        "live survivor" "prin_old"
        (P.principal_id_to_string p)
  | None -> Alcotest.fail "live principal");
  match Job.prepare_execution ~db ~job_id:"job_pre_merge" ~snapshot:snap () with
  | Ok env ->
      Alcotest.(check bool) "exec usable" true env.live_authority.usable;
      Alcotest.(check string)
        "lineage id preserved" "prin_new" env.principal_lineage_id
  | Error inv ->
      Alcotest.fail
        ("exec should succeed after merge: "
        ^ Job.string_of_exec_invalidation inv)

(* -------------------------------------------------------------------------- *)
(* 6. Duplicate-account conflict                                              *)
(* -------------------------------------------------------------------------- *)

let test_duplicate_account_conflict () =
  with_db @@ fun db ->
  let a = seed_principal ~db ~id:"prin_own_a" () in
  let b = seed_principal ~db ~id:"prin_own_b" () in
  let identity =
    assert_ok (B.make_account_identity ~app_id:42 ~github_user_id:4242L ())
  in
  let assert_a =
    assert_ok
      (Op.make_identity_assertion ~principal_id:a ~principal_revision:1
         ~identity ~verified_at:"2026-07-01T00:00:00Z"
         ~expires_at:"2099-01-01T00:00:00Z" ~now:fixed_now ())
  in
  (match
     Op.attach_account ~db ~assertion:assert_a ~id:"bind_first"
       ~lineage_id:"lin_first" ~now:fixed_now ()
   with
  | Op.Attached _ -> ()
  | Op.Refused { denial; _ } ->
      Alcotest.fail (Op.string_of_attach_denial denial));
  let assert_b =
    assert_ok
      (Op.make_identity_assertion ~principal_id:b ~principal_revision:1
         ~identity ~verified_at:"2026-07-01T00:00:00Z"
         ~expires_at:"2099-01-01T00:00:00Z" ~now:fixed_now ())
  in
  match Op.attach_account ~db ~assertion:assert_b ~now:fixed_now () with
  | Op.Attached _ -> Alcotest.fail "duplicate ownership must refuse"
  | Op.Refused
      {
        denial =
          Op.Duplicate_ownership { existing_binding_id; owner_principal_id; _ };
        _;
      } ->
      Alcotest.(check string) "existing" "bind_first" existing_binding_id;
      Alcotest.(check string)
        "owner a" "prin_own_a"
        (P.principal_id_to_string owner_principal_id)
  | Op.Refused { denial; _ } ->
      Alcotest.fail ("unexpected: " ^ Op.string_of_attach_denial denial)

(* -------------------------------------------------------------------------- *)
(* 7. Ambiguity: no Room history / other-credential selection                 *)
(* -------------------------------------------------------------------------- *)

let test_ambiguity_no_room_or_other_selection () =
  with_db @@ fun db ->
  let alice = seed_principal ~db ~id:"prin_alice" () in
  let bob = seed_principal ~db ~id:"prin_bob_amb" () in
  ignore
    (seed_binding ~db ~principal_id:alice ~id:"b_alpha" ~github_user_id:1L
       ~lineage_id:"lin_alpha" ~login:"alpha" ());
  ignore
    (seed_binding ~db ~principal_id:alice ~id:"b_beta" ~github_user_id:2L
       ~lineage_id:"lin_beta" ~login:"beta" ());
  ignore
    (seed_binding ~db ~principal_id:bob ~id:"b_bob" ~github_user_id:99L
       ~lineage_id:"lin_bob_amb" ~login:"alpha" ());
  (* Bob sets a shared-room preference that must never leak to Alice. *)
  let room_scope = assert_ok (Pref.make_room_scope ~room_id:shared_room ()) in
  let bob_val = assert_ok (Pref.make_preference_value ~binding_id:"b_bob" ()) in
  ignore
    (assert_ok
       (Pref.set_preference ~db ~now:fixed_now ~principal_id:bob
          ~scope:room_scope ~value:bob_val ()));
  let ctx =
    Pref.make_resolve_context ~principal_id:alice ~room_id:shared_room
      ~repo_full_name:"acme/widget" ()
  in
  match assert_ok (Pref.resolve ~db ~context:ctx ()) with
  | Pref.Ambiguous { prompt } ->
      Alcotest.(check int)
        "two alice candidates" 2
        (List.length prompt.candidates);
      Alcotest.(check bool)
        "no bob binding" true
        (List.for_all
           (fun (c : Pref.redacted_candidate) -> c.binding_id <> "b_bob")
           prompt.candidates);
      Alcotest.(check bool)
        "no auto-pick by shared login" true
        (List.for_all
           (fun (c : Pref.redacted_candidate) ->
             c.binding_id = "b_alpha" || c.binding_id = "b_beta")
           prompt.candidates)
  | Pref.Resolved { binding; _ } ->
      Alcotest.fail
        (Printf.sprintf "must not auto-pick %s from room/login" binding.id)
  | Pref.None_eligible _ -> Alcotest.fail "alice has two accounts"

(* -------------------------------------------------------------------------- *)
(* 8. Unlink / split / revoke break authority; evidence stable                *)
(* -------------------------------------------------------------------------- *)

let test_unlink_split_revoke_break_authority () =
  with_db @@ fun db ->
  let source =
    seed_principal ~db ~id:"prin_src" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let k_keep = actor_key ~user:"keep" () in
  let k_split =
    actor_key ~connector:P.Slack ~tenant:"ws-split" ~user:"split" ()
  in
  ignore
    (seed_actor_and_link ~db ~principal_id:source ~key:k_keep
       ~link_id:"link_keep" ~display_name:"Keep" ());
  ignore
    (seed_actor_and_link ~db ~principal_id:source ~key:k_split
       ~link_id:"link_split" ~display_name:"Split" ());
  let bind =
    seed_binding ~db ~principal_id:source ~id:"gh_src" ~github_user_id:3001L
      ~lineage_id:"lin_src" ~login:"srcuser" ()
  in
  let snap =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:k_split
         ~delayed_job_id:"job_split" ~account_binding_id:bind.id
         ~room_id:shared_room ~now:fixed_now ())
  in
  Alcotest.(check string)
    "pre-split principal" "prin_src"
    (P.principal_id_to_string snap.lineage.principal_id);
  let new_pid =
    match
      U.unlink_actor ~db ~source_principal_id:source ~actor_key:k_split
        ~plan_id:"psplit_iso" ~unlink_id:"punlink_iso" ~now:fixed_now ()
    with
    | U.Applied receipt ->
        Alcotest.(check bool)
          "new principal distinct" true
          (not
             (P.principal_id_equal receipt.source_principal_id
                receipt.new_principal_id));
        Alcotest.(check (list string))
          "no auto account rebind" [] receipt.rebound_account_ids;
        receipt.new_principal_id
    | other ->
        Alcotest.fail
          (match other with
          | U.Idempotent _ -> "idempotent"
          | U.Refused { reason; _ } -> "refused: " ^ reason
          | U.Stale_revision s -> "stale: " ^ s
          | U.Applied _ -> "unreachable")
  in
  (* Evidence frozen on source principal. *)
  Alcotest.(check string)
    "evidence still source" "prin_src"
    (P.principal_id_to_string snap.lineage.principal_id);
  let auth = assert_ok (A.re_resolve_current_authority ~db snap) in
  Alcotest.(check bool) "not usable after split" false auth.usable;
  Alcotest.(check bool)
    "principal changed" true
    (List.exists
       (function A.Principal_changed _ -> true | _ -> false)
       auth.breaks);
  (match Job.prepare_execution ~db ~job_id:"job_split" ~snapshot:snap () with
  | Ok _ -> Alcotest.fail "split must fail closed at exec"
  | Error inv ->
      let msg = Job.string_of_exec_invalidation inv in
      Alcotest.(check bool)
        "unusable" true
        (contains msg "unusable" || contains msg "authority"
       || contains msg "principal" || contains msg "refused"));
  (* Binding remains on source; new principal is empty of github authority. *)
  (match B.get ~db ~id:bind.id with
  | Ok (Some b) ->
      Alcotest.(check string)
        "binding stayed on source" "prin_src"
        (P.principal_id_to_string b.principal_id)
  | _ -> Alcotest.fail "binding missing");
  (* Revocation path on a separate live actor. *)
  let prin_rev = seed_principal ~db ~id:"prin_rev" () in
  let key_rev = actor_key ~user:"user-rev" () in
  ignore
    (seed_actor_and_link ~db ~principal_id:prin_rev ~key:key_rev
       ~link_id:"link_rev" ());
  let bind_rev =
    seed_binding ~db ~principal_id:prin_rev ~id:"gh_rev" ~github_user_id:3002L
      ~lineage_id:"lin_rev" ~login:"revuser" ()
  in
  let snap_rev =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key_rev
         ~delayed_job_id:"job_rev" ~account_binding_id:bind_rev.id
         ~room_id:shared_room ~now:fixed_now ())
  in
  ignore
    (assert_ok
       (B.update_authorization_status ~db ~id:bind_rev.id ~status:B.Revoked
          ~now:(fixed_now +. 1.) ()));
  let auth_rev = assert_ok (A.re_resolve_current_authority ~db snap_rev) in
  Alcotest.(check bool) "not usable after revoke" false auth_rev.usable;
  Alcotest.(check bool)
    "account not authorized" true
    (List.exists
       (function
         | A.Account_not_authorized { status = B.Revoked } -> true | _ -> false)
       auth_rev.breaks);
  Alcotest.(check string)
    "revoke evidence frozen" "prin_rev"
    (P.principal_id_to_string snap_rev.lineage.principal_id);
  (* new_pid only referenced to silence unused warning if pattern changes *)
  ignore new_pid

(* -------------------------------------------------------------------------- *)
(* 9. Restart / retry preserve snapshot and re-resolve live authority         *)
(* -------------------------------------------------------------------------- *)

let test_restart_retry_preserve_and_reresolve () =
  with_db @@ fun db ->
  let prin = seed_principal ~db ~id:"prin_retry" () in
  let key = actor_key ~user:"user-retry" () in
  ignore
    (seed_actor_and_link ~db ~principal_id:prin ~key ~link_id:"link_retry"
       ~display_name:"Retry" ());
  let bind =
    seed_binding ~db ~principal_id:prin ~id:"gh_retry" ~github_user_id:4001L
      ~lineage_id:"lin_retry" ~login:"retry" ()
  in
  let intent = sample_intent ~id:"ghdi_retry_1" () in
  let snap =
    assert_ok
      (Job.capture_for_delayed_job ~db ~actor_key:key ~delayed_job_id:intent.id
         ~account_binding_id:bind.id ~room_id:shared_room ~now:fixed_now ())
  in
  let entry =
    assert_ok
      (O.enqueue ~db ~room_id:shared_room ~item_key:intent.item_key ~intent
         ~actor_snapshot:snap ~now:fixed_now ())
  in
  (* Restart recovery: claim_due reloads the same immutable evidence. *)
  let claimed = assert_ok (O.claim_due ~db ~now:fixed_now ~limit:10 ()) in
  Alcotest.(check int) "claimed 1" 1 (List.length claimed);
  let c0 = List.hd claimed in
  (match assert_ok (O.snapshot_of_entry c0) with
  | Some s ->
      Alcotest.(check string) "claim keeps snap" snap.id s.id;
      Alcotest.(check string)
        "principal" "prin_retry"
        (P.principal_id_to_string s.lineage.principal_id)
  | None -> Alcotest.fail "snapshot lost on claim");
  (* Transient failure → retry preserves snapshot. *)
  let after_fail =
    assert_ok
      (O.mark_failure ~db ~id:entry.id ~error:"timeout" ~now:(fixed_now +. 1.)
         ())
  in
  (match assert_ok (O.snapshot_of_entry after_fail) with
  | Some s -> Alcotest.(check string) "retry keeps snap" snap.id s.id
  | None -> Alcotest.fail "snapshot lost on failure");
  (* Live re-resolve succeeds while lineage valid. *)
  (match
     Job.prepare_execution_of_json ~db ~job_id:entry.id
       ~snapshot_json:after_fail.actor_snapshot_json ~require_snapshot:true ()
   with
  | Ok (Some env) ->
      Alcotest.(check bool) "usable" true env.live_authority.usable;
      Alcotest.(check string) "job" entry.id env.job_id
  | Ok None -> Alcotest.fail "expected envelope"
  | Error e -> Alcotest.fail e);
  (* Stale lineage after revoke: re-resolve fails closed; stored evidence same. *)
  ignore
    (assert_ok
       (B.update_authorization_status ~db ~id:bind.id ~status:B.Revoked
          ~now:(fixed_now +. 2.) ()));
  (match
     Job.prepare_execution_of_json ~db ~job_id:entry.id
       ~snapshot_json:after_fail.actor_snapshot_json ~require_snapshot:true ()
   with
  | Ok (Some _) -> Alcotest.fail "revoked must fail closed"
  | Ok None -> Alcotest.fail "expected error"
  | Error msg ->
      Alcotest.(check bool)
        "mentions unusable" true
        (contains msg "unusable" || contains msg "authority"
       || contains msg "account" || contains msg "refused"));
  match assert_ok (O.snapshot_of_entry after_fail) with
  | Some s ->
      Alcotest.(check string)
        "evidence still retry" "prin_retry"
        (P.principal_id_to_string s.lineage.principal_id)
  | None -> Alcotest.fail "evidence missing after revoke"

(* -------------------------------------------------------------------------- *)
(* 10. Stale snapshots after identity-link revision / account lineage change  *)
(* -------------------------------------------------------------------------- *)

let test_stale_snapshots_break () =
  with_db @@ fun db ->
  let prin = seed_principal ~db ~id:"prin_stale" () in
  let key = actor_key ~user:"user-stale" () in
  let _actor, link =
    seed_actor_and_link ~db ~principal_id:prin ~key ~link_id:"link_stale" ()
  in
  (* Live binding is lineage v2; historical snapshot pins v1 (relink). *)
  let bind =
    seed_binding ~db ~principal_id:prin ~id:"gh_stale" ~github_user_id:5001L
      ~lineage_id:"lin_stale_v2" ~login:"stale" ()
  in
  let ab_v1 =
    assert_ok
      (A.make_account_binding_evidence ~binding_id:bind.id
         ~lineage_id:"lin_stale_v1" ~identity:bind.identity
         ~authorization_status:B.Authorized ())
  in
  let snap =
    assert_ok
      (A.create ~id:"actorsnap_stale" ~now:fixed_now ~principal_id:prin
         ~actor_key:key ~identity_link_id:link.id
         ~identity_link_revision:link.revision ~account_binding:ab_v1 ())
  in
  Alcotest.(check (option string))
    "pinned lineage v1" (Some "lin_stale_v1") snap.lineage.account_lineage_id;
  let auth = assert_ok (A.re_resolve_current_authority ~db snap) in
  Alcotest.(check bool) "lineage change unusable" false auth.usable;
  Alcotest.(check bool)
    "account lineage changed" true
    (List.exists
       (function A.Account_lineage_changed _ -> true | _ -> false)
       auth.breaks);
  (* Historical evidence still v1. *)
  Alcotest.(check (option string))
    "snap lineage frozen" (Some "lin_stale_v1") snap.lineage.account_lineage_id;
  (* Identity-link inactive also breaks (after re-authorizing lineage match). *)
  ignore
    (assert_ok
       (S.update_identity_link ~db ~id:link.id ~status:P.Unlinked
          ~unlinked_at:(Some "2026-07-13T12:00:00Z") ~now:(fixed_now +. 2.) ()));
  let ab_v2 = A.account_binding_evidence_of_binding bind in
  let snap2 =
    assert_ok
      (A.create ~now:fixed_now ~principal_id:prin ~actor_key:key
         ~identity_link_id:link.id ~identity_link_revision:link.revision
         ~account_binding:ab_v2 ())
  in
  let auth2 = assert_ok (A.re_resolve_current_authority ~db snap2) in
  Alcotest.(check bool) "unlinked unusable" false auth2.usable

(* -------------------------------------------------------------------------- *)
(* 11. Receipts / webhooks: isolation + stable historical evidence            *)
(* -------------------------------------------------------------------------- *)

let test_receipts_webhooks_isolation () =
  with_db @@ fun db ->
  let snap_ada =
    assert_ok
      (A.create ~id:"actorsnap_iso_ada" ~now:fixed_now ~reason:"receipt"
         ~principal_id:(pid "prin_ada") ~principal_revision:2
         ~actor_key:(actor_key ~user:"user-ada" ())
         ~actor_revision:1 ~identity_link_id:"link_ada"
         ~identity_link_revision:1
         ~display:
           {
             display_name = Some "Ada";
             avatar_url = None;
             email = None;
             extra = [];
           }
         ~source:
           { room_id = Some shared_room; session_id = None; message_id = None }
         ~account_binding:
           (assert_ok
              (A.make_account_binding_evidence ~binding_id:"gh_ada"
                 ~lineage_id:"lin_ada"
                 ~identity:
                   (assert_ok
                      (B.make_account_identity ~app_id:42 ~github_user_id:1001L
                         ()))
                 ()))
         ())
  in
  let snap_bob =
    assert_ok
      (A.create ~id:"actorsnap_iso_bob" ~now:fixed_now ~reason:"receipt"
         ~principal_id:(pid "prin_bob") ~principal_revision:1
         ~actor_key:(actor_key ~user:"user-bob" ())
         ~actor_revision:1 ~identity_link_id:"link_bob"
         ~identity_link_revision:1
         ~display:
           {
             display_name = Some "Bob";
             avatar_url = None;
             email = None;
             extra = [];
           }
         ~source:
           { room_id = Some shared_room; session_id = None; message_id = None }
         ())
  in
  let corr_ada =
    Rec.make_correlation ~room_id:shared_room ~action:"comment"
      ~actor_mode:"user" ~item_key ~plan_id:"plan_ada" ~receipt_id:"rcpt_ada"
      ~requested_mode:"user" ~resolved_mode:"user" ~actor_snapshot:snap_ada
      ~expected_github_login:"ada" ()
  in
  let corr_bob =
    Rec.make_correlation ~room_id:shared_room ~action:"comment"
      ~actor_mode:"user" ~item_key ~plan_id:"plan_bob" ~receipt_id:"rcpt_bob"
      ~requested_mode:"user" ~resolved_mode:"user" ~actor_snapshot:snap_bob
      ~expected_github_login:"bob" ()
  in
  assert_ok (Rec.record_correlation ~db ~correlation:corr_ada ~now:fixed_now ());
  assert_ok
    (Rec.record_correlation ~db ~correlation:corr_bob ~now:(fixed_now +. 0.1) ());
  (* Unrelated human / other principal cannot close Ada's receipt. *)
  let stranger =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "deliv-stranger")
      ~actor_login:(Some "carol") ~actor_type:(Some "User") ~merged:None ()
  in
  let r0 =
    Rec.reconcile_webhook ~db ~room_id:shared_room ~envelope:stranger
      ~now:fixed_now ()
  in
  Alcotest.(check string)
    "stranger ignored" "ignored_human_event" (result_tag r0);
  (* Bob's native attribution closes only Bob's receipt. *)
  let bob_evt =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "deliv-bob")
      ~actor_login:(Some "bob") ~actor_type:(Some "User") ~merged:None ()
  in
  let r1 =
    Rec.reconcile_webhook ~db ~room_id:shared_room ~envelope:bob_evt
      ~now:(fixed_now +. 1.) ()
  in
  (match r1 with
  | Rec.Closed { correlation = c; first_time = true } -> (
      Alcotest.(check (option string))
        "bob receipt" (Some "rcpt_bob") c.receipt_id;
      match c.actor_snapshot with
      | Some s ->
          Alcotest.(check string)
            "bob principal" "prin_bob"
            (P.principal_id_to_string s.lineage.principal_id);
          Alcotest.(check bool) "not ada snap" true (s.id <> snap_ada.id)
      | None -> Alcotest.fail "bob snapshot missing")
  | _ -> Alcotest.fail "expected bob closed first");
  (* Ada still open with frozen evidence. *)
  (match Rec.get_by_receipt_id ~db ~receipt_id:"rcpt_ada" with
  | None -> Alcotest.fail "ada receipt missing"
  | Some open_ada -> (
      match open_ada.actor_snapshot with
      | Some s ->
          Alcotest.(check string)
            "ada intact" "prin_ada"
            (P.principal_id_to_string s.lineage.principal_id)
      | None -> Alcotest.fail "ada snapshot missing"));
  (* Ada closes via expected login; historical evidence retained. *)
  let ada_evt =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "deliv-ada")
      ~actor_login:(Some "Ada") ~actor_type:(Some "User") ~merged:None ()
  in
  let r2 =
    Rec.reconcile_webhook ~db ~room_id:shared_room ~envelope:ada_evt
      ~now:(fixed_now +. 2.) ()
  in
  match r2 with
  | Rec.Closed { correlation = c; _ } -> (
      match c.actor_snapshot with
      | Some s ->
          Alcotest.(check string) "snap id" snap_ada.id s.id;
          Alcotest.(check (option string))
            "display frozen" (Some "Ada") s.display.display_name;
          Alcotest.(check bool) "never authority" false (A.is_authority s);
          Alcotest.(check bool)
            "correlation not authority" false
            (Rec.snapshot_is_authority c)
      | None -> Alcotest.fail "ada snapshot after close")
  | _ -> Alcotest.fail "expected ada close"

(* -------------------------------------------------------------------------- *)
(* 12. Legacy rows: only unambiguous verified backfill authorizes user work   *)
(* -------------------------------------------------------------------------- *)

let test_legacy_rows_isolation () =
  with_db @@ fun db ->
  let prin = seed_principal ~db ~id:"prin_legacy" () in
  let key =
    actor_key ~connector:P.Teams ~tenant:"tenant-legacy" ~user:"aad-legacy" ()
  in
  ignore
    (seed_actor_and_link ~db ~principal_id:prin ~key ~link_id:"link_legacy"
       ~display_name:"Legacy Ada" ());
  (* Unambiguous verified actor → backfill + user authority. *)
  let good =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"leg_good"
         ~connector:"teams" ~tenant_or_workspace:"tenant-legacy"
         ~immutable_user_id:"aad-legacy" ~room_id:shared_room ~job_active:true
         ())
  in
  (match assert_ok (L.classify_row ~db good) with
  | L.Backfill b ->
      Alcotest.(check string)
        "principal" "prin_legacy"
        (P.principal_id_to_string b.principal_id);
      let auth = L.authority_of_classification (L.Backfill b) in
      Alcotest.(check bool) "user allowed" true auth.user_attributed_allowed
  | L.Legacy_unresolved { reason } ->
      Alcotest.fail
        ("expected backfill: " ^ L.string_of_unresolved_reason reason));
  (* Display-name-only in same Room must not coalesce or authorize user work. *)
  let display_only =
    assert_ok
      (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"leg_name"
         ~connector:"teams" ~tenant_or_workspace:"tenant-legacy"
         ~requester_name:"Legacy Ada" ~room_id:shared_room ~job_active:true ())
  in
  (match assert_ok (L.classify_row ~db display_only) with
  | L.Backfill _ -> Alcotest.fail "display name must not backfill"
  | L.Legacy_unresolved { reason = L.Display_name_only } ->
      let auth =
        L.authority_of_classification
          (L.Legacy_unresolved { reason = L.Display_name_only })
      in
      Alcotest.(check bool) "deny user" false auth.user_attributed_allowed;
      Alcotest.(check bool) "allow app" true auth.app_behavior_allowed;
      Alcotest.(check bool) "allow read" true auth.read_audit_allowed
  | L.Legacy_unresolved { reason } ->
      Alcotest.fail ("unexpected: " ^ L.string_of_unresolved_reason reason));
  (* Same display name, different users in shared room never coalesce. *)
  let other_prin = seed_principal ~db ~id:"prin_legacy_b" () in
  let key_b =
    actor_key ~connector:P.Teams ~tenant:"tenant-legacy" ~user:"aad-other" ()
  in
  ignore
    (seed_actor_and_link ~db ~principal_id:other_prin ~key:key_b
       ~link_id:"link_legacy_b" ~display_name:"Legacy Ada" ());
  let report =
    assert_ok
      (L.migrate_rows ~db
         ~rows:
           [
             good;
             display_only;
             assert_ok
               (L.make_legacy_row ~source_kind:L.Fixture ~source_id:"leg_other"
                  ~connector:"teams" ~tenant_or_workspace:"tenant-legacy"
                  ~immutable_user_id:"aad-other" ~requester_name:"Legacy Ada"
                  ~room_id:shared_room ());
           ]
         ~run_id:"run_iso" ~now:fixed_now ())
  in
  Alcotest.(check int)
    "never rewrite historical snapshots" 0 report.historical_snapshots_rewritten;
  Alcotest.(check bool) "some backfilled" true (report.backfilled >= 2);
  Alcotest.(check bool) "display unresolved" true (report.unresolved >= 1);
  (* Ambiguous active display-only job is not user-authorizable. *)
  List.iter
    (fun (rec_ : L.migration_record) ->
      if rec_.row.source_id = "leg_name" then
        Alcotest.(check bool)
          "job not user-auth" false rec_.authority.user_attributed_allowed)
    report.records

(* -------------------------------------------------------------------------- *)
(* Suite                                                                      *)
(* -------------------------------------------------------------------------- *)

let suite =
  [
    ( "every trust adapter isolated by namespace",
      `Quick,
      test_every_trust_adapter_isolated );
    ("same ids across tenants isolated", `Quick, test_same_ids_across_tenants);
    ( "two users one room never borrow",
      `Quick,
      test_two_users_one_room_never_borrow );
    ("rename preserves evidence", `Quick, test_rename_preserves_evidence);
    ( "verified link deterministic adoption",
      `Quick,
      test_verified_link_deterministic_adoption );
    ("duplicate account conflict", `Quick, test_duplicate_account_conflict);
    ( "ambiguity no room or other selection",
      `Quick,
      test_ambiguity_no_room_or_other_selection );
    ( "unlink split revoke break authority",
      `Quick,
      test_unlink_split_revoke_break_authority );
    ( "restart retry preserve and reresolve",
      `Quick,
      test_restart_retry_preserve_and_reresolve );
    ("stale snapshots break", `Quick, test_stale_snapshots_break);
    ("receipts webhooks isolation", `Quick, test_receipts_webhooks_isolation);
    ("legacy rows isolation", `Quick, test_legacy_rows_isolation);
  ]
