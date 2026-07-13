(** Tests for redacted GitHub account admin / self-service surfaces
    (P21.M1.E2.T004). *)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding
module Pref = Github_account_preference
module U = Principal_unlink_split
module V = Github_user_token_vault
module TS = Github_user_token_store
module L = Github_user_token_lease
module Surf = Github_account_admin_surface

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-admin-surf-master" ()

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Surf.ensure_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (L.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let fixed_now = 1_785_400_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let pid s = assert_ok (P.principal_id_of_string s)

let actor_key ?(connector = P.Teams) ?(tenant = "tenant-a") ?(user = "user-1")
    () =
  assert_ok
    (P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
       ~immutable_user_id:user)

let seed_principal ~db ~id ?(revision = 1) () =
  let p =
    P.make_principal ~id:(pid id) ~revision ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p))

let seed_owned_actor ~db ~principal_id ~key ~link_id () =
  let actor =
    P.make_connector_actor ~key ~principal_id ~revision:1
      ~verified_at:"2026-07-13T00:00:00Z" ~created_at:"2026-07-13T00:00:00Z"
      ~updated_at:"2026-07-13T00:00:00Z" ()
  in
  ignore (assert_ok (S.insert_connector_actor ~db ~now:fixed_now actor));
  let link =
    P.make_identity_link ~id:link_id ~principal_id ~actor_key:key ~revision:1
      ~linked_at:"2026-07-13T00:00:00Z" ()
  in
  ignore (assert_ok (S.insert_identity_link ~db ~now:fixed_now link))

let sample_identity ?(app_id = 42) ?(github_user_id = 9001L) () =
  assert_ok (B.make_account_identity ~app_id ~github_user_id ())

let insert_binding ~db ~principal_id ~id ~github_user_id ?(login = Some "alice")
    ?(status = B.Authorized) ?(app_id = 42) ?vault_ref () =
  let identity = sample_identity ~app_id ~github_user_id () in
  let b =
    B.make_binding ~id ~principal_id ~identity
      ~display:{ B.login; avatar_url = None }
      ~authorization_status:status ?vault_ref ~lineage_id:id ()
  in
  assert_ok (B.insert ~db ~now:fixed_now b)

let make_keys ?(key_id = "mk-surf-1") () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version:1 ~aes_key ())

let sample_tokens ?(tag = "base") () =
  {
    TS.access_token = Printf.sprintf "ghu_access_SURF_%s_SECRET" tag;
    refresh_token = Some (Printf.sprintf "ghr_refresh_SURF_%s_SECRET" tag);
  }

let create_vault ~db ~keys ~principal_id ~github_user_id ~app_id ~id =
  let account =
    assert_ok
      (V.make_account_key ~principal_id ~github_user_id ~app_id
         ~host:V.default_host ())
  in
  match
    V.create ~db ~keys ~id ~now:fixed_now ~account
      ~tokens:(sample_tokens ~tag:id ()) ~scopes:[ "repo" ]
      ~expires_at:"2026-12-01T00:00:00Z" ()
  with
  | Ok r -> r
  | Error d -> Alcotest.fail ("vault create: " ^ V.string_of_denial d)

(* -------------------------------------------------------------------------- *)
(* Inspect / preference views                                                 *)
(* -------------------------------------------------------------------------- *)

let test_self_service_inspect_redacted () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  seed_principal ~db ~id:"prin_b" ();
  let pa = pid "prin_a" in
  let pb = pid "prin_b" in
  let vault_id = "ghvault_surf_1" in
  let keys = make_keys () in
  ignore
    (create_vault ~db ~keys ~principal_id:"prin_a" ~github_user_id:100L
       ~app_id:7 ~id:vault_id);
  let vref = assert_ok (B.make_vault_ref vault_id) in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_a" ~github_user_id:100L
       ~app_id:7 ~vault_ref:vref ());
  ignore
    (insert_binding ~db ~principal_id:pb ~id:"bind_b" ~github_user_id:200L
       ~login:(Some "bob") ());
  let value =
    assert_ok
      (Pref.make_preference_value ~binding_id:"bind_a" ~lineage_id:"bind_a" ())
  in
  ignore
    (assert_ok
       (Pref.set_preference ~db ~now:fixed_now ~principal_id:pa
          ~scope:Pref.Principal_default ~value ()));
  let surface = assert_ok (Surf.make_self_service ~principal_id:pa ()) in
  let inspect = assert_ok (Surf.inspect_accounts ~db ~surface ()) in
  Alcotest.(check int) "one account" 1 (List.length inspect.accounts);
  Alcotest.(check int) "one pref" 1 (List.length inspect.preferences);
  let acc = List.hd inspect.accounts in
  Alcotest.(check bool) "vault attached flag" true acc.vault_attached;
  Alcotest.(check string) "login" "alice" (Option.get acc.login);
  let json = Surf.account_inspect_to_json inspect in
  let tokens = sample_tokens ~tag:vault_id () in
  Alcotest.(check bool)
    "no access token in export" false
    (Surf.json_contains_plaintext ~json ~plaintext:tokens.access_token);
  Alcotest.(check bool)
    "no refresh token in export" false
    (Surf.json_contains_plaintext ~json
       ~plaintext:(Option.get tokens.refresh_token));
  Alcotest.(check bool)
    "no vault row id in export" false
    (Surf.json_contains_plaintext ~json ~plaintext:vault_id);
  (* Cross-principal inspect refused. *)
  match Surf.inspect_account ~db ~surface ~binding_id:"bind_b" () with
  | Error msg ->
      Alcotest.(check bool)
        "cross-principal denied" true
        (String_util.contains msg "not owned")
  | Ok _ -> Alcotest.fail "must refuse foreign binding"

let test_admin_inspect_and_preference_view () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_subj" ();
  seed_principal ~db ~id:"prin_admin" ();
  let subj = pid "prin_subj" in
  ignore
    (insert_binding ~db ~principal_id:subj ~id:"bind_1" ~github_user_id:11L ());
  let surface =
    assert_ok
      (Surf.make_admin ~admin_principal_id:(pid "prin_admin")
         ~subject_principal_id:subj ~reason:"support ticket 42" ())
  in
  let inspect = assert_ok (Surf.inspect_accounts ~db ~surface ()) in
  Alcotest.(check string) "admin surface" "admin" inspect.surface_kind;
  Alcotest.(check (option string))
    "admin reason" (Some "support ticket 42") inspect.admin_reason;
  let value = assert_ok (Pref.make_preference_value ~binding_id:"bind_1" ()) in
  let pref =
    assert_ok
      (Surf.set_preference ~db ~surface ~now:fixed_now
         ~scope:Pref.Principal_default ~value ())
  in
  Alcotest.(check string) "pref principal" "prin_subj" pref.principal_id;
  let ctx = Pref.make_resolve_context ~principal_id:subj ~app_id:42 () in
  let view =
    assert_ok (Surf.view_preferences ~db ~surface ~resolve_context:ctx ())
  in
  (match view.resolve with
  | Some (Pref.Resolved { binding; _ }) ->
      Alcotest.(check string) "resolved" "bind_1" binding.id
  | Some _ -> Alcotest.fail "expected resolved"
  | None -> Alcotest.fail "expected resolve result");
  let json = Surf.preference_view_to_json view in
  Alcotest.(check bool)
    "no vault_ref key leakage via resolve" false
    (Surf.json_contains_plaintext ~json ~plaintext:"vault_ref")

let test_admin_requires_reason () =
  match
    Surf.make_admin ~admin_principal_id:(pid "a")
      ~subject_principal_id:(pid "b") ~reason:"   " ()
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "empty reason must fail"

(* -------------------------------------------------------------------------- *)
(* Account revoke / unlink with conflict disclosure                           *)
(* -------------------------------------------------------------------------- *)

let test_plan_discloses_already_revoked () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let pa = pid "prin_a" in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_r" ~github_user_id:1L
       ~status:B.Revoked ());
  let surface = assert_ok (Surf.make_self_service ~principal_id:pa ()) in
  let plan =
    assert_ok
      (Surf.plan_account_action ~db ~surface ~kind:Surf.Revoke
         ~binding_id:"bind_r" ~now:fixed_now ())
  in
  Alcotest.(check bool) "hard conflict present" true (plan.hard_conflicts <> []);
  Alcotest.(check string)
    "conflict code" "already_revoked" (List.hd plan.hard_conflicts).code;
  match
    Surf.apply_account_action ~db ~surface ~plan ~presented_digest:plan.digest
      ~now:fixed_now ()
  with
  | Surf.Refused { conflicts; _ } ->
      Alcotest.(check bool) "refused with conflicts" true (conflicts <> [])
  | _ -> Alcotest.fail "must refuse conflicted plan"

let test_revoke_without_vault_snapshots_and_invalidates () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let pa = pid "prin_a" in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_v" ~github_user_id:55L
       ~login:(Some "revokee") ());
  let surface = assert_ok (Surf.make_self_service ~principal_id:pa ()) in
  let plan =
    assert_ok
      (Surf.plan_account_action ~db ~surface ~kind:Surf.Revoke
         ~binding_id:"bind_v" ~now:fixed_now ())
  in
  Alcotest.(check bool) "no hard conflicts" true (plan.hard_conflicts = []);
  Alcotest.(check bool) "will snapshot" true plan.will_snapshot;
  Alcotest.(check bool) "no vault" false plan.vault_attached;
  match
    Surf.apply_account_action ~db ~surface ~plan ~presented_digest:plan.digest
      ~now:(fixed_now +. 1.) ()
  with
  | Surf.Applied receipt ->
      Alcotest.(check string) "prev" "authorized" receipt.previous_status;
      Alcotest.(check string) "new" "revoked" receipt.new_status;
      Alcotest.(check bool)
        "snapshot written" true
        (Option.is_some receipt.snapshot_id);
      let b = Option.get (assert_ok (B.get ~db ~id:"bind_v")) in
      Alcotest.(check string)
        "live status"
        (B.string_of_authorization_status B.Revoked)
        (B.string_of_authorization_status b.authorization_status);
      Alcotest.(check string) "lineage preserved" "bind_v" b.lineage_id;
      let snaps =
        assert_ok (B.list_snapshots_for_binding ~db ~binding_id:"bind_v")
      in
      Alcotest.(check bool) "history retained" true (List.length snaps >= 1);
      let _, redacted_snaps =
        assert_ok (Surf.inspect_account ~db ~surface ~binding_id:"bind_v" ())
      in
      Alcotest.(check bool)
        "redacted snapshot prior status" true
        (List.exists
           (fun (s : Surf.redacted_snapshot) ->
             match s.authorization_status_at_snapshot with
             | Some "authorized" -> true
             | _ -> false)
           redacted_snaps);
      let json = Surf.account_action_receipt_to_json receipt in
      Alcotest.(check bool)
        "receipt has no token-looking secrets" false
        (Surf.json_contains_plaintext ~json ~plaintext:"ghu_")
  | Surf.Refused { reason; _ } -> Alcotest.fail ("refused: " ^ reason)
  | Surf.Stale_revision s -> Alcotest.fail ("stale: " ^ s)

let test_unlink_with_vault_cas_invalidates_immediately () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let pa = pid "prin_a" in
  let keys = make_keys () in
  let vault_id = "ghvault_unlink_1" in
  let rec_ =
    create_vault ~db ~keys ~principal_id:"prin_a" ~github_user_id:77L ~app_id:9
      ~id:vault_id
  in
  let vref = assert_ok (B.make_vault_ref vault_id) in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_u" ~github_user_id:77L
       ~app_id:9 ~vault_ref:vref ());
  (* Issue a process-local lease that must be discarded on unlink. *)
  (match L.issue ~db ~now:fixed_now ~vault_id:rec_.id () with
  | Ok _ -> ()
  | Error d -> Alcotest.fail ("lease issue: " ^ L.string_of_denial d));
  let surface = assert_ok (Surf.make_self_service ~principal_id:pa ()) in
  let plan =
    assert_ok
      (Surf.plan_account_action ~db ~surface ~kind:Surf.Unlink_account
         ~binding_id:"bind_u" ~now:fixed_now ())
  in
  Alcotest.(check bool) "vault attached" true plan.vault_attached;
  Alcotest.(check bool) "will clear vault ref" true plan.will_clear_vault_ref;
  match
    Surf.apply_account_action ~db ~surface ~plan ~presented_digest:plan.digest
      ~keys ~now:(fixed_now +. 2.) ()
  with
  | Surf.Applied receipt ->
      Alcotest.(check string) "unlinked" "unlinked" receipt.new_status;
      Alcotest.(check bool) "vault invalidated" true receipt.vault_invalidated;
      Alcotest.(check bool)
        "leases invalidated" true
        (receipt.leases_invalidated > 0);
      Alcotest.(check bool) "vault ref cleared" true receipt.vault_ref_cleared;
      let b = Option.get (assert_ok (B.get ~db ~id:"bind_u")) in
      Alcotest.(check bool) "no vault ref" true (Option.is_none b.vault_ref);
      (* Canonical invalidate lifecycle destroys sealed secrets after local
         disable (P21.M3.E1.T004). *)
      (match V.get_meta ~db ~id:vault_id with
      | Ok None | Error V.Not_found -> ()
      | Ok (Some meta) ->
          Alcotest.(check bool)
            "if vault remains it must be inactive" false meta.active
      | Error d -> Alcotest.fail (V.string_of_denial d));
      Alcotest.(check bool)
        "lineage broken on unlink" true
        (not (String.equal b.lineage_id "bind_u"));
      let tokens = sample_tokens ~tag:vault_id () in
      let json = Surf.account_action_receipt_to_json receipt in
      Alcotest.(check bool)
        "no access token" false
        (Surf.json_contains_plaintext ~json ~plaintext:tokens.access_token);
      Alcotest.(check bool)
        "no refresh token" false
        (Surf.json_contains_plaintext ~json
           ~plaintext:(Option.get tokens.refresh_token))
  | Surf.Refused { reason; _ } -> Alcotest.fail ("refused: " ^ reason)
  | Surf.Stale_revision s -> Alcotest.fail ("stale: " ^ s)

let test_digest_mismatch_refuses () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let pa = pid "prin_a" in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_d" ~github_user_id:3L ());
  let surface = assert_ok (Surf.make_self_service ~principal_id:pa ()) in
  let plan =
    assert_ok
      (Surf.plan_account_action ~db ~surface ~kind:Surf.Disable
         ~binding_id:"bind_d" ~now:fixed_now ())
  in
  match
    Surf.apply_account_action ~db ~surface ~plan ~presented_digest:"deadbeef"
      ~now:fixed_now ()
  with
  | Surf.Refused { conflicts; _ } ->
      Alcotest.(check string)
        "digest mismatch" "digest_mismatch" (List.hd conflicts).code
  | _ -> Alcotest.fail "expected digest refusal"

let test_stale_revision_on_apply () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let pa = pid "prin_a" in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_s" ~github_user_id:4L ());
  let surface = assert_ok (Surf.make_self_service ~principal_id:pa ()) in
  let plan =
    assert_ok
      (Surf.plan_account_action ~db ~surface ~kind:Surf.Disable
         ~binding_id:"bind_s" ~now:fixed_now ())
  in
  ignore
    (assert_ok
       (B.update_display ~db ~now:(fixed_now +. 1.) ~id:"bind_s"
          ~login:(Some "mutated") ()));
  match
    Surf.apply_account_action ~db ~surface ~plan ~presented_digest:plan.digest
      ~now:(fixed_now +. 2.) ()
  with
  | Surf.Stale_revision _ -> ()
  | Surf.Applied _ -> Alcotest.fail "must be stale"
  | Surf.Refused { reason; _ } -> Alcotest.fail ("refused: " ^ reason)

let test_admin_apply_requires_plan_issuer () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_subject" ();
  seed_principal ~db ~id:"prin_admin_a" ();
  seed_principal ~db ~id:"prin_admin_b" ();
  let subject = pid "prin_subject" in
  ignore
    (insert_binding ~db ~principal_id:subject ~id:"bind_admin_guard"
       ~github_user_id:5L ());
  let issuing_surface =
    assert_ok
      (Surf.make_admin ~admin_principal_id:(pid "prin_admin_a")
         ~subject_principal_id:subject ~reason:"support ticket 501" ())
  in
  let other_admin_surface =
    assert_ok
      (Surf.make_admin ~admin_principal_id:(pid "prin_admin_b")
         ~subject_principal_id:subject ~reason:"support ticket 501" ())
  in
  let plan =
    assert_ok
      (Surf.plan_account_action ~db ~surface:issuing_surface ~kind:Surf.Disable
         ~binding_id:"bind_admin_guard" ~now:fixed_now ())
  in
  (match
     Surf.apply_account_action ~db ~surface:other_admin_surface ~plan
       ~presented_digest:plan.digest ~now:(fixed_now +. 1.) ()
   with
  | Surf.Refused { conflicts; _ } ->
      Alcotest.(check string)
        "issuer mismatch refused" "admin_binding_mismatch"
        (List.hd conflicts).code
  | Surf.Applied _ | Surf.Stale_revision _ ->
      Alcotest.fail "a different admin must not apply the issued plan");
  let unchanged = Option.get (assert_ok (B.get ~db ~id:"bind_admin_guard")) in
  Alcotest.(check string)
    "binding remains authorized" "authorized"
    (B.string_of_authorization_status unchanged.authorization_status);
  match
    Surf.apply_account_action ~db ~surface:issuing_surface ~plan
      ~presented_digest:plan.digest ~now:(fixed_now +. 2.) ()
  with
  | Surf.Applied receipt ->
      Alcotest.(check string) "issuer applies" "disabled" receipt.new_status
  | Surf.Refused { reason; _ } -> Alcotest.fail ("issuer refused: " ^ reason)
  | Surf.Stale_revision s -> Alcotest.fail ("issuer stale: " ^ s)

(* -------------------------------------------------------------------------- *)
(* Actor unlink / split                                                       *)
(* -------------------------------------------------------------------------- *)

let test_actor_unlink_self_service () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_src" ();
  let src = pid "prin_src" in
  let k_keep = actor_key ~user:"keep" () in
  let k_split = actor_key ~connector:P.Slack ~tenant:"ws" ~user:"split" () in
  seed_owned_actor ~db ~principal_id:src ~key:k_keep ~link_id:"link_keep" ();
  seed_owned_actor ~db ~principal_id:src ~key:k_split ~link_id:"link_split" ();
  ignore
    (insert_binding ~db ~principal_id:src ~id:"bind_keep" ~github_user_id:9L ());
  let surface = assert_ok (Surf.make_self_service ~principal_id:src ()) in
  match
    Surf.actor_unlink_self_service ~db ~surface ~actor_key:k_split
      ~plan_id:"psplit_surf_ss" ~unlink_id:"punlink_surf_ss" ~now:fixed_now ()
  with
  | U.Applied receipt ->
      Alcotest.(check bool)
        "new principal" true
        (not
           (P.principal_id_equal receipt.source_principal_id
              receipt.new_principal_id));
      let binds = assert_ok (B.list_for_principal ~db ~principal_id:src) in
      Alcotest.(check int)
        "github binding retained on source" 1 (List.length binds);
      let new_binds =
        assert_ok
          (B.list_for_principal ~db ~principal_id:receipt.new_principal_id)
      in
      Alcotest.(check int) "new principal empty of gh" 0 (List.length new_binds)
  | other ->
      Alcotest.fail
        (match other with
        | U.Refused { reason; _ } -> "refused: " ^ reason
        | U.Stale_revision s -> "stale: " ^ s
        | U.Idempotent _ -> "idempotent?"
        | U.Applied _ -> "unreachable")

let test_admin_actor_unlink_plan_confirm_apply () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_src" ();
  seed_principal ~db ~id:"prin_admin" ();
  seed_principal ~db ~id:"prin_intruder" ();
  seed_principal ~db ~id:"prin_other_subject" ();
  let src = pid "prin_src" in
  let admin = pid "prin_admin" in
  let intruder = pid "prin_intruder" in
  let other_subject = pid "prin_other_subject" in
  let k_keep = actor_key ~user:"keep2" () in
  let k_split = actor_key ~connector:P.Discord ~tenant:"g" ~user:"split2" () in
  seed_owned_actor ~db ~principal_id:src ~key:k_keep ~link_id:"lk_keep" ();
  seed_owned_actor ~db ~principal_id:src ~key:k_split ~link_id:"lk_split" ();
  ignore
    (insert_binding ~db ~principal_id:src ~id:"bind_admin_gh"
       ~github_user_id:88L ());
  let surface =
    assert_ok
      (Surf.make_admin ~admin_principal_id:admin ~subject_principal_id:src
         ~reason:"account takeover repair" ())
  in
  let source_self_service =
    assert_ok (Surf.make_self_service ~principal_id:src ())
  in
  let wrong_admin_surface =
    assert_ok
      (Surf.make_admin ~admin_principal_id:intruder ~subject_principal_id:src
         ~reason:"wrong operator" ())
  in
  let wrong_subject_surface =
    assert_ok
      (Surf.make_admin ~admin_principal_id:admin
         ~subject_principal_id:other_subject ~reason:"wrong subject" ())
  in
  (* Admin one-shot is refused. *)
  (match
     Surf.actor_unlink_self_service ~db ~surface ~actor_key:k_split
       ~now:fixed_now ()
   with
  | U.Refused { reason; _ } ->
      Alcotest.(check bool)
        "admin one-shot refused" true
        (String_util.contains reason "plan-confirm-apply")
  | _ -> Alcotest.fail "admin must not one-shot");
  let surf_plan =
    assert_ok
      (Surf.plan_actor_unlink ~db ~surface ~actor_key:k_split
         ~plan_id:"psplit_admin_1" ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "no hard conflicts for default retain" true
    (surf_plan.hard_conflicts = []);
  Alcotest.(check int)
    "gh retained listed" 1
    (List.length surf_plan.github_accounts_retained);
  let plan_json = Surf.actor_unlink_surface_plan_to_json surf_plan in
  Alcotest.(check bool)
    "export has no vault tokens" false
    (Surf.json_contains_plaintext ~json:plan_json ~plaintext:"ghu_");
  (* A plan id and digest are not authority. The surface must remain bound to
     the exact admin + subject that created this revision-bound plan. *)
  (match
     Surf.confirm_actor_unlink ~db ~surface:source_self_service
       ~plan_id:surf_plan.plan.id ~presented_digest:surf_plan.plan.digest
       ~now:(fixed_now +. 1.) ()
   with
  | Error reason ->
      Alcotest.(check bool)
        "self-service cannot confirm admin plan" true
        (String_util.contains reason "self-service surface")
  | Ok _ -> Alcotest.fail "self-service must not confirm admin plan");
  (match
     Surf.confirm_actor_unlink ~db ~surface:wrong_admin_surface
       ~plan_id:surf_plan.plan.id ~presented_digest:surf_plan.plan.digest
       ~now:(fixed_now +. 1.) ()
   with
  | Error reason ->
      Alcotest.(check bool) "wrong admin refused" true
        (String_util.contains reason "admin principal")
  | Ok _ -> Alcotest.fail "wrong admin must not confirm plan");
  (match
     Surf.confirm_actor_unlink ~db ~surface:wrong_subject_surface
       ~plan_id:surf_plan.plan.id ~presented_digest:surf_plan.plan.digest
       ~now:(fixed_now +. 1.) ()
   with
  | Error reason ->
      Alcotest.(check bool) "wrong subject refused" true
        (String_util.contains reason "subject principal")
  | Ok _ -> Alcotest.fail "wrong subject must not confirm plan");
  let confirmed =
    assert_ok
      (Surf.confirm_actor_unlink ~db ~surface ~plan_id:surf_plan.plan.id
         ~presented_digest:surf_plan.plan.digest ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check string)
    "confirmed"
    (U.string_of_plan_status U.Confirmed)
    (U.string_of_plan_status confirmed.status);
  (match
     Surf.apply_actor_unlink ~db ~surface:wrong_admin_surface
       ~plan_id:surf_plan.plan.id ~now:(fixed_now +. 2.) ()
   with
  | U.Refused { reason; _ } ->
      Alcotest.(check bool) "wrong admin cannot apply" true
        (String_util.contains reason "admin principal")
  | _ -> Alcotest.fail "wrong admin must not apply plan");
  match
    Surf.apply_actor_unlink ~db ~surface ~plan_id:surf_plan.plan.id
      ~now:(fixed_now +. 2.) ()
  with
  | U.Applied receipt ->
      Alcotest.(check bool)
        "split applied" true
        (not
           (P.principal_id_equal receipt.source_principal_id
              receipt.new_principal_id));
      let binds = assert_ok (B.list_for_principal ~db ~principal_id:src) in
      Alcotest.(check int) "binding still on source" 1 (List.length binds)
  | U.Refused { reason; _ } -> Alcotest.fail ("refused: " ^ reason)
  | U.Stale_revision s -> Alcotest.fail ("stale: " ^ s)
  | U.Idempotent _ -> Alcotest.fail "unexpected idempotent"

let test_split_rebind_github_conflict_disclosed () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_src" ();
  let src = pid "prin_src" in
  let k_keep = actor_key ~user:"k3" () in
  let k_split = actor_key ~connector:P.Slack ~tenant:"w" ~user:"s3" () in
  seed_owned_actor ~db ~principal_id:src ~key:k_keep ~link_id:"l3k" ();
  seed_owned_actor ~db ~principal_id:src ~key:k_split ~link_id:"l3s" ();
  ignore
    (insert_binding ~db ~principal_id:src ~id:"bind_conflict"
       ~github_user_id:123L ());
  let surface = assert_ok (Surf.make_self_service ~principal_id:src ()) in
  match
    Surf.plan_actor_unlink ~db ~surface ~actor_key:k_split
      ~ownership:
        (U.Explicit_rebind
           { account_ids = [ "bind_conflict" ]; preference_keys = [] })
      ~plan_id:"psplit_conflict" ~now:fixed_now ()
  with
  | Error msg ->
      (* Plan construction refuses ownership conflicts at plan time. *)
      Alcotest.(check bool)
        "conflict at plan" true
        (String_util.contains (String.lowercase_ascii msg) "conflict"
        || String_util.contains (String.lowercase_ascii msg) "refuse"
        || String_util.contains (String.lowercase_ascii msg) "github")
  | Ok surf_plan ->
      Alcotest.(check bool)
        "hard conflicts disclosed" true
        (surf_plan.hard_conflicts <> [])

(* -------------------------------------------------------------------------- *)
(* Suite                                                                      *)
(* -------------------------------------------------------------------------- *)

let suite =
  [
    Alcotest.test_case "self-service inspect redacted (no tokens/vault ids)"
      `Quick test_self_service_inspect_redacted;
    Alcotest.test_case "admin inspect + preference view" `Quick
      test_admin_inspect_and_preference_view;
    Alcotest.test_case "admin requires reason" `Quick test_admin_requires_reason;
    Alcotest.test_case "plan discloses already-revoked conflict" `Quick
      test_plan_discloses_already_revoked;
    Alcotest.test_case "revoke snapshots and invalidates binding authority"
      `Quick test_revoke_without_vault_snapshots_and_invalidates;
    Alcotest.test_case "unlink with vault CAS invalidates immediately" `Quick
      test_unlink_with_vault_cas_invalidates_immediately;
    Alcotest.test_case "digest mismatch refuses apply" `Quick
      test_digest_mismatch_refuses;
    Alcotest.test_case "stale binding revision on apply" `Quick
      test_stale_revision_on_apply;
    Alcotest.test_case "admin account action requires plan issuer" `Quick
      test_admin_apply_requires_plan_issuer;
    Alcotest.test_case "actor unlink self-service retains github on source"
      `Quick test_actor_unlink_self_service;
    Alcotest.test_case "admin actor unlink plan-confirm-apply" `Quick
      test_admin_actor_unlink_plan_confirm_apply;
    Alcotest.test_case "split github rebind conflict disclosed" `Quick
      test_split_rebind_github_conflict_disclosed;
  ]
