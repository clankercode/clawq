(** Tests for repository- and Room-aware account preferences (P21.M1.E2.T003).
*)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding
module Pref = Github_account_preference
module M = Principal_merge
module U = Principal_unlink_split

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Pref.ensure_schema db;
  U.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_300_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let pid s = assert_ok (P.principal_id_of_string s)

let actor_key ?(connector = P.Teams) ?(tenant = "tenant-a") ?(user = "user-1")
    () =
  assert_ok
    (P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
       ~immutable_user_id:user)

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

let seed_principal ~db ~id ?(created_at = "2026-01-01T00:00:00Z")
    ?(revision = 1) () =
  let p =
    P.make_principal ~id:(pid id) ~revision ~created_at ~updated_at:created_at
      ()
  in
  assert_ok (S.insert_principal ~db ~now:fixed_now p)

let sample_identity ?(host = B.default_host) ?(app_id = 42)
    ?(github_user_id = 9001L) () =
  assert_ok (B.make_account_identity ~host ~app_id ~github_user_id ())

let insert_binding ~db ~principal_id ~id ~github_user_id ?(login = Some "user")
    ?(status = B.Authorized) ?(app_id = 42) ?(lineage_id = id) () =
  let identity = sample_identity ~app_id ~github_user_id () in
  let b =
    B.make_binding ~id ~principal_id ~identity
      ~display:{ B.login; avatar_url = None }
      ~authorization_status:status ~lineage_id ()
  in
  assert_ok (B.insert ~db ~now:fixed_now b)

let set_pref ~db ~principal_id ~scope ~binding_id ?lineage_id () =
  let value =
    assert_ok (Pref.make_preference_value ~binding_id ?lineage_id ())
  in
  assert_ok
    (Pref.set_preference ~db ~now:fixed_now ~principal_id ~scope ~value ())

let resolve_ok ~db context = assert_ok (Pref.resolve ~db ~context ())

(* -------------------------------------------------------------------------- *)
(* Key encoding / scope roundtrip                                             *)
(* -------------------------------------------------------------------------- *)

let test_scope_key_roundtrip () =
  let cases : Pref.preference_scope list =
    [
      Pref.Principal_default;
      Pref.Org (assert_ok (Pref.make_org_ref ~org_login:"acme" ()));
      Pref.Repo
        (assert_ok (Pref.make_repo_ref ~repo_full_name:"acme/widgets" ()));
      assert_ok (Pref.make_room_scope ~room_id:"room_1" ());
      assert_ok
        (Pref.make_room_scope ~room_id:"room_1"
           ~repo:
             (assert_ok (Pref.make_repo_ref ~repo_full_name:"acme/widgets" ()))
           ());
      assert_ok
        (Pref.make_room_scope ~room_id:"room_1"
           ~org:(assert_ok (Pref.make_org_ref ~org_login:"acme" ()))
           ());
      assert_ok
        (Pref.make_room_scope ~room_id:"room:with:colons"
           ~repo:
             (assert_ok
                (Pref.make_repo_ref ~repo_full_name:"org/name-with.dots" ()))
           ());
    ]
  in
  List.iter
    (fun scope ->
      let key = Pref.preference_scope_key scope in
      Alcotest.(check bool)
        "github prefix" true
        (Pref.is_github_account_preference_key key);
      match Pref.preference_scope_of_key key with
      | Error e -> Alcotest.fail e
      | Ok got ->
          Alcotest.(check string)
            "roundtrip"
            (Pref.string_of_preference_scope scope)
            (Pref.string_of_preference_scope got))
    cases

let test_scope_rank_order () =
  let room_repo =
    assert_ok
      (Pref.make_room_scope ~room_id:"r"
         ~repo:(assert_ok (Pref.make_repo_ref ~repo_full_name:"o/n" ()))
         ())
  in
  let room_org =
    assert_ok
      (Pref.make_room_scope ~room_id:"r"
         ~org:(assert_ok (Pref.make_org_ref ~org_login:"o" ()))
         ())
  in
  let room = assert_ok (Pref.make_room_scope ~room_id:"r" ()) in
  let repo =
    Pref.Repo (assert_ok (Pref.make_repo_ref ~repo_full_name:"o/n" ()))
  in
  let org = Pref.Org (assert_ok (Pref.make_org_ref ~org_login:"o" ())) in
  Alcotest.(check bool)
    "room_repo > room_org" true
    (Pref.preference_scope_rank room_repo > Pref.preference_scope_rank room_org);
  Alcotest.(check bool)
    "room_org > room" true
    (Pref.preference_scope_rank room_org > Pref.preference_scope_rank room);
  Alcotest.(check bool)
    "room > repo" true
    (Pref.preference_scope_rank room > Pref.preference_scope_rank repo);
  Alcotest.(check bool)
    "repo > org" true
    (Pref.preference_scope_rank repo > Pref.preference_scope_rank org);
  Alcotest.(check bool)
    "org > default" true
    (Pref.preference_scope_rank org
    > Pref.preference_scope_rank Pref.Principal_default)

(* -------------------------------------------------------------------------- *)
(* CRUD                                                                       *)
(* -------------------------------------------------------------------------- *)

let test_set_get_clear () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  let scope =
    Pref.Repo (assert_ok (Pref.make_repo_ref ~repo_full_name:"acme/alpha" ()))
  in
  ignore
    (set_pref ~db ~principal_id:(pid "prin_a") ~scope ~binding_id:"b1"
       ~lineage_id:"lin1" ());
  (match Pref.get_preference ~db ~principal_id:(pid "prin_a") ~scope with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "missing pref"
  | Ok (Some s) ->
      Alcotest.(check (option string)) "binding" (Some "b1") s.value.binding_id;
      Alcotest.(check (option string))
        "lineage" (Some "lin1") s.value.lineage_id);
  assert_ok (Pref.clear_preference ~db ~principal_id:(pid "prin_a") ~scope);
  match Pref.get_preference ~db ~principal_id:(pid "prin_a") ~scope with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "expected cleared"
  | Error e -> Alcotest.fail e

(* -------------------------------------------------------------------------- *)
(* Resolution precedence                                                      *)
(* -------------------------------------------------------------------------- *)

let seed_two_accounts ~db ~principal_id () =
  ignore
    (insert_binding ~db ~principal_id ~id:"b_alpha" ~github_user_id:1L
       ~login:(Some "alpha_login") ~lineage_id:"lin_alpha" ());
  ignore
    (insert_binding ~db ~principal_id ~id:"b_beta" ~github_user_id:2L
       ~login:(Some "beta_login") ~lineage_id:"lin_beta" ())

let test_explicit_choice_wins () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  seed_two_accounts ~db ~principal_id:(pid "prin_a") ();
  (* Store a room+repo pref for alpha, but explicit chooses beta. *)
  let room_repo =
    assert_ok
      (Pref.make_room_scope ~room_id:"room_x"
         ~repo:(assert_ok (Pref.make_repo_ref ~repo_full_name:"acme/alpha" ()))
         ())
  in
  ignore
    (set_pref ~db ~principal_id:(pid "prin_a") ~scope:room_repo
       ~binding_id:"b_alpha" ());
  let ctx =
    Pref.make_resolve_context ~principal_id:(pid "prin_a") ~room_id:"room_x"
      ~repo_full_name:"acme/alpha" ~explicit_binding_id:"b_beta" ()
  in
  match resolve_ok ~db ctx with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "id" "b_beta" binding.id;
      Alcotest.(check string)
        "source" "explicit_choice"
        (Pref.string_of_resolution_source source)
  | Pref.Ambiguous _ -> Alcotest.fail "ambiguous"
  | Pref.None_eligible _ -> Alcotest.fail "none"

let test_room_repo_beats_lower_scopes () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  seed_two_accounts ~db ~principal_id:(pid "prin_a") ();
  let room_repo =
    assert_ok
      (Pref.make_room_scope ~room_id:"room_x"
         ~repo:(assert_ok (Pref.make_repo_ref ~repo_full_name:"acme/alpha" ()))
         ())
  in
  let principal_repo =
    Pref.Repo (assert_ok (Pref.make_repo_ref ~repo_full_name:"acme/alpha" ()))
  in
  ignore
    (set_pref ~db ~principal_id:(pid "prin_a") ~scope:principal_repo
       ~binding_id:"b_beta" ());
  ignore
    (set_pref ~db ~principal_id:(pid "prin_a") ~scope:room_repo
       ~binding_id:"b_alpha" ());
  let ctx =
    Pref.make_resolve_context ~principal_id:(pid "prin_a") ~room_id:"room_x"
      ~repo_full_name:"acme/alpha" ()
  in
  match resolve_ok ~db ctx with
  | Pref.Resolved { binding; source; matched_scope } ->
      Alcotest.(check string) "room_repo wins" "b_alpha" binding.id;
      Alcotest.(check string)
        "source" "room_repo"
        (Pref.string_of_resolution_source source);
      Alcotest.(check bool) "matched scope" true (Option.is_some matched_scope)
  | _ -> Alcotest.fail "expected resolved"

let test_precedence_chain_room_org_repo_org_default () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  seed_two_accounts ~db ~principal_id:(pid "prin_a") ();
  (* Only principal default → alpha *)
  ignore
    (set_pref ~db ~principal_id:(pid "prin_a") ~scope:Pref.Principal_default
       ~binding_id:"b_alpha" ());
  let ctx0 =
    Pref.make_resolve_context ~principal_id:(pid "prin_a")
      ~repo_full_name:"acme/alpha" ()
  in
  (match resolve_ok ~db ctx0 with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "default" "b_alpha" binding.id;
      Alcotest.(check string)
        "src default" "principal_default"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "default");
  (* Org beats default *)
  let org = Pref.Org (assert_ok (Pref.make_org_ref ~org_login:"acme" ())) in
  ignore
    (set_pref ~db ~principal_id:(pid "prin_a") ~scope:org ~binding_id:"b_beta"
       ());
  (match resolve_ok ~db ctx0 with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "org" "b_beta" binding.id;
      Alcotest.(check string)
        "src org" "principal_org"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "org");
  (* Repo beats org *)
  let repo =
    Pref.Repo (assert_ok (Pref.make_repo_ref ~repo_full_name:"acme/alpha" ()))
  in
  ignore
    (set_pref ~db ~principal_id:(pid "prin_a") ~scope:repo ~binding_id:"b_alpha"
       ());
  (match resolve_ok ~db ctx0 with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "repo" "b_alpha" binding.id;
      Alcotest.(check string)
        "src repo" "principal_repo"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "repo");
  (* Room-only beats principal repo *)
  let room = assert_ok (Pref.make_room_scope ~room_id:"room_x" ()) in
  ignore
    (set_pref ~db ~principal_id:(pid "prin_a") ~scope:room ~binding_id:"b_beta"
       ());
  let ctx_room =
    Pref.make_resolve_context ~principal_id:(pid "prin_a") ~room_id:"room_x"
      ~repo_full_name:"acme/alpha" ()
  in
  match resolve_ok ~db ctx_room with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "room" "b_beta" binding.id;
      Alcotest.(check string)
        "src room" "room_only"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "room"

let test_sole_eligible_when_no_pref () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  ignore
    (insert_binding ~db ~principal_id:(pid "prin_a") ~id:"b_only"
       ~github_user_id:9L ());
  let ctx = Pref.make_resolve_context ~principal_id:(pid "prin_a") () in
  match resolve_ok ~db ctx with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "sole" "b_only" binding.id;
      Alcotest.(check string)
        "src" "sole_eligible"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "expected sole"

let test_ambiguous_private_prompt_no_auto_pick () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  seed_two_accounts ~db ~principal_id:(pid "prin_a") ();
  let ctx =
    Pref.make_resolve_context ~principal_id:(pid "prin_a")
      ~repo_full_name:"acme/alpha" ~room_id:"room_shared" ()
  in
  match resolve_ok ~db ctx with
  | Pref.Ambiguous { prompt } ->
      Alcotest.(check string)
        "reason" "multiple_eligible_no_preference" prompt.reason;
      Alcotest.(check string) "principal" "prin_a" prompt.principal_id;
      Alcotest.(check int) "two candidates" 2 (List.length prompt.candidates);
      (* Stable order by binding id, not recency/login. *)
      let ids =
        List.map
          (fun (c : Pref.redacted_candidate) -> c.binding_id)
          prompt.candidates
      in
      Alcotest.(check (list string)) "stable order" [ "b_alpha"; "b_beta" ] ids;
      (* Login may be present for UI but must not drive selection. *)
      Alcotest.(check bool)
        "login is display only" true
        (List.for_all
           (fun (c : Pref.redacted_candidate) -> Option.is_some c.login)
           prompt.candidates);
      let j = Pref.private_prompt_to_json prompt in
      let s = Yojson.Safe.to_string j in
      Alcotest.(check bool)
        "no token fields" false
        (let lower = String.lowercase_ascii s in
         let has sub =
           let n = String.length sub in
           let m = String.length lower in
           let rec loop i =
             if i + n > m then false
             else if String.sub lower i n = sub then true
             else loop (i + 1)
           in
           loop 0
         in
         has "access_token" || has "refresh_token" || has "vault_ciphertext")
  | Pref.Resolved _ -> Alcotest.fail "must not auto-pick"
  | Pref.None_eligible _ -> Alcotest.fail "has eligible"

let test_never_select_by_login_or_other_participant () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  ignore (seed_principal ~db ~id:"prin_b" ());
  (* Alice has two accounts; Bob has one with a "preferred" login name. *)
  seed_two_accounts ~db ~principal_id:(pid "prin_a") ();
  ignore
    (insert_binding ~db ~principal_id:(pid "prin_b") ~id:"b_bob"
       ~github_user_id:99L ~login:(Some "alpha_login") ());
  (* Bob's room preference must never leak into Alice's resolution. *)
  let room = assert_ok (Pref.make_room_scope ~room_id:"shared_room" ()) in
  ignore
    (set_pref ~db ~principal_id:(pid "prin_b") ~scope:room ~binding_id:"b_bob"
       ());
  let ctx =
    Pref.make_resolve_context ~principal_id:(pid "prin_a")
      ~room_id:"shared_room" ~repo_full_name:"acme/alpha" ()
  in
  match resolve_ok ~db ctx with
  | Pref.Ambiguous { prompt } ->
      Alcotest.(check int)
        "alice candidates only" 2
        (List.length prompt.candidates);
      Alcotest.(check bool)
        "no bob binding" true
        (List.for_all
           (fun (c : Pref.redacted_candidate) -> c.binding_id <> "b_bob")
           prompt.candidates)
  | Pref.Resolved { binding; _ } ->
      Alcotest.fail
        (Printf.sprintf "must not pick %s via login/other participant"
           binding.id)
  | Pref.None_eligible _ -> Alcotest.fail "alice has accounts"

let test_stale_preference_falls_through () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  seed_two_accounts ~db ~principal_id:(pid "prin_a") ();
  (* Preference points at revoked / wrong id → fall through to ambiguous. *)
  ignore
    (set_pref ~db ~principal_id:(pid "prin_a") ~scope:Pref.Principal_default
       ~binding_id:"b_missing" ());
  (* Mark alpha revoked so only beta is eligible if we pointed at alpha via
     another path; here default is stale, both still authorized → ambiguous. *)
  let ctx = Pref.make_resolve_context ~principal_id:(pid "prin_a") () in
  match resolve_ok ~db ctx with
  | Pref.Ambiguous _ -> ()
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.fail
        (Printf.sprintf "unexpected resolve %s via %s" binding.id
           (Pref.string_of_resolution_source source))
  | Pref.None_eligible _ -> Alcotest.fail "eligible exist"

let test_ineligible_status_excluded () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  ignore
    (insert_binding ~db ~principal_id:(pid "prin_a") ~id:"b_ok"
       ~github_user_id:1L ~status:B.Authorized ());
  ignore
    (insert_binding ~db ~principal_id:(pid "prin_a") ~id:"b_revoked"
       ~github_user_id:2L ~status:B.Revoked ());
  ignore
    (insert_binding ~db ~principal_id:(pid "prin_a") ~id:"b_pending"
       ~github_user_id:3L ~status:B.Pending ());
  (* Prefer revoked — must fall through to sole eligible authorized. *)
  ignore
    (set_pref ~db ~principal_id:(pid "prin_a") ~scope:Pref.Principal_default
       ~binding_id:"b_revoked" ());
  let ctx = Pref.make_resolve_context ~principal_id:(pid "prin_a") () in
  match resolve_ok ~db ctx with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "authorized only" "b_ok" binding.id;
      Alcotest.(check string)
        "sole after stale" "sole_eligible"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "expected sole authorized"

let test_lineage_match_after_display_change () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  ignore
    (insert_binding ~db ~principal_id:(pid "prin_a") ~id:"b1" ~github_user_id:1L
       ~login:(Some "old") ~lineage_id:"lin_stable" ());
  ignore
    (insert_binding ~db ~principal_id:(pid "prin_a") ~id:"b2" ~github_user_id:2L
       ~login:(Some "other") ());
  ignore
    (set_pref ~db ~principal_id:(pid "prin_a") ~scope:Pref.Principal_default
       ~binding_id:"b1" ~lineage_id:"lin_stable" ());
  ignore
    (assert_ok
       (B.update_display ~db ~now:(fixed_now +. 1.) ~id:"b1"
          ~login:(Some "brand_new_login") ~expected_revision:1 ()));
  let ctx = Pref.make_resolve_context ~principal_id:(pid "prin_a") () in
  match resolve_ok ~db ctx with
  | Pref.Resolved { binding; _ } ->
      Alcotest.(check string) "same binding" "b1" binding.id;
      Alcotest.(check string) "lineage" "lin_stable" binding.lineage_id;
      Alcotest.(check (option string))
        "login changed" (Some "brand_new_login") binding.display.login
  | _ -> Alcotest.fail "expected resolve via lineage"

let test_none_eligible_prompt () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  let ctx = Pref.make_resolve_context ~principal_id:(pid "prin_a") () in
  match resolve_ok ~db ctx with
  | Pref.None_eligible { prompt } ->
      Alcotest.(check string) "reason" "no_eligible_accounts" prompt.reason;
      Alcotest.(check int) "no candidates" 0 (List.length prompt.candidates)
  | _ -> Alcotest.fail "expected none_eligible"

(* -------------------------------------------------------------------------- *)
(* Merge adoption / split retain                                              *)
(* -------------------------------------------------------------------------- *)

let test_preferences_follow_principal_adoption () =
  with_db @@ fun db ->
  ignore
    (seed_principal ~db ~id:"prin_old" ~created_at:"2026-01-01T00:00:00Z" ());
  ignore
    (seed_principal ~db ~id:"prin_new" ~created_at:"2026-06-01T00:00:00Z" ());
  (* Loser (newer) has a repo preference; survivor (older) does not → adopt. *)
  let repo =
    Pref.Repo (assert_ok (Pref.make_repo_ref ~repo_full_name:"acme/alpha" ()))
  in
  ignore
    (set_pref ~db ~principal_id:(pid "prin_new") ~scope:repo ~binding_id:"b_x"
       ());
  (* Conflicting default: survivor keeps *)
  ignore
    (set_pref ~db ~principal_id:(pid "prin_old") ~scope:Pref.Principal_default
       ~binding_id:"b_surv" ());
  ignore
    (set_pref ~db ~principal_id:(pid "prin_new") ~scope:Pref.Principal_default
       ~binding_id:"b_lose" ());
  let status =
    M.apply_merge ~db ~left_id:(pid "prin_old") ~right_id:(pid "prin_new")
      ~now:fixed_now ()
  in
  (match status with
  | M.Applied _ | M.Idempotent _ -> ()
  | M.Refused { reason; _ } -> Alcotest.fail reason
  | M.Stale_revision s -> Alcotest.fail s);
  let prefs =
    assert_ok (Pref.list_preferences ~db ~principal_id:(pid "prin_old"))
  in
  Alcotest.(check bool)
    "repo adopted" true
    (List.exists
       (fun (p : Pref.stored_preference) ->
         match p.scope with
         | Pref.Repo { repo_full_name = "acme/alpha"; _ } ->
             p.value.binding_id = Some "b_x"
         | _ -> false)
       prefs);
  Alcotest.(check bool)
    "default keeps survivor" true
    (List.exists
       (fun (p : Pref.stored_preference) ->
         match p.scope with
         | Pref.Principal_default -> p.value.binding_id = Some "b_surv"
         | _ -> false)
       prefs);
  let loser_prefs =
    assert_ok (Pref.list_preferences ~db ~principal_id:(pid "prin_new"))
  in
  Alcotest.(check int)
    "loser prefs empty after adopt" 0 (List.length loser_prefs)

let test_split_retains_preferences_unless_explicit_plan () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_src" ());
  let k_keep = actor_key ~user:"keep" () in
  let k_split = actor_key ~connector:P.Slack ~tenant:"ws" ~user:"split" () in
  seed_owned_actor ~db ~principal_id:(pid "prin_src") ~key:k_keep
    ~link_id:"link_keep" ();
  seed_owned_actor ~db ~principal_id:(pid "prin_src") ~key:k_split
    ~link_id:"link_split" ();
  let repo =
    Pref.Repo (assert_ok (Pref.make_repo_ref ~repo_full_name:"acme/alpha" ()))
  in
  ignore
    (set_pref ~db ~principal_id:(pid "prin_src") ~scope:repo ~binding_id:"b1" ());
  ignore
    (set_pref ~db ~principal_id:(pid "prin_src") ~scope:Pref.Principal_default
       ~binding_id:"b2" ());
  let receipt =
    match
      U.unlink_actor ~db ~source_principal_id:(pid "prin_src")
        ~actor_key:k_split ~plan_id:"psplit_pref_retain" ~now:fixed_now ()
    with
    | U.Applied r | U.Idempotent r -> r
    | U.Refused { reason; _ } -> Alcotest.fail reason
    | U.Stale_revision s -> Alcotest.fail s
  in
  Alcotest.(check (list string))
    "no auto pref rebind" [] receipt.rebound_preference_keys;
  let src_prefs =
    assert_ok (Pref.list_preferences ~db ~principal_id:(pid "prin_src"))
  in
  Alcotest.(check int) "source retains prefs" 2 (List.length src_prefs);
  let new_prefs =
    assert_ok (Pref.list_preferences ~db ~principal_id:receipt.new_principal_id)
  in
  Alcotest.(check int) "new principal empty prefs" 0 (List.length new_prefs)

let test_split_explicit_preference_rebind () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_src" ());
  let k = actor_key ~connector:P.Slack ~tenant:"ws" ~user:"split2" () in
  seed_owned_actor ~db ~principal_id:(pid "prin_src") ~key:k ~link_id:"link2" ();
  let repo =
    Pref.Repo (assert_ok (Pref.make_repo_ref ~repo_full_name:"acme/alpha" ()))
  in
  let repo_key = Pref.preference_scope_key repo in
  ignore
    (set_pref ~db ~principal_id:(pid "prin_src") ~scope:repo ~binding_id:"b1" ());
  ignore
    (set_pref ~db ~principal_id:(pid "prin_src") ~scope:Pref.Principal_default
       ~binding_id:"b2" ());
  let receipt =
    match
      U.unlink_actor ~db ~source_principal_id:(pid "prin_src") ~actor_key:k
        ~ownership:
          (U.Explicit_rebind
             { account_ids = []; preference_keys = [ repo_key ] })
        ~plan_id:"psplit_pref_rebind" ~now:fixed_now ()
    with
    | U.Applied r | U.Idempotent r -> r
    | U.Refused { reason; _ } -> Alcotest.fail reason
    | U.Stale_revision s -> Alcotest.fail s
  in
  Alcotest.(check (list string))
    "rebound repo pref" [ repo_key ] receipt.rebound_preference_keys;
  let src =
    assert_ok (Pref.list_preferences ~db ~principal_id:(pid "prin_src"))
  in
  Alcotest.(check bool)
    "default retained on source" true
    (List.exists
       (fun (p : Pref.stored_preference) ->
         match p.scope with Pref.Principal_default -> true | _ -> false)
       src);
  let neu =
    assert_ok (Pref.list_preferences ~db ~principal_id:receipt.new_principal_id)
  in
  Alcotest.(check bool)
    "repo moved" true
    (List.exists
       (fun (p : Pref.stored_preference) ->
         match p.scope with
         | Pref.Repo { repo_full_name = "acme/alpha"; _ } -> true
         | _ -> false)
       neu)

let test_app_id_filter () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  ignore
    (insert_binding ~db ~principal_id:(pid "prin_a") ~id:"b_app42"
       ~github_user_id:1L ~app_id:42 ());
  ignore
    (insert_binding ~db ~principal_id:(pid "prin_a") ~id:"b_app99"
       ~github_user_id:2L ~app_id:99 ());
  let ctx =
    Pref.make_resolve_context ~principal_id:(pid "prin_a") ~app_id:99 ()
  in
  match resolve_ok ~db ctx with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "filtered app" "b_app99" binding.id;
      Alcotest.(check string)
        "sole under filter" "sole_eligible"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "expected resolve"

(* -------------------------------------------------------------------------- *)

let suite =
  [
    ("scope key roundtrip", `Quick, test_scope_key_roundtrip);
    ("scope rank order", `Quick, test_scope_rank_order);
    ("set get clear", `Quick, test_set_get_clear);
    ("explicit choice wins", `Quick, test_explicit_choice_wins);
    ("room+repo beats lower scopes", `Quick, test_room_repo_beats_lower_scopes);
    ( "precedence room/org/repo/default",
      `Quick,
      test_precedence_chain_room_org_repo_org_default );
    ("sole eligible when no pref", `Quick, test_sole_eligible_when_no_pref);
    ( "ambiguous private prompt no auto-pick",
      `Quick,
      test_ambiguous_private_prompt_no_auto_pick );
    ( "never select by login or other participant",
      `Quick,
      test_never_select_by_login_or_other_participant );
    ( "stale preference falls through",
      `Quick,
      test_stale_preference_falls_through );
    ("ineligible status excluded", `Quick, test_ineligible_status_excluded);
    ( "lineage match after display change",
      `Quick,
      test_lineage_match_after_display_change );
    ("none eligible prompt", `Quick, test_none_eligible_prompt);
    ( "preferences follow principal adoption",
      `Quick,
      test_preferences_follow_principal_adoption );
    ( "split retains preferences unless explicit plan",
      `Quick,
      test_split_retains_preferences_unless_explicit_plan );
    ( "split explicit preference rebind",
      `Quick,
      test_split_explicit_preference_rebind );
    ("app_id filter", `Quick, test_app_id_filter);
  ]
