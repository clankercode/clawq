(** Tests for currently-valid eligible account resolution and first-use context
    preferences (P21.M3.E2.T002). *)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding
module Pref = Github_account_preference
module V = Github_user_token_vault
module TS = Github_user_token_store
module R = Github_eligible_account_resolve

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-eligible-resolve-master"
    ()

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  R.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_500_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let pid s = assert_ok (P.principal_id_of_string s)

let seed_principal ~db ~id ?(revision = 1) () =
  let p =
    P.make_principal ~id:(pid id) ~revision ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p))

let make_keys ?(key_id = "mk-elig-1") () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version:1 ~aes_key ())

let sample_tokens ~tag =
  {
    TS.access_token = Printf.sprintf "ghu_access_ELIG_%s_SECRET" tag;
    refresh_token = Some (Printf.sprintf "ghr_refresh_ELIG_%s_SECRET" tag);
  }

let create_vault ~db ~keys ~principal_id ~github_user_id ~app_id ~id =
  let account =
    assert_ok
      (V.make_account_key ~principal_id ~github_user_id ~app_id
         ~host:V.default_host ())
  in
  match
    V.create ~db ~keys ~id ~now:fixed_now ~account
      ~tokens:(sample_tokens ~tag:id) ~scopes:[ "repo" ]
      ~expires_at:"2026-12-01T00:00:00Z" ()
  with
  | Ok r -> r
  | Error d -> Alcotest.fail ("vault create: " ^ V.string_of_denial d)

let insert_binding ~db ~principal_id ~id ~github_user_id ?(login = Some "alice")
    ?(status = B.Authorized) ?(app_id = 42) ?vault_ref ?(lineage_id = id) () =
  let identity =
    assert_ok (B.make_account_identity ~app_id ~github_user_id ())
  in
  let b =
    B.make_binding ~id ~principal_id ~identity
      ~display:{ B.login; avatar_url = None }
      ~authorization_status:status ?vault_ref ~lineage_id ()
  in
  assert_ok (B.insert ~db ~now:fixed_now b)

let seed_valid_account ~db ~keys ~principal_id ~id ~github_user_id
    ?(login = Some "alice") ?(app_id = 42) () =
  let vault_id = "vault_" ^ id in
  ignore
    (create_vault ~db ~keys
       ~principal_id:(P.principal_id_to_string principal_id)
       ~github_user_id ~app_id ~id:vault_id);
  let vref = assert_ok (B.make_vault_ref vault_id) in
  insert_binding ~db ~principal_id ~id ~github_user_id ~login ~app_id
    ~vault_ref:vref ()

let set_pref ~db ~principal_id ~scope ~binding_id ?lineage_id () =
  let value =
    assert_ok (Pref.make_preference_value ~binding_id ?lineage_id ())
  in
  assert_ok
    (Pref.set_preference ~db ~now:fixed_now ~principal_id ~scope ~value ())

let resolve_ok ~db context = assert_ok (R.resolve ~db ~context ())

(* -------------------------------------------------------------------------- *)
(* Current validity                                                           *)
(* -------------------------------------------------------------------------- *)

let test_authorized_without_vault_not_eligible () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  ignore
    (insert_binding ~db ~principal_id:(pid "prin_a") ~id:"b_no_vault"
       ~github_user_id:1L ());
  let listed =
    assert_ok
      (R.list_currently_valid_bindings ~db ~principal_id:(pid "prin_a") ())
  in
  Alcotest.(check int) "no vault => not valid" 0 (List.length listed);
  let ctx = Pref.make_resolve_context ~principal_id:(pid "prin_a") () in
  match resolve_ok ~db ctx with
  | Pref.None_eligible { prompt } ->
      Alcotest.(check string) "reason" "no_eligible_accounts" prompt.reason
  | _ -> Alcotest.fail "expected none_eligible without vault"

let test_inactive_vault_excluded () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let keys = make_keys () in
  let pa = pid "prin_a" in
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_active"
       ~github_user_id:1L ~login:(Some "active_login") ());
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_inactive"
       ~github_user_id:2L ~login:(Some "inactive_login") ());
  (* Deactivate second vault. *)
  (match
     V.cas_set_active ~db ~keys ~id:"vault_b_inactive" ~expected_generation:1
       ~expected_active:true ~active:false ()
   with
  | Ok _ -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d));
  let listed =
    assert_ok (R.list_currently_valid_bindings ~db ~principal_id:pa ())
  in
  Alcotest.(check int) "only active vault" 1 (List.length listed);
  Alcotest.(check string) "active id" "b_active" (List.hd listed).id;
  (* Preference pointing at inactive vault must fall through to sole valid. *)
  ignore
    (set_pref ~db ~principal_id:pa ~scope:Pref.Principal_default
       ~binding_id:"b_inactive" ());
  let ctx = Pref.make_resolve_context ~principal_id:pa () in
  match resolve_ok ~db ctx with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "sole valid" "b_active" binding.id;
      Alcotest.(check string)
        "source" "sole_eligible"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "expected sole valid after inactive fallthrough"

let test_revoked_excluded_even_with_vault () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let keys = make_keys () in
  let pa = pid "prin_a" in
  let vault_id = "vault_rev" in
  ignore
    (create_vault ~db ~keys ~principal_id:"prin_a" ~github_user_id:9L ~app_id:42
       ~id:vault_id);
  let vref = assert_ok (B.make_vault_ref vault_id) in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"b_rev" ~github_user_id:9L
       ~status:B.Revoked ~vault_ref:vref ());
  let listed =
    assert_ok (R.list_currently_valid_bindings ~db ~principal_id:pa ())
  in
  Alcotest.(check int) "revoked out" 0 (List.length listed)

(* -------------------------------------------------------------------------- *)
(* Precedence walk under current validity                                     *)
(* -------------------------------------------------------------------------- *)

let test_explicit_choice_wins () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let keys = make_keys () in
  let pa = pid "prin_a" in
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_alpha"
       ~github_user_id:1L ~login:(Some "alpha") ());
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_beta"
       ~github_user_id:2L ~login:(Some "beta") ());
  let ctx =
    Pref.make_resolve_context ~principal_id:pa ~explicit_binding_id:"b_beta" ()
  in
  match resolve_ok ~db ctx with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "explicit" "b_beta" binding.id;
      Alcotest.(check string)
        "src" "explicit_choice"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "expected explicit"

let test_room_repo_beats_lower () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let keys = make_keys () in
  let pa = pid "prin_a" in
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_alpha"
       ~github_user_id:1L ());
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_beta"
       ~github_user_id:2L ());
  ignore
    (set_pref ~db ~principal_id:pa ~scope:Pref.Principal_default
       ~binding_id:"b_alpha" ());
  let room_repo =
    assert_ok
      (Pref.make_room_scope ~room_id:"room_1"
         ~repo:
           (assert_ok (Pref.make_repo_ref ~repo_full_name:"acme/widgets" ()))
         ())
  in
  ignore
    (set_pref ~db ~principal_id:pa ~scope:room_repo ~binding_id:"b_beta" ());
  let ctx =
    Pref.make_resolve_context ~principal_id:pa ~room_id:"room_1"
      ~repo_full_name:"acme/widgets" ()
  in
  match resolve_ok ~db ctx with
  | Pref.Resolved { binding; source; matched_scope; _ } ->
      Alcotest.(check string) "room+repo" "b_beta" binding.id;
      Alcotest.(check string)
        "src" "room_repo"
        (Pref.string_of_resolution_source source);
      Alcotest.(check bool) "matched scope" true (Option.is_some matched_scope)
  | _ -> Alcotest.fail "expected room_repo"

let test_precedence_chain () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let keys = make_keys () in
  let pa = pid "prin_a" in
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_default"
       ~github_user_id:10L ());
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_org"
       ~github_user_id:11L ());
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_repo"
       ~github_user_id:12L ());
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_room_org"
       ~github_user_id:13L ());
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_room_repo"
       ~github_user_id:14L ());
  ignore
    (set_pref ~db ~principal_id:pa ~scope:Pref.Principal_default
       ~binding_id:"b_default" ());
  let org = assert_ok (Pref.make_org_ref ~org_login:"acme" ()) in
  ignore
    (set_pref ~db ~principal_id:pa ~scope:(Pref.Org org) ~binding_id:"b_org" ());
  let repo = assert_ok (Pref.make_repo_ref ~repo_full_name:"acme/widgets" ()) in
  ignore
    (set_pref ~db ~principal_id:pa ~scope:(Pref.Repo repo) ~binding_id:"b_repo"
       ());
  let room_org = assert_ok (Pref.make_room_scope ~room_id:"room_x" ~org ()) in
  ignore
    (set_pref ~db ~principal_id:pa ~scope:room_org ~binding_id:"b_room_org" ());
  let room_repo = assert_ok (Pref.make_room_scope ~room_id:"room_x" ~repo ()) in
  ignore
    (set_pref ~db ~principal_id:pa ~scope:room_repo ~binding_id:"b_room_repo" ());
  let ctx_full =
    Pref.make_resolve_context ~principal_id:pa ~room_id:"room_x"
      ~repo_full_name:"acme/widgets" ()
  in
  (match resolve_ok ~db ctx_full with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "full room+repo" "b_room_repo" binding.id;
      Alcotest.(check string)
        "src" "room_repo"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "room_repo");
  (* Drop room+repo pref → room+org *)
  ignore
    (assert_ok (Pref.clear_preference ~db ~principal_id:pa ~scope:room_repo));
  (match resolve_ok ~db ctx_full with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "room+org" "b_room_org" binding.id;
      Alcotest.(check string)
        "src" "room_org"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "room_org");
  ignore
    (assert_ok (Pref.clear_preference ~db ~principal_id:pa ~scope:room_org));
  (match resolve_ok ~db ctx_full with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "principal repo" "b_repo" binding.id;
      Alcotest.(check string)
        "src" "principal_repo"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "principal_repo");
  ignore
    (assert_ok
       (Pref.clear_preference ~db ~principal_id:pa ~scope:(Pref.Repo repo)));
  (match resolve_ok ~db ctx_full with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "principal org" "b_org" binding.id;
      Alcotest.(check string)
        "src" "principal_org"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "principal_org");
  ignore
    (assert_ok
       (Pref.clear_preference ~db ~principal_id:pa ~scope:(Pref.Org org)));
  match resolve_ok ~db ctx_full with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "default" "b_default" binding.id;
      Alcotest.(check string)
        "src" "principal_default"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "default"

let test_ambiguous_never_guesses_login_or_recency () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  seed_principal ~db ~id:"prin_b" ();
  let keys = make_keys () in
  let pa = pid "prin_a" in
  let pb = pid "prin_b" in
  (* Insert beta first so recency would prefer it if we wrongly used insertion
     order without stable sort — eligible list sorts by binding id. *)
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_beta"
       ~github_user_id:2L ~login:(Some "zzz_newest") ());
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_alpha"
       ~github_user_id:1L ~login:(Some "aaa_oldest") ());
  ignore
    (seed_valid_account ~db ~keys ~principal_id:pb ~id:"b_bob"
       ~github_user_id:99L ~login:(Some "aaa_oldest") ());
  let room = assert_ok (Pref.make_room_scope ~room_id:"shared" ()) in
  ignore (set_pref ~db ~principal_id:pb ~scope:room ~binding_id:"b_bob" ());
  let ctx =
    Pref.make_resolve_context ~principal_id:pa ~room_id:"shared"
      ~repo_full_name:"acme/alpha" ()
  in
  match resolve_ok ~db ctx with
  | Pref.Ambiguous { prompt } ->
      Alcotest.(check string)
        "reason" "multiple_eligible_no_preference" prompt.reason;
      let ids =
        List.map
          (fun (c : Pref.redacted_candidate) -> c.binding_id)
          prompt.candidates
      in
      Alcotest.(check (list string))
        "stable by binding id not login/recency" [ "b_alpha"; "b_beta" ] ids;
      Alcotest.(check bool)
        "no bob" true
        (List.for_all (fun id -> id <> "b_bob") ids)
  | Pref.Resolved { binding; _ } ->
      Alcotest.fail (Printf.sprintf "must not auto-pick %s" binding.id)
  | Pref.None_eligible _ -> Alcotest.fail "has valid accounts"

let test_sole_eligible_when_no_pref () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let keys = make_keys () in
  ignore
    (seed_valid_account ~db ~keys ~principal_id:(pid "prin_a") ~id:"b_only"
       ~github_user_id:7L ());
  let ctx = Pref.make_resolve_context ~principal_id:(pid "prin_a") () in
  match resolve_ok ~db ctx with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "sole" "b_only" binding.id;
      Alcotest.(check string)
        "src" "sole_eligible"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "expected sole"

(* -------------------------------------------------------------------------- *)
(* First-use context preferences                                              *)
(* -------------------------------------------------------------------------- *)

let test_first_use_scope_specificity () =
  let pa = pid "prin_a" in
  let scope_of ctx =
    Pref.string_of_preference_scope (assert_ok (R.first_use_scope ~context:ctx))
  in
  Alcotest.(check string)
    "default" "principal_default"
    (scope_of (Pref.make_resolve_context ~principal_id:pa ()));
  Alcotest.(check string)
    "org" "org:github.com:acme"
    (scope_of (Pref.make_resolve_context ~principal_id:pa ~org_login:"Acme" ()));
  Alcotest.(check string)
    "repo" "repo:github.com:acme/widgets"
    (scope_of
       (Pref.make_resolve_context ~principal_id:pa
          ~repo_full_name:"Acme/Widgets" ()));
  Alcotest.(check string)
    "room+org" "room_org:room1:github.com:acme"
    (scope_of
       (Pref.make_resolve_context ~principal_id:pa ~room_id:"room1"
          ~org_login:"acme" ()));
  Alcotest.(check string)
    "room+repo beats org" "room_repo:room1:github.com:acme/widgets"
    (scope_of
       (Pref.make_resolve_context ~principal_id:pa ~room_id:"room1"
          ~repo_full_name:"acme/widgets" ~org_login:"acme" ()))

let test_record_first_use_and_subsequent_resolve () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let keys = make_keys () in
  let pa = pid "prin_a" in
  let b_alpha =
    seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_alpha"
      ~github_user_id:1L ()
  in
  let b_beta =
    seed_valid_account ~db ~keys ~principal_id:pa ~id:"b_beta"
      ~github_user_id:2L ()
  in
  let ctx =
    Pref.make_resolve_context ~principal_id:pa ~room_id:"room_ops"
      ~repo_full_name:"acme/widgets" ()
  in
  (* First use: ambiguous without preference. *)
  (match resolve_ok ~db ctx with
  | Pref.Ambiguous _ -> ()
  | _ -> Alcotest.fail "expected ambiguous before first-use record");
  (* Private selection of beta recorded at Room+Repo. *)
  (match
     assert_ok
       (R.record_first_use_preference ~db ~now:fixed_now ~context:ctx
          ~binding:b_beta ())
   with
  | R.Recorded sp ->
      Alcotest.(check string)
        "scope" "room_repo:room_ops:github.com:acme/widgets"
        (Pref.string_of_preference_scope sp.scope);
      Alcotest.(check (option string))
        "binding" (Some "b_beta") sp.value.binding_id
  | R.Already_set _ -> Alcotest.fail "should be first record"
  | R.Not_eligible e -> Alcotest.fail e);
  (* Subsequent resolve uses the recorded preference. *)
  (match resolve_ok ~db ctx with
  | Pref.Resolved { binding; source; _ } ->
      Alcotest.(check string) "uses first-use" "b_beta" binding.id;
      Alcotest.(check string)
        "src" "room_repo"
        (Pref.string_of_resolution_source source)
  | _ -> Alcotest.fail "expected resolve after first-use");
  (* Second record does not overwrite. *)
  (match
     assert_ok
       (R.record_first_use_preference ~db ~now:(fixed_now +. 1.) ~context:ctx
          ~binding:b_alpha ())
   with
  | R.Already_set sp ->
      Alcotest.(check (option string))
        "kept beta" (Some "b_beta") sp.value.binding_id
  | R.Recorded _ -> Alcotest.fail "must not overwrite"
  | R.Not_eligible e -> Alcotest.fail e);
  (* Ineligible binding (no vault) cannot be recorded. *)
  let bare =
    insert_binding ~db ~principal_id:pa ~id:"b_bare" ~github_user_id:3L ()
  in
  match
    assert_ok (R.record_first_use_preference ~db ~context:ctx ~binding:bare ())
  with
  | R.Not_eligible _ -> ()
  | R.Recorded _ | R.Already_set _ ->
      Alcotest.fail "must refuse invalid binding for first-use"

let test_missing_principal_fails_closed () =
  with_db @@ fun db ->
  let ctx = Pref.make_resolve_context ~principal_id:(pid "ghost") () in
  match R.resolve ~db ~context:ctx () with
  | Error msg ->
      Alcotest.(check bool)
        "mentions principal" true
        (let lower = String.lowercase_ascii msg in
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
         has "principal" || has "missing")
  | Ok _ -> Alcotest.fail "ghost principal must fail"

(* -------------------------------------------------------------------------- *)

let suite =
  [
    ( "authorized without vault not eligible",
      `Quick,
      test_authorized_without_vault_not_eligible );
    ("inactive vault excluded", `Quick, test_inactive_vault_excluded);
    ( "revoked excluded even with vault",
      `Quick,
      test_revoked_excluded_even_with_vault );
    ("explicit choice wins", `Quick, test_explicit_choice_wins);
    ("room+repo beats lower", `Quick, test_room_repo_beats_lower);
    ("precedence chain", `Quick, test_precedence_chain);
    ( "ambiguous never guesses login or recency",
      `Quick,
      test_ambiguous_never_guesses_login_or_recency );
    ("sole eligible when no pref", `Quick, test_sole_eligible_when_no_pref);
    ("first use scope specificity", `Quick, test_first_use_scope_specificity);
    ( "record first use and subsequent resolve",
      `Quick,
      test_record_first_use_and_subsequent_resolve );
    ( "missing principal fails closed",
      `Quick,
      test_missing_principal_fails_closed );
  ]
