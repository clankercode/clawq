(* Tests for vault master-key source version and startup boundary
   (P21.M2.E4.T006). *)

module M = Github_user_token_master_key

let secret_material = "vault-master-key-SUPER-SECRET-do-not-export-42"
let other_secret = "another-secret-key-material-NEVER-LOG"

let active_env ?(var = M.default_env_var) ?(key_id = "mk-1") ?(key_version = 1)
    ?min_length ?expected_length () =
  match
    M.make_source
      ~kind:(M.Env { var_name = var })
      ~key_id ~key_version ~role:M.Active ?min_length ?expected_length ()
  with
  | Ok c -> c
  | Error e -> Alcotest.fail ("make_source env: " ^ e)

let active_file ?(path = "/run/clawq/vault.key") ?(key_id = "mk-file-1")
    ?(key_version = 1) ?max_file_mode ?min_length ?expected_length () =
  match
    M.make_source
      ~kind:(M.File { path })
      ~key_id ~key_version ~role:M.Active ?max_file_mode ?min_length
      ?expected_length ()
  with
  | Ok c -> c
  | Error e -> Alcotest.fail ("make_source file: " ^ e)

let staged_env ?(var = "CLAWQ_GITHUB_VAULT_STAGED_KEY") ?(key_id = "mk-2")
    ?(key_version = 2) () =
  match
    M.make_source
      ~kind:(M.Env { var_name = var })
      ~key_id ~key_version ~role:M.Staged ()
  with
  | Ok c -> c
  | Error e -> Alcotest.fail ("make_source staged: " ^ e)

let keyring sources =
  match M.make_keyring ~sources () with
  | Ok k -> k
  | Error e -> Alcotest.fail ("make_keyring: " ^ e)

let env_map pairs ~var_name = List.assoc_opt var_name pairs
let file_table : (string, M.file_stat * string) Hashtbl.t = Hashtbl.create 8
let reset_files () = Hashtbl.clear file_table

let put_file ~path ?(mode = 0o600) ?(readable = true) ?(is_regular = true)
    content =
  let size = String.length content in
  Hashtbl.replace file_table path
    ({ M.exists = true; readable; is_regular; mode; size }, content)

let file_stat ~path =
  match Hashtbl.find_opt file_table path with
  | Some (st, _) -> Ok st
  | None ->
      Ok
        {
          M.exists = false;
          readable = false;
          is_regular = false;
          mode = 0;
          size = 0;
        }

let file_read ~path =
  match Hashtbl.find_opt file_table path with
  | Some (_, content) -> Ok content
  | None -> Error "enoent"

let probe_env pairs keyring =
  M.probe ~env_reader:(env_map pairs) ~file_stat ~file_read keyring

let probe_files keyring =
  M.probe ~env_reader:(fun ~var_name:_ -> None) ~file_stat ~file_read keyring

let reasons_of = function
  | M.Ready _ -> []
  | M.NotReady { reasons; _ } -> List.map M.string_of_reason reasons

let expect_ready label r =
  Alcotest.(check bool) (label ^ " ready") true (M.is_ready r);
  Alcotest.(check bool)
    (label ^ " allows auth") true
    (M.allows_user_authorization r)

let expect_not_ready label r ~has =
  Alcotest.(check bool) (label ^ " not ready") false (M.is_ready r);
  Alcotest.(check bool)
    (label ^ " refuses auth") false
    (M.allows_user_authorization r);
  let rs = reasons_of r in
  Alcotest.(check bool) (label ^ " has " ^ has) true (List.mem has rs)

(* -------------------------------------------------------------------------- *)
(* Config validation                                                          *)
(* -------------------------------------------------------------------------- *)

let test_make_and_validate_ok () =
  let src = active_env () in
  let k = keyring [ src ] in
  Alcotest.(check int) "schema" M.schema_version k.schema_version;
  match M.validate_keyring_config k with
  | Ok () -> ()
  | Error rs ->
      Alcotest.fail
        ("unexpected validate error: "
        ^ String.concat "," (List.map M.string_of_reason rs))

let test_validate_duplicate_active () =
  let a = active_env ~key_id:"mk-a" () in
  let b = active_env ~var:"OTHER_VAR" ~key_id:"mk-b" () in
  let k = keyring [ a; b ] in
  match M.validate_keyring_config k with
  | Ok () -> Alcotest.fail "expected duplicate active to fail"
  | Error rs ->
      Alcotest.(check bool) "duplicated" true (List.mem M.Duplicated rs)

let test_validate_duplicate_key_id () =
  let a = active_env ~key_id:"same" () in
  let b =
    match
      M.make_source
        ~kind:(M.Env { var_name = "STAGED" })
        ~key_id:"same" ~key_version:2 ~role:M.Staged ()
    with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in
  let k = keyring [ a; b ] in
  match M.validate_keyring_config k with
  | Ok () -> Alcotest.fail "expected duplicate key_id to fail"
  | Error rs ->
      Alcotest.(check bool) "duplicated key_id" true (List.mem M.Duplicated rs)

let test_validate_unsupported_schema () =
  let src = active_env () in
  match M.make_keyring ~schema_version:99 ~sources:[ src ] () with
  | Ok _ -> Alcotest.fail "schema 99 should be rejected at construction"
  | Error msg ->
      Alcotest.(check bool)
        "mentions unsupported" true
        (String_util.contains msg "unsupported")

let test_validate_no_active () =
  let staged = staged_env () in
  (* make_keyring allows construction; validate rejects missing Active. *)
  let k = { M.schema_version = 1; sources = [ staged ] } in
  match M.validate_keyring_config k with
  | Ok () -> Alcotest.fail "expected no_active"
  | Error rs -> Alcotest.(check bool) "no_active" true (List.mem M.No_active rs)

let test_validate_retired_unsupported () =
  let retired =
    match
      M.make_source
        ~kind:(M.Env { var_name = "OLD" })
        ~key_id:"mk-old" ~key_version:1 ~role:M.Retired ()
    with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in
  let active = active_env ~key_id:"mk-new" ~key_version:2 () in
  let k = keyring [ active; retired ] in
  match M.validate_keyring_config k with
  | Ok () -> Alcotest.fail "retired live source should be unsupported"
  | Error rs ->
      Alcotest.(check bool) "unsupported" true (List.mem M.Unsupported rs)

(* -------------------------------------------------------------------------- *)
(* Ready / fail-closed                                                        *)
(* -------------------------------------------------------------------------- *)

let test_ready_from_env () =
  let k = keyring [ active_env () ] in
  let r = probe_env [ (M.default_env_var, secret_material) ] k in
  expect_ready "env" r;
  match M.active_metadata r with
  | None -> Alcotest.fail "missing active metadata"
  | Some m ->
      Alcotest.(check string) "key_id" "mk-1" m.key_id;
      Alcotest.(check int) "key_version" 1 m.key_version;
      Alcotest.(check string) "role" "active" (M.string_of_role m.role)

let test_missing_env_fail_closed () =
  let k = keyring [ active_env () ] in
  let r = probe_env [] k in
  expect_not_ready "missing env" r ~has:"missing"

let test_empty_env_fail_closed () =
  let k = keyring [ active_env () ] in
  let r = probe_env [ (M.default_env_var, "   ") ] k in
  expect_not_ready "empty env" r ~has:"empty"

let test_wrong_length_fail_closed () =
  let k = keyring [ active_env ~expected_length:32 () ] in
  let r = probe_env [ (M.default_env_var, "too-short") ] k in
  expect_not_ready "wrong length" r ~has:"wrong"

let test_min_length_ok () =
  let k = keyring [ active_env ~min_length:16 () ] in
  let r = probe_env [ (M.default_env_var, secret_material) ] k in
  expect_ready "min length" r

let test_ready_from_file () =
  reset_files ();
  let path = "/tmp/clawq-vault-test.key" in
  put_file ~path ~mode:0o600 secret_material;
  let k = keyring [ active_file ~path () ] in
  let r = probe_files k in
  expect_ready "file" r

let test_file_missing_fail_closed () =
  reset_files ();
  let k = keyring [ active_file ~path:"/no/such/vault.key" () ] in
  let r = probe_files k in
  expect_not_ready "file missing" r ~has:"missing"

let test_file_permissions_fail_closed () =
  reset_files ();
  let path = "/tmp/clawq-vault-world.key" in
  put_file ~path ~mode:0o644 secret_material;
  let k = keyring [ active_file ~path () ] in
  let r = probe_files k in
  expect_not_ready "world-readable" r ~has:"permissions"

let test_file_group_readable_fail_closed () =
  reset_files ();
  let path = "/tmp/clawq-vault-group.key" in
  put_file ~path ~mode:0o640 secret_material;
  let k = keyring [ active_file ~path () ] in
  let r = probe_files k in
  expect_not_ready "group-readable" r ~has:"permissions"

let test_file_not_regular_fail_closed () =
  reset_files ();
  let path = "/tmp/clawq-vault-dir" in
  put_file ~path ~mode:0o700 ~is_regular:false secret_material;
  let k = keyring [ active_file ~path () ] in
  let r = probe_files k in
  expect_not_ready "not regular" r ~has:"permissions"

let test_file_unreadable_inaccessible () =
  reset_files ();
  let path = "/tmp/clawq-vault-locked.key" in
  put_file ~path ~mode:0o600 ~readable:false secret_material;
  let k = keyring [ active_file ~path () ] in
  let r = probe_files k in
  expect_not_ready "unreadable" r ~has:"inaccessible"

let test_file_read_error_inaccessible () =
  reset_files ();
  let path = "/tmp/clawq-vault-io.key" in
  (* Stat succeeds; read fails. *)
  Hashtbl.replace file_table path
    ( {
        M.exists = true;
        readable = true;
        is_regular = true;
        mode = 0o600;
        size = 10;
      },
      secret_material );
  let bad_read ~path:_ = Error "eio" in
  let k = keyring [ active_file ~path () ] in
  let r =
    M.probe
      ~env_reader:(fun ~var_name:_ -> None)
      ~file_stat ~file_read:bad_read k
  in
  expect_not_ready "read error" r ~has:"inaccessible"

let test_no_silent_fallback_to_other_env () =
  (* Only the configured var is accepted; CLAWQ_MASTER_KEY must not silently
     satisfy the vault keyring. *)
  let k = keyring [ active_env () ] in
  let r =
    probe_env
      [ ("CLAWQ_MASTER_KEY", secret_material); ("OTHER", other_secret) ]
      k
  in
  expect_not_ready "no cross-fallback" r ~has:"missing"

let test_duplicate_active_probe_not_ready () =
  (* Even if validate is bypassed somehow with two actives that both load. *)
  let a = active_env ~key_id:"a" ~var:"VAR_A" () in
  let b = active_env ~key_id:"b" ~var:"VAR_B" () in
  let k = { M.schema_version = 1; sources = [ a; b ] } in
  let r = probe_env [ ("VAR_A", secret_material); ("VAR_B", other_secret) ] k in
  expect_not_ready "two actives" r ~has:"duplicated";
  Alcotest.(check bool) "refuses auth" false (M.allows_user_authorization r)

let test_staged_optional_missing_ok () =
  let k = keyring [ active_env (); staged_env () ] in
  let r = probe_env [ (M.default_env_var, secret_material) ] k in
  expect_ready "staged missing ok" r;
  match r with
  | M.Ready { available; _ } ->
      Alcotest.(check int) "no available staged" 0 (List.length available)
  | M.NotReady _ -> Alcotest.fail "expected ready"

let test_staged_present_listed () =
  let k = keyring [ active_env (); staged_env () ] in
  let r =
    probe_env
      [
        (M.default_env_var, secret_material);
        ("CLAWQ_GITHUB_VAULT_STAGED_KEY", other_secret);
      ]
      k
  in
  expect_ready "staged present" r;
  match r with
  | M.Ready { available; _ } ->
      Alcotest.(check int) "one staged" 1 (List.length available);
      Alcotest.(check string) "staged id" "mk-2" (List.hd available).key_id
  | M.NotReady _ -> Alcotest.fail "expected ready"

let test_staged_wrong_hard_fail () =
  let staged =
    match
      M.make_source
        ~kind:(M.Env { var_name = "CLAWQ_GITHUB_VAULT_STAGED_KEY" })
        ~key_id:"mk-2" ~key_version:2 ~role:M.Staged ~expected_length:64 ()
    with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in
  let k = keyring [ active_env (); staged ] in
  let r =
    probe_env
      [
        (M.default_env_var, secret_material);
        ("CLAWQ_GITHUB_VAULT_STAGED_KEY", "short");
      ]
      k
  in
  expect_not_ready "staged wrong" r ~has:"wrong"

(* -------------------------------------------------------------------------- *)
(* Redaction                                                                  *)
(* -------------------------------------------------------------------------- *)

let test_diagnostics_redact_ready () =
  let k = keyring [ active_env () ] in
  let r = probe_env [ (M.default_env_var, secret_material) ] k in
  let d = M.diagnostics ~schema_version:k.schema_version r in
  let json = M.diagnostics_to_json d in
  let text = M.format_diagnostics d in
  Alcotest.(check bool) "ready" true d.ready;
  Alcotest.(check bool)
    "diag lacks secret" false
    (M.diagnostics_contains_plaintext ~diagnostics:d ~plaintext:secret_material);
  Alcotest.(check bool)
    "json lacks secret" false
    (M.json_contains_plaintext ~json ~plaintext:secret_material);
  Alcotest.(check bool)
    "format lacks secret" false
    (String_util.contains text secret_material);
  Alcotest.(check bool)
    "format has key_id" true
    (String_util.contains text "mk-1");
  Alcotest.(check (option string)) "active key id" (Some "mk-1") d.active_key_id

let test_diagnostics_redact_not_ready () =
  let k = keyring [ active_env () ] in
  (* Put secret in env but use empty configured path via wrong var so missing. *)
  let r =
    probe_env
      [ ("WRONG_VAR", secret_material); ("CLAWQ_MASTER_KEY", other_secret) ]
      k
  in
  let d = M.diagnostics ~schema_version:k.schema_version r in
  let json = M.diagnostics_to_json d in
  let text = M.format_diagnostics d in
  Alcotest.(check bool) "not ready" false d.ready;
  Alcotest.(check bool) "refuses" false d.allows_user_authorization;
  List.iter
    (fun secret ->
      Alcotest.(check bool)
        ("diag lacks " ^ secret) false
        (M.diagnostics_contains_plaintext ~diagnostics:d ~plaintext:secret);
      Alcotest.(check bool)
        ("json lacks " ^ secret) false
        (M.json_contains_plaintext ~json ~plaintext:secret);
      Alcotest.(check bool)
        ("format lacks " ^ secret) false
        (String_util.contains text secret))
    [ secret_material; other_secret ];
  Alcotest.(check bool)
    "mentions refuse" true
    (String_util.contains d.note "refuse")

let test_observation_does_not_retain_material () =
  let src = active_env () in
  let k = keyring [ src ] in
  let obs =
    M.observe_keyring
      ~env_reader:(env_map [ (M.default_env_var, secret_material) ])
      ~file_stat ~file_read k
  in
  match obs with
  | [ o ] ->
      Alcotest.(check bool) "valid" true o.material.valid;
      Alcotest.(check (option int))
        "length only"
        (Some (String.length secret_material))
        o.material.byte_length;
      (* Ensure no field accidentally holds the secret via access_error. *)
      Alcotest.(check (option string)) "no access error" None o.access_error;
      let d =
        M.diagnostics ~schema_version:1
          (M.evaluate ~keyring:k ~observations:obs)
      in
      Alcotest.(check bool)
        "still redacted" false
        (M.diagnostics_contains_plaintext ~diagnostics:d
           ~plaintext:secret_material)
  | _ -> Alcotest.fail "expected one observation"

let test_default_file_probe_inaccessible () =
  (* Without injectable file probes, File sources fail closed. *)
  let k = keyring [ active_file () ] in
  let r = M.probe ~env_reader:(fun ~var_name:_ -> None) k in
  expect_not_ready "default file probe" r ~has:"inaccessible"

let test_string_of_helpers () =
  Alcotest.(check string) "missing" "missing" (M.string_of_reason M.Missing);
  Alcotest.(check string) "wrong" "wrong" (M.string_of_reason M.Wrong);
  Alcotest.(check string)
    "duplicated" "duplicated"
    (M.string_of_reason M.Duplicated);
  Alcotest.(check string)
    "unsupported" "unsupported"
    (M.string_of_reason M.Unsupported);
  Alcotest.(check string)
    "inaccessible" "inaccessible"
    (M.string_of_reason M.Inaccessible);
  Alcotest.(check string)
    "permissions" "permissions"
    (M.string_of_reason M.Permissions);
  Alcotest.(check string)
    "env kind" "env:FOO"
    (M.string_of_source_kind (M.Env { var_name = "FOO" }));
  Alcotest.(check string)
    "file kind" "file:/p"
    (M.string_of_source_kind (M.File { path = "/p" }));
  Alcotest.(check string)
    "default env var" "CLAWQ_GITHUB_VAULT_MASTER_KEY" M.default_env_var;
  Alcotest.(check int) "default mode" 0o600 M.default_max_file_mode

let suite =
  [
    Alcotest.test_case "make/validate happy path" `Quick
      test_make_and_validate_ok;
    Alcotest.test_case "validate rejects duplicate active" `Quick
      test_validate_duplicate_active;
    Alcotest.test_case "validate rejects duplicate key_id" `Quick
      test_validate_duplicate_key_id;
    Alcotest.test_case "validate rejects unsupported schema" `Quick
      test_validate_unsupported_schema;
    Alcotest.test_case "validate rejects no active" `Quick
      test_validate_no_active;
    Alcotest.test_case "validate rejects retired live source" `Quick
      test_validate_retired_unsupported;
    Alcotest.test_case "ready from env source" `Quick test_ready_from_env;
    Alcotest.test_case "missing env fail-closed" `Quick
      test_missing_env_fail_closed;
    Alcotest.test_case "empty env fail-closed" `Quick test_empty_env_fail_closed;
    Alcotest.test_case "wrong length fail-closed" `Quick
      test_wrong_length_fail_closed;
    Alcotest.test_case "min length accepts valid material" `Quick
      test_min_length_ok;
    Alcotest.test_case "ready from file source" `Quick test_ready_from_file;
    Alcotest.test_case "file missing fail-closed" `Quick
      test_file_missing_fail_closed;
    Alcotest.test_case "file world-readable fail-closed" `Quick
      test_file_permissions_fail_closed;
    Alcotest.test_case "file group-readable fail-closed" `Quick
      test_file_group_readable_fail_closed;
    Alcotest.test_case "file not regular fail-closed" `Quick
      test_file_not_regular_fail_closed;
    Alcotest.test_case "file unreadable inaccessible" `Quick
      test_file_unreadable_inaccessible;
    Alcotest.test_case "file read error inaccessible" `Quick
      test_file_read_error_inaccessible;
    Alcotest.test_case "no silent fallback to other env vars" `Quick
      test_no_silent_fallback_to_other_env;
    Alcotest.test_case "duplicate active probe not ready" `Quick
      test_duplicate_active_probe_not_ready;
    Alcotest.test_case "missing staged optional still ready" `Quick
      test_staged_optional_missing_ok;
    Alcotest.test_case "present staged listed as available" `Quick
      test_staged_present_listed;
    Alcotest.test_case "wrong staged material fail-closed" `Quick
      test_staged_wrong_hard_fail;
    Alcotest.test_case "ready diagnostics redact key material" `Quick
      test_diagnostics_redact_ready;
    Alcotest.test_case "not-ready diagnostics redact secrets" `Quick
      test_diagnostics_redact_not_ready;
    Alcotest.test_case "observations retain length only" `Quick
      test_observation_does_not_retain_material;
    Alcotest.test_case "default file probe is inaccessible" `Quick
      test_default_file_probe_inaccessible;
    Alcotest.test_case "string_of helpers and defaults" `Quick
      test_string_of_helpers;
  ]
