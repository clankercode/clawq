(** Tests for the redacted GitHub account CLI / agent surface (P21.M4.E1.T001).

    Validates:
    - CLI output is redacted (no vault tokens, no vault row ids, no
      authorization URLs / device codes / callback errors).
    - `list` / `status` / `unlink` plan-confirm-apply path produces redacted
      output.
    - Cross-Principal inspection is refused on the CLI.
    - Minimal-build disabled guidance mentions the disabled features.
    - The narrow `github_account` agent tool returns redacted status only and
      refuses on binding-id input owned by another Principal. *)

module P = Principal_identity
module B = Github_account_binding
module Pref = Github_account_preference
module Surf = Github_account_admin_surface
module V = Github_user_token_vault
module TS = Github_user_token_store
module L = Github_user_token_lease
module Cli = Github_account_cli
module MinCli = Github_account_cli_min
module ATool = Github_account_tool

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-acct-cli-master" ()

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

let with_principal_id id f =
  let prev = Sys.getenv_opt Cli.principal_env_var in
  Unix.putenv Cli.principal_env_var id;
  Fun.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Unix.putenv Cli.principal_env_var v
      | None -> Unix.putenv Cli.principal_env_var "")
    (fun () -> f ())

let assert_error = function
  | Error e -> e
  | Ok _ -> Alcotest.fail "expected Error"

let seed_principal ~db ~id ?(revision = 1) () =
  let p =
    P.make_principal ~id:(pid id) ~revision ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  ignore
    (assert_ok (Principal_identity_store.insert_principal ~db ~now:fixed_now p))

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

let make_keys ?(key_id = "mk-acct-cli-1") () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version:1 ~aes_key ())

let sample_tokens ?(tag = "base") () =
  {
    TS.access_token = Printf.sprintf "ghu_access_CLI_%s_SECRET" tag;
    refresh_token = Some (Printf.sprintf "ghr_refresh_CLI_%s_SECRET" tag);
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

let contains hay needle = Test_helpers.string_contains hay needle

(* -------------------------------------------------------------------------- *)
(* CLI: list / status / unlink redaction                                      *)
(* -------------------------------------------------------------------------- *)

let test_list_redacts_tokens_and_vault_ids () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let pa = pid "prin_a" in
  let vault_id = "ghvault_cli_1" in
  let keys = make_keys () in
  ignore
    (create_vault ~db ~keys ~principal_id:"prin_a" ~github_user_id:100L
       ~app_id:7 ~id:vault_id);
  let vref = assert_ok (B.make_vault_ref vault_id) in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_cli" ~github_user_id:100L
       ~app_id:7 ~vault_ref:vref ());
  let tokens = sample_tokens ~tag:vault_id () in
  with_principal_id "prin_a" @@ fun () ->
  let out = Cli.cmd_with_db ~db [ "account"; "list" ] in
  Alcotest.(check bool)
    "no access token in list output" false
    (contains out tokens.access_token);
  Alcotest.(check bool)
    "no refresh token in list output" false
    (contains out (Option.get tokens.refresh_token));
  Alcotest.(check bool)
    "vault attachment indicated without raw vault row id" true
    (contains out "vault=attached"
    || contains out "vault:      attached"
    || contains out "vault:       attached");
  Alcotest.(check bool)
    "raw vault row id not exported" false (contains out vault_id);
  (* private continuation surface is referenced but no URL/code embedded. *)
  Alcotest.(check bool)
    "no authorization URL" false
    (contains out "github.com/login/oauth/authorize");
  Alcotest.(check bool) "no device user code" false (contains out "user_code");
  Alcotest.(check bool) "no client secret" false (contains out "client_secret")

let test_status_lists_all_when_no_binding () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let pa = pid "prin_a" in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_one" ~github_user_id:1L ());
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_two" ~github_user_id:2L
       ~login:(Some "carol") ());
  with_principal_id "prin_a" @@ fun () ->
  let out = Cli.cmd_with_db ~db [ "account"; "status" ] in
  Alcotest.(check bool)
    "contains both binding ids" true
    (contains out "bind_one" && contains out "bind_two");
  Alcotest.(check bool) "contains carol" true (contains out "carol")

let test_status_single_binding_includes_lineage_and_vault_flag () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let pa = pid "prin_a" in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_x" ~github_user_id:11L ());
  with_principal_id "prin_a" @@ fun () ->
  let out = Cli.cmd_with_db ~db [ "account"; "status"; "bind_x" ] in
  Alcotest.(check bool) "binding id shown" true (contains out "bind_x");
  Alcotest.(check bool) "lineage id shown" true (contains out "lineage");
  Alcotest.(check bool) "vault flag column" true (contains out "vault:");
  Alcotest.(check bool) "vault=none text" true (contains out "none")

let test_status_refuses_foreign_binding () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  seed_principal ~db ~id:"prin_b" ();
  let pb = pid "prin_b" in
  ignore
    (insert_binding ~db ~principal_id:pb ~id:"bind_b" ~github_user_id:99L ());
  with_principal_id "prin_a" @@ fun () ->
  let out = Cli.cmd_with_db ~db [ "account"; "status"; "bind_b" ] in
  Alcotest.(check bool)
    "cross-principal refused" true
    (contains out "not owned by subject principal")

let test_use_sets_preference_and_returns_redacted_receipt () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let pa = pid "prin_a" in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_use" ~github_user_id:13L
       ~login:(Some "dana") ());
  with_principal_id "prin_a" @@ fun () ->
  let out = Cli.cmd_with_db ~db [ "account"; "use"; "bind_use" ] in
  Alcotest.(check bool)
    "selected confirmation" true
    (contains out "Selected GitHub account bind_use");
  Alcotest.(check bool)
    "principal_default scope" true
    (contains out "principal_default");
  Alcotest.(check bool) "no token leakage" false (contains out "ghu_access");
  (* Preference row is now resolvable. *)
  let ctx = Pref.make_resolve_context ~principal_id:pa ~app_id:42 () in
  let result = assert_ok (Pref.resolve ~db ~context:ctx ()) in
  match result with
  | Pref.Resolved { binding; _ } ->
      Alcotest.(check string) "resolved to bind_use" "bind_use" binding.id
  | _ -> Alcotest.fail "expected resolved preference"

let test_use_refuses_foreign_binding () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  seed_principal ~db ~id:"prin_b" ();
  let pb = pid "prin_b" in
  ignore
    (insert_binding ~db ~principal_id:pb ~id:"bind_f" ~github_user_id:7L ());
  with_principal_id "prin_a" @@ fun () ->
  let out = Cli.cmd_with_db ~db [ "account"; "use"; "bind_f" ] in
  Alcotest.(check bool)
    "refuses foreign use" true
    (contains out "not owned by the current Principal")

let test_link_returns_secret_free_guidance () =
  with_db @@ fun db ->
  let out = Cli.cmd_with_db ~db [ "account"; "link" ] in
  Alcotest.(check bool)
    "mentions private continuation" true
    (contains out "private continuation");
  Alcotest.(check bool)
    "no URL embedded" false
    (contains out "github.com/login/oauth/authorize");
  Alcotest.(check bool) "no code embedded" false (contains out "user_code")

let test_relink_plan_disclosed () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let pa = pid "prin_a" in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_r" ~github_user_id:21L ());
  with_principal_id "prin_a" @@ fun () ->
  let out = Cli.cmd_with_db ~db [ "account"; "relink"; "bind_r" ] in
  Alcotest.(check bool) "plan label" true (contains out "Relink plan");
  Alcotest.(check bool) "digest line" true (contains out "digest:");
  Alcotest.(check bool) "no token leakage" false (contains out "ghu_")

let test_unlink_plan_then_apply_with_digest () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let pa = pid "prin_a" in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_u" ~github_user_id:33L ());
  with_principal_id "prin_a" @@ fun () ->
  let plan_out = Cli.cmd_with_db ~db [ "account"; "unlink"; "bind_u" ] in
  Alcotest.(check bool) "plan label" true (contains plan_out "Unlink plan");
  (* Extract digest from the plan output by scanning the "digest: <hex>" line.
     We keep this lightweight rather than touching the surface internals. *)
  let extract_digest text =
    let lines = String.split_on_char '\n' text in
    let rec find = function
      | [] -> None
      | line :: rest ->
          let trimmed = String.trim line in
          if String.starts_with ~prefix:"digest:" trimmed then
            Some
              (String.trim (String.sub trimmed 7 (String.length trimmed - 7)))
          else find rest
    in
    find lines
  in
  let digest =
    match extract_digest plan_out with
    | Some d -> d
    | None -> Alcotest.fail "no digest in plan output"
  in
  let apply_out =
    Cli.cmd_with_db ~db [ "account"; "unlink"; "bind_u"; "--digest"; digest ]
  in
  Alcotest.(check bool)
    "applied confirmation" true
    (contains apply_out "Unlinked binding bind_u");
  Alcotest.(check bool)
    "new status in receipt" true
    (contains apply_out "new_status:     unlinked");
  Alcotest.(check bool) "no token leakage" false (contains apply_out "ghu_");
  let b = Option.get (assert_ok (B.get ~db ~id:"bind_u")) in
  Alcotest.(check string)
    "binding is unlinked"
    (B.string_of_authorization_status B.Unlinked)
    (B.string_of_authorization_status b.authorization_status)

let test_unlink_apply_with_wrong_digest_refused () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let pa = pid "prin_a" in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_w" ~github_user_id:34L ());
  with_principal_id "prin_a" @@ fun () ->
  let out =
    Cli.cmd_with_db ~db
      [ "account"; "unlink"; "bind_w"; "--digest"; "deadbeef" ]
  in
  Alcotest.(check bool)
    "mismatch detected" true
    (contains out "does not match current plan digest")

(* -------------------------------------------------------------------------- *)
(* CLI: principal-id env requirement                                          *)
(* -------------------------------------------------------------------------- *)

let test_missing_principal_id_returns_actionable_error () =
  with_db @@ fun db ->
  let prev = Sys.getenv_opt Cli.principal_env_var in
  Unix.putenv Cli.principal_env_var "";
  Fun.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Unix.putenv Cli.principal_env_var v
      | None -> Unix.putenv Cli.principal_env_var "")
    (fun () ->
      let out = Cli.cmd_with_db ~db [ "account"; "list" ] in
      Alcotest.(check bool)
        "explains env var" true
        (contains out Cli.principal_env_var);
      Alcotest.(check bool) "error prefix" true (contains out "Error:"))

let test_unknown_subcommand_returns_usage () =
  with_db @@ fun db ->
  let out = Cli.cmd_with_db ~db [ "account"; "wat" ] in
  Alcotest.(check bool) "usage shown" true (contains out "Usage:");
  Alcotest.(check bool) "lists subcommands" true (contains out "list");
  Alcotest.(check bool)
    "mentions delivery module" true
    (contains out "Github_user_auth_delivery")

(* -------------------------------------------------------------------------- *)
(* Minimal-build disabled guidance                                            *)
(* -------------------------------------------------------------------------- *)

let test_min_cli_refuses_account_subcommand () =
  let out = MinCli.cmd [ "account"; "list" ] in
  Alcotest.(check bool)
    "mentions disabled" true
    (contains out "not available in the minimal build");
  Alcotest.(check bool)
    "points to full clawq binary" true
    (contains out "full `clawq` binary");
  Alcotest.(check bool)
    "names the agent tool" true
    (contains out "github_account");
  Alcotest.(check bool) "no token leakage" false (contains out "ghu_")

let test_min_cli_unknown_subcommand_returns_usage () =
  let out = MinCli.cmd [ "wat" ] in
  Alcotest.(check bool)
    "mentions disabled" true
    (contains out "not available in the minimal build")

(* -------------------------------------------------------------------------- *)
(* Agent tool: redacted status only                                           *)
(* -------------------------------------------------------------------------- *)

let test_tool_returns_redacted_status_only () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_t" ();
  let pa = pid "prin_t" in
  let vault_id = "ghvault_tool_1" in
  let keys = make_keys () in
  ignore
    (create_vault ~db ~keys ~principal_id:"prin_t" ~github_user_id:200L
       ~app_id:9 ~id:vault_id);
  let vref = assert_ok (B.make_vault_ref vault_id) in
  ignore
    (insert_binding ~db ~principal_id:pa ~id:"bind_t" ~github_user_id:200L
       ~app_id:9 ~vault_ref:vref ());
  let tokens = sample_tokens ~tag:vault_id () in
  with_principal_id "prin_t" @@ fun () ->
  let t = ATool.tool ~db in
  let out = Lwt_main.run (t.Tool.invoke (`Assoc [])) in
  Alcotest.(check bool) "title" true (contains out "GitHub account status");
  Alcotest.(check bool) "binding id shown" true (contains out "bind_t");
  Alcotest.(check bool)
    "vault_attached flag visible" true
    (contains out "vault=attached");
  Alcotest.(check bool)
    "no access token in tool output" false
    (contains out tokens.access_token);
  Alcotest.(check bool)
    "no refresh token in tool output" false
    (contains out (Option.get tokens.refresh_token));
  Alcotest.(check bool)
    "no authorization URL" false
    (contains out "github.com/login/oauth/authorize");
  Alcotest.(check bool) "no device user code" false (contains out "user_code")

let test_tool_single_binding_refuses_foreign_binding () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_owner" ();
  seed_principal ~db ~id:"prin_other" ();
  let pother = pid "prin_other" in
  ignore
    (insert_binding ~db ~principal_id:pother ~id:"bind_foreign"
       ~github_user_id:777L ());
  with_principal_id "prin_owner" @@ fun () ->
  let t = ATool.tool ~db in
  let out =
    Lwt_main.run
      (t.Tool.invoke (`Assoc [ ("binding_id", `String "bind_foreign") ]))
  in
  Alcotest.(check bool)
    "refuses foreign binding" true
    (contains out "not owned by subject principal")

let test_tool_requires_principal_env () =
  with_db @@ fun db ->
  let prev = Sys.getenv_opt ATool.principal_env_var in
  Unix.putenv ATool.principal_env_var "";
  Fun.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Unix.putenv ATool.principal_env_var v
      | None -> Unix.putenv ATool.principal_env_var "")
    (fun () ->
      let t = ATool.tool ~db in
      let out = Lwt_main.run (t.Tool.invoke (`Assoc [])) in
      Alcotest.(check bool)
        "explains principal env" true
        (contains out ATool.principal_env_var))

let test_tool_rejects_non_string_binding_id () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_t" ();
  with_principal_id "prin_t" @@ fun () ->
  let t = ATool.tool ~db in
  let out = Lwt_main.run (t.Tool.invoke (`Assoc [ ("binding_id", `Int 42) ])) in
  Alcotest.(check bool)
    "rejects non-string" true
    (contains out "must be a string")

(* -------------------------------------------------------------------------- *)
(* Suite                                                                       *)
(* -------------------------------------------------------------------------- *)

let suite =
  [
    Alcotest.test_case "list redacts tokens/vault" `Quick
      test_list_redacts_tokens_and_vault_ids;
    Alcotest.test_case "status lists all when no binding" `Quick
      test_status_lists_all_when_no_binding;
    Alcotest.test_case "status single binding" `Quick
      test_status_single_binding_includes_lineage_and_vault_flag;
    Alcotest.test_case "status refuses foreign binding" `Quick
      test_status_refuses_foreign_binding;
    Alcotest.test_case "use sets preference and returns redacted receipt" `Quick
      test_use_sets_preference_and_returns_redacted_receipt;
    Alcotest.test_case "use refuses foreign binding" `Quick
      test_use_refuses_foreign_binding;
    Alcotest.test_case "link returns secret-free guidance" `Quick
      test_link_returns_secret_free_guidance;
    Alcotest.test_case "relink plan disclosed" `Quick test_relink_plan_disclosed;
    Alcotest.test_case "unlink plan then apply with digest" `Quick
      test_unlink_plan_then_apply_with_digest;
    Alcotest.test_case "unlink apply wrong digest refused" `Quick
      test_unlink_apply_with_wrong_digest_refused;
    Alcotest.test_case "missing CLAWQ_PRINCIPAL_ID returns error" `Quick
      test_missing_principal_id_returns_actionable_error;
    Alcotest.test_case "unknown subcommand returns usage" `Quick
      test_unknown_subcommand_returns_usage;
    Alcotest.test_case "min CLI refuses account subcommand" `Quick
      test_min_cli_refuses_account_subcommand;
    Alcotest.test_case "min CLI unknown subcommand returns usage" `Quick
      test_min_cli_unknown_subcommand_returns_usage;
    Alcotest.test_case "tool returns redacted status only" `Quick
      test_tool_returns_redacted_status_only;
    Alcotest.test_case "tool refuses foreign binding" `Quick
      test_tool_single_binding_refuses_foreign_binding;
    Alcotest.test_case "tool requires CLAWQ_PRINCIPAL_ID" `Quick
      test_tool_requires_principal_env;
    Alcotest.test_case "tool rejects non-string binding_id" `Quick
      test_tool_rejects_non_string_binding_id;
  ]
