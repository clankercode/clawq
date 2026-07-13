(** Tests for App/PAT + minimal-build compatibility when user auth is
    disabled/unconfigured (P21.M4.E1.T004). *)

module C = Github_app_pat_compat
module Auth = Github_auth_selection
module Rollout = Github_attribution_rollout
module Fallback = Github_attribution_fallback
module Policy = Github_attribution_policy
module MinAccount = Github_account_cli_min
module MinUserAuth = Github_user_auth_enablement_cli_min
module E = Github_user_auth_enablement

let contains hay needle = String_util.contains hay needle

(* -------------------------------------------------------------------------- *)
(* Pure self-check                                                            *)
(* -------------------------------------------------------------------------- *)

let test_evaluate_compatibility_all_ok () =
  let r = C.evaluate_compatibility () in
  if not r.all_ok then (
    Printf.eprintf "%s\n" (C.format_report r);
    Alcotest.fail "compatibility report expected all_ok");
  Alcotest.(check bool) "all_ok" true r.all_ok;
  Alcotest.(check int) "schema" 1 C.schema_version;
  Alcotest.(check bool) "user auth unavailable" false r.user_auth.available;
  Alcotest.(check bool) "production off" false r.user_auth.production_enabled

let test_report_json_redacted () =
  let r = C.evaluate_compatibility () in
  let j = C.report_to_json r in
  let s = Yojson.Safe.to_string j in
  Alcotest.(check bool) "has all_ok" true (contains s "all_ok");
  Alcotest.(check bool) "no ghp_" false (contains s "ghp_compat_probe");
  Alcotest.(check bool) "no ghu_" false (contains s "ghu_")

(* -------------------------------------------------------------------------- *)
(* App/PAT paths with user auth off                                           *)
(* -------------------------------------------------------------------------- *)

let test_app_reads_open_when_user_auth_off () =
  List.iter
    (fun action ->
      let path = C.resolve_action ~action () in
      Alcotest.(check bool)
        (action ^ " app primary") true (C.is_app_primary path);
      Alcotest.(check bool)
        (action ^ " policy permitted")
        true
        (C.policy_permitted_with_user_auth_off ~action))
    (C.app_read_actions ())

let test_pat_reads_open_when_user_auth_off () =
  List.iter
    (fun action ->
      let path = C.resolve_action ~action () in
      Alcotest.(check bool) (action ^ " pat compat") true (C.is_pat_compat path);
      Alcotest.(check bool)
        (action ^ " policy permitted")
        true
        (C.policy_permitted_with_user_auth_off ~action))
    (C.pat_read_actions ())

let test_user_attributed_denied_no_fallback () =
  List.iter
    (fun action ->
      let path = C.resolve_action ~action () in
      Alcotest.(check bool)
        (action ^ " denied no App/PAT fallback")
        true
        (C.is_denied_without_app_pat_fallback path);
      Alcotest.(check bool)
        (action ^ " not policy-permitted under user-auth off")
        false
        (C.policy_permitted_with_user_auth_off ~action);
      match path with
      | Rollout.Path_denied { message; _ } ->
          let m = String.lowercase_ascii message in
          Alcotest.(check bool)
            (action ^ " message mentions fallback ban")
            true
            (contains m "app/pat" || contains m "fall back"
           || contains m "cannot fall")
      | other ->
          Alcotest.fail
            (Printf.sprintf "%s: expected Path_denied, got %s" action
               (Rollout.effective_path_to_string other)))
    (C.user_attributed_actions ())

let test_unconfigured_alias_matches_off () =
  let a = C.resolve_action ~ctx:(C.user_auth_off ()) ~action:"read" () in
  let b =
    C.resolve_action ~ctx:(C.user_auth_unconfigured ()) ~action:"read" ()
  in
  Alcotest.(check string)
    "same path"
    (Rollout.effective_path_to_string a)
    (Rollout.effective_path_to_string b)

(* -------------------------------------------------------------------------- *)
(* PAT exact-Repo only                                                        *)
(* -------------------------------------------------------------------------- *)

let test_pat_exact_repo_transport () =
  let auth = Auth.snapshot_of_parts ~pat:"ghp_test_pat_token" () in
  let sel = C.select_transport ~auth ~repo_full_name:"acme/alpha" () in
  Alcotest.(check bool) "chosen pat" true (sel.chosen = `Pat);
  Alcotest.(check string)
    "reason" "pat_exact_repo"
    (Auth.selection_reason_to_string sel.reason);
  Alcotest.(check bool) "exact-repo only" true (C.pat_is_exact_repo_only sel)

let test_pat_cannot_claim_org () =
  let auth = Auth.snapshot_of_parts ~pat:"ghp_test_pat_token" () in
  let sel = C.select_org_transport ~auth ~org:"acme" () in
  Alcotest.(check bool) "chosen none" true (sel.chosen = `None);
  Alcotest.(check string)
    "reason" "rejected_org_requires_app"
    (Auth.selection_reason_to_string sel.reason);
  Alcotest.(check bool) "exact-repo only" true (C.pat_is_exact_repo_only sel);
  Alcotest.(check bool)
    "explanation mentions App" true
    (contains (String.lowercase_ascii sel.explanation) "app")

(* -------------------------------------------------------------------------- *)
(* Fallback gate off                                                          *)
(* -------------------------------------------------------------------------- *)

let test_fallback_user_preferred_gate_off () =
  match C.fallback_with_gate_off ~action:"comment" () with
  | Fallback.Deny d ->
      Alcotest.(check string) "code" "attribution_gate_disabled" d.code;
      Alcotest.(check bool)
        "mentions no fallback" true
        (contains (String.lowercase_ascii d.message) "cannot fall back"
        || contains (String.lowercase_ascii d.message) "app or pat")
  | Fallback.Allow _ -> Alcotest.fail "expected Deny for User_preferred"

let test_fallback_app_installation_gate_off () =
  let req : Policy.requirement =
    {
      action = "read";
      tier = Policy.Low;
      attribution = Policy.App_installation;
      pilot_allowed = false;
    }
  in
  match
    C.fallback_with_gate_off ~action:"read" ~requirement:req
      ~preview_actor:Fallback.Names_app ()
  with
  | Fallback.Allow a ->
      Alcotest.(check bool) "mode App" true (a.mode = Fallback.App);
      Alcotest.(check bool) "not fallback" false a.used_app_fallback
  | Fallback.Deny d ->
      Alcotest.fail ("expected Allow for App_installation, got " ^ d.code)

(* -------------------------------------------------------------------------- *)
(* Additive migration + schema                                                *)
(* -------------------------------------------------------------------------- *)

let test_migration_retains_pat_until_confirm () =
  let before = Auth.snapshot_of_parts ~pat:"ghp_old" () in
  let after_keep =
    Auth.snapshot_of_parts ~pat:"ghp_old"
      ~app:
        {
          app_id = 9;
          private_key_path = "/tmp/k.pem";
          webhook_secret = "wh";
          installations = [];
        }
      ()
  in
  let after_drop =
    Auth.snapshot_of_parts
      ~app:
        {
          app_id = 9;
          private_key_path = "/tmp/k.pem";
          webhook_secret = "wh";
          installations = [];
        }
      ()
  in
  (match
     C.migration_is_additive ~before ~after:after_keep ~confirmed_apply:false
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match
    C.migration_is_additive ~before ~after:after_drop ~confirmed_apply:false
  with
  | Error msg -> Alcotest.(check bool) "mentions PAT" true (contains msg "PAT")
  | Ok () -> Alcotest.fail "drop without confirm must error"

let test_enablement_schema_additive_and_idempotent () =
  Alcotest.(check bool) "ddl additive" true (C.enablement_schema_is_additive ());
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      E.ensure_schema db;
      let g1 = E.load_gate ~db () in
      Alcotest.(check bool) "default production off" false g1.production.enabled;
      E.ensure_schema db;
      let g2 = E.load_gate ~db () in
      Alcotest.(check int) "revision stable" g1.revision g2.revision;
      Alcotest.(check bool)
        "still off after re-ensure" false g2.production.enabled)

let test_schema_ddl_rejects_drop () =
  Alcotest.(check bool)
    "create ok" true
    (C.schema_ddl_is_additive
       ~ddl:"CREATE TABLE IF NOT EXISTS foo (id TEXT PRIMARY KEY)");
  Alcotest.(check bool)
    "drop rejected" false
    (C.schema_ddl_is_additive
       ~ddl:"DROP TABLE IF EXISTS foo; CREATE TABLE foo (id)")

(* -------------------------------------------------------------------------- *)
(* Minimal-build coverage (extend account + user-auth stubs)                  *)
(* -------------------------------------------------------------------------- *)

let test_min_surfaces_inventory () =
  let surfaces = C.min_build_surfaces () in
  Alcotest.(check int) "two surfaces" 2 (List.length surfaces);
  Alcotest.(check bool)
    "refuse without integrations" true
    (C.min_surfaces_refuse_without_integrations ());
  List.iter
    (fun (s : C.min_surface) ->
      Alcotest.(check bool)
        (s.command_prefix ^ " minimal")
        true
        (contains (String.lowercase_ascii s.disabled_message) "minimal");
      Alcotest.(check bool)
        (s.command_prefix ^ " full binary")
        true
        (contains s.disabled_message "full `clawq` binary"
        || contains s.disabled_message "full clawq binary");
      Alcotest.(check bool)
        (s.command_prefix ^ " no ghp")
        false
        (contains s.disabled_message "ghp_");
      Alcotest.(check bool)
        (s.command_prefix ^ " no ghu")
        false
        (contains s.disabled_message "ghu_"))
    surfaces

let test_min_account_subcommands_all_refuse () =
  List.iter
    (fun args ->
      let out = MinAccount.cmd args in
      Alcotest.(check bool)
        (String.concat " " args ^ " disabled")
        true
        (contains out "not available in the minimal build");
      Alcotest.(check bool)
        (String.concat " " args ^ " full clawq")
        true
        (contains out "full `clawq` binary"))
    [
      [ "account"; "list" ];
      [ "account"; "status" ];
      [ "account"; "use"; "bind_1" ];
      [ "account"; "link" ];
      [ "account"; "relink"; "bind_1" ];
      [ "account"; "unlink"; "bind_1" ];
      [ "account" ];
    ]

let test_min_user_auth_subcommands_all_refuse () =
  List.iter
    (fun args ->
      let out = MinUserAuth.cmd args in
      Alcotest.(check bool)
        (String.concat " " args ^ " disabled")
        true
        (contains out "not available in the minimal build"
        || contains out "minimal build");
      Alcotest.(check bool)
        (String.concat " " args ^ " full clawq")
        true
        (contains out "full `clawq` binary" || contains out "full clawq binary"))
    [
      [ "user-auth"; "status" ];
      [ "user-auth"; "readiness" ];
      [ "user-auth"; "repair" ];
      [ "user-auth"; "enable" ];
      [ "user-auth"; "disable" ];
      [ "user-auth"; "apply"; "plan_1" ];
      [ "user-auth" ];
    ]

let test_command_bridge_min_routes_account_and_user_auth () =
  let account_out = Command_bridge_min.handle [ "github"; "account"; "list" ] in
  Alcotest.(check bool)
    "bridge account disabled" true
    (contains account_out "not available in the minimal build");
  let ua_out = Command_bridge_min.handle [ "github"; "user-auth"; "status" ] in
  Alcotest.(check bool)
    "bridge user-auth disabled" true
    (contains ua_out "minimal");
  (* Other github surfaces still use the generic unsupported path. *)
  let route_out = Command_bridge_min.handle [ "github"; "route"; "list" ] in
  Alcotest.(check bool)
    "bridge route disabled" true
    (contains route_out "not available in the minimal build")

let test_min_messages_match_module_exports () =
  Alcotest.(check string)
    "account message" MinAccount.disabled_message
    (C.min_account_disabled_message ());
  Alcotest.(check string)
    "user-auth message" MinUserAuth.disabled_message
    (C.min_user_auth_disabled_message ())

(* -------------------------------------------------------------------------- *)

let suite =
  [
    Alcotest.test_case "evaluate_compatibility all ok under user-auth off"
      `Quick test_evaluate_compatibility_all_ok;
    Alcotest.test_case "report json omits probe tokens" `Quick
      test_report_json_redacted;
    Alcotest.test_case "App reads open when user auth off" `Quick
      test_app_reads_open_when_user_auth_off;
    Alcotest.test_case "PAT reads open when user auth off" `Quick
      test_pat_reads_open_when_user_auth_off;
    Alcotest.test_case "user-attributed denied without App/PAT fallback" `Quick
      test_user_attributed_denied_no_fallback;
    Alcotest.test_case "unconfigured alias matches off" `Quick
      test_unconfigured_alias_matches_off;
    Alcotest.test_case "PAT exact-Repo transport" `Quick
      test_pat_exact_repo_transport;
    Alcotest.test_case "PAT cannot claim Org" `Quick test_pat_cannot_claim_org;
    Alcotest.test_case "fallback User_preferred gate off denies" `Quick
      test_fallback_user_preferred_gate_off;
    Alcotest.test_case "fallback App_installation gate off allows primary"
      `Quick test_fallback_app_installation_gate_off;
    Alcotest.test_case "migration retains PAT until confirm" `Quick
      test_migration_retains_pat_until_confirm;
    Alcotest.test_case "enablement schema additive and idempotent" `Quick
      test_enablement_schema_additive_and_idempotent;
    Alcotest.test_case "schema_ddl rejects DROP" `Quick
      test_schema_ddl_rejects_drop;
    Alcotest.test_case "min surfaces inventory" `Quick
      test_min_surfaces_inventory;
    Alcotest.test_case "min account subcommands all refuse" `Quick
      test_min_account_subcommands_all_refuse;
    Alcotest.test_case "min user-auth subcommands all refuse" `Quick
      test_min_user_auth_subcommands_all_refuse;
    Alcotest.test_case "command_bridge_min routes account and user-auth" `Quick
      test_command_bridge_min_routes_account_and_user_auth;
    Alcotest.test_case "min messages match module exports" `Quick
      test_min_messages_match_module_exports;
  ]
