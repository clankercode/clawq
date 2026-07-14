(** Drift checks for P21 operator contract + implementation inventory
    (P21.M4.E3.T002). *)

let contains = Test_helpers.string_contains

let repo_root () =
  let rec find_from dir =
    let has_file name = Sys.file_exists (Filename.concat dir name) in
    if has_file "dune-project" && has_file "src" && has_file "docs" then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else find_from parent
  in
  match find_from (Sys.getcwd ()) with
  | Some dir -> dir
  | None -> Sys.getcwd ()

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let doc path_rel =
  let path = Filename.concat (repo_root ()) path_rel in
  Alcotest.(check bool) (path_rel ^ " exists") true (Sys.file_exists path);
  read_file path

let must_contain ~label ~doc phrases =
  List.iter
    (fun phrase ->
      Alcotest.(check bool)
        (Printf.sprintf "%s contains %S" label phrase)
        true (contains doc phrase))
    phrases

let test_operator_contract () =
  let body = doc "docs/github-user-auth-operator-contract.md" in
  must_contain ~label:"operator contract" ~doc:body
    [
      "safe_default";
      "User_required";
      "User_preferred";
      "clawq github account";
      "clawq github user-auth";
      "Whole-store vault rollback";
      "0009-principal-token-vault-security-boundary.md";
      "principal-attribution-implementation-inventory.md";
    ];
  Alcotest.(check bool) "no access_token sample" false
    (contains body "ghu_");
  Alcotest.(check bool) "no client_secret sample" false
    (contains body "client_secret_value")

let test_inventory () =
  let body = doc "docs/principal-attribution-implementation-inventory.md" in
  must_contain ~label:"inventory" ~doc:body
    [
      "principal_identity";
      "github_user_token_vault";
      "github_attribution_authorize";
      "github_p21_integration";
      "github_user_auth_diagnostics";
      "github-user-auth-operator-contract.md";
    ];
  let acceptance_crosswalk =
    [
      ( "trust-adapter readiness",
        [
          "Trust-adapter readiness";
          "P21.M1.E1.T003";
          "clawq github user-auth readiness";
          "github_user_auth_readiness";
        ] );
      ( "linking conflicts",
        [
          "Linking conflicts";
          "P21.M1.E1.T010–T012";
          "clawq github account link|relink|unlink";
          "principal_link_exec";
        ] );
      ( "authorization",
        [
          "Authorization";
          "P21.M2.E2.T001–T004";
          "clawq github user-auth status|readiness|repair|enable|disable|apply";
          "github_user_auth_tx";
        ] );
      ( "key lifecycle",
        [
          "Key lifecycle";
          "P21.M2.E4.T001–T008";
          "Github_user_token_vault_recovery.check_compatibility";
          "github_user_token_master_key";
        ] );
      ( "attribution rollout",
        [
          "Attribution rollout";
          "P21.M3.E2.T003–T007";
          "Github_attribution_rollout.cleanup_complete";
          "github_attribution_rollout";
        ] );
      ( "delayed-job repair",
        [
          "Delayed-job repair";
          "P21.M3.E3.T003";
          "Principal_legacy_migrate.rollback_run";
          "github_durable_job_actor_attribution";
        ] );
      ( "revoke/relink",
        [
          "Revoke/relink";
          "P21.M3.E1.T003–T004";
          "clawq github account relink|unlink";
          "github_user_auth_revocation_webhook";
        ] );
      ( "backup/restore",
        [
          "Backup/restore";
          "P21.M2.E4.T008";
          "Github_user_token_vault_recovery.restore";
          "github_user_token_vault_recovery";
        ] );
      ( "pilot cleanup",
        [
          "Pilot cleanup";
          "P21.M4.E2.T003–T004";
          "Github_attribution_rollout.no_residual_authority";
          "github_p21_pilot_dryrun";
        ] );
      ( "compatibility",
        [
          "Compatibility";
          "P21.M4.E1.T004";
          "command_bridge_min";
          "github_app_pat_compat";
        ] );
    ]
  in
  must_contain ~label:"inventory crosswalk headers" ~doc:body
    [
      "Task lineage";
      "Source";
      "Schema/store";
      "API/operator surface";
      "Regression evidence";
    ];
  List.iter
    (fun (category, evidence) ->
      must_contain ~label:("inventory crosswalk " ^ category) ~doc:body evidence)
    acceptance_crosswalk;
  must_contain ~label:"inventory doc drift coverage" ~doc:body
    [ "github_p21_operator_docs" ]

let suite =
  [
    ( "operator contract states safe defaults and failure classes",
      `Quick,
      test_operator_contract );
    ( "implementation inventory crosswalks modules and tests",
      `Quick,
      test_inventory );
  ]
