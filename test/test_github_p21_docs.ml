(** Documentation drift checks for P21 pilot rollout/backout guide, redacted
    receipt template, and filled dry-run receipt (P21.M4.E2.T004).

    Asserts the canonical pilot docs exist and still state gate defaults,
    cleanup of production path / bindings / credentials, secrets redaction, and
    whole-store vault rollback limitations. *)

let contains = Test_helpers.string_contains

let repo_root () =
  let rec find_from dir =
    let has_file name = Sys.file_exists (Filename.concat dir name) in
    if has_file "dune-project" && has_file "src" && has_file "docs" then
      Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else find_from parent
  in
  match find_from (Sys.getcwd ()) with
  | Some dir -> dir
  | None ->
      let exe =
        if Filename.is_relative Sys.executable_name then
          Filename.concat (Sys.getcwd ()) Sys.executable_name
        else Sys.executable_name
      in
      find_from (Filename.dirname exe) |> Option.value ~default:(Sys.getcwd ())

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

let must_not_contain ~label ~doc phrases =
  List.iter
    (fun phrase ->
      Alcotest.(check bool)
        (Printf.sprintf "%s must not contain %S" label phrase)
        false (contains doc phrase))
    phrases

let test_rollout_backout_guide () =
  let body = doc "docs/pilots/p21-rollout-backout-guide.md" in
  must_contain ~label:"P21 rollout/backout guide" ~doc:body
    [
      "Safe default";
      "enabled=false";
      "safe_default";
      "p21_production";
      "Gate_production_enable";
      "Gate_rollback";
      "Gate_cleanup";
      "no App/PAT fallback";
      "no_residual_authority";
      "pilot_credentials_destroyed";
      "bindings_unlinked";
      "Cleanup result";
      "User_required";
      "User_preferred";
      "whole-store";
      "external monotonic anchor";
      "whole_store_rollback_detectable_without_external_anchor";
      "whole_store_rollback_not_detectable_without_external_monotonic_anchor";
      "redacted receipt";
      "Secrets and redaction";
      "P21.M4.E2.T004";
      "never";
    ]

let test_redacted_receipt_template () =
  let body = doc "docs/pilots/p21-redacted-pilot-receipt-template.md" in
  must_contain ~label:"P21 redacted receipt template" ~doc:body
    [
      "Never";
      "secrets";
      "P21.M4.E2.T003";
      "P21.M4.E2.T004";
      "dry-run_blocked";
      "safe_default";
      "web PKCE";
      "device";
      "no_residual_authority";
      "access_token";
      "refresh_token";
      "User_required";
      "Cleanup result";
      "Limitations";
      "whole-store";
      "external monotonic anchor";
      "p21-rollout-backout-guide.md";
    ]

let test_filled_dryrun_receipt () =
  let body = doc "docs/pilots/receipts/p21-dual-attr-20260713-dryrun.md" in
  must_contain ~label:"filled dry-run receipt" ~doc:body
    [
      "p21-dual-attr-20260713-dryrun";
      "dry-run_blocked";
      "P21.M4.E2.T003";
      "P21.M4.E2.T004";
      "NOT EXECUTED";
      "github_p21_pilot_dryrun";
      "safe_default";
      "whole-store";
      "external monotonic anchor";
      "whole_store_rollback_detectable_without_external_anchor";
      "no secrets";
      "n/a_blocked";
      "no App/PAT fallback";
    ];
  (* Guard against accidental secret-like material in the published example. *)
  must_not_contain ~label:"filled dry-run receipt" ~doc:body
    [
      "BEGIN RSA PRIVATE KEY";
      "BEGIN PRIVATE KEY";
      "ghp_";
      "gho_";
      "ghu_";
      "client_secret=";
      "webhook_secret=";
    ]

let test_guide_links_receipts_and_plan () =
  let body = doc "docs/pilots/p21-rollout-backout-guide.md" in
  must_contain ~label:"guide cross-links" ~doc:body
    [
      "p21-redacted-pilot-receipt-template.md";
      "p21-dual-attr-20260713-dryrun.md";
      "p21-attribution-migration-rollout.md";
      "p21-teams-dual-attribution-pilot-runbook.md";
      "github-vault-recovery.md";
      "2026-07-13-github-user-attribution-and-feature-discovery.md";
      "p19-rollout-backout-guide.md";
    ]

let test_receipt_links_guide () =
  let body = doc "docs/pilots/p21-redacted-pilot-receipt-template.md" in
  must_contain ~label:"receipt template cross-links" ~doc:body
    [
      "p21-teams-dual-attribution-pilot-runbook.md";
      "p21-attribution-migration-rollout.md";
      "2026-07-13-github-user-attribution-and-feature-discovery.md";
    ]

let test_filled_receipt_links_guide () =
  let body = doc "docs/pilots/receipts/p21-dual-attr-20260713-dryrun.md" in
  must_contain ~label:"filled receipt cross-links" ~doc:body
    [
      "p21-rollout-backout-guide.md";
      "p21-redacted-pilot-receipt-template.md";
      "p21-teams-dual-attribution-pilot-runbook.md";
    ]

let suite =
  [
    ( "P21 rollout backout guide states gates cleanup and limitations",
      `Quick,
      test_rollout_backout_guide );
    ( "P21 redacted pilot receipt template covers dual-attr and redaction",
      `Quick,
      test_redacted_receipt_template );
    ( "P21 filled dry-run receipt is redacted and states blocked + limitations",
      `Quick,
      test_filled_dryrun_receipt );
    ( "P21 rollout guide cross-links receipt plan and vault recovery",
      `Quick,
      test_guide_links_receipts_and_plan );
    ( "P21 receipt template cross-links runbook and plan",
      `Quick,
      test_receipt_links_guide );
    ( "P21 filled dry-run receipt cross-links guide",
      `Quick,
      test_filled_receipt_links_guide );
  ]
