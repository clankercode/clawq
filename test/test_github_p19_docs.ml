(** Documentation drift checks for P19 pilot rollout/backout guide and redacted
    receipt template (P19.M4.E3.T003).

    Asserts the canonical pilot docs exist and still state gate defaults,
    cleanup of routes/outbox/dead letters, secrets redaction, and P21 handoff.
*)

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

let test_rollout_backout_guide () =
  let body = doc "docs/pilots/p19-rollout-backout-guide.md" in
  must_contain ~label:"rollout/backout guide" ~doc:body
    [
      "Safe default";
      "enabled=false";
      "p19-merge-pilot";
      "p19-pr-review-pilot";
      "time-bounded";
      "Enable pilot gates";
      "Disable pilot gates";
      "no App/PAT fallback";
      "plan_disable";
      "setup-owned";
      "dead letter";
      "supersede";
      "pending=0";
      "in_flight=0";
      "Cleanup result";
      "P21";
      "User_required";
      "never re-enable";
      "redacted receipt";
      "Secrets and redaction";
    ]

let test_redacted_receipt_template () =
  let body = doc "docs/pilots/p19-redacted-pilot-receipt-template.md" in
  must_contain ~label:"redacted receipt template" ~doc:body
    [
      "Never";
      "secrets";
      "off by default";
      "p19-merge-pilot";
      "p19-issue-lifecycle-pilot";
      "p19-workflow-dispatch-pilot";
      "p19-code-change-pilot";
      "p19-room-background-work-pilot";
      "p19-pr-review-pilot";
      "actor";
      "pilot_name";
      "Cleanup result";
      "User_required";
      "App/PAT";
      "webhook_secret";
      "Tool catalog";
      "Outbox";
      "P19.M4.E3.T003";
    ]

let test_guide_links_receipt_and_plan () =
  let body = doc "docs/pilots/p19-rollout-backout-guide.md" in
  must_contain ~label:"guide cross-links" ~doc:body
    [
      "p19-redacted-pilot-receipt-template.md";
      "2026-07-12-github-item-room-routing.md";
      "github-route-operator-contract.md";
    ]

let test_receipt_links_guide () =
  let body = doc "docs/pilots/p19-redacted-pilot-receipt-template.md" in
  must_contain ~label:"receipt cross-links" ~doc:body
    [ "p19-rollout-backout-guide.md"; "2026-07-12-github-item-room-routing.md" ]

let suite =
  [
    ( "P19 rollout backout guide states gates cleanup and P21 handoff",
      `Quick,
      test_rollout_backout_guide );
    ( "P19 redacted pilot receipt template covers families and redaction",
      `Quick,
      test_redacted_receipt_template );
    ( "rollout guide cross-links receipt and plan",
      `Quick,
      test_guide_links_receipt_and_plan );
    ("receipt template cross-links guide", `Quick, test_receipt_links_guide);
  ]
