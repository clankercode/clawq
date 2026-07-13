(** Documentation drift checks for GitHub route model ADR, glossary, and
    operator contract (P19.M2.E3.T005).

    Asserts the canonical docs exist and still state selector specificity,
    fail-closed mute, Org-requires-App, plan-not-apply on callback, delivery ACK
    independence from Connector, and secrets redaction. *)

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

let test_adr_0008_route_model_and_setup () =
  let body = doc "docs/adr/0008-github-route-model-and-setup.md" in
  must_contain ~label:"ADR 0008" ~doc:body
    [
      "Item > Repo > Org";
      "fail-closed mute";
      "no-fallthrough";
      "Org routes require live GitHub App";
      "callback never counts as confirmation";
      "independent of Connector";
      "redaction";
      "PAT authentication is exact-Repo";
    ]

let test_operator_contract () =
  let body = doc "docs/github-route-operator-contract.md" in
  must_contain ~label:"operator contract" ~doc:body
    [
      "Selector specificity";
      "Fail-closed mute";
      "Org requires App";
      "Plan is not apply";
      "callback is not apply";
      "Delivery ACK independent of Connector";
      "Secrets redaction";
      "assess_readiness";
      "explain_match";
      "Upgrade validation and drift checks";
      "Github_route_upgrade_validate";
      "github route diagnostics";
      "github route export";
      "github route validate";
      "CLAWQ_ADMIN=1";
      "github-route-runtime-contract";
      "unavailable";
      "--envelope-json";
      "Room-effective";
      "Github_route_match.specificity_rank";
      "Deprecated aliases";
    ]

let test_glossary_github_routes () =
  let body = doc "docs/glossary-github-routes.md" in
  must_contain ~label:"glossary" ~doc:body
    [
      "Item > Repo > Org";
      "No-fallthrough";
      "Fail-closed mute";
      "can_claim_org_scope";
      "Plan-confirm-apply";
      "callback is not apply confirmation";
      "Delivery ACK independent of Connector";
      "Redaction";
      "exact-Repo";
    ]

let test_plan_cross_links () =
  let body = doc "docs/plans/2026-07-12-github-item-room-routing.md" in
  must_contain ~label:"routing plan" ~doc:body
    [
      "adr/0008-github-route-model-and-setup.md";
      "glossary-github-routes.md";
      "github-route-operator-contract.md";
    ]

let test_advanced_filter_operator_surface () =
  let operator = doc "docs/github-route-operator-contract.md" in
  let glossary = doc "docs/glossary-github-routes.md" in
  let adr = doc "docs/adr/0008-github-route-model-and-setup.md" in
  let plan = doc "docs/plans/2026-07-12-github-item-room-routing.md" in
  let cli = doc "docs/cli-reference.md" in
  must_contain ~label:"operator advanced filter contract" ~doc:operator
    [
      "--filter-json";
      "route preview";
      "schema_version";
      "mutually exclusive";
      "item's";
    ];
  must_contain ~label:"advanced filter glossary" ~doc:glossary
    [
      "Versioned advanced filter";
      "Item author / webhook actor";
      "Filter preview";
    ];
  must_contain ~label:"advanced filter ADR" ~doc:adr
    [ "Versioned advanced filters"; "route preview"; "head.ref" ];
  must_contain ~label:"P20 current-state plan" ~doc:plan
    [ "Current P20 filter interface"; "--filter-json"; "head.ref" ];
  must_contain ~label:"CLI advanced filter reference" ~doc:cli
    [ "clawq github route preview"; "--filter-json"; "schema_version" ]

let suite =
  [
    ( "ADR 0008 states route model and setup contract",
      `Quick,
      test_adr_0008_route_model_and_setup );
    ( "operator contract covers setup inspect apply repair",
      `Quick,
      test_operator_contract );
    ( "glossary defines route and setup terms",
      `Quick,
      test_glossary_github_routes );
    ( "plan cross-links ADR glossary operator contract",
      `Quick,
      test_plan_cross_links );
    ( "advanced filters are documented across operator surfaces",
      `Quick,
      test_advanced_filter_operator_surface );
  ]
