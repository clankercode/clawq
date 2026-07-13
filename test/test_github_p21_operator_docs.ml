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
    ]

let suite =
  [
    ( "operator contract states safe defaults and failure classes",
      `Quick,
      test_operator_contract );
    ( "implementation inventory crosswalks modules and tests",
      `Quick,
      test_inventory );
  ]
