(** Drift checks for P21 Principal/token-vault ADR 0009 and glossary
    (P21.M4.E3.T001). *)

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

let test_adr_0009 () =
  let body = doc "docs/adr/0009-principal-token-vault-security-boundary.md" in
  must_contain ~label:"ADR 0009" ~doc:body
    [
      "Status: Accepted";
      "Verified Connector trust adapters";
      "Immutable Actor snapshots";
      "### 2. Principal adoption, tombstone, and split";
      "### 3. Immutable Actor snapshots vs current authority";
      "Authorization activation";
      "generation";
      "### 6. Key source, version, rotation, backup, compromise";
      "### 7. P19 → P21 rollout states";
      "Whole-store rollback";
      "Token confinement";
      "User_required";
      "User_preferred";
    ]

let test_glossary () =
  let body = doc "docs/glossary-principal-github-attribution.md" in
  must_contain ~label:"glossary" ~doc:body
    [
      "Principal";
      "Identity link";
      "Actor snapshot";
      "Current authority";
      "Token generation";
      "Master key / key id";
      "Opaque lease";
      "Pending activation";
      "Attribution rollout stage";
      "Whole-store rollback limitation";
      "0009-principal-token-vault-security-boundary.md";
    ]

let suite =
  [
    ( "ADR 0009 states shipped Principal token-vault boundary",
      `Quick,
      test_adr_0009 );
    ("glossary defines P21 attribution terms", `Quick, test_glossary);
  ]
