(* Tests for Repo_manager: sanitize_repo_name, is_url, DB CRUD *)

let mk_db () = Memory.init ~db_path:":memory:" ()

let test_sanitize_repo_name_https () =
  let result =
    Repo_manager.sanitize_repo_name "https://github.com/user/my-repo.git"
  in
  Alcotest.(check bool)
    "starts with my-repo" true
    (String.length result > 8 && String.sub result 0 7 = "my-repo");
  Alcotest.(check bool)
    "has underscore separator" true
    (String.contains result '_')

let test_sanitize_repo_name_ssh () =
  let result =
    Repo_manager.sanitize_repo_name "git@github.com:user/cool-project.git"
  in
  Alcotest.(check bool)
    "starts with cool-project" true
    (String.length result > 12 && String.sub result 0 12 = "cool-project")

let test_sanitize_repo_name_bare () =
  let result = Repo_manager.sanitize_repo_name "https://example.com/repo" in
  Alcotest.(check bool)
    "starts with repo" true
    (String.length result > 4 && String.sub result 0 4 = "repo")

let test_sanitize_different_urls_different_hashes () =
  let a =
    Repo_manager.sanitize_repo_name "https://github.com/user/repo-a.git"
  in
  let b =
    Repo_manager.sanitize_repo_name "https://github.com/user/repo-b.git"
  in
  Alcotest.(check bool) "different hashes" true (a <> b)

let test_sanitize_same_url_same_hash () =
  let a = Repo_manager.sanitize_repo_name "https://github.com/user/repo.git" in
  let b = Repo_manager.sanitize_repo_name "https://github.com/user/repo.git" in
  Alcotest.(check string) "same result" a b

let test_is_url () =
  Alcotest.(check bool)
    "https" true
    (Repo_manager.is_url "https://github.com/user/repo.git");
  Alcotest.(check bool)
    "http" true
    (Repo_manager.is_url "http://example.com/repo");
  Alcotest.(check bool)
    "git@" true
    (Repo_manager.is_url "git@github.com:user/repo.git");
  Alcotest.(check bool)
    "ssh://" true
    (Repo_manager.is_url "ssh://git@github.com/repo");
  Alcotest.(check bool)
    "local path" false
    (Repo_manager.is_url "/home/user/project");
  Alcotest.(check bool) "relative path" false (Repo_manager.is_url "my-project");
  Alcotest.(check bool) "empty" false (Repo_manager.is_url "");
  Alcotest.(check bool) "whitespace" false (Repo_manager.is_url "  ")

let test_db_set_get_repo () =
  let db = mk_db () in
  Repo_manager.set_repo ~db ~session_key:"test:1"
    ~repo_url:"https://github.com/user/repo.git"
    ~local_path:"/tmp/repos/repo_12345678" ~is_managed:true ();
  match Repo_manager.get_repo ~db ~session_key:"test:1" with
  | None -> Alcotest.fail "expected repo info"
  | Some info ->
      Alcotest.(check string) "session_key" "test:1" info.session_key;
      Alcotest.(check (option string))
        "repo_url" (Some "https://github.com/user/repo.git") info.repo_url;
      Alcotest.(check string)
        "local_path" "/tmp/repos/repo_12345678" info.local_path;
      Alcotest.(check bool) "is_managed" true info.is_managed

let test_db_get_repo_not_found () =
  let db = mk_db () in
  match Repo_manager.get_repo ~db ~session_key:"nonexistent" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for nonexistent session"

let test_db_forget_repo () =
  let db = mk_db () in
  Repo_manager.set_repo ~db ~session_key:"test:2" ~local_path:"/tmp/repos/r"
    ~is_managed:false ();
  Repo_manager.forget_repo ~db ~session_key:"test:2";
  match Repo_manager.get_repo ~db ~session_key:"test:2" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None after forget"

let test_db_set_repo_upsert () =
  let db = mk_db () in
  Repo_manager.set_repo ~db ~session_key:"test:3" ~local_path:"/old/path"
    ~is_managed:false ();
  Repo_manager.set_repo ~db ~session_key:"test:3"
    ~repo_url:"https://example.com/repo.git" ~local_path:"/new/path"
    ~is_managed:true ();
  match Repo_manager.get_repo ~db ~session_key:"test:3" with
  | None -> Alcotest.fail "expected repo info after upsert"
  | Some info ->
      Alcotest.(check string) "updated local_path" "/new/path" info.local_path;
      Alcotest.(check bool) "updated is_managed" true info.is_managed

let test_db_list_managed_repos () =
  let db = mk_db () in
  Repo_manager.set_repo ~db ~session_key:"test:m1" ~local_path:"/a"
    ~is_managed:true ();
  Repo_manager.set_repo ~db ~session_key:"test:m2" ~local_path:"/b"
    ~is_managed:true ();
  Repo_manager.set_repo ~db ~session_key:"test:l1" ~local_path:"/c"
    ~is_managed:false ();
  let managed = Repo_manager.list_managed_repos ~db in
  Alcotest.(check int) "count managed" 2 (List.length managed)

let test_db_update_fetch_status () =
  let db = mk_db () in
  Repo_manager.set_repo ~db ~session_key:"test:f1" ~local_path:"/r"
    ~is_managed:true ();
  Repo_manager.update_fetch_status ~db ~session_key:"test:f1" ();
  (match Repo_manager.get_repo ~db ~session_key:"test:f1" with
  | None -> Alcotest.fail "expected repo info"
  | Some info ->
      Alcotest.(check bool)
        "last_fetched_at set" true
        (info.last_fetched_at <> None);
      Alcotest.(check (option string)) "no error" None info.last_fetch_error);
  Repo_manager.update_fetch_status ~db ~session_key:"test:f1"
    ~error:"network error" ();
  match Repo_manager.get_repo ~db ~session_key:"test:f1" with
  | None -> Alcotest.fail "expected repo info"
  | Some info ->
      Alcotest.(check (option string))
        "error recorded" (Some "network error") info.last_fetch_error

let test_local_path_not_url () =
  Repo_manager.set_repo ~db:(mk_db ()) ~session_key:"test:lp"
    ~local_path:"/some/local/path" ~is_managed:false ();
  ()

let suite =
  [
    Alcotest.test_case "sanitize_repo_name https" `Quick
      test_sanitize_repo_name_https;
    Alcotest.test_case "sanitize_repo_name ssh" `Quick
      test_sanitize_repo_name_ssh;
    Alcotest.test_case "sanitize_repo_name bare" `Quick
      test_sanitize_repo_name_bare;
    Alcotest.test_case "different urls different hashes" `Quick
      test_sanitize_different_urls_different_hashes;
    Alcotest.test_case "same url same hash" `Quick
      test_sanitize_same_url_same_hash;
    Alcotest.test_case "is_url" `Quick test_is_url;
    Alcotest.test_case "db set/get repo" `Quick test_db_set_get_repo;
    Alcotest.test_case "db get nonexistent" `Quick test_db_get_repo_not_found;
    Alcotest.test_case "db forget repo" `Quick test_db_forget_repo;
    Alcotest.test_case "db upsert repo" `Quick test_db_set_repo_upsert;
    Alcotest.test_case "db list managed repos" `Quick test_db_list_managed_repos;
    Alcotest.test_case "db update fetch status" `Quick
      test_db_update_fetch_status;
    Alcotest.test_case "local path not url" `Quick test_local_path_not_url;
  ]
