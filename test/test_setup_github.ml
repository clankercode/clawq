(* test_setup_github.ml — Unit tests for Setup_github pure functions *)

let validate_repo_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "owner/repo")
    (Setup_github.validate_repo_name "owner/repo")

let validate_repo_spaces () =
  Alcotest.(check (result string string))
    "spaces trimmed" (Ok "owner/repo")
    (Setup_github.validate_repo_name "  owner/repo  ")

let validate_repo_no_slash () =
  match Setup_github.validate_repo_name "noslash" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for no slash"

let validate_repo_empty () =
  match Setup_github.validate_repo_name "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty"

let validate_repo_empty_owner () =
  match Setup_github.validate_repo_name "/repo" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty owner"

let validate_repo_empty_repo () =
  match Setup_github.validate_repo_name "owner/" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty repo"

let validate_pat_ghp () =
  Alcotest.(check (result string string))
    "ghp_ prefix" (Ok "ghp_abc123")
    (Setup_github.validate_pat "ghp_abc123")

let validate_pat_github_pat () =
  Alcotest.(check (result string string))
    "github_pat_ prefix" (Ok "github_pat_abc123")
    (Setup_github.validate_pat "github_pat_abc123")

let validate_pat_empty () =
  match Setup_github.validate_pat "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty PAT"

let validate_pat_nonstandard () =
  (* Non-standard prefix accepted with warning *)
  Alcotest.(check (result string string))
    "non-standard" (Ok "custom_token_123")
    (Setup_github.validate_pat "custom_token_123")

let default_path_basic () =
  Alcotest.(check string)
    "basic path" "/github/webhook/repo"
    (Setup_github.default_webhook_path "owner/repo")

let default_path_complex () =
  Alcotest.(check string)
    "complex name" "/github/webhook/my-project"
    (Setup_github.default_webhook_path "org/my-project")

let default_path_no_slash () =
  Alcotest.(check string)
    "no slash fallback" "/github/webhook/default"
    (Setup_github.default_webhook_path "noslash")

let build_json_basic () =
  let json =
    Setup_github.build_github_json ~pat_token:"ghp_test" ~repo_name:"acme/app"
      ~webhook_secret:"secret123" ~webhook_path:"/github/webhook/app"
      ~react_to:[] ~allow_users:[ "*" ] ~include_pr_files:true ~agent_name:None
  in
  (* Parse through config_loader to verify compatibility *)
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.github with
  | Some g -> (
      match g.auth with
      | Runtime_config.GithubPat token ->
          Alcotest.(check string) "token" "ghp_test" token;
          Alcotest.(check int) "repos count" 1 (List.length g.repos);
          let r = List.hd g.repos in
          Alcotest.(check string) "name" "acme/app" r.name;
          Alcotest.(check string) "secret" "secret123" r.webhook_secret;
          Alcotest.(check string) "path" "/github/webhook/app" r.webhook_path;
          Alcotest.(check (list string)) "allow_users" [ "*" ] r.allow_users;
          Alcotest.(check (list string)) "react_to" [] r.react_to;
          Alcotest.(check bool) "include_pr_files" true r.include_pr_files;
          Alcotest.(check (option string)) "agent_name" None r.agent_name)
  | None -> Alcotest.fail "expected github config"

let build_json_custom_react_to () =
  let json =
    Setup_github.build_github_json ~pat_token:"ghp_x" ~repo_name:"o/r"
      ~webhook_secret:"s" ~webhook_path:"/gh"
      ~react_to:[ "pull_request"; "issue_comment" ]
      ~allow_users:[ "*" ] ~include_pr_files:true ~agent_name:None
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.github with
  | Some g ->
      let r = List.hd g.repos in
      Alcotest.(check (list string))
        "react_to"
        [ "pull_request"; "issue_comment" ]
        r.react_to
  | None -> Alcotest.fail "expected github config"

let build_json_specific_users () =
  let json =
    Setup_github.build_github_json ~pat_token:"ghp_x" ~repo_name:"o/r"
      ~webhook_secret:"s" ~webhook_path:"/gh" ~react_to:[]
      ~allow_users:[ "alice"; "bob" ] ~include_pr_files:false ~agent_name:None
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.github with
  | Some g ->
      let r = List.hd g.repos in
      Alcotest.(check (list string))
        "allow_users" [ "alice"; "bob" ] r.allow_users;
      Alcotest.(check bool) "include_pr_files" false r.include_pr_files
  | None -> Alcotest.fail "expected github config"

let build_json_with_agent_name () =
  let json =
    Setup_github.build_github_json ~pat_token:"ghp_x" ~repo_name:"o/r"
      ~webhook_secret:"s" ~webhook_path:"/gh" ~react_to:[] ~allow_users:[ "*" ]
      ~include_pr_files:true ~agent_name:(Some "my-reviewer")
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.github with
  | Some g ->
      let r = List.hd g.repos in
      Alcotest.(check (option string))
        "agent_name" (Some "my-reviewer") r.agent_name
  | None -> Alcotest.fail "expected github config"

let instructions_without_tunnel () =
  let s =
    Setup_github.post_setup_instructions ~repo_name:"acme/app"
      ~webhook_path:"/github/webhook/app" ~webhook_secret:"abc123"
      ~gateway_port:13451 ~tunnel_url:None
  in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has localhost url" true
    (contains "http://localhost:13451/github/webhook/app");
  Alcotest.(check bool) "has secret" true (contains "abc123");
  Alcotest.(check bool) "has tunnel note" true (contains "set up a tunnel");
  Alcotest.(check bool)
    "has direct link" true
    (contains "https://github.com/acme/app/settings/hooks/new")

let instructions_with_tunnel () =
  let s =
    Setup_github.post_setup_instructions ~repo_name:"acme/app"
      ~webhook_path:"/github/webhook/app" ~webhook_secret:"abc123"
      ~gateway_port:13451 ~tunnel_url:(Some "https://my.tunnel.example.com")
  in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has tunnel url" true
    (contains "https://my.tunnel.example.com/github/webhook/app");
  Alcotest.(check bool) "no tunnel note" false (contains "set up a tunnel")

let deep_merge_empty () =
  let overlay =
    Setup_github.build_github_json ~pat_token:"ghp_x" ~repo_name:"o/r"
      ~webhook_secret:"s" ~webhook_path:"/gh" ~react_to:[] ~allow_users:[ "*" ]
      ~include_pr_files:true ~agent_name:None
  in
  let result = Setup_common.deep_merge_json (`Assoc []) overlay in
  let config = Config_loader.parse_config ~resolve_secrets:false result in
  match config.channels.github with
  | Some g -> Alcotest.(check int) "repos" 1 (List.length g.repos)
  | None -> Alcotest.fail "expected github config after merge into empty"

let deep_merge_existing_channels () =
  let existing =
    Yojson.Safe.from_string
      {|{"channels":{"cli":true,"telegram":{"accounts":{"default":{"bot_token":"tok"}}}},"default_temperature":0.7}|}
  in
  let overlay =
    Setup_github.build_github_json ~pat_token:"ghp_x" ~repo_name:"o/r"
      ~webhook_secret:"s" ~webhook_path:"/gh" ~react_to:[] ~allow_users:[ "*" ]
      ~include_pr_files:true ~agent_name:None
  in
  let result = Setup_common.deep_merge_json existing overlay in
  let config = Config_loader.parse_config ~resolve_secrets:false result in
  (* GitHub should be present *)
  (match config.channels.github with
  | Some _ -> ()
  | None -> Alcotest.fail "expected github config after merge");
  (* Telegram should be preserved *)
  match config.channels.telegram with
  | Some _ -> ()
  | None -> Alcotest.fail "telegram should be preserved after merge"

let build_full_multi_repo () =
  let repos : Runtime_config.github_repo_config list =
    [
      {
        name = "acme/app";
        webhook_secret = "s1";
        webhook_path = "/gh/app";
        agent_name = None;
        allow_users = [ "*" ];
        react_to = [];
        include_pr_files = true;
      };
      {
        name = "acme/lib";
        webhook_secret = "s2";
        webhook_path = "/gh/lib";
        agent_name = Some "reviewer";
        allow_users = [ "alice" ];
        react_to = [ "pull_request" ];
        include_pr_files = false;
      };
    ]
  in
  let json = Setup_github.build_full_github_json ~pat_token:"ghp_x" ~repos in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.github with
  | Some g ->
      Alcotest.(check int) "repos count" 2 (List.length g.repos);
      let r1 = List.nth g.repos 0 in
      let r2 = List.nth g.repos 1 in
      Alcotest.(check string) "r1 name" "acme/app" r1.name;
      Alcotest.(check string) "r2 name" "acme/lib" r2.name;
      Alcotest.(check (option string))
        "r2 agent" (Some "reviewer") r2.agent_name;
      Alcotest.(check (list string))
        "r2 react_to" [ "pull_request" ] r2.react_to
  | None -> Alcotest.fail "expected github config"

let instructions_has_settings_link () =
  let s =
    Setup_github.post_setup_instructions ~repo_name:"org/my-repo"
      ~webhook_path:"/gh" ~webhook_secret:"s" ~gateway_port:8080
      ~tunnel_url:None
  in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "settings link" true
    (contains "https://github.com/org/my-repo/settings/hooks/new")

let suite =
  [
    Alcotest.test_case "validate_repo valid" `Quick validate_repo_valid;
    Alcotest.test_case "validate_repo spaces" `Quick validate_repo_spaces;
    Alcotest.test_case "validate_repo no slash" `Quick validate_repo_no_slash;
    Alcotest.test_case "validate_repo empty" `Quick validate_repo_empty;
    Alcotest.test_case "validate_repo empty owner" `Quick
      validate_repo_empty_owner;
    Alcotest.test_case "validate_repo empty repo" `Quick
      validate_repo_empty_repo;
    Alcotest.test_case "validate_pat ghp_" `Quick validate_pat_ghp;
    Alcotest.test_case "validate_pat github_pat_" `Quick validate_pat_github_pat;
    Alcotest.test_case "validate_pat empty" `Quick validate_pat_empty;
    Alcotest.test_case "validate_pat non-standard" `Quick
      validate_pat_nonstandard;
    Alcotest.test_case "default_webhook_path basic" `Quick default_path_basic;
    Alcotest.test_case "default_webhook_path complex" `Quick
      default_path_complex;
    Alcotest.test_case "default_webhook_path no slash" `Quick
      default_path_no_slash;
    Alcotest.test_case "build_json basic roundtrip" `Quick build_json_basic;
    Alcotest.test_case "build_json custom react_to" `Quick
      build_json_custom_react_to;
    Alcotest.test_case "build_json specific users" `Quick
      build_json_specific_users;
    Alcotest.test_case "build_json with agent_name" `Quick
      build_json_with_agent_name;
    Alcotest.test_case "instructions without tunnel" `Quick
      instructions_without_tunnel;
    Alcotest.test_case "instructions with tunnel" `Quick
      instructions_with_tunnel;
    Alcotest.test_case "deep merge into empty" `Quick deep_merge_empty;
    Alcotest.test_case "deep merge preserves existing" `Quick
      deep_merge_existing_channels;
    Alcotest.test_case "build_full multi repo roundtrip" `Quick
      build_full_multi_repo;
    Alcotest.test_case "instructions has settings link" `Quick
      instructions_has_settings_link;
  ]
