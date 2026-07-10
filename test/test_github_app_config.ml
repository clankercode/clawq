(* Tests for GitHub App config parsing, validation, and roundtrip. *)

let test_parse_github_app_auth () =
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "github": {
            "auth": {
              "type": "app",
              "app_id": 12345,
              "private_key_path": "/path/to/key.pem",
              "webhook_secret": "whsec_abc123",
              "installations": [
                {"installation_id": 67890, "repos": ["acme/backend", "acme/frontend"]}
              ]
            },
            "repos": [{"name": "acme/backend", "webhook_secret": "repo_secret", "webhook_path": "/github"}]
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  match cfg.channels.github with
  | None -> Alcotest.fail "expected github config"
  | Some g -> (
      match g.auth with
      | Runtime_config.GithubPat _ -> Alcotest.fail "expected GithubApp"
      | Runtime_config.GithubApp app ->
          Alcotest.(check int) "app_id" 12345 app.app_id;
          Alcotest.(check string)
            "private_key_path" "/path/to/key.pem" app.private_key_path;
          Alcotest.(check string)
            "webhook_secret" "whsec_abc123" app.webhook_secret;
          Alcotest.(check int)
            "installations count" 1
            (List.length app.installations);
          let inst = List.hd app.installations in
          Alcotest.(check int) "installation_id" 67890 inst.installation_id;
          Alcotest.(check (list string))
            "repos"
            [ "acme/backend"; "acme/frontend" ]
            inst.repos;
          Alcotest.(check int) "repos count" 1 (List.length g.repos))

let test_github_app_roundtrip () =
  let app : Runtime_config.github_app_config =
    {
      app_id = 42;
      private_key_path = "~/.clawq/github-app.pem";
      webhook_secret = "my_webhook_secret";
      installations =
        [
          { installation_id = 100; repos = [ "owner/repo1" ] };
          { installation_id = 200; repos = [ "owner/repo2"; "owner/repo3" ] };
        ];
    }
  in
  let cfg =
    {
      Runtime_config.default with
      channels =
        {
          Runtime_config.default.channels with
          github =
            Some
              {
                auth = Runtime_config.GithubApp app;
                repos = [];
                default_model = None;
                trigger_login = None;
                trigger_label = None;
                auth_credential_handle = None;
              };
        };
    }
  in
  let json = Runtime_config.to_json cfg in
  let cfg2 = Config_loader.parse_config ~resolve_secrets:false json in
  match cfg2.channels.github with
  | None -> Alcotest.fail "expected github config after roundtrip"
  | Some g -> (
      match g.auth with
      | Runtime_config.GithubPat _ ->
          Alcotest.fail "expected GithubApp after roundtrip"
      | Runtime_config.GithubApp app2 ->
          Alcotest.(check int) "app_id roundtrip" 42 app2.app_id;
          Alcotest.(check string)
            "private_key_path roundtrip" "~/.clawq/github-app.pem"
            app2.private_key_path;
          Alcotest.(check string)
            "webhook_secret roundtrip" "my_webhook_secret" app2.webhook_secret;
          Alcotest.(check int)
            "installations count roundtrip" 2
            (List.length app2.installations);
          let inst1 = List.nth app2.installations 0 in
          Alcotest.(check int) "inst1 installation_id" 100 inst1.installation_id;
          Alcotest.(check (list string))
            "inst1 repos" [ "owner/repo1" ] inst1.repos;
          let inst2 = List.nth app2.installations 1 in
          Alcotest.(check int) "inst2 installation_id" 200 inst2.installation_id;
          Alcotest.(check (list string))
            "inst2 repos"
            [ "owner/repo2"; "owner/repo3" ]
            inst2.repos)

let test_github_app_invalid_no_app_id () =
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "github": {
            "auth": {
              "type": "app",
              "private_key_path": "/path/to/key.pem",
              "webhook_secret": "whsec"
            },
            "repos": []
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  match cfg.channels.github with
  | None -> () (* fail-closed: invalid config rejected *)
  | Some g -> (
      match g.auth with
      | Runtime_config.GithubApp _ ->
          Alcotest.fail "expected GithubApp to be rejected when app_id=0"
      | Runtime_config.GithubPat _ -> Alcotest.fail "expected no github config")

let test_github_app_invalid_no_private_key () =
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "github": {
            "auth": {
              "type": "app",
              "app_id": 123,
              "webhook_secret": "whsec"
            },
            "repos": []
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  match cfg.channels.github with
  | None -> () (* fail-closed *)
  | Some g -> (
      match g.auth with
      | Runtime_config.GithubApp _ ->
          Alcotest.fail
            "expected GithubApp to be rejected when private_key_path is empty"
      | Runtime_config.GithubPat _ -> Alcotest.fail "expected no github config")

let test_github_app_unknown_auth_type_rejected () =
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "github": {
            "auth": {"type": "oauth", "token": "x"},
            "repos": []
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  match cfg.channels.github with
  | None -> () (* fail-closed: unknown type rejected *)
  | Some _ -> Alcotest.fail "expected unknown auth type to be rejected"

let test_github_app_valid_credentials () =
  let app : Runtime_config.github_app_config =
    {
      app_id = 42;
      private_key_path = "/path/to/key.pem";
      webhook_secret = "whsec";
      installations = [ { installation_id = 100; repos = [ "owner/repo" ] } ];
    }
  in
  Alcotest.(check bool)
    "valid app config" true
    (Runtime_config.github_app_has_valid_credentials app)

let test_github_app_invalid_credentials () =
  let app_empty_id : Runtime_config.github_app_config =
    {
      app_id = 0;
      private_key_path = "/path";
      webhook_secret = "whsec";
      installations = [ { installation_id = 1; repos = [ "r" ] } ];
    }
  in
  let app_empty_key : Runtime_config.github_app_config =
    {
      app_id = 1;
      private_key_path = "";
      webhook_secret = "whsec";
      installations = [ { installation_id = 1; repos = [ "r" ] } ];
    }
  in
  let app_empty_secret : Runtime_config.github_app_config =
    {
      app_id = 1;
      private_key_path = "/path";
      webhook_secret = "";
      installations = [ { installation_id = 1; repos = [ "r" ] } ];
    }
  in
  let app_no_installations : Runtime_config.github_app_config =
    {
      app_id = 1;
      private_key_path = "/path";
      webhook_secret = "whsec";
      installations = [];
    }
  in
  let app_bad_installation : Runtime_config.github_app_config =
    {
      app_id = 1;
      private_key_path = "/path";
      webhook_secret = "whsec";
      installations = [ { installation_id = 0; repos = [ "r" ] } ];
    }
  in
  Alcotest.(check bool)
    "empty app_id" false
    (Runtime_config.github_app_has_valid_credentials app_empty_id);
  Alcotest.(check bool)
    "empty private_key_path" false
    (Runtime_config.github_app_has_valid_credentials app_empty_key);
  Alcotest.(check bool)
    "empty webhook_secret" false
    (Runtime_config.github_app_has_valid_credentials app_empty_secret);
  Alcotest.(check bool)
    "no installations" false
    (Runtime_config.github_app_has_valid_credentials app_no_installations);
  Alcotest.(check bool)
    "bad installation_id" false
    (Runtime_config.github_app_has_valid_credentials app_bad_installation)

let test_github_pat_still_works () =
  let json =
    Yojson.Safe.from_string
      {|{"channels":{"github":{"auth":{"type":"pat","token":"ghp_test12345"},"repos":[]}}}|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  match cfg.channels.github with
  | None -> Alcotest.fail "expected github config"
  | Some g -> (
      match g.auth with
      | Runtime_config.GithubPat token ->
          Alcotest.(check string) "token" "ghp_test12345" token
      | Runtime_config.GithubApp _ ->
          Alcotest.fail "expected GithubPat, not GithubApp")

let test_github_app_multiple_installations () =
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "github": {
            "auth": {
              "type": "app",
              "app_id": 99,
              "private_key_path": "/key.pem",
              "webhook_secret": "ws",
              "installations": [
                {"installation_id": 1, "repos": ["a/b"]},
                {"installation_id": 2, "repos": ["c/d", "e/f"]},
                {"installation_id": 3, "repos": ["g/h"]}
              ]
            },
            "repos": []
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  match cfg.channels.github with
  | None -> Alcotest.fail "expected github config"
  | Some g -> (
      match g.auth with
      | Runtime_config.GithubPat _ -> Alcotest.fail "expected GithubApp"
      | Runtime_config.GithubApp app ->
          Alcotest.(check int)
            "3 installations" 3
            (List.length app.installations);
          let inst2 = List.nth app.installations 1 in
          Alcotest.(check int) "inst2 id" 2 inst2.installation_id;
          Alcotest.(check (list string))
            "inst2 repos" [ "c/d"; "e/f" ] inst2.repos)

let test_github_app_invalid_no_webhook_secret () =
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "github": {
            "auth": {
              "type": "app",
              "app_id": 123,
              "private_key_path": "/key.pem",
              "installations": [{"installation_id": 1, "repos": ["a/b"]}]
            },
            "repos": []
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  match cfg.channels.github with
  | None -> () (* fail-closed *)
  | Some g -> (
      match g.auth with
      | Runtime_config.GithubApp _ ->
          Alcotest.fail
            "expected GithubApp to be rejected when webhook_secret is missing"
      | Runtime_config.GithubPat _ -> Alcotest.fail "expected no github config")

let test_github_app_invalid_empty_installations () =
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "github": {
            "auth": {
              "type": "app",
              "app_id": 123,
              "private_key_path": "/key.pem",
              "webhook_secret": "ws",
              "installations": []
            },
            "repos": []
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  match cfg.channels.github with
  | None -> () (* fail-closed *)
  | Some g -> (
      match g.auth with
      | Runtime_config.GithubApp _ ->
          Alcotest.fail
            "expected GithubApp to be rejected when installations is empty"
      | Runtime_config.GithubPat _ -> Alcotest.fail "expected no github config")

let test_github_app_invalid_bad_installation_id () =
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "github": {
            "auth": {
              "type": "app",
              "app_id": 123,
              "private_key_path": "/key.pem",
              "webhook_secret": "ws",
              "installations": [{"installation_id": 0, "repos": ["a/b"]}]
            },
            "repos": []
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  match cfg.channels.github with
  | None -> () (* fail-closed *)
  | Some g -> (
      match g.auth with
      | Runtime_config.GithubApp _ ->
          Alcotest.fail
            "expected GithubApp to be rejected when installation_id=0"
      | Runtime_config.GithubPat _ -> Alcotest.fail "expected no github config")

let test_github_app_invalid_empty_repos () =
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "github": {
            "auth": {
              "type": "app",
              "app_id": 123,
              "private_key_path": "/key.pem",
              "webhook_secret": "ws",
              "installations": [{"installation_id": 1, "repos": []}]
            },
            "repos": []
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  match cfg.channels.github with
  | None -> () (* fail-closed *)
  | Some g -> (
      match g.auth with
      | Runtime_config.GithubApp _ ->
          Alcotest.fail
            "expected GithubApp to be rejected when installation repos is empty"
      | Runtime_config.GithubPat _ -> Alcotest.fail "expected no github config")

let suite =
  [
    Alcotest.test_case "parse github app auth" `Quick test_parse_github_app_auth;
    Alcotest.test_case "github app config roundtrip" `Quick
      test_github_app_roundtrip;
    Alcotest.test_case "github app invalid no app_id" `Quick
      test_github_app_invalid_no_app_id;
    Alcotest.test_case "github app invalid no private_key" `Quick
      test_github_app_invalid_no_private_key;
    Alcotest.test_case "github app unknown auth type rejected" `Quick
      test_github_app_unknown_auth_type_rejected;
    Alcotest.test_case "github app valid credentials" `Quick
      test_github_app_valid_credentials;
    Alcotest.test_case "github app invalid credentials" `Quick
      test_github_app_invalid_credentials;
    Alcotest.test_case "github pat still works" `Quick
      test_github_pat_still_works;
    Alcotest.test_case "github app multiple installations" `Quick
      test_github_app_multiple_installations;
    Alcotest.test_case "github app invalid no webhook_secret" `Quick
      test_github_app_invalid_no_webhook_secret;
    Alcotest.test_case "github app invalid empty installations" `Quick
      test_github_app_invalid_empty_installations;
    Alcotest.test_case "github app invalid bad installation_id" `Quick
      test_github_app_invalid_bad_installation_id;
    Alcotest.test_case "github app invalid empty repos" `Quick
      test_github_app_invalid_empty_repos;
  ]
