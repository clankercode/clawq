(* Tests for GitHub App JWT generation and installation token cache.

   All tests use mocked HTTP responses -- no real GitHub API calls. *)

(* ---- Test helpers ---- *)

(* Generate a test RSA key at runtime. *)
let generate_test_key_file () =
  let tmp = Filename.temp_file "test-key" ".pem" in
  let rc =
    Sys.command
      (Printf.sprintf
         "openssl genrsa 2048 2>/dev/null | openssl pkcs8 -topk8 -nocrypt > %s \
          2>/dev/null"
         tmp)
  in
  if rc <> 0 then (
    Sys.remove tmp;
    failwith "Failed to generate test RSA key");
  tmp

let with_test_rsa_key f =
  let key_path = generate_test_key_file () in
  Fun.protect ~finally:(fun () -> Sys.remove key_path) (fun () -> f key_path)

(* ---- JWT generation tests ---- *)

let jwt_format () =
  with_test_rsa_key (fun key_path ->
      match
        Github_app_token.create
          ~config:
            ({
               app_id = 12345;
               private_key_path = key_path;
               webhook_secret = "test";
               installations =
                 [ { Runtime_config.installation_id = 67890; repos = [] } ];
             }
              : Runtime_config.github_app_config)
          ()
      with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          let jwt = Github_app_token.generate_jwt ~key:tok.key ~app_id:12345 in
          (* JWT has 3 dot-separated parts *)
          let parts = String.split_on_char '.' jwt in
          Alcotest.(check int) "jwt parts" 3 (List.length parts);
          (* Header should decode to RS256 *)
          let header_b64 = List.nth parts 0 in
          let header_json =
            Base64.decode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet
              header_b64
          in
          let header = Yojson.Safe.from_string header_json in
          let alg =
            Yojson.Safe.Util.member "alg" header |> Yojson.Safe.Util.to_string
          in
          Alcotest.(check string) "alg" "RS256" alg)

let jwt_claims_contain_app_id () =
  with_test_rsa_key (fun key_path ->
      match
        Github_app_token.create
          ~config:
            ({
               app_id = 99;
               private_key_path = key_path;
               webhook_secret = "test";
               installations =
                 [ { Runtime_config.installation_id = 1; repos = [] } ];
             }
              : Runtime_config.github_app_config)
          ()
      with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          let jwt = Github_app_token.generate_jwt ~key:tok.key ~app_id:99 in
          let parts = String.split_on_char '.' jwt in
          let payload_b64 = List.nth parts 1 in
          let payload_json =
            Base64.decode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet
              payload_b64
          in
          let payload = Yojson.Safe.from_string payload_json in
          let iss =
            Yojson.Safe.Util.member "iss" payload |> Yojson.Safe.Util.to_int
          in
          Alcotest.(check int) "iss" 99 iss)

let jwt_signature_valid () =
  with_test_rsa_key (fun key_path ->
      match
        Github_app_token.create
          ~config:
            ({
               app_id = 42;
               private_key_path = key_path;
               webhook_secret = "test";
               installations =
                 [ { Runtime_config.installation_id = 1; repos = [] } ];
             }
              : Runtime_config.github_app_config)
          ()
      with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          let jwt = Github_app_token.generate_jwt ~key:tok.key ~app_id:42 in
          let parts = String.split_on_char '.' jwt in
          let signing_input = List.nth parts 0 ^ "." ^ List.nth parts 1 in
          let signature_b64 = List.nth parts 2 in
          let signature =
            Base64.decode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet
              signature_b64
          in
          (* Verify with the public key *)
          let pub = Mirage_crypto_pk.Rsa.pub_of_priv tok.key in
          let valid =
            Mirage_crypto_pk.Rsa.PKCS1.verify
              ~hashp:(fun h -> h = `SHA256)
              ~key:pub ~signature (`Message signing_input)
          in
          Alcotest.(check bool) "signature valid" true valid)

(* ---- Private key loading tests ---- *)

let create_with_valid_key () =
  with_test_rsa_key (fun key_path ->
      match
        Github_app_token.create
          ~config:
            ({
               app_id = 1;
               private_key_path = key_path;
               webhook_secret = "test";
               installations =
                 [ { Runtime_config.installation_id = 1; repos = [ "a/b" ] } ];
             }
              : Runtime_config.github_app_config)
          ()
      with
      | Error msg -> Alcotest.failf "expected Ok, got Error: %s" msg
      | Ok _ -> ())

let create_with_missing_key () =
  match
    Github_app_token.create
      ~config:
        ({
           app_id = 1;
           private_key_path = "/nonexistent/key.pem";
           webhook_secret = "test";
           installations =
             [ { Runtime_config.installation_id = 1; repos = [] } ];
         }
          : Runtime_config.github_app_config)
      ()
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for missing key file"

(* ---- Installation lookup tests ---- *)

let find_installation_all_repos () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [ { Runtime_config.installation_id = 100; repos = [] } ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok -> (
          match
            Github_app_token.find_installation_for_repo tok
              ~repo_full_name:"any/repo"
          with
          | Some inst ->
              Alcotest.(check int) "installation_id" 100 inst.installation_id
          | None -> Alcotest.fail "expected Some"))

let find_installation_specific_repo () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [
              {
                Runtime_config.installation_id = 200;
                repos = [ "acme/backend"; "acme/frontend" ];
              };
              { Runtime_config.installation_id = 300; repos = [ "other/repo" ] };
            ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok -> (
          match
            Github_app_token.find_installation_for_repo tok
              ~repo_full_name:"acme/backend"
          with
          | Some inst ->
              Alcotest.(check int) "installation_id" 200 inst.installation_id
          | None -> Alcotest.fail "expected Some for acme/backend"))

let find_installation_no_match () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [
              {
                Runtime_config.installation_id = 200;
                repos = [ "acme/backend" ];
              };
            ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok -> (
          match
            Github_app_token.find_installation_for_repo tok
              ~repo_full_name:"other/repo"
          with
          | None -> ()
          | Some _ -> Alcotest.fail "expected None for non-matching repo"))

(* ---- Token cache tests ---- *)

let cache_hit_returns_same_token () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations = [ { Runtime_config.installation_id = 1; repos = [] } ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok -> (
          (* Manually store a token in the cache *)
          Github_app_token.store_cache tok.cache ~installation_id:1 ~repos:[]
            ~token:"ghs_cached_12345"
            ~expires_at:(Unix.gettimeofday () +. 3600.0);
          let result =
            Lwt_main.run
              (Github_app_token.get_installation_token tok ~installation_id:1
                 ~repos:[] ())
          in
          match result with
          | Ok token ->
              Alcotest.(check string) "cached token" "ghs_cached_12345" token
          | Error msg -> Alcotest.failf "expected Ok, got Error: %s" msg))

let cache_expired_refreshes () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations = [ { Runtime_config.installation_id = 1; repos = [] } ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          (* Store an expired token *)
          Github_app_token.store_cache tok.cache ~installation_id:1 ~repos:[]
            ~token:"ghs_expired"
            ~expires_at:(Unix.gettimeofday () -. 10.0);
          (* The cache lookup should return None for expired tokens *)
          let cached =
            Github_app_token.lookup_cache tok.cache ~installation_id:1 ~repos:[]
          in
          Alcotest.(check (option string)) "expired cache" None cached)

let cache_scoped_to_repos () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations = [ { Runtime_config.installation_id = 1; repos = [] } ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          (* Cache for specific repos *)
          Github_app_token.store_cache tok.cache ~installation_id:1
            ~repos:[ "a/b"; "c/d" ] ~token:"ghs_scoped"
            ~expires_at:(Unix.gettimeofday () +. 3600.0);
          (* Same repos (order-independent) should hit *)
          let cached_same =
            Github_app_token.lookup_cache tok.cache ~installation_id:1
              ~repos:[ "c/d"; "a/b" ]
          in
          Alcotest.(check (option string))
            "same repos different order" (Some "ghs_scoped") cached_same;
          (* Different repos should miss *)
          let cached_diff =
            Github_app_token.lookup_cache tok.cache ~installation_id:1
              ~repos:[ "x/y" ]
          in
          Alcotest.(check (option string)) "different repos" None cached_diff)

let invalidate_cache_clears_all () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations = [ { Runtime_config.installation_id = 1; repos = [] } ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          Github_app_token.store_cache tok.cache ~installation_id:1 ~repos:[]
            ~token:"ghs_will_be_cleared"
            ~expires_at:(Unix.gettimeofday () +. 3600.0);
          Github_app_token.invalidate_cache tok;
          let cached =
            Github_app_token.lookup_cache tok.cache ~installation_id:1 ~repos:[]
          in
          Alcotest.(check (option string)) "after invalidate" None cached)

(* ---- Mocked installation token fetch test ---- *)

let fetch_installation_token_mock () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 42;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [ { Runtime_config.installation_id = 999; repos = [ "acme/app" ] } ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          (* Start a mock HTTP server that returns a fake installation token *)
          let mock_token = "ghs_mocked_installation_token_abc123" in
          let mock_expires = "2099-12-31T23:59:59Z" in
          let response_body =
            Printf.sprintf
              {|{"token":"%s","expires_at":"%s","permissions":{"contents":"read"},"repository_selection":"selected"}|}
              mock_token mock_expires
          in
          let previous_api_base = Sys.getenv_opt "CLAWQ_GITHUB_API_BASE" in
          Lwt_main.run
            (let open Lwt.Syntax in
             let callback _conn req req_body =
               let path = Cohttp.Request.resource req in
               let meth = Cohttp.Request.meth req in
               if meth = `POST && path = "/app/installations/999/access_tokens"
               then (
                 (* Verify the Authorization header uses a JWT (Bearer token) *)
                 let auth_header =
                   Cohttp.Request.headers req |> Cohttp.Header.get_authorization
                 in
                 let is_jwt =
                   match auth_header with
                   | Some (`Other s) ->
                       String.length s > 7
                       && String.sub s 0 7 = "Bearer "
                       && String.contains s '.'
                   | _ -> false
                 in
                 if not is_jwt then
                   Alcotest.fail "Expected JWT Bearer token in Authorization";
                 let* _body_str = Cohttp_lwt.Body.to_string req_body in
                 Cohttp_lwt_unix.Server.respond_string ~status:`OK
                   ~body:response_body ())
               else
                 Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
                   ~body:"not found" ()
             in
             let port = 19879 in
             let server =
               Cohttp_lwt_unix.Server.create
                 ~mode:(`TCP (`Port port))
                 (Cohttp_lwt_unix.Server.make ~callback ())
             in
             Lwt.async (fun () -> server);
             Unix.putenv "CLAWQ_GITHUB_API_BASE"
               (Printf.sprintf "http://127.0.0.1:%d" port);
             let* () = Lwt_unix.sleep 0.05 in
             let* result =
               Github_app_token.get_installation_token tok ~installation_id:999
                 ~repos:[ "acme/app" ] ()
             in
             let* () = Lwt_unix.sleep 0.1 in
             (match previous_api_base with
             | Some v -> Unix.putenv "CLAWQ_GITHUB_API_BASE" v
             | None -> Unix.putenv "CLAWQ_GITHUB_API_BASE" "");
             match result with
             | Ok token ->
                 Alcotest.(check string) "mocked token" mock_token token;
                 (* Verify it is now cached *)
                 let cached =
                   Github_app_token.lookup_cache tok.cache ~installation_id:999
                     ~repos:[ "acme/app" ]
                 in
                 Alcotest.(check (option string))
                   "cached after fetch" (Some mock_token) cached;
                 Lwt.return_unit
             | Error msg -> Alcotest.failf "expected Ok, got Error: %s" msg))

let fetch_installation_token_api_error () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 42;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [ { Runtime_config.installation_id = 888; repos = [] } ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          let previous_api_base = Sys.getenv_opt "CLAWQ_GITHUB_API_BASE" in
          Lwt_main.run
            (let open Lwt.Syntax in
             let callback _conn _req _req_body =
               Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
                 ~body:{|{"message":"Bad credentials"}|} ()
             in
             let port = 19880 in
             let server =
               Cohttp_lwt_unix.Server.create
                 ~mode:(`TCP (`Port port))
                 (Cohttp_lwt_unix.Server.make ~callback ())
             in
             Lwt.async (fun () -> server);
             Unix.putenv "CLAWQ_GITHUB_API_BASE"
               (Printf.sprintf "http://127.0.0.1:%d" port);
             let* () = Lwt_unix.sleep 0.05 in
             let* result =
               Github_app_token.get_installation_token tok ~installation_id:888
                 ~repos:[] ()
             in
             let* () = Lwt_unix.sleep 0.1 in
             (match previous_api_base with
             | Some v -> Unix.putenv "CLAWQ_GITHUB_API_BASE" v
             | None -> Unix.putenv "CLAWQ_GITHUB_API_BASE" "");
             match result with
             | Error _ -> Lwt.return_unit
             | Ok _ -> Alcotest.fail "expected Error for 401 response"))

(* ---- Redaction tests ---- *)

let token_redacted_in_logs () =
  let token = "ghs_abcdefghij1234567890" in
  let redacted = String_util.redact_token token in
  Alcotest.(check bool)
    "redacted does not contain full token" false (redacted = token);
  Alcotest.(check bool)
    "redacted preserves prefix" true
    (String.length redacted >= 4);
  Alcotest.(check bool)
    "redacted is shorter than full token" true
    (String.length redacted < String.length token)

(* ---- ISO 8601 parsing tests ---- *)

let iso8601_epoch () =
  let ts = Github_app_token.parse_iso8601_utc "1970-01-01T00:00:00Z" in
  Alcotest.(check (float 0.001)) "epoch" 0.0 ts

let iso8601_known_date () =
  (* 2024-01-01T00:00:00Z = 1704067200 *)
  let ts = Github_app_token.parse_iso8601_utc "2024-01-01T00:00:00Z" in
  Alcotest.(check (float 0.001)) "2024-01-01" 1704067200.0 ts

let iso8601_with_millis () =
  let ts = Github_app_token.parse_iso8601_utc "2024-06-15T12:30:45.123Z" in
  Alcotest.(check (float 0.001)) "with millis" 1718454645.0 ts

let iso8601_leap_year () =
  (* 2024-02-29T00:00:00Z = 1709164800 *)
  let ts = Github_app_token.parse_iso8601_utc "2024-02-29T00:00:00Z" in
  Alcotest.(check (float 0.001)) "leap day" 1709164800.0 ts

(* ---- Integration test: Github_api with GithubApp auth ---- *)

let github_api_with_app_auth_posts_comment () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 77;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [ { Runtime_config.installation_id = 555; repos = [ "acme/app" ] } ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          let mock_install_token = "ghs_integration_test_token_xyz" in
          let mock_expires = "2099-12-31T23:59:59Z" in
          let token_response =
            Printf.sprintf {|{"token":"%s","expires_at":"%s"}|}
              mock_install_token mock_expires
          in
          (* Track what Authorization header was used for the comment POST *)
          let used_auth = ref "" in
          let previous_api_base = Sys.getenv_opt "CLAWQ_GITHUB_API_BASE" in
          Lwt_main.run
            (let open Lwt.Syntax in
             let callback _conn req req_body =
               let path = Cohttp.Request.resource req in
               let meth = Cohttp.Request.meth req in
               let auth =
                 Cohttp.Request.headers req |> Cohttp.Header.get_authorization
               in
               (match auth with Some (`Other s) -> used_auth := s | _ -> ());
               if meth = `POST && path = "/app/installations/555/access_tokens"
               then
                 Cohttp_lwt_unix.Server.respond_string ~status:`OK
                   ~body:token_response ()
               else if
                 meth = `POST && path = "/repos/acme/app/issues/42/comments"
               then (
                 let* _body_str = Cohttp_lwt.Body.to_string req_body in
                 (* Verify the comment was posted with the installation token *)
                 Alcotest.(check bool)
                   "used installation token" true
                   (String.sub !used_auth 0 (min 7 (String.length !used_auth))
                    = "Bearer "
                   && !used_auth = "Bearer " ^ mock_install_token);
                 Cohttp_lwt_unix.Server.respond_string ~status:`OK
                   ~body:{|{"id":999}|} ())
               else
                 Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
                   ~body:"not found" ()
             in
             let port = 19881 in
             let server =
               Cohttp_lwt_unix.Server.create
                 ~mode:(`TCP (`Port port))
                 (Cohttp_lwt_unix.Server.make ~callback ())
             in
             Lwt.async (fun () -> server);
             Unix.putenv "CLAWQ_GITHUB_API_BASE"
               (Printf.sprintf "http://127.0.0.1:%d" port);
             let* () = Lwt_unix.sleep 0.05 in
             (* Call Github_api.post_comment with app_token *)
             let* () =
               Github_api.post_comment ~app_token:(Some tok)
                 ~auth:(GithubApp config) ~owner:"acme" ~repo:"app"
                 ~issue_number:42 ~body:"test comment" ()
             in
             let* () = Lwt_unix.sleep 0.1 in
             (match previous_api_base with
             | Some v -> Unix.putenv "CLAWQ_GITHUB_API_BASE" v
             | None -> Unix.putenv "CLAWQ_GITHUB_API_BASE" "");
             Lwt.return_unit);
          (* Verify the installation token was cached *)
          let cached =
            Github_app_token.lookup_cache tok.cache ~installation_id:555
              ~repos:[ "acme/app" ]
          in
          Alcotest.(check (option string))
            "token cached after API call" (Some mock_install_token) cached)

(* ---- Test suite ---- *)

let jwt_suite =
  [
    Alcotest.test_case "JWT has correct format" `Quick jwt_format;
    Alcotest.test_case "JWT claims contain app_id" `Quick
      jwt_claims_contain_app_id;
    Alcotest.test_case "JWT signature is valid" `Quick jwt_signature_valid;
  ]

let key_suite =
  [
    Alcotest.test_case "create with valid key" `Quick create_with_valid_key;
    Alcotest.test_case "create with missing key" `Quick create_with_missing_key;
  ]

let installation_lookup_suite =
  [
    Alcotest.test_case "find all-repos installation" `Quick
      find_installation_all_repos;
    Alcotest.test_case "find specific repo installation" `Quick
      find_installation_specific_repo;
    Alcotest.test_case "no match returns None" `Quick find_installation_no_match;
  ]

let cache_suite =
  [
    Alcotest.test_case "cache hit returns same token" `Quick
      cache_hit_returns_same_token;
    Alcotest.test_case "expired cache returns None" `Quick
      cache_expired_refreshes;
    Alcotest.test_case "cache scoped to repos" `Quick cache_scoped_to_repos;
    Alcotest.test_case "invalidate clears all" `Quick
      invalidate_cache_clears_all;
  ]

let fetch_suite =
  [
    Alcotest.test_case "fetch with mocked HTTP" `Quick
      fetch_installation_token_mock;
    Alcotest.test_case "fetch handles API error" `Quick
      fetch_installation_token_api_error;
  ]

let redaction_suite =
  [ Alcotest.test_case "token redacted in logs" `Quick token_redacted_in_logs ]

let iso8601_suite =
  [
    Alcotest.test_case "epoch" `Quick iso8601_epoch;
    Alcotest.test_case "known date" `Quick iso8601_known_date;
    Alcotest.test_case "with milliseconds" `Quick iso8601_with_millis;
    Alcotest.test_case "leap year" `Quick iso8601_leap_year;
  ]

(* ---- Module-level init/resolve/invalidate tests ---- *)

let init_from_config_pat_sets_none () =
  let config : Runtime_config.github_config =
    {
      auth = Runtime_config.GithubPat "ghp_test";
      repos = [];
      default_model = None;
      trigger_login = None;
      trigger_label = None;
      auth_credential_handle = None;
    }
  in
  Github_app_token.init_from_config config;
  Alcotest.(check (option string))
    "PAT auth resolves to None" None
    (Option.map
       (fun _tok -> "has_token")
       (Github_app_token.resolve_app_token ()))

let init_from_config_app_sets_token () =
  with_test_rsa_key (fun key_path ->
      let app_config : Runtime_config.github_app_config =
        {
          app_id = 55;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [ { Runtime_config.installation_id = 1; repos = [ "a/b" ] } ];
        }
      in
      let config : Runtime_config.github_config =
        {
          auth = Runtime_config.GithubApp app_config;
          repos = [];
          default_model = None;
          trigger_login = None;
          trigger_label = None;
          auth_credential_handle = None;
        }
      in
      Github_app_token.init_from_config config;
      match Github_app_token.resolve_app_token () with
      | Some tok -> Alcotest.(check int) "app_id" 55 tok.config.app_id
      | None -> Alcotest.fail "expected Some token after init")

let init_from_config_invalid_key_sets_none () =
  let app_config : Runtime_config.github_app_config =
    {
      app_id = 1;
      private_key_path = "/nonexistent/key.pem";
      webhook_secret = "test";
      installations =
        [ { Runtime_config.installation_id = 1; repos = [ "a/b" ] } ];
    }
  in
  let config : Runtime_config.github_config =
    {
      auth = Runtime_config.GithubApp app_config;
      repos = [];
      default_model = None;
      trigger_login = None;
      trigger_label = None;
      auth_credential_handle = None;
    }
  in
  Github_app_token.init_from_config config;
  Alcotest.(check (option string))
    "invalid key resolves to None" None
    (Option.map
       (fun _tok -> "has_token")
       (Github_app_token.resolve_app_token ()))

let invalidate_all_clears_token () =
  with_test_rsa_key (fun key_path ->
      let app_config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [ { Runtime_config.installation_id = 1; repos = [ "a/b" ] } ];
        }
      in
      let config : Runtime_config.github_config =
        {
          auth = Runtime_config.GithubApp app_config;
          repos = [];
          default_model = None;
          trigger_login = None;
          trigger_label = None;
          auth_credential_handle = None;
        }
      in
      Github_app_token.init_from_config config;
      Alcotest.(check bool)
        "token exists before invalidate" true
        (Option.is_some (Github_app_token.resolve_app_token ()));
      Github_app_token.invalidate_all ();
      Alcotest.(check (option string))
        "token gone after invalidate" None
        (Option.map
           (fun _tok -> "has_token")
           (Github_app_token.resolve_app_token ())))

(* ---- verify_installation tests ---- *)

let verify_installation_matching_id_and_repo () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [
              {
                Runtime_config.installation_id = 100;
                repos = [ "acme/backend" ];
              };
            ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          Alcotest.(check bool)
            "authorized" true
            (Github_app_token.verify_installation tok ~installation_id:100
               ~repo_full_name:"acme/backend"))

let verify_installation_wrong_id () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [
              {
                Runtime_config.installation_id = 100;
                repos = [ "acme/backend" ];
              };
            ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          Alcotest.(check bool)
            "wrong id denied" false
            (Github_app_token.verify_installation tok ~installation_id:999
               ~repo_full_name:"acme/backend"))

let verify_installation_wrong_repo () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [
              {
                Runtime_config.installation_id = 100;
                repos = [ "acme/backend" ];
              };
            ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          Alcotest.(check bool)
            "wrong repo denied" false
            (Github_app_token.verify_installation tok ~installation_id:100
               ~repo_full_name:"other/repo"))

let verify_installation_all_repos () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [ { Runtime_config.installation_id = 200; repos = [] } ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          Alcotest.(check bool)
            "all repos authorized" true
            (Github_app_token.verify_installation tok ~installation_id:200
               ~repo_full_name:"any/repo"))

let verify_installation_multiple_installations () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [
              {
                Runtime_config.installation_id = 100;
                repos = [ "acme/backend" ];
              };
              {
                Runtime_config.installation_id = 200;
                repos = [ "acme/frontend" ];
              };
            ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          Alcotest.(check bool)
            "first installation" true
            (Github_app_token.verify_installation tok ~installation_id:100
               ~repo_full_name:"acme/backend");
          Alcotest.(check bool)
            "second installation" true
            (Github_app_token.verify_installation tok ~installation_id:200
               ~repo_full_name:"acme/frontend");
          Alcotest.(check bool)
            "cross installation denied" false
            (Github_app_token.verify_installation tok ~installation_id:100
               ~repo_full_name:"acme/frontend"))

let verify_installation_case_insensitive () =
  with_test_rsa_key (fun key_path ->
      let config : Runtime_config.github_app_config =
        {
          app_id = 1;
          private_key_path = key_path;
          webhook_secret = "test";
          installations =
            [
              {
                Runtime_config.installation_id = 100;
                repos = [ "Acme/Backend" ];
              };
            ];
        }
      in
      match Github_app_token.create ~config () with
      | Error msg -> Alcotest.failf "create failed: %s" msg
      | Ok tok ->
          Alcotest.(check bool)
            "case-insensitive match" true
            (Github_app_token.verify_installation tok ~installation_id:100
               ~repo_full_name:"acme/backend"))

let verify_installation_suite =
  [
    Alcotest.test_case "verify matching id and repo" `Quick
      verify_installation_matching_id_and_repo;
    Alcotest.test_case "verify wrong id denied" `Quick
      verify_installation_wrong_id;
    Alcotest.test_case "verify wrong repo denied" `Quick
      verify_installation_wrong_repo;
    Alcotest.test_case "verify all repos authorized" `Quick
      verify_installation_all_repos;
    Alcotest.test_case "verify multiple installations" `Quick
      verify_installation_multiple_installations;
    Alcotest.test_case "verify case-insensitive repo match" `Quick
      verify_installation_case_insensitive;
  ]

let integration_suite =
  [
    Alcotest.test_case "Github_api uses app installation token" `Quick
      github_api_with_app_auth_posts_comment;
    Alcotest.test_case "init_from_config with PAT sets None" `Quick
      init_from_config_pat_sets_none;
    Alcotest.test_case "init_from_config with App sets token" `Quick
      init_from_config_app_sets_token;
    Alcotest.test_case "init_from_config with invalid key sets None" `Quick
      init_from_config_invalid_key_sets_none;
    Alcotest.test_case "invalidate_all clears token" `Quick
      invalidate_all_clears_token;
  ]

let suite =
  jwt_suite @ key_suite @ installation_lookup_suite @ cache_suite @ fetch_suite
  @ redaction_suite @ iso8601_suite @ verify_installation_suite
  @ integration_suite
