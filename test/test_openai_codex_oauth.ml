let with_temp_home ?master_key f =
  let base = Filename.get_temp_dir_name () in
  let dir =
    Filename.concat base ("clawq_home_" ^ string_of_int (Random.bits ()))
  in
  Unix.mkdir dir 0o755;
  let old_home = Sys.getenv_opt "HOME" in
  let old_master = Sys.getenv_opt "CLAWQ_MASTER_KEY" in
  Unix.putenv "HOME" dir;
  (match master_key with
  | Some v -> Unix.putenv "CLAWQ_MASTER_KEY" v
  | None -> Unix.putenv "CLAWQ_MASTER_KEY" "");
  Fun.protect
    (fun () -> f dir)
    ~finally:(fun () ->
      (match old_home with
      | Some v -> Unix.putenv "HOME" v
      | None -> Unix.putenv "HOME" "");
      (match old_master with
      | Some v -> Unix.putenv "CLAWQ_MASTER_KEY" v
      | None -> Unix.putenv "CLAWQ_MASTER_KEY" "");
      (try
         Unix.unlink
           (Filename.concat (Filename.concat dir ".clawq") "config.json")
       with _ -> ());
      (try Unix.rmdir (Filename.concat dir ".clawq") with _ -> ());
      try Unix.rmdir dir with _ -> ())

let test_parse_callback_input_url () =
  match
    Openai_codex_oauth.parse_callback_input ~expected_state:"abc"
      "http://localhost:1455/auth/callback?code=hello&state=abc"
  with
  | Ok code -> Alcotest.(check string) "code parsed" "hello" code
  | Error msg -> Alcotest.fail msg

let test_parse_callback_input_state_mismatch () =
  match
    Openai_codex_oauth.parse_callback_input ~expected_state:"abc"
      "http://localhost:1455/auth/callback?code=hello&state=def"
  with
  | Ok _ -> Alcotest.fail "expected state mismatch"
  | Error _ -> ()

let test_extract_account_id_from_access_token () =
  let header = Base64.encode_exn {|{"alg":"none"}|} in
  let payload =
    Base64.encode_exn
      {|{"https://api.openai.com/auth":{"chatgpt_account_id":"acct_123"}}|}
  in
  let token =
    String.concat "."
      [
        String.map (function '+' -> '-' | '/' -> '_' | c -> c) header;
        String.map (function '+' -> '-' | '/' -> '_' | c -> c) payload;
        "sig";
      ]
  in
  Alcotest.(check (option string))
    "account id extracted" (Some "acct_123")
    (Openai_codex_oauth.extract_account_id ~access_token:token ~id_token:None)

let test_validate_provider_name_rejects_non_codex_provider () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let config_path = Filename.concat clawq_dir "config.json" in
      let oc = open_out config_path in
      output_string oc
        {|{
  "providers": {
    "openai": {
      "api_key": "sk-test"
    }
  }
}|};
      close_out oc;
      match
        Openai_codex_oauth.validate_provider_name ~provider_name:"openai"
      with
      | Ok () -> Alcotest.fail "expected validation error"
      | Error msg ->
          Alcotest.(check bool)
            "mentions codex oauth" true
            (String.length msg > 0))

let test_save_provider_credentials_encrypts_when_enabled () =
  with_temp_home ~master_key:"test-master-key" (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let config_path = Filename.concat clawq_dir "config.json" in
      let oc = open_out config_path in
      output_string oc
        {|{
  "security": {
    "encrypt_secrets": true
  },
  "providers": {
    "openai-codex": {
      "kind": "openai-codex"
    }
  }
}|};
      close_out oc;
      let creds =
        {
          Runtime_config.access_token = "access-token";
          refresh_token = "refresh-token";
          expires_at_ms = 4102444800000;
          account_id = Some "acct_123";
          email = Some "user@example.com";
        }
      in
      match
        Openai_codex_oauth.save_provider_credentials
          ~provider_name:"openai-codex" creds
      with
      | Error msg -> Alcotest.fail msg
      | Ok () ->
          let json = Yojson.Safe.from_file config_path in
          let open Yojson.Safe.Util in
          let access_token =
            json |> member "providers" |> member "openai-codex"
            |> member "codex_oauth" |> member "access_token" |> to_string
          in
          let refresh_token =
            json |> member "providers" |> member "openai-codex"
            |> member "codex_oauth" |> member "refresh_token" |> to_string
          in
          Alcotest.(check bool)
            "access token encrypted" true
            (Secret_store.is_encrypted access_token);
          Alcotest.(check bool)
            "refresh token encrypted" true
            (Secret_store.is_encrypted refresh_token))

let suite =
  [
    Alcotest.test_case "parse callback input url" `Quick
      test_parse_callback_input_url;
    Alcotest.test_case "parse callback input state mismatch" `Quick
      test_parse_callback_input_state_mismatch;
    Alcotest.test_case "extract account id from access token" `Quick
      test_extract_account_id_from_access_token;
    Alcotest.test_case "validate provider name rejects non-codex provider"
      `Quick test_validate_provider_name_rejects_non_codex_provider;
    Alcotest.test_case "save provider credentials encrypts when enabled" `Quick
      test_save_provider_credentials_encrypts_when_enabled;
  ]
