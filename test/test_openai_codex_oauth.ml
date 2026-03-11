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

let contains s sub =
  let sl = String.length s and subl = String.length sub in
  if subl > sl then false
  else if subl = 0 then true
  else
    let found = ref false in
    for i = 0 to sl - subl do
      if String.sub s i subl = sub then found := true
    done;
    !found

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

let test_inspect_credentials_expired_refreshable () =
  let creds =
    {
      Runtime_config.access_token = "access-token";
      refresh_token = "refresh-token";
      expires_at_ms = 1_000;
      account_id = None;
      email = None;
    }
  in
  let health = Openai_codex_oauth.inspect_credentials ~now_ms:400_000 creds in
  Alcotest.(check bool) "has access token" true health.has_access_token;
  Alcotest.(check bool) "has refresh token" true health.has_refresh_token;
  Alcotest.(check bool) "refresh possible" true health.refresh_possible;
  Alcotest.(check bool) "expired" true health.expired;
  Alcotest.(check int) "expires in ms" (-399_000) health.expires_in_ms

let test_status_reports_refresh_window_without_expired_label () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let config_path = Filename.concat clawq_dir "config.json" in
      let oc = open_out config_path in
      output_string oc
        (Printf.sprintf
           {|{
  "providers": {
    "openai-codex": {
      "kind": "openai-codex",
      "codex_oauth": {
        "access_token": "tok",
        "refresh_token": "ref",
        "expires_at_ms": %d,
        "email": "user@example.com"
      }
    }
  }
}|}
           (Openai_codex_oauth.now_ms () + 240000));
      close_out oc;
      let status = Openai_codex_oauth.status ~provider_name:"openai-codex" () in
      Alcotest.(check bool)
        "mentions refresh window" true
        (contains status "inside refresh window, will refresh on use");
      Alcotest.(check bool)
        "does not say token expired" false
        (contains status "token expired"))

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

let test_save_then_load_roundtrip () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let config_path = Filename.concat clawq_dir "config.json" in
      let oc = open_out config_path in
      output_string oc
        {|{
  "providers": {
    "openai-codex": {
      "kind": "openai-codex"
    }
  }
}|};
      close_out oc;
      let creds =
        {
          Runtime_config.access_token = "access-token-roundtrip";
          refresh_token = "refresh-token-roundtrip";
          expires_at_ms = 4102444800000;
          account_id = Some "acct_456";
          email = Some "rt@example.com";
        }
      in
      (match
         Openai_codex_oauth.save_provider_credentials
           ~provider_name:"openai-codex" creds
       with
      | Error msg -> Alcotest.fail ("save failed: " ^ msg)
      | Ok () -> ());
      let cfg = Config_loader.load () in
      match List.assoc_opt "openai-codex" cfg.providers with
      | None -> Alcotest.fail "provider openai-codex not found after save+load"
      | Some provider -> (
          match provider.Runtime_config.codex_oauth with
          | None ->
              Alcotest.fail "codex_oauth is None after save+load roundtrip"
          | Some oauth ->
              Alcotest.(check string)
                "access_token roundtrip" "access-token-roundtrip"
                oauth.access_token;
              Alcotest.(check string)
                "refresh_token roundtrip" "refresh-token-roundtrip"
                oauth.refresh_token;
              Alcotest.(check int)
                "expires_at_ms roundtrip" 4102444800000 oauth.expires_at_ms))

let test_save_then_load_roundtrip_encrypted () =
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
          Runtime_config.access_token = "access-token-enc";
          refresh_token = "refresh-token-enc";
          expires_at_ms = 4102444800000;
          account_id = Some "acct_789";
          email = Some "enc@example.com";
        }
      in
      (match
         Openai_codex_oauth.save_provider_credentials
           ~provider_name:"openai-codex" creds
       with
      | Error msg -> Alcotest.fail ("save failed: " ^ msg)
      | Ok () -> ());
      let cfg = Config_loader.load () in
      match List.assoc_opt "openai-codex" cfg.providers with
      | None ->
          Alcotest.fail
            "provider openai-codex not found after encrypted save+load"
      | Some provider -> (
          match provider.Runtime_config.codex_oauth with
          | None ->
              Alcotest.fail
                "codex_oauth is None after encrypted save+load roundtrip"
          | Some oauth ->
              Alcotest.(check string)
                "access_token decrypted" "access-token-enc" oauth.access_token;
              Alcotest.(check string)
                "refresh_token decrypted" "refresh-token-enc"
                oauth.refresh_token))

let test_backfill_preserves_codex_oauth () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let config_path = Filename.concat clawq_dir "config.json" in
      let oc = open_out config_path in
      output_string oc
        {|{
  "providers": {
    "openai-codex": {
      "kind": "openai-codex",
      "codex_oauth": {
        "access_token": "my-token",
        "refresh_token": "my-refresh",
        "expires_at_ms": 4102444800000,
        "account_id": "acct_bf",
        "email": "bf@example.com"
      }
    }
  }
}|};
      close_out oc;
      (* First load triggers backfill (adds default fields) *)
      let _cfg1 = Config_loader.load () in
      (* Second load reads the backfilled file *)
      let cfg2 = Config_loader.load () in
      match List.assoc_opt "openai-codex" cfg2.providers with
      | None ->
          Alcotest.fail
            "provider openai-codex not found after backfill roundtrip"
      | Some provider -> (
          match provider.Runtime_config.codex_oauth with
          | None ->
              Alcotest.fail
                "codex_oauth is None after backfill — backfill stripped it!"
          | Some oauth ->
              Alcotest.(check string)
                "access_token preserved" "my-token" oauth.access_token;
              Alcotest.(check string)
                "refresh_token preserved" "my-refresh" oauth.refresh_token;
              Alcotest.(check int)
                "expires_at_ms preserved" 4102444800000 oauth.expires_at_ms))

let suite =
  [
    Alcotest.test_case "parse callback input url" `Quick
      test_parse_callback_input_url;
    Alcotest.test_case "parse callback input state mismatch" `Quick
      test_parse_callback_input_state_mismatch;
    Alcotest.test_case "extract account id from access token" `Quick
      test_extract_account_id_from_access_token;
    Alcotest.test_case "inspect credentials expired refreshable" `Quick
      test_inspect_credentials_expired_refreshable;
    Alcotest.test_case "status reports refresh window without expired label"
      `Quick test_status_reports_refresh_window_without_expired_label;
    Alcotest.test_case "validate provider name rejects non-codex provider"
      `Quick test_validate_provider_name_rejects_non_codex_provider;
    Alcotest.test_case "save provider credentials encrypts when enabled" `Quick
      test_save_provider_credentials_encrypts_when_enabled;
    Alcotest.test_case "save then load roundtrip" `Quick
      test_save_then_load_roundtrip;
    Alcotest.test_case "save then load roundtrip encrypted" `Quick
      test_save_then_load_roundtrip_encrypted;
    Alcotest.test_case "backfill preserves codex_oauth" `Quick
      test_backfill_preserves_codex_oauth;
  ]
