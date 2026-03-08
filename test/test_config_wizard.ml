(* test_config_wizard.ml — Tests for wizard update logic *)

open Config_wizard_model
open Config_wizard_update

let check_step =
  Alcotest.testable
    (fun fmt s ->
      let name =
        match s with
        | Welcome -> "Welcome"
        | ProviderSelect -> "ProviderSelect"
        | ProviderApiKey -> "ProviderApiKey"
        | ProviderBaseUrl -> "ProviderBaseUrl"
        | ProviderTestOffer -> "ProviderTestOffer"
        | ProviderTestResult -> "ProviderTestResult"
        | ModelSelect -> "ModelSelect"
        | SecurityTools -> "SecurityTools"
        | ToolSearchConfig -> "ToolSearchConfig"
        | SecurityWorkspace -> "SecurityWorkspace"
        | ChannelMenu -> "ChannelMenu"
        | ChannelTelegram -> "ChannelTelegram"
        | ChannelDiscord -> "ChannelDiscord"
        | ChannelSlack -> "ChannelSlack"
        | GatewayConfig -> "GatewayConfig"
        | MemoryConfig -> "MemoryConfig"
        | Review -> "Review"
        | Confirm -> "Confirm"
        | Done -> "Done"
      in
      Format.pp_print_string fmt name)
    ( = )

let check_action =
  Alcotest.testable
    (fun fmt a ->
      let name =
        match a with
        | Noop -> "Noop"
        | WriteConfig -> "WriteConfig"
        | TestProvider _ -> "TestProvider"
        | Quit -> "Quit"
      in
      Format.pp_print_string fmt name)
    (fun a b ->
      match (a, b) with
      | Noop, Noop | WriteConfig, WriteConfig | Quit, Quit -> true
      | TestProvider _, TestProvider _ -> true
      | _ -> false)

let test_welcome_to_provider_select () =
  let m = initial_model Onboard in
  let m', _ = update KeyEnter m in
  Alcotest.check check_step "after welcome" ProviderSelect m'.step

let test_provider_select_navigation () =
  let m = initial_model Onboard in
  let m, _ = update KeyEnter m in
  (* Should be at ProviderSelect *)
  let m, _ = update KeyDown m in
  match m.widget with
  | Select si -> Alcotest.(check int) "moved to 1" 1 si.selected
  | _ -> Alcotest.fail "expected Select widget"

let test_provider_select_wraps () =
  let m = initial_model Onboard in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyUp m in
  match m.widget with
  | Select si ->
      Alcotest.(check int)
        "wrapped to last"
        (List.length provider_names - 1)
        si.selected
  | _ -> Alcotest.fail "expected Select widget"

let test_full_onboard_flow () =
  let m = initial_model Onboard in
  (* Welcome -> ProviderSelect *)
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "provider select" ProviderSelect m.step;
  (* Select openrouter (second option) -> ProviderApiKey *)
  let m, _ = update KeyDown m in
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "api key" ProviderApiKey m.step;
  (* Type a key *)
  let m, _ = update (KeyChar 'k') m in
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "base url" ProviderBaseUrl m.step;
  (* Accept default url *)
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "test offer" ProviderTestOffer m.step;
  (* Decline test *)
  let m, _ = update (KeyChar 'n') m in
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "model select" ModelSelect m.step;
  (* Accept default model *)
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "security tools" SecurityTools m.step;
  (* Accept tools enabled *)
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "tool search config" ToolSearchConfig m.step;
  (* Accept tool search default *)
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "security workspace" SecurityWorkspace m.step;
  (* Accept workspace_only *)
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "review" Review m.step;
  (* Confirm *)
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "confirm" Confirm m.step;
  let m, action = update KeyEnter m in
  Alcotest.check check_step "done" Done m.step;
  Alcotest.check check_action "write" WriteConfig action

let test_back_navigation () =
  let m = initial_model Onboard in
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "at provider select" ProviderSelect m.step;
  let m, _ = update KeyEsc m in
  Alcotest.check check_step "back to welcome" Welcome m.step

let test_text_input_typing () =
  let ti =
    { label = "test"; value = ""; cursor = 0; secret = false; placeholder = "" }
  in
  let ti = text_input_update (KeyChar 'a') ti in
  let ti = text_input_update (KeyChar 'b') ti in
  let ti = text_input_update (KeyChar 'c') ti in
  Alcotest.(check string) "typed abc" "abc" ti.value;
  Alcotest.(check int) "cursor at 3" 3 ti.cursor

let test_text_input_backspace () =
  let ti =
    {
      label = "test";
      value = "abc";
      cursor = 3;
      secret = false;
      placeholder = "";
    }
  in
  let ti = text_input_update KeyBackspace ti in
  Alcotest.(check string) "after backspace" "ab" ti.value;
  Alcotest.(check int) "cursor at 2" 2 ti.cursor

let test_confirm_toggle () =
  let ci = { label = "test"; value = true } in
  let ci = confirm_update (KeyChar 'n') ci in
  Alcotest.(check bool) "toggled to no" false ci.value;
  let ci = confirm_update (KeyChar 'y') ci in
  Alcotest.(check bool) "toggled to yes" true ci.value

let test_test_provider_action () =
  let m = initial_model Onboard in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyDown m in
  let m, _ = update KeyEnter m in
  (* Type key *)
  let m, _ = update (KeyChar 'k') m in
  let m, _ = update KeyEnter m in
  (* Accept url *)
  let m, _ = update KeyEnter m in
  (* Accept test offer (default yes) *)
  let m, action = update KeyEnter m in
  Alcotest.check check_step "test result" ProviderTestResult m.step;
  Alcotest.check check_action "test action" (TestProvider ("", "", "")) action

let test_full_wizard_has_channel_menu () =
  let m = initial_model FullWizard in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyDown m in
  let m, _ = update KeyEnter m in
  let m, _ = update (KeyChar 'k') m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update (KeyChar 'n') m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "channel menu" ChannelMenu m.step

let test_openai_codex_skips_api_key_prompt () =
  let m = initial_model Onboard in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "codex goes to base url" ProviderBaseUrl m.step;
  Alcotest.(check string) "provider name" "openai-codex" m.current_provider.name

let test_prepopulated_model_preserves_tools_enabled () =
  (* Simulate a model pre-populated from existing config with tools disabled *)
  let m = { (initial_model FullWizard) with tools_enabled = false } in
  let m, _ = update KeyEnter m in
  (* Navigate through provider flow *)
  let m, _ = update KeyDown m in
  let m, _ = update KeyEnter m in
  let m, _ = update (KeyChar 'k') m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update (KeyChar 'n') m in
  let m, _ = update KeyEnter m in
  (* Accept default model *)
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "security tools" SecurityTools m.step;
  (* The confirm widget should reflect the pre-populated tools_enabled=false *)
  match m.widget with
  | ConfirmInput ci ->
      Alcotest.(check bool) "tools default false" false ci.value
  | _ -> Alcotest.fail "expected ConfirmInput widget"

let test_prepopulated_model_preserves_workspace_only () =
  let m = { (initial_model FullWizard) with workspace_only = false } in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyDown m in
  let m, _ = update KeyEnter m in
  let m, _ = update (KeyChar 'k') m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update (KeyChar 'n') m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "security workspace" SecurityWorkspace m.step;
  match m.widget with
  | ConfirmInput ci ->
      Alcotest.(check bool) "workspace default false" false ci.value
  | _ -> Alcotest.fail "expected ConfirmInput widget"

let test_prepopulated_existing_provider_prefills_api_key () =
  let existing_provider =
    {
      name = "openrouter";
      kind = None;
      api_key = "sk-existing-key";
      base_url = "https://openrouter.ai/api/v1";
      default_model = "";
    }
  in
  let m = { (initial_model Onboard) with providers = [ existing_provider ] } in
  let m, _ = update KeyEnter m in
  (* Select openrouter (second option) *)
  let m, _ = update KeyDown m in
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "api key" ProviderApiKey m.step;
  match m.widget with
  | TextInput ti ->
      Alcotest.(check string) "api key prefilled" "sk-existing-key" ti.value
  | _ -> Alcotest.fail "expected TextInput widget"

let test_prepopulated_provider_replaces_not_duplicates () =
  let existing_provider =
    {
      name = "openrouter";
      kind = None;
      api_key = "old-key";
      base_url = "https://openrouter.ai/api/v1";
      default_model = "";
    }
  in
  let m = { (initial_model Onboard) with providers = [ existing_provider ] } in
  let m, _ = update KeyEnter m in
  (* Select openrouter *)
  let m, _ = update KeyDown m in
  let m, _ = update KeyEnter m in
  (* Clear and type new key *)
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update KeyBackspace m in
  let m, _ = update (KeyChar 'n') m in
  let m, _ = update (KeyChar 'e') m in
  let m, _ = update (KeyChar 'w') m in
  let m, _ = update KeyEnter m in
  (* Accept base url *)
  let m, _ = update KeyEnter m in
  (* Decline test *)
  let m, _ = update (KeyChar 'n') m in
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "model select" ModelSelect m.step;
  (* Should have exactly 1 provider, not 2 *)
  Alcotest.(check int) "one provider" 1 (List.length m.providers);
  let p = List.hd m.providers in
  Alcotest.(check string) "updated key" "new" p.api_key

let test_prepopulated_channel_token_prefills () =
  let m =
    {
      (initial_model FullWizard) with
      telegram_token = "123:EXISTING";
      channel_sel = { telegram = true; discord = false; slack = false };
    }
  in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyDown m in
  let m, _ = update KeyEnter m in
  let m, _ = update (KeyChar 'k') m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update (KeyChar 'n') m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "channel menu" ChannelMenu m.step;
  (* Select Telegram *)
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "channel telegram" ChannelTelegram m.step;
  match m.widget with
  | TextInput ti ->
      Alcotest.(check string) "telegram token prefilled" "123:EXISTING" ti.value
  | _ -> Alcotest.fail "expected TextInput widget"

let test_wizard_merge_preserves_codex_oauth () =
  let base = Filename.get_temp_dir_name () in
  let dir =
    Filename.concat base ("clawq_wiz_" ^ string_of_int (Random.bits ()))
  in
  Unix.mkdir dir 0o755;
  let old_home = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" dir;
  Fun.protect
    (fun () ->
      let clawq_dir = Filename.concat dir ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let config_path = Filename.concat clawq_dir "config.json" in
      let oc = open_out config_path in
      output_string oc
        {|{
  "providers": {
    "openai-codex": {
      "kind": "openai-codex",
      "base_url": "https://chatgpt.com/backend-api/codex",
      "default_model": "openai-codex/gpt-5.4",
      "codex_oauth": {
        "access_token": "secret-access",
        "refresh_token": "secret-refresh",
        "expires_at_ms": 4102444800000,
        "account_id": "acct_test",
        "email": "test@example.com"
      }
    }
  }
}|};
      close_out oc;
      let m =
        {
          (Config_wizard_model.initial_model Onboard) with
          providers =
            [
              {
                Config_wizard_model.empty_provider with
                name = "openai-codex";
                kind = Some "openai-codex";
                base_url = "https://chatgpt.com/backend-api/codex";
                default_model = "openai-codex/gpt-5.4";
              };
            ];
          primary_model = "openai-codex/gpt-5.4";
        }
      in
      let _path = Config_wizard_tui.write_wizard_config m in
      let json = Yojson.Safe.from_file config_path in
      let open Yojson.Safe.Util in
      let oauth =
        json |> member "providers" |> member "openai-codex"
        |> member "codex_oauth"
      in
      match oauth with
      | `Null -> Alcotest.fail "codex_oauth was stripped by wizard merge"
      | `Assoc _ ->
          let at = oauth |> member "access_token" |> to_string in
          Alcotest.(check string) "access_token preserved" "secret-access" at;
          let rt = oauth |> member "refresh_token" |> to_string in
          Alcotest.(check string) "refresh_token preserved" "secret-refresh" rt
      | _ -> Alcotest.fail "codex_oauth has unexpected type")
    ~finally:(fun () ->
      (match old_home with
      | Some v -> Unix.putenv "HOME" v
      | None -> Unix.putenv "HOME" "");
      (try
         Unix.unlink
           (Filename.concat (Filename.concat dir ".clawq") "config.json")
       with _ -> ());
      (try Unix.rmdir (Filename.concat dir ".clawq") with _ -> ());
      try Unix.rmdir dir with _ -> ())

let suite =
  [
    Alcotest.test_case "welcome -> provider select" `Quick
      test_welcome_to_provider_select;
    Alcotest.test_case "provider select navigation" `Quick
      test_provider_select_navigation;
    Alcotest.test_case "provider select wraps" `Quick test_provider_select_wraps;
    Alcotest.test_case "full onboard flow" `Quick test_full_onboard_flow;
    Alcotest.test_case "back navigation" `Quick test_back_navigation;
    Alcotest.test_case "text input typing" `Quick test_text_input_typing;
    Alcotest.test_case "text input backspace" `Quick test_text_input_backspace;
    Alcotest.test_case "confirm toggle" `Quick test_confirm_toggle;
    Alcotest.test_case "test provider action" `Quick test_test_provider_action;
    Alcotest.test_case "full wizard channel menu" `Quick
      test_full_wizard_has_channel_menu;
    Alcotest.test_case "openai codex skips api key prompt" `Quick
      test_openai_codex_skips_api_key_prompt;
    Alcotest.test_case "prepopulated preserves tools_enabled" `Quick
      test_prepopulated_model_preserves_tools_enabled;
    Alcotest.test_case "prepopulated preserves workspace_only" `Quick
      test_prepopulated_model_preserves_workspace_only;
    Alcotest.test_case "prepopulated provider prefills api key" `Quick
      test_prepopulated_existing_provider_prefills_api_key;
    Alcotest.test_case "prepopulated provider replaces not duplicates" `Quick
      test_prepopulated_provider_replaces_not_duplicates;
    Alcotest.test_case "prepopulated channel token prefills" `Quick
      test_prepopulated_channel_token_prefills;
    Alcotest.test_case "wizard merge preserves codex_oauth" `Quick
      test_wizard_merge_preserves_codex_oauth;
  ]
