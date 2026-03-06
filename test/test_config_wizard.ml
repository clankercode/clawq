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
  (* Select openrouter (first option) -> ProviderApiKey *)
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
  let m, _ = update KeyEnter m in
  let m, _ = update (KeyChar 'k') m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update (KeyChar 'n') m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  let m, _ = update KeyEnter m in
  Alcotest.check check_step "channel menu" ChannelMenu m.step

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
  ]
