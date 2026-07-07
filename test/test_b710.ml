(* B710: Pre-switch context check tests *)

let make_fake_provider_config = Test_helpers.make_fake_provider_config
let with_text_provider = Test_helpers.with_text_provider

(* Fill agent history with n message pairs (user + assistant) *)
let fill_agent_history agent n =
  for _ = 1 to n do
    agent.Agent.history <-
      Provider.make_message ~role:"user"
        ~content:"This is a test message with enough content to use tokens."
      :: agent.Agent.history;
    agent.Agent.history <-
      Provider.make_message ~role:"assistant"
        ~content:"This is a response with enough content to use some tokens."
      :: agent.Agent.history
  done

(* B710 test 1: Returns None when new model has unknown context window *)
let test_returns_none_unknown_model () =
  with_text_provider (fun config ->
      let agent = Agent.create ~config () in
      fill_agent_history agent 10;
      Lwt_main.run
        (let open Lwt.Syntax in
         let* result =
           Agent.pre_switch_compact_if_needed agent
             ~new_model:"unknown-model-xyz" ()
         in
         Alcotest.(check bool)
           "returns None for unknown model" true (result = None);
         Lwt.return_unit))

(* B710 test 2: Returns None when new window >= current window *)
let test_returns_none_larger_or_equal_window () =
  with_text_provider (fun config ->
      (* Set up agent with a 128k window model *)
      let config =
        {
          config with
          agent_defaults =
            { config.agent_defaults with primary_model = "small-window-model" };
          model_context_limits =
            [ ("small-window-model", 128000); ("large-window-model", 200000) ];
        }
      in
      let agent = Agent.create ~config () in
      fill_agent_history agent 10;
      Lwt_main.run
        (let open Lwt.Syntax in
         let* result =
           Agent.pre_switch_compact_if_needed agent
             ~new_model:"large-window-model" ()
         in
         Alcotest.(check bool)
           "returns None when new window is larger" true (result = None);
         Lwt.return_unit))

(* B710 test 3: Returns None when current tokens fit in new window *)
let test_returns_none_fits_in_new_window () =
  with_text_provider (fun config ->
      (* Set up agent with a large 200k window *)
      let config =
        {
          config with
          agent_defaults =
            { config.agent_defaults with primary_model = "large-window-model" };
          model_context_limits =
            [ ("large-window-model", 200000); ("medium-window-model", 128000) ];
        }
      in
      let agent = Agent.create ~config () in
      (* Fill with only 10 pairs — ~1300 tokens, well under 128k *)
      fill_agent_history agent 10;
      Lwt_main.run
        (let open Lwt.Syntax in
         let* result =
           Agent.pre_switch_compact_if_needed agent
             ~new_model:"medium-window-model" ()
         in
         Alcotest.(check bool)
           "returns None when tokens fit in new window" true (result = None);
         Lwt.return_unit))

(* B710 test 4: Compacts when current tokens exceed new window *)
let test_compacts_when_exceeds_new_window () =
  with_text_provider (fun config ->
      (* Set up agent with a very small 2000 token window (current model) *)
      let config =
        {
          config with
          agent_defaults =
            { config.agent_defaults with primary_model = "current-model" };
          model_context_limits =
            [ ("current-model", 200000); ("new-small-model", 2000) ];
          memory = { config.memory with pre_compaction_flush = false };
        }
      in
      let agent = Agent.create ~config () in
      (* Fill with 260 pairs — ~6500 tokens, exceeds 2000 token window *)
      fill_agent_history agent 260;
      let pre_tokens = Agent.estimate_history_tokens agent.history in
      Alcotest.(check bool)
        "pre_tokens exceed new window" true (pre_tokens > 2000);
      Lwt_main.run
        (let open Lwt.Syntax in
         let* result =
           Agent.pre_switch_compact_if_needed agent ~new_model:"new-small-model"
             ()
         in
         Alcotest.(check bool)
           "returns Some when compaction triggered" true (result <> None);
         (match result with
         | Some info ->
             Alcotest.(check bool)
               "post tokens less than pre tokens" true
               (info.Agent.post_tokens < info.Agent.pre_tokens);
             Alcotest.(check int)
               "context window matches new model" 2000 info.Agent.context_window
         | None -> Alcotest.fail "expected Some compaction_info");
         (* Verify the agent's config was left at the new model *)
         Alcotest.(check string)
           "agent config set to new model" "new-small-model"
           agent.Agent.config.agent_defaults.primary_model;
         Lwt.return_unit))

(* B710 test 5: Config restored when compaction not needed *)
let test_config_unchanged_when_no_compaction () =
  with_text_provider (fun config ->
      let config =
        {
          config with
          agent_defaults =
            { config.agent_defaults with primary_model = "current-model" };
          model_context_limits =
            [ ("current-model", 200000); ("new-model", 300000) ];
        }
      in
      let agent = Agent.create ~config () in
      fill_agent_history agent 10;
      let original_model = agent.Agent.config.agent_defaults.primary_model in
      Lwt_main.run
        (let open Lwt.Syntax in
         let* result =
           Agent.pre_switch_compact_if_needed agent ~new_model:"new-model" ()
         in
         Alcotest.(check bool) "returns None" true (result = None);
         Alcotest.(check string)
           "config unchanged" original_model
           agent.Agent.config.agent_defaults.primary_model;
         Lwt.return_unit))

let suite =
  [
    Alcotest.test_case "returns None for unknown model" `Quick
      test_returns_none_unknown_model;
    Alcotest.test_case "returns None for larger window" `Quick
      test_returns_none_larger_or_equal_window;
    Alcotest.test_case "returns None when tokens fit" `Quick
      test_returns_none_fits_in_new_window;
    Alcotest.test_case "compacts when exceeds new window" `Quick
      test_compacts_when_exceeds_new_window;
    Alcotest.test_case "config unchanged when no compaction" `Quick
      test_config_unchanged_when_no_compaction;
  ]
