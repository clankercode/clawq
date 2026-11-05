open Lwt.Syntax
open Test_slack_huddle_fixtures

let name = "slack_canvas_create"

let outside =
  "slack_canvas_create requires an authorized Slack turn. Reply in the current \
   conversation, or run this tool from Slack; do not supply a channel ID or \
   token."

let dm =
  "slack_canvas_create works in channels; in a DM reply normally and clawq \
   attaches long answers as a file."

let args ?(title = "Research <&>") ?(markdown = "Complete report.") () =
  obj [ ("title", str title); ("markdown", str markdown) ]

let registry config current_config db =
  let sandbox =
    Sandbox.create ~backend:Sandbox.None
      ~workspace:config.Runtime_config.workspace ~extra_allowed_paths:[]
      ~workspace_only:false ()
  in
  Option.get
    (Daemon_startup.init_tool_registry ~config ~current_config ~sandbox
       ~db:(Some db))

let with_canvas f =
  Test_slack_threads.with_ingress (fun base calls handler _ _ _ ->
      Test_slack_reply_paths.artifacts handler;
      let registered = registry !(base.cfg) base.cfg base.db in
      List.iter
        (Tool_registry.replace (Option.get base.manager.tool_registry))
        (Tool_registry.list registered);
      check_bool "full startup replaces placeholder" true
        (Tool_registry.find registered name <> None);
      f base calls handler)

let writes calls =
  List.filter
    (fun (m, _) ->
      List.mem m
        [
          "canvases.create";
          "canvases.access.set";
          "canvases.delete";
          "chat.postMessage";
          "chat.update";
          "chat.delete";
          "files.getUploadURLExternal";
          "files.completeUploadExternal";
        ])
    !calls

let invoke base ?(arguments = args ()) () = base.invoke ~args:arguments name

let test_outside () =
  with_canvas (fun base calls _ ->
      let* out = invoke base () in
      Alcotest.(check string) "exact outside error" outside out;
      let* out =
        Slack_reply_context.with_context
          { connector = "slack"; conversation = Channel } (fun () ->
            invoke base ())
      in
      Alcotest.(check string)
        "presentation metadata grants no authority" outside out;
      check_bool "zero writes" true (writes calls = []);
      Lwt.return_unit)

let test_dm () =
  List.iter
    (fun group ->
      with_canvas (fun base calls handler ->
          let previous = !handler in
          (handler :=
             fun m p ->
               if m <> "conversations.info" then previous m p
               else
                 ok
                   [
                     ( "channel",
                       obj
                         [
                           ("id", str "C123");
                           ("is_member", `Bool true);
                           ("is_private", `Bool true);
                           ("is_im", `Bool (not group));
                           ("is_mpim", `Bool group);
                           ("user", str "U123");
                         ] );
                   ]);
          base.turn (fun () ->
              let* out = invoke base () in
              Alcotest.(check string) "typed DM guidance" dm out;
              check_bool "zero writes" true (writes calls = []);
              Lwt.return_unit)))
    [ false; true ]

let test_selectors () =
  with_canvas (fun base calls _ ->
      base.turn (fun () ->
          let tool =
            Option.get
              (Tool_registry.find (Option.get base.manager.tool_registry) name)
          in
          let expected =
            parse
              {|{"type":"object","properties":{"title":{"type":"string","minLength":1,"maxLength":120,"description":"Short canvas title (required)."},"markdown":{"type":"string","minLength":1,"maxLength":1048576,"description":"Complete Markdown write-up (required)."}},"required":["title","markdown"],"additionalProperties":false}|}
          in
          check_bool "exact strict schema" true
            (tool.parameters_schema = expected);
          check_bool "medium and non streaming" true
            (tool.risk_level = Tool.Medium && tool.invoke_stream = None);
          let* () =
            Lwt_list.iter_s
              (fun key ->
                let* out =
                  invoke base
                    ~arguments:(update (args ()) [ (key, str "COTHER") ])
                    ()
                in
                expect_error out;
                check_bool "zero writes" true (writes calls = []);
                Lwt.return_unit)
              [ "channel_id"; "token"; "thread_ts"; "parent"; "user_id" ]
          in
          Lwt.return_unit))

let test_stale () =
  with_canvas (fun base calls _ ->
      base.turn (fun () ->
          let tool =
            Option.get
              (Tool_registry.find (Option.get base.manager.tool_registry) name)
          in
          let context =
            {
              Tool.default_context with
              session_key = Some base.key;
              snapshot_id = base.agent.access_snapshot_id;
            }
          in
          base.snapshot ();
          let* out = tool.invoke ~context (args ()) in
          Alcotest.(check string) "cached snapshot rejected" outside out;
          Tool.before_message_injection ();
          let* out = invoke base () in
          Alcotest.(check string) "injection invalidates authority" outside out;
          check_bool "zero writes" true (writes calls = []);
          Lwt.return_unit))

let test_channel () =
  with_canvas (fun base calls _ ->
      base.turn (fun () ->
          let* out = invoke base () in
          let json = parse out in
          check_bool "success after read grant" true
            (json
            = obj
                [
                  ("canvas_id", str "FCANVAS");
                  ("permalink", str "https://example.slack.com/docs/FCANVAS");
                  ("shared_message_ts", str "200.000001");
                ]);
          let operations = writes calls in
          Alcotest.(check (list string))
            "create, carrier, read grant"
            [ "canvases.create"; "chat.postMessage"; "canvases.access.set" ]
            (List.map fst operations);
          let post = snd (List.nth operations 1)
          and grant = snd (List.nth operations 2) in
          check_bool "server owned channel and parent" true
            (get "channel" post = str "C123" && get "thread_ts" post = `Null);
          check_bool "escaped title and labeled link" true
            (contains
               (Slack_huddles.field "text" post)
               "Research &lt;&amp;&gt;\n\
                <https://example.slack.com/docs/FCANVAS|Read full answer>");
          check_bool "read only channel" true
            (get "access_level" grant = str "read"
            && get "channel_ids" grant = arr [ str "C123" ]
            && get "user_ids" grant = `Null);
          Lwt.return_unit))

let test_title () =
  with_canvas (fun base calls _ ->
      base.turn (fun () ->
          let emoji n = String.concat "" (List.init n (fun _ -> "😀")) in
          let* () =
            Lwt_list.iter_s
              (fun title ->
                let* out = invoke base ~arguments:(args ~title ()) () in
                expect_error out;
                check_bool "invalid title never writes" true (writes calls = []);
                Lwt.return_unit)
              [ ""; " \n\t "; "\xff"; emoji 121 ]
          in
          let* out =
            invoke base ~arguments:(args ~title:(" " ^ emoji 120 ^ " ") ()) ()
          in
          ignore (parse out);
          let create =
            List.find (fun (m, _) -> m = "canvases.create") !calls |> snd
          in
          check_bool "trimmed 120 Unicode scalars" true
            (get "title" create = str (emoji 120));
          Lwt.return_unit))

let test_failure () =
  with_canvas (fun base calls handler ->
      let old = !handler in
      (handler :=
         fun m p ->
           if m = "canvases.access.set" then
             obj [ ("ok", `Bool false); ("error", str "internal_error") ]
           else old m p);
      base.turn (fun () ->
          let* out = invoke base () in
          expect_error out;
          check_bool "actionable normal reply" true
            (contains out "reply normally");
          check_bool "no fake success" false (contains out "permalink");
          Alcotest.(check int)
            "one create" 1
            (Test_slack_reply_paths.count calls "canvases.create");
          check_bool "owned canvas cleanup" true
            (List.mem_assoc "canvases.delete" !calls);
          check_bool "carrier link removed" true
            (List.mem_assoc "chat.update" !calls);
          check_bool "no explicit tool file fallback" false
            (List.mem_assoc "files.getUploadURLExternal" !calls);
          Lwt.return_unit))

let test_minimal () =
  with_canvas (fun base _ _ ->
      let full =
        Option.get
          (Tool_registry.find (Option.get base.manager.tool_registry) name)
      in
      let sandbox =
        Sandbox.create ~backend:Sandbox.None
          ~workspace:base.manager.config.workspace ~extra_allowed_paths:[]
          ~workspace_only:false ()
      in
      let core = Tool_registry.create () in
      Tools_builtin.register_all ~config:base.manager.config ~sandbox core;
      let minimal = Option.get (Tool_registry.find core name) in
      check_bool "identical schema and description" true
        (minimal.parameters_schema = full.parameters_schema
        && minimal.description = full.description);
      let* out = minimal.invoke (args ()) in
      Alcotest.(check string)
        "exact disabled error"
        "slack_canvas_create is disabled in minimal build; use the full clawq \
         binary."
        out;
      Lwt.return_unit)

let test_parent_and_modes () =
  List.iter
    (fun mode ->
      with_canvas (fun base calls _ ->
          Test_slack_reply_paths.mode base mode;
          Slack_huddle_context.with_request ~session_manager:base.manager
            ~key:base.key ~channel_id:"C123" ~user_id:"U123" ~message_id:"101.1"
            ~thread_ts:"100.000001" (fun () ->
              base.snapshot ();
              let* out = invoke base () in
              ignore (parse out);
              let post =
                List.find (fun (m, _) -> m = "chat.postMessage") !calls |> snd
              in
              check_bool "verified legacy parent preserved in file/off" true
                (get "thread_ts" post = str "100.000001");
              Lwt.return_unit)))
    [ Slack_config_types.File; Off ]

let test_revoked_mid_call () =
  Test_slack_threads.with_ingress (fun base calls handler _ _ _ ->
      Test_slack_reply_paths.artifacts handler;
      let old = !handler in
      (handler :=
         fun m p ->
           let answer = old m p in
           if m = "users.info" then Tool.before_message_injection ();
           answer);
      let tool =
        Slack_canvas_tool.make ~current_config:(fun () -> !(base.cfg))
      in
      base.turn (fun () ->
          let* out =
            tool.invoke
              ~context:
                {
                  Tool.default_context with
                  session_key = Some base.key;
                  snapshot_id = base.agent.access_snapshot_id;
                }
              (args ())
          in
          expect_error out;
          check_bool "invalidation during authorization stops publication" true
            (writes calls = []);
          Lwt.return_unit))

let test_builtin_filters () =
  let registry = Tool_registry.create () in
  Tool_registry.register registry (Tools_builtin.slack_canvas_create ());
  List.iter
    (fun (template : Agent_template.t) ->
      let filtered = Agent_template.filter_tool_registry registry template in
      check_bool
        (template.name ^ " effective default tool access")
        (List.mem template.name [ "researcher"; "documenter" ])
        (Tool_registry.find filtered name <> None))
    Agent_template_builtins.all

let test_room_policy_revoked ~stage ~after_members () =
  with_canvas (fun base calls handler ->
      let old = !handler in
      let revoked = ref false and writes_at_revocation = ref [] in
      let membership_finished = ref false in
      let revoke () =
        let cfg = !(base.cfg) in
        let cfg =
          {
            cfg with
            external_room_policy =
              {
                default_action =
                  Runtime_config_types.Policy_deny ("room access revoked", false);
                per_connector = [];
              };
          }
        in
        revoked := true;
        writes_at_revocation := writes calls;
        base.cfg := cfg;
        Session.update_config base.manager cfg
      in
      (handler :=
         fun m p ->
           let answer = old m p in
           if m = "auth.test" then membership_finished := false;
           if m = "conversations.members" then membership_finished := true;
           let reached_stage =
             if stage = 2 then List.mem_assoc "chat.postMessage" !calls
             else Test_slack_reply_paths.count calls "canvases.create" >= stage
           in
           if (not !revoked) && reached_stage then
             if after_members && m = "conversations.members" then revoke ()
             else if
               (not after_members) && m = "conversations.info"
               && !membership_finished
             then revoke ();
           if m = "conversations.info" then
             update answer
               [
                 ( "channel",
                   update (get "channel" answer)
                     [ ("is_ext_shared", `Bool true) ] );
               ]
           else answer);
      base.turn (fun () ->
          let* out = invoke base () in
          expect_error out;
          check_bool "room policy reloaded during authorization" true !revoked;
          check_bool "no writes after room policy revocation" true
            (writes calls = !writes_at_revocation);
          check_bool "revocation prevents read grant" false
            (List.mem_assoc "canvases.access.set" !calls);
          Lwt.return_unit))

let test_queue_guidance () =
  Test_slack_threads.with_ingress (fun base _ handler _ provider run ->
      Test_slack_reply_paths.artifacts handler;
      let direct = ref false in
      let old = !handler in
      (handler :=
         fun m p ->
           let response = old m p in
           if m = "conversations.info" then
             update response
               [
                 ( "channel",
                   update (get "channel" response)
                     [ ("is_mpim", `Bool !direct) ] );
               ]
           else response);
      let hold, release = Lwt.wait () in
      let prompts = ref [] in
      (provider :=
         fun ~model ~messages ->
           let prompt = (List.hd messages).Provider.content in
           prompts := !prompts @ [ prompt ];
           if List.length !prompts = 1 then
             Lwt.map
               (fun () -> Test_slack_empty_reply.answer "first answer" ~model)
               hold
           else
             Lwt.return (Test_slack_empty_reply.answer "second answer" ~model));
      let first = run (Test_slack_threads.message "first request") in
      let* () = Test_slack_socket.settle 3 in
      check_bool "first request reached provider" true (List.length !prompts = 1);
      direct := true;
      let* _ =
        run ~id:"second"
          (Test_slack_threads.message ~thread:"100.000001" ~ts:"100.000002"
             ~user:"U456" "second request")
      in
      Lwt.wakeup_later release ();
      let* _ = first in
      check_bool "both requests ran" true (List.length !prompts = 2);
      check_bool "first prompt channel guidance" true
        (contains (List.hd !prompts) "automatically placed in a canvas");
      check_bool "queue uses its own current metadata" true
        (contains (List.nth !prompts 1) "Markdown files");
      check_bool "queue did not borrow channel guidance" false
        (contains (List.nth !prompts 1) "automatically placed in a canvas");
      Lwt.return_unit)

let test_cancelled () =
  with_canvas (fun base calls _ ->
      base.turn (fun () ->
          let tool =
            Option.get
              (Tool_registry.find (Option.get base.manager.tool_registry) name)
          in
          let context =
            {
              Tool.default_context with
              session_key = Some base.key;
              snapshot_id = base.agent.access_snapshot_id;
              interrupt_check = Some (fun () -> Some "cancelled by user");
            }
          in
          let* out = tool.invoke ~context (args ()) in
          expect_error out;
          check_bool "cancelled invocation makes no writes" true
            (writes calls = []);
          Lwt.return_unit))

let suite =
  List.map
    (fun (n, f) -> (n, `Quick, f))
    [
      ("cancelled invocation makes no writes", test_cancelled);
      ("outside slack gives actionable error", test_outside);
      ("dm tool gives file guidance", test_dm);
      ("arguments cannot select destination", test_selectors);
      ("stale snapshot cannot publish", test_stale);
      ("channel tool shares read only", test_channel);
      ("canvas title validates Unicode and length", test_title);
      ("failed tool cleans and returns error", test_failure);
      ("minimal tool is disabled", test_minimal);
      ( "explicit file and off modes preserve verified parent",
        test_parent_and_modes );
      ("invalidation during authorization stops writes", test_revoked_mid_call);
      ( "room policy revocation during membership prevents create",
        test_room_policy_revoked ~stage:0 ~after_members:true );
      ( "room policy revocation during membership prevents later publication",
        test_room_policy_revoked ~stage:1 ~after_members:true );
      ( "room policy revocation during last membership prevents read grant",
        test_room_policy_revoked ~stage:2 ~after_members:true );
      ( "room policy revocation after membership prevents publication",
        test_room_policy_revoked ~stage:0 ~after_members:false );
      ( "room policy revocation after last membership prevents read grant",
        test_room_policy_revoked ~stage:2 ~after_members:false );
      ( "builtin filters grant researcher and documenter only",
        test_builtin_filters );
      ("native queued request gets its own guidance", test_queue_guidance);
    ]
