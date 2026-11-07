(* Exercise the public executor wrappers, using the phase3 registry/agent
   fixture pattern. Logging/audit projections must not alter execution. *)
let title = "CANVAS_TITLE_SENTINEL_5"
let markdown = "CANVAS_MARKDOWN_SENTINEL_5"
let raw_secret = "CANVAS_RAW_SENTINEL_5"
let contains = Test_helpers.string_contains
let check = Alcotest.(check bool)

let with_logs f =
  let messages = ref [] in
  let old_reporter = Logs.reporter () and old_level = Logs.level () in
  let reporter =
    {
      Logs.report =
        (fun _ _ ~over k msgf ->
          msgf (fun ?header:_ ?tags:_ fmt ->
              Format.kasprintf
                (fun s ->
                  messages := s :: !messages;
                  over ();
                  k ())
                fmt));
    }
  in
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter reporter;
  Fun.protect
    ~finally:(fun () ->
      Logs.set_reporter old_reporter;
      Logs.set_level old_level)
    (fun () -> f messages)

let test_executor streaming () =
  Test_helpers.with_temp_home (fun _ ->
      let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          Audit.init_schema db;
          List.iter
            (fun (scenario, arguments, raw, denied, result, sibling) ->
              let registry = Tool_registry.create () and invoked = ref [] in
              let tool : Tool.t =
                {
                  name = "slack_canvas_create";
                  description = "test executor boundary";
                  parameters_schema =
                    `Assoc
                      [
                        ( "required",
                          `List [ `String "title"; `String "markdown" ] );
                      ];
                  invoke =
                    (fun ?context:_ args ->
                      invoked := args :: !invoked;
                      Lwt.return result);
                  invoke_stream = None;
                  risk_level = Medium;
                  deferred = false;
                }
              in
              Tool_registry.register registry tool;
              Tool_registry.register registry
                {
                  tool with
                  name = "ordinary";
                  parameters_schema = `Assoc [];
                  invoke =
                    (fun ?context:_ _ -> Lwt.return "Error: ordinary failure");
                };
              let config = Test_phase3.default_config in
              let config =
                if not denied then config
                else
                  {
                    config with
                    room_profiles =
                      [
                        {
                          Runtime_config.id = "deny-canvas";
                          display_name = None;
                          model = "";
                          system_prompt = "";
                          max_tool_iterations = 10;
                          status = "active";
                          allowed_tools = [];
                          denied_tools = [ "slack_canvas_create" ];
                          access_bundle_ids = [];
                          ambient_enabled = false;
                          ambient_quiet_start = 0;
                          ambient_quiet_end = 0;
                          ambient_rate_limit_rph = 0;
                          low_volume = false;
                        };
                      ];
                    room_profile_bindings =
                      [
                        {
                          Runtime_config.room = "redaction";
                          profile_id = "deny-canvas";
                          active = true;
                        };
                      ];
                  }
              in
              let agent = Agent.create ~config ~tool_registry:registry () in
              let call : Provider.tool_call =
                {
                  id = "canvas";
                  function_name = "slack_canvas_create";
                  arguments;
                }
              in
              let calls =
                if sibling then
                  [
                    call;
                    {
                      Provider.id = "sibling";
                      function_name = "ordinary";
                      arguments = "{}";
                    };
                  ]
                else [ call ]
              in
              let original =
                Provider.make_message ~role:"assistant" ~content:""
              in
              agent.history <- [ { original with tool_calls = calls } ];
              let events = ref [] in
              with_logs (fun logs ->
                  Lwt_main.run
                    (if streaming then
                       Agent.execute_tool_calls_stream agent ~db:(Some db)
                         ~audit_enabled:true
                         ~session_key:(Some "slack:redaction")
                         ~raw_tool_calls_json:raw
                         ~on_chunk:(fun e ->
                           events := e :: !events;
                           Lwt.return_unit)
                         calls
                     else
                       Agent.execute_tool_calls agent ~db:(Some db)
                         ~audit_enabled:true
                         ~session_key:(Some "slack:redaction")
                         ~raw_tool_calls_json:raw calls);
                  let audit =
                    Audit.query ~db ~event_type:"tool_invocation" ~limit:100 ()
                    |> List.filter_map (fun (r : Audit.row) -> r.details)
                    |> String.concat "\n"
                  in
                  List.iter
                    (fun secret ->
                      check
                        (scenario ^ " logs redact " ^ secret)
                        false
                        (contains (String.concat "\n" !logs) secret);
                      check
                        (scenario ^ " audit redacts " ^ secret)
                        false (contains audit secret))
                    [ title; markdown; raw_secret ];
                  check "captured INFO output" true (!logs <> []);
                  check "audit invocation exists" true (audit <> "");
                  check "original calls remain in history" true
                    (List.exists
                       (fun (m : Provider.message) -> m.tool_calls = calls)
                       agent.history);
                  if
                    denied
                    || List.mem scenario [ "malformed"; "empty"; "validation" ]
                  then check (scenario ^ " never invokes") true (!invoked = []);
                  if scenario = "malformed" then
                    check "model sees original parse error" true
                      (List.exists
                         (fun (m : Provider.message) ->
                           contains m.content title)
                         agent.history);
                  if scenario = "success" then begin
                    check "execution receives original arguments" true
                      (!invoked = [ Yojson.Safe.from_string arguments ]);
                    check "history retains original result" true
                      (List.exists
                         (fun (m : Provider.message) ->
                           m.role = "tool" && m.content = result)
                         agent.history);
                    if streaming then
                      check "ToolStart retains arguments" true
                        (List.exists
                           (function
                             | Provider.ToolStart e -> e.arguments = arguments
                             | _ -> false)
                           !events)
                  end))
            [
              ( "success",
                Printf.sprintf {|{"title":"%s","markdown":"%s"}|} title markdown,
                Printf.sprintf
                  {|[{"function":{"name":"slack_canvas_create","arguments":"%s"}}]|}
                  raw_secret,
                false,
                title ^ markdown,
                false );
              ( "malformed",
                "{" ^ title ^ markdown,
                raw_secret,
                false,
                "unused",
                false );
              ("empty", "", raw_secret, false, "unused", false);
              ( "validation",
                Printf.sprintf {|{"title":"%s"}|} title,
                raw_secret,
                false,
                "unused",
                false );
              ( "denied",
                Printf.sprintf {|{"title":"%s","markdown":"%s"}|} title markdown,
                raw_secret,
                true,
                "unused",
                false );
              ( "mixed",
                "{}",
                Printf.sprintf
                  {|[{"name":"slack_canvas_create","arguments":"%s"},{"name":"ordinary","arguments":"{}"}]|}
                  raw_secret,
                false,
                "Error: " ^ markdown,
                true );
              ( "malformed raw",
                "{}",
                "[" ^ raw_secret,
                false,
                "Error: " ^ title,
                true );
            ]))

let test_ordinary () =
  let registry = Tool_registry.create () in
  Tool_registry.register registry
    {
      Tool.name = "ordinary";
      description = "";
      parameters_schema = `Assoc [];
      invoke = (fun ?context:_ _ -> Lwt.return "ordinary-result");
      invoke_stream = None;
      risk_level = Low;
      deferred = false;
    };
  let agent =
    Agent.create ~config:Test_phase3.default_config ~tool_registry:registry ()
  in
  with_logs (fun logs ->
      Lwt_main.run
        (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
           ~session_key:None
           [
             {
               Provider.id = "ordinary";
               function_name = "ordinary";
               arguments = {|{"ordinary":"value"}|};
             };
           ]);
      let logs = String.concat "\n" !logs in
      check "ordinary arguments preserved" true
        (contains logs {|{"ordinary":"value"}|});
      check "ordinary result preserved" true (contains logs "ordinary-result"))

let test_raw_only streaming () =
  Test_helpers.with_temp_home (fun _ ->
      let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          Audit.init_schema db;
          List.iter
            (fun (raw, suppressed) ->
              let registry = Tool_registry.create () in
              Tool_registry.register registry
                {
                  Tool.name = "ordinary";
                  description = "";
                  parameters_schema = `Assoc [];
                  invoke =
                    (fun ?context:_ _ -> Lwt.return "Error: ordinary failure");
                  invoke_stream = None;
                  risk_level = Low;
                  deferred = false;
                };
              let agent =
                Agent.create ~config:Test_phase3.default_config
                  ~tool_registry:registry ()
              in
              with_logs (fun logs ->
                  let calls =
                    [
                      {
                        Provider.id = "ordinary";
                        function_name = "ordinary";
                        arguments = "{}";
                      };
                    ]
                  in
                  Lwt_main.run
                    (if streaming then
                       Agent.execute_tool_calls_stream agent ~db:(Some db)
                         ~audit_enabled:true ~session_key:(Some "raw-only")
                         ~raw_tool_calls_json:raw
                         ~on_chunk:(fun _ -> Lwt.return_unit)
                         calls
                     else
                       Agent.execute_tool_calls agent ~db:(Some db)
                         ~audit_enabled:true ~session_key:(Some "raw-only")
                         ~raw_tool_calls_json:raw calls);
                  check "raw-only sensitive/unclassifiable batch suppressed"
                    false
                    (contains (String.concat "\n" !logs) raw_secret);
                  check "only valid ordinary raw batches reach diagnostics"
                    (not suppressed)
                    (contains (String.concat "\n" !logs) raw)))
            (List.map
               (fun raw -> (raw, true))
               [
                 Printf.sprintf
                   {|[{"function":{"name":"slack_canvas_create","arguments":"%s"}}]|}
                   raw_secret;
                 "[" ^ raw_secret;
                 Printf.sprintf {|[{"type":"mystery","payload":"%s"}]|}
                   raw_secret;
                 Printf.sprintf
                   {|[{"name":"ordinary","name":"slack_canvas_create","arguments":"%s"}]|}
                   raw_secret;
                 Printf.sprintf
                   {|[{"type":"function","name":"ordinary","arguments":"%s"}]|}
                   raw_secret;
                 Printf.sprintf
                   {|[{"type":"function_call","function":{"name":"ordinary","arguments":"%s"}}]|}
                   raw_secret;
                 Printf.sprintf
                   {|[{"type":"tool_use","name":"ordinary","arguments":"%s"}]|}
                   raw_secret;
                 Printf.sprintf
                   {|[{"type":"function","function":{"name":"ordinary","arguments":{"raw":"%s"}}}]|}
                   raw_secret;
                 Printf.sprintf
                   {|[{"type":"function_call","name":"ordinary","arguments":{"raw":"%s"}}]|}
                   raw_secret;
                 Printf.sprintf
                   {|[{"type":"tool_use","name":"ordinary","input":"%s"}]|}
                   raw_secret;
                 Printf.sprintf {|[{"name":"ordinary","payload":"%s"}]|}
                   raw_secret;
                 Printf.sprintf
                   {|[{"type":"function","function":{"name":"ordinary","arguments":"{}"},"input":{"raw":"%s"}}]|}
                   raw_secret;
                 Printf.sprintf
                   {|[{"type":"tool_use","name":"ordinary","input":{},"arguments":"%s"}]|}
                   raw_secret;
                 Printf.sprintf
                   {|[{"type":"function","function":{"name":"ordinary","name":"slack_canvas_create","arguments":"%s"}}]|}
                   raw_secret;
                 Printf.sprintf
                   {|[{"type":"function_call","name":"ordinary","arguments":"{}"},{"type":"function","name":"ordinary","arguments":"%s"}]|}
                   raw_secret;
               ]
            @ List.map
                (fun raw -> (raw, false))
                [
                  {|[{"type":"function","id":"ordinary","function":{"name":"ordinary","arguments":"{}"}}]|};
                  {|[{"type":"function_call","call_id":"ordinary","name":"ordinary","arguments":"{}"}]|};
                  {|[{"type":"tool_use","id":"ordinary","name":"ordinary","input":{}}]|};
                  {|[{"id":"ordinary","name":"ordinary","arguments":"{}"}]|};
                  {|[{"function":{"name":"ordinary","arguments":"{}"}}]|};
                ])))

let test_failure_labels streaming () =
  let reason =
    "Error: connector_history.enabled is false. Enable it in config."
  in
  List.iter
    (fun (label, raw, marker) ->
      let registry = Tool_registry.create () in
      Tool_registry.register registry
        {
          Tool.name = "inject_connector_history";
          description = "";
          parameters_schema = `Assoc [];
          invoke = (fun ?context:_ _ -> Lwt.return reason);
          invoke_stream = None;
          risk_level = Low;
          deferred = false;
        };
      let agent =
        Agent.create ~config:Test_phase3.default_config ~tool_registry:registry
          ()
      in
      with_logs (fun logs ->
          let calls =
            [
              {
                Provider.id = "history";
                function_name = "inject_connector_history";
                arguments = "{}";
              };
            ]
          in
          Lwt_main.run
            (if streaming then
               Agent.execute_tool_calls_stream agent ~db:None
                 ~audit_enabled:false ~session_key:None ?raw_tool_calls_json:raw
                 ~on_chunk:(fun _ -> Lwt.return_unit)
                 calls
             else
               Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
                 ~session_key:None ?raw_tool_calls_json:raw calls);
          let diagnostics =
            List.filter (fun s -> contains s "Raw model tool-call data") !logs
            |> String.concat "\n"
          in
          check (label ^ " has failure diagnostic") true (diagnostics <> "");
          check
            (label ^ " canvas label requires canvas evidence")
            (marker = Some "[slack canvas content redacted]")
            (contains diagnostics "[slack canvas content redacted]");
          check
            (label ^ " safe reason remains usable")
            (marker = None)
            (contains diagnostics reason);
          Option.iter
            (fun marker ->
              check
                (label ^ " correct redaction marker")
                true
                (contains diagnostics marker))
            marker;
          check
            (label ^ " raw secrets stay redacted")
            false
            (contains diagnostics raw_secret)))
    [
      ("no raw", None, None);
      ( "known ordinary",
        Some {|[{"name":"inject_connector_history","arguments":"{}"}]|},
        None );
      ( "unknown",
        Some ("[" ^ raw_secret),
        Some "[tool-call data redacted: unclassified batch]" );
      ( "unknown shape",
        Some (Printf.sprintf {|[{"type":"mystery","payload":"%s"}]|} raw_secret),
        Some "[tool-call data redacted: unclassified batch]" );
      ( "blank raw",
        Some "  ",
        Some "[tool-call data redacted: unclassified batch]" );
      ( "ambiguous duplicate name",
        Some
          (Printf.sprintf
             {|[{"name":"inject_connector_history","name":"slack_canvas_create","arguments":"%s"}]|}
             raw_secret),
        Some "[tool-call data redacted: unclassified batch]" );
      ( "unknown plus known canvas",
        Some
          (Printf.sprintf
             {|[{"type":"mystery","payload":"%s"},{"type":"tool_use","name":"slack_canvas_create","input":{}}]|}
             raw_secret),
        Some "[slack canvas content redacted]" );
      ( "known canvas plus unknown",
        Some
          (Printf.sprintf
             {|[{"type":"function_call","name":"slack_canvas_create","arguments":"{}"},{"type":"mystery","payload":"%s"}]|}
             raw_secret),
        Some "[slack canvas content redacted]" );
      ( "raw canvas sibling",
        Some
          (Printf.sprintf
             {|[{"name":"inject_connector_history","arguments":"{}"},{"name":"slack_canvas_create","arguments":"%s"}]|}
             raw_secret),
        Some "[slack canvas content redacted]" );
    ]

let suite =
  [
    ( "buffered failure diagnostics classify redaction",
      `Quick,
      test_failure_labels false );
    ( "streaming failure diagnostics classify redaction",
      `Quick,
      test_failure_labels true );
    ( "executor redacts canvas arguments in buffered calls",
      `Quick,
      test_executor false );
    ( "executor redacts canvas arguments in streaming calls",
      `Quick,
      test_executor true );
    ("non canvas structured logging remains", `Quick, test_ordinary);
    ( "buffered raw batch inspected beyond structured calls",
      `Quick,
      test_raw_only false );
    ( "streaming raw batch inspected beyond structured calls",
      `Quick,
      test_raw_only true );
  ]
