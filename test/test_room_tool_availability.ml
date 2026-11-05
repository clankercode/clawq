(** B844: a Room turn leaves out room tools that cannot work in its room, and
    the room tool descriptions say what they cover. *)

open Lwt.Syntax

let check_bool = Alcotest.(check bool)
let contains = Test_helpers.string_contains
let room = "C0CA6UR8FFU"
let session_key = "slack:" ^ room
let memory_tools = Room_tool_availability.room_memory_tools
let github_item_tools = Room_tool_availability.github_item_tools
let room_tools = memory_tools @ github_item_tools

let ensure_github_schema db =
  Github_room_event_journal.ensure_schema db;
  Github_item_projection.ensure_schema db;
  Github_pr_subscriptions.init_schema db;
  Github_route_store.ensure_schema db

let with_db f =
  Test_helpers.with_temp_home (fun _ ->
      Test_helpers.with_memory_store
        ~init_schema:[ Access_snapshot.init_schema; ensure_github_schema ]
        f)

let bind_room_memory ~db ~room_id =
  let profile_id = Memory.insert_room_profile ~db ~name:("p-" ^ room_id) in
  Memory.upsert_room_profile_binding ~db ~room_id ~profile_id;
  ignore
    (Memory.create_scope ~db ~kind:"room" ~key:room_id ~profile_id
       ~provenance:"test" ())

let config =
  let d = Runtime_config.default in
  {
    d with
    default_provider = Some "openai";
    providers =
      [
        ( "openai",
          {
            Runtime_config.default_provider_config with
            api_key = "fixture-only";
            kind = Some "openai";
            default_model = Some "gpt-test";
            base_url = Some "http://127.0.0.1:1";
          } );
      ];
    (* Dynamic, so the system prompt carries its "## Tools" section; the
       sections that read the host (cwd, git, clock, docs) stay off. *)
    prompt =
      {
        d.prompt with
        dynamic_enabled = true;
        include_tools_section = true;
        include_workspace_section = false;
        include_runtime_section = false;
        include_datetime_section = false;
        include_project_docs = false;
        workspace_files = [];
      };
    observer = { d.observer with enabled = false };
    postmortem = { d.postmortem with enabled = false };
    resilience = { d.resilience with max_retries = 0; fallback_provider = None };
    agent_defaults =
      {
        d.agent_defaults with
        primary_model = "openai:gpt-test";
        show_thinking = false;
        show_tool_calls = false;
      };
  }

let simple_tool ?(result = "ok") name =
  {
    Tool.name;
    description = "test tool " ^ name;
    parameters_schema =
      `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ];
    invoke = (fun ?context:_ _ -> Lwt.return result);
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let pearl_fact = "Pearl DEX: public testnet since October (fake search result)"

(* The production room tools, plus working tools that never need room setup. *)
let register_tools ~db ~config registry =
  Tools_builtin_room_memory.register_room_memory_tools ~db registry;
  Github_room_tools.register_runtime_tools ~db ~config registry;
  Tool_registry.register registry (simple_tool ~result:pearl_fact "web_search")

let tool_names = function
  | Some (`List tools) ->
      List.filter_map
        (fun tool ->
          match
            Yojson.Safe.Util.(tool |> member "function" |> member "name")
          with
          | `String name -> Some name
          | _ -> None
          | exception _ -> None)
        tools
  | _ -> []

let system_text messages =
  List.find_map
    (fun (m : Provider.message) ->
      if m.role = "system" then Some m.content else None)
    messages
  |> Option.value ~default:""

let text model content =
  Provider.Text
    {
      content;
      model;
      usage = None;
      provider_response_items_json = None;
      thinking = None;
    }

let calls model specs =
  Provider.ToolCalls
    {
      calls =
        List.mapi
          (fun i (function_name, arguments) ->
            {
              Provider.id = Printf.sprintf "call_%d_%s" i function_name;
              function_name;
              arguments;
            })
          specs;
      model;
      usage = None;
      thinking = None;
      provider_response_items_json = None;
    }

type capture = {
  offered : string list list ref;  (** tool names per provider request *)
  systems : string list ref;  (** system prompt per provider request *)
  tool_results : string list ref;  (** tool results seen so far *)
  requests : int ref;
}

(* Route the native OpenAI-compatible provider to [respond], recording the
   tools and system prompt of every request. The caller restores. *)
let install_provider respond =
  let cap =
    {
      offered = ref [];
      systems = ref [];
      tool_results = ref [];
      requests = ref 0;
    }
  in
  let complete ~config:_ ~provider:_ ~model ~messages ?tools ?session_key:_ () =
    cap.offered := !(cap.offered) @ [ tool_names tools ];
    cap.systems := !(cap.systems) @ [ system_text messages ];
    cap.tool_results :=
      List.filter_map
        (fun (m : Provider.message) ->
          if m.role = "tool" then Some m.content else None)
        messages;
    let step = !(cap.requests) in
    incr cap.requests;
    Lwt.return (respond ~model step)
  in
  Provider.native_complete := [ (Provider.OpenAICompat, complete) ];
  Provider.native_stream :=
    [
      ( Provider.OpenAICompat,
        fun ~config
          ~provider
          ~model
          ~messages
          ?tools
          ?session_key
          ~on_chunk:_
          ()
        -> complete ~config ~provider ~model ~messages ?tools ?session_key () );
    ];
  cap

let with_provider respond f =
  let old_complete = !Provider.native_complete
  and old_stream = !Provider.native_stream in
  Fun.protect
    ~finally:(fun () ->
      Provider.native_complete := old_complete;
      Provider.native_stream := old_stream)
    (fun () -> f (install_provider respond))

(* A Room turn in [room], as Slack runs it: room snapshot, db and session key. *)
let room_turn ?(extra_tools = []) ~db respond =
  let registry = Tool_registry.create () in
  register_tools ~db ~config registry;
  List.iter (Tool_registry.register registry) extra_tools;
  let snap =
    Access_snapshot.create_and_persist ~db ~config
      ~work_type:Access_snapshot.Room_turn ~session_key ~room_id:room
      ~room_policy_decision:"allow" ()
  in
  let agent = Agent.create ~config ~tool_registry:registry () in
  agent.Agent.access_snapshot_id <- Some snap.id;
  agent.access_snapshot <- Some snap;
  with_provider respond (fun cap ->
      let response =
        Lwt_main.run
          (Agent.turn agent ~db ~session_key
             ~user_message:"What's the status of the Pearl DEX in Amaroo?" ())
      in
      (response, cap))

let offered_any cap name = List.exists (List.mem name) !(cap.offered)

let prompt_lists cap name =
  List.exists (fun s -> contains s name) !(cap.systems)

let test_unbound_hides () =
  with_db (fun db ->
      Alcotest.(check (list string))
        "an unbound room with no GitHub feed cannot use any room tool"
        room_tools
        (Room_tool_availability.unavailable_tools ~db ~session_key ~room_id:room
           ());
      let response, cap =
        room_turn ~db (fun ~model _ -> text model "No tracked data here.")
      in
      Alcotest.(check string) "turn answers" "No tracked data here." response;
      check_bool "provider called" true (!(cap.offered) <> []);
      List.iter
        (fun name ->
          check_bool (name ^ " not offered") false (offered_any cap name);
          check_bool
            (name ^ " not listed in the system prompt")
            false (prompt_lists cap name))
        room_tools;
      check_bool "working tool still offered" true
        (offered_any cap "web_search");
      check_bool "feed settings do not need item data" true
        (offered_any cap "github_feed_settings"))

(* Replays the #core-sync incident: the model reaches for the room tools first,
   then a tool that works. Before B844 the two room-not-configured errors
   aborted the turn with "[Room not configured]". *)
let test_unbound_answers () =
  with_db (fun db ->
      let answer = "The Pearl DEX is on public testnet." in
      let response, cap =
        room_turn ~db (fun ~model step ->
            match step with
            | 0 ->
                calls model
                  [
                    ("room_memory_list", "{}");
                    ("github_room_search_items", {|{"query":"Pearl DEX"}|});
                  ]
            | 1 -> calls model [ ("web_search", {|{"query":"Pearl DEX"}|}) ]
            | _ -> text model answer)
      in
      Alcotest.(check string)
        "final answer from the working tool" answer response;
      check_bool "no room-not-configured abort" false
        (contains response "[Room not configured]");
      Alcotest.(check int) "three model calls" 3 !(cap.requests);
      List.iter
        (fun name ->
          check_bool
            (name ^ " call told the tool is unavailable here")
            true
            (List.exists
               (fun r ->
                 contains r ("Tool '" ^ name ^ "' is not available in this room"))
               !(cap.tool_results)))
        [ "room_memory_list"; "github_room_search_items" ];
      List.iter
        (fun name ->
          check_bool
            (name ^ " unavailable error is not configuration-class")
            false
            (Stuck_detector.is_configuration_error
               (Room_tool_availability.unavailable_error name)))
        room_tools)

let principal_error =
  "Error: github_account tool: CLAWQ_PRINCIPAL_ID must be set so the tool can \
   scope inspection to the current Principal."

let test_config_loop_aborts () =
  with_db (fun db ->
      let response, cap =
        room_turn ~db
          ~extra_tools:[ simple_tool ~result:principal_error "principal_probe" ]
          (fun ~model step ->
            if step < 5 then calls model [ ("principal_probe", "{}") ]
            else text model "should not get here")
      in
      check_bool "aborts as room not configured" true
        (contains response "[Room not configured]");
      Alcotest.(check int)
        "stops after the second identical configuration error" 2 !(cap.requests))

let test_bound_offers () =
  with_db (fun db ->
      bind_room_memory ~db ~room_id:room;
      Test_github_room_tools.seed_room db ~room;
      Alcotest.(check (list string))
        "a bound room with a GitHub feed can use every room tool" []
        (Room_tool_availability.unavailable_tools ~db ~session_key ~room_id:room
           ());
      let _, cap = room_turn ~db (fun ~model _ -> text model "done") in
      List.iter
        (fun name ->
          check_bool (name ^ " offered") true (offered_any cap name);
          check_bool
            (name ^ " listed in the system prompt")
            true (prompt_lists cap name))
        room_tools)

let test_partial_rooms () =
  with_db (fun db ->
      bind_room_memory ~db ~room_id:"CMEMONLY";
      Alcotest.(check (list string))
        "memory binding without GitHub data hides only GitHub item tools"
        github_item_tools
        (Room_tool_availability.unavailable_tools ~db
           ~session_key:"slack:CMEMONLY" ~room_id:"CMEMONLY" ());
      Test_github_room_tools.seed_room db ~room:"CGITONLY";
      Alcotest.(check (list string))
        "GitHub data without a memory binding hides only room memory tools"
        memory_tools
        (Room_tool_availability.unavailable_tools ~db
           ~session_key:"slack:CGITONLY" ~room_id:"CGITONLY" ()))

let test_non_room_turn () =
  with_db (fun db ->
      let registry = Tool_registry.create () in
      register_tools ~db ~config registry;
      let agent = Agent.create ~config ~tool_registry:registry () in
      with_provider
        (fun ~model _ -> text model "done")
        (fun cap ->
          ignore
            (Lwt_main.run
               (Agent.turn agent ~db ~session_key:"cli:local" ~user_message:"hi"
                  ()));
          check_bool "a turn without a Room snapshot keeps every tool" true
            (List.for_all (offered_any cap) room_tools)))

let test_slack_unbound () =
  Test_slack_threads.with_ingress (fun base _ _ _ _ run ->
      let db = base.Test_slack_huddle_fixtures.db in
      ensure_github_schema db;
      register_tools ~db
        ~config:(Session.get_config base.manager)
        (Option.get base.manager.tool_registry);
      let cap = install_provider (fun ~model _ -> text model "answered") in
      let* _ =
        run
          (Test_slack_threads.message
             "What's the status of the Pearl DEX in Amaroo?")
      in
      check_bool "Slack turn reached the provider" true (!(cap.offered) <> []);
      List.iter
        (fun name ->
          check_bool
            (name ^ " not offered in Slack")
            false (offered_any cap name))
        room_tools;
      check_bool "working tool offered in Slack" true
        (offered_any cap "web_search");
      Lwt.return_unit)

let test_slack_bound () =
  Test_slack_threads.with_ingress (fun base _ _ _ _ run ->
      let db = base.Test_slack_huddle_fixtures.db in
      ensure_github_schema db;
      bind_room_memory ~db ~room_id:"C123";
      Test_github_room_tools.seed_room db ~room:"C123";
      register_tools ~db
        ~config:(Session.get_config base.manager)
        (Option.get base.manager.tool_registry);
      let cap = install_provider (fun ~model _ -> text model "answered") in
      let* _ = run (Test_slack_threads.message "list our tracked PRs") in
      check_bool "Slack turn reached the provider" true (!(cap.offered) <> []);
      List.iter
        (fun name ->
          check_bool
            (name ^ " offered in a bound Slack room")
            true (offered_any cap name))
        room_tools;
      Lwt.return_unit)

let description registry name =
  match Tool_registry.find registry name with
  | Some tool -> tool.Tool.description
  | None -> Alcotest.fail ("missing tool " ^ name)

let test_descriptions () =
  with_db (fun db ->
      let registry = Tool_registry.create () in
      register_tools ~db ~config registry;
      let registered prefix =
        Tool_registry.list registry
        |> List.map (fun (t : Tool.t) -> t.name)
        |> List.filter (String.starts_with ~prefix)
        |> List.sort compare
      in
      Alcotest.(check (list string))
        "every room_memory_* tool is gated"
        (List.sort compare memory_tools)
        (registered "room_memory_");
      Alcotest.(check (list string))
        "every github_room_* tool is gated"
        (List.sort compare github_item_tools)
        (registered "github_room_");
      let pins name phrases =
        let d = description registry name in
        List.iter
          (fun phrase ->
            check_bool
              (Printf.sprintf "%s mentions %S" name phrase)
              true (contains d phrase))
          phrases
      in
      List.iter
        (fun name ->
          pins name
            [
              "GitHub issue";
              "PR";
              "projections";
              "Needs a GitHub feed";
              "Not for";
            ])
        github_item_tools;
      List.iter
        (fun name ->
          pins name
            [
              "this room's bound memory profile"; "Needs a room profile binding";
            ])
        memory_tools;
      pins "room_memory_list" [ "Not for" ])

let suite =
  [
    Alcotest.test_case
      "unbound room turn does not offer room memory or github room tools" `Quick
      test_unbound_hides;
    Alcotest.test_case "unbound room question completes with an answer" `Quick
      test_unbound_answers;
    Alcotest.test_case "genuine configuration loop still aborts" `Quick
      test_config_loop_aborts;
    Alcotest.test_case "bound room still offers room tools" `Quick
      test_bound_offers;
    Alcotest.test_case "partly configured rooms hide only the tools that fail"
      `Quick test_partial_rooms;
    Alcotest.test_case "turn without a Room snapshot keeps room tools" `Quick
      test_non_room_turn;
    Alcotest.test_case "unbound Slack room turn is not offered room tools"
      `Quick test_slack_unbound;
    Alcotest.test_case "bound Slack room turn is offered room tools" `Quick
      test_slack_bound;
    Alcotest.test_case
      "room tool descriptions name their data, prerequisite and limits" `Quick
      test_descriptions;
  ]
