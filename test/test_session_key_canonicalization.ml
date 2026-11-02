let raw = "github:acme/backend:pr:831"
let canonical = Session.sanitize_session_key raw

let rows db sql params =
  Sql_util.query_rows db sql
    ~bind:(fun stmt -> Sql_util.bind_params stmt params)
    ~of_stmt:(fun stmt ->
      List.init (Sqlite3.column_count stmt) (Sqlite3.column stmt))

let keys db table =
  rows db
    ("SELECT DISTINCT session_key FROM " ^ table ^ " ORDER BY session_key")
    []
  |> List.filter_map (function
    | [ Sqlite3.Data.TEXT key ] -> Some key
    | _ -> None)

let text model content =
  Provider.Text
    {
      content;
      model;
      usage = None;
      provider_response_items_json = None;
      thinking = None;
    }

let with_provider ?reply f =
  Test_helpers.with_temp_home (fun workspace ->
      let config =
        {
          Runtime_config.default with
          workspace;
          providers =
            [
              ( "openai",
                {
                  Runtime_config.default_provider_config with
                  api_key = "test";
                  kind = Some "openai";
                  default_model = Some "gpt-test";
                } );
            ];
          prompt =
            { Runtime_config.default.prompt with dynamic_enabled = false };
          security =
            { Runtime_config.default.security with tools_enabled = false };
          agent_defaults =
            {
              Runtime_config.default.agent_defaults with
              primary_model = "openai:gpt-test";
              show_thinking = false;
              show_tool_calls = false;
            };
        }
      in
      let requests = ref [] in
      let saved_complete = !Provider.native_complete in
      let saved_stream = !Provider.native_stream in
      let complete ~config:_ ~provider:_ ~model ~messages ?tools:_ ?session_key
          () =
        requests := (session_key, messages, model) :: !requests;
        match reply with
        | Some reply -> reply model messages
        | None -> Lwt.return (text model "ok")
      in
      Fun.protect
        ~finally:(fun () ->
          Provider.native_complete := saved_complete;
          Provider.native_stream := saved_stream)
        (fun () ->
          Provider.register_native_complete Provider.OpenAICompat complete;
          Provider.register_native_stream Provider.OpenAICompat
            (fun
              ~config
              ~provider
              ~model
              ~messages
              ?tools
              ?session_key
              ~on_chunk
              ()
            ->
              let open Lwt.Syntax in
              let* response =
                complete ~config ~provider ~model ~messages ?tools ?session_key
                  ()
              in
              let* () =
                match response with
                | Provider.Text { content; _ } ->
                    on_chunk (Provider.Delta content)
                | Provider.ToolCalls _ -> Lwt.return_unit
              in
              let* () = on_chunk Provider.Done in
              Lwt.return response);
          f config requests))

type entry = Buffered | Try | Stream

let run entry mgr message =
  match entry with
  | Buffered ->
      Session.turn mgr ~key:raw ~message ~channel:"github"
        ~channel_id:"acme/backend"
        ~snapshot_work_type:Access_snapshot.GitHub_trigger ()
  | Try ->
      let open Lwt.Syntax in
      let* result =
        Session.try_turn mgr ~key:raw ~message ~channel:"github"
          ~channel_id:"acme/backend" ()
      in
      Lwt.return (Option.value ~default:"busy" result)
  | Stream ->
      Session.turn_stream mgr ~key:raw ~message ~channel:"github"
        ~channel_id:"acme/backend"
        ~snapshot_work_type:Access_snapshot.GitHub_trigger
        ~on_chunk:(fun _ -> Lwt.return_unit)
        ()

let check_keys mgr db =
  List.iter
    (fun table ->
      Alcotest.(check (list string))
        (table ^ " uses canonical identity")
        [ canonical ] (keys db table))
    [ "messages"; "session_state"; "session_workspace_state" ];
  Alcotest.(check bool)
    "canonical session exists" true
    (Hashtbl.mem mgr.Session.sessions canonical);
  List.iter
    (fun (name, present) -> Alcotest.(check bool) name false present)
    [
      ("no raw activity", Hashtbl.mem mgr.live_activity raw);
      ("no raw timestamp", Hashtbl.mem mgr.last_active raw);
      ("no raw continuation", Hashtbl.mem mgr.continuation_checks raw);
      ("no raw observer", Hashtbl.mem mgr.observer_last_checked raw);
    ];
  Alcotest.(check (option (pair string string)))
    "routing remains raw owner/repo"
    (Some ("github", "acme/backend"))
    (Memory.get_session_channel ~db ~session_key:canonical)

let test_entry entry () =
  with_provider (fun config requests ->
      Test_helpers.with_memory_store (fun db ->
          let mgr = Session.create ~config ~db () in
          Alcotest.(check string)
            "turn completes" "ok"
            (Lwt_main.run (run entry mgr "first"));
          check_keys mgr db;
          List.iter
            (fun (key, _, _) ->
              Alcotest.(check (option string))
                "provider uses canonical key" (Some canonical) key)
            !requests;
          let agent, _, _ = Hashtbl.find mgr.sessions canonical in
          if entry <> Try then
            Alcotest.(check (option string))
              "snapshot uses canonical key" (Some canonical)
              (Option.bind agent.Agent.access_snapshot (fun snap ->
                   snap.Access_snapshot.session_key))))

let test_restart_and_eviction () =
  with_provider (fun config requests ->
      let path = Filename.concat config.workspace "memory.db" in
      let open_db f =
        let db = Memory.init ~db_path:path () in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.db_close db))
          (fun () -> f db)
      in
      open_db (fun db ->
          let mgr = Session.create ~config ~db () in
          ignore (Lwt_main.run (run Buffered mgr "first exchange")));
      open_db (fun db ->
          let mgr = Session.create ~config ~db () in
          ignore (Lwt_main.run (run Stream mgr "after restart"));
          let _, messages, _ = List.hd !requests in
          Alcotest.(check int)
            "first input restored exactly once" 1
            (List.length
               (List.filter
                  (fun (m : Provider.message) -> m.content = "first exchange")
                  messages));
          let evicted =
            Lwt_main.run
              (Session.evict_idle_sessions
                 ~now:(Unix.gettimeofday () +. 3600.)
                 mgr ~idle_threshold_s:1800. ())
          in
          Alcotest.(check int) "evicted" 1 evicted;
          Alcotest.(check int)
            "activity side tables cleaned" 0
            (Hashtbl.length mgr.live_activity);
          ignore (Lwt_main.run (run Try mgr "after eviction"));
          let _, messages, _ = List.hd !requests in
          List.iter
            (fun content ->
              Alcotest.(check int)
                (content ^ " restored once")
                1
                (List.length
                   (List.filter
                      (fun (m : Provider.message) -> m.content = content)
                      messages)))
            [ "first exchange"; "after restart" ];
          Alcotest.(check int)
            "all exchanges persisted once" 6
            (List.length (Memory.load_history ~db ~session_key:canonical));
          check_keys mgr db))

let test_special_command entry () =
  with_provider (fun config requests ->
      let mgr = Session.create ~config () in
      let seen = ref None in
      Session.set_special_command_handler mgr
        (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
          seen := Some key;
          Lwt.return (Some "handled"));
      Alcotest.(check string)
        "handled before provider" "handled"
        (Lwt_main.run (run entry mgr "/status"));
      Alcotest.(check (option string))
        "command gets canonical identity" (Some canonical) !seen;
      Alcotest.(check int) "no provider request" 0 (List.length !requests);
      Alcotest.(check bool)
        "no raw activity row" false
        (Hashtbl.mem mgr.live_activity raw))

let test_tool_identity () =
  let calls = ref 0 in
  let reply model _messages =
    incr calls;
    if !calls = 1 then
      Lwt.return
        (Provider.ToolCalls
           {
             calls =
               [
                 {
                   Provider.id = "inspect-key";
                   function_name = "inspect_key";
                   arguments = "{}";
                 };
               ];
             model;
             usage = None;
             provider_response_items_json = None;
             thinking = None;
           })
    else Lwt.return (text model "ok")
  in
  with_provider ~reply (fun config _ ->
      Test_helpers.with_memory_store (fun db ->
          let config =
            {
              config with
              security = { config.security with tools_enabled = true };
            }
          in
          let registry = Tool_registry.create () in
          let seen = ref None and found = ref false and persisted = ref false in
          let manager = ref None in
          Tool_registry.register registry
            {
              Tool.name = "inspect_key";
              description = "inspect test identity";
              parameters_schema = `Assoc [ ("type", `String "object") ];
              risk_level = Tool.Low;
              deferred = false;
              invoke_stream = None;
              invoke =
                (fun ?context _ ->
                  seen := Option.bind context (fun ctx -> ctx.Tool.session_key);
                  (found :=
                     match (!manager, !seen) with
                     | Some mgr, Some key ->
                         Hashtbl.mem mgr.Session.sessions key
                     | _ -> false);
                  persisted := keys db "messages" = [ canonical ];
                  Lwt.return "inspected");
            };
          let mgr = Session.create ~config ~db ~tool_registry:registry () in
          manager := Some mgr;
          ignore (Lwt_main.run (run Buffered mgr "inspect"));
          Alcotest.(check (option string))
            "tool context key" (Some canonical) !seen;
          Alcotest.(check bool) "tool can index actual session" true !found;
          Alcotest.(check bool) "mid-round history is canonical" true !persisted;
          check_keys mgr db))

let test_busy_aliases () =
  let entered, enter = Lwt.wait () and release, unlock = Lwt.wait () in
  let count = ref 0 in
  let reply model _ =
    incr count;
    if !count = 1 then (
      Lwt.wakeup_later enter ();
      let open Lwt.Syntax in
      let* () = release in
      Lwt.return (text model "ok"))
    else Lwt.return (text model "ok")
  in
  with_provider ~reply (fun config _ ->
      Test_helpers.with_memory_store (fun db ->
          let mgr = Session.create ~config ~db () in
          Session.register_channel_notifier mgr ~key:canonical (fun _ ->
              Lwt.return_unit);
          Lwt_main.run
            (let open Lwt.Syntax in
             let first = run Buffered mgr "first" in
             Lwt.finalize
               (fun () ->
                 let* () = entered in
                 let* busy =
                   Session.try_turn mgr ~key:canonical ~message:"busy probe" ()
                 in
                 Alcotest.(check (option string))
                   "alias shares busy mutex" None busy;
                 let* bang =
                   Session.try_turn mgr ~key:raw ~message:"!urgent" ()
                 in
                 Alcotest.(check (option string))
                   "bang also sees busy mutex" None bang;
                 let _, _, interrupt = Hashtbl.find mgr.sessions canonical in
                 Alcotest.(check (option string))
                   "bang reaches canonical interrupt" (Some "urgent") !interrupt;
                 let* queued =
                   Session.turn mgr ~key:raw ~message:"follow-up"
                     ~deferred_if_busy:true ()
                 in
                 Alcotest.(check string)
                   "queued on canonical session" Session.queued_message_response
                   queued;
                 Alcotest.(check bool)
                   "no raw queue bucket" false
                   (Hashtbl.mem mgr.queued_messages raw);
                 Lwt.wakeup_later unlock ();
                 let* _ = first in
                 Lwt.return_unit)
               (fun () ->
                 if Lwt.is_sleeping release then Lwt.wakeup_later unlock ();
                 Lwt.catch
                   (fun () ->
                     let* _ = first in
                     Lwt.return_unit)
                   (fun _ -> Lwt.return_unit)));
          check_keys mgr db))

let test_resume () =
  with_provider (fun config requests ->
      Test_helpers.with_memory_store (fun db ->
          let mgr = Session.create ~config ~db () in
          Memory.store_message ~db ~session_key:canonical
            (Provider.make_message ~role:"user" ~content:"resume me");
          Lwt_main.run
            (Daemon_resume.resume_agent_session ~session_manager:mgr ~config
               ~session_key:raw ~channel:"github" ~channel_id:"acme/backend" ());
          List.iter
            (fun (key, _, _) ->
              Alcotest.(check (option string))
                "restart-resume uses canonical provider key" (Some canonical)
                key)
            !requests;
          Alcotest.(check (list string))
            "resume persists only canonical messages" [ canonical ]
            (keys db "messages")))

let test_legacy_file_restart () =
  with_provider (fun config requests ->
      let path = Filename.concat config.workspace "legacy.db" in
      let use_db f =
        let db = Memory.init ~db_path:path () in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.db_close db))
          (fun () -> f db)
      in
      use_db (fun db ->
          Memory.store_message ~db ~session_key:raw
            (Provider.make_message ~role:"user" ~content:"legacy prefix");
          Memory.set_session_model_override ~db ~session_key:canonical
            ~model:"openai:chosen-model");
      use_db (fun db ->
          let mgr = Session.create ~config ~db () in
          ignore (Lwt_main.run (run Buffered mgr "after legacy restart"));
          let _, messages, model = List.hd !requests in
          Alcotest.(check string)
            "canonical model used for real turn" "chosen-model" model;
          Alcotest.(check bool)
            "legacy prefix included in first request" true
            (List.exists
               (fun (m : Provider.message) -> m.content = "legacy prefix")
               messages);
          ignore
            (Lwt_main.run
               (Session.evict_idle_sessions
                  ~now:(Unix.gettimeofday () +. 3600.)
                  mgr ~idle_threshold_s:1800. ()));
          ignore (Lwt_main.run (run Try mgr "after legacy eviction"));
          let _, messages, _ = List.hd !requests in
          Alcotest.(check int)
            "legacy prefix survives eviction exactly once" 1
            (List.length
               (List.filter
                  (fun (m : Provider.message) -> m.content = "legacy prefix")
                  messages));
          Alcotest.(check int)
            "recovered prefix and both exchanges persisted" 5
            (List.length (Memory.load_history ~db ~session_key:canonical));
          check_keys mgr db))

let test_failed_turn () =
  let reply _model _ = Lwt.fail (Failure "injected provider failure") in
  with_provider ~reply (fun config _ ->
      Test_helpers.with_memory_store (fun db ->
          let config =
            {
              config with
              resilience =
                { config.resilience with max_retries = 0; base_delay_s = 0.0 };
            }
          in
          let mgr = Session.create ~config ~db () in
          ignore
            (Lwt_main.run
               (Lwt.catch
                  (fun () -> run Stream mgr "persist before provider error")
                  (fun _ -> Lwt.return "failed")));
          Alcotest.(check (list string))
            "error path keeps canonical persisted key" [ canonical ]
            (keys db "messages");
          Alcotest.(check bool)
            "error path does not leave raw activity" false
            (Hashtbl.mem mgr.live_activity raw)))

let suite =
  [
    Alcotest.test_case "raw buffered turn persists canonical keys" `Quick
      (test_entry Buffered);
    Alcotest.test_case "raw try turn persists canonical keys" `Quick
      (test_entry Try);
    Alcotest.test_case "raw streaming turn persists canonical keys" `Quick
      (test_entry Stream);
    Alcotest.test_case "restart and eviction restore raw-key exchanges once"
      `Quick test_restart_and_eviction;
    Alcotest.test_case "buffered special command receives canonical key" `Quick
      (test_special_command Buffered);
    Alcotest.test_case "try special command receives canonical key" `Quick
      (test_special_command Try);
    Alcotest.test_case "stream special command receives canonical key" `Quick
      (test_special_command Stream);
    Alcotest.test_case
      "tool and incremental persistence share canonical identity" `Quick
      test_tool_identity;
    Alcotest.test_case
      "raw and canonical aliases share busy queue and interrupt" `Quick
      test_busy_aliases;
    Alcotest.test_case "restart resume uses canonical key" `Quick test_resume;
    Alcotest.test_case
      "legacy prefix and model survive file restart and eviction" `Quick
      test_legacy_file_restart;
    Alcotest.test_case "failed stream persists canonical history" `Quick
      test_failed_turn;
  ]
