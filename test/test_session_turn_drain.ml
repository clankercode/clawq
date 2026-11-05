open Lwt.Syntax

type entry = Buffered | Try | Stream

let key = "telegram:turn-drain:user"

let text model content =
  Provider.Text
    {
      content;
      model;
      usage = None;
      provider_response_items_json = None;
      thinking = None;
    }

let run entry mgr ?before_drain message =
  match entry with
  | Buffered -> Session.turn mgr ~key ~message ?before_drain ()
  | Try ->
      let* response = Session.try_turn mgr ~key ~message ?before_drain () in
      Lwt.return (Option.get response)
  | Stream ->
      Session.turn_stream mgr ~key ~message ?before_drain
        ~on_chunk:(fun _ -> Lwt.return_unit)
        ()

let with_fixture f =
  Test_helpers.with_temp_home (fun workspace ->
      Test_helpers.with_memory_store (fun db ->
          let config =
            {
              Runtime_config.default with
              workspace;
              default_provider = Some "fake";
              providers =
                [
                  ( "fake",
                    Test_helpers.make_fake_provider_config "http://127.0.0.1:1"
                  );
                ];
              prompt =
                { Runtime_config.default.prompt with dynamic_enabled = false };
              security =
                { Runtime_config.default.security with tools_enabled = false };
              agent_defaults =
                {
                  Runtime_config.default.agent_defaults with
                  primary_model = "fake:fake-model";
                  show_thinking = false;
                  show_tool_calls = false;
                };
              resilience =
                {
                  Runtime_config.default.resilience with
                  max_retries = 0;
                  fallback_provider = None;
                };
              observer =
                { Runtime_config.default.observer with enabled = false };
              postmortem =
                { Runtime_config.default.postmortem with enabled = false };
            }
          in
          let calls = ref [] in
          let reply =
            ref (fun model message -> Lwt.return (text model message))
          in
          let complete ~config:_ ~provider:_ ~model ~messages ?tools:_
              ?session_key:_ () =
            let message =
              List.rev messages
              |> List.find (fun (m : Provider.message) -> m.role = "user")
              |> fun m -> m.Provider.content
            in
            calls := !calls @ [ message ];
            !reply model message
          in
          let saved_complete = !Provider.native_complete
          and saved_stream = !Provider.native_stream in
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
                  ~on_chunk:_
                  ()
                ->
                  complete ~config ~provider ~model ~messages ?tools
                    ?session_key ());
              let mgr = Session.create ~config ~db () in
              let sent = ref [] in
              Session.register_channel_notifier mgr ~key (fun response ->
                  sent := !sent @ [ response ];
                  Lwt.return_unit);
              f mgr db calls reply sent)))

let with_queued_turn entry ?first_error ?before_drain_error ?followup_error
    ?(extra_followup = false) ?(before_release = fun _ -> Lwt.return_unit) check
    =
  with_fixture (fun mgr db calls reply sent ->
      let started, signal_started = Lwt.wait () in
      let release, signal_release = Lwt.wait () in
      (reply :=
         fun model message ->
           if message = "A" then (
             Lwt.wakeup_later signal_started ();
             let* () = release in
             match first_error with
             | Some exn -> Lwt.fail exn
             | None -> Lwt.return (text model "reply:A"))
           else
             match followup_error with
             | Some exn when message = "B" -> Lwt.fail exn
             | _ -> Lwt.return (text model ("reply:" ^ message)));
      let before_drain =
        Option.map (fun exn _response -> Lwt.fail exn) before_drain_error
      in
      Lwt_main.run
        (let outcome =
           Lwt.try_bind
             (fun () -> run entry mgr ?before_drain "A")
             (fun response -> Lwt.return (Ok response))
             (fun exn -> Lwt.return (Error exn))
         in
         let* () = started in
         let enqueue message =
           let* response =
             Session.turn mgr ~key ~message ~deferred_if_busy:true ()
           in
           Alcotest.(check string)
             "follow-up queued while A holds lock"
             Session.queued_message_response response;
           Lwt.return_unit
         in
         let* () = enqueue "B" in
         let* () = if extra_followup then enqueue "C" else Lwt.return_unit in
         Alcotest.(check int)
           "durable rows before release"
           (if extra_followup then 2 else 1)
           (Memory.queue_count ~db ~session_key:key);
         let* () = before_release mgr in
         Lwt.wakeup_later signal_release ();
         let* outcome = outcome in
         let* () = Lwt.pause () in
         let _, mutex, _ = Hashtbl.find mgr.Session.sessions key in
         Alcotest.(check bool)
           "session lock released" false
           (Lwt_mutex.is_locked mutex);
         Alcotest.(check int)
           "in-flight counter cleared" 0
           (Session.current_in_flight mgr);
         check mgr db !calls !sent outcome;
         Lwt.return_unit))

let check_error expected = function
  | Error actual ->
      Alcotest.(check bool)
        "original exception preserved unchanged" true (actual == expected)
  | Ok response -> Alcotest.failf "expected exception, got %S" response

let test_failed_turn entry () =
  let original = Failure "turn A failed" in
  with_queued_turn entry ~first_error:original (fun _ _ calls sent outcome ->
      check_error original outcome;
      Alcotest.(check (list string)) "both turns ran once" [ "A"; "B" ] calls;
      Alcotest.(check (list string)) "follow-up delivered" [ "reply:B" ] sent)

let test_before_drain entry () =
  let original = Failure "before_drain failed" in
  with_queued_turn entry ~before_drain_error:original
    (fun _ _ calls sent outcome ->
      check_error original outcome;
      Alcotest.(check (list string)) "both turns ran once" [ "A"; "B" ] calls;
      Alcotest.(check (list string)) "follow-up delivered" [ "reply:B" ] sent)

let test_followup_failure entry () =
  let original = Failure "turn A failed" in
  with_queued_turn entry ~first_error:original
    ~followup_error:(Failure "turn B failed") ~extra_followup:true
    (fun mgr db calls sent outcome ->
      check_error original outcome;
      Alcotest.(check (list string))
        "failed follow-up attempted once" [ "A"; "B" ] calls;
      Alcotest.(check (list string)) "no failed response delivered" [] sent;
      Alcotest.(check int)
        "failed and unattempted durable rows retained" 2
        (Memory.queue_count ~db ~session_key:key);
      Alcotest.(check (option string))
        "drain stops before C" (Some "C")
        (Option.map
           (fun (q : Session_types.queued_message) -> q.message)
           (Session.take_next_queued_message mgr ~key)))

let test_success entry () =
  with_queued_turn entry ~extra_followup:true (fun _ db calls sent outcome ->
      (match outcome with
      | Ok response -> Alcotest.(check string) "A response" "reply:A" response
      | Error exn -> raise exn);
      Alcotest.(check (list string))
        "turns run in FIFO order" [ "A"; "B"; "C" ] calls;
      Alcotest.(check (list string))
        "follow-ups delivered in FIFO order" [ "reply:B"; "reply:C" ] sent;
      Alcotest.(check int)
        "durable rows removed" 0
        (Memory.queue_count ~db ~session_key:key))

let test_durable_row entry () =
  let original = Failure "turn A failed" in
  with_queued_turn entry ~first_error:original ~extra_followup:true
    (fun _ db calls _ outcome ->
      check_error original outcome;
      Alcotest.(check int)
        "durable rows removed after failure-path drain" 0
        (Memory.queue_count ~db ~session_key:key);
      Alcotest.(check (list string))
        "failure-path drain preserves FIFO" [ "A"; "B"; "C" ] calls)

let test_retained entry () =
  let original = Session_queue.Request_retained "retain current request" in
  with_queued_turn entry ~first_error:original (fun _ db calls _ outcome ->
      check_error original outcome;
      Alcotest.(check (list string))
        "retained request does not drain" [ "A" ] calls;
      Alcotest.(check int)
        "follow-up row retained" 1
        (Memory.queue_count ~db ~session_key:key))

let test_cancelled entry () =
  with_queued_turn entry ~first_error:Lwt.Canceled (fun _ db calls _ outcome ->
      check_error Lwt.Canceled outcome;
      Alcotest.(check (list string)) "cancellation does not drain" [ "A" ] calls;
      Alcotest.(check int)
        "follow-up row retained" 1
        (Memory.queue_count ~db ~session_key:key))

let test_shutdown entry () =
  let original = Failure "turn failed during shutdown" in
  with_queued_turn entry ~first_error:original
    ~before_release:Session.start_draining (fun _ db calls _ outcome ->
      check_error original outcome;
      Alcotest.(check (list string))
        "shutdown does not run follow-up" [ "A" ] calls;
      Alcotest.(check int)
        "shutdown retains durable row" 1
        (Memory.queue_count ~db ~session_key:key))

let test_admin_stop entry () =
  let original = Failure "turn failed after admin stop" in
  with_queued_turn entry ~first_error:original
    ~before_release:(fun mgr ->
      let* stopped =
        Session.turn mgr ~key ~message:"/stop" ~user_group:"admin" ()
      in
      Alcotest.(check string)
        "admin stop acknowledged" Session.queued_message_response stopped;
      let _, _, interrupt = Hashtbl.find mgr.Session.sessions key in
      Alcotest.(check (option string))
        "admin stop interrupt set" (Some Agent.stop_interrupt_token) !interrupt;
      Lwt.return_unit)
    (fun _ db calls _ outcome ->
      check_error original outcome;
      Alcotest.(check (list string))
        "admin stop does not run follow-up" [ "A" ] calls;
      Alcotest.(check int)
        "admin stop retains durable row" 1
        (Memory.queue_count ~db ~session_key:key))

let test_retained_followup entry () =
  let original = Failure "turn A failed" in
  let attempts = ref 0 in
  with_queued_turn entry ~first_error:original ~extra_followup:true
    ~before_release:(fun mgr ->
      mgr.Session.queued_execution_adapter <-
        Some
          (fun ~key:_ ~message:_ ~run:_ ->
            incr attempts;
            Lwt.return
              (Session_types.Queue_retained "authorization unavailable"));
      Lwt.return_unit)
    (fun mgr db calls sent outcome ->
      check_error original outcome;
      Alcotest.(check int) "retained follow-up attempted once" 1 !attempts;
      Alcotest.(check (list string))
        "retained follow-up never reaches model" [ "A" ] calls;
      Alcotest.(check (list string)) "no retained response delivered" [] sent;
      Alcotest.(check int)
        "both durable rows retained" 2
        (Memory.queue_count ~db ~session_key:key);
      let pending =
        List.init 2 (fun _ ->
            Option.map
              (fun (q : Session_types.queued_message) -> q.message)
              (Session.take_next_queued_message mgr ~key))
      in
      Alcotest.(check (list (option string)))
        "retained head preserves FIFO" [ Some "B"; Some "C" ] pending)

let suite =
  List.concat_map
    (fun (entry, name) ->
      List.map
        (fun (description, test) ->
          Alcotest.test_case
            (description ^ " (" ^ name ^ ")")
            `Quick (test entry))
        [
          ("failed turn still drains queued follow-up", test_failed_turn);
          ( "before_drain failure still drains queued follow-up",
            test_before_drain );
          ("drained follow-up failure does not loop", test_followup_failure);
          ("successful turn drain unchanged", test_success);
          ( "durable row removed after failure-path drain succeeds",
            test_durable_row );
          ("retained current request does not drain", test_retained);
          ("cancellation retains follow-up", test_cancelled);
          ("shutdown retains follow-up", test_shutdown);
          ("admin stop retains follow-up after failure", test_admin_stop);
          ( "retained follow-up holds queue without looping",
            test_retained_followup );
        ])
    [ (Buffered, "buffered"); (Try, "try"); (Stream, "stream") ]
