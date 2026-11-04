(* Shared standing-order ingress fixture (no suite): phase 1's thread ingress
   on [thread_key], with [S.default_response env.world] answering every
   standing-order Slack method, the management tools registered on the
   session's registry, the store clock pinned to [S.now] for the run, and
   helpers to speak in Slack and to invoke the tools. Fake ids only. *)
module S = Test_slack_standing_order_support
module Store = Slack_standing_order_store

let thread_root = "100.000001"

let thread_key =
  Slack_thread_route.key
    { team = "T123"; app = "A123"; channel = "C123"; root = thread_root }

type env = {
  base : Test_slack_huddle_fixtures.fixture;
  calls : (string * Yojson.Safe.t) list ref;
      (** Every Slack call (method, params), oldest first. *)
  seen : Provider.message list list ref;  (** Every provider request. *)
  db : Sqlite3.db;
  posted : (Yojson.Safe.t * string) list ref;
      (** Accepted [chat.postMessage] calls (params, returned ts), oldest first.
      *)
  world : S.world;
  answer : (string -> Yojson.Safe.t -> Yojson.Safe.t) ref;
      (** The Slack answers; [override] layers on it. *)
  provider :
    (model:string ->
    messages:Provider.message list ->
    Provider.completion_response Lwt.t)
    ref;
  run : string -> Yojson.Safe.t -> string Lwt.t;
      (** Delivers one event callback with the given event id. *)
}

let set_config env f =
  let config = f (Session.get_config env.base.manager) in
  env.base.cfg := config;
  Session.update_config env.base.manager config

let default_orders (cfg : Runtime_config.t) =
  S.with_slack cfg (fun sc ->
      {
        sc with
        standing_orders =
          {
            Slack_config_types.orders_enabled = true;
            allowed_targets = [ S.claude_target ];
            creator_policy = Slack_config_types.So_creator_member;
          };
      })

let with_orders ?(config = Fun.id) f =
  Test_slack_threads.with_ingress ~key:thread_key
    (fun base calls handler seen provider run ->
      let world = S.world () and posted = ref [] in
      let ingress = !handler in
      let answer =
        ref (fun method_ params ->
            if List.mem_assoc method_ Slack_standing_order_slack.methods then
              S.default_response world method_ params
            else ingress method_ params)
      in
      (handler :=
         fun method_ params ->
           let response = !answer method_ params in
           if method_ = "chat.postMessage" && Slack_huddles.flag "ok" response
           then posted := !posted @ [ (params, S.param response "ts") ];
           response);
      let env =
        {
          base;
          calls;
          seen;
          db = base.db;
          posted;
          world;
          answer;
          provider;
          run = (fun id event -> run ~id event);
        }
      in
      set_config env (fun cfg -> config (default_orders cfg));
      let registry = Option.get base.manager.Session_core.tool_registry
      and current_config () = Session.get_config base.manager in
      Slack_standing_order_tools.register ~current_config registry;
      Slack_standing_order_change.register ~current_config registry;
      S.at S.now;
      Lwt.finalize
        (fun () -> f env)
        (fun () ->
          Store.clock := Unix.gettimeofday;
          Lwt.return_unit))

(* [f params] answers [method_]; other methods keep their previous answer. *)
let override env method_ f =
  let previous = !(env.answer) in
  env.answer :=
    fun called params ->
      if called = method_ then f params else previous called params

let seed env ?channel ?grantor ?state () =
  S.seed env.db ?channel ?grantor ?state ()

(* A fresh event id every call, and a fresh valid [ts] unless one is given:
   ingress dedups on both. *)
let say env ?(user = "U123") ?(channel = "C123") ?thread ?ts ?subtype text =
  incr env.world.seq;
  let n = !(env.world.seq) in
  let ts =
    match ts with Some ts -> ts | None -> Printf.sprintf "300.%06d" n
  in
  let event = Test_slack_threads.message ~channel ?thread ~ts ~user text in
  let event =
    match (subtype, event) with
    | Some s, `Assoc fields -> `Assoc (fields @ [ ("subtype", `String s) ])
    | _ -> event
  in
  env.run (Printf.sprintf "say%d" n) event

let fail_provider env =
  env.provider :=
    fun ~model:_ ~messages ->
      env.seen := messages :: !(env.seen);
      Lwt.fail_with "fixture: the LLM provider is down"

let texts env =
  List.map (fun (params, _) -> S.param params "text") !(env.posted)

let methods env = List.map fst !(env.calls)

let admin env id =
  Sql_util.exec_with_params env.db
    "INSERT INTO channel_admins(channel, sender_id) VALUES ('slack', ?)"
    [ Sqlite3.Data.TEXT id ]

(* ---- Card clicks ---- *)

(* A [block_actions] button press through [Slack.handle_event]. Both
   [container.message_ts] and [message.ts] are [message_ts]; [~action_ts:None]
   leaves the action's click time out. *)
let click env ~action_id ~card_id ~message_ts ?(user = "U123")
    ?(action_ts = Some "1700000000.000200") ?(team = "T123") ?(app = "A123")
    ?(channel = "C123") () =
  let action =
    S.obj
      ([
         ("type", S.str "button");
         ("action_id", S.str action_id);
         ("value", S.str card_id);
       ]
      @
      match action_ts with
      | Some ts -> [ ("action_ts", S.str ts) ]
      | None -> [])
  in
  let payload =
    S.obj
      [
        ("type", S.str "block_actions");
        ("api_app_id", S.str app);
        ("team", S.obj [ ("id", S.str team) ]);
        ("user", S.obj [ ("id", S.str user) ]);
        ("channel", S.obj [ ("id", S.str channel) ]);
        ( "container",
          S.obj
            [
              ("type", S.str "message");
              ("message_ts", S.str message_ts);
              ("channel_id", S.str channel);
            ] );
        ("message", S.obj [ ("ts", S.str message_ts) ]);
        ("actions", `List [ action ]);
      ]
  in
  Slack.handle_event
    ~config:(Option.get (Session.get_config env.base.manager).channels.slack)
    ~session_manager:env.base.manager
    (Yojson.Safe.to_string payload)

(* ---- Management tools ---- *)

(* [`Live]: a verified ingress turn on [thread_key] as [user] with a fresh
   Room_turn snapshot; [`Bare]: no turn at all; [`Drained]: the turn a queue
   drain starts; [`Steered]: a live turn invalidated by injected steering. *)
let invoke env ?(user = "U123") ?(mode = `Live) tool args =
  let registry = Option.get env.base.manager.Session_core.tool_registry in
  let tool =
    match Tool_registry.find registry tool with
    | Some t -> t
    | None -> Alcotest.fail ("F.invoke: no registered tool " ^ tool)
  in
  let call () =
    tool.Tool.invoke
      ~context:
        {
          Tool.default_context with
          session_key = Some thread_key;
          snapshot_id = env.base.agent.Agent.access_snapshot_id;
        }
      args
  in
  incr env.world.seq;
  let message_id = Printf.sprintf "400.%06d" !(env.world.seq) in
  let request f =
    Slack_huddle_context.with_request ~session_manager:env.base.manager
      ~key:thread_key ~channel_id:"C123" ~user_id:user ~message_id f
  in
  match mode with
  | `Bare -> call ()
  | `Live ->
      request (fun () ->
          env.base.snapshot ();
          call ())
  | `Drained ->
      request (fun () ->
          Slack_huddle_context.before_queued ~key:thread_key (Some message_id);
          env.base.snapshot ();
          call ())
  | `Steered ->
      request (fun () ->
          env.base.snapshot ();
          Tool.before_message_injection ();
          call ())

let create_args ?(args = []) () : Yojson.Safe.t =
  let defaults =
    [
      ("instruction", `String "Update the project docs.");
      ("target", `String "Claude");
    ]
  in
  `Assoc
    (args @ List.filter (fun (k, _) -> not (List.mem_assoc k args)) defaults)

(* [args] add to or replace the default instruction and target. *)
let create env ?user ?args () =
  invoke env ?user "slack_standing_order_create" (create_args ?args ())

(* The newest card of [kind] for the order. *)
let card_for env ~order_id ~kind =
  match
    Sql_util.query_rows env.db
      "SELECT id FROM slack_standing_order_cards WHERE order_id = ? AND kind = \
       ? ORDER BY created_at DESC, rowid DESC LIMIT 1"
      ~bind:(fun stmt ->
        Sql_util.bind_params stmt
          [
            Sqlite3.Data.TEXT order_id;
            Sqlite3.Data.TEXT
              (Slack_standing_order_types.card_kind_to_string kind);
          ])
      ~of_stmt:(fun stmt -> Sql_util.text_column stmt 0)
  with
  | [ id ] -> (
      match Store.find_card env.db id with
      | Some card -> card
      | None -> Alcotest.fail ("F.card_for: card does not decode " ^ id))
  | _ -> Alcotest.fail ("F.card_for: no card for " ^ order_id)
