open Lwt.Syntax

let obj fields = `Assoc fields
let str s = `String s
let arr xs = `List xs
let get key json = Yojson.Safe.Util.member key json
let ok fields = obj (("ok", `Bool true) :: fields)

let root =
  obj
    [
      ("ts", str "100.000001");
      ("subtype", str "huddle_thread");
      ( "room",
        obj
          [
            ("id", str "R123");
            ("thread_root_ts", str "100.000001");
            ("attached_file_ids", arr [ str "F123" ]);
          ] );
    ]

let discovery =
  obj
    [ ("channel_id", str "C123"); ("oldest", str "90"); ("latest", str "110") ]

let content =
  obj [ ("channel_id", str "C123"); ("huddle_ts", str "100.000001") ]

type fixture = {
  cfg : Runtime_config.t ref;
  calls : (string * string * (string * Yojson.Safe.t) list) list ref;
  handle : (string -> (string * Yojson.Safe.t) list -> Yojson.Safe.t) ref;
  status : int ref;
  invoke : ?args:Yojson.Safe.t -> string -> string Lwt.t;
  turn : (unit -> unit Lwt.t) -> unit Lwt.t;
  snapshot : unit -> unit;
  manager : Session.t;
  agent : Agent.t;
  db : Sqlite3.db;
  key : string;
}

let with_fixture ?(key = "slack:C123:U123") f =
  Test_helpers.with_temp_home (fun _ ->
      let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
      Access_snapshot.init_schema db;
      Admin.init_schema db;
      let slack = Test_slack.make_config () in
      let base = Runtime_config.default in
      let config =
        {
          base with
          channels = { base.channels with slack = Some slack };
          egress =
            {
              base.egress with
              default_allowlist =
                [
                  {
                    host = "slack.com";
                    path = Some "/api/*";
                    method_ = Some "GET";
                    action = Allow;
                    log_policy = No_log;
                  };
                ];
            };
        }
      in
      let cfg = ref config in
      let registry = Tool_registry.create () in
      let manager = Session.create ~config ~tool_registry:registry ~db () in
      let agent, _, _ = Session_core.get_or_create_locked manager ~key in
      let calls = ref [] in
      let handle =
        ref (fun method_ params ->
            match method_ with
            | "auth.test" ->
                ok [ ("team_id", str "T123"); ("user_id", str "UBOT") ]
            | "users.info" ->
                ok
                  [
                    ( "user",
                      obj
                        [
                          ("id", str "U123");
                          ("team_id", str "T123");
                          ("is_bot", `Bool false);
                          ("deleted", `Bool false);
                          ("is_restricted", `Bool false);
                          ("is_ultra_restricted", `Bool false);
                        ] );
                  ]
            | "conversations.info" ->
                ok
                  [
                    ( "channel",
                      obj
                        [
                          ("id", List.assoc "channel" params);
                          ("is_private", `Bool false);
                          ("is_member", `Bool true);
                          ("is_ext_shared", `Bool false);
                        ] );
                  ]
            | "conversations.members" ->
                ok [ ("members", arr [ str "U123"; str "UBOT" ]) ]
            | "conversations.history" ->
                ok [ ("messages", arr [ root ]); ("has_more", `Bool false) ]
            | "conversations.replies" ->
                ok
                  [
                    ( "messages",
                      arr
                        [
                          root;
                          obj
                            [
                              ("ts", str "101.1");
                              ("user", str "U123");
                              ( "text",
                                str "Thread discussion, not a transcript." );
                            ];
                        ] );
                  ]
            | "files.info" ->
                ok
                  [
                    ( "file",
                      obj
                        [
                          ("id", List.assoc "file" params);
                          ( "huddle_transcription",
                            obj
                              [
                                ("channel_id", str "C123");
                                ( "lines",
                                  arr
                                    [
                                      obj
                                        [
                                          ("contents", str "Actual spoken words");
                                        ];
                                    ] );
                              ] );
                        ] );
                  ]
            | _ -> Alcotest.fail ("unexpected Slack method " ^ method_))
      in
      let status = ref 200 in
      let http ~token ~method_ ~params =
        calls := (token, method_, params) :: !calls;
        Lwt.return (!status, Yojson.Safe.to_string (!handle method_ params))
      in
      Slack_huddle_tools.register ~http
        ~current_config:(fun () -> !cfg)
        registry;
      let invoke ?(args = discovery) name =
        match Tool_registry.find registry name with
        | None -> Alcotest.fail ("missing registered tool " ^ name)
        | Some tool ->
            tool.invoke
              ~context:
                {
                  Tool.default_context with
                  session_key = Some key;
                  snapshot_id = agent.Agent.access_snapshot_id;
                }
              args
      in
      let snapshot () =
        manager.config <- !cfg;
        let snap =
          Access_snapshot.create_and_persist ~db ~config:!cfg
            ~work_type:Access_snapshot.Room_turn ~session_key:key
            ~room_id:"C123" ~room_policy_decision:"allow" ()
        in
        agent.access_snapshot_id <- Some snap.id;
        agent.access_snapshot <- Some snap
      in
      let turn f =
        manager.config <- !cfg;
        Slack_huddle_context.with_request ~session_manager:manager ~key
          ~channel_id:"C123" ~user_id:"U123" ~message_id:"99.1" (fun () ->
            snapshot ();
            f ())
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          Lwt_main.run
            (f
               {
                 cfg;
                 calls;
                 handle;
                 status;
                 invoke;
                 turn;
                 snapshot;
                 manager;
                 agent;
                 db;
                 key;
               })))

let check_bool = Alcotest.(check bool)
let parse s = try Yojson.Safe.from_string s with _ -> Alcotest.fail s
let count key json = get key json |> Yojson.Safe.Util.to_list |> List.length
let contains = Test_helpers.string_contains

let expect_error result =
  check_bool result true (String.starts_with ~prefix:"Error:" result)

let no_content f =
  check_bool "no history, replies or files read" true
    (List.for_all
       (fun (_, m, _) ->
         not
           (List.mem m
              [ "conversations.history"; "conversations.replies"; "files.info" ]))
       !(f.calls))

let update json fields =
  match json with
  | `Assoc xs ->
      obj (fields @ List.filter (fun (k, _) -> not (List.mem_assoc k fields)) xs)
  | _ -> assert false

let override f method_ handler =
  let original = !(f.handle) in
  f.handle :=
    fun m p -> if m = method_ then handler (original m p) p else original m p

let update_slack f change =
  let config = !(f.cfg) in
  let slack = Option.get config.channels.slack in
  f.cfg :=
    {
      config with
      channels = { config.channels with slack = Some (change slack) };
    }

let admin f =
  ignore
    (Sqlite3.exec f.db
       "INSERT INTO channel_admins(channel,sender_id) VALUES ('slack','U123')")

let at_target args = update args [ ("channel_id", str "C999") ]

let call_content f =
  let* result = f.invoke ~args:content "slack_huddle_content" in
  Lwt.return (parse result)
