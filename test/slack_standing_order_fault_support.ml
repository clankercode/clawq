(* External Slack state lives independently of every daemon DB/Session. The
   checkpoint contains server objects only, never worker rows or authority. *)
open Lwt.Syntax
module S = Test_slack_standing_order_support
module P = Test_slack_standing_order_provenance
module F = Test_slack_standing_order_fixtures
module H = Test_slack_standing_order_reconcile_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module R = Slack_standing_order_recovery_store
module C = Slack_standing_order_reconcile
module E = Slack_standing_order_exec
module SO = Slack_standing_order_slack
module Sql = Slack_standing_order_sql

let str = S.str
let obj = S.obj
let set = P.set
let get = P.get
let field = Slack_huddles.field
let check = Alcotest.(check bool)
let eq = Alcotest.(check int)

type point =
  | Allocate
  | Binary_upload
  | Complete_before_share
  | Complete_after_share
  | Share_lookup
  | Notice_before_post
  | Notice_after_post
  | Pause_page
  | History_page
  | Retention_delete
  | Retention_note

type outcome = Timeout | Status_500 | Crash

let points =
  [
    Allocate;
    Binary_upload;
    Complete_before_share;
    Complete_after_share;
    Share_lookup;
    Notice_before_post;
    Notice_after_post;
    Pause_page;
    History_page;
    Retention_delete;
    Retention_note;
  ]

let point_name = function
  | Allocate -> "allocate"
  | Binary_upload -> "binary_upload"
  | Complete_before_share -> "complete_before_share"
  | Complete_after_share -> "complete_after_share"
  | Share_lookup -> "share_lookup"
  | Notice_before_post -> "notice_before_post"
  | Notice_after_post -> "notice_after_post"
  | Pause_page -> "pause_page"
  | History_page -> "history_page"
  | Retention_delete -> "retention_delete"
  | Retention_note -> "retention_note"

let outcome_name = function
  | Timeout -> "timeout"
  | Status_500 -> "500"
  | Crash -> "crash"

let assert_relay_bound count =
  Alcotest.(check bool) "at most two relay messages" true (count <= 2)

type instance = {
  order_id : string;
  room_id : string;
  date_start : string;
  root_ts : string;
}

type message = {
  json : Yojson.Safe.t;
  instance : instance option;
  kind : string;
  visible_at : float;
}

type allocation = { id : string; uploaded : bool }

type request = {
  at : float;
  method_ : string;
  ids : (string * string) list;
  lane : string;
}

type state = {
  allocations : allocation list;
  messages : message list;
  deleted : string list;
  seq : int;
  requests : request list;
}

type server = {
  path : string;
  mutable state : state;
  clock : H.clock;
  world : S.world;
  mutable roots : Yojson.Safe.t list;
  mutable ready_ts : string;
  mutable ready : bool;
  mutable transcript : bool;
  mutable crash_witness : string option;
  mutable faults : (point * outcome * bool) list;
  mutable hits : (point * outcome * bool) list;
  mutable calls : request list;
  mutable instances : instance list;
  mutable complete_shares : bool;
  mutable visibility_delay : float;
  mutable hook : string -> Yojson.Safe.t -> unit Lwt.t;
  mutable override : string -> Yojson.Safe.t -> (int * Yojson.Safe.t) option;
  mutable inspect : string -> unit;
}

let checkpoint s =
  let ch = open_out_bin s.path in
  Fun.protect
    ~finally:(fun () -> close_out ch)
    (fun () ->
      Marshal.to_channel ch
        (s.state, s.roots, s.instances, s.ready, s.transcript, s.ready_ts)
        [])

let reload s =
  let ch = open_in_bin s.path in
  let state, roots, instances, ready, transcript, ready_ts =
    Fun.protect
      ~finally:(fun () -> close_in ch)
      (fun () -> Marshal.from_channel ch)
  in
  s.state <- state;
  s.roots <- roots;
  s.instances <- instances;
  s.ready <- ready;
  s.transcript <- transcript;
  s.ready_ts <- ready_ts;
  s.calls <- s.state.requests

let create () =
  let s =
    {
      path = Filename.temp_file "fault-slack-server" ".bin";
      state =
        {
          allocations = [];
          messages = [];
          deleted = [];
          seq = 0;
          requests = [];
        };
      clock = { H.now = S.now +. 100.; timers = [] };
      world = S.world ();
      roots = [];
      ready_ts = P.ready_ts;
      ready = true;
      transcript = true;
      crash_witness = None;
      faults = [];
      hits = [];
      calls = [];
      instances = [];
      complete_shares = true;
      visibility_delay = 0.;
      hook = (fun _ _ -> Lwt.return_unit);
      override = (fun _ _ -> None);
      inspect = (fun _ -> ());
    }
  in
  checkpoint s;
  s

let with_server f =
  let s = create () in
  Fun.protect ~finally:(fun () -> Sys.remove s.path) (fun () -> f s)

let inject s point outcome = s.faults <- s.faults @ [ (point, outcome, false) ]

let inject_after s point outcome =
  s.faults <- s.faults @ [ (point, outcome, true) ]

let trip s point after =
  match s.faults with
  | (p, o, a) :: rest when p = point && a = after -> (
      s.faults <- rest;
      s.hits <- (p, o, a) :: s.hits;
      match o with
      | Timeout -> Lwt.fail Lwt_unix.Timeout
      | Crash -> (
          match s.crash_witness with
          | None -> invalid_arg "Crash injection requires process_boundary"
          | Some path ->
              let ch = open_out_bin path in
              Marshal.to_channel ch s.clock.now [];
              close_out ch;
              Unix._exit 77)
      | Status_500 -> Lwt.return (Some (500, obj [])))
  | _ -> Lwt.return_none

let fault s p after next =
  let* result = trip s p after in
  match result with Some r -> Lwt.return r | None -> next ()

let fresh_ts s =
  s.state <- { s.state with seq = s.state.seq + 1 };
  Printf.sprintf "%.0f.%06d" s.clock.now s.state.seq

let relay_count s ~order_id ~room_id ~date_start =
  List.length
    (List.filter
       (fun m ->
         match m.instance with
         | Some i ->
             i.order_id = order_id && i.room_id = room_id
             && i.date_start = date_start
         | None -> false)
       s.state.messages)

let bounds s =
  List.iter
    (fun i ->
      assert_relay_bound
        (relay_count s ~order_id:i.order_id ~room_id:i.room_id
           ~date_start:i.date_start))
    s.instances;
  let markers =
    List.filter_map
      (fun m ->
        if m.kind = "notice" then
          match get "blocks" m.json with
          | `List blocks ->
              List.find_map
                (fun b ->
                  let id = field "block_id" b in
                  if String.starts_with ~prefix:"clawq-catchup/1:" id then
                    Some id
                  else None)
                blocks
          | _ -> None
        else None)
      s.state.messages
  in
  List.iter
    (fun marker ->
      eq "at most one notice per exact batch marker" 1
        (List.length (List.filter (( = ) marker) markers)))
    markers

let record s method_ params =
  s.inspect method_;
  let lane =
    match Lwt.get SO.call_policy with
    | Some p when p.SO.priority = Slack_read_limiter.Maintenance ->
        "maintenance"
    | _ -> "live"
  in
  let ids =
    List.filter_map
      (fun k ->
        let v = field k params in
        if v = "" then None else Some (k, v))
      [
        "channel";
        "file";
        "ts";
        "timestamp";
        "user";
        "bot";
        "oldest";
        "latest";
        "cursor";
      ]
  in
  let request = { at = s.clock.now; method_; ids; lane } in
  s.calls <- s.calls @ [ request ];
  s.state <- { s.state with requests = s.state.requests @ [ request ] };
  checkpoint s;
  bounds s

let visible s =
  List.filter (fun m -> m.visible_at <= s.clock.now) s.state.messages

let ready_for ?(ts = P.ready_ts) root =
  P.ready
  |> set "ts" (str ts)
  |> set "files" (get "files" root)
  |> set "thread_ts" (get "ts" root)
  |> set "channel" (str "C123")
  |> set "type" (str "message")

let root = H.root

let ended_root =
  root
  |> set "room"
       (get "room" root
       |> set "has_ended" (`Bool true)
       |> set "date_end" (`Int 1700000030))

let event_root root =
  root
  |> set "channel" (str "C123")
  |> set "type" (str "message")
  |> set "user" (str "USLACKBOT")

let canvas s =
  P.canvas
  |> set "huddle_transcript_file_id"
       (if s.transcript then str "FTRANS" else `Null)
  |> set "shares"
       (obj
          [
            ( "private",
              obj [ ("C123", `List [ P.share H.root_ts; P.share s.ready_ts ]) ]
            );
          ])

let transcript =
  P.transcript
  |> set "huddle_transcription"
       (obj
          [
            ("channel_id", str "C123");
            ("date_start", `Int 1700000010);
            ( "lines",
              `List
                [
                  obj
                    [
                      ("start_time_ms", `Int 1000);
                      ("user_id", str "U123");
                      ("contents", str "MEETING_PRIVATE_ONE");
                    ];
                  obj
                    [
                      ("start_time_ms", `Int 2000);
                      ("user_id", str "U456");
                      ("contents", str "MEETING_PRIVATE_TWO");
                    ];
                ] );
          ])

let shares ts =
  obj [ ("private", obj [ ("C123", `List [ obj [ ("ts", str ts) ] ]) ]) ]

let save_message s m =
  s.state <- { s.state with messages = s.state.messages @ [ m ] };
  checkpoint s;
  bounds s

let respond s method_ p =
  match s.override method_ p with
  | Some r -> Lwt.return r
  | None -> (
      match method_ with
      | "conversations.history" ->
          let point =
            if get "limit" p = `Int 1 then History_page else Pause_page
          in
          fault s point false (fun () ->
              let messages =
                if get "limit" p = `Int 1 then
                  List.filter (fun r -> get "ts" r = get "latest" p) s.roots
                else s.roots @ List.map (fun m -> m.json) (visible s)
              in
              fault s point true (fun () -> Lwt.return (200, H.page messages)))
      | "conversations.replies" ->
          let messages =
            match
              List.find_opt (fun r -> field "ts" r = field "ts" p) s.roots
            with
            | Some r ->
                if s.ready && Slack_huddles.flag "has_ended" (get "room" r) then
                  [ r; ready_for ~ts:s.ready_ts r ]
                else [ r ]
            | None ->
                List.filter_map
                  (fun m ->
                    if field "thread_ts" m.json = field "ts" p then Some m.json
                    else None)
                  (visible s)
          in
          Lwt.return (200, H.page messages)
      | "files.info" ->
          let id = field "file" p in
          if List.mem id s.state.deleted then
            Lwt.return
              (200, obj [ ("ok", `Bool false); ("error", str "file_not_found") ])
          else if id = "FCANVAS" then
            Lwt.return (200, S.ok [ ("file", canvas s) ])
          else if id = "FTRANS" then
            Lwt.return (200, S.ok [ ("file", transcript) ])
          else
            fault s Share_lookup false (fun () ->
                let m =
                  List.find_opt
                    (fun m ->
                      match get "files" m.json with
                      | `List xs -> List.exists (fun f -> field "id" f = id) xs
                      | _ -> false)
                    (visible s)
                in
                let json =
                  obj
                    ([ ("id", str id); ("user", str "UBOT") ]
                    @
                    match m with
                    | Some m -> [ ("shares", shares (field "ts" m.json)) ]
                    | None ->
                        [
                          ( "shares",
                            obj [ ("public", obj []); ("private", obj []) ] );
                        ])
                in
                fault s Share_lookup true (fun () ->
                    Lwt.return (200, S.ok [ ("file", json) ])))
      | "canvases.getContent" ->
          Lwt.return (200, S.ok [ ("content", str "MEETING_PRIVATE_NOTES") ])
      | "files.getUploadURLExternal" ->
          fault s Allocate false (fun () ->
              let id = "FUPLOAD" ^ string_of_int (s.state.seq + 1) in
              ignore (fresh_ts s);
              s.state <-
                {
                  s.state with
                  allocations =
                    s.state.allocations @ [ { id; uploaded = false } ];
                };
              checkpoint s;
              fault s Allocate true (fun () ->
                  Lwt.return
                    ( 200,
                      S.ok
                        [
                          ("file_id", str id);
                          ( "upload_url",
                            str
                              ("https://files.slack.com/upload/SECRET_CAP_" ^ id)
                          );
                        ] )))
      | "files.completeUploadExternal" ->
          fault s Complete_before_share false (fun () ->
              let ids =
                match get "files" p with
                | `List xs -> List.map (field "id") xs
                | _ -> []
              in
              check "complete only allocated and uploaded files" true
                (ids <> []
                && List.for_all
                     (fun id ->
                       List.exists
                         (fun a -> a.id = id && a.uploaded)
                         s.state.allocations)
                     ids);
              let ts = fresh_ts s in
              let instance =
                List.find_opt
                  (fun i ->
                    String_util.contains
                      (field "initial_comment" p)
                      ("p"
                      ^ String.concat "" (String.split_on_char '.' i.root_ts)))
                  s.instances
              in
              check "real complete bound to registered huddle URL" true
                (instance <> None);
              check "relay is top level" true (get "thread_ts" p = `Null);
              let json =
                obj
                  [
                    ("ts", str ts);
                    ("user", str "UBOT");
                    ("bot_id", str "BOWN");
                    ("text", get "initial_comment" p);
                    ( "files",
                      `List (List.map (fun id -> obj [ ("id", str id) ]) ids) );
                  ]
              in
              save_message s
                {
                  json;
                  instance;
                  kind = "relay";
                  visible_at = s.clock.now +. s.visibility_delay;
                };
              let* () = s.hook "shared" p in
              fault s Complete_after_share false (fun () ->
                  fault s Complete_after_share true (fun () ->
                      fault s Complete_before_share true (fun () ->
                          Lwt.return
                            ( 200,
                              S.ok
                                [
                                  ( "files",
                                    `List
                                      (List.map
                                         (fun id ->
                                           obj
                                             ([ ("id", str id) ]
                                             @
                                             if s.complete_shares then
                                               [ ("shares", shares ts) ]
                                             else []))
                                         ids) );
                                ] )))))
      | "chat.postMessage" ->
          let text = field "text" p in
          let notice =
            String_util.contains
              (Yojson.Safe.to_string (get "blocks" p))
              "clawq-catchup"
          in
          let retention =
            String_util.contains text "Clawq removed its relay files"
          in
          let point =
            if retention then Retention_note else Notice_before_post
          in
          fault s point false (fun () ->
              let ts = fresh_ts s in
              save_message s
                {
                  json =
                    p
                    |> set "ts" (str ts)
                    |> set "user" (str "UBOT")
                    |> set "bot_id" (str "BOWN");
                  instance = None;
                  kind =
                    (if retention then "cleanup"
                     else if notice then "notice"
                     else "post");
                  visible_at = s.clock.now +. s.visibility_delay;
                };
              fault s point true (fun () ->
                  fault s Notice_after_post false (fun () ->
                      fault s Notice_after_post true (fun () ->
                          Lwt.return
                            ( 200,
                              S.ok [ ("ts", str ts); ("channel", str "C123") ]
                            )))))
      | "files.delete" ->
          fault s Retention_delete false (fun () ->
              s.state <-
                { s.state with deleted = field "file" p :: s.state.deleted };
              checkpoint s;
              fault s Retention_delete true (fun () ->
                  Lwt.return (200, S.ok [])))
      | "reactions.get" ->
          Lwt.return
            ( 200,
              S.ok
                [
                  ("message", obj [ ("reactions", `List []) ]);
                  ("file", obj [ ("reactions", `List []) ]);
                ] )
      | _ -> Lwt.return (200, S.default_response s.world method_ p))

let server_http s ~verb:_ ~token:_ ~method_ ~params =
  let p = obj params in
  record s method_ p;
  let* () = s.hook method_ p in
  let+ status, json = respond s method_ p in
  bounds s;
  (status, Yojson.Safe.to_string json)

let server_upload s (r : Slack_standing_order_upload.request) =
  record s "raw_upload" (obj []);
  check "raw host pinned" true (Slack_standing_order_upload.pinned r.uri);
  check "raw has no Authorization" false
    (List.exists
       (fun (k, _) -> String.lowercase_ascii k = "authorization")
       r.headers);
  let* result =
    fault s Binary_upload false (fun () ->
        let id =
          List.find_opt
            (fun a -> String_util.contains r.uri a.id)
            s.state.allocations
          |> Option.get
        in
        s.state <-
          {
            s.state with
            allocations =
              List.map
                (fun a ->
                  if a.id = id.id then { a with uploaded = true } else a)
                s.state.allocations;
          };
        checkpoint s;
        let* () = s.hook "raw_upload" (obj []) in
        fault s Binary_upload true (fun () -> Lwt.return (200, obj [])))
  in
  Lwt.return
    Slack_standing_order_upload.{ status = fst result; headers = []; body = "" }

let transport s work =
  let http = server_http s in
  SO.with_read_wait (H.wait s.clock) (fun () ->
      Slack_transport.with_http
        {
          get =
            (fun ~uri ~headers:_ ->
              let u = Uri.of_string uri in
              let params =
                Uri.query u
                |> List.map (fun (k, v) ->
                    let v = List.hd v in
                    (k, if k = "limit" then `Int (int_of_string v) else str v))
              in
              http ~verb:"GET" ~token:"fixture"
                ~method_:(Filename.basename (Uri.path u))
                ~params);
          post =
            (fun ~uri ~headers:_ ~body ->
              let params =
                match Yojson.Safe.from_string body with
                | `Assoc xs -> xs
                | _ -> []
              in
              http ~verb:"POST" ~token:"fixture"
                ~method_:(Filename.basename (Uri.path (Uri.of_string uri)))
                ~params);
        }
        work)

type env = {
  server : server;
  path : string;
  mutable db : Sqlite3.db;
  mutable manager : Session.t;
  mutable order : T.order;
  mutable ctx : Slack_standing_order_ctx.t;
  accepting : bool ref;
  mutable closed : bool;
}

let context s accepting =
  let limiter =
    Slack_read_limiter.create_with
      ~now:(fun () -> s.clock.now)
      ~wait:(H.wait s.clock)
  in
  {
    (Slack_standing_order_ctx.make ~limiter ~wait:(H.wait s.clock)
       ~accepting:(fun () -> !accepting)
       ~limit:(fun ~channel_id:_ -> Lwt.return_unit)
       ())
    with
    now = (fun () -> s.clock.now);
  }

let configure e = C.configure ~db:e.db ~manager:e.manager ~ctx:e.ctx

let start e =
  C.start ~db:e.db ~manager:e.manager;
  E.start ~ctx:e.ctx ~db:e.db ~manager:e.manager ~http:(server_http e.server)
    ~upload_http:(server_upload e.server) ()

let stop () = Lwt.finalize C.stop E.stop

let pass e =
  H.drain e.server.clock
    (C.pass_with ~ctx:e.ctx ~http:(server_http e.server) ~db:e.db
       ~manager:e.manager
       (Option.value (Store.find e.db e.order.id) ~default:e.order))

let worker e =
  H.drain e.server.clock
    (E.pass ~ctx:e.ctx ~db:e.db ~manager:e.manager ~http:(server_http e.server)
       ~upload_http:(server_upload e.server) ())

let advance e seconds =
  let* () = H.advance e.server.clock (e.server.clock.now +. seconds) in
  bounds e.server;
  Lwt.return_unit

let observe e json =
  Slack_huddle_observer.observe ~manager:e.manager
    (Yojson.Safe.from_string (H.event json));
  E.wake ()

let install_instance e root =
  let r = get "room" root in
  let date_start =
    match get "date_start" r with
    | `Int n -> string_of_int n
    | `String s -> s
    | _ -> assert false
  in
  let i =
    {
      order_id = e.order.id;
      room_id = field "id" r;
      date_start;
      root_ts = field "ts" root;
    }
  in
  e.server.instances <-
    i :: List.filter (fun old -> old <> i) e.server.instances;
  i

let fires e = Store.list_fires e.db ~order_id:e.order.id ~limit:100

let one e =
  match fires e with [ f ] -> f | _ -> Alcotest.fail "expected one real fire"

let count s m = List.length (List.filter (fun r -> r.method_ = m) s.calls)

let relays e =
  List.length (List.filter (fun m -> m.kind = "relay") e.server.state.messages)

let until e label predicate seconds =
  let rec loop n =
    if predicate () then Lwt.return_unit
    else if n = 0 then (
      Printf.eprintf "T8_TIMEOUT %s now=%.0f covered=%b calls=%s fires=%s\n%!"
        label e.server.clock.now
        (R.covered e.db e.order.id e.order.epoch)
        (String.concat "," (List.map (fun r -> r.method_) e.server.calls))
        (String.concat ","
           (List.map
              (fun (f : T.fire) ->
                T.fire_state_to_string f.state
                ^ ":"
                ^ Option.value f.reason ~default:"-")
              (fires e)));
      Printf.eprintf "T8_CANDIDATES %s ready=%s allowed=%s\n%!"
        (String.concat ","
           (Sql.query e.db
              "SELECT origin || ':' || COALESCE(batch_id,'-') FROM \
               slack_standing_order_recovery_candidates"
              [] (fun s -> Sql_util.text_column s 0)))
        (R.readiness_name (R.rollout_readiness e.db ~order_id:e.order.id))
        (String.concat ","
           (List.map
              (fun (f : T.fire) ->
                string_of_bool
                  (R.relay_allowed e.db ~fire_id:f.id ~now:e.server.clock.now))
              (fires e)));
      Alcotest.fail label)
    else
      let* () = advance e 1. in
      loop (n - 1)
  in
  loop seconds

let live e =
  e.server.roots <- [ ended_root ];
  ignore (install_instance e ended_root);
  observe e (event_root ended_root);
  observe e (ready_for ~ts:e.server.ready_ts ended_root)

let restart e =
  let config = Session.get_config e.manager in
  let* () = stop () in
  List.iter (fun t -> Lwt.cancel t.H.p) e.server.clock.timers;
  e.server.clock.timers <- [];
  check "daemon DB really closes" true (Sqlite3.db_close e.db);
  e.closed <- true;
  reload e.server;
  e.db <- Sqlite3.db_open e.path;
  e.closed <- false;
  e.manager <- Session.create ~config ~db:e.db ();
  e.ctx <- context e.server e.accepting;
  configure e;
  Lwt.return_unit

let with_env ?server:provided work =
  let server =
    match provided with Some server -> server | None -> create ()
  in
  let old_limiter = !SO.configured_limiter in
  SO.configure_limiter
    (Some
       (Slack_read_limiter.create_with
          ~now:(fun () -> server.clock.now)
          ~wait:(H.wait server.clock)));
  F.with_orders (fun f ->
      let path = Filename.temp_file "fault-daemon" ".db" in
      let accepting = ref true in
      (Store.clock := fun () -> server.clock.now);
      F.set_config f (fun cfg ->
          {
            cfg with
            egress =
              {
                cfg.egress with
                default_allowlist =
                  {
                    Runtime_config.host = "files.slack.com";
                    path = Some "/upload/*";
                    method_ = Some "POST";
                    action = Allow;
                    log_policy = No_log;
                  }
                  :: cfg.egress.default_allowlist;
              };
          });
      transport server (fun () ->
          let* order, _ =
            H.drain server.clock (Test_slack_standing_order_cards.confirmed f)
          in
          check "ordinary confirmation automatically bound fresh proof" true
            (R.rollout_readiness f.db ~order_id:order.id
            = R.Ready R.Fresh_no_legacy_work);
          eq "no reviewed import ceremony" 0
            (Sql.int_query f.db
               "SELECT count(*) FROM slack_standing_order_inventory_manifests"
               []);
          Sql.exec f.db "VACUUM INTO ?" [ Sql.text path ];
          let db = Sqlite3.db_open path in
          let manager =
            Session.create ~config:(Session.get_config f.base.manager) ~db ()
          in
          let e =
            {
              server;
              path;
              db;
              manager;
              order;
              ctx = context server accepting;
              accepting;
              closed = false;
            }
          in
          configure e;
          Lwt.finalize
            (fun () -> work e)
            (fun () ->
              let* () =
                Lwt.catch
                  (fun () -> H.drain server.clock (stop ()))
                  (fun exn ->
                    match R.rollout_readiness e.db ~order_id:e.order.id with
                    | R.Denied _ ->
                        Printf.eprintf "T8_EXPECTED_INVALID_PROOF_STOP %s\n%!"
                          (Printexc.to_string exn);
                        Lwt.return_unit
                    | R.Ready _ -> Lwt.fail exn)
              in
              List.iter (fun t -> Lwt.cancel t.H.p) server.clock.timers;
              SO.configure_limiter old_limiter;
              if not e.closed then ignore (Sqlite3.db_close e.db);
              Sys.remove path;
              if Option.is_none provided then Sys.remove server.path;
              Lwt.return_unit)))

let case name work = S.case name (fun () -> with_env work)

(* The parent never executes the child's worker continuation. _exit skips
   every transport catch, SQL continuation and shutdown finalizer. *)
let process_boundary ?(prime = false) e setup work =
  let config = Session.get_config e.manager in
  let* () = H.drain e.server.clock (stop ()) in
  check "parent closes SQLite before process fork" true (Sqlite3.db_close e.db);
  e.closed <- true;
  let reopen () =
    e.db <- Sqlite3.db_open e.path;
    e.closed <- false;
    e.manager <- Session.create ~config ~db:e.db ();
    e.ctx <- context e.server e.accepting;
    configure e
  in
  let witness = Filename.temp_file "fault-crash-boundary" ".bin" in
  let pid = Unix.fork () in
  if pid = 0 then (
    (* A fresh daemon does not inherit the parent's PRNG snapshot IDs. *)
    Random.self_init ();
    let die () =
      let ch = open_out_bin witness in
      Marshal.to_channel ch e.server.clock.now [];
      close_out ch;
      Unix._exit 77
    in
    reopen ();
    Lwt.async (fun () ->
        let* () =
          if prime then (
            let roots = e.server.roots in
            e.server.roots <- [];
            let* () = pass e in
            check "child establishes verified coverage before live fault" true
              (R.covered e.db e.order.id e.order.epoch);
            e.server.roots <- roots;
            Lwt.return_unit)
          else Lwt.return_unit
        in
        setup witness die;
        let* () = work () in
        Unix._exit 78);
    fst (Lwt.wait ()))
  else
    let _, status = Unix.waitpid [] pid in
    check "crash exits process at named durable boundary" true
      (status = Unix.WEXITED 77);
    let ch = open_in_bin witness in
    let now = Marshal.from_channel ch in
    close_in ch;
    Sys.remove witness;
    e.server.clock.now <- now;
    reload e.server;
    reopen ();
    bounds e.server;
    Lwt.return_unit

let fault_run e point outcome after work =
  if outcome <> Crash then (
    (if after then inject_after else inject) e.server point outcome;
    work ())
  else
    let prime =
      List.mem point
        [
          Allocate;
          Binary_upload;
          Complete_before_share;
          Complete_after_share;
          Share_lookup;
        ]
    in
    let* () =
      process_boundary ~prime e
        (fun witness _ ->
          e.server.crash_witness <- Some witness;
          (if after then inject_after else inject) e.server point Crash)
        work
    in
    e.server.hits <- (point, Crash, after) :: e.server.hits;
    Lwt.return_unit

let crash_state e state =
  e.server.roots <- [];
  ignore (install_instance e ended_root);
  process_boundary ~prime:true e
    (fun _ die ->
      if state = T.Posting_unknown then
        inject e.server Complete_after_share Timeout;
      e.server.hook <-
        (fun _ _ ->
          if List.exists (fun (f : T.fire) -> f.state = state) (fires e) then
            die ()
          else Lwt.return_unit))
    (fun () ->
      start e;
      let* () = advance e 3. in
      live e;
      advance e 90.)
