open Lwt.Syntax
module S = Test_slack_standing_order_support
module F = Test_slack_standing_order_fixtures
module T = Slack_standing_order_types
module R = Slack_standing_order_recovery_store
module Sql = Slack_standing_order_sql
module C = Slack_standing_order_reconcile
module SO = Slack_standing_order_slack
module Ctx = Slack_standing_order_ctx
module E = Slack_standing_order_exec
module P = Test_slack_standing_order_provenance

let check = Alcotest.(check bool)
let eq = Alcotest.(check int)
let str = S.str
let obj = S.obj
let set = P.set

let page xs =
  S.ok
    [
      ("messages", `List xs);
      ("has_more", `Bool false);
      ("response_metadata", obj [ ("next_cursor", str "") ]);
    ]

let root_ts = "1700000010.000100"

let room =
  P.room
  |> set "date_start" (`Int 1700000010)
  |> set "date_end" `Null
  |> set "has_ended" (`Bool false)
  |> set "thread_root_ts" (str root_ts)
  |> set "canvas_thread_ts" (str root_ts)

let root =
  P.root
  |> set "ts" (str root_ts)
  |> set "room" room
  |> set "reply_count" (`Int 0)

type timer = { at : float; p : unit Lwt.t; u : unit Lwt.u }
type clock = { mutable now : float; mutable timers : timer list }

let wait clock ~seconds =
  let p, u = Lwt.task () in
  clock.timers <- { at = clock.now +. seconds; p; u } :: clock.timers;
  p

let rec settle n =
  if n = 0 then Lwt.return_unit
  else
    let* () = Lwt.pause () in
    settle (n - 1)

let step clock target =
  clock.now <- target;
  List.iter
    (fun t ->
      if t.at <= target && Lwt.is_sleeping t.p then Lwt.wakeup_later t.u ())
    clock.timers;
  settle 40

let rec drain ?(remaining = 1000) clock p =
  let* () = settle 40 in
  if not (Lwt.is_sleeping p) then p
  else if remaining = 0 then Alcotest.fail "fake-time work did not quiesce"
  else
    match
      List.filter (fun t -> Lwt.is_sleeping t.p) clock.timers
      |> List.sort (fun a b -> Float.compare a.at b.at)
    with
    | [] -> Alcotest.fail "work hung without a fake timer"
    | t :: _ ->
        let* () = step clock t.at in
        drain ~remaining:(remaining - 1) clock p

let rec advance clock target =
  let* () = settle 30 in
  match
    List.filter (fun t -> Lwt.is_sleeping t.p && t.at <= target) clock.timers
    |> List.sort (fun a b -> Float.compare a.at b.at)
  with
  | t :: _ ->
      let* () = step clock t.at in
      advance clock target
  | [] -> step clock target

type env = {
  f : F.env;
  order : T.order;
  clock : clock;
  ctx : Ctx.t;
  calls : (float * string * Yojson.Safe.t) list ref;
  classified : (string * string) list ref;
  answer : (string -> Yojson.Safe.t -> (int * Yojson.Safe.t) Lwt.t) ref;
  http : SO.http;
  roots : Yojson.Safe.t list ref;
  disk_path : string option;
}

let with_env ?(socket = false) ?(disk = false) ?(legacy = false) work =
  F.with_orders (fun f ->
      let* f, order =
        if legacy then (
          let db, order, _ =
            Test_slack_standing_order_receipt_import.legacy ()
          in
          Access_snapshot.init_schema db;
          Admin.init_schema db;
          Egress_audit.init_schema db;
          Setup_plan_apply.init_schema db;
          Room_activity_ledger.init_schema db;
          let card =
            Slack_standing_order_store.insert_card db ~kind:T.Pause_button
              ~order ~created_by:order.grantor_user_id ~now:S.now ()
          in
          Slack_standing_order_store.set_card_message_ts db ~card_id:card.id
            "1700000000.000200";
          let manager =
            Session.create ~config:(Session.get_config f.base.manager) ~db ()
          in
          Lwt.return ({ f with db; base = { f.base with db; manager } }, order))
        else
          let+ order, _ = Test_slack_standing_order_cards.confirmed f in
          (f, order)
      in
      check "ordinary readiness matches installation origin" true
        (R.rollout_readiness f.db ~order_id:order.id
        =
        if legacy then R.Denied R.Inventory_pending
        else R.Ready R.Fresh_no_legacy_work);
      eq "no inventory ceremony" 0
        (Sql.int_query f.db
           "SELECT count(*) FROM slack_standing_order_inventory_manifests" []);
      let disk_path =
        if disk then Some (Filename.temp_file "reconcile-owner" ".db") else None
      in
      let f =
        match disk_path with
        | None -> f
        | Some path ->
            Sql.exec f.db "VACUUM INTO ?" [ Sql.text path ];
            let db = Sqlite3.db_open path in
            let manager =
              Session.create ~config:(Session.get_config f.base.manager) ~db ()
            in
            { f with db; base = { f.base with db; manager } }
      in
      let clock = { now = S.now +. 100.; timers = [] } in
      (Slack_standing_order_store.clock := fun () -> clock.now);
      let old_limiter = !SO.configured_limiter in
      let limiter =
        Slack_read_limiter.create_with
          ~now:(fun () -> clock.now)
          ~wait:(wait clock)
      in
      let ctx =
        {
          (Ctx.make ~limiter ~wait:(wait clock)
             ~limit:(fun ~channel_id:_ -> Lwt.return_unit)
             ~accepting:(fun () -> true)
             ())
          with
          Ctx.now = (fun () -> clock.now);
        }
      in
      F.set_config f (fun cfg ->
          S.with_slack cfg (fun sc ->
              {
                sc with
                socket_mode = socket;
                app_token = (if socket then "xapp-fixture" else sc.app_token);
              }));
      let calls = ref [] and classified = ref [] and roots = ref [] in
      let default method_ params =
        Lwt.return
          ( 200,
            if method_ = "conversations.history" then page !roots
            else if method_ = "conversations.replies" then page []
            else !(f.answer) method_ params )
      in
      let answer = ref default in
      let http ~verb:_ ~token:_ ~method_ ~params =
        let phase =
          match Lwt.get SO.call_policy with
          | Some p when p.SO.priority = Slack_read_limiter.Maintenance ->
              "maintenance"
          | _ -> "live"
        in
        classified := !classified @ [ (phase, method_) ];
        let p = obj params in
        calls := !calls @ [ (clock.now, method_, p) ];
        let+ status, json = !answer method_ p in
        (status, Yojson.Safe.to_string json)
      in
      C.configure ~db:f.db ~manager:f.base.manager ~ctx;
      Lwt.finalize
        (fun () ->
          work
            {
              f;
              order;
              clock;
              ctx;
              calls;
              classified;
              answer;
              http;
              roots;
              disk_path;
            })
        (fun () ->
          let* () = C.stop () in
          let* () = E.stop () in
          List.iter (fun t -> Lwt.cancel t.p) clock.timers;
          SO.configure_limiter old_limiter;
          Option.iter
            (fun path ->
              ignore (Sqlite3.db_close f.db);
              Sys.remove path)
            disk_path;
          if legacy && disk_path = None then ignore (Sqlite3.db_close f.db);
          Lwt.return_unit))

let pass e =
  drain e.clock
    (C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
       e.order)

let covered e = R.covered e.f.db e.order.id e.order.epoch

let count e method_ =
  List.length (List.filter (fun (_, m, _) -> m = method_) !(e.calls))

let no_other_reads e =
  check "only grant and exact/window history requests" true
    (List.for_all
       (fun (_, m, _) ->
         List.mem m
           [
             "auth.test";
             "bots.info";
             "conversations.info";
             "conversations.members";
             "users.list";
             "conversations.history";
           ])
       !(e.calls))

let case name work = S.case name (fun () -> with_env work)

let event ?(id = "Ev-ready") event =
  Yojson.Safe.to_string
    (obj
       [
         ("type", str "event_callback");
         ("team_id", str "T123");
         ("api_app_id", str "A123");
         ("event_id", str id);
         ("event", event);
       ])

let observe e json =
  Slack_huddle_observer.observe ~manager:e.f.base.manager
    (Yojson.Safe.from_string (event json))

let with_transport e work =
  let meth uri = Filename.basename (Uri.path (Uri.of_string uri)) in
  Slack_transport.with_http
    {
      get =
        (fun ~uri ~headers:_ ->
          let params =
            Uri.query (Uri.of_string uri)
            |> List.map (fun (k, v) -> (k, str (List.hd v)))
          in
          e.http ~verb:"GET" ~token:"fixture" ~method_:(meth uri) ~params);
      post =
        (fun ~uri ~headers:_ ~body ->
          let params =
            match Yojson.Safe.from_string body with `Assoc xs -> xs | _ -> []
          in
          e.http ~verb:"POST" ~token:"fixture" ~method_:(meth uri) ~params);
    }
    work

let print_histogram phase calls =
  let methods =
    List.map (fun (_, m, _) -> m) calls |> List.sort_uniq String.compare
  in
  Printf.eprintf
    "Task6 actual HTTP installation=T123:A123 channel=C123 phase=%s total=%d %s\n\
     %!"
    phase (List.length calls)
    (String.concat ","
       (List.map
          (fun m ->
            m ^ "="
            ^ string_of_int
                (List.length (List.filter (fun (_, k, _) -> k = m) calls)))
          methods))
