open Lwt.Syntax
module S = Test_slack_standing_order_support
module RS = Test_slack_standing_order_recovery_store
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module Sql = Slack_standing_order_sql
module R = Slack_standing_order_recovery_store
module C = Slack_standing_order_catchup
module SO = Slack_standing_order_slack
module Ctx = Slack_standing_order_ctx
module Render = Slack_standing_order_render

let check = Alcotest.(check bool)
let eq = Alcotest.(check int)
let str = S.str
let obj = S.obj
let get = Slack_huddles.get
let field = Slack_huddles.field
let ok = RS.ok

let page ?(cursor = "") ?(more = false) messages =
  S.ok
    [
      ("messages", `List messages);
      ("has_more", `Bool more);
      ("response_metadata", obj [ ("next_cursor", str cursor) ]);
    ]

let set key v = function
  | `Assoc xs -> `Assoc ((key, v) :: List.remove_assoc key xs)
  | _ -> assert false

type env = {
  db : Sqlite3.db;
  other : Sqlite3.db;
  path : string;
  manager : Session.t;
  order : T.order;
  owner : R.owner;
  gap : R.gap_token;
  coverage : R.coverage;
  clock : float ref;
  calls : (string * Yojson.Safe.t) list ref;
  shared : Yojson.Safe.t list ref;
  http : SO.http;
  answer : (string -> Yojson.Safe.t -> (int * Yojson.Safe.t) Lwt.t) ref;
  ctx : Ctx.t;
}

let with_env f =
  let original = S.fresh_db () in
  let order = RS.new_order original "so_catchup" in
  let card =
    Store.insert_card original ~kind:T.Pause_button ~order ~created_by:"U123"
      ~now:S.now ()
  in
  Store.set_card_message_ts original ~card_id:card.id "1700000000.000200";
  let path = Filename.temp_file "catchup" ".db" in
  Sql.exec original "VACUUM INTO ?" [ Sql.text path ];
  ignore (Sqlite3.db_close original);
  let db = Sqlite3.db_open path and other = Sqlite3.db_open path in
  let old_limiter = !SO.configured_limiter in
  SO.configure_limiter None;
  Lwt.finalize
    (fun () ->
      let gap, owner, coverage = RS.scan db order in
      RS.finish db order gap owner coverage;
      let clock = ref (S.now +. 100.) in
      (Store.clock := fun () -> !clock);
      check "owner renewed" true
        (R.renew db ~order_id:order.id ~owner ~now:!clock);
      let manager = Session.create ~config:(S.config ()) ~db () in
      let calls = ref [] and shared = ref [] and world = S.world () in
      let default method_ params =
        let response =
          match method_ with
          | "conversations.history" -> page !shared
          | "chat.postMessage" ->
              let ts =
                Printf.sprintf "%.6f"
                  (!clock +. (0.001 *. float_of_int (List.length !shared + 1)))
              in
              if get "blocks" params <> `Null then
                shared :=
                  !shared
                  @ [
                      set "ts" (str ts)
                        (set "user" (str "UBOT")
                           (set "bot_id" (str "BOWN") params));
                    ];
              S.ok [ ("channel", str "C123"); ("ts", str ts) ]
          | _ -> S.default_response world method_ params
        in
        Lwt.return (200, response)
      in
      let answer = ref default in
      let http ~verb:_ ~token:_ ~method_ ~params =
        let p = obj params in
        calls := !calls @ [ (method_, p) ];
        let+ status, json = !answer method_ p in
        (status, Yojson.Safe.to_string json)
      in
      let limiter =
        Slack_read_limiter.create_with
          ~now:(fun () -> !clock)
          ~wait:(fun ~seconds ->
            clock := !clock +. seconds;
            Lwt.return_unit)
      in
      let ctx =
        {
          (Ctx.make ~limiter
             ~accepting:(fun () -> true)
             ~limit:(fun ~channel_id:_ -> Lwt.return_unit)
             ())
          with
          Ctx.now = (fun () -> !clock);
        }
      in
      let* () =
        f
          {
            db;
            other;
            path;
            manager;
            order;
            owner;
            gap;
            coverage;
            clock;
            calls;
            shared;
            http;
            answer;
            ctx;
          }
      in
      Alcotest.(check (list string)) "known mock methods" [] !(world.failures);
      Lwt.return_unit)
    (fun () ->
      SO.configure_limiter old_limiter;
      Store.clock := Unix.gettimeofday;
      ignore (Sqlite3.db_close other);
      ignore (Sqlite3.db_close db);
      Sys.remove path;
      Lwt.return_unit)

let fire e n =
  let root = Printf.sprintf "17000000%02d.000001" n in
  let start = Printf.sprintf "17000000%02d" n in
  let room = "R" ^ string_of_int n in
  R.record_root ~recovery:true e.db ~order:e.order ~root_ts:root ~room_id:room
    ~date_start:start ~fire_id:None;
  Store.insert_fire e.db ~order:e.order ~root_ts:root ~room_id:room
    ~date_start:start ~now:!(e.clock) ()
  |> ok |> Option.get

let prepare e fires =
  C.prepare ~db:e.db ~order:e.order
    ~fire_ids:(List.map (fun (f : T.fire) -> f.id) fires)
    ~now:!(e.clock)

let ensure ?ctx ?db ?manager e b =
  C.ensure_notice
    ~ctx:(Option.value ctx ~default:e.ctx)
    ~db:(Option.value db ~default:e.db)
    ~manager:(Option.value manager ~default:e.manager)
    ~http:e.http b

let batch e b = R.find_batch e.other ~batch_id:b.R.id |> Option.get
let status e b = R.batch_details e.other ~batch_id:b.R.id |> Option.get |> fst
let posts e = List.filter (fun (m, _) -> m = "chat.postMessage") !(e.calls)
let notices e = List.filter (fun (_, p) -> get "blocks" p <> `Null) (posts e)

let renew e =
  check "renew" true
    (R.renew e.db ~order_id:e.order.id ~owner:e.owner ~now:!(e.clock))

let rec advance e delta =
  if delta > 0. then (
    let step = min 90. delta in
    e.clock := !(e.clock) +. step;
    renew e;
    advance e (delta -. step))

let release e b = C.release_due ~db:e.db ~batch_id:b.R.id ~now:!(e.clock)

let allowed e (f : T.fire) =
  R.relay_allowed e.other ~fire_id:f.T.id ~now:!(e.clock)

let case name fn = S.case name (fun () -> Lwt_main.run (with_env fn))

let with_transport e work =
  let method_of uri = Filename.basename (Uri.path (Uri.of_string uri)) in
  let transport : Slack_transport.t =
    {
      get =
        (fun ~uri ~headers:_ ->
          let params =
            Uri.query (Uri.of_string uri)
            |> List.map (fun (k, v) -> (k, str (List.hd v)))
          in
          e.http ~verb:"GET" ~token:"fixture" ~method_:(method_of uri) ~params);
      post =
        (fun ~uri ~headers:_ ~body ->
          let params =
            match Yojson.Safe.from_string body with `Assoc p -> p | _ -> []
          in
          e.http ~verb:"POST" ~token:"fixture" ~method_:(method_of uri) ~params);
    }
  in
  Slack_transport.with_http transport work

let click e b ?(user = "U456") ?(team = "T123") ?(app = "A123")
    ?(channel = "C123") ?message_ts ?card_id () =
  let ts =
    Option.value message_ts ~default:(Option.get (batch e b).notice_ts)
  in
  let action =
    obj
      [
        ("action_id", str Render.action_pause);
        ("value", str (Option.value card_id ~default:b.R.id));
        ("action_ts", str "1700000101.000001");
      ]
  in
  let payload =
    obj
      [
        ("type", str "block_actions");
        ("api_app_id", str app);
        ("team", obj [ ("id", str team) ]);
        ("user", obj [ ("id", str user) ]);
        ("channel", obj [ ("id", str channel) ]);
        ( "container",
          obj
            [
              ("type", str "message");
              ("message_ts", str ts);
              ("channel_id", str channel);
            ] );
        ("message", obj [ ("ts", str ts) ]);
        ("actions", `List [ action ]);
      ]
  in
  with_transport e (fun () ->
      Slack.handle_event
        ~config:(Option.get (Session.get_config e.manager).channels.slack)
        ~session_manager:e.manager
        (Yojson.Safe.to_string payload))
  |> Lwt.map ignore
