(* Shared standing-order test helpers (no suite). Every standing-order suite
   builds its cases with [case], which pins the store clock to [now]. Fake ids
   only. *)
module T = Slack_standing_order_types
module Store = Slack_standing_order_store

let now = 1_700_000_000.
let at t = Store.clock := fun () -> t

let case name f =
  ( name,
    `Quick,
    fun () ->
      at now;
      Fun.protect ~finally:(fun () -> Store.clock := Unix.gettimeofday) f )

let fresh_db () =
  let db = Sqlite3.db_open ":memory:" in
  Store.ensure_schema db;
  Access_snapshot.init_schema db;
  Admin.init_schema db;
  Egress_audit.init_schema db;
  Setup_plan_apply.init_schema db;
  Room_activity_ledger.init_schema db;
  db

let state_t =
  Alcotest.testable
    (fun ppf s -> Format.pp_print_string ppf (T.state_to_string s))
    ( = )

let fire_state_t =
  Alcotest.testable
    (fun ppf s -> Format.pp_print_string ppf (T.fire_state_to_string s))
    ( = )

let claude : T.target =
  {
    user_id = "UCLAUDE";
    app_id = "ACLAUDE";
    bot_id = "BCLAUDE";
    label = "Claude (Anthropic)";
  }

let relay_action ?(instruction = "Update the project docs.") ?(target = claude)
    ?(when_no = T.Relay_notes) () =
  T.Relay_to_app_mention { target; instruction; when_no_transcript = when_no }

let seed db ?(team = "T123") ?(channel = "C123") ?(grantor = "U123")
    ?(target = claude) ?(state = T.Active) () : T.order =
  let order_id = Store.new_id ~prefix:"so" in
  let pending =
    match
      Store.insert_pending db ~id:order_id ~team_id:team ~api_app_id:"A123"
        ~channel_id:channel ~grantor_user_id:grantor
        ~action:(relay_action ~target ()) ~management_thread_ts:"100.000001"
        ~now ()
    with
    | Ok o -> o
    | Error (Store.Duplicate_live { display_no }) ->
        Alcotest.failf "S.seed: SO-%d is already live" display_no
    | Error (Store.Insert_failed e) -> Alcotest.fail ("S.seed: " ^ e)
  in
  let activate () =
    match
      Store.activate_in_tx db ~order_id ~active_since:"1700000000.000100"
        ~confirmed_by:grantor ~bot_user_id:"UBOT" ~bot_id:"BOWN" ~now
    with
    | Ok o -> o
    | Error e -> Alcotest.fail ("S.seed activate: " ^ e)
  in
  let deactivate op =
    ignore (activate () : T.order);
    match Store.deactivate db ~order_id op ~now with
    | Ok change -> change.Store.after
    | Error e -> Alcotest.fail ("S.seed deactivate: " ^ e)
  in
  match state with
  | T.Pending_confirmation -> pending
  | T.Active -> activate ()
  | T.Paused -> deactivate (Store.Pause grantor)
  | T.Suspended -> deactivate (Store.Suspend "test")
  | T.Cancelled -> deactivate (Store.Cancel grantor)

let fire_flags db fire_id =
  match
    Sql_util.query_rows db
      "SELECT state, repost_forbidden, cancel_requested FROM \
       slack_standing_order_fires WHERE id = ?"
      ~bind:(fun stmt ->
        Sql_util.bind_params stmt [ Sqlite3.Data.TEXT fire_id ])
      ~of_stmt:(fun stmt ->
        ( T.fire_state_of_string (Sql_util.text_column stmt 0),
          Sql_util.sql_bool stmt 1,
          Sql_util.sql_bool stmt 2 ))
  with
  | [ (Some state, repost_forbidden, cancel_requested) ] ->
      (state, repost_forbidden, cancel_requested)
  | _ -> Alcotest.fail ("S.fire_flags: no decodable fire " ^ fire_id)

(* [insert_fire], then a raw UPDATE forcing the given columns. *)
let fire db (o : T.order) ?state ?claim_until ?dispatched_at ?reposts ?relay_ts
    ?(date_start = "1700000000") ~room () : T.fire =
  let inserted =
    match Store.insert_fire db ~order:o ~room_id:room ~date_start ~now () with
    | Ok (Some f) -> f
    | Ok None -> Alcotest.fail ("S.fire: duplicate fire in room " ^ room)
    | Error e -> Alcotest.fail ("S.fire: " ^ e)
  in
  let open Sqlite3.Data in
  let set column value = Option.map (fun v -> (column ^ " = ?", [ value v ])) in
  let sets =
    List.filter_map Fun.id
      [
        set "state" (fun s -> TEXT (T.fire_state_to_string s)) state;
        Option.map
          (fun t -> ("claim_owner = 'test', lease_until = ?", [ FLOAT t ]))
          claim_until;
        set "dispatched_at" (fun t -> FLOAT t) dispatched_at;
        set "reposts" (fun n -> INT (Int64.of_int n)) reposts;
        set "relay_ts" (fun ts -> TEXT ts) relay_ts;
      ]
  in
  if sets <> [] then
    Sql_util.exec_with_params db
      ("UPDATE slack_standing_order_fires SET "
      ^ String.concat ", " (List.map fst sets)
      ^ " WHERE id = ?")
      (List.concat_map snd sets @ [ TEXT inserted.id ]);
  match
    List.find_opt
      (fun (f : T.fire) -> f.id = inserted.id)
      (Store.list_fires db ~order_id:o.id ~limit:max_int)
  with
  | Some f -> f
  | None -> Alcotest.fail ("S.fire: fire does not read back in room " ^ room)
