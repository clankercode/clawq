open Lwt.Syntax
module S = Test_slack_standing_order_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module Sql = Slack_standing_order_sql
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Ctx = Slack_standing_order_ctx
module Reconcile = Slack_standing_order_reconcile

let check = Alcotest.(check bool)
let eq = Alcotest.(check int)
let identity = Test_slack_standing_order_exec_support.fixture_identity
let obj xs = `Assoc xs
let str s = `String s

let fixture ?(state = T.Paused) db =
  let order = S.seed ~verified_identity:identity db () in
  let fire =
    S.fire db order ~state:T.Picked_up ~room:"RRETENTION"
      ~date_start:"1700000001" ~dispatched_at:(S.now -. 60.)
      ~relay_ts:"1699999940.000001" ()
  in
  let a =
    {
      (Store.empty_attempt 0) with
      uploaded_file_ids = [ "FOWN0" ];
      dispatched_at = Some (S.now -. 60.);
      relay_ts = fire.relay_ts;
    }
  in
  Store.save_attempt db fire.id a;
  Sql.exec db
    "UPDATE slack_standing_order_fires SET \
     source_canvas_id='FSOURCE',source_transcript_id='FTRANS' WHERE id=?"
    [ Sql.text fire.id ];
  let order =
    if state = T.Active then order
    else
      let op =
        match state with
        | T.Paused -> Store.Pause "U123"
        | T.Suspended -> Store.Suspend "test"
        | T.Cancelled -> Store.Cancel "U123"
        | _ -> assert false
      in
      (Result.get_ok (Store.deactivate db ~order_id:order.id op ~now:S.now))
        .after
  in
  (order, fire)

let context () =
  Ctx.make
    ~wait:(fun ~seconds:_ -> fst (Lwt.task ()))
    ~accepting:(fun () -> true)
    ~limit:(fun ~channel_id:_ -> Lwt.return_unit)
    ()

let mock () =
  let m = S.mock () in
  S.override m "files.info" (fun _ ->
      (200, obj [ ("ok", `Bool false); ("error", str "file_not_found") ]));
  S.override m "files.delete" (fun _ -> (200, S.ok []));
  m

let count m name = List.length (List.filter (( = ) name) (S.methods_called m))

module Retention = Slack_standing_order_retention
module Cleanup = Slack_standing_order_cleanup_store
module Scope = Slack_standing_order_cleanup_scope

let set = Test_slack_standing_order_provenance.set

let with_fixture ?state ?config f =
  let db = S.fresh_db () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let order, fire = fixture ?state db in
      let m = mock () in
      let manager =
        Session.create
          ~config:(Option.value config ~default:(S.config ()))
          ~db ()
      in
      f db order fire m manager)

let run ?ctx db order m manager =
  Lwt_main.run
    (Retention.pass
       ~ctx:(Option.value ctx ~default:(context ()))
       ~db ~manager ~http:m.S.http order)

let deleted m =
  !(m.S.calls)
  |> List.filter_map (fun (_, method_, p) ->
      if method_ = "files.delete" then Some (S.param p "file") else None)

let complete db id = (Option.get (Cleanup.find db id)).status = "complete"

let no_writes m =
  eq "zero deletes" 0 (count m "files.delete");
  eq "zero notes" 0 (count m "chat.postMessage")

let add_attempt db (fire : T.fire) =
  let a =
    {
      (Store.empty_attempt 1) with
      uploaded_file_ids = [ "FOWN1"; "FOWN2" ];
      dispatched_at = Some (S.now -. 30.);
      relay_ts = Some "1699999970.000001";
    }
  in
  Store.save_attempt db fire.T.id a
