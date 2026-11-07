open Slack_standing_order_fault_support
open Lwt.Syntax

let audit_tracking e =
  let* () = pass e in
  start e;
  live e;
  let* () =
    until e "ordinary healthy first share" (fun () -> relays e = 1) 59
  in
  let fire = one e in
  let r = Option.get (Store.runtime e.db ~fire_id:fire.id) in
  let* call =
    H.drain e.server.clock
      (Slack_standing_order_dispatch.track ~ctx:e.ctx ~db:e.db
         ~manager:e.manager ~http:(server_http e.server) r e.order)
  in
  let* _ =
    H.drain e.server.clock
      (call "reactions.get"
         [
           ("channel", str "C123");
           ("timestamp", str (Option.get r.fire.relay_ts));
         ])
  in
  let* () =
    H.drain e.server.clock
      (Slack_standing_order_report.post_once ~ctx:e.ctx ~db:e.db
         ~manager:e.manager ~http:(server_http e.server) ~fire_id:fire.id
         e.order Slack_standing_order_report.Transcript_unavailable)
  in
  let labels =
    Sql.query e.db
      "SELECT a.tool_name FROM egress_audit a JOIN access_snapshots s ON \
       s.id=a.snapshot_id WHERE s.standing_order_fire_id=?"
      [ Sql.text fire.id ]
      (fun s -> Sql_util.text_column s 0)
  in
  check "tracking audit carries exact fire identity on every actual call" true
    (labels <> []
    && List.for_all
         (fun label -> String_util.contains label ("fire:" ^ fire.id))
         labels);
  Lwt.return_unit

let suite =
  [
    case "actual retained tracking attribution carries persisted fire id"
      audit_tracking;
  ]

let suite = suite @ Test_slack_standing_order_fault_budgets.suite
let suite = suite @ Test_slack_standing_order_fault_matrix.suite
let suite = suite @ Test_slack_standing_order_fault_races.suite
let suite = suite @ Test_slack_standing_order_fault_admission.suite
let suite = suite @ Test_slack_standing_order_fault_security.suite
let suite = suite @ Test_slack_standing_order_fault_identity.suite
let suite = suite @ Test_slack_standing_order_fault_process.suite

let resolve_audit e =
  let* () = pass e in
  e.server.roots <- [ root ];
  observe e (event_root root);
  let resolve () =
    H.drain e.server.clock
      (Slack_standing_order_fetch.resolve_pending ~ctx:e.ctx ~db:e.db
         ~manager:e.manager ~http:(server_http e.server) e.order)
  in
  let start_id =
    Sql.int_query e.db "SELECT COALESCE(MAX(id),0) FROM egress_audit" []
  in
  let* () = resolve () in
  let* () = advance e 301. in
  let* () = resolve () in
  let labels =
    Sql.query e.db "SELECT DISTINCT tool_name FROM egress_audit WHERE id>?"
      [ Sql.int start_id ]
      (fun s -> Sql_util.text_column s 0)
  in
  eq "two actual resolution passes carry distinct reconcile identities" 2
    (List.length labels);
  check "every resolution label names its persisted order and reconcile pass"
    true
    (List.for_all
       (fun label ->
         String.starts_with
           ~prefix:("standing_order:" ^ e.order.id ^ ":reconcile:resolve")
           label)
       labels);
  eq "audit-only live root remains no synthetic fire" 0 (List.length (fires e));
  Lwt.return_unit

let suite =
  suite
  @ [
      case "resolution reads distinguish actual pass identities in audit"
        resolve_audit;
    ]
