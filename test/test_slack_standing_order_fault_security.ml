open Slack_standing_order_fault_support
open Lwt.Syntax
module G = Slack_standing_order_grant

let cfg e f = Session.update_config e.manager (f (Session.get_config e.manager))
let user e id key value = S.edit_user e.server.world id (set key value)

let denied name e =
  let* () = pass e in
  (match name with
  | "grantor_deleted" -> user e "U123" "deleted" (`Bool true)
  | "grantor_left" -> S.remove_member e.server.world "U123"
  | "grantor_guest" -> user e "U123" "is_restricted" (`Bool true)
  | "lost_admin" ->
      cfg e (fun c ->
          S.with_slack c (fun s ->
              {
                s with
                standing_orders =
                  { s.standing_orders with creator_policy = So_creator_admin };
              }))
  | "grantor_allowlist" -> cfg e (fun c -> S.with_allow_users c [ "UOTHER" ])
  | "private_policy" ->
      cfg e (fun c ->
          S.with_slack c (fun s -> { s with private_channel_policy = Pc_deny }));
      e.server.override <-
        (fun m p ->
          if m = "conversations.info" then
            let j = S.default_response e.server.world m p in
            Some
              ( 200,
                j
                |> set "channel"
                     (get "channel" j |> set "is_private" (`Bool true)) )
          else None)
  | "credential_inactive" ->
      cfg e (fun c -> S.with_slack c (fun s -> { s with bot_token = "" }))
  | "target_unlisted" ->
      cfg e (fun c ->
          S.with_slack c (fun s ->
              {
                s with
                standing_orders =
                  { s.standing_orders with allowed_targets = [] };
              }))
  | "target_deleted" -> user e "UCLAUDE" "deleted" (`Bool true)
  | "target_changed_app" ->
      S.edit_user e.server.world "UCLAUDE" (fun u ->
          set "profile" (get "profile" u |> set "api_app_id" (str "AEVIL")) u)
  | "channel_guest" -> user e "U456" "is_restricted" (`Bool true)
  | "missing_egress" ->
      cfg e (fun c ->
          { c with egress = { c.egress with default_allowlist = [] } })
  | "team_mismatch" ->
      e.server.override <-
        (fun m _ ->
          if m = "auth.test" then
            Some
              ( 200,
                S.ok
                  [
                    ("team_id", str "TOTHER");
                    ("user_id", str "UBOT");
                    ("bot_id", str "BOWN");
                  ] )
          else None)
  | "app_mismatch" ->
      e.server.override <-
        (fun m _ ->
          if m = "bots.info" then
            Some
              ( 200,
                S.ok
                  [
                    ( "bot",
                      obj
                        [
                          ("id", str "BOWN");
                          ("app_id", str "AOTHER");
                          ("user_id", str "UBOT");
                        ] );
                  ] )
          else None)
  | capability when List.mem capability G.capability_names ->
      cfg e (fun c -> S.profile c ~denied:[ capability ] ())
  | flag
    when List.mem flag
           [
             "is_shared";
             "is_ext_shared";
             "is_org_shared";
             "is_pending_ext_shared";
           ] ->
      e.server.override <-
        (fun m p ->
          if m = "conversations.info" then
            let j = S.default_response e.server.world m p in
            Some
              ( 200,
                j |> set "channel" (get "channel" j |> set flag (`Bool true)) )
          else None)
  | _ -> assert false);
  e.server.calls <- [];
  e.server.roots <- [ ended_root ];
  ignore (install_instance e ended_root);
  let* () = pass e in
  start e;
  observe e (event_root ended_root);
  observe e (ready_for ended_root);
  let* () = advance e 12. in
  List.iter
    (fun m -> eq ("denial prevents " ^ m) 0 (count e.server m))
    [
      "files.getUploadURLExternal";
      "raw_upload";
      "files.completeUploadExternal";
      "files.delete";
    ];
  eq "authority denied no actual relay" 0 (relays e);
  if List.mem name G.capability_names then
    eq "capability refusal zero HTTP including reports" 0
      (List.length e.server.calls);
  let* decision =
    H.drain e.server.clock
      (G.evaluate ~db:e.db
         ~current_config:(fun () -> Session.get_config e.manager)
         ~http:(server_http e.server) ~limiter:(Option.get e.ctx.limiter)
         ~now:e.server.clock.now
         ~stage:
           (G.Reconcile { reconcile_id = Store.new_id ~prefix:"denied-audit" })
         ~unit_label:"denial"
         (G.subject_of_order (Option.get (Store.find e.db e.order.id))))
  in
  check
    "current complete Grant refuses authority despite any prior scan \
     certificate"
    true (Result.is_error decision);
  if List.mem name G.capability_names then
    eq "local refusal remains zero HTTP" 0 (List.length e.server.calls);
  Lwt.return_unit

let audit e =
  let check_snapshot method_ =
    let rows =
      Sql.query e.db
        "SELECT tool_name,snapshot_id,session_key FROM egress_audit ORDER BY \
         id DESC LIMIT 1"
        [] (fun s ->
          ( Sql_util.text_column s 0,
            Sql_util.text_column s 1,
            Sql_util.text_column s 2 ))
    in
    match rows with
    | [ (label, id, key) ] ->
        let snap = Option.get (Access_snapshot.get_by_id ~db:e.db id) in
        check
          (method_ ^ " stored standing-order snapshot")
          true
          (snap.work_type = Access_snapshot.Standing_order);
        Alcotest.(check string)
          (method_ ^ " explicit access key")
          "slack:C123" key;
        let ref_ = Option.get snap.standing_order in
        Alcotest.(check string)
          (method_ ^ " persisted order attribution")
          e.order.id ref_.order_id;
        check
          (method_ ^ " immutable audit label includes order")
          true
          (String_util.contains label e.order.id)
    | _ -> Alcotest.fail "HTTP without persisted egress decision"
  in
  e.server.inspect <- check_snapshot;
  let* (), logs =
    S.with_log_capture (fun () ->
        let* () = pass e in
        start e;
        live e;
        let* () = until e "audited actual share" (fun () -> relays e = 1) 59 in
        let* () = advance e 5. in
        let* () = stop () in
        e.server.state <- { e.server.state with deleted = [ "FCANVAS" ] };
        checkpoint e.server;
        let* () = Test_slack_standing_order_fault_matrix.retention e in
        let* () = advance e 301. in
        Test_slack_standing_order_fault_matrix.retention e)
  in
  let audits =
    Sql.query e.db
      "SELECT tool_name,host_redacted,path_redacted,credential_handle_ids_json \
       FROM egress_audit"
      [] (fun s -> String.concat " " (List.init 4 (Sql_util.text_column s)))
  in
  let captured =
    String.concat "\n"
      (audits @ List.map snd logs
      @ List.map
          (fun r -> r.method_ ^ String.concat "" (List.map snd r.ids))
          e.server.calls)
  in
  List.iter
    (fun secret ->
      check
        ("captured logs/audit/request metadata exclude " ^ secret)
        false
        (String_util.contains captured secret))
    [
      "MEETING_PRIVATE";
      "SECRET_CAP_";
      "https://files.slack.com/upload/";
      "xoxb-";
    ];
  let reconcile_ids =
    Sql.query e.db
      "SELECT DISTINCT a.tool_name FROM egress_audit a JOIN access_snapshots s \
       ON s.id=a.snapshot_id WHERE s.standing_order_id=? AND a.tool_name LIKE \
       '%reconcile:%'"
      [ Sql.text e.order.id ]
      (fun s -> Sql_util.text_column s 0)
  in
  check "reconcile reads have inspectable pass identities" true
    (reconcile_ids <> []);
  eq "security fixture sent exactly two actual binary requests" 2
    (count e.server "raw_upload");
  Lwt.return_unit

let accepting e =
  let* () = pass e in
  e.accepting := false;
  start e;
  live e;
  e.server.calls <- [];
  let* () = advance e 10. in
  eq "accepting drain stops relay allocations" 0
    (count e.server "files.getUploadURLExternal");
  eq "accepting drain stops shares" 0 (relays e);
  Lwt.return_unit

let suite =
  List.map
    (fun name -> case ("authority matrix actual server " ^ name) (denied name))
    ([
       "grantor_deleted";
       "grantor_left";
       "grantor_guest";
       "lost_admin";
       "grantor_allowlist";
       "private_policy";
       "credential_inactive";
       "target_unlisted";
       "target_deleted";
       "target_changed_app";
       "channel_guest";
       "missing_egress";
       "team_mismatch";
       "app_mismatch";
       "is_shared";
       "is_ext_shared";
       "is_org_shared";
       "is_pending_ext_shared";
     ]
    @ G.capability_names)
  @ [
      case
        "every integrated HTTP and raw call has persisted snapshot and \
         redacted audit"
        audit;
      case "accepting drain blocks actual worker writes" accepting;
    ]
