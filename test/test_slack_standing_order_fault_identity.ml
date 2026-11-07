open Slack_standing_order_fault_support
open Lwt.Syntax

let second =
  ended_root
  |> set "ts" (str "1700000020.000100")
  |> set "files" (`List [ obj [ ("id", str "FCANVAS2") ] ])
  |> set "room"
       (get "room" ended_root
       |> set "date_start" (`Int 1700000020)
       |> set "transcript_file_id" (str "FCANVAS2")
       |> set "thread_root_ts" (str "1700000020.000100")
       |> set "canvas_thread_ts" (str "1700000020.000100"))

let sources ~collision e =
  e.server.override <-
    (fun m p ->
      if m = "files.info" && field "file" p = "FCANVAS2" then
        Some
          ( 200,
            S.ok
              [
                ( "file",
                  P.canvas
                  |> set "id" (str "FCANVAS2")
                  |> set "huddle_transcript_file_id"
                       (if collision then str "FTRANS" else `Null)
                  |> set "shares"
                       (obj
                          [
                            ( "private",
                              obj
                                [
                                  ( "C123",
                                    `List
                                      [
                                        P.share "1700000020.000100";
                                        P.share P.ready_ts;
                                      ] );
                                ] );
                          ]) );
              ] )
      else None)

let reuse collision e =
  let* () = pass e in
  start e;
  live e;
  let* () = until e "first actual use shares" (fun () -> relays e = 1) 59 in
  sources ~collision e;
  e.server.roots <- [ ended_root; second ];
  ignore (install_instance e second);
  observe e (event_root second);
  observe e (ready_for second);
  observe e (ready_for second);
  observe e (event_root ended_root);
  (* delayed stale edit after the new room use *)
  let* () = advance e 65. in
  eq "reuse with separate start respects transcript/canvas constraints"
    (if collision then 1 else 2)
    (relays e);
  if collision then
    check "second use is refused by durable transcript uniqueness" true
      (List.exists
         (fun (f : T.fire) ->
           f.date_start = "1700000020" && f.reason = Some "transcript_conflict")
         (fires e));
  let rows =
    Slack_huddle_ledger.list_rows ~db:e.db ~team:"T123" ~channel:"C123"
  in
  check "delayed earlier edit never replaces new room use" true
    (List.exists
       (fun (r : Slack_huddle_ledger.row) ->
         r.room_id = Some "R123" && r.root_ts = Some "1700000020.000100")
       rows);
  check "same room different start retained as separate durable instances" true
    (List.length (fires e) >= 2);
  let* () = restart e in
  let* () = pass e in
  let* () = worker e in
  eq "duplicate Socket hints and history cannot add a share"
    (if collision then 1 else 2)
    (relays e);
  bounds e.server;
  Lwt.return_unit

let cursor e =
  e.server.roots <- [ ended_root ];
  ignore (install_instance e ended_root);
  e.server.override <-
    (fun m p ->
      if m = "conversations.history" && get "limit" p <> `Int 1 then
        Some
          ( 200,
            H.page [ ended_root ]
            |> set "has_more" (`Bool true)
            |> set "response_metadata" (obj [ ("next_cursor", str "repeated") ])
          )
      else None);
  let* () = pass e in
  check "repeated cursor cannot become full coverage" false
    (R.covered e.db e.order.id e.order.epoch);
  eq "repeated cursor never releases a relay" 0 (relays e);
  let* () = restart e in
  let* () = advance e 121. in
  e.server.override <- (fun _ _ -> None);
  let* () = pass e in
  check "corrected complete rescan obtains coverage" true
    (R.covered e.db e.order.id e.order.epoch);
  eq "recovered root remains held behind notice" 0 (relays e);
  let* () = advance e 601. in
  let* () = pass e in
  let* () = worker e in
  eq "dedup page replay shares only once" 1 (relays e);
  Lwt.return_unit

let spoof e =
  let* () = pass e in
  start e;
  e.server.roots <- [ ended_root ];
  ignore (install_instance e ended_root);
  List.iter
    (fun user -> observe e (ready_for ended_root |> set "user" (str user)))
    [ "U123"; "UOTHERBOT" ];
  let* () = advance e 20. in
  eq "spoofed ready never allocates" 0
    (count e.server "files.getUploadURLExternal");
  eq "spoofed ready never shares" 0 (relays e);
  live e;
  let* () =
    until e "real Slackbot positive control shares" (fun () -> relays e = 1) 59
  in
  let fire = one e in
  observe e
    (obj
       [
         ("type", str "message");
         ("channel", str "C123");
         ("user", str "UCLAUDE");
         ("bot_id", str "BCLAUDE");
         ("app_id", str "ACLAUDE");
         ("thread_ts", str "1700000999.000100");
         ("ts", str "1700001000.000100");
         ("text", str "ignored");
       ]);
  let* () = advance e 1. in
  check "unrelated target thread cannot imply pickup" true
    ((Option.get (Store.runtime e.db ~fire_id:fire.id)).fire.state
   <> T.Picked_up);
  Lwt.return_unit

let suite =
  [
    case
      "reused room distinct start and stale edit preserve both actual \
       deliveries"
      (reuse false);
    case "globally unique transcript blocks second reused-room relay"
      (reuse true);
    case "repeated cursors stay held and dedup after reopened full rescan"
      cursor;
    case
      "spoofed readiness and unrelated assistant thread never grant authority"
      spoof;
  ]
