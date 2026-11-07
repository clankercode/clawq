open Lwt.Syntax
module C = Slack_standing_order_cleanup_store
module Scope = Slack_standing_order_cleanup_scope
module Sql = Slack_standing_order_sql
module SO = Slack_standing_order_slack
module Pages = Slack_standing_order_pages

let run ~db ~(job : C.job) ~owner ~now ~check ~authorize ~budget ~channel thread
    status =
  let set status ts =
    ignore
      (C.update db ~job:job.id ~owner ~now:(now ()) (fun () ->
           Sql.exec db
             "UPDATE slack_order_cleanup_notes SET status=?,ts=? WHERE job=? \
              AND thread=?"
             Sql.
               [
                 text status; Sqlite3.Data.opt_text ts; text job.id; text thread;
               ]))
  in
  let reserve () =
    check () = Ok ()
    && C.update db ~job:job.id ~owner ~now:(now ()) (fun () ->
        Sql.exec db
          "UPDATE slack_order_cleanup_notes SET status='attempted' WHERE job=? \
           AND thread=? AND status='pending'"
          Sql.[ text job.id; text thread ];
        if Sqlite3.changes db <> 1 then
          failwith "cleanup note already attempted")
  in
  if !budget < 3 || status = "confirmed" then Lwt.return_unit
  else if status = "pending" then (
    let* call = authorize () in
    match call with
    | None -> Lwt.return_unit
    | Some call ->
        decr budget;
        let* result =
          SO.with_policy ~priority:Slack_read_limiter.Maintenance
            ~check:(fun () ->
              if reserve () then Ok () else Error SO.Dispatch_refused)
            (fun () ->
              call "chat.postMessage"
                (Scope.note_params ~channel ~job:job.id ~thread))
        in
        (match result with
        | Ok json
          when Pages.valid_ts (Pages.field "ts" json)
               && Pages.field "channel" json = channel ->
            set "confirmed" (Some (Pages.field "ts" json))
        | _ ->
            (* If preflight refused before the reservation, it remains pending. *)
            ignore
              (C.update db ~job:job.id ~owner ~now:(now ()) (fun () ->
                   Sql.exec db
                     "UPDATE slack_order_cleanup_notes SET status='unknown' \
                      WHERE job=? AND thread=? AND status='attempted'"
                     Sql.[ text job.id; text thread ])));
        Lwt.return_unit)
  else
    let () =
      ignore
        (C.update db ~job:job.id ~owner ~now:(now ()) (fun () ->
             Sql.exec db
               "UPDATE slack_order_cleanup_notes SET latest=COALESCE(latest,?) \
                WHERE job=? AND thread=?"
               Sql.
                 [
                   text (Printf.sprintf "%.6f" (now ()));
                   text job.id;
                   text thread;
                 ]))
    in
    let latest =
      Sql.query db
        "SELECT latest FROM slack_order_cleanup_notes WHERE job=? AND thread=?"
        Sql.[ text job.id; text thread ]
        (fun s -> Sql_util.text_column s 0)
      |> List.hd
    in
    let rec page () =
      if !budget < 3 || check () <> Ok () then Lwt.return_unit
      else
        match
          Sql.query db
            "SELECT cursor,seen,match_ts,ambiguous,pages FROM \
             slack_order_cleanup_notes WHERE job=? AND thread=?"
            Sql.[ text job.id; text thread ]
            (fun s ->
              ( Sql_util.text_column s 0,
                C.ids (Sql_util.text_column s 1),
                Sql_util.opt_text_column s 2,
                Sql_util.sql_bool s 3,
                Sql_util.int_column s 4 ))
        with
        | [ (cursor, seen, matched, ambiguous, pages) ] -> (
            let* call = authorize () in
            match call with
            | None -> Lwt.return_unit
            | Some call -> (
                decr budget;
                let* result =
                  call "conversations.replies"
                    ([
                       ("channel", `String channel);
                       ("ts", `String thread);
                       ("limit", `Int 200);
                       ("latest", `String latest);
                       ("inclusive", `Bool true);
                     ]
                    @ if cursor = "" then [] else [ ("cursor", `String cursor) ]
                    )
                in
                if check () <> Ok () then Lwt.return_unit
                else
                  match result with
                  | Error _ -> Lwt.return_unit
                  | Ok json -> (
                      match
                        ( Pages.messages json,
                          Pages.next ~seen ~remaining:(200 - pages) json )
                      with
                      | Ok messages, Ok next
                        when List.for_all
                               (fun m ->
                                 (Pages.field "ts" m = thread
                                 || Pages.field "thread_ts" m = thread)
                                 && Slack_standing_order_provenance.compare_ts
                                      (Pages.field "ts" m) thread
                                    >= 0
                                 && Slack_standing_order_provenance.compare_ts
                                      (Pages.field "ts" m) latest
                                    <= 0)
                               messages ->
                          let matches =
                            List.filter
                              (Scope.matches ~job ~channel ~thread)
                              messages
                            |> List.map (Pages.field "ts")
                          in
                          let matches =
                            C.sorted (Option.to_list matched @ matches)
                          in
                          let ambiguous =
                            ambiguous || List.length matches > 1
                          in
                          let match_ts = List.nth_opt matches 0 in
                          let saved =
                            C.update db ~job:job.id ~owner ~now:(now ())
                              (fun () ->
                                Sql.exec db
                                  "UPDATE slack_order_cleanup_notes SET \
                                   status=?,cursor=?,seen=?,match_ts=?,ambiguous=?,pages=?,latest=CASE \
                                   WHEN ? THEN NULL ELSE latest END,ts=CASE \
                                   WHEN ? THEN ? ELSE ts END WHERE job=? AND \
                                   thread=?"
                                  Sql.
                                    [
                                      text
                                        (if
                                           next = "" && match_ts <> None
                                           && not ambiguous
                                         then "confirmed"
                                         else "unknown");
                                      text next;
                                      text
                                        (C.json_ids
                                           (if next = "" then []
                                            else next :: seen));
                                      Sqlite3.Data.opt_text match_ts;
                                      bool ambiguous;
                                      int (if next = "" then 0 else pages + 1);
                                      bool (next = "");
                                      bool (next = "" && not ambiguous);
                                      Sqlite3.Data.opt_text match_ts;
                                      text job.id;
                                      text thread;
                                    ])
                          in
                          if saved && next <> "" then page ()
                          else Lwt.return_unit
                      | _ -> Lwt.return_unit)))
        | _ -> Lwt.return_unit
    in
    page ()
