open Test_slack_standing_order_reconcile_support
open Lwt.Syntax
module Store = Slack_standing_order_store
module Journal = Slack_standing_order_pause_store

let parent_ts = "1700000050.000100"
let pause_ts = "1700000060.000100"

let parent =
  obj
    [
      ("ts", str parent_ts);
      ("user", str "U123");
      ("text", str "ordinary parent");
      ("reply_count", `Int 1);
      ("latest_reply", str pause_ts);
    ]

let pause ?(broadcast = true) () =
  obj
    ([
       ("type", str "message");
       ("ts", str pause_ts);
       ("thread_ts", str parent_ts);
       ("user", str "U456");
       ("text", str "pause SO-1");
     ]
    @ if broadcast then [ ("subtype", str "thread_broadcast") ] else [])

type surface = History | Replies | Paired

let install e surface message =
  (e.roots :=
     match surface with
     | History -> [ message ]
     | Replies -> [ parent ]
     | Paired -> [ parent; message ]);
  let previous = !(e.answer) in
  e.answer :=
    fun method_ params ->
      if
        method_ = "conversations.replies"
        && Slack_huddles.field "ts" params = parent_ts
      then Lwt.return (200, page [ parent; message ])
      else previous method_ params

let current e = Option.get (Store.find e.f.db e.order.id)

let journal_count e =
  Journal.ensure_schema e.f.db;
  Sql.int_query e.f.db "SELECT count(*) FROM slack_order_pause_replays" []

let journal_message =
  { Journal.ts = pause_ts; thread_ts = parent_ts; user_id = "U456" }

let prepare e maintenance =
  F.set_config e.f (fun cfg -> S.with_allow_users cfg [ "U123" ]);
  F.fail_provider e.f;
  if maintenance then (
    let* () = pass e in
    check "healthy control starts covered" true (covered e);
    advance e.clock (e.clock.now +. 300.))
  else Lwt.return_unit

let no_relay e =
  List.iter
    (fun method_ -> eq ("no " ^ method_) 0 (count e method_))
    [ "files.getUploadURLExternal"; "files.completeUploadExternal" ];
  eq "no raw upload" 0 (List.length !(e.raw_calls));
  eq "no LLM turn" 0 (List.length !(e.f.seen))

let accepted ~maintenance ~broadcast surface () =
  with_env (fun e ->
      let* () = prepare e maintenance in
      let old = R.load_coverage e.f.db ~order_id:e.order.id in
      let fire = S.fire e.f.db e.order ~room:"R123" ~state:T.Fetching () in
      install e surface (pause ~broadcast ());
      let* () = pass e in
      check "missed pause denies coverage before another pass" false (covered e);
      if maintenance then
        check "contradiction cannot advance healthy watermark" true
          ((Option.get (R.load_coverage e.f.db ~order_id:e.order.id))
             .last_success = (Option.get old).last_success);
      let* () = if maintenance then pass e else Lwt.return_unit in
      Alcotest.check S.state_t "eligible nonallowlisted human pauses order"
        T.Paused (current e).state;
      eq "one lifecycle transition" (e.order.epoch + 1) (current e).epoch;
      eq "one exact journal identity" 1 (journal_count e);
      check "history and replies retain original thread identity" true
        (Journal.remember e.f.db e.order journal_message = "complete");
      Alcotest.check S.fire_state_t "pause sweeps pre-dispatch fire"
        T.Skipped_inactive
        (Option.get (Store.runtime e.f.db ~fire_id:fire.id)).fire.state;
      List.iter
        (fun kind ->
          check "notification reservation survives any send refusal" true
            (Journal.notification_status e.f.db e.order journal_message kind
            <> None))
        [ "pause_notice"; "pause_receipt" ];
      let receipts =
        !(e.calls)
        |> List.filter_map (fun (_, method_, params) ->
            if
              method_ = "chat.postMessage"
              && String_util.contains
                   (Slack_huddles.field "text" params)
                   "Late pause receipt."
            then Some params
            else None)
      in
      List.iter
        (fun params ->
          Alcotest.(check string)
            "late receipt uses original thread" parent_ts
            (Slack_huddles.field "thread_ts" params))
        receipts;
      (match surface with
      | History ->
          eq "history alone suffices" 0 (count e "conversations.replies")
      | Replies | Paired ->
          check "actual replies fetched" true
            (count e "conversations.replies" > 0));
      let notifications = count e "chat.postMessage" in
      E.start ~ctx:e.ctx ~db:e.f.db ~manager:e.f.base.manager ~http:e.http
        ~upload_http:(raw_http e) ();
      let* () = advance e.clock (e.clock.now +. 601.) in
      let* () = pass e in
      eq "no duplicate notification after repeat/hold expiry" notifications
        (count e "chat.postMessage");
      eq "no catch-up batch or release after pause" 0
        (Sql.int_query e.f.db
           "SELECT count(*) FROM slack_standing_order_recovery_batches" []);
      no_relay e;
      Lwt.return_unit)

let receipt_identity () =
  let module SS = Test_slack_standing_order_scan_support in
  SS.with_scan
    ~config:(fun cfg -> S.with_allow_users cfg [ "U123" ])
    (fun e ctx owner _ validated ->
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ parent; pause () ]));
      S.override e.mock "conversations.replies" (fun _ ->
          (200, page [ parent; pause () ]));
      let at_send = ref [] in
      S.override e.mock "chat.postMessage" (fun params ->
          at_send :=
            (SS.state e = T.Paused
            && (not (R.covered e.db e.order.id e.order.epoch))
            && Journal.completed_outcome e.db e.order journal_message
               = Some "paused")
            :: !at_send;
          (200, S.default_response e.mock.world "chat.postMessage" params));
      SS.unwrap (SS.sweep e ctx owner validated);
      Alcotest.check S.state_t "paired broadcast commits actual pause" T.Paused
        (SS.state e);
      let posts = SS.posts e in
      eq "one grantor notice and one actual late receipt" 2 (List.length posts);
      check "pause and journal commit before every actual notification" true
        (!at_send = [ true; true ]);
      let receipts =
        List.filter
          (fun p ->
            String_util.contains (S.param p "text") "Late pause receipt.")
          posts
      in
      eq "one actual late receipt" 1 (List.length receipts);
      Alcotest.(check string)
        "actual receipt uses original root" parent_ts
        (S.param (List.hd receipts) "thread_ts");
      check "paired representations share completed identity" true
        (Journal.remember e.db e.order journal_message = "complete");
      check "pause invalidates another active sweep" true
        (SS.sweep e ctx owner validated
        = Error Slack_standing_order_reconcile_scan.Lease_lost);
      eq "repeated actual sweep does not send again" 2
        (List.length (SS.posts e)))

let membership_unknown () =
  with_env (fun e ->
      let* () = prepare e false in
      install e Paired (pause ());
      let previous = !(e.answer) in
      (e.answer :=
         fun method_ params ->
           if method_ = "users.info" then Lwt.return (503, obj [])
           else previous method_ params);
      let* () = pass e in
      Alcotest.check S.state_t "uncertain member cannot commit pause" T.Active
        (current e).state;
      check "uncertain member keeps admission closed" false (covered e);
      eq "one unresolved exact pause" 1
        (List.length (Journal.messages e.f.db e.order));
      e.answer := previous;
      let* () = advance e.clock (e.clock.now +. 300.) in
      let* () = pass e in
      Alcotest.check S.state_t "recovered member check commits pending pause"
        T.Paused (current e).state;
      eq "uncertainty retry retains one identity" 1 (journal_count e);
      no_relay e;
      Lwt.return_unit)

let nonmember () =
  with_env (fun e ->
      let* () = prepare e false in
      S.remove_member e.f.world "U456";
      install e Paired (pause ());
      let* () = pass e in
      Alcotest.check S.state_t "nonmember cannot pause" T.Active
        (current e).state;
      check "exact nonmember decision retained" true
        (Journal.completed_outcome e.f.db e.order journal_message
        = Some "member_not_member");
      no_relay e;
      Lwt.return_unit)

let rejected_message name change () =
  with_env (fun e ->
      let* () = prepare e false in
      install e History (change (pause ()));
      let* () = pass e in
      Alcotest.check S.state_t (name ^ " cannot pause") T.Active
        (current e).state;
      eq "no excluded journal identity" 0 (journal_count e);
      eq "no excluded notification" 0 (count e "chat.postMessage");
      no_relay e;
      Lwt.return_unit)

let rejected_member name edit () =
  with_env (fun e ->
      let* () = prepare e false in
      install e Paired (pause ());
      let previous = !(e.answer) in
      (e.answer :=
         fun method_ params ->
           if method_ = "users.info" then
             let+ status, json = previous method_ params in
             (status, set "user" (edit (P.get "user" json)) json)
           else previous method_ params);
      let* () = pass e in
      Alcotest.check S.state_t (name ^ " cannot pause") T.Active
        (current e).state;
      eq "one refused message despite paired representation" 1 (journal_count e);
      check "refused message durably settled" true
        (Journal.remember e.f.db e.order journal_message = "complete");
      no_relay e;
      Lwt.return_unit)

let suite =
  [
    S.case "paired broadcast posts one actual receipt in original thread"
      receipt_identity;
    S.case "unknown broadcast membership holds until verified"
      membership_unknown;
    S.case "nonmember broadcast cannot pause" nonmember;
  ]
  @ List.concat_map
      (fun maintenance ->
        List.concat_map
          (fun broadcast ->
            List.map
              (fun (surface_name, surface) ->
                S.case
                  ((if maintenance then "maintenance" else "gap")
                  ^ (if broadcast then " broadcast " else " plain ")
                  ^ surface_name ^ " pause closes before dispatch")
                  (accepted ~maintenance ~broadcast surface))
              [ ("history", History); ("replies", Replies); ("paired", Paired) ])
          [ false; true ])
      [ false; true ]
  @ List.map
      (fun (name, change) ->
        S.case ("exclude " ^ name) (rejected_message name change))
      [
        ("bot_id", set "bot_id" (str "BFOREIGN"));
        ("Slackbot", set "user" (str "USLACKBOT"));
        ("invalid actor", set "user" (str "not-a-user"));
        ("edit", set "edited" (obj [ ("ts", str pause_ts) ]));
        ("message_changed", set "subtype" (str "message_changed"));
        ("file_share", set "subtype" (str "file_share"));
        ("unsupported subtype", set "subtype" (str "bot_message"));
        ("foreign channel", set "channel" (str "COTHER"));
        ("wrong order binding", set "text" (str "pause SO-2"));
        ("nonexact command", set "text" (str "please pause SO-1"));
        ("missing broadcast root", set "thread_ts" `Null);
        ("malformed broadcast root", set "thread_ts" (str "invalid"));
      ]
  @ List.map
      (fun (name, edit) ->
        S.case ("refuse " ^ name) (rejected_member name edit))
      [
        ("bot actor without bot_id", set "is_bot" (`Bool true));
        ("deleted actor", set "deleted" (`Bool true));
        ("guest actor", set "is_restricted" (`Bool true));
        ("foreign team actor", set "team_id" (str "TOTHER"));
      ]
