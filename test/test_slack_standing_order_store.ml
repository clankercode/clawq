(* Slack_standing_order_store: per-channel numbering, the one-live index,
   pending expiry, epoch-fenced transitions and the fire sweep, fire and card
   ledgers, fail-closed decoding. *)
module S = Test_slack_standing_order_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let check_opt = Alcotest.(check (option string))
let check_state = Alcotest.check S.state_t

let check_flags =
  Alcotest.check (Alcotest.triple S.fire_state_t Alcotest.bool Alcotest.bool)

let ok what = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: unexpected Error %s" what e

let order db id =
  match Store.find db id with
  | Some o -> o
  | None -> Alcotest.failf "order %s not found" id

let count db table =
  match
    Sql_util.query_rows db ("SELECT COUNT(*) FROM " ^ table) ~bind:ignore
      ~of_stmt:(fun stmt -> Sql_util.int_column stmt 0)
  with
  | [ n ] -> n
  | _ -> Alcotest.fail "count failed"

let resume_error_to_string = function
  | Store.Not_resumable s -> "Not_resumable " ^ T.state_to_string s
  | Store.Stale_epoch -> "Stale_epoch"
  | Store.Duplicate_live_on_resume n -> Printf.sprintf "Duplicate_live %d" n
  | Store.Resume_failed e -> "Resume_failed " ^ e

let resume ?(active_since = "1700000500.000000") db (o : T.order)
    ~expected_epoch =
  Store.resume db ~order_id:o.id ~expected_epoch ~active_since
    ~bot_user_id:"UBOT" ~bot_id:"BOWN" ~now:S.now

let expect_resume_error name expected r =
  match r with
  | Ok _ -> Alcotest.failf "%s: resumed" name
  | Error e ->
      check_string name
        (resume_error_to_string expected)
        (resume_error_to_string e)

let deactivate db (o : T.order) op =
  ok "deactivate" (Store.deactivate db ~order_id:o.id op ~now:S.now)

let other_target =
  { S.claude with app_id = "AOTHER"; user_id = "UOTHER"; bot_id = "BOTHER" }

let test_display_numbers () =
  let db = S.fresh_db () in
  let a = S.seed db () in
  let b = S.seed db ~target:other_target () in
  let c = S.seed db ~channel:"C999" () in
  Alcotest.(check (list int))
    "per-channel ordinals" [ 1; 2; 1 ]
    (List.map (fun (o : T.order) -> o.display_no) [ a; b; c ]);
  check_string "display name" "SO-2" (T.display_name b)

let test_one_live () =
  let db = S.fresh_db () in
  let o = S.seed db () in
  (match
     Store.insert_pending db ~id:"so_dup" ~team_id:"T123" ~api_app_id:"A123"
       ~channel_id:"C123" ~grantor_user_id:"U456" ~action:(S.relay_action ())
       ~management_thread_ts:"1.1" ~now:S.now ()
   with
  | Error (Store.Duplicate_live { display_no }) ->
      check_int "names the live order" 1 display_no
  | Error (Store.Insert_failed e) -> Alcotest.fail ("Insert_failed " ^ e)
  | Ok _ -> Alcotest.fail "a second live order was inserted");
  check_bool "no duplicate row" true (Store.find db "so_dup" = None);
  check_int "one row" 1 (count db "slack_standing_orders");
  check_bool "cancelled" true (deactivate db o (Store.Cancel "U123")).changed;
  let next = S.seed db () in
  check_int "numbers are never reused" 2 next.display_no

let test_pending_expiry () =
  let db = S.fresh_db () in
  let o = S.seed db ~state:T.Pending_confirmation () in
  let active = S.seed db ~channel:"C999" () in
  check_int "not yet expired" 0 (Store.expire_pending db ~now:(S.now +. 599.));
  check_int "expired" 1 (Store.expire_pending db ~now:(S.now +. 601.));
  let after = order db o.id in
  check_state "withdrawn" T.Cancelled after.state;
  check_opt "by" (Some "expired") after.cancelled_by;
  check_int "epoch bumped" (o.epoch + 1) after.epoch;
  check_state "active orders untouched" T.Active (order db active.id).state;
  check_int "nothing left to expire" 0
    (Store.expire_pending db ~now:(S.now +. 601.))

let activate ?(active_since = "1700000000.000100") ?(bot_user_id = "UBOT")
    ?(bot_id = "BOWN") ?(now = S.now) db (o : T.order) =
  Store.activate_in_tx db ~order_id:o.id ~active_since ~confirmed_by:"U123"
    ~bot_user_id ~bot_id ~now

let expect_error name = function
  | Ok _ -> Alcotest.failf "%s: expected Error" name
  | Error _ -> ()

let test_activation_cas () =
  let db = S.fresh_db () in
  let o = S.seed db ~state:T.Pending_confirmation () in
  let a = ok "first activation" (activate db o) in
  check_state "active" T.Active a.state;
  check_opt "active_since" (Some "1700000000.000100") a.active_since;
  check_int "epoch" 1 a.epoch;
  check_int "revision" 1 a.revision;
  check_opt "bot user" (Some "UBOT") a.bot_user_id;
  check_opt "bot" (Some "BOWN") a.bot_id;
  check_opt "confirmed by" (Some "U123") a.confirmed_by;
  check_bool "pending window cleared" true (a.pending_expires_at = None);
  check_bool "stored" true (Store.find db o.id = Some a);
  expect_error "second activation" (activate db o);
  check_int "epoch unchanged" 1 (order db o.id).epoch;
  let p = S.seed db ~channel:"C999" ~state:T.Pending_confirmation () in
  expect_error "empty bot user" (activate ~bot_user_id:"" db p);
  expect_error "bad bot id" (activate ~bot_id:"UBOWN" db p);
  expect_error "expired window" (activate ~now:(S.now +. 600.) db p);
  check_state "still pending" T.Pending_confirmation (order db p.id).state

let test_activation_rejects_bad_ts () =
  let db = S.fresh_db () in
  let o = S.seed db ~state:T.Pending_confirmation () in
  List.iter
    (fun ts -> expect_error ts (activate ~active_since:ts db o))
    [ ""; "abc"; "0.000000"; "-1.5" ];
  let after = order db o.id in
  check_state "still pending" T.Pending_confirmation after.state;
  check_bool "no active_since" true (after.active_since = None);
  check_int "epoch unchanged" 0 after.epoch

let test_pause_sweep () =
  let db = S.fresh_db () in
  let o = S.seed db () in
  let fire ?claim_until state room =
    (S.fire db o ~state ?claim_until ~room ()).id
  in
  let detected = fire T.Detected "R1" in
  let live = fire ~claim_until:(S.now +. 60.) T.Fetching "R2" in
  let stale = fire ~claim_until:(S.now -. 1.) T.Fetching "RSTALE" in
  let uploading = fire T.Uploading "R4" in
  let dispatched =
    List.map
      (fun st -> (st, fire st ("R_" ^ T.fire_state_to_string st)))
      T.[ Posting; Posting_unknown; Posted; Acknowledged; Picked_up ]
  in
  let no_pickup = fire T.No_pickup "R10" in
  let change = deactivate db o (Store.Pause "U456") in
  check_bool "changed" true change.changed;
  check_int "in flight" 2 change.in_flight;
  check_int "epoch" (o.epoch + 1) change.after.epoch;
  check_int "revision" (o.revision + 1) change.after.revision;
  check_state "paused" T.Paused change.after.state;
  check_opt "paused by" (Some "U456") change.after.paused_by;
  check_bool "before is the old row" true (change.before = o);
  check_bool "after is stored" true (Store.find db o.id = Some change.after);
  let skipped = (T.Skipped_inactive, false, false) in
  check_flags "detected" skipped (S.fire_flags db detected);
  check_flags "stale fetching" skipped (S.fire_flags db stale);
  check_flags "uploading" skipped (S.fire_flags db uploading);
  check_flags "live claim" (T.Fetching, false, true) (S.fire_flags db live);
  List.iter
    (fun (st, id) ->
      check_flags
        (T.fire_state_to_string st)
        (st, true, false) (S.fire_flags db id))
    dispatched;
  check_flags "terminal untouched"
    (T.No_pickup, false, false)
    (S.fire_flags db no_pickup);
  let reasons =
    Store.list_fires db ~order_id:o.id ~limit:20
    |> List.filter (fun (f : T.fire) -> f.state = T.Skipped_inactive)
    |> List.map (fun (f : T.fire) -> f.reason)
  in
  Alcotest.(check (list (option string)))
    "skip reason"
    (List.init 3 (fun _ -> Some "order_inactive"))
    reasons

let test_deactivate_idempotent () =
  let db = S.fresh_db () in
  let o = S.seed db () in
  let first = deactivate db o (Store.Pause "U456") in
  let fired = S.fire db o ~state:T.Detected ~room:"R1" () in
  let again = deactivate db o (Store.Pause "U789") in
  check_bool "unchanged" false again.changed;
  check_int "same epoch" first.after.epoch again.after.epoch;
  check_bool "row untouched" true (Store.find db o.id = Some first.after);
  check_opt "first pauser kept" (Some "U456") again.after.paused_by;
  check_flags "no second sweep" (T.Detected, false, false)
    (S.fire_flags db fired.id)

(* Every (op, state) pair outside the four transitions changes nothing. *)
let test_transition_table () =
  let ops =
    [
      ("pause", Store.Pause "U9");
      ("suspend", Store.Suspend "r");
      ("cancel", Store.Cancel "U9");
      ("withdraw", Store.Withdraw "U9");
    ]
  in
  let expected op (st : T.state) =
    match (op, st) with
    | "pause", T.Active -> Some T.Paused
    | "suspend", T.Active -> Some T.Suspended
    | "cancel", (T.Pending_confirmation | T.Active | T.Paused | T.Suspended) ->
        Some T.Cancelled
    | "withdraw", T.Pending_confirmation -> Some T.Cancelled
    | _ -> None
  in
  List.iter
    (fun st ->
      List.iter
        (fun (name, op) ->
          let db = S.fresh_db () in
          let o = S.seed db ~state:st () in
          let c = deactivate db o op in
          let label = name ^ " on " ^ T.state_to_string st in
          match expected name st with
          | Some next ->
              check_bool label true c.changed;
              check_state label next c.after.state;
              check_int label (o.epoch + 1) (order db o.id).epoch
          | None ->
              check_bool label false c.changed;
              check_bool label true (Store.find db o.id = Some o))
        ops)
    T.[ Pending_confirmation; Active; Paused; Suspended; Cancelled ]

let test_deactivate_in_caller_tx () =
  let db = S.fresh_db () in
  let o = S.seed db () in
  Sql_util.exec_exn db "BEGIN IMMEDIATE";
  let c =
    ok "in tx"
      (Store.deactivate_in_tx db ~order_id:o.id (Store.Pause "U456") ~now:S.now)
  in
  check_bool "changed inside the caller's transaction" true c.changed;
  expect_error "deactivate refuses to nest"
    (Store.deactivate db ~order_id:o.id (Store.Cancel "U456") ~now:S.now);
  check_state "caller's transaction still open" T.Paused (order db o.id).state;
  Sql_util.exec_exn db "ROLLBACK";
  check_bool "rolled back with the caller" true (Store.find db o.id = Some o)

let test_resume () =
  let db = S.fresh_db () in
  let o = S.seed db ~state:T.Paused () in
  let e = o.epoch in
  expect_resume_error "stale" Store.Stale_epoch
    (resume db o ~expected_epoch:(e - 1));
  (match resume ~active_since:"" db o ~expected_epoch:e with
  | Error (Store.Resume_failed _) -> ()
  | r ->
      Alcotest.failf "empty active_since: %s"
        (match r with Ok _ -> "resumed" | Error e -> resume_error_to_string e));
  check_bool "still paused" true (Store.find db o.id = Some o);
  match resume db o ~expected_epoch:e with
  | Error err -> Alcotest.fail (resume_error_to_string err)
  | Ok r ->
      check_state "active" T.Active r.state;
      check_int "epoch" (e + 1) r.epoch;
      check_int "revision" (o.revision + 1) r.revision;
      check_opt "paused_by cleared" None r.paused_by;
      check_opt "active_since reset" (Some "1700000500.000000") r.active_since;
      check_bool "stored" true (Store.find db o.id = Some r)

let test_resume_duplicate () =
  let db = S.fresh_db () in
  let a = S.seed db ~state:T.Suspended () in
  let b = S.seed db () in
  expect_resume_error "duplicate" (Store.Duplicate_live_on_resume b.display_no)
    (resume db a ~expected_epoch:a.epoch);
  check_bool "still suspended" true (Store.find db a.id = Some a)

let test_cancel_terminal () =
  let db = S.fresh_db () in
  let o = S.seed db () in
  let c = deactivate db o (Store.Cancel "U123") in
  check_state "cancelled" T.Cancelled c.after.state;
  check_opt "by" (Some "U123") c.after.cancelled_by;
  let p = deactivate db o (Store.Pause "U456") in
  check_bool "pause changes nothing" false p.changed;
  check_int "same epoch" c.after.epoch p.after.epoch;
  expect_resume_error "terminal" (Store.Not_resumable T.Cancelled)
    (resume db o ~expected_epoch:c.after.epoch)

let test_spoken_defaults () =
  let db = S.fresh_db () in
  let o = S.seed db ~state:T.Pending_confirmation () in
  check_bool "spoken requests" true o.spoken_requests;
  check_int "card version" T.current_card_version o.card_version;
  check_int "daily cap" Store.default_max_fires_per_day o.max_fires_per_day;
  check_string "management channel" "C123" o.management_channel_id;
  check_string "management thread" "100.000001" o.management_thread_ts;
  check_bool "pending window" true
    (o.pending_expires_at = Some (S.now +. Store.pending_ttl_seconds));
  check_int "epoch" 0 o.epoch;
  check_int "revision" 1 o.revision;
  check_bool "trigger" true (o.trigger = T.Huddle_notes_ready);
  check_bool "action" true (o.action = S.relay_action ())

let test_channel_scoped_lookup () =
  let db = S.fresh_db () in
  let o = S.seed db ~channel:"C999" () in
  check_bool "other channel" true
    (Store.find_by_display db ~team_id:"T123" ~channel_id:"C123" 1 = None);
  check_bool "other team" true
    (Store.find_by_display db ~team_id:"T999" ~channel_id:"C999" 1 = None);
  check_bool "own channel" true
    (Store.find_by_display db ~team_id:"T123" ~channel_id:"C999" 1 = Some o)

let test_live_conflict () =
  let db = S.fresh_db () in
  let a = S.seed db ~state:T.Paused () in
  let find excluding =
    Store.find_live_conflict db ~team_id:"T123" ~channel_id:"C123"
      ~target_app_id:"ACLAUDE" ~excluding
  in
  check_bool "itself is excluded" true (find a.id = None);
  check_bool "found from elsewhere" true (find "so_other" = Some a);
  ignore (deactivate db a (Store.Cancel "U123") : Store.state_change);
  check_bool "cancelled is not live" true (find "so_other" = None)

let test_fire_uniqueness () =
  let db = S.fresh_db () in
  let a = S.seed db () in
  let b = S.seed db ~target:other_target () in
  let insert ?canvas ?transcript ?(date_start = "1700000000") o room =
    Store.insert_fire db ~order:o ~room_id:room ~date_start
      ?canvas_file_id:canvas ?transcript_file_id:transcript ~now:S.now ()
  in
  let fires () = count db "slack_standing_order_fires" in
  (match insert a "R1" with
  | Ok (Some f) ->
      check_bool "detected" true (f.state = T.Detected);
      check_int "order epoch" a.epoch f.epoch
  | _ -> Alcotest.fail "first insert");
  (match insert a "R1" with
  | Ok None -> ()
  | _ -> Alcotest.fail "the same huddle is not a second fire");
  check_int "one row" 1 (fires ());
  ignore (ok "transcript on A" (insert ~transcript:"F9" a "R2"));
  let n = fires () in
  expect_error "transcript on B" (insert ~transcript:"F9" b "R2");
  check_int "transcript conflict wrote nothing" n (fires ());
  ignore (ok "canvas" (insert ~canvas:"F1" ~date_start:"1" a "R3"));
  let n = fires () in
  expect_error "canvas twice" (insert ~canvas:"F1" ~date_start:"2" a "R3");
  check_int "canvas conflict wrote nothing" n (fires ());
  expect_error "empty date_start" (insert ~date_start:"" a "R4");
  check_int "empty date_start wrote nothing" n (fires ())

let test_fires_newest_first () =
  let db = S.fresh_db () in
  let o = S.seed db () in
  List.iter
    (fun room -> ignore (S.fire db o ~room () : T.fire))
    [ "R1"; "R2"; "R3" ];
  (* Force the same-second tie that rowid must break. *)
  Sql_util.exec_exn db
    "UPDATE slack_standing_order_fires SET created_at = '2023-11-14 22:13:20'";
  Alcotest.(check (list string))
    "newest first" [ "R3"; "R2" ]
    (List.map
       (fun (f : T.fire) -> f.room_id)
       (Store.list_fires db ~order_id:o.id ~limit:2))

let test_dispatch_window () =
  let db = S.fresh_db () in
  let o = S.seed db () in
  ignore
    (S.fire db o ~room:"R1" ~dispatched_at:(S.now -. 100.) ~reposts:1 ()
      : T.fire);
  ignore (S.fire db o ~room:"R2" ~dispatched_at:(S.now -. 90000.) () : T.fire);
  ignore (S.fire db o ~room:"R3" () : T.fire);
  check_int "dispatches in the last day" 2
    (Store.dispatches_since db ~order_id:o.id ~since:(S.now -. 86400.))

let show_card (c : T.card) =
  let o = Option.value ~default:"-" in
  String.concat "|"
    [
      c.id;
      T.card_kind_to_string c.kind;
      c.order_id;
      c.team_id;
      c.app_id;
      c.channel_id;
      o c.reply_thread_ts;
      o c.message_ts;
      o c.plan_id;
      o c.plan_digest;
      string_of_int c.order_epoch;
      o (Option.map string_of_float c.expires_at);
      o c.used_at;
      c.created_by;
    ]

let test_cards () =
  let db = S.fresh_db () in
  let o = S.seed db ~state:T.Pending_confirmation () in
  let card =
    Store.insert_card db ~kind:T.Confirm_card ~order:o ~created_by:"U123"
      ~reply_thread_ts:"100.000001" ~plan_id:"plan_1" ~plan_digest:"digest_1"
      ~expires_at:(S.now +. 600.) ~now:S.now ()
  in
  check_bool "card id" true (String.starts_with ~prefix:"soc_" card.id);
  Store.set_card_message_ts db ~card_id:card.id "100.000002";
  let expected : T.card =
    {
      id = card.id;
      kind = T.Confirm_card;
      order_id = o.id;
      team_id = "T123";
      app_id = "A123";
      channel_id = "C123";
      reply_thread_ts = Some "100.000001";
      message_ts = Some "100.000002";
      plan_id = Some "plan_1";
      plan_digest = Some "digest_1";
      order_epoch = o.epoch;
      expires_at = Some (S.now +. 600.);
      used_at = None;
      created_by = "U123";
    }
  in
  (match Store.find_card db card.id with
  | Some found ->
      check_string "every field" (show_card expected) (show_card found)
  | None -> Alcotest.fail "card not found");
  check_bool "first use" true (Store.use_card db ~card_id:card.id ~now:S.now);
  check_bool "second use" false (Store.use_card db ~card_id:card.id ~now:S.now);
  check_bool "used_at set" true
    (match Store.find_card db card.id with
    | Some c -> c.used_at <> None
    | None -> false);
  check_bool "unknown card" true (Store.find_card db "soc_missing" = None)

let test_undecodable_rows () =
  let db = S.fresh_db () in
  let bad = S.seed db () in
  let good = S.seed db ~channel:"C999" () in
  Sql_util.exec_with_params db
    "UPDATE slack_standing_orders SET action_json = '{}' WHERE id = ?"
    [ Sqlite3.Data.TEXT bad.id ];
  check_bool "find skips it" true (Store.find db bad.id = None);
  check_bool "display lookup skips it" true
    (Store.find_by_display db ~team_id:"T123" ~channel_id:"C123" 1 = None);
  Alcotest.(check (list string))
    "list skips it" [ good.id ]
    (List.map
       (fun (o : T.order) -> o.id)
       (Store.list db ~include_cancelled:true ()))

let test_instruction_validation () =
  let valid s = Result.is_ok (T.validate_instruction s) in
  let repeat n s = String.concat "" (List.init n (fun _ -> s)) in
  check_bool "1000 emoji" true (valid (repeat 1000 "\xF0\x9F\x98\x80"));
  check_bool "1000 ascii" true (valid (String.make 1000 'a'));
  check_bool "1001 ascii" false (valid (String.make 1001 'a'));
  check_bool "blank" false (valid "   ");
  check_bool "invalid utf-8" false (valid "\xff\xfe");
  check_bool "bell" false (valid "a\x07b");
  check_bool "carriage return" false (valid "a\rb");
  let markup = "line1\nline2 <!channel> & <@U1>" in
  Alcotest.(check (result string string))
    "markup kept verbatim" (Ok markup)
    (T.validate_instruction markup);
  Alcotest.(check (result string string))
    "trimmed" (Ok "a\tb")
    (T.validate_instruction "  a\tb \n")

let test_codecs () =
  List.iter
    (fun s ->
      check_bool (T.state_to_string s) true
        (T.state_of_string (T.state_to_string s) = Some s))
    T.[ Pending_confirmation; Active; Paused; Suspended; Cancelled ];
  check_int "17 fire states" 17 (List.length T.all_fire_states);
  List.iter
    (fun s ->
      check_bool (T.fire_state_to_string s) true
        (T.fire_state_of_string (T.fire_state_to_string s) = Some s))
    T.all_fire_states;
  List.iter
    (fun k ->
      check_bool (T.card_kind_to_string k) true
        (T.card_kind_of_string (T.card_kind_to_string k) = Some k))
    T.[ Confirm_card; Resume_card; Pause_button; Change_card ];
  check_bool "unknown state" true (T.state_of_string "Active" = None);
  check_bool "unknown fire state" true (T.fire_state_of_string "" = None);
  check_string "fire state spelling" "posting_unknown"
    (T.fire_state_to_string T.Posting_unknown);
  let action = S.relay_action ~when_no:T.Skip () in
  check_bool "action round trip" true
    (T.action_of_json (T.action_to_json action) = Ok action);
  let with_field key value =
    match T.action_to_json action with
    | `Assoc fields -> `Assoc ((key, value) :: List.remove_assoc key fields)
    | json -> json
  in
  List.iter
    (fun (name, json) -> expect_error name (T.action_of_json json))
    [
      ("unknown kind", with_field "kind" (`String "post_message"));
      ("bad user id", with_field "target_user_id" (`String "BCLAUDE"));
      ("bad app id", with_field "target_app_id" (`String "aclaude"));
      ("bad bot id", with_field "target_bot_id" (`Int 1));
      ("empty label", with_field "target_label" (`String ""));
      ("blank instruction", with_field "instruction_text" (`String " "));
      ("unknown fallback", with_field "when_no_transcript" (`String "relay"));
      ("not an object", `List []);
    ];
  Alcotest.(check (list (option int)))
    "parse display"
    [ Some 3; Some 3; Some 999999; None; None; None; None; None; None ]
    (List.map T.parse_display
       [
         "SO-3";
         "so-03";
         "SO-999999";
         "SO-0";
         "SO-";
         "SO-1234567";
         "SO-3.";
         "SO 3";
         "SO-+3";
       ])

let raises name f =
  match f () with
  | _ -> Alcotest.failf "%s: a failed read returned a result" name
  | exception exn when Store.is_db_error exn -> ()

(* A read that fails is an error, never "no rows" or a count of 0. *)
let test_strict_reads () =
  let db = S.fresh_db () in
  raises "a failed step is not an empty result" (fun () ->
      Store.query db S.overflow_sql [] (fun _ -> ()));
  raises "a failed aggregate is not 0" (fun () ->
      Store.int_query db S.overflow_sql []);
  let o = S.seed db () in
  S.overflow_dispatches db o;
  raises "dispatches_since" (fun () ->
      Store.dispatches_since db ~order_id:o.id ~since:0.);
  ignore (S.seed db ~channel:"C999" ~state:T.Pending_confirmation () : T.order);
  S.drop_order_column db;
  raises "find" (fun () -> Store.find db o.id);
  raises "find_by_display" (fun () ->
      Store.find_by_display db ~team_id:"T123" ~channel_id:"C123" 1);
  raises "channel_has_orders" (fun () ->
      Store.channel_has_orders db ~team_id:"T123" ~api_app_id:"A123"
        ~channel_id:"C123");
  raises "list" (fun () -> Store.list db ());
  check_int "expire_pending logs a failed read and never raises" 0
    (Store.expire_pending db ~now:(S.now +. 601.))

let suite =
  [
    S.case "display numbers" test_display_numbers;
    S.case "one live" test_one_live;
    S.case "pending expiry" test_pending_expiry;
    S.case "activation cas" test_activation_cas;
    S.case "activation rejects a non-Slack timestamp"
      test_activation_rejects_bad_ts;
    S.case "pause sweep" test_pause_sweep;
    S.case "deactivate idempotent" test_deactivate_idempotent;
    S.case "deactivate transition table" test_transition_table;
    S.case "deactivate_in_tx joins the caller's transaction"
      test_deactivate_in_caller_tx;
    S.case "resume" test_resume;
    S.case "resume duplicate" test_resume_duplicate;
    S.case "cancel terminal" test_cancel_terminal;
    S.case "spoken defaults" test_spoken_defaults;
    S.case "channel scoped lookup" test_channel_scoped_lookup;
    S.case "live conflict lookup" test_live_conflict;
    S.case "fire uniqueness" test_fire_uniqueness;
    S.case "fires newest first" test_fires_newest_first;
    S.case "dispatch window" test_dispatch_window;
    S.case "a failed read is never empty" test_strict_reads;
    S.case "cards" test_cards;
    S.case "undecodable rows" test_undecodable_rows;
    S.case "instruction validation counts code points"
      test_instruction_validation;
    S.case "codecs round trip" test_codecs;
  ]
