(* Operator CLI (spec §6.8 CLI parity; Decisions 11-12; rulings F9, F10, F16):
   reads are open, mutations need CLAWQ_ADMIN and are audited, pause and cancel
   post nothing, and resume runs the full Resume preflight. *)
module Cli = Slack_standing_order_cli
module S = Test_slack_standing_order_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module Render = Slack_standing_order_render
module Pause = Slack_standing_order_pause

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)
let check_opt = Alcotest.(check (option string))
let contains = Test_helpers.string_contains

(* A denial runs Command_bridge_room_common.require_admin_audited, which opens
   the global get_db () under $CLAWQ_HOME or ~/.clawq. Every case therefore runs
   with CLAWQ_ADMIN cleared and HOME and CLAWQ_HOME pointing at a temp dir, so
   no case reads a live config or writes into a live memory.db. *)
let with_cli_home f =
  let old_admin = Sys.getenv_opt "CLAWQ_ADMIN" in
  Unix.putenv "CLAWQ_ADMIN" "";
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "CLAWQ_ADMIN" (Option.value old_admin ~default:""))
    (fun () -> Test_helpers.with_temp_home f)

let case name f = S.case name (fun () -> with_cli_home f)
let run ?http db args = Cli.cmd ~db ~config:(S.config ()) ?http args

let find db (o : T.order) =
  match Store.find db o.id with
  | Some o -> o
  | None -> Alcotest.fail ("no order " ^ o.id)

let lines out = String.split_on_char '\n' out

let ledger db event_type =
  Room_activity_ledger.query ~db ~room_id:"slack:C123" ~event_type ()

let metadata_action (e : Room_activity_ledger.event) =
  match Slack_huddles.get "action" e.metadata with `String s -> s | _ -> ""

let no_calls (m : S.mock) =
  check_int "no Slack calls" 0 (List.length !(m.calls))

let test_list _dir =
  let db = S.fresh_db () in
  check_string "empty store" "No standing orders." (run db [ "orders"; "list" ]);
  let a = S.seed db () in
  let b = S.seed db ~channel:"C456" () in
  let c = S.seed db ~channel:"C789" ~state:T.Cancelled () in
  let out = run db [ "orders"; "list" ] in
  List.iter
    (fun (o : T.order) -> check_bool ("lists " ^ o.id) true (contains out o.id))
    [ a; b; c ];
  check_bool "shows SO-1" true (contains out "SO-1");
  check_bool "shows the target label" true (contains out "Claude (Anthropic)");
  check_bool "cancelled orders are listed" true (contains out "cancelled");
  check_bool "no instruction text" false
    (contains out "Update the project docs.");
  let only = run db [ "orders"; "list"; "--channel"; "C456" ] in
  check_bool "filter keeps C456" true (contains only b.id);
  check_bool "filter drops C123" false (contains only a.id)

let test_show_and_fires _dir =
  let db = S.fresh_db () in
  let o = S.seed db () in
  let show = run db [ "orders"; "show"; o.id ] in
  check_bool "show names the id" true (contains show o.id);
  check_bool "show names SO-1" true (contains show "SO-1");
  check_bool "instruction line" true
    (List.mem "Instruction (untrusted): Update the project docs." (lines show));
  List.iter
    (fun i -> ignore (S.fire db o ~room:(Printf.sprintf "room-%02d" i) ()))
    (List.init 25 (fun i -> i + 1));
  let rows out = List.filter (fun l -> contains l "room-") (lines out) in
  let fires = rows (run db [ "orders"; "fires"; o.id ]) in
  check_int "twenty by default" 20 (List.length fires);
  check_bool "newest first" true (contains (List.hd fires) "room-25");
  check_bool "twentieth is room-06" true
    (contains (List.nth fires 19) "room-06");
  check_int "--limit 3" 3
    (List.length (rows (run db [ "orders"; "fires"; o.id; "--limit"; "3" ])));
  check_bool "--limit 0 is an error" true
    (String.starts_with ~prefix:"Error:"
       (run db [ "orders"; "fires"; o.id; "--limit"; "0" ]));
  check_bool "show reports the last fire" true
    (List.mem "Last fire: detected in room-25"
       (List.map
          (fun l ->
            match String.index_opt l '(' with
            | Some i -> String.trim (String.sub l 0 i)
            | None -> l)
          (lines (run db [ "orders"; "show"; o.id ]))));
  List.iter
    (fun sub ->
      check_string (sub ^ ": unknown id")
        "No standing order with id so_missing."
        (run db [ "orders"; sub; "so_missing" ]))
    [ "show"; "fires"; "pause"; "resume"; "cancel" ]

let test_mutations_need_admin dir =
  let db = S.fresh_db () and m = S.mock () in
  let o = S.seed db () in
  List.iter
    (fun op ->
      check_bool (op ^ " is refused") true
        (contains
           (run ~http:m.http db [ "orders"; op; o.id ])
           "requires admin privileges"))
    [ "pause"; "resume"; "cancel" ];
  let unchanged = find db o in
  Alcotest.check S.state_t "still active" T.Active unchanged.state;
  check_int "epoch unchanged" o.epoch unchanged.epoch;
  no_calls m;
  let path = Dot_dir.db_path () in
  check_bool "denials land in the temp home" true
    (String.starts_with ~prefix:dir path);
  let home_db = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close home_db : bool))
    (fun () ->
      let denied =
        Room_activity_ledger.query ~db:home_db ~room_id:"slack:C123"
          ~event_type:"admin_denied" ()
      in
      Alcotest.(check (list string))
        "one denial per mutation"
        [
          "standing_order_pause";
          "standing_order_resume";
          "standing_order_cancel";
        ]
        (List.map metadata_action denied);
      List.iter
        (fun (e : Room_activity_ledger.event) ->
          check_string "denial actor" "cli" e.actor)
        denied);
  Unix.putenv "CLAWQ_ADMIN" "1";
  let out = run ~http:m.http db [ "orders"; "pause"; o.id ] in
  let paused = find db o in
  Alcotest.check S.state_t "paused" T.Paused paused.state;
  check_opt "paused by cli" (Some "cli") paused.paused_by;
  check_string "pause receipt"
    (Pause.receipt ~display_no:o.display_no ~by:"cli"
       (Pause.Paused { order = paused; in_flight = Some 0 }))
    out;
  check_bool "no bogus mention" false (contains out "<@cli>");
  (match ledger db "standing_order_paused" with
  | [ e ] -> check_string "pause ledger actor" "cli" e.actor
  | events ->
      Alcotest.failf "expected one pause event, got %d" (List.length events));
  check_string "pause again"
    (Pause.receipt ~display_no:o.display_no ~by:"cli"
       (Pause.Already_paused paused))
    (run ~http:m.http db [ "orders"; "pause"; o.id ]);
  no_calls m

let test_unknown_count_receipt _dir =
  Unix.putenv "CLAWQ_ADMIN" "1";
  let db = S.fresh_db () and m = S.mock () in
  let o = S.seed db () in
  let fire = S.fire db o ~state:T.Posting ~room:"R1" () in
  Sqlite3.create_fun0 db "count" (fun () -> failwith "fixture count failure");
  let out = run ~http:m.http db [ "orders"; "pause"; o.id ] in
  let after = find db o in
  Alcotest.check S.state_t "paused" T.Paused after.state;
  check_int "epoch bumped" (o.epoch + 1) after.epoch;
  check_string "unknown count receipt"
    "Standing order SO-1 is paused (by cli). Huddles that become ready while \
     it is paused will never be relayed, even after a resume. Relays may \
     already be in flight."
    out;
  check_string "same shared receipt" out
    (Pause.receipt ~display_no:1 ~by:"cli"
       (Pause.Paused { order = after; in_flight = None }));
  check_bool "repost forbidden" true
    (S.fire_flags db fire.id = (T.Posting, true, false));
  no_calls m

let test_pause_withdraws_pending _dir =
  Unix.putenv "CLAWQ_ADMIN" "1";
  let db = S.fresh_db () and m = S.mock () in
  let o = S.seed db ~state:T.Pending_confirmation () in
  let out = run ~http:m.http db [ "orders"; "pause"; o.id ] in
  let after = find db o in
  Alcotest.check S.state_t "withdrawn" T.Cancelled after.state;
  check_opt "withdrawn by cli" (Some "cli") after.cancelled_by;
  check_string "withdraw receipt"
    (Pause.receipt ~display_no:o.display_no ~by:"cli" (Pause.Withdrawn after))
    out;
  check_int "withdraw ledger" 1
    (List.length (ledger db "standing_order_withdrawn"));
  no_calls m

let announce db (o : T.order) =
  let card =
    Store.insert_card db ~kind:T.Pause_button ~order:o ~created_by:"U123"
      ~now:S.now ()
  in
  Store.set_card_message_ts db ~card_id:card.id "100.000001"

let test_resume_unannounced _dir =
  Unix.putenv "CLAWQ_ADMIN" "1";
  let db = S.fresh_db () and m = S.mock () in
  let o = S.seed db ~state:T.Paused () in
  let out = run ~http:m.http db [ "orders"; "resume"; o.id ] in
  check_bool "actionable refusal" true
    (contains out
       "resume it from Slack so the channel gets its activation notice");
  Alcotest.check S.state_t "remains paused" T.Paused (find db o).state;
  no_calls m

let test_resume_preflight _dir =
  Unix.putenv "CLAWQ_ADMIN" "1";
  let db = S.fresh_db () and m = S.mock () in
  let o = S.seed db ~state:T.Paused () in
  announce db o;
  let out = run ~http:m.http db [ "orders"; "resume"; o.id ] in
  check_string "resumed"
    "Standing order SO-1 resumed (cli). Huddles that became ready while it was \
     paused will not be relayed."
    out;
  let after = find db o in
  Alcotest.check S.state_t "active" T.Active after.state;
  check_opt "active_since is the CLI's time"
    (Some (Store.ts_of_unix S.now))
    after.active_since;
  check_int "epoch bumped" (o.epoch + 1) after.epoch;
  let called = S.methods_called m in
  check_bool "the preflight asked Slack" true (List.mem "auth.test" called);
  check_bool "nothing posted" false
    (List.exists
       (fun meth -> meth = "chat.postMessage" || meth = "chat.update")
       called);
  (match ledger db "standing_order_resumed" with
  | [ e ] -> check_string "resume ledger actor" "cli" e.actor
  | events ->
      Alcotest.failf "expected one resume event, got %d" (List.length events));
  check_string "resume an active order"
    "Standing order SO-1 is already active; there is nothing to resume."
    (run ~http:m.http db [ "orders"; "resume"; o.id ]);
  let db = S.fresh_db () and m = S.mock () in
  let o = S.seed db ~state:T.Paused () in
  announce db o;
  S.override m "conversations.info" (fun params ->
      let response = S.default_response m.world "conversations.info" params in
      match response with
      | `Assoc fields -> (
          match List.assoc_opt "channel" fields with
          | Some (`Assoc channel) ->
              ( 200,
                `Assoc
                  (List.remove_assoc "channel" fields
                  @ [
                      ( "channel",
                        `Assoc
                          (List.remove_assoc "is_ext_shared" channel
                          @ [ ("is_ext_shared", `Bool true) ]) );
                    ]) )
          | _ -> Alcotest.fail "conversations.info has no channel")
      | _ -> Alcotest.fail "conversations.info is not an object");
  check_string "shared channel"
    "Not resumed: this channel is shared with another organisation."
    (run ~http:m.http db [ "orders"; "resume"; o.id ]);
  Alcotest.check S.state_t "still paused" T.Paused (find db o).state

let test_cancel _dir =
  Unix.putenv "CLAWQ_ADMIN" "1";
  let db = S.fresh_db () and m = S.mock () in
  let o = S.seed db () in
  let out = run ~http:m.http db [ "orders"; "cancel"; o.id ] in
  let after = find db o in
  Alcotest.check S.state_t "cancelled" T.Cancelled after.state;
  check_opt "cancelled by cli" (Some "cli") after.cancelled_by;
  check_string "cancel receipt" (Render.cancel_receipt after ~by:"cli") out;
  check_bool "no bogus mention" false (contains out "<@cli>");
  (match ledger db "standing_order_cancelled" with
  | [ e ] -> check_string "cancel ledger actor" "cli" e.actor
  | events ->
      Alcotest.failf "expected one cancel event, got %d" (List.length events));
  check_string "cancel again"
    "Standing order SO-1 was already cancelled; nothing changed."
    (run ~http:m.http db [ "orders"; "cancel"; o.id ]);
  no_calls m

(* Exercise the same handler as [cmd_mutate], after its lookup-time sweep.
   Calling [cmd] here would hide the missing Resume-boundary expiration. *)
let test_resume_expired_conflict _dir =
  let db = S.fresh_db () and m = S.mock () in
  let o = S.seed db ~state:T.Suspended () in
  announce db o;
  let competitor = S.seed db ~state:T.Pending_confirmation () in
  Sql_util.exec_with_params db
    "UPDATE slack_standing_orders SET pending_expires_at = ? WHERE id = ?"
    [ Sqlite3.Data.FLOAT (S.now -. 1.); Sqlite3.Data.TEXT competitor.id ];
  let out = Cli.cmd_resume db ~config:(S.config ()) ~http:m.http ~now:S.now o in
  Alcotest.check S.state_t "competitor cancelled" T.Cancelled
    (find db competitor).state;
  Alcotest.check S.state_t "resumed active" T.Active (find db o).state;
  check_bool "resumed receipt" true (contains out "resumed (cli)");
  check_bool "authority checked" true
    (List.mem "auth.test" (S.methods_called m))

let test_resume_expiration_failure _dir =
  let db = S.fresh_db () and m = S.mock () in
  let o = S.seed db ~state:T.Suspended () in
  announce db o;
  let competitor = S.seed db ~state:T.Pending_confirmation () in
  Sql_util.exec_with_params db
    "UPDATE slack_standing_orders SET pending_expires_at = ? WHERE id = ?"
    [ Sqlite3.Data.FLOAT (S.now -. 1.); Sqlite3.Data.TEXT competitor.id ];
  Sqlite3.Rc.check
    (Sqlite3.exec db
       "CREATE TRIGGER fail_pending_expiration BEFORE UPDATE OF state ON \
        slack_standing_orders WHEN OLD.state = 'pending_confirmation' AND \
        NEW.state = 'cancelled' BEGIN SELECT RAISE(ABORT, 'fixture expiration \
        failure'); END");
  let out = Cli.cmd_resume db ~config:(S.config ()) ~http:m.http ~now:S.now o in
  Alcotest.check S.state_t "still suspended" T.Suspended (find db o).state;
  Alcotest.check S.state_t "competitor remains" T.Pending_confirmation
    (find db competitor).state;
  check_string "uniqueness refusal"
    ("Not resumed: "
    ^ Render.reason_text
        (Slack_standing_order_grant.Duplicate_order competitor.display_no)
    ^ ".")
    out;
  no_calls m

let test_minimal_stub _dir =
  check_bool "minimal build refuses" true
    (contains
       (Command_bridge_min.handle [ "slack"; "orders"; "list" ])
       "not available in the minimal build")

let test_usage _dir =
  let db = S.fresh_db () in
  check_string "orders alone" Cli.usage (run db [ "orders" ]);
  check_string "no arguments" Cli.usage (run db []);
  check_string "no create from the CLI" Cli.usage
    (run db [ "orders"; "create"; "x" ])

let suite =
  [
    case "list shows every order with opaque ids" test_list;
    case "show and fires" test_show_and_fires;
    case "mutations need CLAWQ_ADMIN" test_mutations_need_admin;
    case "unknown count receipt" test_unknown_count_receipt;
    case "cli pause withdraws a pending order" test_pause_withdraws_pending;
    case "CLI refuses unannounced resume" test_resume_unannounced;
    case "cli resume runs the full preflight" test_resume_preflight;
    case "CLI resume expires stale pending conflict"
      test_resume_expired_conflict;
    case "resume expiration failure preserves uniqueness"
      test_resume_expiration_failure;
    case "cancel" test_cancel;
    case "minimal build stub" test_minimal_stub;
    case "usage" test_usage;
  ]
