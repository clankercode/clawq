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

(* ---- Config ---- *)

(* An allow rule for [slack.com] at [path] ([method_] [None] = any method). *)
let slack_rule ?method_ path : Runtime_config.egress_rule =
  {
    host = "slack.com";
    path = Some path;
    method_;
    action = Allow;
    log_policy = No_log;
  }

let claude_target : Slack_config_types.standing_order_target_config =
  { app_id = "ACLAUDE"; label = "Claude (Anthropic)" }

(* Slack on, [slack.com/api/*] allowed for any method; [~strict_rules] gives a
   Strict egress with exactly those rules instead. *)
let config ?(targets = [ claude_target ])
    ?(creator = Slack_config_types.So_creator_member) ?(enabled = true)
    ?allow_users ?bot_token ?strict_rules () : Runtime_config.t =
  let standing_orders : Slack_config_types.slack_standing_orders_config =
    {
      orders_enabled = enabled;
      allowed_targets = targets;
      creator_policy = creator;
    }
  in
  let slack =
    Test_slack.make_config ?allow_users ?bot_token ~standing_orders ()
  in
  let base = Runtime_config.default in
  {
    base with
    channels = { base.channels with slack = Some slack };
    egress =
      {
        strictness = Strict;
        default_allowlist =
          Option.value strict_rules ~default:[ slack_rule "/api/*" ];
      };
  }

let with_slack (cfg : Runtime_config.t) f : Runtime_config.t =
  match cfg.channels.slack with
  | Some sc -> { cfg with channels = { cfg.channels with slack = Some (f sc) } }
  | None -> Alcotest.fail "S.with_slack: config has no Slack section"

let with_allow_users cfg ids =
  with_slack cfg (fun sc -> { sc with allow_users = ids })

(* Binds room profile ["so"] to [C123]. *)
let profile (cfg : Runtime_config.t) ?(allowed = []) ?(denied = []) () :
    Runtime_config.t =
  let tools key = function
    | [] -> []
    | xs -> [ (key, `List (List.map (fun s -> `String s) xs)) ]
  in
  let parsed =
    Config_loader.parse_config ~resolve_secrets:false
      (`Assoc
         [
           ( "room_profiles",
             `List
               [
                 `Assoc
                   ([
                      ("id", `String "so"); ("model", `String "openai:gpt-test");
                    ]
                   @ tools "allowed_tools" allowed
                   @ tools "denied_tools" denied);
               ] );
           ( "room_profile_bindings",
             `List
               [
                 `Assoc
                   [
                     ("profile_id", `String "so");
                     ("room", `String "C123");
                     ("active", `Bool true);
                   ];
               ] );
         ])
  in
  {
    cfg with
    room_profiles = parsed.room_profiles;
    room_profile_bindings = parsed.room_profile_bindings;
  }

(* ---- Slack HTTP mock ---- *)

let obj fields : Yojson.Safe.t = `Assoc fields
let str s : Yojson.Safe.t = `String s
let ok fields = obj (("ok", `Bool true) :: fields)

type world = {
  users : (string * Yojson.Safe.t) list ref;
  members : string list ref;
  seq : int ref;
}

let user ~id ~is_bot extra =
  obj
    ([
       ("id", str id);
       ("team_id", str "T123");
       ("deleted", `Bool false);
       ("is_bot", `Bool is_bot);
       ("is_restricted", `Bool false);
       ("is_ultra_restricted", `Bool false);
     ]
    @ extra)

let world () =
  {
    users =
      ref
        [
          ("U123", user ~id:"U123" ~is_bot:false []);
          ("U456", user ~id:"U456" ~is_bot:false []);
          ( "UBOT",
            user ~id:"UBOT" ~is_bot:true
              [
                ( "profile",
                  obj [ ("api_app_id", str "A123"); ("bot_id", str "BOWN") ] );
              ] );
          ( "UCLAUDE",
            user ~id:"UCLAUDE" ~is_bot:true
              [
                ("name", str "claude");
                ("real_name", str "Claude");
                ( "profile",
                  obj
                    [
                      ("api_app_id", str "ACLAUDE");
                      ("bot_id", str "BCLAUDE");
                      ("display_name", str "Claude");
                    ] );
              ] );
        ];
    members = ref [ "U123"; "U456"; "UBOT"; "UCLAUDE" ];
    seq = ref 0;
  }

(* [users.info] and [users.list] both read [w.users]. *)
let edit_user w id f =
  if not (List.mem_assoc id !(w.users)) then
    Alcotest.fail ("S.edit_user: no user " ^ id);
  w.users :=
    List.map (fun (i, u) -> if i = id then (i, f u) else (i, u)) !(w.users)

let remove_member w id = w.members := List.filter (( <> ) id) !(w.members)

let param params key =
  match params with
  | `Assoc xs -> (
      match List.assoc_opt key xs with Some (`String s) -> s | _ -> "")
  | _ -> ""

let no_more = ("response_metadata", obj [ ("next_cursor", str "") ])

let default_response w method_ params : Yojson.Safe.t =
  match method_ with
  | "auth.test" ->
      ok
        [
          ("team_id", str "T123");
          ("user_id", str "UBOT");
          ("bot_id", str "BOWN");
        ]
  | "bots.info" ->
      ok
        [
          ( "bot",
            obj
              [
                ("id", str "BOWN");
                ("app_id", str "A123");
                ("user_id", str "UBOT");
              ] );
        ]
  | "users.info" -> (
      match List.assoc_opt (param params "user") !(w.users) with
      | Some u -> ok [ ("user", u) ]
      | None -> obj [ ("ok", `Bool false); ("error", str "user_not_found") ])
  | "users.list" -> ok [ ("members", `List (List.map snd !(w.users))); no_more ]
  | "conversations.info" ->
      ok
        [
          ( "channel",
            obj
              [
                ("id", str (param params "channel"));
                ("is_member", `Bool true);
                ("is_private", `Bool false);
                ("is_shared", `Bool false);
                ("is_ext_shared", `Bool false);
                ("is_org_shared", `Bool false);
                ("is_pending_ext_shared", `Bool false);
              ] );
        ]
  | "conversations.members" ->
      ok [ ("members", `List (List.map str !(w.members))); no_more ]
  | "chat.postMessage" ->
      incr w.seq;
      ok
        [
          ("ts", str (Printf.sprintf "200.%06d" !(w.seq)));
          ("channel", str (param params "channel"));
        ]
  | "chat.update" -> ok [ ("ts", str (param params "ts")) ]
  | other ->
      Alcotest.fail ("S.default_response: unexpected Slack method " ^ other)

type mock = {
  http : Slack_standing_order_slack.http;
  calls : (string * string * Yojson.Safe.t) list ref;
      (** (verb, method, params), oldest first. *)
  world : world;
  respond : (string -> Yojson.Safe.t -> int * Yojson.Safe.t) ref;
}

let mock () =
  let world = world () in
  let calls = ref [] in
  let respond =
    ref (fun method_ params -> (200, default_response world method_ params))
  in
  let http ~verb ~token:_ ~method_ ~params =
    let params = `Assoc params in
    calls := !calls @ [ (verb, method_, params) ];
    let status, body = !respond method_ params in
    Lwt.return (status, Yojson.Safe.to_string body)
  in
  { http; calls; world; respond }

(* [f params] answers [method_]; other methods keep their previous answer. *)
let override m method_ f =
  let previous = !(m.respond) in
  m.respond :=
    fun called params ->
      if called = method_ then f params else previous called params

let methods_called m = List.map (fun (_, method_, _) -> method_) !(m.calls)
