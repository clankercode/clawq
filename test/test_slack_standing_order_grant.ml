(* Slack_standing_order_grant: the zero-HTTP capability gate, the §6.4
   re-validation at every stage (the §7.1 authority matrix), failure classes
   and the pause-time member check. Fake ids only. *)
module S = Test_slack_standing_order_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module SO = Slack_standing_order_slack
module G = Slack_standing_order_grant

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)

let class_name : G.failure_class -> string = function
  | Hard -> "hard"
  | Transient -> "transient"
  | Inactive -> "inactive"
  | Capped -> "capped"

let unit_label : G.stage -> string = function
  | Plan _ -> "plan"
  | Confirm -> "confirm"
  | Replace _ -> "replace"
  | Resume -> "resume"
  | Fire _ -> "fire"

(* ---- JSON edits ---- *)

let set key (value : Yojson.Safe.t) (json : Yojson.Safe.t) : Yojson.Safe.t =
  match json with
  | `Assoc xs -> `Assoc (List.remove_assoc key xs @ [ (key, value) ])
  | other -> other

let drop key (json : Yojson.Safe.t) : Yojson.Safe.t =
  match json with
  | `Assoc xs -> `Assoc (List.remove_assoc key xs)
  | other -> other

let set_profile key value user =
  set "profile" (set key value (Slack_huddles.get "profile" user)) user

(* ---- Harness ---- *)

type env = { db : Sqlite3.db; m : S.mock; cfg : Runtime_config.t ref }

let env ?(cfg = S.config ()) () =
  { db = S.fresh_db (); m = S.mock (); cfg = ref cfg }

let plan_subject ?(grantor = "U123") () : G.subject =
  {
    order_id = Store.new_id ~prefix:"so";
    team_id = "T123";
    enterprise_id = None;
    api_app_id = "A123";
    channel_id = "C123";
    grantor_user_id = grantor;
    target = S.claude;
  }

(* One evaluation; returns the result and only this evaluation's methods. *)
let evaluate ?(now = S.now) e ~stage subject =
  e.m.calls := [];
  let r =
    Lwt_main.run
      (G.evaluate ~db:e.db
         ~current_config:(fun () -> !(e.cfg))
         ~http:e.m.http ~now ~stage ~unit_label:(unit_label stage) subject)
  in
  (r, S.methods_called e.m)

(* Seeds an order in [state] in a fresh db and mock; [mutate] then edits the
   world, overrides methods or arranges a mid-evaluation change. The stage
   defaults to a fire at the order's epoch. *)
let eval ?cfg ?(state = T.Active) ?stage ?now ?(mutate = fun _ _ -> ()) () =
  let e = env ?cfg () in
  let order = S.seed e.db ~state () in
  mutate e order;
  let stage =
    match stage with
    | Some f -> f order
    | None -> G.Fire { fire_epoch = order.epoch }
  in
  evaluate ?now e ~stage (G.subject_of_order order)

let eval_plan cfg =
  let e = env ~cfg () in
  evaluate e ~stage:(G.Plan { replaces = None }) (plan_subject ())

let eval_resume cfg = eval ~cfg ~state:T.Paused ~stage:(fun _ -> G.Resume) ()
let eval_fire cfg = eval ~cfg ()

let expect_fail ?(http = true) name cls reason (r, calls) =
  match r with
  | Ok (_ : G.validated) ->
      Alcotest.failf "%s: expected %s, got Ok" name (G.reason_code reason)
  | Error (f : G.failure) ->
      check_string (name ^ ": reason") (G.reason_code reason)
        (G.reason_code f.reason);
      check_string (name ^ ": class") (class_name cls) (class_name f.cls);
      if not http then
        check_bool
          (name ^ ": zero HTTP (" ^ String.concat "," calls ^ ")")
          true (calls = [])

let expect_ok name (r, _) : G.validated =
  match r with
  | Ok v -> v
  | Error (f : G.failure) ->
      Alcotest.failf "%s: unexpected %s" name (G.reason_code f.reason)

let count method_ calls = List.length (List.filter (( = ) method_) calls)

let params_of e method_ =
  List.filter_map
    (fun (_, m, params) -> if m = method_ then Some params else None)
    !(e.m.calls)

(* ---- Mock shapes ---- *)

let extra prefix n = List.init n (fun i -> Printf.sprintf "%s%04d" prefix i)

let add_users e ids =
  e.m.world.users :=
    !(e.m.world.users)
    @ List.map (fun id -> (id, S.user ~id ~is_bot:false [])) ids

let add_members e ids = e.m.world.members := !(e.m.world.members) @ ids

let rec chunks n = function
  | [] -> []
  | xs ->
      let rec take k acc = function
        | x :: rest when k > 0 -> take (k - 1) (x :: acc) rest
        | rest -> (List.rev acc, rest)
      in
      let page, rest = take n [] xs in
      page :: chunks n rest

let page_response ids next =
  S.ok
    [
      ("members", `List (List.map S.str ids));
      ("response_metadata", S.obj [ ("next_cursor", S.str next) ]);
    ]

(* [conversations.members] answers page i for cursor ["p<i>"]; past the last
   page the cursor is empty or, when [endless], continues over empty pages. *)
let paged_members ?(endless = false) e pages =
  S.override e.m "conversations.members" (fun params ->
      let i =
        match S.param params "cursor" with
        | "" -> 0
        | c -> int_of_string (String.sub c 1 (String.length c - 1))
      in
      let page = Option.value (List.nth_opt pages i) ~default:[] in
      let next =
        if endless || i + 1 < List.length pages then Printf.sprintf "p%d" (i + 1)
        else ""
      in
      (200, page_response page next))

let users_listing e ?(next = "") keep =
  S.override e.m "users.list" (fun _ ->
      ( 200,
        S.ok
          [
            ( "members",
              `List
                (List.filter_map
                   (fun (id, u) -> if keep id then Some u else None)
                   !(e.m.world.users)) );
            ("response_metadata", S.obj [ ("next_cursor", S.str next) ]);
          ] ))

let channel_with edits e _ =
  S.override e.m "conversations.info" (fun params ->
      let response = S.default_response e.m.world "conversations.info" params in
      let channel =
        List.fold_left
          (fun c (k, v) -> set k v c)
          (Slack_huddles.get "channel" response)
          edits
      in
      (200, set "channel" channel response))

let answer method_ status body e _ =
  S.override e.m method_ (fun _ -> (status, body))

let slack_error code = S.obj [ ("ok", `Bool false); ("error", S.str code) ]

let make_admin db id =
  Sql_util.exec_with_params db
    "INSERT INTO channel_admins(channel, sender_id) VALUES ('slack', ?)"
    [ Sqlite3.Data.TEXT id ]

(* ---- Cases ---- *)

let test_capability_gate () =
  (* §7.1, at Plan, Resume and Fire; row 2 is the deny-list profile denying
     only content *)
  List.iter
    (fun stage_case ->
      List.iter
        (fun (allowed, denied, missing) ->
          expect_fail ~http:false missing Hard (Tool_denied missing)
            (stage_case (S.profile (S.config ()) ~allowed ~denied ())))
        [
          ( [ "slack_huddles"; "slack_standing_order_fire" ],
            [],
            "slack_huddle_content" );
          ([], [ "slack_huddle_content" ], "slack_huddle_content");
          ([], [ "slack_standing_order_fire" ], "slack_standing_order_fire");
          ([], [ "slack_huddles" ], "slack_huddles");
        ])
    [ eval_plan; eval_resume; eval_fire ];
  ignore
    (expect_ok "a profile allowing all three"
       (eval_fire
          (S.profile (S.config ()) ~allowed:G.capability_names ~denied:[ "x" ]
             ())))

let test_pass () =
  let e = env () in
  let order = S.seed e.db () in
  let r, calls =
    evaluate e
      ~stage:(G.Fire { fire_epoch = order.epoch })
      (G.subject_of_order order)
  in
  let v = expect_ok "fire on an active order" (r, calls) in
  check_bool "standing-order snapshot" true
    (v.snapshot.work_type = Access_snapshot.Standing_order);
  check_bool "keyed on the channel" true
    (v.snapshot.session_key = Some "slack:C123");
  check_bool "names the order" true
    (v.snapshot.standing_order
    = Some { Access_snapshot.order_id = order.id; fire_id = None });
  check_string "bot user from auth.test" "UBOT" v.bot_user_id;
  check_string "bot id from auth.test" "BOWN" v.bot_id;
  check_bool "no enterprise" true (v.enterprise_id = None);
  check_bool "the fresh order row" true (v.order = Some order);
  check_bool "nothing replaced" true (v.replaced = None);
  check_bool "the evaluated config" true (v.config == !(e.cfg));
  List.iter
    (fun m ->
      check_bool (m ^ " is a listed method") true (List.mem_assoc m SO.methods))
    calls;
  check_bool "the §6.4 read sequence, no writes" true
    (calls
    = [
        "auth.test";
        "bots.info";
        "conversations.info";
        "conversations.members";
        "users.list";
      ]);
  check_int "users.list once" 1 (count "users.list" calls);
  check_int "users.info never" 0 (count "users.info" calls);
  check_bool "bots.info asks for our bot" true
    (params_of e "bots.info" = [ `Assoc [ ("bot", `String "BOWN") ] ]);
  check_bool "members are paged by 200" true
    (params_of e "conversations.members"
    = [ `Assoc [ ("channel", `String "C123"); ("limit", `Int 200) ] ]);
  check_bool "egress audit rows name the fire unit" true
    (Egress_audit.query ~db:e.db
       ~tool_name:("standing_order:" ^ order.id ^ ":fire")
       ()
    <> []);
  match Lwt_main.run (v.call "users.info" [ ("user", `String "U123") ]) with
  | Ok _ -> ()
  | Error err -> Alcotest.fail ("the validated call works: " ^ SO.error_code err)

let test_allowlist () =
  expect_fail ~http:false "empty allowlist" Hard Target_not_allowlisted
    (eval ~cfg:(S.config ~targets:[] ()) ());
  expect_fail ~http:false "empty allowlist at plan" Hard Target_not_allowlisted
    (eval_plan (S.config ~targets:[] ()));
  expect_fail ~http:false "target not listed" Hard Target_not_allowlisted
    (eval
       ~cfg:(S.config ~targets:[ { app_id = "AOTHER"; label = "Other" } ] ())
       ())

let test_grantor_matrix () =
  let grantor f = eval ~mutate:(fun e _ -> S.edit_user e.m.world "U123" f) () in
  expect_fail "deleted grantor" Hard Grantor_unavailable
    (grantor (set "deleted" (`Bool true)));
  expect_fail "bot grantor" Hard Grantor_unavailable
    (grantor (set "is_bot" (`Bool true)));
  expect_fail "restricted grantor" Hard Grantor_guest
    (grantor (set "is_restricted" (`Bool true)));
  expect_fail "ultra-restricted grantor" Hard Grantor_guest
    (grantor (set "is_ultra_restricted" (`Bool true)));
  expect_fail "grantor left the channel" Hard Grantor_not_member
    (eval ~mutate:(fun e _ -> S.remove_member e.m.world "U123") ());
  expect_fail ~http:false "grantor off the allowlist" Hard Grantor_not_allowed
    (eval ~cfg:(S.with_allow_users (S.config ()) [ "U999" ]) ());
  let admin_policy = S.config ~creator:So_creator_admin () in
  expect_fail ~http:false "admin policy, grantor not an admin" Hard
    Grantor_not_admin
    (eval ~cfg:admin_policy ());
  ignore
    (expect_ok "admin policy, admin grantor"
       (eval ~cfg:admin_policy ~mutate:(fun e _ -> make_admin e.db "U123") ()));
  let revoked e _ =
    make_admin e.db "U123";
    S.override e.m "users.list" (fun params ->
        Admin.remove_admin ~db:e.db ~channel:"slack" ~sender_id:"U123" |> ignore;
        (200, S.default_response e.m.world "users.list" params))
  in
  expect_fail "admin revoked mid-evaluation" Hard Grantor_not_admin
    (eval ~cfg:admin_policy ~mutate:revoked ())

let test_posture_matrix () =
  List.iter
    (fun key ->
      expect_fail key Hard Channel_shared
        (eval ~mutate:(channel_with [ (key, `Bool true) ]) ()))
    [ "is_ext_shared"; "is_shared"; "is_org_shared"; "is_pending_ext_shared" ];
  expect_fail "ultra-restricted member" Hard Channel_has_guests
    (eval
       ~mutate:(fun e _ ->
         S.edit_user e.m.world "U456" (set "is_ultra_restricted" (`Bool true)))
       ());
  expect_fail "restricted member" Hard Channel_has_guests
    (eval
       ~mutate:(fun e _ ->
         S.edit_user e.m.world "U456" (set "is_restricted" (`Bool true)))
       ());
  expect_fail "endless member cursor" Transient
    (Indeterminate "conversations.members")
    (eval
       ~mutate:(fun e _ ->
         paged_members ~endless:true e [ !(e.m.world.members) ])
       ());
  expect_fail "1001 members on one page" Transient
    (Indeterminate "conversations.members")
    (eval
       ~mutate:(fun e _ ->
         let ids = !(e.m.world.members) @ extra "UM" 997 in
         paged_members e [ ids ])
       ());
  expect_fail "member page without a cursor" Transient
    (Indeterminate "conversations.members")
    (eval
       ~mutate:
         (answer "conversations.members" 200
            (S.ok [ ("members", `List (List.map S.str [ "U123"; "UCLAUDE" ])) ]))
       ());
  let deny_private =
    S.with_slack (S.config ()) (fun sc ->
        {
          sc with
          private_channel_policy = Runtime_config.Pc_deny;
          allow_private_channels = [];
        })
  in
  expect_fail "private channel under deny" Hard Private_policy_denied
    (eval ~cfg:deny_private
       ~mutate:(channel_with [ ("is_private", `Bool true) ])
       ());
  ignore
    (expect_ok "private channel listed under deny"
       (eval
          ~cfg:
            (S.with_slack deny_private (fun sc ->
                 { sc with allow_private_channels = [ "C123" ] }))
          ~mutate:(channel_with [ ("is_private", `Bool true) ])
          ()));
  ignore
    (expect_ok "private channel under allow_if_listed"
       (eval ~mutate:(channel_with [ ("is_private", `Bool true) ]) ()));
  expect_fail "is_private unreported" Transient
    (Indeterminate "conversations.info")
    (eval ~mutate:(channel_with [ ("is_private", `Null) ]) ());
  expect_fail "is_shared untyped" Transient (Indeterminate "conversations.info")
    (eval ~mutate:(channel_with [ ("is_shared", `String "false") ]) ())

let test_member_flags_scale () =
  let e = env () in
  let order = S.seed e.db () in
  let subject = G.subject_of_order order in
  let stage = G.Fire { fire_epoch = order.epoch } in
  let ids = extra "UM" 596 in
  add_users e ids;
  add_members e ids;
  paged_members e (chunks 200 !(e.m.world.members));
  let r, calls = evaluate e ~stage subject in
  ignore (expect_ok "600 members over 3 pages" (r, calls));
  check_int "3 member pages" 3 (count "conversations.members" calls);
  check_int "one users.list" 1 (count "users.list" calls);
  check_int "zero users.info" 0 (count "users.info" calls);
  let e = env () in
  let order = S.seed e.db () in
  let subject = G.subject_of_order order in
  let stage = G.Fire { fire_epoch = order.epoch } in
  users_listing e (( <> ) "U456");
  ignore
    (expect_ok "one member missed by users.list" (evaluate e ~stage subject));
  check_bool "exactly one users.info, for U456" true
    (params_of e "users.info" = [ `Assoc [ ("user", `String "U456") ] ]);
  users_listing e ~next:"more" (( <> ) "U456");
  let r, calls = evaluate e ~stage subject in
  ignore (expect_ok "users.list paging stops at five pages" (r, calls));
  check_int "five users.list pages" 5 (count "users.list" calls);
  check_int "then one users.info" 1 (count "users.info" calls);
  let twenty = extra "UX" 20 in
  add_users e twenty;
  add_members e twenty;
  users_listing e (fun id -> not (List.mem id twenty));
  let r, calls = evaluate e ~stage subject in
  ignore (expect_ok "twenty misses are looked up" (r, calls));
  check_int "twenty users.info" 20 (count "users.info" calls);
  let e = env () in
  add_members e (extra "UZ" 21);
  let order = S.seed e.db () in
  let r, calls =
    evaluate e
      ~stage:(G.Fire { fire_epoch = order.epoch })
      (G.subject_of_order order)
  in
  expect_fail "21 misses" Transient (Indeterminate "users.list") (r, calls);
  check_int "zero users.info" 0 (count "users.info" calls);
  expect_fail "users.list record without is_restricted" Transient
    (Indeterminate "users.list")
    (eval
       ~mutate:(fun e _ -> S.edit_user e.m.world "U456" (drop "is_restricted"))
       ());
  expect_fail "users.list record from another team" Transient
    (Indeterminate "users.list")
    (eval
       ~mutate:(fun e _ ->
         S.edit_user e.m.world "U456" (set "team_id" (S.str "T999")))
       ());
  expect_fail "users.info record without deleted" Transient
    (Indeterminate "users.info")
    (eval
       ~mutate:(fun e _ ->
         S.edit_user e.m.world "U456" (drop "deleted");
         users_listing e (( <> ) "U456"))
       ())

let test_installation () =
  expect_fail "auth.test team" Hard Team_mismatch
    (eval
       ~mutate:
         (answer "auth.test" 200
            (S.ok
               [
                 ("team_id", S.str "T999");
                 ("user_id", S.str "UBOT");
                 ("bot_id", S.str "BOWN");
               ]))
       ());
  expect_fail "auth.test without a bot id" Transient (Indeterminate "auth.test")
    (eval
       ~mutate:
         (answer "auth.test" 200
            (S.ok [ ("team_id", S.str "T123"); ("user_id", S.str "UBOT") ]))
       ());
  let auth_with_enterprise ent =
    answer "auth.test" 200
      (S.ok
         [
           ("team_id", S.str "T123");
           ("user_id", S.str "UBOT");
           ("bot_id", S.str "BOWN");
           ("enterprise_id", S.str ent);
         ])
  in
  let e = env () in
  auth_with_enterprise "E123" e ();
  let v =
    expect_ok "enterprise taken from auth.test at plan"
      (evaluate e ~stage:(G.Plan { replaces = None }) (plan_subject ()))
  in
  check_bool "enterprise id" true (v.enterprise_id = Some "E123");
  let with_enterprise ent =
    { (plan_subject ()) with enterprise_id = Some ent }
  in
  expect_fail "enterprise differs" Hard Team_mismatch
    (evaluate e ~stage:(G.Plan { replaces = None }) (with_enterprise "E999"));
  let e = env () in
  expect_fail "enterprise no longer reported" Hard Team_mismatch
    (evaluate e ~stage:(G.Plan { replaces = None }) (with_enterprise "E123"));
  expect_fail "bots.info app" Hard App_mismatch
    (eval
       ~mutate:
         (answer "bots.info" 200
            (S.ok
               [
                 ( "bot",
                   S.obj [ ("id", S.str "BOWN"); ("app_id", S.str "A999") ] );
               ]))
       ());
  expect_fail "bot not a member" Hard Bot_not_member
    (eval ~mutate:(channel_with [ ("is_member", `Bool false) ]) ());
  expect_fail "another channel's metadata" Hard Bot_not_member
    (eval ~mutate:(channel_with [ ("id", S.str "C999") ]) ())

let test_target_matrix () =
  let target f =
    eval ~mutate:(fun e _ -> S.edit_user e.m.world "UCLAUDE" f) ()
  in
  expect_fail "target deleted" Hard Target_unavailable
    (target (set "deleted" (`Bool true)));
  expect_fail "target not a bot" Hard Target_unavailable
    (target (set "is_bot" (`Bool false)));
  expect_fail "target app changed" Hard Target_unavailable
    (target (set_profile "api_app_id" (S.str "AOTHER")));
  expect_fail "target bot changed" Hard Target_unavailable
    (target (set_profile "bot_id" (S.str "BOTHER")));
  expect_fail "target left the channel" Hard Target_not_member
    (eval ~mutate:(fun e _ -> S.remove_member e.m.world "UCLAUDE") ())

let test_slack_errors () =
  expect_fail "users.list 429" Transient (Indeterminate "users.list")
    (eval ~mutate:(answer "users.list" 429 (S.ok [])) ());
  expect_fail "users.list 503" Transient (Indeterminate "users.list")
    (eval ~mutate:(answer "users.list" 503 (S.ok [])) ());
  expect_fail "members missing_scope" Hard
    (Missing_scope "conversations.members")
    (eval
       ~mutate:
         (answer "conversations.members" 200 (slack_error "missing_scope"))
       ());
  expect_fail "auth.test invalid_auth" Hard Credentials_invalid
    (eval ~mutate:(answer "auth.test" 200 (slack_error "invalid_auth")) ());
  expect_fail "auth.test token_revoked" Hard Credentials_invalid
    (eval ~mutate:(answer "auth.test" 200 (slack_error "token_revoked")) ());
  expect_fail "conversations.info channel_not_found" Hard Bot_not_member
    (eval
       ~mutate:
         (answer "conversations.info" 200 (slack_error "channel_not_found"))
       ());
  expect_fail "bots.info bot_not_found" Hard App_mismatch
    (eval ~mutate:(answer "bots.info" 200 (slack_error "bot_not_found")) ());
  expect_fail "an unknown Slack error" Transient (Indeterminate "auth.test")
    (eval ~mutate:(answer "auth.test" 200 (slack_error "fatal_error")) ());
  let gone id e _ =
    e.m.world.users := List.remove_assoc id !(e.m.world.users)
  in
  expect_fail "grantor user_not_found" Hard Grantor_unavailable
    (eval ~mutate:(gone "U123") ());
  expect_fail "target user_not_found" Hard Target_unavailable
    (eval ~mutate:(gone "UCLAUDE") ());
  expect_fail "member user_not_found" Transient (Indeterminate "users.info")
    (eval ~mutate:(gone "U456") ());
  let rule = S.slack_rule in
  let strict =
    S.config
      ~strict_rules:
        [
          rule ~method_:"POST" "/api/auth.test";
          rule ~method_:"GET" "/api/bots.info";
          rule ~method_:"GET" "/api/conversations.info";
          rule ~method_:"GET" "/api/users.list";
          rule ~method_:"GET" "/api/users.info";
        ]
      ()
  in
  expect_fail "strict egress without conversations.members" Hard
    (Egress_denied "conversations.members") (eval ~cfg:strict ());
  expect_fail ~http:false "empty bot token" Hard Credentials_invalid
    (eval ~cfg:(S.config ~bot_token:"" ()) ())

let test_snapshot_fails_closed () =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      Store.ensure_schema db;
      let e = { db; m = S.mock (); cfg = ref (S.config ()) } in
      expect_fail ~http:false "no access_snapshots table" Transient
        (Indeterminate "access_snapshot")
        (evaluate e ~stage:(G.Plan { replaces = None }) (plan_subject ()));
      match
        G.gate ~db ~config:(S.config ()) ~channel_id:"C123" ~order_ref:None
      with
      | Error f ->
          check_string "gate reason" "indeterminate:access_snapshot"
            (G.reason_code f.reason)
      | Ok _ -> Alcotest.fail "the gate needs a persisted snapshot")

let test_store_errors_are_transient () =
  (* No channel_admins table: the admin read raises inside evaluate. *)
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      Store.ensure_schema db;
      Access_snapshot.init_schema db;
      Egress_audit.init_schema db;
      let e =
        { db; m = S.mock (); cfg = ref (S.config ~creator:So_creator_admin ()) }
      in
      expect_fail ~http:false "a raising store read" Transient
        (Indeterminate "internal")
        (evaluate e ~stage:(G.Plan { replaces = None }) (plan_subject ())))

let test_inactive_and_cap () =
  expect_fail ~http:false "fire on a paused order" Inactive
    (Order_not_in_state Paused) (eval ~state:T.Paused ());
  expect_fail ~http:false "fire at a stale epoch" Inactive Epoch_mismatch
    (eval ~stage:(fun o -> G.Fire { fire_epoch = o.epoch - 1 }) ());
  expect_fail ~http:false "kill switch" Inactive Feature_disabled
    (eval ~cfg:(S.config ~enabled:false ()) ());
  let base = S.config () in
  expect_fail ~http:false "tools disabled" Inactive Feature_disabled
    (eval
       ~cfg:
         { base with security = { base.security with tools_enabled = false } }
       ());
  expect_fail ~http:false "no Slack section" Inactive Feature_disabled
    (eval ~cfg:{ base with channels = { base.channels with slack = None } } ());
  let fires n ~at e (o : T.order) =
    List.iter
      (fun i ->
        ignore
          (S.fire e.db o ~dispatched_at:at ~room:(Printf.sprintf "R%03d" i) ()
            : T.fire))
      (List.init n Fun.id)
  in
  expect_fail ~http:false "ten dispatches today" Capped Daily_cap_reached
    (eval ~mutate:(fires 10 ~at:(S.now -. 60.)) ());
  ignore
    (expect_ok "nine dispatches today"
       (eval ~mutate:(fires 9 ~at:(S.now -. 60.)) ()));
  ignore
    (expect_ok "ten dispatches yesterday"
       (eval ~mutate:(fires 10 ~at:(S.now -. 86_401.)) ()));
  let expires value e (o : T.order) =
    Sql_util.exec_with_params e.db
      "UPDATE slack_standing_orders SET expires_at = ? WHERE id = ?"
      [ Sqlite3.Data.TEXT value; Sqlite3.Data.TEXT o.id ]
  in
  expect_fail ~http:false "expired order" Inactive Order_expired
    (eval ~mutate:(expires "2023-01-01 00:00:00") ());
  expect_fail ~http:false "unreadable expiry" Inactive Order_expired
    (eval ~mutate:(expires "never") ());
  ignore
    (expect_ok "unexpired order"
       (eval ~mutate:(expires "2999-01-01 00:00:00") ()));
  let e = env () in
  let o = S.seed e.db () in
  let subject = G.subject_of_order o in
  let stage = G.Fire { fire_epoch = o.epoch } in
  expect_fail ~http:false "missing order" Inactive Order_missing
    (evaluate e ~stage { subject with order_id = "so_missing" });
  expect_fail ~http:false "another grantor" Hard Identity_changed
    (evaluate e ~stage { subject with grantor_user_id = "U456" });
  expect_fail ~http:false "another channel" Hard Identity_changed
    (evaluate e ~stage { subject with channel_id = "C999" });
  expect_fail ~http:false "another target bot" Hard Identity_changed
    (evaluate e ~stage
       { subject with target = { subject.target with bot_id = "BOTHER" } })

let test_state_read_last () =
  let paused_once e (o : T.order) =
    let first = ref true in
    S.override e.m "users.list" (fun params ->
        if !first then begin
          first := false;
          match
            Store.deactivate e.db ~order_id:o.id (Pause "U456") ~now:S.now
          with
          | Ok _ -> ()
          | Error err -> Alcotest.fail ("pause: " ^ err)
        end;
        (200, S.default_response e.m.world "users.list" params))
  in
  expect_fail "paused during the evaluation" Inactive
    (Order_not_in_state Paused)
    (eval ~mutate:paused_once ());
  let swap e _ =
    S.override e.m "users.list" (fun params ->
        e.cfg := S.config ();
        (200, S.default_response e.m.world "users.list" params))
  in
  expect_fail "config swapped during the evaluation" Transient Config_changed
    (eval ~mutate:swap ())

let test_plan_and_confirm () =
  let e = env () in
  let live = S.seed e.db () in
  expect_fail ~http:false "same target already live" Hard (Duplicate_order 1)
    (evaluate e ~stage:(G.Plan { replaces = None }) (plan_subject ()));
  expect_fail ~http:false "plan for an existing order id" Hard Identity_changed
    (evaluate e
       ~stage:(G.Plan { replaces = None })
       { (plan_subject ()) with order_id = live.id });
  let e = env () in
  let v =
    expect_ok "plan with no conflict"
      (evaluate e ~stage:(G.Plan { replaces = None }) (plan_subject ()))
  in
  check_bool "no order row at plan" true (v.order = None && v.replaced = None);
  let pending = S.seed e.db ~state:T.Pending_confirmation () in
  let v =
    expect_ok "confirm a pending order"
      (evaluate e ~stage:G.Confirm (G.subject_of_order pending))
  in
  check_bool "the pending row" true (v.order = Some pending);
  expect_fail ~http:false "confirm after the window" Inactive Order_expired
    (evaluate ~now:(S.now +. 601.) e ~stage:G.Confirm
       (G.subject_of_order pending));
  expect_fail ~http:false "confirm by another grantor" Hard Identity_changed
    (evaluate e ~stage:G.Confirm
       { (G.subject_of_order pending) with grantor_user_id = "U456" });
  expect_fail ~http:false "confirm an active order" Inactive
    (Order_not_in_state Active)
    (eval ~stage:(fun _ -> G.Confirm) ())

let test_resume_stage () =
  ignore (expect_ok "resume a paused order" (eval_resume (S.config ())));
  ignore
    (expect_ok "resume a suspended order"
       (eval ~state:T.Suspended ~stage:(fun _ -> G.Resume) ()));
  expect_fail ~http:false "resume an active order" Inactive
    (Order_not_in_state Active)
    (eval ~stage:(fun _ -> G.Resume) ());
  let e = env () in
  let suspended = S.seed e.db ~state:T.Suspended () in
  ignore (S.seed e.db () : T.order);
  expect_fail ~http:false "resume beside a live order" Hard (Duplicate_order 2)
    (evaluate e ~stage:G.Resume (G.subject_of_order suspended))

let test_replace_stage () =
  let e = env () in
  let a = S.seed e.db () in
  let subject = plan_subject ~grantor:"U456" () in
  let v =
    expect_ok "plan a change"
      (evaluate e ~stage:(G.Plan { replaces = Some a.id }) subject)
  in
  check_bool "the fresh old row" true (v.replaced = Some a);
  check_bool "no new row yet" true (v.order = None);
  let v =
    expect_ok "replace at the card's epoch"
      (evaluate e
         ~stage:(G.Replace { replaces = a.id; replaced_epoch = a.epoch })
         subject)
  in
  check_bool "replace reads the old row" true (v.replaced = Some a);
  expect_fail ~http:false "replace at a stale epoch" Inactive Epoch_mismatch
    (evaluate e
       ~stage:(G.Replace { replaces = a.id; replaced_epoch = a.epoch - 1 })
       subject);
  expect_fail ~http:false "replace another channel's order" Hard
    Identity_changed
    (let other = S.seed e.db ~channel:"C999" () in
     evaluate e ~stage:(G.Plan { replaces = Some other.id }) subject);
  (match Store.deactivate e.db ~order_id:a.id (Cancel "U123") ~now:S.now with
  | Ok _ -> ()
  | Error err -> Alcotest.fail ("cancel: " ^ err));
  expect_fail ~http:false "replace a cancelled order" Inactive
    (Order_not_in_state Cancelled)
    (evaluate e ~stage:(G.Plan { replaces = Some a.id }) subject);
  expect_fail ~http:false "replace a missing order" Inactive Order_missing
    (evaluate e ~stage:(G.Plan { replaces = Some "so_missing" }) subject)

let refusal_name : G.member_refusal -> string = function
  | Member_unavailable -> "unavailable"
  | Member_guest -> "guest"
  | Member_not_member -> "not_member"
  | Member_not_allowed -> "not_allowed"
  | Member_team_mismatch -> "team_mismatch"
  | Member_indeterminate m -> "indeterminate:" ^ m
  | Member_egress m -> "egress:" ^ m

let member ?(require_allowlist = false) e user =
  let call =
    match
      SO.snapshot ~db:e.db ~config:!(e.cfg) ~channel_id:"C123" ~order_ref:None
    with
    | Ok snapshot ->
        SO.make ~db:e.db
          ~current_config:(fun () -> !(e.cfg))
          ~snapshot ~channel_id:"C123" ~audit:"standing_order:none:pause"
          ~http:e.m.http ()
    | Error err -> Alcotest.fail ("snapshot: " ^ err)
  in
  e.m.calls := [];
  let r =
    Lwt_main.run
      (G.check_member ~call ~config:!(e.cfg) ~team_id:"T123" ~channel_id:"C123"
         ~user_id:user ~require_allowlist)
  in
  ( (match r with Ok () -> "ok" | Error x -> refusal_name x),
    S.methods_called e.m )

let test_check_member () =
  let e = env ~cfg:(S.with_allow_users (S.config ()) [ "U123" ]) () in
  check_string "an allowlist is not needed to pause" "ok"
    (fst (member e "U456"));
  let outcome, calls = member ~require_allowlist:true e "U456" in
  check_string "resume needs the allowlist" "not_allowed" outcome;
  check_bool "the allowlist costs no HTTP" true (calls = []);
  check_string "an allowlisted member" "ok"
    (fst (member ~require_allowlist:true e "U123"));
  let with_user f =
    let e = env () in
    S.edit_user e.m.world "U456" f;
    fst (member e "U456")
  in
  check_string "guest" "guest" (with_user (set "is_restricted" (`Bool true)));
  check_string "ultra guest" "guest"
    (with_user (set "is_ultra_restricted" (`Bool true)));
  check_string "deleted" "unavailable" (with_user (set "deleted" (`Bool true)));
  check_string "a bot" "unavailable" (with_user (set "is_bot" (`Bool true)));
  check_string "another team" "team_mismatch"
    (with_user (set "team_id" (S.str "T999")));
  check_string "untyped flags" "indeterminate:users.info"
    (with_user (drop "is_bot"));
  let e = env () in
  check_string "unknown user" "unavailable" (fst (member e "U777"));
  S.remove_member e.m.world "U456";
  check_string "absent from a complete listing" "not_member"
    (fst (member e "U456"));
  let e = env () in
  answer "users.info" 429 (S.ok []) e ();
  check_string "users.info 429" "indeterminate:users.info"
    (fst (member e "U456"));
  let e = env () in
  paged_members ~endless:true e [ [ "U123" ]; [ "U456" ] ];
  let outcome, calls = member e "U456" in
  check_string "found on page 2" "ok" outcome;
  check_int "two member pages" 2 (count "conversations.members" calls);
  let e = env () in
  paged_members ~endless:true e [ [ "U123" ] ];
  let outcome, calls = member e "U456" in
  check_string "absent with a cursor left" "indeterminate:conversations.members"
    outcome;
  check_int "five member pages" 5 (count "conversations.members" calls);
  let e =
    env
      ~cfg:
        (S.config
           ~strict_rules:[ S.slack_rule ~method_:"GET" "/api/users.info" ]
           ())
      ()
  in
  check_string "egress denied" "egress:conversations.members"
    (fst (member e "U456"))

let test_classes_and_codes () =
  check_bool "capability names" true
    (G.capability_names
    = [ "slack_huddles"; "slack_huddle_content"; "slack_standing_order_fire" ]);
  check_int "member scan bound" 1000 G.max_scan_members;
  check_int "member lookup bound" 20 G.max_member_lookups;
  List.iter
    (fun ((reason : G.reason), cls, code) ->
      check_string code code (G.reason_code reason);
      check_string (code ^ " class") (class_name cls)
        (class_name (G.classify reason)))
    [
      (Feature_disabled, G.Inactive, "feature_disabled");
      (Order_missing, Inactive, "order_missing");
      (Order_not_in_state T.Paused, Inactive, "order_not_in_state:paused");
      (Order_expired, Inactive, "order_expired");
      (Epoch_mismatch, Inactive, "epoch_mismatch");
      (Daily_cap_reached, Capped, "daily_cap_reached");
      (Indeterminate "users.list", Transient, "indeterminate:users.list");
      (Config_changed, Transient, "config_changed");
      (Credentials_invalid, Hard, "credentials_invalid");
      (Identity_changed, Hard, "identity_changed");
      (Target_not_allowlisted, Hard, "target_not_allowlisted");
      (Grantor_not_allowed, Hard, "grantor_not_allowed");
      (Grantor_not_admin, Hard, "grantor_not_admin");
      (Tool_denied "slack_huddles", Hard, "tool_denied:slack_huddles");
      (Duplicate_order 3, Hard, "duplicate_order:SO-3");
      (Team_mismatch, Hard, "team_mismatch");
      (App_mismatch, Hard, "app_mismatch");
      (Bot_not_member, Hard, "bot_not_member");
      (Private_policy_denied, Hard, "private_policy_denied");
      (Channel_shared, Hard, "channel_shared");
      (Channel_has_guests, Hard, "channel_has_guests");
      (Grantor_unavailable, Hard, "grantor_unavailable");
      (Grantor_guest, Hard, "grantor_guest");
      (Grantor_not_member, Hard, "grantor_not_member");
      (Target_unavailable, Hard, "target_unavailable");
      (Target_not_member, Hard, "target_not_member");
      ( Missing_scope "conversations.members",
        Hard,
        "missing_scope:conversations.members" );
      (Egress_denied "users.list", Hard, "egress_denied:users.list");
    ]

let suite =
  [
    S.case "capability gate" test_capability_gate;
    S.case "pass" test_pass;
    S.case "allowlist" test_allowlist;
    S.case "grantor matrix" test_grantor_matrix;
    S.case "posture matrix" test_posture_matrix;
    S.case "member flags scale" test_member_flags_scale;
    S.case "installation" test_installation;
    S.case "target matrix" test_target_matrix;
    S.case "slack errors" test_slack_errors;
    S.case "snapshot fails closed" test_snapshot_fails_closed;
    S.case "store errors are transient" test_store_errors_are_transient;
    S.case "inactive and cap" test_inactive_and_cap;
    S.case "state read last" test_state_read_last;
    S.case "plan and confirm stages" test_plan_and_confirm;
    S.case "resume stage" test_resume_stage;
    S.case "replace stage" test_replace_stage;
    S.case "check member" test_check_member;
    S.case "classes and codes" test_classes_and_codes;
  ]
