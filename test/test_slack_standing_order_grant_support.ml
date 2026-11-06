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
  | Resolve -> "resolve"
  | Reconcile _ -> "reconcile"
  | Fire _ -> "fire"
  | Track _ -> "track"

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

let announce e (o : T.order) =
  let c =
    Store.insert_card e.db ~kind:T.Pause_button ~order:o
      ~created_by:o.grantor_user_id ~now:S.now ()
  in
  Store.set_card_message_ts e.db ~card_id:c.id "1700000000.000200"

let fire_stage e ?epoch (o : T.order) =
  if not (Store.activation_announced e.db ~order_id:o.id) then announce e o;
  let f = S.fire e.db o ~room:(Store.new_id ~prefix:"R") () in
  G.Fire { fire_epoch = Option.value epoch ~default:o.epoch; fire_id = f.id }

(* Seeds an order in [state] in a fresh db and mock; [mutate] then edits the
   world, overrides methods or arranges a mid-evaluation change. The stage
   defaults to a fire at the order's epoch. *)
let eval ?cfg ?(state = T.Active) ?stage ?now ?(mutate = fun _ _ -> ()) () =
  let e = env ?cfg () in
  let order = S.seed e.db ~state () in
  let default_stage = fire_stage e order in
  mutate e order;
  let stage = match stage with Some f -> f e order | None -> default_stage in
  evaluate ?now e ~stage (G.subject_of_order order)

let eval_plan cfg =
  let e = env ~cfg () in
  evaluate e ~stage:(G.Plan { replaces = None }) (plan_subject ())

let eval_resume cfg = eval ~cfg ~state:T.Paused ~stage:(fun _ _ -> G.Resume) ()
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

let check_order_snapshot (v : G.validated) =
  check_bool "management snapshot has no fire id" true
    (Option.bind v.snapshot.standing_order (fun r -> r.Access_snapshot.fire_id)
    = None)

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
