open Lwt.Syntax
(** Typed pickup hints are durable, but only a leased worker may reduce them.
    [call] is a guarded Grant.Track call; this module never creates authority.
*)

module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module C = Slack_huddle_classify
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant

let matchers : C.matchers = { first_reply = []; any_reply = [] }

let init db =
  Store.ensure_schema db;
  Store.exec db
    "CREATE TABLE IF NOT EXISTS slack_standing_order_observations (fire_id \
     TEXT NOT NULL,attempt_no INTEGER NOT NULL CHECK(attempt_no IN \
     (0,1)),team_id TEXT NOT NULL,api_app_id TEXT NOT NULL,payload TEXT NOT \
     NULL,observed_at REAL NOT \
     NULL,UNIQUE(fire_id,attempt_no,team_id,api_app_id,payload))"
    []

let kind = function
  | C.Reply -> "reply"
  | C.Notice C.Rate_limited -> "rate_limited"
  | C.Notice C.Spend_limit -> "spend_limit"
  | C.Notice C.Disabled -> "disabled"
  | C.Notice C.Not_delivered -> "not_delivered"

let decode_kind = function
  | "reply" -> C.Reply
  | "rate_limited" -> C.Notice C.Rate_limited
  | "spend_limit" -> C.Notice C.Spend_limit
  | "disabled" -> C.Notice C.Disabled
  | "not_delivered" -> C.Notice C.Not_delivered
  | _ -> failwith "invalid pickup kind"

let json_opt = function Some s -> `String s | None -> `Null

let decode_opt = function
  | `String s when s <> "" -> Some s
  | `Null -> None
  | _ -> failwith "invalid pickup identity"

(* Fixed positional payload, never event JSON, text, username or blocks. *)
let encode = function
  | C.Own_relay { channel; ts; file_ids; nonce } ->
      `List
        [
          `String "relay";
          `String channel;
          `String ts;
          `List (List.map (fun id -> `String id) file_ids);
          `String nonce;
        ]
  | C.Target_reaction { channel; user; item_ts; item_file } ->
      `List
        [
          `String "reaction";
          json_opt channel;
          `String user;
          json_opt item_ts;
          json_opt item_file;
        ]
  | C.Target_reply
      {
        channel;
        thread_ts;
        ts;
        bot_id;
        app_id;
        kind_if_first;
        kind_if_later;
        _;
      } ->
      `List
        [
          `String "reply";
          `String channel;
          `String thread_ts;
          `String ts;
          `String bot_id;
          json_opt app_id;
          `String (kind kind_if_first);
          `String (kind kind_if_later);
        ]
  | _ -> failwith "invalid pickup observation"

let decode payload =
  let json =
    try Yojson.Safe.from_string payload
    with Yojson.Json_error _ -> failwith "invalid pickup payload"
  in
  match json with
  | `List
      [ `String "relay"; `String channel; `String ts; `List ids; `String nonce ]
    ->
      let file_ids =
        List.map
          (function
            | `String id -> id | _ -> failwith "invalid pickup file identity")
          ids
      in
      C.Own_relay { channel; ts; file_ids; nonce }
  | `List [ `String "reaction"; channel; `String user; ts; file ] ->
      C.Target_reaction
        {
          channel = decode_opt channel;
          user;
          item_ts = decode_opt ts;
          item_file = decode_opt file;
        }
  | `List
      [
        `String "reply";
        `String channel;
        `String thread_ts;
        `String ts;
        `String bot_id;
        app;
        `String first;
        `String later;
      ] ->
      C.Target_reply
        {
          channel;
          thread_ts;
          ts;
          bot_id;
          app_id = decode_opt app;
          username = None;
          kind_if_first = decode_kind first;
          kind_if_later = decode_kind later;
        }
  | _ -> failwith "invalid pickup payload"

let matches (o : T.order) (r : Store.fire_runtime) (a : Store.relay_attempt) =
  function
  | C.Own_relay { channel; ts; file_ids; nonce } ->
      (* The observer must verify the stored own bot_id before enqueueing this
         constructor, whose classifier shape deliberately omits bot_id. *)
      channel = o.channel_id && r.nonce = Some nonce && T.valid_slack_ts ts
      && a.dispatched_at <> None
      && (a.relay_ts = None || a.relay_ts = Some ts)
      && file_ids <> []
      && List.for_all (Slack_config_types.valid_id "F") file_ids
      && List.length (List.sort_uniq String.compare file_ids)
         = List.length file_ids
      && List.sort String.compare file_ids
         = List.sort String.compare a.uploaded_file_ids
  | C.Target_reaction { channel; user; item_ts; item_file } -> (
      user = (T.action_target o.action).user_id
      && (channel = None || channel = Some o.channel_id)
      &&
      match (item_ts, item_file) with
      | Some ts, None ->
          channel = Some o.channel_id
          && a.relay_ts = Some ts && T.valid_slack_ts ts
      | None, Some id ->
          Slack_config_types.valid_id "F" id && List.mem id a.uploaded_file_ids
      | _ -> false)
  | C.Target_reply { channel; thread_ts; ts; bot_id; app_id; _ } ->
      let target = T.action_target o.action in
      channel = o.channel_id
      && a.relay_ts = Some thread_ts
      && thread_ts <> ts && T.valid_slack_ts ts && bot_id = target.bot_id
      && (app_id = None || app_id = Some target.app_id)
  | _ -> false

let enqueue ~db ~team_id ~api_app_id ~now observation =
  init db;
  if team_id <> "" && api_app_id <> "" then
    match observation with
    | C.Own_relay _ | C.Target_reaction _ | C.Target_reply _ -> (
        let result =
          Store.in_tx db ~on_error:Fun.id (fun () ->
              let fires =
                Store.query db
                  "SELECT f.id FROM slack_standing_order_fires f JOIN \
                   slack_standing_orders o ON o.id=f.order_id WHERE \
                   o.team_id=? AND o.api_app_id=?"
                  [ Store.text team_id; Store.text api_app_id ]
                  (fun s -> Sql_util.text_column s 0)
              in
              List.iter
                (fun id ->
                  match Store.runtime_raw db ~fire_id:id with
                  | None -> ()
                  | Some r -> (
                      match Store.find_raw db r.fire.order_id with
                      | None -> ()
                      | Some o ->
                          List.iter
                            (fun (a : Store.relay_attempt) ->
                              if matches o r a observation then
                                Store.exec db
                                  "INSERT INTO \
                                   slack_standing_order_observations \
                                   (fire_id,attempt_no,team_id,api_app_id,payload,observed_at) \
                                   VALUES (?,?,?,?,?,?) ON CONFLICT DO NOTHING"
                                  [
                                    Store.text id;
                                    Store.int a.attempt_no;
                                    Store.text team_id;
                                    Store.text api_app_id;
                                    Store.text
                                      (Yojson.Safe.to_string
                                         (encode observation));
                                    Sqlite3.Data.FLOAT now;
                                  ])
                            r.attempts))
                fires;
              Ok ())
        in
        match result with Ok () -> () | Error e -> failwith e)
    | _ -> ()

let apply ~db ~team_id ~api_app_id observation =
  enqueue ~db ~team_id ~api_app_id ~now:(Store.now ()) observation

(* The fire row is the result ledger: revisions replace one result, never add a
   second failure. Neutral outcomes do not break or erase the streak. *)
let failure_streak ~db ~order_id =
  Store.ensure_schema db;
  Store.query db
    "SELECT state FROM slack_standing_order_fires WHERE order_id=? ORDER BY \
     COALESCE(dispatched_at,CAST(date_start AS REAL)),rowid"
    [ Store.text order_id ]
    (fun s -> Sql_util.text_column s 0)
  |> List.fold_left
       (fun streak -> function
         | "no_pickup" | "not_delivered" | "failed" -> streak + 1
         | "picked_up" -> 0
         | _ -> streak)
       0

let failed_result (r : Store.fire_runtime) =
  List.mem r.fire.state T.[ No_pickup; Not_delivered; Failed ]

let first_some old value = match old with Some _ -> old | None -> Some value

let live_lease lease r now =
  Store.valid_lease lease r now && Store.valid_lease lease r (Store.now ())

let reduce (a : Store.relay_attempt) observation observed_at =
  match observation with
  | C.Own_relay { ts; _ } -> { a with relay_ts = Some ts }
  | C.Target_reaction _ ->
      {
        a with
        reaction_seen = true;
        acknowledged_at = first_some a.acknowledged_at observed_at;
      }
  | C.Target_reply { ts; kind_if_first; kind_if_later; _ } -> (
      let first = a.first_reply_ts = None || a.first_reply_ts = Some ts in
      let a = { a with first_reply_ts = first_some a.first_reply_ts ts } in
      match if first then kind_if_first else kind_if_later with
      | C.Reply ->
          {
            a with
            acknowledged_at = first_some a.acknowledged_at observed_at;
            picked_up_at = first_some a.picked_up_at observed_at;
          }
      | C.Notice C.Rate_limited ->
          { a with rate_limited_at = first_some a.rate_limited_at observed_at }
      | C.Notice (C.Disabled | C.Spend_limit | C.Not_delivered) ->
          { a with not_delivered = true })
  | _ -> a

let project (r : Store.fire_runtime) =
  let relay_ts =
    Option.bind (Store.attempt r r.current_attempt)
      (fun (a : Store.relay_attempt) -> a.relay_ts)
  in
  let any f = List.exists f r.attempts in
  let state, reason =
    if any (fun (a : Store.relay_attempt) -> a.not_delivered) then
      (T.Not_delivered, Some "not_delivered")
    else if any (fun (a : Store.relay_attempt) -> a.picked_up_at <> None) then
      (T.Picked_up, None)
    else if any (fun (a : Store.relay_attempt) -> a.acknowledged_at <> None)
    then (T.Acknowledged, None)
    else if
      relay_ts <> None
      && List.mem r.fire.state T.[ Posting; Posting_unknown; Uploading ]
    then (T.Posted, None)
    else (r.fire.state, r.fire.reason)
  in
  { r with fire = { r.fire with state; reason; relay_ts } }

let drain db ~lease ~fire_id ~now =
  Store.in_tx db ~on_error:Fun.id (fun () ->
      match Store.runtime_raw db ~fire_id with
      | None -> Ok ()
      | Some before when not (live_lease lease before now) -> Ok ()
      | Some before -> (
          let o =
            match Store.find_raw db before.fire.order_id with
            | Some o -> o
            | None -> failwith "pickup order missing"
          in
          let rows =
            Store.query db
              "SELECT rowid,attempt_no,team_id,api_app_id,payload,observed_at \
               FROM slack_standing_order_observations WHERE fire_id=? ORDER BY \
               rowid"
              [ Store.text fire_id ]
              (fun s ->
                ( Sql_util.int_column s 0,
                  Sql_util.int_column s 1,
                  Sql_util.text_column s 2,
                  Sql_util.text_column s 3,
                  decode (Sql_util.text_column s 4),
                  match Store.opt_float_column s 5 with
                  | Some f when Float.is_finite f -> f
                  | _ -> failwith "invalid observation time" ))
          in
          let attempts =
            List.fold_left
              (fun attempts (_, n, team, app, observation, at) ->
                List.map
                  (fun (a : Store.relay_attempt) ->
                    if
                      a.attempt_no = n && team = o.team_id && app = o.api_app_id
                      && matches o before a observation
                    then reduce a observation at
                    else a)
                  attempts)
              before.attempts rows
          in
          let after = project { before with attempts } in
          match Store.cas_in_tx db ~lease ~before ~after ~now with
          | Error _ as e -> e
          | Ok false -> Ok ()
          | Ok true ->
              List.iter
                (fun (id, _, _, _, _, _) ->
                  Store.exec db
                    "DELETE FROM slack_standing_order_observations WHERE \
                     rowid=?"
                    [ Store.int id ])
                rows;
              if failed_result after && failure_streak ~db ~order_id:o.id >= 3
              then
                Result.map
                  (fun _ -> ())
                  (Store.pause_if_epoch_in_tx db ~order_id:o.id
                     ~epoch:before.fire.epoch ~by:"automatic_failures" ~now)
              else Ok ()))

let get key = function
  | `Assoc fields -> Option.value (List.assoc_opt key fields) ~default:`Null
  | _ -> `Null

(* A partial reaction user list can prove acknowledgement, never absence. *)
let read_reactions ~call ~(order : T.order) ~file ~ts =
  let params, key =
    match (file, ts) with
    | Some id, _ -> ([ ("file", `String id) ], "file")
    | _, Some ts ->
        ( [ ("channel", `String order.T.channel_id); ("timestamp", `String ts) ],
          "message" )
    | _ -> failwith "missing reaction identity"
  in
  let* result = call "reactions.get" params in
  match result with
  | Error _ -> Lwt.return (false, [])
  | Ok json -> (
      match get "reactions" (get key json) with
      | `List reactions ->
          let target = (T.action_target order.action).user_id in
          let complete, seen =
            List.fold_left
              (fun (complete, seen) reaction ->
                match (get "users" reaction, get "count" reaction) with
                | `List users, `Int count ->
                    ( complete
                      && count = List.length users
                      && count >= 0
                      && List.length (List.sort_uniq compare users) = count
                      && List.for_all
                           (function
                             | `String id -> Slack_config_types.valid_id "UW" id
                             | _ -> false)
                           users,
                      seen || List.mem (`String target) users )
                | _ -> (false, seen))
              (true, false) reactions
          in
          let observations =
            if seen then
              [
                C.Target_reaction
                  {
                    channel = Some order.channel_id;
                    user = target;
                    item_ts = ts;
                    item_file = file;
                  };
              ]
            else []
          in
          Lwt.return (complete, observations)
      | _ -> Lwt.return (false, []))

let read_replies ~call ~(order : T.order) ts =
  let target = T.action_target order.action in
  let context =
    C.
      {
        self_user_id = None;
        targets =
          [
            {
              target_user_id = target.user_id;
              target_bot_id = target.bot_id;
              target_app_id = target.app_id;
            };
          ];
        matchers;
      }
  in
  let rec pages remaining seen cursor observations =
    let* result =
      call "conversations.replies"
        ([
           ("channel", `String order.channel_id);
           ("ts", `String ts);
           ("limit", `Int 200);
         ]
        @ if cursor = "" then [] else [ ("cursor", `String cursor) ])
    in
    match result with
    | Error _ -> Lwt.return (false, observations)
    | Ok json -> (
        match get "messages" json with
        | `List messages when List.length messages <= 200 -> (
            let valid =
              List.for_all
                (fun m ->
                  match get "ts" m with
                  | `String s -> T.valid_slack_ts s
                  | _ -> false)
                messages
            in
            let malformed_target = ref false in
            let new_observations =
              List.filter_map
                (fun m ->
                  let event =
                    match m with
                    | `Assoc fields ->
                        `Assoc
                          (("type", `String "message")
                          :: ("channel", `String order.channel_id)
                          :: fields)
                    | _ -> `Null
                  in
                  match
                    C.classify context
                      (`Assoc
                         [
                           ("type", `String "event_callback"); ("event", event);
                         ])
                  with
                  | C.Target_reply _ as observation -> Some observation
                  | _ ->
                      if
                        get "bot_id" m = `String target.bot_id
                        || get "user" m = `String target.user_id
                      then malformed_target := true;
                      None)
                messages
            in
            let observations = observations @ new_observations in
            if (not valid) || !malformed_target then
              Lwt.return (false, observations)
            else
              let cursor = get "next_cursor" (get "response_metadata" json) in
              match (get "has_more" json, cursor) with
              | (`Bool false | `Null), `String "" | `Bool false, `Null ->
                  Lwt.return (true, observations)
              | (`Bool true | `Bool false | `Null), `String next
                when next <> "" && remaining > 1 && not (List.mem next seen) ->
                  pages (remaining - 1) (next :: seen) next observations
              | _ -> Lwt.return (false, observations))
        | _ -> Lwt.return (false, observations))
  in
  pages 5 [] "" []

let read_all ~call ~order (r : Store.fire_runtime) =
  let* results =
    Lwt_list.map_s
      (fun (a : Store.relay_attempt) ->
        let* files =
          Lwt_list.map_s
            (fun file -> read_reactions ~call ~order ~file:(Some file) ~ts:None)
            a.uploaded_file_ids
        in
        match a.relay_ts with
        | None -> Lwt.return ((false, []) :: files)
        | Some ts ->
            let* reactions =
              read_reactions ~call ~order ~file:None ~ts:(Some ts)
            in
            let* replies = read_replies ~call ~order ts in
            Lwt.return (reactions :: replies :: files))
      r.attempts
  in
  let results = List.concat results in
  Lwt.return
    (List.for_all fst results && r.attempts <> [], List.concat_map snd results)

let rate_wait db (r : Store.fire_runtime) =
  r.current_attempt = 0 && r.fire.reposts = 0
  && (not r.fire.repost_forbidden)
  && Store.relay_allowed db r
  && (not
        (List.exists
           (fun (a : Store.relay_attempt) ->
             a.reaction_seen || a.acknowledged_at <> None
             || a.picked_up_at <> None || a.not_delivered)
           r.attempts))
  && Option.fold ~none:false
       ~some:(fun (a : Store.relay_attempt) -> a.rate_limited_at <> None)
       (Store.attempt r 0)

let schedule db (r : Store.fire_runtime) ~complete ~now =
  let current = Store.attempt r r.current_attempt in
  let start =
    Option.bind current (fun (a : Store.relay_attempt) -> a.pickup_started_at)
  in
  let deadline seconds = Option.map (fun at -> at +. seconds) start in
  let pending due =
    Some (if due > now then due else now +. Store.renewal_seconds)
  in
  if rate_wait db r then
    let due =
      Option.get (Option.get (Store.attempt r 0)).rate_limited_at +. 300.
    in
    { r with next_attempt_at = Some due }
  else
    match r.fire.state with
    | T.Posted | T.Posting_unknown -> (
        match deadline 180. with
        | Some due when due <= now && complete ->
            {
              r with
              fire =
                { r.fire with state = T.No_pickup; reason = Some "no_pickup" };
              next_attempt_at = None;
            }
        | Some due -> { r with next_attempt_at = pending due }
        | None -> r)
    | T.Acknowledged -> (
        let deadlines =
          List.filter_map
            (fun (a : Store.relay_attempt) ->
              if a.acknowledged_at = None then None
              else Option.map (fun at -> at +. 3600.) a.pickup_started_at)
            r.attempts
        in
        match List.sort Float.compare deadlines with
        | due :: _ when due <= now && complete ->
            {
              r with
              fire = { r.fire with reason = Some "acknowledged_no_reply" };
              next_attempt_at = None;
            }
        | due :: _ -> { r with next_attempt_at = pending due }
        | [] -> r)
    | _ -> { r with next_attempt_at = None }

let confirm ~db ~call ~lease (r : Store.fire_runtime) ~now =
  init db;
  let unwrap = function Ok v -> v | Error e -> failwith e in
  unwrap (drain db ~lease ~fire_id:r.fire.id ~now);
  match Store.runtime db ~fire_id:r.fire.id with
  | None -> Lwt.return_unit
  | Some before when not (live_lease lease before now) -> Lwt.return_unit
  | Some before ->
      let order = Option.get (Store.find db before.fire.order_id) in
      let hard_reason = ref None in
      let call method_ params =
        let live =
          Option.fold ~none:false
            ~some:(fun r -> live_lease lease r now)
            (Store.runtime db ~fire_id:r.fire.id)
        in
        if (not live) || !hard_reason <> None then
          Lwt.return (Error SO.Dispatch_refused)
        else
          let* result = call method_ params in
          (match result with
          | Error error ->
              let failure =
                Slack_standing_order_provenance.grant_failure ~method_ error
              in
              if failure.cls = Grant.Hard then
                hard_reason := Some (Grant.reason_code failure.reason)
          | Ok _ -> ());
          Lwt.return result
      in
      let* complete, observations = read_all ~call ~order before in
      List.iter
        (enqueue ~db ~team_id:order.team_id ~api_app_id:order.api_app_id ~now)
        observations;
      unwrap (drain db ~lease ~fire_id:r.fire.id ~now);
      unwrap
        (Store.in_tx db ~on_error:Fun.id (fun () ->
             match Store.runtime_raw db ~fire_id:r.fire.id with
             | None -> Ok ()
             | Some current when not (live_lease lease current now) -> Ok ()
             | Some current -> (
                 let identities r =
                   List.map
                     (fun (a : Store.relay_attempt) ->
                       ( a.attempt_no,
                         a.relay_ts,
                         a.uploaded_file_ids,
                         a.dispatched_at ))
                     r.Store.attempts
                 in
                 let complete =
                   complete
                   && identities before = identities current
                   && Store.int_query db
                        "SELECT COUNT(*) FROM \
                         slack_standing_order_observations WHERE fire_id=?"
                        [ Store.text r.fire.id ]
                      = 0
                 in
                 let after = schedule db current ~complete ~now in
                 let after =
                   match !hard_reason with
                   | None -> after
                   | Some reason ->
                       {
                         after with
                         fire = { after.fire with reason = Some reason };
                       }
                 in
                 match
                   Store.cas_in_tx db ~lease ~before:current ~after ~now
                 with
                 | Error _ as e -> e
                 | Ok false -> Ok ()
                 | Ok true -> (
                     match !hard_reason with
                     | Some reason ->
                         Result.map
                           (fun _ -> ())
                           (Store.suspend_if_epoch_in_tx db ~order_id:order.id
                              ~epoch:current.fire.epoch ~reason ~now)
                     | None ->
                         if
                           failed_result after
                           && failure_streak ~db ~order_id:order.id >= 3
                         then
                           Result.map
                             (fun _ -> ())
                             (Store.pause_if_epoch_in_tx db ~order_id:order.id
                                ~epoch:current.fire.epoch
                                ~by:"automatic_failures" ~now)
                         else Ok ()))));
      Lwt.return_unit
