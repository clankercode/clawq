open Lwt.Syntax
(** Fixed management-thread reports (spec §5.5, §6.4, §6.7). A pending receipt
    is consumed synchronously before transport. Ambiguous attempts are terminal:
    Slack has no idempotency key for these notifications. *)

module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module SQL = Slack_standing_order_sql
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Render = Slack_standing_order_render
module Ctx = Slack_standing_order_ctx

type cause =
  | Missing_scope of string
  | Target_not_member
  | No_pickup
  | Not_delivered
  | Grantor_unauthorised
  | Posture_changed
  | Repeated_failures
  | Cap_hit
  | Transcript_unavailable
  | Catchup_notice_unknown of string
  | Reconcile_gap of string
  | No_notes_hint
  | Reply_delayed
  | Authority_refused of Grant.reason

let method_code s =
  if List.mem s SO.operations then s
  else invalid_arg "standing-order report: invalid method code"

let window_code s =
  let timestamp = T.valid_slack_ts in
  if
    Slack_config_types.valid_id "R" s
    || timestamp s
    ||
    match String.split_on_char '-' s with
    | [ first; last ] -> timestamp first && timestamp last
    | _ -> false
  then s
  else invalid_arg "standing-order report: invalid reconciliation window"

(* Render.reason_text is shared copy, but its string-bearing constructors
   must be validated here before entering report text or receipt keys. *)
let checked_reason = function
  | Grant.Missing_scope m -> Grant.Missing_scope (method_code m)
  | Grant.Egress_denied m -> Grant.Egress_denied (method_code m)
  | Grant.Indeterminate m -> Grant.Indeterminate (method_code m)
  | Grant.Tool_denied name when List.mem name Grant.capability_names ->
      Grant.Tool_denied name
  | Grant.Tool_denied _ ->
      invalid_arg "standing-order report: invalid capability code"
  | Grant.Duplicate_order n when n < 1 ->
      invalid_arg "standing-order report: invalid order number"
  | r -> r

let batch_code s =
  if
    String.length s <= 128
    && String.length s > 0
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false)
         s
  then s
  else invalid_arg "invalid catch-up batch identity"

let cause_key = function
  | Missing_scope m -> "missing_scope:" ^ method_code m
  | Target_not_member -> "target_not_member"
  | No_pickup -> "no_pickup"
  | Not_delivered -> "not_delivered"
  | Grantor_unauthorised -> "grantor_unauthorised"
  | Posture_changed -> "posture_changed"
  | Repeated_failures -> "repeated_failures"
  | Cap_hit -> "cap_hit"
  | Transcript_unavailable -> "transcript_unavailable"
  | Catchup_notice_unknown id -> "catchup_notice_unknown:" ^ batch_code id
  | Reconcile_gap window -> "reconcile_gap:" ^ window_code window
  | No_notes_hint -> "no_notes_hint"
  | Reply_delayed -> "reply_delayed"
  | Authority_refused reason ->
      "authority_refused:" ^ Grant.reason_code (checked_reason reason)

let text (o : T.order) cause =
  if
    (not (Slack_config_types.valid_id "UW" o.grantor_user_id))
    || o.display_no < 1
  then invalid_arg "standing-order report: invalid order identity";
  let suffix =
    match cause with
    | Missing_scope method_ ->
        Printf.sprintf
          "Slack permission is missing for %s; ask an operator to check scopes \
           and reinstall."
          (method_code method_)
    | Target_not_member ->
        "the target is no longer a channel member; the order is suspended."
    | No_pickup -> "Claude did not acknowledge the relay within 3 minutes."
    | Not_delivered -> "Claude reported that the relay was not delivered."
    | Grantor_unauthorised ->
        "the grantor no longer has authority; the order is suspended."
    | Posture_changed ->
        "channel privacy or membership changed; the order is suspended."
    | Repeated_failures ->
        "3 consecutive deliveries failed; the order is paused."
    | Cap_hit -> "the daily relay cap was reached."
    | Transcript_unavailable ->
        "transcript unavailable (Slack API change?); the configured notes-only \
         or skip policy applies."
    | Catchup_notice_unknown id ->
        "catch-up notice outcome is unknown for batch " ^ batch_code id
        ^ "; its huddles remain held for operator inspection."
    | Reconcile_gap window ->
        "reconciliation did not cover " ^ window_code window ^ "."
    | No_notes_hint ->
        "no AI notes appeared within 60 minutes; check automatic huddle notes."
    | Reply_delayed ->
        "Claude acknowledged the relay but has not replied within 60 minutes; \
         this is information only."
    | Authority_refused reason ->
        "authority check refused the relay: "
        ^ Render.reason_text (checked_reason reason)
        ^ "."
  in
  Printf.sprintf "<@%s> Standing order %s: %s" o.grantor_user_id
    (T.display_name o) suffix

(* Pure routing for Task 16. Suspension and failure-streak pausing remain
   epoch-guarded store operations owned by the executor, never by reporting. *)
let of_hard_reason : Grant.reason -> cause = function
  | Missing_scope m -> Missing_scope m
  | Target_not_member -> Target_not_member
  | Channel_shared | Channel_has_guests | Private_policy_denied ->
      Posture_changed
  | Grantor_unavailable | Grantor_guest | Grantor_not_member
  | Grantor_not_allowed | Grantor_not_admin ->
      Grantor_unauthorised
  | reason -> Authority_refused reason

let of_transcript ~unavailable =
  if unavailable then Some Transcript_unavailable else None

let of_fire (f : T.fire) =
  match f.state with
  | No_notes -> Some No_notes_hint
  | No_pickup -> Some No_pickup
  | Not_delivered -> Some Not_delivered
  | Skipped_cap -> Some Cap_hit
  | Acknowledged when f.reason = Some "acknowledged_no_reply" ->
      Some Reply_delayed
  | _ -> None

let init db =
  Store.ensure_schema db;
  SQL.exec db
    "CREATE TABLE IF NOT EXISTS slack_standing_order_reports (order_id TEXT \
     NOT NULL, cause_key TEXT NOT NULL, period_key TEXT NOT NULL,fire_id TEXT, \
     status TEXT NOT NULL CHECK (status IN \
     ('pending','sent','ambiguous','refused','local_only')), message_ts \
     TEXT,PRIMARY KEY(order_id,cause_key,period_key))"
    []

let statuses ~db ~order_id =
  init db;
  SQL.query db
    "SELECT cause_key,status FROM slack_standing_order_reports WHERE \
     order_id=? ORDER BY cause_key,period_key"
    [ SQL.text order_id ]
    (fun stmt -> (Sql_util.text_column stmt 0, Sql_util.text_column stmt 1))

let period_key ~now = function
  | Cap_hit ->
      let tm = Unix.gmtime now in
      Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
        tm.tm_mday
  | _ -> "once"

(* Unlike Grant.gate, this check is entirely local and creates no snapshot.
   Reports must be possible after suspension, without granting relay writes. *)
let capabilities_allowed (config : Runtime_config.t) ~channel_id =
  if not config.security.tools_enabled then false
  else
    let access =
      Runtime_config.resolve_effective_access config
        ~session_key:(SO.access_key ~channel_id)
        ()
    in
    let values =
      List.map (fun (item : Runtime_config.effective_access_item) -> item.value)
    in
    List.for_all
      (fun name ->
        match
          Tool_authz.decide ~equivalence_names:[ name ]
            ~allowed_tools:(values access.allowed_tools)
            ~denied_tools:(values access.denied_tools)
            ()
        with
        | Tool_authz.Allowed -> true
        | Tool_authz.Denied _ -> false)
      Grant.capability_names

let post_once ~(ctx : Ctx.t) ~db ~manager ?http ?http_response ?reconcile_id
    ?fire_id ?period (order : T.order) cause =
  init db;
  (* Reload the immutable management route and grantor from persistence. *)
  match Store.find db order.id with
  | None -> Lwt.return_unit
  | Some o -> (
      let key = cause_key cause in
      let period =
        Option.value period ~default:(period_key ~now:(ctx.now ()) cause)
      in
      let identity = [ SQL.text o.id; SQL.text key; SQL.text period ] in
      SQL.exec db
        "INSERT INTO \
         slack_standing_order_reports(order_id,cause_key,period_key,fire_id,status) \
         VALUES(?,?,?,?,'pending') ON CONFLICT(order_id,cause_key,period_key) \
         DO NOTHING"
        (identity
        @ [ Option.fold ~none:Sqlite3.Data.NULL ~some:SQL.text fire_id ]);
      let pending () =
        SQL.int_query db
          "SELECT COUNT(*) FROM slack_standing_order_reports WHERE order_id=? \
           AND cause_key=? AND period_key=? AND status='pending'"
          identity
        = 1
      in
      let update ~from status message_ts =
        SQL.exec db
          "UPDATE slack_standing_order_reports SET status=?,message_ts=? WHERE \
           order_id=? AND cause_key=? AND period_key=? AND status=?"
          ([
             SQL.text status;
             Option.fold ~none:Sqlite3.Data.NULL ~some:SQL.text message_ts;
           ]
          @ identity
          @ [ SQL.text from ]);
        Sqlite3.changes db = 1
      in
      let finish_pending status =
        if update ~from:"pending" status None then
          Logs.warn (fun m ->
              m "standing-order report %s %s %s" o.id key status)
      in
      let current_config () = Session.get_config manager in
      let local_allowed () =
        match cause with
        | Authority_refused (Grant.Tool_denied _) -> false
        | _ ->
            capabilities_allowed (current_config ())
              ~channel_id:o.management_channel_id
      in
      let fence () =
        if not (local_allowed ()) then (
          finish_pending "local_only";
          Error SO.Dispatch_refused)
        else if not (ctx.accepting ()) then Error SO.Dispatch_refused
        else Ok ()
      in
      if not (pending ()) then Lwt.return_unit
      else
        match fence () with
        | Error _ -> Lwt.return_unit
        | Ok () -> (
            let config = current_config () in
            let order_ref = Some { Access_snapshot.order_id = o.id; fire_id } in
            match
              SO.snapshot ~db ~config ~channel_id:o.management_channel_id
                ~order_ref
            with
            | Error _ ->
                finish_pending "refused";
                Lwt.return_unit
            | Ok snapshot ->
                let attempted = ref false in
                let before_send () =
                  match fence () with
                  | Error _ as e -> e
                  | Ok () ->
                      (* The CAS is durable before raw HTTP, with no intervening await. *)
                      if update ~from:"pending" "ambiguous" None then (
                        attempted := true;
                        Ok ())
                      else Error SO.Dispatch_refused
                in
                let limit () =
                  match fence () with
                  | Error _ -> Lwt.return_unit
                  | Ok () ->
                      let+ () = ctx.limit ~channel_id:o.management_channel_id in
                      ignore (fence ())
                in
                let call =
                  SO.make ?limiter:ctx.Ctx.limiter ~db ~current_config ~snapshot
                    ~channel_id:o.management_channel_id
                    ~audit:
                      (SO.audit_label ~order_id:(Some o.id)
                         ~unit_label:
                           (match (fire_id, reconcile_id) with
                           | Some id, _ -> "fire:" ^ id ^ ":report"
                           | None, Some id -> "reconcile:" ^ id
                           | None, None ->
                               "reconcile:report:" ^ key ^ ":" ^ period))
                    ?http ?http_response ~limit ~before_send ()
                in
                let* result =
                  call "chat.postMessage"
                    [
                      ("channel", `String o.management_channel_id);
                      ("thread_ts", `String o.management_thread_ts);
                      ("text", `String (text o cause));
                      ("unfurl_links", `Bool false);
                      ("unfurl_media", `Bool false);
                    ]
                in
                if !attempted then (
                  let status, ts =
                    match result with
                    | Ok json ->
                        let ts = Slack_huddles.field "ts" json in
                        if
                          T.valid_slack_ts ts
                          && Slack_huddles.field "channel" json
                             = o.management_channel_id
                        then ("sent", Some ts)
                        else ("ambiguous", None)
                    | Error (SO.Transport | SO.Http_status _) ->
                        ("ambiguous", None)
                    | Error _ -> ("refused", None)
                  in
                  ignore (update ~from:"ambiguous" status ts);
                  if status <> "sent" then
                    Logs.warn (fun m ->
                        m "standing-order report %s %s %s" o.id key status))
                else if result <> Error SO.Dispatch_refused then
                  finish_pending "refused";
                Lwt.return_unit))
