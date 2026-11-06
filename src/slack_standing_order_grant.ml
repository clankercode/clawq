(* Grant authority (spec §6.4): the zero-HTTP capability gate, the full
   re-validation run at plan, confirm, replace, resume and fire, typed failure
   classes, and the pause-time member check. Every Slack call goes through
   [Slack_standing_order_slack]. No HTTP happens before [auth.test], and the
   order row is read last, after the final yield, so a caller can act on [Ok]
   without yielding. *)
open Lwt.Syntax
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module SO = Slack_standing_order_slack

let get = Slack_huddles.get
let field = Slack_huddles.field
let flag = Slack_huddles.flag
let valid_id = Slack_huddles.valid_id
let capability_names = SO.capability_names
let max_pages = 5
let page_size = 200
let max_scan_members = max_pages * page_size
let max_member_lookups = 20

type stage =
  | Plan of { replaces : string option }
  | Confirm
  | Replace of { replaces : string; replaced_epoch : int }
  | Resume
  | Resolve
  | Fire of { fire_epoch : int; fire_id : string }
  | Track of { fire_id : string }

type failure_class = Hard | Transient | Inactive | Capped

(* No [Room_policy_denied] (Decision 7): every non-[Rm_group] posture is
   already a hard deny here. *)
type reason =
  | Feature_disabled
  | Credentials_invalid
  | Order_missing
  | Order_not_in_state of T.state
  | Order_expired
  | Epoch_mismatch
  | Daily_cap_reached
  | Activation_unannounced
  | Identity_changed
  | Target_not_allowlisted
  | Grantor_not_allowed
  | Grantor_not_admin
  | Tool_denied of string
  | Duplicate_order of int
  | Team_mismatch
  | App_mismatch
  | Bot_not_member
  | Private_policy_denied
  | Channel_shared
  | Channel_has_guests
  | Grantor_unavailable
  | Grantor_guest
  | Grantor_not_member
  | Target_unavailable
  | Target_not_member
  | Missing_scope of string
  | Egress_denied of string
  | Indeterminate of string
  | Config_changed

let classify = function
  | Feature_disabled | Order_missing | Order_not_in_state _ | Order_expired
  | Epoch_mismatch ->
      Inactive
  | Daily_cap_reached -> Capped
  | Indeterminate _ | Config_changed | Activation_unannounced -> Transient
  | Credentials_invalid | Identity_changed | Target_not_allowlisted
  | Grantor_not_allowed | Grantor_not_admin | Tool_denied _ | Duplicate_order _
  | Team_mismatch | App_mismatch | Bot_not_member | Private_policy_denied
  | Channel_shared | Channel_has_guests | Grantor_unavailable | Grantor_guest
  | Grantor_not_member | Target_unavailable | Target_not_member
  | Missing_scope _ | Egress_denied _ ->
      Hard

type failure = { cls : failure_class; reason : reason }

let failure reason = { cls = classify reason; reason }

let reason_code = function
  | Feature_disabled -> "feature_disabled"
  | Credentials_invalid -> "credentials_invalid"
  | Order_missing -> "order_missing"
  | Order_not_in_state s -> "order_not_in_state:" ^ T.state_to_string s
  | Order_expired -> "order_expired"
  | Epoch_mismatch -> "epoch_mismatch"
  | Daily_cap_reached -> "daily_cap_reached"
  | Activation_unannounced -> "activation_unannounced"
  | Identity_changed -> "identity_changed"
  | Target_not_allowlisted -> "target_not_allowlisted"
  | Grantor_not_allowed -> "grantor_not_allowed"
  | Grantor_not_admin -> "grantor_not_admin"
  | Tool_denied name -> "tool_denied:" ^ name
  | Duplicate_order n -> "duplicate_order:SO-" ^ string_of_int n
  | Team_mismatch -> "team_mismatch"
  | App_mismatch -> "app_mismatch"
  | Bot_not_member -> "bot_not_member"
  | Private_policy_denied -> "private_policy_denied"
  | Channel_shared -> "channel_shared"
  | Channel_has_guests -> "channel_has_guests"
  | Grantor_unavailable -> "grantor_unavailable"
  | Grantor_guest -> "grantor_guest"
  | Grantor_not_member -> "grantor_not_member"
  | Target_unavailable -> "target_unavailable"
  | Target_not_member -> "target_not_member"
  | Missing_scope m -> "missing_scope:" ^ m
  | Egress_denied m -> "egress_denied:" ^ m
  | Indeterminate m -> "indeterminate:" ^ m
  | Config_changed -> "config_changed"

type subject = {
  order_id : string;
  team_id : string;
  enterprise_id : string option;
  api_app_id : string;
  channel_id : string;
  grantor_user_id : string;
  target : T.target;
}

let subject_of_order (o : T.order) =
  {
    order_id = o.id;
    team_id = o.team_id;
    enterprise_id = o.enterprise_id;
    api_app_id = o.api_app_id;
    channel_id = o.channel_id;
    grantor_user_id = o.grantor_user_id;
    target = T.action_target o.action;
  }

type validated = {
  snapshot : Access_snapshot.t;
  config : Runtime_config.t;
  order : T.order option;  (** The fresh row, read last; [None] at a plan. *)
  replaced : T.order option;  (** The fresh old row when a plan replaces one. *)
  enterprise_id : string option;
  bot_user_id : string;
  bot_id : string;
  team_url : string;
  call : SO.call;
}

(* ── Step 1: the zero-HTTP gate ─────────────────────────────────────────── *)

let gate ~db ~(config : Runtime_config.t) ~channel_id ~order_ref =
  let deny reason = Error (failure reason) in
  Store.ensure_schema db;
  match config.channels.slack with
  | Some sc
    when sc.standing_orders.orders_enabled && config.security.tools_enabled -> (
      if not (Runtime_config.slack_has_valid_credentials sc) then
        deny Credentials_invalid
      else if sc.standing_orders.allowed_targets = [] then
        deny Target_not_allowlisted
      else
        match SO.snapshot ~db ~config ~channel_id ~order_ref with
        | Error _ -> deny (Indeterminate "access_snapshot")
        | Ok (snap : Access_snapshot.t) -> (
            let denied name =
              match
                Tool_authz.decide ~equivalence_names:[ name ]
                  ~allowed_tools:snap.allowed_tools
                  ~denied_tools:snap.denied_tools ()
              with
              | Tool_authz.Allowed -> false
              | Tool_authz.Denied _ -> true
            in
            match List.find_opt denied capability_names with
            | Some name -> deny (Tool_denied name)
            | None -> Ok (sc, snap)))
  | _ -> deny Feature_disabled

(* ── Step 2: the stage's view of the order rows (sync) ──────────────────── *)

(* Field names differ from [validated]'s so later modules' unannotated
   [v.order] keeps resolving to [validated]. *)
type rows = { row : T.order option; old_row : T.order option }

let ( let*? ) = Result.bind
let target_ids (t : T.target) = (t.user_id, t.app_id, t.bot_id)

(* Same installation, channel and target; the grantor is compared apart, since
   a change may be requested by someone else. *)
let same_scope (s : subject) (o : T.order) =
  o.team_id = s.team_id
  && o.api_app_id = s.api_app_id
  && o.channel_id = s.channel_id
  && target_ids (T.action_target o.action) = target_ids s.target

let require_state (o : T.order) states =
  if List.mem o.state states then Ok () else Error (Order_not_in_state o.state)

let no_live_conflict db (s : subject) ~excluding =
  match
    Store.find_live_conflict db ~team_id:s.team_id ~channel_id:s.channel_id
      ~target_app_id:s.target.app_id ~excluding
  with
  | Some o -> Error (Duplicate_order o.display_no)
  | None -> Ok ()

let own_row db (s : subject) =
  match Store.find db s.order_id with
  | None -> Error Order_missing
  | Some o
    when same_scope s o
         && o.enterprise_id = s.enterprise_id
         && o.grantor_user_id = s.grantor_user_id ->
      Ok o
  | Some _ -> Error Identity_changed

let replaced_row db (s : subject) id =
  match Store.find db id with
  | None -> Error Order_missing
  | Some o when not (same_scope s o) -> Error Identity_changed
  | Some o ->
      Result.map (fun () -> o) (require_state o T.[ Active; Paused; Suspended ])

let canonical_sql_datetime s =
  match
    Scanf.sscanf_opt s "%4d-%2d-%2d %2d:%2d:%2d%!" (fun y mo d h mi se ->
        Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" y mo d h mi se)
  with
  | Some canonical -> canonical = s
  | None -> false

(* [expires_at] is a [Time_util.sql_datetime_utc] text; anything else counts
   as expired. *)
let unexpired (o : T.order) ~now =
  match o.expires_at with
  | None -> true
  | Some at ->
      canonical_sql_datetime at && at > Time_util.sql_datetime_utc ~t:now ()

let plan_rows db (s : subject) ~replaces ~replaced_epoch =
  if Option.is_some (Store.find db s.order_id) then Error Identity_changed
  else
    let*? replaced =
      match replaces with
      | None -> Ok None
      | Some id -> Result.map Option.some (replaced_row db s id)
    in
    let*? () =
      match (replaced, replaced_epoch) with
      | Some (o : T.order), Some epoch when o.epoch <> epoch ->
          Error Epoch_mismatch
      | _ -> Ok ()
    in
    let*? () =
      no_live_conflict db s
        ~excluding:(Option.value replaces ~default:s.order_id)
    in
    Ok { row = None; old_row = replaced }

(* A fire id is authority only for its owning order. Use the strict store
   query before any Slack reads, and repeat it after the last await. *)
let bound_fire db (o : T.order) fire_id =
  match Store.query_fires db "WHERE id = ?" [ Sqlite3.Data.TEXT fire_id ] with
  | [ f ] when f.order_id = o.id -> Ok f
  | [ _ ] -> Error Identity_changed
  | _ -> Error (Indeterminate "fire")

let stage_rows db ~now ~stage (s : subject) =
  let only o = Ok { row = Some o; old_row = None } in
  match stage with
  | Plan { replaces } -> plan_rows db s ~replaces ~replaced_epoch:None
  | Replace { replaces; replaced_epoch } ->
      plan_rows db s ~replaces:(Some replaces)
        ~replaced_epoch:(Some replaced_epoch)
  | Confirm ->
      let*? o = own_row db s in
      let*? () = require_state o [ T.Pending_confirmation ] in
      if Store.pending_open o ~now then only o else Error Order_expired
  | Resume ->
      let*? o = own_row db s in
      let*? () = require_state o T.[ Paused; Suspended ] in
      let*? () = no_live_conflict db s ~excluding:o.id in
      only o
  | Track { fire_id } ->
      let*? o = own_row db s in
      let*? f = bound_fire db o fire_id in
      (* Dispatch evidence survives terminal states and pause/cancel epochs. *)
      if Option.is_some f.dispatched_at then only o
      else Error (Indeterminate "fire")
  | Resolve ->
      let*? o = own_row db s in
      let*? () = require_state o [ T.Active ] in
      if not (unexpired o ~now) then Error Order_expired
      else if not (Store.activation_announced db ~order_id:o.id) then
        Error Activation_unannounced
      else only o
  | Fire { fire_epoch; fire_id } ->
      let*? o = own_row db s in
      let*? f = bound_fire db o fire_id in
      let*? () = require_state o [ T.Active ] in
      let*? () =
        let root =
          Store.query db
            "SELECT root_ts FROM slack_standing_order_fires WHERE id=?"
            [ Sqlite3.Data.TEXT fire_id ] (fun s ->
              Sql_util.opt_text_column s 0)
        in
        let root = match root with [ Some ts ] -> ts | _ -> "" in
        if
          Slack_standing_order_rollout_store.suppressed db ~order_id:o.id
            ~root_ts:root ~room_id:f.room_id ~date_start:f.date_start
        then Error (Indeterminate "already_delivered")
        else Ok ()
      in
      if o.epoch <> fire_epoch || f.epoch <> fire_epoch then
        Error Epoch_mismatch
      else if not (unexpired o ~now) then Error Order_expired
      else if not (Store.activation_announced db ~order_id:o.id) then
        Error Activation_unannounced
      else if
        Store.dispatches_since db ~order_id:o.id ~since:(now -. 86400.)
        >= o.max_fires_per_day
      then Error Daily_cap_reached
      else only o

(* ── Step 3: target allowlist, grantor allowlist, creator policy (sync) ──── *)

let grantor_policy ~db (sc : Runtime_config.slack_config) (s : subject) =
  if
    not
      (List.exists
         (fun (t : Slack_config_types.standing_order_target_config) ->
           t.app_id = s.target.app_id)
         sc.standing_orders.allowed_targets)
  then Error Target_not_allowlisted
  else if
    not
      (Slack_api.is_allowed ~config:sc ~channel_id:s.channel_id
         ~user_id:s.grantor_user_id)
  then Error Grantor_not_allowed
  else
    match sc.standing_orders.creator_policy with
    | So_creator_admin
      when not
             (Admin.is_admin ~db ~channel:"slack" ~sender_id:s.grantor_user_id)
      ->
        Error Grantor_not_admin
    | So_creator_admin | So_creator_member -> Ok ()

(* ── Steps 5-10: Slack reads ────────────────────────────────────────────── *)

let ( let*! ) promise f =
  let* result = promise in
  match result with Ok v -> f v | Error _ as e -> Lwt.return e

let refuse reason = Lwt.return (Error reason)
let typed_bool key json = match get key json with `Bool _ -> true | _ -> false
let user_flags = [ "deleted"; "is_bot"; "is_restricted"; "is_ultra_restricted" ]

let reported_id prefix key json =
  match get key json with `String id -> valid_id prefix id | _ -> false

let cursor_params cursor =
  if cursor = "" then [] else [ ("cursor", `String cursor) ]

(* An untyped cursor is unknown, never "no more pages". *)
let next_cursor json =
  match get "next_cursor" (get "response_metadata" json) with
  | `String c -> Some c
  | _ -> None

let reason_of_error ~user_not_found method_ : SO.error -> reason = function
  | Dispatch_refused -> Epoch_mismatch
  | Rate_limited | Http_status _ | Transport | Audit_unavailable
  | Snapshot_unavailable ->
      Indeterminate method_
  | Slack_error "missing_scope" -> Missing_scope method_
  | Slack_error
      ("invalid_auth" | "token_revoked" | "account_inactive" | "not_authed") ->
      Credentials_invalid
  | Slack_error ("channel_not_found" | "not_in_channel") -> Bot_not_member
  | Slack_error "user_not_found" when method_ = "users.info" -> user_not_found
  | Slack_error "bot_not_found" -> App_mismatch
  | Slack_error _ -> Indeterminate method_
  | Egress_denied m | Method_not_permitted m -> Egress_denied m
  | Channel_denied -> Grantor_not_allowed
  | Not_configured -> Credentials_invalid

let request (call : SO.call) ?(user_not_found = Indeterminate "users.info")
    method_ params =
  let+ result = call method_ params in
  Result.map_error (reason_of_error ~user_not_found method_) result

(* Only workspace origins may supply relay links. Return a normalized trailing
   slash; paths, credentials and markup never enter the fixed comment. *)
let workspace_url url =
  try
    let uri = Uri.of_string url in
    match Uri.host uri with
    | Some host
      when Uri.scheme uri = Some "https"
           && String.ends_with ~suffix:".slack.com" host
           && String.length host > String.length ".slack.com"
           && String.for_all
                (function
                  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' -> true
                  | _ -> false)
                host
           && Uri.userinfo uri = None
           && Uri.port uri = None
           && Uri.query uri = []
           && Uri.fragment uri = None
           && (Uri.path uri = "" || Uri.path uri = "/") ->
        "https://" ^ String.lowercase_ascii host ^ "/"
    | _ -> ""
  with _ -> ""

(* Steps 5-6: the team (and enterprise) by auth.test, our app by bots.info. *)
let installation call (s : subject) =
  let*! auth = request call "auth.test" [] in
  let bot_user_id = field "user_id" auth and bot_id = field "bot_id" auth in
  let enterprise =
    match field "enterprise_id" auth with "" -> None | e -> Some e
  in
  if
    not
      (reported_id "T" "team_id" auth
      && valid_id "UW" bot_user_id && valid_id "B" bot_id
      && Option.fold ~none:true ~some:(valid_id "E") enterprise)
  then refuse (Indeterminate "auth.test")
  else if field "team_id" auth <> s.team_id then refuse Team_mismatch
  else if Option.is_some s.enterprise_id && s.enterprise_id <> enterprise then
    refuse Team_mismatch
  else
    let*! info = request call "bots.info" [ ("bot", `String bot_id) ] in
    if not (reported_id "A" "app_id" (get "bot" info)) then
      refuse (Indeterminate "bots.info")
    else if field "app_id" (get "bot" info) <> s.api_app_id then
      refuse App_mismatch
    else
      Lwt.return
        (Ok (bot_user_id, bot_id, enterprise, workspace_url (field "url" auth)))

(* Step 7, with [Grant_fire]'s share-flag rule ([Slack_huddles.channel_sharing])
   so an order that confirms can also read its huddles: a reported share is
   [Channel_shared] (hard), an unreported share state is indeterminate
   (transient). Room policy is not evaluated (Decision 7). *)
let posture call (sc : Runtime_config.slack_config) (s : subject) =
  let*! info =
    request call "conversations.info" [ ("channel", `String s.channel_id) ]
  in
  let channel = get "channel" info in
  let private_policy () =
    Slack_api.check_private_channel_policy ~config:sc ~channel_id:s.channel_id
      ~is_private_opt:(Some (flag "is_private" channel))
  in
  if not (reported_id "CG" "id" channel && typed_bool "is_member" channel) then
    refuse (Indeterminate "conversations.info")
  else if field "id" channel <> s.channel_id || not (flag "is_member" channel)
  then refuse Bot_not_member
  else
    match Slack_huddles.channel_sharing channel with
    | Slack_huddles.Shared -> refuse Channel_shared
    | Slack_huddles.Sharing_unreported ->
        refuse (Indeterminate "conversations.info")
    | Slack_huddles.Not_shared ->
        if not (typed_bool "is_private" channel) then
          refuse (Indeterminate "conversations.info")
        else if not (private_policy ()) then refuse Private_policy_denied
        else Lwt.return (Ok ())

let members_params ~channel_id cursor =
  [ ("channel", `String channel_id); ("limit", `Int page_size) ]
  @ cursor_params cursor

(* Step 8: one complete listing within [max_scan_members] ids. *)
let channel_members call ~channel_id =
  let indeterminate = refuse (Indeterminate "conversations.members") in
  let rec page cursor n acc =
    let*! json =
      request call "conversations.members" (members_params ~channel_id cursor)
    in
    match (get "members" json, next_cursor json) with
    | `List items, Some next
      when List.for_all (function `String _ -> true | _ -> false) items ->
        let acc = List.rev_append (List.map Slack_huddles.text items) acc in
        if List.length acc > max_scan_members then indeterminate
        else if next = "" then Lwt.return (Ok (List.rev acc))
        else if n >= max_pages then indeterminate
        else page next (n + 1) acc
    | _ -> indeterminate
  in
  page "" 1 []

let member_metadata (s : subject) ~id ~method_ user =
  let profile = get "profile" user in
  if
    not
      (reported_id "UW" "id" user
      && reported_id "T" "team_id" user
      && List.for_all (fun key -> typed_bool key user) user_flags
      && (id <> s.target.user_id
         || reported_id "A" "api_app_id" profile
            && reported_id "B" "bot_id" profile))
  then Error (Indeterminate method_)
  else if field "id" user <> id then Error Identity_changed
  else if field "team_id" user <> s.team_id then Error Team_mismatch
  else Ok ()

(* Step 9: a well-formed record per needed member, from at most [max_pages]
   users.list pages and then at most [max_member_lookups] users.info calls. *)
let member_records call (s : subject) ~needed =
  let records = Hashtbl.create 64 and wanted = Hashtbl.create 64 in
  List.iter (fun id -> Hashtbl.replace wanted id ()) needed;
  let missing () =
    List.filter (fun id -> not (Hashtbl.mem records id)) needed
  in
  (* Records for members we don't need are ignored, malformed or not. *)
  let take result user =
    let*? () = result in
    let id = field "id" user in
    if (not (Hashtbl.mem wanted id)) || Hashtbl.mem records id then Ok ()
    else
      let*? () = member_metadata s ~id ~method_:"users.list" user in
      Ok (Hashtbl.replace records id user)
  in
  let rec list_page cursor n =
    let*! json =
      request call "users.list"
        (("limit", `Int page_size) :: cursor_params cursor)
    in
    match (get "members" json, next_cursor json) with
    | `List users, Some next -> (
        match List.fold_left take (Ok ()) users with
        | Error _ as malformed -> Lwt.return malformed
        | Ok () when missing () = [] || next = "" || n >= max_pages ->
            Lwt.return (Ok ())
        | Ok () -> list_page next (n + 1))
    | _ -> refuse (Indeterminate "users.list")
  in
  let*! () = list_page "" 1 in
  let misses = missing () in
  if List.length misses > max_member_lookups then
    refuse (Indeterminate "users.list")
  else
    let rec lookup = function
      | [] -> Lwt.return (Ok records)
      | id :: rest -> (
          let user_not_found =
            if id = s.grantor_user_id then Grantor_unavailable
            else if id = s.target.user_id then Target_unavailable
            else Indeterminate "users.info"
          in
          let*! json =
            request call ~user_not_found "users.info" [ ("user", `String id) ]
          in
          let user = get "user" json in
          match member_metadata s ~id ~method_:"users.info" user with
          | Ok () ->
              Hashtbl.replace records id user;
              lookup rest
          | Error r -> refuse r)
    in
    lookup misses

let guest user = flag "is_restricted" user || flag "is_ultra_restricted" user

let judge_members (s : subject) records =
  match Hashtbl.find_opt records s.grantor_user_id with
  | None -> Error Grantor_unavailable
  | Some g when flag "deleted" g || flag "is_bot" g -> Error Grantor_unavailable
  | Some g when guest g -> Error Grantor_guest
  | Some _ -> (
      match Hashtbl.find_opt records s.target.user_id with
      | Some t
        when flag "is_bot" t
             && (not (flag "deleted" t))
             && field "api_app_id" (get "profile" t) = s.target.app_id
             && field "bot_id" (get "profile" t) = s.target.bot_id ->
          if
            Hashtbl.fold (fun _ user found -> found || guest user) records false
          then Error Channel_has_guests
          else Ok ()
      | _ -> Error Target_unavailable)

let slack_reads call sc (s : subject) =
  let*! bot_user_id, bot_id, enterprise_id, team_url = installation call s in
  let*! () = posture call sc s in
  let*! members = channel_members call ~channel_id:s.channel_id in
  if not (List.mem s.grantor_user_id members) then refuse Grantor_not_member
  else if not (List.mem s.target.user_id members) then refuse Target_not_member
  else
    let needed =
      List.sort_uniq String.compare
        (List.filter (fun id -> id <> bot_user_id) members)
    in
    let*! records = member_records call s ~needed in
    match judge_members s records with
    | Error r -> refuse r
    | Ok () -> Lwt.return (Ok (bot_user_id, bot_id, enterprise_id, team_url))

(* ── evaluate ───────────────────────────────────────────────────────────── *)

(* [auth.test] and [canvases.getContent] are reads despite using POST. An
   explicit list keeps Track read-only when the transport gains new writes. *)
let tracking_call (call : SO.call) method_ params =
  match method_ with
  | "auth.test" | "bots.info" | "users.info" | "users.list"
  | "users.conversations" | "conversations.info" | "conversations.members"
  | "conversations.history" | "conversations.replies" | "files.info"
  | "canvases.getContent" | "reactions.get" ->
      call method_ params
  | _ -> Lwt.return (Error (SO.Method_not_permitted method_))

let evaluate ~db ~current_config ?http ?http_response ?limiter ?priority ?limit
    ?before_send ?now ~stage ~unit_label (s : subject) =
  let now = match now with Some t -> t | None -> Store.now () in
  let run () =
    let cfg : Runtime_config.t = current_config () in
    let fire_id =
      match stage with
      | Fire { fire_id; _ } | Track { fire_id } -> Some fire_id
      | Plan _ | Confirm | Replace _ | Resume | Resolve -> None
    in
    let order_ref = Some { Access_snapshot.order_id = s.order_id; fire_id } in
    match gate ~db ~config:cfg ~channel_id:s.channel_id ~order_ref with
    | Error f -> Lwt.return (Error f)
    | Ok (sc, snapshot) -> (
        match
          let*? (_ : rows) = stage_rows db ~now ~stage s in
          grantor_policy ~db sc s
        with
        | Error r -> Lwt.return (Error (failure r))
        | Ok () ->
            let call =
              SO.make ~capability_fence:true ~db ~current_config ~snapshot
                ~channel_id:s.channel_id
                ~audit:(SO.audit_label ~order_id:(Some s.order_id) ~unit_label)
                ?http ?http_response ?limiter ?priority ~team_id:s.team_id
                ~app_id:s.api_app_id ?limit ?before_send ()
            in
            let call =
              match stage with Track _ -> tracking_call call | _ -> call
            in
            let+ reads = slack_reads call sc s in
            (* Step 11, with no yield from here to the caller's action: the
               same config, then step 3's admin read, then the rows, last. *)
            Result.map_error failure
              (let*? () =
                 match
                   SO.capability_denied (current_config ())
                     ~channel_id:s.channel_id
                 with
                 | Some name -> Error (Tool_denied name)
                 | None -> Ok ()
               in
               let*? bot_user_id, bot_id, enterprise_id, team_url = reads in
               let*? () =
                 if current_config () != cfg then Error Config_changed
                 else Ok ()
               in
               let*? () =
                 match
                   Slack_standing_order_rollout_store.revalidate_identity db
                     {
                       team_id = s.team_id;
                       app_id = s.api_app_id;
                       enterprise_id;
                       own_user_id = bot_user_id;
                       own_bot_id = bot_id;
                     }
                 with
                 | Ok true -> Ok ()
                 | Ok false -> Error Identity_changed
                 | Error _ -> Error (Indeterminate "rollout_identity")
               in
               let*? () = grantor_policy ~db sc s in
               let*? rows = stage_rows db ~now ~stage s in
               Ok
                 {
                   snapshot;
                   config = cfg;
                   order = rows.row;
                   replaced = rows.old_row;
                   enterprise_id;
                   bot_user_id;
                   bot_id;
                   team_url;
                   call;
                 }))
  in
  Lwt.catch run (function
    | Lwt.Canceled -> Lwt.fail Lwt.Canceled
    | exn ->
        Logs.warn (fun m ->
            m "standing-order grant evaluation failed: %s"
              (Printexc.to_string exn));
        Lwt.return (Error (failure (Indeterminate "internal"))))

(* ── Pause-time member check ────────────────────────────────────────────── *)

type member_refusal =
  | Member_unavailable
  | Member_guest
  | Member_not_member
  | Member_not_allowed
  | Member_team_mismatch
  | Member_indeterminate of string
  | Member_egress of string

let member_refusal_of_error method_ : SO.error -> member_refusal = function
  | Egress_denied m | Method_not_permitted m -> Member_egress m
  | Slack_error "user_not_found" when method_ = "users.info" ->
      Member_unavailable
  | Rate_limited | Http_status _ | Transport | Audit_unavailable
  | Snapshot_unavailable | Slack_error _ | Channel_denied | Not_configured
  | Dispatch_refused ->
      Member_indeterminate method_

let check_member ~(call : SO.call) ~(config : Runtime_config.t) ~team_id
    ~channel_id ~user_id ~require_allowlist =
  let ask method_ params =
    let+ result = call method_ params in
    Result.map_error (member_refusal_of_error method_) result
  in
  let allowlisted () =
    match config.channels.slack with
    | Some sc -> Slack_api.is_allowed ~config:sc ~channel_id ~user_id
    | None -> false
  in
  if require_allowlist && not (allowlisted ()) then refuse Member_not_allowed
  else
    let*! json = ask "users.info" [ ("user", `String user_id) ] in
    let user = get "user" json in
    if
      field "id" user <> user_id
      || (not (reported_id "T" "team_id" user))
      || not (List.for_all (fun key -> typed_bool key user) user_flags)
    then refuse (Member_indeterminate "users.info")
    else if field "team_id" user <> team_id then refuse Member_team_mismatch
    else if flag "deleted" user || flag "is_bot" user then
      refuse Member_unavailable
    else if guest user then refuse Member_guest
    else
      let rec page cursor n =
        let*! json =
          ask "conversations.members" (members_params ~channel_id cursor)
        in
        (* A page that lists the user proves membership; the cursor only
           matters for proving absence. Pause must not fail on a missing one. *)
        match get "members" json with
        | `List items when List.mem (`String user_id) items ->
            Lwt.return (Ok ())
        | `List _ -> (
            match next_cursor json with
            | Some "" -> refuse Member_not_member
            | Some next when n < max_pages -> page next (n + 1)
            | _ -> refuse (Member_indeterminate "conversations.members"))
        | _ -> refuse (Member_indeterminate "conversations.members")
      in
      page "" 1
