(** The fail-safe pause (spec §6.8): the exact-match [pause SO-n] parser, the
    pause core shared by the parser, the button and the tool, code-rendered
    receipts and the ingress hook.

    Pause authority is independent of chat authorization: a transport-verified
    route, a [users.info] human who is not deleted and not a guest, and channel
    membership. [allow_users], room policy, admin status and the capability gate
    are deliberately not consulted. A Slack write never rolls back a state
    change. *)

open Lwt.Syntax
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Render = Slack_standing_order_render
module Journal = Slack_standing_order_pause_store
module Sql = Slack_standing_order_sql

type replay = {
  call : SO.call;
  reconcile_id : string;
  snapshot_id : string;
  limiter : Slack_read_limiter.t;
  check : unit -> bool;
}

(* Replay never inherits upload or arbitrary management write authority. *)
let replay_call r method_ params =
  match method_ with
  | "users.info" | "conversations.members" | "chat.postMessage" ->
      r.call method_ params
  | _ -> Lwt.return (Error (SO.Method_not_permitted method_))

type source = Text | Button | Tool | Cli

let source_to_string = function
  | Text -> "text"
  | Button -> "button"
  | Tool -> "tool"
  | Cli -> "cli"

(* ── Parser ─────────────────────────────────────────────────────────────── *)

let is_mention_id id =
  String.length id >= 2
  && (id.[0] = 'U' || id.[0] = 'W')
  && String.for_all (function 'A' .. 'Z' | '0' .. '9' -> true | _ -> false) id

(* One leading [<@U…>] or [<@W…>], nothing else ([<@U…|label>] is not one). *)
let drop_mention s =
  if String.starts_with ~prefix:"<@" s then
    match String.index_opt s '>' with
    | Some close when is_mention_id (String.sub s 2 (close - 2)) ->
        String.sub s (close + 1) (String.length s - close - 1)
    | _ -> s
  else s

let drop_final_mark s =
  let n = String.length s in
  if n > 0 && (s.[n - 1] = '.' || s.[n - 1] = '!') then String.sub s 0 (n - 1)
  else s

let words s =
  String.split_on_char ' '
    (String.map (function '\t' | '\n' | '\r' -> ' ' | c -> c) s)
  |> List.filter (fun w -> w <> "")

let parse_command text =
  match
    words (drop_final_mark (String.trim (drop_mention (String.trim text))))
  with
  | [ verb; order ] when String.lowercase_ascii verb = "pause" ->
      T.parse_display order
  | _ -> None

(* ── Core ───────────────────────────────────────────────────────────────── *)

type outcome =
  | Paused of { order : T.order; in_flight : int option }
  | Already_paused of T.order
  | Withdrawn of T.order
  | Not_active of T.order  (** Suspended or cancelled. *)
  | Not_found of int
  | Unreadable of int
      (** The store could not be read, so whether SO-n exists here is unknown.
          Only a [`Display] lookup gives it: an [`Order] target is a known
          order, whose failed read is [store_refused]. *)
  | Refused of Grant.member_refusal

let store_refusal = Grant.Member_indeterminate "standing_order_store"
let store_refused = Refused store_refusal

let order_of_outcome = function
  | Paused { order; _ }
  | Already_paused order
  | Withdrawn order
  | Not_active order ->
      Some order
  | Not_found _ | Unreadable _ | Refused _ -> None

(* A failed store read is never "no order": it comes back as [Error] with its
   cause, which the caller logs at warn. *)
let read f =
  try Ok (f ())
  with exn when Store.is_db_error exn -> Error (Printexc.to_string exn)

let refusal_code : Grant.member_refusal -> string = function
  | Member_unavailable -> "member_unavailable"
  | Member_guest -> "member_guest"
  | Member_not_member -> "member_not_member"
  | Member_not_allowed -> "member_not_allowed"
  | Member_team_mismatch -> "member_team_mismatch"
  | Member_indeterminate m -> "member_indeterminate:" ^ m
  | Member_egress m -> "member_egress:" ^ m

let remembered_outcome db o message =
  match Option.bind message (Journal.completed_outcome db o) with
  | Some ("paused" | "already_paused") -> Already_paused o
  | Some "withdrawn" -> Withdrawn o
  | Some "inactive" -> Not_active o
  | Some "member_unavailable" -> Refused Grant.Member_unavailable
  | Some "member_guest" -> Refused Grant.Member_guest
  | Some "member_not_member" -> Refused Grant.Member_not_member
  | Some "member_not_allowed" -> Refused Grant.Member_not_allowed
  | Some "member_team_mismatch" -> Refused Grant.Member_team_mismatch
  | _ -> store_refused

let order_ref (o : T.order) =
  Some { Access_snapshot.order_id = o.id; fire_id = None }

let ledger ~db ~channel_id ~actor ~event_type metadata =
  try
    ignore
      (Room_activity_ledger.append_now ~db ~room_id:("slack:" ^ channel_id)
         ~event_type ~actor ~metadata:(`Assoc metadata))
  with exn ->
    Logs.warn (fun m ->
        m "standing-order ledger %s not recorded: %s" event_type
          (Printexc.to_string exn))

(* Best effort under its own snapshot: a failure is a warning carrying the
   error code, never an exception. *)
let post ~manager ?order ?team_id ?app_id ?replay ?message
    ?(priority = Slack_read_limiter.Live) ?(capability_fence = false)
    ~unit_label ~channel_id ~thread_ts text =
  let failed code =
    Logs.warn (fun m ->
        m "standing-order %s not posted in %s: %s" unit_label channel_id code);
    Lwt.return_unit
  in
  Lwt.catch
    (fun () ->
      match Session.get_db manager with
      | None -> failed "no_database"
      | Some db -> (
          let current_config () = Session.get_config manager in
          let delivery =
            match (order, message) with
            | Some o, Some m -> Some (o, m)
            | _ -> None
          in
          let reserved =
            match delivery with
            | None -> true
            | Some (o, m) -> Journal.reserve db o m unit_label
          in
          if not reserved then Lwt.return_unit
          else
            let finish status ts =
              Option.iter
                (fun (o, m) -> Journal.finish db o m unit_label status ts)
                delivery
            in
            let send call =
              let text =
                match (replay, delivery) with
                | Some _, Some (o, m) ->
                    text ^ "\n" ^ Journal.marker db o m unit_label
                | _ -> text
              in
              let* result =
                call "chat.postMessage"
                  [
                    ("channel", `String channel_id);
                    ("thread_ts", `String thread_ts);
                    ("text", `String text);
                    ("unfurl_links", `Bool false);
                    ("unfurl_media", `Bool false);
                  ]
              in
              match result with
              | Ok json
                when Slack_standing_order_pages.valid_ts
                       (Slack_huddles.field "ts" json) ->
                  finish "confirmed" (Some (Slack_huddles.field "ts" json));
                  Lwt.return_unit
              | Ok _ ->
                  finish "unknown" None;
                  failed "malformed_receipt"
              | Error error ->
                  finish
                    (if
                       SO.capability_denied (current_config ()) ~channel_id
                       <> None
                     then "local_only"
                     else "unknown")
                    None;
                  failed (SO.error_code error)
            in
            if
              capability_fence
              && SO.capability_denied (current_config ()) ~channel_id <> None
            then (
              finish "local_only" None;
              Lwt.return_unit)
            else
              match replay with
              | Some replay -> send (replay_call replay)
              | None -> (
                  match
                    SO.snapshot ~db ~config:(current_config ()) ~channel_id
                      ~order_ref:(Option.bind order order_ref)
                  with
                  | Error code ->
                      finish "unknown" None;
                      failed code
                  | Ok snapshot ->
                      send
                        (SO.make ~capability_fence ~priority ?team_id ?app_id
                           ~db ~current_config ~snapshot ~channel_id
                           ~audit:
                             (SO.audit_label
                                ~order_id:
                                  (Option.map (fun (o : T.order) -> o.id) order)
                                ~unit_label)
                           ()))))
    (function
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn -> failed ("exception: " ^ Printexc.to_string exn))

let post_receipt ~manager ?order ?team_id ?app_id ?replay ?message ~channel_id
    ~thread_ts text =
  post ~manager ?order ?team_id ?app_id ?replay ?message ~capability_fence:true
    ~priority:Slack_read_limiter.Pause ~unit_label:"pause_receipt" ~channel_id
    ~thread_ts text

let notify_grantor ~manager ?replay ?message (o : T.order) ~user_id event =
  if user_id = o.grantor_user_id then Lwt.return_unit
  else
    post ~manager ~order:o ?replay ?message ~capability_fence:true
      ~priority:Slack_read_limiter.Pause ~unit_label:"pause_notice"
      ~channel_id:o.management_channel_id ~thread_ts:o.management_thread_ts
      (Render.grantor_notice o event)

(* Lookups only within this team, app and channel, so a missing order and
   another channel's order answer the same, with no HTTP. *)
let lookup db ~team_id ~app_id ~channel_id = function
  | `Display n ->
      Option.bind (Store.find_by_display db ~team_id ~channel_id n)
        (fun (o : T.order) -> if o.api_app_id = app_id then Some o else None)
  | `Order id ->
      Option.bind (Store.find db id) (fun (o : T.order) ->
          if
            o.team_id = team_id && o.channel_id = channel_id
            && o.api_app_id = app_id
          then Some o
          else None)

(* The privacy transition, pre-dispatch sweep and replay decision commit once.
   Membership/notification I/O stays outside this synchronous transaction. *)
let apply ~db ~now ?message ?guard ~channel_id ~user_id ~source (o : T.order) =
  let settled (o : T.order) =
    match o.state with T.Paused -> Already_paused o | _ -> Not_active o
  in
  let failed e =
    Logs.warn (fun m -> m "standing-order pause of %s failed: %s" o.id e);
    Lwt.return store_refused
  in
  let deactivate (fresh : T.order) op =
    Result.map
      (function
        | { Store.changed = false; after; _ } -> settled after
        | { Store.after; in_flight; _ } -> (
            match op with
            | Store.Withdraw _ -> Withdrawn after
            | _ -> Paused { order = after; in_flight }))
      (Store.deactivate_in_tx db ~order_id:fresh.id op ~now)
  in
  let result =
    Sql.in_tx db ~on_error:Fun.id (fun () ->
        (* Another event/sweep may have committed while membership awaited.
           Recheck under the write lock, including after a later resume. *)
        match Option.bind message (Journal.completed_outcome db o) with
        | Some _ -> Ok (remembered_outcome db o message)
        | None ->
            let fresh = Option.value (Store.find db o.id) ~default:o in
            let result =
              if
                Option.fold ~none:false
                  ~some:(fun check -> not (check fresh))
                  guard
              then Ok (Refused (Grant.Member_indeterminate "catchup_binding"))
              else
                match fresh.state with
                | T.Pending_confirmation ->
                    deactivate fresh (Store.Withdraw user_id)
                | T.Active -> deactivate fresh (Store.Pause user_id)
                | T.Paused | T.Suspended | T.Cancelled -> Ok (settled fresh)
            in
            Result.map
              (fun outcome ->
                let code =
                  match outcome with
                  | Paused _ -> "paused"
                  | Withdrawn _ -> "withdrawn"
                  | Already_paused _ -> "already_paused"
                  | Not_active _ -> "inactive"
                  | _ -> "refused"
                in
                Option.iter (fun m -> Journal.outcome db o m code) message;
                outcome)
              result)
  in
  match result with
  | Error e -> failed e
  | Ok result ->
      (match result with
      | Paused { order; _ } | Withdrawn order ->
          ledger ~db ~channel_id ~actor:user_id
            ~event_type:
              (match result with
              | Withdrawn _ -> "standing_order_withdrawn"
              | _ -> "standing_order_paused")
            [
              ("order", `String (T.display_name order));
              ("source", `String (source_to_string source));
            ]
      | _ -> ());
      Lwt.return result

let run ~manager ?now ?replay ?message ?guard ~team_id ~app_id ~channel_id
    ~user_id ~target ~source () =
  let now = match now with Some t -> t | None -> Store.now () in
  let n = match target with `Display n -> n | `Order _ -> 0 in
  match Session.get_db manager with
  | None -> Lwt.return (Not_found n)
  | Some db -> (
      ignore (Store.expire_pending db ~now : int);
      match read (fun () -> lookup db ~team_id ~app_id ~channel_id target) with
      | Error e ->
          let name, outcome =
            match target with
            | `Display n -> (Render.so_n n, Unreadable n)
            | `Order id -> (id, store_refused)
          in
          Logs.warn (fun m ->
              m "standing-order pause of %s in %s not run: store unreadable: %s"
                name channel_id e);
          Lwt.return outcome
      | Ok None -> Lwt.return (Not_found n)
      | Ok (Some o) ->
          let status =
            match message with
            | None -> "pending"
            | Some m -> Journal.remember db o m
          in
          if status = "complete" then
            Lwt.return (remembered_outcome db o message)
          else
            Lwt.catch
              (fun () ->
                let current_config () = Session.get_config manager in
                let config = current_config () in
                let call =
                  match replay with
                  | Some r -> (
                      match Access_snapshot.get_by_id ~db r.snapshot_id with
                      | Some snap
                        when r.check ()
                             && snap.work_type = Access_snapshot.Standing_order
                             && snap.session_key
                                = Some (SO.access_key ~channel_id)
                             && snap.standing_order = order_ref o ->
                          Ok (replay_call r)
                      | _ -> Error "replay_snapshot")
                  | None ->
                      Result.map
                        (fun snapshot ->
                          SO.make ~capability_fence:false
                            ~priority:Slack_read_limiter.Pause ~team_id ~app_id
                            ~db ~current_config ~snapshot ~channel_id
                            ~audit:
                              (SO.audit_label ~order_id:(Some o.id)
                                 ~unit_label:"pause")
                            ())
                        (SO.snapshot ~db ~config ~channel_id
                           ~order_ref:(order_ref o))
                in
                match call with
                | Error _ ->
                    Lwt.return
                      (Refused (Grant.Member_indeterminate "access_snapshot"))
                | Ok call -> (
                    let* checked =
                      Grant.check_member ~call ~config ~team_id ~channel_id
                        ~user_id ~require_allowlist:false
                    in
                    match checked with
                    | Error r ->
                        (match r with
                        | Grant.Member_indeterminate _ | Member_egress _ -> ()
                        | _ ->
                            Option.iter
                              (fun m -> Journal.outcome db o m (refusal_code r))
                              message);
                        ledger ~db ~channel_id ~actor:user_id
                          ~event_type:"standing_order_pause_refused"
                          [
                            ("order", `String (T.display_name o));
                            ("source", `String (source_to_string source));
                            ("reason", `String (refusal_code r));
                          ];
                        Lwt.return (Refused r)
                    | Ok ()
                      when Option.fold ~none:false
                             ~some:(fun r -> not (r.check ()))
                             replay ->
                        Lwt.return
                          (Refused (Grant.Member_indeterminate "replay_fence"))
                    | Ok () ->
                        let* result =
                          apply ~db ~now ?message ?guard ~channel_id ~user_id
                            ~source o
                        in
                        let* () =
                          match result with
                          | Paused { order; _ } ->
                              notify_grantor ~manager ?replay ?message order
                                ~user_id (Render.Paused_by user_id)
                          | Withdrawn order ->
                              notify_grantor ~manager ?replay ?message order
                                ~user_id (Render.Cancelled_by user_id)
                          | _ -> Lwt.return_unit
                        in
                        Lwt.return result))
              (function
                | Lwt.Canceled -> Lwt.fail Lwt.Canceled
                | exn ->
                    Logs.warn (fun m ->
                        m "standing-order pause of %s failed: %s" o.id
                          (Printexc.to_string exn));
                    Lwt.return (Refused (Grant.Member_indeterminate "internal")))
      )

(* ── Receipts ───────────────────────────────────────────────────────────── *)

let in_flight_note = function
  | None -> " Relays may already be in flight."
  | Some 0 -> ""
  | Some 1 -> " 1 relay was already being sent before the pause."
  | Some k ->
      Printf.sprintf " %d relays were already being sent before the pause." k

let refused_text r =
  "Pause not applied: " ^ Render.member_refusal_text ~action:`Pause r ^ "."

let receipt ~display_no ~by outcome =
  let n = Render.so_n display_no in
  match outcome with
  | Paused { in_flight; _ } ->
      Printf.sprintf
        "Standing order %s is paused (by %s). Huddles that become ready while \
         it is paused will never be relayed, even after a resume.%s"
        n (Render.user_ref by) (in_flight_note in_flight)
  | Already_paused _ -> Printf.sprintf "Standing order %s is already paused." n
  | Withdrawn _ ->
      Printf.sprintf
        "Standing order %s was waiting for confirmation; it has been withdrawn \
         (by %s)."
        n (Render.user_ref by)
  | Not_active { T.state = T.Suspended; _ } ->
      Printf.sprintf
        "Standing order %s is suspended and is not relaying; nothing changed." n
  | Not_active { T.state = T.Cancelled; _ } ->
      Printf.sprintf "Standing order %s was cancelled; nothing to pause." n
  | Not_active { T.state; _ } ->
      Printf.sprintf "Standing order %s %s; nothing changed." n
        (Render.state_phrase state)
  | Not_found k -> Render.not_found k
  | Unreadable _ -> refused_text store_refusal
  | Refused r -> refused_text r

(* ── Ingress hook ───────────────────────────────────────────────────────── *)

let conversation_channel channel =
  String.length channel > 0 && (channel.[0] = 'C' || channel.[0] = 'G')

let channel_has_orders manager (route : Slack_thread_route.t) =
  match Session.get_db manager with
  | None -> Ok false
  | Some db ->
      read (fun () ->
          Store.channel_has_orders db ~team_id:route.team ~api_app_id:route.app
            ~channel_id:route.channel)

(* Decision 20: clawq speaks only where an order row proves a grant evaluation
   admitted the channel. An unreadable store proves nothing, so it stays
   silent there too (final review, Important 1), with the cause at warn. *)
let handle_text ~manager ~(route : Slack_thread_route.t) ~user ~ts text =
  match parse_command text with
  | Some n when conversation_channel route.channel ->
      Lwt.catch
        (fun () ->
          let* outcome =
            run ~manager ~team_id:route.team ~app_id:route.app
              ~channel_id:route.channel ~user_id:user ~target:(`Display n)
              ~source:Text
              ~message:{ Journal.ts; thread_ts = route.root; user_id = user }
              ()
          in
          let answer () =
            let+ () =
              post_receipt ~manager
                ~message:{ Journal.ts; thread_ts = route.root; user_id = user }
                ~team_id:route.team ~app_id:route.app
                ?order:(order_of_outcome outcome) ~channel_id:route.channel
                ~thread_ts:route.root
                (receipt ~display_no:n ~by:user outcome)
            in
            true
          in
          match outcome with
          | Unreadable _ -> Lwt.return_true (* [run] logged the cause. *)
          | Not_found _ -> (
              match channel_has_orders manager route with
              | Ok true -> answer ()
              | Ok false ->
                  Logs.info (fun m ->
                      m
                        "Slack pause command for SO-%d at %s in %s: no \
                         standing orders in this channel; not answered"
                        n ts route.channel);
                  Lwt.return_true
              | Error e ->
                  Logs.warn (fun m ->
                      m
                        "Slack pause command for SO-%d at %s in %s: standing \
                         orders unreadable, not answered: %s"
                        n ts route.channel e);
                  Lwt.return_true)
          | _ -> answer ())
        (function
          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
          | exn ->
              Logs.warn (fun m ->
                  m "Slack pause command at %s in %s failed: %s" ts
                    route.channel (Printexc.to_string exn));
              Lwt.return_true)
  | _ -> Lwt.return_false
