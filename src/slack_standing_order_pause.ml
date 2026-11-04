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
  | Paused of { order : T.order; in_flight : int }
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
let post ~manager ?order ~unit_label ~channel_id ~thread_ts text =
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
          match
            SO.snapshot ~db ~config:(current_config ()) ~channel_id
              ~order_ref:(Option.bind order order_ref)
          with
          | Error code -> failed code
          | Ok snapshot -> (
              let call =
                SO.make ~db ~current_config ~snapshot ~channel_id
                  ~audit:
                    (SO.audit_label
                       ~order_id:(Option.map (fun (o : T.order) -> o.id) order)
                       ~unit_label)
                  ()
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
              | Ok _ -> Lwt.return_unit
              | Error e -> failed (SO.error_code e))))
    (function
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn -> failed ("exception: " ^ Printexc.to_string exn))

let post_receipt ~manager ?order ~channel_id ~thread_ts text =
  post ~manager ?order ~unit_label:"pause_receipt" ~channel_id ~thread_ts text

let notify_grantor ~manager (o : T.order) ~user_id event =
  if user_id = o.grantor_user_id then Lwt.return_unit
  else
    post ~manager ~order:o ~unit_label:"pause_notice"
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

(* Steps 4-5 on the row as it is now; [deactivate] re-checks it in its
   transaction, so a concurrent change yields [changed = false]. A failed store
   read or write refuses: the order is known, so it is never "not found". *)
let apply ~manager ~db ~now ~channel_id ~user_id ~source (o : T.order) =
  let settled (o : T.order) =
    Lwt.return
      (match o.state with T.Paused -> Already_paused o | _ -> Not_active o)
  in
  let failed e =
    Logs.warn (fun m -> m "standing-order pause of %s failed: %s" o.id e);
    Lwt.return store_refused
  in
  let deactivate (fresh : T.order) op =
    match
      Result.join
        (read (fun () -> Store.deactivate db ~order_id:fresh.id op ~now))
    with
    | Error e -> failed e
    | Ok { Store.changed = false; after; _ } -> settled after
    | Ok { Store.after; in_flight; _ } ->
        let withdrawn = match op with Store.Withdraw _ -> true | _ -> false in
        ledger ~db ~channel_id ~actor:user_id
          ~event_type:
            (if withdrawn then "standing_order_withdrawn"
             else "standing_order_paused")
          [
            ("order", `String (T.display_name after));
            ("source", `String (source_to_string source));
          ];
        let+ () =
          notify_grantor ~manager after ~user_id
            (if withdrawn then Render.Cancelled_by user_id
             else Render.Paused_by user_id)
        in
        if withdrawn then Withdrawn after
        else Paused { order = after; in_flight }
  in
  match read (fun () -> Store.find db o.id) with
  | Error e -> failed e
  | Ok found -> (
      let fresh = Option.value found ~default:o in
      match fresh.state with
      | T.Pending_confirmation -> deactivate fresh (Store.Withdraw user_id)
      | T.Active -> deactivate fresh (Store.Pause user_id)
      | T.Paused | T.Suspended | T.Cancelled -> settled fresh)

let run ~manager ?now ~team_id ~app_id ~channel_id ~user_id ~target ~source () =
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
          Lwt.catch
            (fun () ->
              let current_config () = Session.get_config manager in
              let config = current_config () in
              match
                SO.snapshot ~db ~config ~channel_id ~order_ref:(order_ref o)
              with
              | Error _ ->
                  Lwt.return
                    (Refused (Grant.Member_indeterminate "access_snapshot"))
              | Ok snapshot -> (
                  let call =
                    SO.make ~db ~current_config ~snapshot ~channel_id
                      ~audit:
                        (SO.audit_label ~order_id:(Some o.id)
                           ~unit_label:"pause")
                      ()
                  in
                  let* checked =
                    Grant.check_member ~call ~config ~team_id ~channel_id
                      ~user_id ~require_allowlist:false
                  in
                  match checked with
                  | Error r ->
                      ledger ~db ~channel_id ~actor:user_id
                        ~event_type:"standing_order_pause_refused"
                        [
                          ("order", `String (T.display_name o));
                          ("source", `String (source_to_string source));
                          ("reason", `String (refusal_code r));
                        ];
                      Lwt.return (Refused r)
                  | Ok () ->
                      apply ~manager ~db ~now ~channel_id ~user_id ~source o))
            (function
              | Lwt.Canceled -> Lwt.fail Lwt.Canceled
              | exn ->
                  Logs.warn (fun m ->
                      m "standing-order pause of %s failed: %s" o.id
                        (Printexc.to_string exn));
                  Lwt.return (Refused (Grant.Member_indeterminate "internal"))))

(* ── Receipts ───────────────────────────────────────────────────────────── *)

let in_flight_note = function
  | 0 -> ""
  | 1 -> " 1 relay was already being sent before the pause."
  | k -> Printf.sprintf " %d relays were already being sent before the pause." k

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
              ~source:Text ()
          in
          let answer () =
            let+ () =
              post_receipt ~manager ?order:(order_of_outcome outcome)
                ~channel_id:route.channel ~thread_ts:route.root
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
