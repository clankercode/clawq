open Lwt.Syntax
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Relay = Slack_standing_order_relay
module P = Slack_standing_order_provenance
module Ctx = Slack_standing_order_ctx

exception Stop

let accepting ctx = if not (ctx.Ctx.accepting ()) then raise Stop

let attempt (r : Store.fire_runtime) =
  Option.value
    (Store.attempt r r.current_attempt)
    ~default:(Store.empty_attempt r.current_attempt)

let scan ~ctx ~(call : SO.call) ~(order : T.order) ~(r : Store.fire_runtime)
    ~nonce ~oldest =
  let a = attempt r in
  let exclude_relay_ts =
    List.filter_map
      (fun (old : Store.relay_attempt) ->
        if old.attempt_no <> a.attempt_no then old.relay_ts else None)
      r.attempts
  in
  let prior_files =
    List.concat_map
      (fun (old : Store.relay_attempt) ->
        if old.attempt_no <> a.attempt_no then old.uploaded_file_ids else [])
      r.attempts
  in
  let latest = Printf.sprintf "%.6f" (ctx.Ctx.now ()) in
  let rec page remaining seen cursor found =
    let* result =
      call "conversations.history"
        ([
           ("channel", `String order.channel_id);
           ("oldest", `String oldest);
           ("latest", `String latest);
           ("inclusive", `Bool true);
           ("limit", `Int 200);
         ]
        @ if cursor = "" then [] else [ ("cursor", `String cursor) ])
    in
    accepting ctx;
    match result with
    | Error _ -> Lwt.return (Error ())
    | Ok json -> (
        match P.get "messages" json with
        | `List messages when List.length messages <= 200 -> (
            let found =
              List.fold_left
                (fun acc m ->
                  match
                    Relay.adopt ~order ~nonce ~attempt:a ~exclude_relay_ts m
                  with
                  | Some ts ->
                      let ids = Relay.own_files m in
                      if
                        a.attempt_no = 1 && a.uploaded_file_ids = []
                        && List.exists (fun id -> List.mem id prior_files) ids
                      then acc
                      else (ts, ids) :: acc
                  | None -> acc)
                found messages
            in
            let cursor =
              P.get "response_metadata" json |> P.get "next_cursor"
            in
            match (P.get "has_more" json, cursor) with
            | (`Bool false | `Null), `String "" | `Bool false, `Null -> (
                match List.sort_uniq compare found with
                | [] -> Lwt.return (Ok None)
                | [ one ] -> Lwt.return (Ok (Some one))
                | _ -> Lwt.return (Error ()))
            | (`Bool true | `Bool false | `Null), `String next
              when next <> "" && remaining > 1 && not (List.mem next seen) ->
                page (remaining - 1) (next :: seen) next found
            | _ -> Lwt.return (Error ()))
        | _ -> Lwt.return (Error ()))
  in
  page 5 [] "" []

type share_result = Shared of string | Proven_absent | Ambiguous

(* Only an explicit argument rejection is known to precede sharing. Transport
   failures, 5xx and even successful empty completions are not such evidence. *)
let rejected_before_share = function
  | Error (SO.Slack_error "invalid_arguments") -> true
  | _ -> false

let resolve_share ~ctx ?(observe = fun () -> None) ~call ~(order : T.order)
    ~attempt_no (initial : Store.fire_runtime) =
  let deadline = ctx.Ctx.now () +. 120. in
  let fresh () = Option.value (observe ()) ~default:initial in
  let get_attempt r =
    Option.value
      (Store.attempt r attempt_no)
      ~default:(Store.empty_attempt attempt_no)
  in
  let bounded method_ params =
    accepting ctx;
    if ctx.Ctx.now () >= deadline then raise Stop;
    let pending = call method_ params in
    let* result =
      if not (Lwt.is_sleeping pending) then pending
      else
        Lwt.pick
          [
            pending;
            (let* () = ctx.Ctx.wait ~seconds:(deadline -. ctx.Ctx.now ()) in
             Lwt.return (Error SO.Transport));
          ]
    in
    accepting ctx;
    Lwt.return result
  in
  let absent_file id =
    let* result = bounded "files.info" [ ("file", `String id) ] in
    match result with
    | Error _ -> Lwt.return (None, false)
    | Ok json ->
        let file = P.get "file" json in
        if P.field "id" file <> id then Lwt.return (None, false)
        else
          let empty visibility =
            match P.get "shares" file |> P.get visibility with
            | `Assoc fields -> (
                match List.assoc_opt order.channel_id fields with
                | None | Some (`List []) -> true
                | _ -> false)
            | _ -> false
          in
          Lwt.return
            ( Relay.share_ts ~channel_id:order.channel_id file,
              empty "public" && empty "private" )
  in
  let rec poll step =
    let r = fresh () in
    let a = get_attempt r in
    match a.relay_ts with
    | Some ts -> Lwt.return (Shared ts)
    | None when ctx.Ctx.now () >= deadline -> Lwt.return Ambiguous
    | None -> (
        let rec files absent = function
          | [] -> Lwt.return (None, absent)
          | id :: rest -> (
              let* ts, empty = absent_file id in
              match ts with
              | Some _ -> Lwt.return (ts, false)
              | None -> files (absent && empty) rest)
        in
        let* ts, all_absent =
          files (a.uploaded_file_ids <> []) a.uploaded_file_ids
        in
        match ts with
        | Some ts -> Lwt.return (Shared ts)
        | None -> (
            let* history =
              match (r.nonce, a.dispatched_at) with
              | Some nonce, Some started ->
                  scan ~ctx ~call:bounded ~order
                    ~r:{ r with current_attempt = attempt_no }
                    ~nonce
                    ~oldest:(Printf.sprintf "%.6f" started)
              | _ -> Lwt.return (Error ())
            in
            match history with
            | Ok (Some (ts, _)) -> Lwt.return (Shared ts)
            | Ok None when all_absent && a.complete_rejected_before_share ->
                Lwt.return Proven_absent
            | _ -> (
                let r = fresh () in
                match (get_attempt r).relay_ts with
                | Some ts -> Lwt.return (Shared ts)
                | None ->
                    let remaining = deadline -. ctx.Ctx.now () in
                    if remaining <= 0. then Lwt.return Ambiguous
                    else
                      let seconds =
                        min remaining
                          (match step with
                          | 0 -> 1.
                          | 1 -> 2.
                          | 2 -> 5.
                          | _ -> 10.)
                      in
                      let* () = ctx.Ctx.wait ~seconds in
                      accepting ctx;
                      poll (step + 1))))
  in
  Lwt.catch
    (fun () ->
      match (get_attempt initial).relay_ts with
      | Some ts -> Lwt.return (Shared ts)
      | None -> poll 0)
    (function
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled | _ -> Lwt.return Ambiguous)
