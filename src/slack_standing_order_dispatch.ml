open Lwt.Syntax
(** Upload-v2 dispatch. Only the synchronous complete hook debits capacity; once
    invoked, completion evidence is recorded even after pause or drain. *)

module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Relay = Slack_standing_order_relay
module Upload = Slack_standing_order_upload
module P = Slack_standing_order_provenance
module Ctx = Slack_standing_order_ctx
module Sql = Slack_standing_order_sql

type wait = Ctx.wait

let default_wait = Ctx.default_wait

exception Stop

let current db fire_id =
  match Store.runtime db ~fire_id with Some r -> r | None -> raise Stop

let save ~ctx ~db ~lease ~before ~after =
  match Store.cas db ~lease ~before ~after ~now:(ctx.Ctx.now ()) with
  | Ok true -> ()
  | _ -> raise Stop

let update_attempt (r : Store.fire_runtime) (a : Store.relay_attempt) =
  {
    r with
    attempts =
      a
      :: List.filter
           (fun (old : Store.relay_attempt) -> old.attempt_no <> a.attempt_no)
           r.attempts;
  }

let attempt (r : Store.fire_runtime) =
  Option.value
    (Store.attempt r r.current_attempt)
    ~default:(Store.empty_attempt r.current_attempt)

let accepting ctx = if not (ctx.Ctx.accepting ()) then raise Stop

let limit ~ctx ~channel_id () =
  accepting ctx;
  let* () = ctx.Ctx.limit ~channel_id in
  accepting ctx;
  Lwt.return_unit

let set_state ~ctx ~db ~lease ~fire_id state reason next_attempt_at =
  let before = current db fire_id in
  save ~ctx ~db ~lease ~before
    ~after:
      { before with fire = { before.fire with state; reason }; next_attempt_at }

let dispatch_refused ~ctx ~db ~lease ~fire_id () =
  let r = current db fire_id in
  if r.fire.state = T.Uploading then
    set_state ~ctx ~db ~lease ~fire_id T.Skipped_inactive
      (Some "dispatch_refused") None;
  raise Stop

let source_matches (r : Store.fire_runtime) (payload : P.verified) =
  String.trim payload.notes_md <> ""
  && r.canvas_id = Some payload.canvas_id
  && r.root_ts = Some payload.root_ts
  && r.notes_ready_ts = Some payload.notes_ready_ts
  && r.transcript_id
     = Option.map (fun (t : P.transcript) -> t.file_id) payload.transcript
  && Option.fold ~none:true
       ~some:(fun (t : P.transcript) -> String.trim t.markdown <> "")
       payload.transcript
  && r.fire.room_id = payload.room_id
  && r.fire.date_start = string_of_int payload.date_start

let refuse ~ctx ~db ~lease ~fire_id (f : Grant.failure) =
  let r = current db fire_id in
  let reason = Grant.reason_code f.reason in
  match f.cls with
  | Grant.Hard ->
      ignore
        (Store.deny_and_suspend_if_epoch db ~lease ~before:r ~reason
           ~now:(ctx.Ctx.now ()))
  | Inactive ->
      set_state ~ctx ~db ~lease ~fire_id T.Skipped_inactive (Some reason) None
  | Capped ->
      set_state ~ctx ~db ~lease ~fire_id T.Skipped_cap (Some reason) None
  | Transient ->
      set_state ~ctx ~db ~lease ~fire_id r.fire.state (Some reason)
        (Some (ctx.Ctx.now () +. 120.))

let evaluate ~ctx ~db ~manager ?http ~lease (r : Store.fire_runtime)
    (order : T.order) =
  accepting ctx;
  let* result =
    Grant.evaluate ~db
      ~current_config:(fun () -> Session.get_config manager)
      ?http
      ~limit:(limit ~ctx ~channel_id:order.channel_id)
      ~now:(ctx.Ctx.now ())
      ~stage:(Grant.Fire { fire_epoch = r.fire.epoch; fire_id = r.fire.id })
      ~unit_label:"dispatch"
      (Grant.subject_of_order order)
  in
  accepting ctx;
  match result with
  | Ok v -> Lwt.return v
  | Error f ->
      refuse ~ctx ~db ~lease ~fire_id:r.fire.id f;
      raise Stop

(* This hook is synchronous all the way through the DB transaction. No Lwt
   yield may separate its reservation from the transport invocation. *)
let fence ~ctx ~db ~manager ~lease ~(v : Grant.validated)
    ~(prepared : Store.fire_runtime) ~reserve () =
  try
    accepting ctx;
    if Session.get_config manager != v.config then raise Stop;
    let r = current db prepared.fire.id in
    if
      (not (Store.valid_lease lease r (ctx.Ctx.now ())))
      || r.current_attempt <> prepared.current_attempt
      || r.canvas_id <> prepared.canvas_id
      || r.transcript_id <> prepared.transcript_id
      || r.notes_ready_ts <> prepared.notes_ready_ts
      || r.nonce <> prepared.nonce
      || r.attempts <> prepared.attempts
      || r.fire.state <> T.Uploading
      || r.fire.cancel_requested || r.fire.repost_forbidden
    then raise Stop;
    let order = Option.get v.order in
    (match
       Grant.stage_rows db ~now:(ctx.Ctx.now ())
         ~stage:(Grant.Fire { fire_epoch = r.fire.epoch; fire_id = r.fire.id })
         (Grant.subject_of_order order)
     with
    | Ok _ -> ()
    | Error _ -> raise Stop);
    (if reserve then
       match
         Store.reserve_dispatch db ~fire_id:r.fire.id ~lease
           ~repost:(r.current_attempt = 1) ~now:(ctx.Ctx.now ())
       with
       | Ok true -> ()
       | _ -> raise Stop);
    Ok ()
  with _ -> Error SO.Dispatch_refused

let scan ~ctx ~(call : SO.call) ~(order : T.order) ~(r : Store.fire_runtime)
    ~nonce ~oldest =
  let a = attempt r in
  let exclude_relay_ts =
    List.filter_map
      (fun (old : Store.relay_attempt) ->
        if old.attempt_no <> a.attempt_no then old.relay_ts else None)
      r.attempts
  in
  let rec page remaining seen cursor found =
    let* result =
      call "conversations.history"
        ([
           ("channel", `String order.channel_id);
           ("oldest", `String oldest);
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
                  | Some ts -> (ts, Relay.own_files m) :: acc
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

let oldest db (r : Store.fire_runtime) =
  match
    Sql.query db "SELECT strftime('%s',?)"
      [ Sql.text r.fire.created_at ]
      (fun s -> Sql_util.opt_text_column s 0)
  with
  | [ Some ts ] when Grant.canonical_sql_datetime r.fire.created_at -> ts
  | _ -> raise Stop

let record_posted ~ctx ~db ~lease ~fire_id ~ts ~ids =
  let before = current db fire_id in
  let a =
    { (attempt before) with relay_ts = Some ts; uploaded_file_ids = ids }
  in
  let after = update_attempt before a in
  save ~ctx ~db ~lease ~before
    ~after:
      {
        after with
        fire =
          {
            after.fire with
            state = T.Posted;
            relay_ts = Some ts;
            reason = None;
          };
        next_attempt_at = None;
      }

let deliver ~ctx ~db ~manager ?http ?upload_http ~lease
    (initial : Store.fire_runtime) (payload : P.verified) =
  let fire_id = initial.fire.id in
  let fail reason =
    set_state ~ctx ~db ~lease ~fire_id T.Failed (Some reason) None;
    raise Stop
  in
  let run () =
    accepting ctx;
    let r = current db fire_id in
    if not (List.mem r.fire.state T.[ Fetching; Uploading ]) then raise Stop;
    if not (source_matches r payload) then fail "source_payload_missing";
    let order =
      match Store.find db r.fire.order_id with
      | Some o -> o
      | None -> raise Stop
    in
    let* v = evaluate ~ctx ~db ~manager ?http ~lease r order in
    let nonce =
      match r.nonce with
      | Some n -> n
      | None ->
          let secret =
            match Relay.secret db with Ok s -> s | Error _ -> raise Stop
          in
          Relay.nonce ~secret ~order_id:order.id ~room_id:r.fire.room_id
            ~date_start:r.fire.date_start
    in
    let prepared = { r with nonce = Some nonce } in
    save ~ctx ~db ~lease ~before:r ~after:prepared;
    let* existing =
      scan ~ctx ~call:v.call ~order ~r:prepared ~nonce ~oldest:(oldest db r)
    in
    match existing with
    | Error () ->
        set_state ~ctx ~db ~lease ~fire_id r.fire.state
          (Some "relay_scan_ambiguous")
          (Some (ctx.Ctx.now () +. 120.));
        Lwt.return_unit
    | Ok found -> (
        let r = current db fire_id in
        if not (source_matches r payload) then raise Stop;
        let a = attempt r in
        let a =
          match found with
          | Some (_, ids) -> { a with uploaded_file_ids = ids }
          | None when a.upload_started_at <> None -> fail "upload_interrupted"
          | None -> { a with upload_started_at = Some (ctx.Ctx.now ()) }
        in
        let after = update_attempt r a in
        save ~ctx ~db ~lease ~before:r
          ~after:{ after with fire = { after.fire with state = T.Uploading } };
        match found with
        | Some (ts, ids) ->
            let r = current db fire_id in
            let* v = evaluate ~ctx ~db ~manager ?http ~lease r order in
            (match
               fence ~ctx ~db ~manager ~lease ~v ~prepared:r ~reserve:true ()
             with
            | Error _ -> raise Stop
            | Ok () -> ());
            record_posted ~ctx ~db ~lease ~fire_id ~ts ~ids;
            Lwt.return_unit
        | None -> (
            let notes_name, transcript_name = Relay.filenames payload in
            let files =
              (notes_name, payload.notes_md)
              ::
              (match (payload.transcript, transcript_name) with
              | Some t, Some filename -> [ (filename, t.markdown) ]
              | _ -> [])
            in
            let rec upload acc = function
              | [] -> Lwt.return (List.rev acc)
              | (filename, body) :: rest ->
                  let r = current db fire_id in
                  let* v = evaluate ~ctx ~db ~manager ?http ~lease r order in
                  let call =
                    SO.make ~db
                      ~current_config:(fun () -> Session.get_config manager)
                      ~snapshot:v.snapshot ~channel_id:order.channel_id
                      ~audit:"standing_order:upload_url" ?http
                      ~limit:(limit ~ctx ~channel_id:order.channel_id)
                      ~before_send:
                        (fence ~ctx ~db ~manager ~lease ~v ~prepared:r
                           ~reserve:false)
                      ()
                  in
                  let* allocated =
                    call "files.getUploadURLExternal"
                      [
                        ("filename", `String filename);
                        ("length", `Int (String.length body));
                      ]
                  in
                  accepting ctx;
                  let id, uri =
                    match allocated with
                    | Ok json ->
                        (P.field "file_id" json, P.field "upload_url" json)
                    | Error SO.Dispatch_refused ->
                        dispatch_refused ~ctx ~db ~lease ~fire_id ()
                    | Error _ -> fail "upload_allocation_failed"
                  in
                  if
                    (not (Slack_config_types.valid_id "F" id))
                    || (not (Upload.pinned uri))
                    || List.mem id acc
                  then fail "upload_allocation_invalid";
                  let r = current db fire_id in
                  if not (source_matches r payload) then raise Stop;
                  let a = attempt r in
                  let after =
                    update_attempt r
                      {
                        a with
                        uploaded_file_ids = a.uploaded_file_ids @ [ id ];
                      }
                  in
                  save ~ctx ~db ~lease ~before:r ~after;
                  let r = current db fire_id in
                  let* v = evaluate ~ctx ~db ~manager ?http ~lease r order in
                  let* uploaded =
                    Upload.post ~db
                      ~current_config:(fun () -> Session.get_config manager)
                      ~snapshot:v.snapshot ~channel_id:order.channel_id
                      ~audit:"standing_order:upload" ?http:upload_http
                      ~limit:(limit ~ctx ~channel_id:order.channel_id)
                      ~before_send:
                        (fence ~ctx ~db ~manager ~lease ~v ~prepared:r
                           ~reserve:false)
                      ~uri ~body ()
                  in
                  accepting ctx;
                  (match uploaded with
                  | Ok () -> ()
                  | Error SO.Dispatch_refused ->
                      dispatch_refused ~ctx ~db ~lease ~fire_id ()
                  | Error _ -> fail "upload_failed");
                  upload (id :: acc) rest
            in
            let* ids = upload [] files in
            let r = current db fire_id in
            let* v = evaluate ~ctx ~db ~manager ?http ~lease r order in
            let reserved = ref false in
            let before_send () =
              let result =
                fence ~ctx ~db ~manager ~lease ~v ~prepared:r ~reserve:true ()
              in
              (match result with Ok () -> reserved := true | Error _ -> ());
              result
            in
            let call =
              SO.make ~db
                ~current_config:(fun () -> Session.get_config manager)
                ~snapshot:v.snapshot ~channel_id:order.channel_id
                ~audit:"standing_order:complete" ?http
                ~limit:(limit ~ctx ~channel_id:order.channel_id)
                ~before_send ()
            in
            let* completed =
              call "files.completeUploadExternal"
                [
                  ("channel_id", `String order.channel_id);
                  ( "files",
                    `List
                      (List.map (fun id -> `Assoc [ ("id", `String id) ]) ids)
                  );
                  ( "initial_comment",
                    `String
                      (Relay.initial_comment ~order ~verified:payload
                         ~team_url:v.team_url ~nonce) );
                ]
            in
            (* No accepting/epoch fence here: an in-flight send belongs to the
                retained attempt even when pause won while HTTP was pending. *)
            match completed with
            | Error SO.Dispatch_refused ->
                dispatch_refused ~ctx ~db ~lease ~fire_id ()
            | Error _ when not !reserved -> fail "complete_preflight_failed"
            | Ok json
              when Relay.share_ts ~channel_id:order.channel_id json <> None ->
                record_posted ~ctx ~db ~lease ~fire_id
                  ~ts:
                    (Option.get
                       (Relay.share_ts ~channel_id:order.channel_id json))
                  ~ids;
                Lwt.return_unit
            | _ ->
                set_state ~ctx ~db ~lease ~fire_id T.Posting_unknown
                  (Some "complete_unknown")
                  (Some (ctx.Ctx.now ()));
                Lwt.return_unit))
  in
  Lwt.catch run (function
    | Stop -> Lwt.return_unit
    | Lwt.Canceled -> Lwt.fail Lwt.Canceled
    | _ -> Lwt.return_unit)
