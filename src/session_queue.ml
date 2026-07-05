open Session_types

let starts_with ~prefix text =
  let prefix_len = String.length prefix in
  String.length text >= prefix_len && String.sub text 0 prefix_len = prefix

let queued_message_has_user_origin (msg : queued_message) =
  match
    ( msg.sender_id,
      msg.sender_name,
      msg.user_group,
      msg.message_id,
      msg.content_parts,
      msg.attachments )
  with
  | None, None, None, None, [], [] -> false
  | _ -> true

let queued_message_is_lifecycle (msg : queued_message) =
  (not (queued_message_has_user_origin msg))
  &&
  let text = String.trim msg.message in
  starts_with ~prefix:"[bg #" text
  || starts_with ~prefix:"[automatic restart-resume]" text
  || starts_with ~prefix:"Automatic restart-resume:" text
  || starts_with ~prefix:"Autonomous session check-in:" text
  || starts_with ~prefix:"[Automated Keepalive Check-In]" text

let queued_message_is_steering (msg : queued_message) =
  (not msg.deferred_followup) && not (queued_message_is_lifecycle msg)

let delete_queued_message_row mgr (msg : queued_message) =
  match (msg.inbound_queue_id, mgr.db) with
  | Some qid, Some db -> ignore (Memory.queue_delete ~db ~queue_id:qid)
  | _ -> ()

let queued_message_payload_json (msg : queued_message) =
  let fields =
    [
      ("message", `String msg.message);
      ("bang", `Bool msg.bang);
      ("deferred_followup", `Bool msg.deferred_followup);
    ]
  in
  let fields =
    match msg.channel_id with
    | Some cid -> ("channel_id", `String cid) :: fields
    | None -> fields
  in
  let fields =
    match msg.user_group with
    | Some ug -> ("user_group", `String ug) :: fields
    | None -> fields
  in
  Yojson.Safe.to_string (`Assoc fields)

let persist_queued_message mgr ~key ~source (msg : queued_message) =
  match mgr.db with
  | Some db -> (
      try
        Some
          (Memory.queue_enqueue ~db ~session_key:key ~source
             ~payload_json:(queued_message_payload_json msg))
      with exn ->
        Logs.warn (fun m ->
            m "[%s] Failed to persist queued message to SQLite: %s" key
              (Printexc.to_string exn));
        None)
  | None -> None

let update_queued_message_row mgr (msg : queued_message) =
  match (msg.inbound_queue_id, mgr.db) with
  | Some qid, Some db ->
      ignore
        (Memory.queue_update_payload ~db ~queue_id:qid
           ~payload_json:(queued_message_payload_json msg))
  | _ -> ()

let append_followup_text existing addition =
  match String.trim existing with
  | "" -> addition
  | _ -> existing ^ "\n\n" ^ addition

let replace_last_deferred_followup msgs addition =
  let last_idx = ref None in
  List.iteri
    (fun idx (msg : queued_message) ->
      if msg.deferred_followup then last_idx := Some idx)
    msgs;
  match !last_idx with
  | None -> None
  | Some target_idx ->
      let updated_msg = ref None in
      let updated =
        List.mapi
          (fun idx (msg : queued_message) ->
            if idx = target_idx then (
              let msg =
                {
                  msg with
                  message = append_followup_text msg.message addition.message;
                }
              in
              updated_msg := Some msg;
              msg)
            else msg)
          msgs
      in
      Option.map (fun msg -> (updated, msg)) !updated_msg

let enqueue_followup_locked mgr ~key queued_message existing =
  let msg = { queued_message with bang = false; deferred_followup = true } in
  let msg =
    {
      msg with
      inbound_queue_id = persist_queued_message mgr ~key ~source:"live" msg;
    }
  in
  Hashtbl.replace mgr.queued_messages key (existing @ [ msg ]);
  Logs.info (fun m ->
      m "[%s] Queued deferred follow-up (queue depth: %d)" key
        (List.length existing + 1));
  `Queued

let enqueue_followup_if_busy mgr ~key queued_message =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      match Hashtbl.find_opt mgr.sessions key with
      | Some (_, mutex, _) when Lwt_mutex.is_locked mutex ->
          let existing =
            match Hashtbl.find_opt mgr.queued_messages key with
            | Some msgs -> msgs
            | None -> []
          in
          Lwt.return (enqueue_followup_locked mgr ~key queued_message existing)
      | _ -> Lwt.return `Idle)

let append_followup_if_busy mgr ~key queued_message =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      match Hashtbl.find_opt mgr.sessions key with
      | Some (_, mutex, _) when Lwt_mutex.is_locked mutex -> (
          let existing =
            match Hashtbl.find_opt mgr.queued_messages key with
            | Some msgs -> msgs
            | None -> []
          in
          match replace_last_deferred_followup existing queued_message with
          | Some (updated, updated_msg) ->
              Hashtbl.replace mgr.queued_messages key updated;
              update_queued_message_row mgr updated_msg;
              Lwt.return `Appended
          | None ->
              Lwt.return
                (enqueue_followup_locked mgr ~key queued_message existing))
      | _ -> Lwt.return `Idle)

let take_next_queued_message mgr ~key =
  match Hashtbl.find_opt mgr.queued_messages key with
  | Some (msg :: rest) ->
      if rest = [] then Hashtbl.remove mgr.queued_messages key
      else Hashtbl.replace mgr.queued_messages key rest;
      Some msg
  | _ -> None

let take_all_queued_messages mgr ~key =
  match Hashtbl.find_opt mgr.queued_messages key with
  | Some msgs ->
      Hashtbl.remove mgr.queued_messages key;
      (match mgr.db with
      | Some db ->
          List.iter
            (fun (msg : queued_message) ->
              match msg.inbound_queue_id with
              | Some qid -> ignore (Memory.queue_delete ~db ~queue_id:qid)
              | None -> ())
            msgs
      | None -> ());
      msgs
  | None -> []

let has_queued_steering_message mgr ~key =
  Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
    ~label:"sessions_lock" mgr.sessions_lock (fun () ->
      let msgs =
        match Hashtbl.find_opt mgr.queued_messages key with
        | Some msgs -> msgs
        | None -> []
      in
      Lwt.return (List.exists queued_message_is_steering msgs))

let take_next_queued_message_for_drain mgr ~key =
  match Hashtbl.find_opt mgr.queued_messages key with
  | Some msgs -> (
      let rec select_steering prefix = function
        | [] -> None
        | msg :: rest when not msg.deferred_followup ->
            Some (msg, List.rev_append prefix rest)
        | msg :: rest -> select_steering (msg :: prefix) rest
      in
      let selected =
        match select_steering [] msgs with
        | Some item -> Some item
        | None -> (
            match msgs with msg :: rest -> Some (msg, rest) | [] -> None)
      in
      match selected with
      | Some (msg, rest) ->
          if rest = [] then Hashtbl.remove mgr.queued_messages key
          else Hashtbl.replace mgr.queued_messages key rest;
          Some msg
      | None -> None)
  | None -> None

let queued_message_prompt message =
  "A new message arrived while you were working. Treat it as steering "
  ^ "information or a side-question — incorporate it without interrupting "
  ^ "your current task unless it explicitly asks you to stop or change "
  ^ "course.\n\nInjected message:\n" ^ message
