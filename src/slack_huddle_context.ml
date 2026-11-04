type request = {
  manager : Session.t;
  key : string;
  channel_id : string;
  user_id : string;
  message_id : string;
  received_at : float;
}
(** Slack ingress authority. Neither model arguments nor a session name can
    create this evidence. Queue drains explicitly select the next ingress. *)

type turn = {
  request : request;
  config : Runtime_config.t;
  previous_snapshot : string option;
  is_admin : bool;
  drained : bool;
      (** Started by a queue drain, not by live ingress. Huddle tools ignore it;
          standing-order management refuses it (spec §6.8, §7.8). *)
}

let current : turn option ref Lwt.key = Lwt.new_key ()
let pending : (string * string, request) Hashtbl.t = Hashtbl.create 32

let snapshot_id (r : request) =
  match Hashtbl.find_opt r.manager.sessions r.key with
  | Some (agent, _, _) -> agent.Agent.access_snapshot_id
  | None -> None

let start ?(drained = false) request =
  {
    request;
    config = Session.get_config request.manager;
    previous_snapshot = snapshot_id request;
    is_admin =
      (match Session.get_db request.manager with
      | Some db ->
          Admin.is_admin ~db ~channel:"slack" ~sender_id:request.user_id
      | None -> false);
    drained;
  }

let clear () =
  match Lwt.get current with Some cell -> cell := None | None -> ()

let before_queued ~key message_id =
  let key = Session.sanitize_session_key key in
  clear ();
  match (Lwt.get current, message_id) with
  | Some cell, Some id -> (
      match Hashtbl.find_opt pending (key, id) with
      | Some request when Unix.gettimeofday () -. request.received_at < 3600. ->
          Hashtbl.remove pending (key, id);
          cell := Some (start ~drained:true request)
      | _ -> ())
  | _ -> ()

let with_request ?(drained = false) ~session_manager ~key ~channel_id ~user_id
    ~message_id f =
  let key = Session.sanitize_session_key key in
  let now = Unix.gettimeofday () in
  Hashtbl.filter_map_inplace
    (fun _ r -> if now -. r.received_at < 3600. then Some r else None)
    pending;
  let request =
    {
      manager = session_manager;
      key;
      channel_id;
      user_id;
      message_id;
      received_at = now;
    }
  in
  (* Bound retention of queued evidence. Missing evidence always fails closed. *)
  if Hashtbl.length pending >= 1024 then Hashtbl.clear pending;
  Hashtbl.replace pending (key, message_id) request;
  let cell = ref (Some (start ~drained request)) in
  Lwt.with_value current (Some cell) (fun () ->
      Lwt.finalize
        (fun () -> Tool.with_message_injection_guard (fun () -> cell := None) f)
        (fun () ->
          cell := None;
          (* Queued requests must remain available for the active drainer. *)
          let queued =
            match
              Hashtbl.find_opt session_manager.Session.queued_messages key
            with
            | Some messages ->
                List.exists
                  (fun (m : Session.queued_message) ->
                    m.message_id = Some message_id)
                  messages
            | None -> false
          in
          if not queued then Hashtbl.remove pending (key, message_id);
          Lwt.return_unit))

(* Positive evidence that the current cell is the live ingress turn for exactly
   this message: made by [with_request], not drained, steered or cleared since.
   [before_queued] always clears the cell first, so a drain never passes, even
   when its queued evidence is missing; neither does a replay outside ingress. *)
let live ~key ~channel_id ~user_id ~message_id =
  let key = Session.sanitize_session_key key in
  match Lwt.get current with
  | Some cell -> (
      match !cell with
      | Some t ->
          (not t.drained) && t.request.key = key
          && t.request.channel_id = channel_id
          && t.request.user_id = user_id
          && t.request.message_id = message_id
      | None -> false)
  | None -> false

let resolve ~tool_name (context : Tool.invoke_context) =
  let error =
    Error
      "Slack huddle access requires a verified active Slack turn; send the \
       request in Slack again."
  in
  match Lwt.get current with
  | Some cell -> (
      match (!cell, context.snapshot_id) with
      | Some turn, Some id
        when context.session_key = Some turn.request.key
             && Some id <> turn.previous_snapshot
             && snapshot_id turn.request = Some id -> (
          match Session.get_db turn.request.manager with
          | Some db -> (
              match Access_snapshot.get_by_id ~db id with
              | Some snap
                when snap.session_key = context.session_key
                     && snap.room_id = Some turn.request.channel_id
                     && snap.work_type = Access_snapshot.Room_turn
                     && snap.config_hash
                        = Access_snapshot.config_hash turn.config
                     && (snap.room_policy_decision = "allow"
                        || String.starts_with ~prefix:"warn:"
                             snap.room_policy_decision
                        || String.starts_with ~prefix:"admin_override:"
                             snap.room_policy_decision) -> (
                  match
                    Tool_authz.decide ~equivalence_names:[ tool_name ]
                      ~allowed_tools:snap.allowed_tools
                      ~denied_tools:snap.denied_tools ()
                  with
                  | Tool_authz.Allowed -> Ok (turn, snap)
                  | Tool_authz.Denied reason -> Error reason)
              | _ -> error)
          | None -> error)
      | _ -> error)
  | None -> error
