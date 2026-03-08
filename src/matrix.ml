(* Matrix channel integration via Client-Server API v3 *)

let chunk_text ?(max_bytes = 4000) text =
  let len = String.length text in
  if len <= max_bytes then [ text ]
  else
    let rec go off acc =
      if off >= len then List.rev acc
      else
        let remaining = len - off in
        if remaining <= max_bytes then
          go len (String.sub text off remaining :: acc)
        else
          let limit = off + max_bytes in
          let break_at =
            let rec find i =
              if i <= off then limit
              else if text.[i] = '\n' then i + 1
              else find (i - 1)
            in
            find (limit - 1)
          in
          let chunk_len = break_at - off in
          go break_at (String.sub text off chunk_len :: acc)
    in
    go 0 []

let txn_counter = ref 0

let make_txn_id ~room_id =
  incr txn_counter;
  let ts = string_of_float (Unix.gettimeofday ()) in
  let raw = room_id ^ ts ^ string_of_int !txn_counter in
  let hash = Digest.to_hex (Digest.string raw) in
  String.sub hash 0 16

let auth_header ~(cfg : Runtime_config.matrix_config) =
  [ ("Authorization", "Bearer " ^ cfg.access_token) ]

let send_message ~(cfg : Runtime_config.matrix_config) ~room_id ~text =
  let open Lwt.Syntax in
  let chunks = chunk_text text in
  Lwt_list.iter_s
    (fun chunk ->
      let txn_id = make_txn_id ~room_id in
      let uri =
        Printf.sprintf "%s/_matrix/client/v3/rooms/%s/send/m.room.message/%s"
          cfg.homeserver_url (Uri.pct_encode room_id) txn_id
      in
      let body =
        `Assoc [ ("msgtype", `String "m.text"); ("body", `String chunk) ]
        |> Yojson.Safe.to_string
      in
      let headers = auth_header ~cfg in
      let* _status, _body = Http_client.put_json ~uri ~headers ~body in
      Lwt.return_unit)
    chunks

(* Sync token persistence *)
let sync_token_path ~(cfg : Runtime_config.matrix_config) =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let safe_id =
    String.concat "_"
      (String.split_on_char ':'
         (String.concat "_" (String.split_on_char '/' cfg.user_id)))
  in
  Filename.concat
    (Filename.concat home ".clawq")
    ("matrix_sync_" ^ safe_id ^ ".json")

let load_sync_token ~cfg =
  let path = sync_token_path ~cfg in
  try
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let json = Yojson.Safe.from_string content in
    let open Yojson.Safe.Util in
    Some (json |> member "since" |> to_string)
  with _ -> None

let save_sync_token ~cfg ~token =
  let path = sync_token_path ~cfg in
  try
    let json = `Assoc [ ("since", `String token) ] in
    let oc = open_out path in
    output_string oc (Yojson.Safe.to_string json);
    close_out oc
  with _ -> ()

let is_room_allowed ~(cfg : Runtime_config.matrix_config) ~room_id =
  match cfg.allow_rooms with [] -> true | rooms -> List.mem room_id rooms

let is_user_allowed ~(cfg : Runtime_config.matrix_config) ~user_id =
  match cfg.allow_users with [] -> true | users -> List.mem user_id users

(* Parse room timeline events from a sync response *)
let parse_sync_events json ~(cfg : Runtime_config.matrix_config) =
  let open Yojson.Safe.Util in
  try
    let rooms = json |> member "rooms" |> member "join" in
    let room_assoc = try to_assoc rooms with _ -> [] in
    List.concat_map
      (fun (room_id, room_data) ->
        if not (is_room_allowed ~cfg ~room_id) then []
        else
          let events =
            try room_data |> member "timeline" |> member "events" |> to_list
            with _ -> []
          in
          List.filter_map
            (fun event ->
              try
                let typ = event |> member "type" |> to_string in
                if typ <> "m.room.message" then None
                else
                  let sender = event |> member "sender" |> to_string in
                  (* Skip own messages *)
                  if sender = cfg.user_id then None
                  else if not (is_user_allowed ~cfg ~user_id:sender) then None
                  else
                    let content = event |> member "content" in
                    let msgtype =
                      try content |> member "msgtype" |> to_string
                      with _ -> ""
                    in
                    if msgtype <> "m.text" then None
                    else
                      let body =
                        try content |> member "body" |> to_string with _ -> ""
                      in
                      if body = "" then None else Some (room_id, sender, body)
              with _ -> None)
            events)
      room_assoc
  with _ -> []

let start ~(config : Runtime_config.t) ~(session_manager : Session.t) =
  match config.channels.matrix with
  | None ->
      Logs.info (fun m -> m "Matrix: no config found, skipping");
      Lwt.return_unit
  | Some cfg ->
      if cfg.homeserver_url = "" || cfg.access_token = "" then begin
        Logs.warn (fun m ->
            m "Matrix: homeserver_url or access_token is empty, skipping");
        Lwt.return_unit
      end
      else begin
        Logs.info (fun m ->
            m "Matrix: starting sync loop for user %s on %s" cfg.user_id
              cfg.homeserver_url);
        let open Lwt.Syntax in
        let since = ref (load_sync_token ~cfg) in
        let backoff = ref 1.0 in
        let rec loop () =
          let sync_uri =
            let base =
              cfg.homeserver_url ^ "/_matrix/client/v3/sync?timeout=30000"
            in
            match !since with
            | None -> base
            | Some token -> base ^ "&since=" ^ Uri.pct_encode token
          in
          let result =
            Lwt.catch
              (fun () ->
                let* status, body =
                  Http_client.get ~uri:sync_uri ~headers:(auth_header ~cfg)
                in
                if status >= 200 && status < 300 then begin
                  let json =
                    try Yojson.Safe.from_string body with _ -> `Null
                  in
                  (* Update since token *)
                  (try
                     let open Yojson.Safe.Util in
                     let next = json |> member "next_batch" |> to_string in
                     since := Some next;
                     save_sync_token ~cfg ~token:next
                   with _ -> ());
                  let events = parse_sync_events json ~cfg in
                  let* () =
                    Lwt_list.iter_s
                      (fun (room_id, sender, text) ->
                        Logs.info (fun m ->
                            m "Matrix: message from %s in %s" sender room_id);
                        let key = "matrix:" ^ room_id ^ ":" ^ sender in
                        let* result =
                          Session.with_registered_notifier session_manager ~key
                            ~notify:(fun text ->
                              send_message ~cfg ~room_id ~text)
                            (fun () ->
                              Lwt.catch
                                (fun () ->
                                  let* response =
                                    Session.turn session_manager ~key
                                      ~message:text ~channel_name:room_id
                                      ~channel_type:"group" ~sender_id:sender ()
                                  in
                                  Lwt.return (Ok response))
                                (fun exn ->
                                  Lwt.return (Error (Printexc.to_string exn))))
                        in
                        match result with
                        | Ok response
                          when Session.is_queued_message_response response ->
                            Lwt.return_unit
                        | Ok response ->
                            send_message ~cfg ~room_id ~text:response
                        | Error err ->
                            Logs.err (fun m ->
                                m "Matrix: agent error in %s from %s: %s"
                                  room_id sender err);
                            send_message ~cfg ~room_id
                              ~text:
                                (Printf.sprintf
                                   "Sorry, an error occurred processing your \
                                    message: %s"
                                   err))
                      events
                  in
                  backoff := 1.0;
                  Lwt.return `Ok
                end
                else begin
                  Logs.warn (fun m -> m "Matrix: sync returned HTTP %d" status);
                  Lwt.return `Error
                end)
              (fun exn ->
                Logs.err (fun m ->
                    m "Matrix: sync error: %s" (Printexc.to_string exn));
                Lwt.return `Error)
          in
          let* outcome = result in
          (match outcome with
          | `Error -> backoff := Float.min (!backoff *. 2.0) 120.0
          | `Ok -> ());
          let* () = Lwt_unix.sleep !backoff in
          loop ()
        in
        loop ()
      end
