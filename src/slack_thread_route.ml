type t = { team : string; app : string; channel : string; root : string }
(** A reversible, canonical session key is also the durable Slack delivery
    route. The authorization room remains the real channel ID in session_state.
*)

let valid r =
  Slack_huddles.valid_id "T" r.team
  && Slack_huddles.valid_id "A" r.app
  && Slack_huddles.valid_id "CGD" r.channel
  && Slack_huddles.valid_ts r.root

let key r =
  Session.sanitize_session_key
    (String.concat ":" [ "slack"; r.channel; "thread"; r.team; r.app; r.root ])

let of_key key =
  Option.map
    (fun (r : Room_session.slack_thread) ->
      { team = r.team; app = r.app; channel = r.channel; root = r.root })
    (Room_session.slack_thread_of_key key)

let target ?session_key channel_id =
  match Option.bind session_key of_key with
  | Some r when r.channel = channel_id || of_key channel_id = Some r -> key r
  | _ -> channel_id

let send ~bot_token ~channel_id ~text =
  match of_key channel_id with
  | Some r when Slack_api.blank_text text ->
      Slack_api.log_blank ~channel_id:r.channel ~what:"thread message";
      Lwt.return_unit
  | Some r ->
      let open Lwt.Syntax in
      let* status, body =
        Slack_transport.post_json ~uri:"https://slack.com/api/chat.postMessage"
          ~headers:[ ("Authorization", "Bearer " ^ bot_token) ]
          ~body:
            (Yojson.Safe.to_string
               (`Assoc
                  [
                    ("channel", `String r.channel);
                    ("thread_ts", `String r.root);
                    ("text", `String text);
                  ]))
      in
      let json = Yojson.Safe.from_string body in
      if status = 200 && Slack_huddles.flag "ok" json then Lwt.return_unit
      else
        Lwt.fail_with
          ("Slack thread delivery failed: " ^ Slack_huddles.field "error" json)
  | None -> Slack_api.send_message ~bot_token ~channel_id ~text

let owner_schema db =
  match
    Sqlite3.exec db
      "CREATE TABLE IF NOT EXISTS slack_thread_owners (session_key TEXT \
       PRIMARY KEY, user_id TEXT NOT NULL)"
  with
  | Sqlite3.Rc.OK -> ()
  | rc -> failwith ("Slack thread owner schema: " ^ Sqlite3.Rc.to_string rc)

let save_owner manager route user =
  match Session.get_db manager with
  | None -> ()
  | Some db ->
      owner_schema db;
      let stmt =
        Sqlite3.prepare db
          "INSERT INTO slack_thread_owners(session_key,user_id) VALUES (?,?) \
           ON CONFLICT(session_key) DO UPDATE SET user_id=excluded.user_id"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (key route)));
          ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT user));
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> ()
          | rc -> failwith (Sqlite3.Rc.to_string rc))

let load_owner manager session_key =
  match Session.get_db manager with
  | None -> None
  | Some db ->
      owner_schema db;
      let stmt =
        Sqlite3.prepare db
          "SELECT user_id FROM slack_thread_owners WHERE session_key=?"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> (
              match Sqlite3.column stmt 0 with
              | Sqlite3.Data.TEXT user when Slack_huddles.valid_id "UW" user ->
                  Some user
              | _ -> None)
          | _ -> None)
