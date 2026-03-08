(* iMessage channel via AppleScript + SQLite polling (macOS only) *)

let osascript_path = "/usr/bin/osascript"

let chat_db_path () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat home "Library/Messages/chat.db"

let state_path () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat (Filename.concat home ".clawq") "imessage_state.json"

let is_macos () = Sys.file_exists osascript_path

let is_allowed ~(config : Runtime_config.imessage_config) ~handle_id =
  match config.allow_from with [ "*" ] -> true | ids -> List.mem handle_id ids

let load_last_seen_id () =
  let path = state_path () in
  if not (Sys.file_exists path) then 0
  else
    try
      let json = Yojson.Safe.from_file path in
      let open Yojson.Safe.Util in
      json |> member "last_seen_id" |> to_int
    with _ -> 0

let save_last_seen_id id =
  let path = state_path () in
  let dir = Filename.dirname path in
  (try if not (Sys.file_exists dir) then Sys.mkdir dir 0o755 with _ -> ());
  try
    let json = `Assoc [ ("last_seen_id", `Int id) ] |> Yojson.Safe.to_string in
    let oc = open_out path in
    output_string oc json;
    close_out oc
  with exn ->
    Logs.warn (fun m ->
        m "iMessage: failed to save state: %s" (Printexc.to_string exn))

(* Query new messages from chat.db *)
let query_new_messages ~db ~last_id =
  try
    let stmt =
      Sqlite3.prepare db
        "SELECT m.ROWID, m.text, h.id FROM message m LEFT JOIN handle h ON \
         m.handle_id = h.ROWID WHERE m.ROWID > ? AND m.is_from_me = 0 AND \
         m.text IS NOT NULL ORDER BY m.ROWID ASC"
    in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int last_id)));
    let rows = ref [] in
    (try
       while Sqlite3.step stmt = Sqlite3.Rc.ROW do
         let row_id =
           match Sqlite3.column stmt 0 with
           | Sqlite3.Data.INT i -> Int64.to_int i
           | _ -> 0
         in
         let text =
           match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
         in
         let handle_id =
           match Sqlite3.column stmt 2 with Sqlite3.Data.TEXT s -> s | _ -> ""
         in
         if text <> "" then rows := (row_id, handle_id, text) :: !rows
       done
     with _ -> ());
    ignore (Sqlite3.finalize stmt);
    List.rev !rows
  with exn ->
    Logs.warn (fun m ->
        m "iMessage: SQLite query error: %s" (Printexc.to_string exn));
    []

(* Escape a string for use inside AppleScript double quotes *)
let escape_applescript s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | _ -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* Send iMessage via AppleScript *)
let send_imessage ~recipient ~text =
  let open Lwt.Syntax in
  let escaped_text = escape_applescript text in
  let escaped_recipient = escape_applescript recipient in
  let script =
    Printf.sprintf
      "tell application \"Messages\" to send \"%s\" to buddy \"%s\" of service \
       \"iMessage\""
      escaped_text escaped_recipient
  in
  let cmd = (osascript_path, [| osascript_path; "-e"; script |]) in
  Lwt.catch
    (fun () ->
      let proc = Lwt_process.open_process_none cmd in
      let* status = proc#status in
      (match status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED n ->
          Logs.warn (fun m -> m "iMessage: osascript exited with code %d" n)
      | _ -> Logs.warn (fun m -> m "iMessage: osascript terminated abnormally"));
      Lwt.return_unit)
    (fun exn ->
      Logs.err (fun m ->
          m "iMessage: failed to send: %s" (Printexc.to_string exn));
      Lwt.return_unit)

let start ~(config : Runtime_config.t) ~(session_manager : Session.t) =
  match config.channels.imessage with
  | None ->
      Logs.info (fun m -> m "No iMessage config found, skipping");
      Lwt.return_unit
  | Some im_config ->
      if not (is_macos ()) then begin
        Logs.info (fun m ->
            m "iMessage: osascript not found, disabled (not macOS)");
        Lwt.return_unit
      end
      else begin
        let db_path = chat_db_path () in
        if not (Sys.file_exists db_path) then begin
          Logs.warn (fun m ->
              m "iMessage: chat.db not found at %s, skipping" db_path);
          Lwt.return_unit
        end
        else begin
          Logs.info (fun m ->
              m "iMessage channel starting (poll_interval=%.1fs)"
                im_config.poll_interval_s);
          let open Lwt.Syntax in
          let last_id = ref (load_last_seen_id ()) in
          let rec poll_loop () =
            let* () =
              Lwt.catch
                (fun () ->
                  let db = Sqlite3.db_open ~mode:`READONLY db_path in
                  let rows = query_new_messages ~db ~last_id:!last_id in
                  ignore (Sqlite3.db_close db);
                  let* () =
                    Lwt_list.iter_s
                      (fun (row_id, handle_id, text) ->
                        last_id := max !last_id row_id;
                        save_last_seen_id !last_id;
                        if not (is_allowed ~config:im_config ~handle_id) then begin
                          Logs.warn (fun m ->
                              m
                                "iMessage: ignoring message from unauthorized \
                                 handle=%s"
                                handle_id);
                          Lwt.return_unit
                        end
                        else
                          let key = "imessage:" ^ handle_id in
                          let* result =
                            Session.with_registered_notifier session_manager
                              ~key
                              ~notify:(fun text ->
                                send_imessage ~recipient:handle_id ~text)
                              (fun () ->
                                Lwt.catch
                                  (fun () ->
                                    let* response =
                                      Session.turn session_manager ~key
                                        ~message:text ~channel_name:"imessage"
                                        ~channel_type:"dm" ()
                                    in
                                    Lwt.return (Ok response))
                                  (fun exn ->
                                    Lwt.return (Error (Printexc.to_string exn))))
                          in
                          match result with
                          | Ok response ->
                              if Session.is_queued_message_response response
                              then Lwt.return_unit
                              else
                                send_imessage ~recipient:handle_id
                                  ~text:response
                          | Error err ->
                              Logs.err (fun m ->
                                  m "iMessage: agent error for handle=%s: %s"
                                    handle_id err);
                              Lwt.return_unit)
                      rows
                  in
                  Lwt.return_unit)
                (fun exn ->
                  Logs.err (fun m ->
                      m "iMessage: poll error: %s" (Printexc.to_string exn));
                  Lwt.return_unit)
            in
            let* () = Lwt_unix.sleep im_config.poll_interval_s in
            poll_loop ()
          in
          poll_loop ()
        end
      end
