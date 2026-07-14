(** CLI commands for room session record inspection. *)

let admin_env_var = "CLAWQ_ADMIN"

let is_admin_cli () =
  match Sys.getenv_opt admin_env_var with
  | Some v -> v = "1" || v = "true"
  | None -> false

let require_admin () =
  if is_admin_cli () then None
  else
    Some
      "Error: this command requires admin privileges. Set CLAWQ_ADMIN=1 in \
       your environment."

let get_db () = Command_bridge_helpers.get_db ()

let format_session_record_detail (r : Room_session_record.t) =
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add (Printf.sprintf "ID:                  %s" r.id);
  add (Printf.sprintf "Created:             %s" r.created_at);
  (match r.session_key with
  | Some s -> add (Printf.sprintf "Session Key:         %s" s)
  | None -> add "Session Key:         (none)");
  (match r.room_id with
  | Some s -> add (Printf.sprintf "Room ID:             %s" s)
  | None -> add "Room ID:             (none)");
  add (Printf.sprintf "Config Hash:         %s" r.config_hash);
  add (Printf.sprintf "Access Snapshot ID:  %s" r.access_snapshot_id);
  (match r.agent_config with
  | Some ac ->
      add "";
      add "--- Agent Config ---";
      add (Printf.sprintf "Profile ID:      %s" ac.profile_id);
      (match ac.display_name with
      | Some n -> add (Printf.sprintf "Display Name:    %s" n)
      | None -> ());
      add (Printf.sprintf "Model:           %s" ac.model);
      add (Printf.sprintf "Prompt Digest:   %s" ac.system_prompt_digest);
      add (Printf.sprintf "Status:          %s" ac.status);
      add (Printf.sprintf "Max Iterations:  %d" ac.max_tool_iterations);
      if ac.allowed_tools <> [] then
        add
          (Printf.sprintf "Allowed Tools:   %s"
             (String.concat ", " ac.allowed_tools));
      if ac.denied_tools <> [] then
        add
          (Printf.sprintf "Denied Tools:    %s"
             (String.concat ", " ac.denied_tools));
      if ac.access_bundle_ids <> [] then
        add
          (Printf.sprintf "Access Bundles:  %s"
             (String.concat ", " ac.access_bundle_ids));
      if ac.ambient_enabled then begin
        add (Printf.sprintf "Ambient:         enabled");
        add
          (Printf.sprintf "Ambient Hours:   %02d:00-%02d:00"
             ac.ambient_quiet_start ac.ambient_quiet_end);
        add (Printf.sprintf "Ambient Rate:    %d rph" ac.ambient_rate_limit_rph)
      end;
      if ac.low_volume then add (Printf.sprintf "Low volume:      yes")
  | None -> add "Agent Config:        (none)");
  (match r.delivery with
  | Some d -> (
      add "";
      add "--- Delivery State ---";
      add (Printf.sprintf "State:           %s" d.state);
      add (Printf.sprintf "Last Update:     %s" d.last_update);
      (match d.message_id with
      | Some id -> add (Printf.sprintf "Message ID:      %s" id)
      | None -> ());
      match d.error_detail with
      | Some detail -> add (Printf.sprintf "Error:           %s" detail)
      | None -> ())
  | None -> add "Delivery:            (none)");
  (match r.transcript_url with
  | Some url -> add (Printf.sprintf "Transcript URL:  %s" url)
  | None -> add "Transcript URL:      (none)");
  (match r.session_url with
  | Some url -> add (Printf.sprintf "Session URL:     %s" url)
  | None -> add "Session URL:         (none)");
  let cc = r.connector_context in
  if cc <> Room_session_record.empty_connector_context then begin
    add "";
    add "--- Connector Context ---";
    Option.iter
      (fun s -> add (Printf.sprintf "Connector:       %s" s))
      cc.connector;
    Option.iter
      (fun s -> add (Printf.sprintf "Workspace:       %s" s))
      cc.workspace_id;
    Option.iter
      (fun s -> add (Printf.sprintf "Room ID:         %s" s))
      cc.room_id;
    Option.iter
      (fun s -> add (Printf.sprintf "Requester ID:    %s" s))
      cc.requester_id;
    Option.iter
      (fun s -> add (Printf.sprintf "Requester:       %s" s))
      cc.requester_name;
    Option.iter
      (fun s -> add (Printf.sprintf "Source Msg:      %s" s))
      cc.source_message_id;
    Option.iter
      (fun s -> add (Printf.sprintf "Thread ID:       %s" s))
      cc.thread_id;
    Option.iter
      (fun s -> add (Printf.sprintf "Service URL:     %s" s))
      cc.service_url
  end;
  String.concat "\n" (List.rev !lines)

let format_session_record_list (records : Room_session_record.t list) =
  match records with
  | [] -> "No room session records found."
  | _ ->
      let columns =
        Table_format.
          [
            { header = "ID"; align = Left; min_width = 20; flex = false };
            { header = "CREATED"; align = Left; min_width = 20; flex = false };
            { header = "ROOM"; align = Left; min_width = 8; flex = false };
            {
              header = "SNAPSHOT_ID";
              align = Left;
              min_width = 15;
              flex = false;
            };
            { header = "DELIVERY"; align = Left; min_width = 10; flex = false };
          ]
      in
      let rows =
        List.map
          (fun (r : Room_session_record.t) ->
            let room_str = match r.room_id with Some s -> s | None -> "-" in
            let delivery_str =
              match r.delivery with Some d -> d.state | None -> "-"
            in
            [ r.id; r.created_at; room_str; r.access_snapshot_id; delivery_str ])
          records
      in
      Format_adapter.bold Format_adapter.Plain "Room Session Records"
      ^ "\n\n"
      ^ Format_adapter.render_table Format_adapter.Plain ~max_width:120 columns
          rows

(** Extract [--json] from a flag list, returning (is_json, remaining). *)
let extract_json_flag args =
  let rec loop acc = function
    | "--json" :: rest -> (true, List.rev_append acc rest)
    | x :: rest -> loop (x :: acc) rest
    | [] -> (false, List.rev acc)
  in
  loop [] args

let parse_session_record_filters flags =
  let rec loop room_id session_key snapshot_id limit = function
    | [] -> Ok (room_id, session_key, snapshot_id, limit)
    | ("--room-id" | "--room") :: value :: rest ->
        loop (Some value) session_key snapshot_id limit rest
    | ("--session-key" | "--session") :: value :: rest ->
        loop room_id (Some value) snapshot_id limit rest
    | ("--snapshot-id" | "--snapshot") :: value :: rest ->
        loop room_id session_key (Some value) limit rest
    | "--limit" :: value :: rest -> (
        match int_of_string_opt value with
        | Some n when n > 0 -> loop room_id session_key snapshot_id n rest
        | _ -> Error "--limit must be a positive integer")
    | "--json" :: rest -> loop room_id session_key snapshot_id limit rest
    | flag :: _ -> Error (Printf.sprintf "unknown rooms session flag: %s" flag)
  in
  loop None None None 20 flags

let cmd_rooms_session args =
  match require_admin () with
  | Some err -> err
  | None -> (
      let db = get_db () in
      Room_session_record.init_schema db;
      match args with
      | "list" :: flags -> (
          match parse_session_record_filters flags with
          | Error msg -> "Error: " ^ msg
          | Ok (room_id, session_key, snapshot_id, limit) ->
              let records =
                Room_session_record.query ~db ?room_id ?session_key
                  ?access_snapshot_id:snapshot_id ~limit ()
              in
              if List.mem "--json" flags then
                let json_list =
                  `List (List.map Room_session_record.to_json records)
                in
                Yojson.Safe.pretty_to_string json_list
              else format_session_record_list records)
      | "show" :: args -> (
          let is_json, remaining = extract_json_flag args in
          match remaining with
          | [ id ] -> (
              match Room_session_record.get ~db ~id () with
              | None -> Printf.sprintf "Room session record not found: %s" id
              | Some r ->
                  if is_json then
                    Yojson.Safe.pretty_to_string (Room_session_record.to_json r)
                  else format_session_record_detail r)
          | _ ->
              "Error: rooms session show requires a record ID.\n\n\
               Usage: clawq rooms session show <id> [--json]")
      | "get-latest" :: args -> (
          let is_json, remaining = extract_json_flag args in
          match remaining with
          | [ room_id ] -> (
              match Room_session_record.get_latest_for_room ~db ~room_id () with
              | None ->
                  Printf.sprintf "No session record found for room '%s'."
                    room_id
              | Some r ->
                  if is_json then
                    Yojson.Safe.pretty_to_string (Room_session_record.to_json r)
                  else format_session_record_detail r)
          | _ ->
              "Error: rooms session get-latest requires a room_id.\n\n\
               Usage: clawq rooms session get-latest <room_id> [--json]")
      | _ ->
          "Usage: clawq rooms session <list|show|get-latest> [args]\n\n\
           Subcommands:\n\
          \  list [--room-id ID] [--session-key KEY] [--snapshot-id ID] \
           [--limit N] [--json]\n\
          \                              List room session records (admin-only)\n\
          \  show <id> [--json]         Show a room session record by ID \
           (admin-only)\n\
          \  get-latest <room_id> [--json]\n\
          \                              Get the latest session record for a \
           room (admin-only)")
