(** CLI commands for surfacing recent delivery failures.

    Provides [clawq rooms deliveries] subcommand that queries the room activity
    ledger for recent delivery failure events, enabling operators to diagnose
    Teams delivery issues without reading daemon logs. *)

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

(** Extract the error string from a delivery failure event's metadata. *)
let error_of_event (event : Room_activity_ledger.event) =
  match event.metadata with
  | `Assoc fields -> (
      match List.assoc_opt "error" fields with
      | Some (`String s) -> s
      | _ -> "(unknown)")
  | _ -> "(unknown)"

(** Extract the connector string from event metadata. *)
let connector_of_event (event : Room_activity_ledger.event) =
  match event.metadata with
  | `Assoc fields -> (
      match List.assoc_opt "connector" fields with
      | Some (`String s) -> s
      | _ -> event.actor)
  | _ -> event.actor

(** Extract the task_id from event metadata. *)
let task_id_of_event (event : Room_activity_ledger.event) =
  match event.metadata with
  | `Assoc fields -> (
      match List.assoc_opt "task_id" fields with
      | Some (`Int n) -> Some n
      | Some (`Intlit s) -> int_of_string_opt s
      | _ -> None)
  | _ -> None

(** Format a delivery failure event as a row for table display. *)
let format_failure_row (event : Room_activity_ledger.event) =
  let connector = connector_of_event event in
  let task_str =
    match task_id_of_event event with
    | Some id -> string_of_int id
    | None -> "-"
  in
  let error = error_of_event event in
  let error_short =
    if String.length error > 60 then String.sub error 0 57 ^ "..." else error
  in
  [
    event.room_id;
    connector;
    task_str;
    event.event_type;
    error_short;
    event.timestamp;
  ]

(** Format delivery failure events as a table. *)
let format_failures_table events =
  match events with
  | [] -> "No recent delivery failures found."
  | _ ->
      let columns =
        Table_format.
          [
            { header = "ROOM"; align = Left; min_width = 8; flex = false };
            { header = "CONNECTOR"; align = Left; min_width = 8; flex = false };
            { header = "TASK"; align = Right; min_width = 4; flex = false };
            { header = "EVENT"; align = Left; min_width = 12; flex = false };
            { header = "ERROR"; align = Left; min_width = 15; flex = true };
            { header = "TIMESTAMP"; align = Left; min_width = 20; flex = false };
          ]
      in
      let rows = List.map format_failure_row events in
      Format_adapter.bold Format_adapter.Plain "Recent Delivery Failures"
      ^ "\n\n"
      ^ Format_adapter.render_table Format_adapter.Plain ~max_width:120 columns
          rows

(** Convert a delivery failure event to JSON for [--json] output. *)
let failure_event_to_json (event : Room_activity_ledger.event) =
  `Assoc
    [
      ("id", `Int event.id);
      ("room_id", `String event.room_id);
      ("event_type", `String event.event_type);
      ("timestamp", `String event.timestamp);
      ("actor", `String event.actor);
      ("connector", `String (connector_of_event event));
      ( "task_id",
        match task_id_of_event event with Some id -> `Int id | None -> `Null );
      ("error", `String (error_of_event event));
      ("metadata", event.metadata);
    ]

type deliveries_args = {
  room_id : string option;
  connector : string option;
  from_timestamp : string option;
  limit : int;
  as_json : bool;
}

let default_deliveries_args =
  {
    room_id = None;
    connector = None;
    from_timestamp = None;
    limit = 20;
    as_json = false;
  }

let parse_deliveries_args flags =
  let rec loop args = function
    | [] -> Ok args
    | ("--room-id" | "--room") :: value :: rest ->
        loop { args with room_id = Some value } rest
    | "--connector" :: value :: rest ->
        loop { args with connector = Some value } rest
    | ("--from" | "--since") :: value :: rest ->
        loop { args with from_timestamp = Some value } rest
    | "--limit" :: value :: rest -> (
        match int_of_string_opt value with
        | Some n when n > 0 -> loop { args with limit = n } rest
        | _ -> Error "--limit must be a positive integer")
    | "--json" :: rest -> loop { args with as_json = true } rest
    | flag :: _ -> Error (Printf.sprintf "unknown deliveries flag: %s" flag)
  in
  loop default_deliveries_args flags

(** Main handler for [clawq rooms deliveries]. *)
let cmd_rooms_deliveries args =
  match require_admin () with
  | Some err -> err
  | None -> (
      match parse_deliveries_args args with
      | Error msg -> "Error: " ^ msg
      | Ok parsed ->
          let db = get_db () in
          Room_activity_ledger.init_schema db;
          let results =
            Room_activity_ledger.query_delivery_failures ~db
              ?room_id:parsed.room_id ?actor:parsed.connector
              ?from_timestamp:parsed.from_timestamp ~limit:parsed.limit ()
          in
          if parsed.as_json then
            let json =
              `Assoc
                [
                  ("failures", `List (List.map failure_event_to_json results));
                  ("total", `Int (List.length results));
                  ( "query",
                    `Assoc
                      [
                        ( "room_id",
                          match parsed.room_id with
                          | Some r -> `String r
                          | None -> `Null );
                        ( "connector",
                          match parsed.connector with
                          | Some c -> `String c
                          | None -> `Null );
                        ( "from_timestamp",
                          match parsed.from_timestamp with
                          | Some t -> `String t
                          | None -> `Null );
                        ("limit", `Int parsed.limit);
                      ] );
                ]
            in
            Yojson.Safe.pretty_to_string json
          else format_failures_table results)

(** Summary line for [rooms show] — returns a short failure count or empty. *)
let delivery_failure_summary_line ~db ~room_id () =
  Room_activity_ledger.init_schema db;
  let count_24h =
    Room_activity_ledger.failure_count_last_hours ~db ~room_id ~hours:24 ()
  in
  let count_1h =
    Room_activity_ledger.failure_count_last_hours ~db ~room_id ~hours:1 ()
  in
  if count_24h = 0 then None
  else
    let recent_note =
      if count_1h > 0 then Printf.sprintf " (%d in last hour)" count_1h else ""
    in
    Some (Printf.sprintf "Delivery failures (24h): %d%s" count_24h recent_note)
