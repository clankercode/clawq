open Command_bridge_helpers
open Command_bridge_room_common

type ledger_filters = {
  room_id : string option;
  event_type : string option;
  from_timestamp : string option;
  to_timestamp : string option;
  actor : string option;
  metadata_filters : (string * string) list;
}

let empty_ledger_filters =
  {
    room_id = None;
    event_type = None;
    from_timestamp = None;
    to_timestamp = None;
    actor = None;
    metadata_filters = [];
  }

let metadata_filter_key = function
  | "--profile-id" | "--profile" -> Some "profile_id"
  | "--thread-id" | "--thread" -> Some "thread_id"
  | "--task-id" | "--task" -> Some "task_id"
  | "--background-id" | "--background" -> Some "background_id"
  | "--requester" -> Some "requester"
  | "--status" -> Some "status"
  | _ -> None

let parse_ledger_filters flags =
  let rec loop filters jsonl = function
    | [] -> Ok (filters, jsonl)
    | ("--room-id" | "--room") :: value :: rest ->
        loop { filters with room_id = Some value } jsonl rest
    | ("--event-type" | "--type") :: value :: rest ->
        loop { filters with event_type = Some value } jsonl rest
    | ("--from" | "--since") :: value :: rest ->
        loop { filters with from_timestamp = Some value } jsonl rest
    | ("--to" | "--until") :: value :: rest ->
        loop { filters with to_timestamp = Some value } jsonl rest
    | "--actor" :: value :: rest ->
        loop { filters with actor = Some value } jsonl rest
    | "--format" :: "json" :: rest -> loop filters false rest
    | "--format" :: "jsonl" :: rest -> loop filters true rest
    | "--jsonl" :: rest -> loop filters true rest
    | flag :: value :: rest -> (
        match metadata_filter_key flag with
        | Some key ->
            loop
              {
                filters with
                metadata_filters = (key, value) :: filters.metadata_filters;
              }
              jsonl rest
        | None -> Error (Printf.sprintf "unknown rooms ledger flag: %s" flag))
    | [ flag ] ->
        Error (Printf.sprintf "missing value for rooms ledger flag: %s" flag)
  in
  loop empty_ledger_filters false flags

let metadata_value_matches expected = function
  | `String value -> String.equal value expected
  | `Int value -> String.equal (string_of_int value) expected
  | `Intlit value -> String.equal value expected
  | `Bool value -> String.equal (string_of_bool value) expected
  | _ -> false

let event_matches_metadata filters (event : Room_activity_ledger.event) =
  match event.metadata with
  | `Assoc fields ->
      List.for_all
        (fun (key, expected) ->
          match List.assoc_opt key fields with
          | Some value -> metadata_value_matches expected value
          | None -> false)
        filters
  | _ -> filters = []

let apply_ledger_filters filters events =
  events
  |> List.filter (fun (event : Room_activity_ledger.event) ->
      match filters.actor with
      | Some actor -> String.equal event.actor actor
      | None -> true)
  |> List.filter (event_matches_metadata filters.metadata_filters)

let query_room_ledger ~db filters =
  Room_activity_ledger.query ?room_id:filters.room_id
    ?event_type:filters.event_type ?from_timestamp:filters.from_timestamp
    ?to_timestamp:filters.to_timestamp ~db ()
  |> apply_ledger_filters filters

let format_ledger_events events =
  match events with
  | [] -> "No room activity ledger entries matched."
  | _ ->
      let columns =
        Table_format.
          [
            { header = "ID"; align = Right; min_width = 3; flex = false };
            { header = "TIMESTAMP"; align = Left; min_width = 20; flex = false };
            { header = "ROOM"; align = Left; min_width = 8; flex = false };
            { header = "EVENT"; align = Left; min_width = 10; flex = false };
            { header = "ACTOR"; align = Left; min_width = 8; flex = false };
          ]
      in
      let rows =
        List.map
          (fun (event : Room_activity_ledger.event) ->
            [
              string_of_int event.id;
              event.timestamp;
              event.room_id;
              event.event_type;
              event.actor;
            ])
          events
      in
      Format_adapter.render_table Format_adapter.Plain ~max_width:120 columns
        rows

let floor_div a b =
  let q = a / b in
  let r = a mod b in
  if r <> 0 && r < 0 <> (b < 0) then q - 1 else q

let days_from_civil year month day =
  let year = if month <= 2 then year - 1 else year in
  let era = floor_div (if year >= 0 then year else year - 399) 400 in
  let yoe = year - (era * 400) in
  let mp = month + if month > 2 then -3 else 9 in
  let doy = (((153 * mp) + 2) / 5) + day - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let civil_from_days days =
  let days = days + 719468 in
  let era = floor_div (if days >= 0 then days else days - 146096) 146097 in
  let doe = days - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let year = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let day = doy - (((153 * mp) + 2) / 5) + 1 in
  let month = mp + if mp < 10 then 3 else -9 in
  let year = year + if month <= 2 then 1 else 0 in
  (year, month, day)

let strip_fractional_seconds timestamp =
  match (String.index_opt timestamp '.', String.rindex_opt timestamp 'Z') with
  | Some dot, Some z when dot < z -> String.sub timestamp 0 dot ^ "Z"
  | _ -> timestamp

let epoch_seconds_of_timestamp timestamp =
  try
    Scanf.sscanf (strip_fractional_seconds timestamp) "%d-%d-%dT%d:%d:%dZ"
      (fun year month day hour minute second ->
        Ok
          ((days_from_civil year month day * 86400)
          + (hour * 3600) + (minute * 60) + second))
  with _ ->
    Error
      (Printf.sprintf
         "timestamp must use UTC ISO-8601 format like 2026-06-27T10:00:00Z: %s"
         timestamp)

let timestamp_of_epoch_seconds seconds =
  let days = floor_div seconds 86400 in
  let seconds_of_day = seconds - (days * 86400) in
  let year, month, day = civil_from_days days in
  let hour = seconds_of_day / 3600 in
  let minute = seconds_of_day mod 3600 / 60 in
  let second = seconds_of_day mod 60 in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" year month day hour minute
    second

let parse_ledger_retention flags =
  let rec loop retention_days now = function
    | [] -> Ok (retention_days, now)
    | "--retention-days" :: value :: rest -> (
        match float_of_string_opt value with
        | Some days when days >= 0.0 -> loop days now rest
        | _ -> Error "--retention-days must be a non-negative number")
    | "--now" :: value :: rest -> loop retention_days value rest
    | flag :: _ ->
        Error (Printf.sprintf "unknown rooms ledger retention flag: %s" flag)
  in
  loop Room_workspace.default_retention_days
    (Room_activity_ledger.timestamp_now ())
    flags

let retention_cutoff ~now ~retention_days =
  match epoch_seconds_of_timestamp now with
  | Error _ as err -> err
  | Ok now_seconds ->
      let retention_seconds = int_of_float (retention_days *. 86400.0) in
      Ok (timestamp_of_epoch_seconds (now_seconds - retention_seconds))

let cmd_rooms_ledger args =
  match require_admin () with
  | Some err -> err
  | None -> (
      match args with
      | "list" :: flags -> (
          match parse_ledger_filters flags with
          | Error msg -> "Error: " ^ msg
          | Ok (filters, _) ->
              let events = query_room_ledger ~db:(get_db ()) filters in
              format_ledger_events events)
      | "export" :: flags -> (
          match parse_ledger_filters flags with
          | Error msg -> "Error: " ^ msg
          | Ok (filters, jsonl) ->
              let events = query_room_ledger ~db:(get_db ()) filters in
              if jsonl then Room_activity_ledger.events_to_jsonl events
              else Room_activity_ledger.events_to_json_string events)
      | "retention-cleanup" :: flags | "retention" :: flags -> (
          match parse_ledger_retention flags with
          | Error msg -> "Error: " ^ msg
          | Ok (retention_days, now) -> (
              match retention_cutoff ~now ~retention_days with
              | Error msg -> "Error: " ^ msg
              | Ok before_timestamp ->
                  let deleted =
                    Room_activity_ledger.delete_before ~db:(get_db ())
                      ~before_timestamp
                  in
                  Printf.sprintf
                    "Room activity ledger retention cleanup complete: deleted \
                     %d entr%s older than %s (retention %.1f day(s))."
                    deleted
                    (if deleted = 1 then "y" else "ies")
                    before_timestamp retention_days))
      | _ ->
          "Usage: clawq rooms ledger <list|export|retention-cleanup> [filters]\n\n\
           Filters: --room-id ID --event-type TYPE --from TS --to TS --actor \
           ACTOR\n\
          \         --profile-id ID --thread-id ID --task-id ID \
           --background-id ID\n\
          \         --requester ID --status STATUS\n\
           Export:  add --format jsonl or --jsonl for JSON Lines")
