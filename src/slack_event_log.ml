(** Metadata-only Slack event log (spec §4, Phase 0 diagnostics). Records ids,
    types, timestamps, room keys and file ids. No text is logged, with one
    exception: a message whose outer event channel and own [bot_id] match one
    [capture_text_from] rule exactly. *)

let file_name = "slack-events.jsonl"
let default_path () = Dot_dir.sub file_name
let default_max_bytes = 16 * 1024 * 1024
let member key = function `Assoc xs -> List.assoc_opt key xs | _ -> None

let scalar key json =
  match member key json with
  | Some ((`String _ | `Bool _ | `Int _ | `Intlit _ | `Float _) as v) -> Some v
  | _ -> None

let scalars keys json =
  List.filter_map (fun k -> Option.map (fun v -> (k, v)) (scalar k json)) keys

let string_field key json =
  match member key json with Some (`String s) -> Some s | _ -> None

let object_field key json =
  match member key json with Some (`Assoc _ as o) -> Some o | _ -> None

let opt key = function None -> [] | Some v -> [ (key, v) ]

let envelope json =
  let callback = scalar "type" json = Some (`String "event_callback") in
  let team_id =
    match member "team_id" json with
    | (None | Some `Null) when not callback ->
        Option.bind (object_field "team" json) (scalar "id")
    | _ -> scalar "team_id" json
  in
  List.filter_map
    (fun key ->
      let value = if key = "team_id" then team_id else scalar key json in
      Option.map (fun v -> (key, v)) value)
    [ "type"; "team_id"; "api_app_id"; "event_id"; "event_time" ]

(* file_id, channel_id and user_id are the ids file_shared, file_deleted and
   file_change carry. *)
let message_scalars =
  [
    "type";
    "subtype";
    "user";
    "bot_id";
    "app_id";
    "username";
    "channel";
    "channel_type";
    "ts";
    "thread_ts";
    "event_ts";
    "hidden";
    "deleted_ts";
    "reaction";
    "item_user";
    "file_id";
    "channel_id";
    "user_id";
  ]

let room_scalars =
  [
    "id";
    "call_family";
    "has_ended";
    "date_start";
    "date_end";
    "thread_root_ts";
    "canvas_thread_ts";
    "transcript_file_id";
  ]

let room_meta fields =
  let r = `Assoc fields in
  let keys = List.sort_uniq String.compare (List.map fst fields) in
  let attached =
    match member "attached_file_ids" r with
    | Some (`List ids) ->
        Some
          (`List (List.filter (function `String _ -> true | _ -> false) ids))
    | _ -> None
  in
  `Assoc
    (("keys", `List (List.map (fun k -> `String k) keys))
     :: scalars room_scalars r
    @ opt "attached_file_ids" attached)

let files_meta xs =
  `List
    (List.filter_map
       (function
         | `Assoc _ as f ->
             Some (`Assoc (scalars [ "id"; "mimetype"; "filetype"; "user" ] f))
         | _ -> None)
       xs)

let text_allowed ~rules ~channel json =
  match (channel, string_field "bot_id" json) with
  | Some channel, Some bot_id ->
      List.exists
        (fun (r : Slack_config_types.event_log_text_rule) ->
          r.text_channel_id = channel && r.text_bot_id = bot_id)
        rules
  | _ -> false

(* One message level. [channel] is always the outer event's channel. *)
let message_fields ~rules ~channel json =
  let item =
    Option.map
      (fun i -> `Assoc (scalars [ "type"; "ts"; "file"; "channel" ] i))
      (object_field "item" json)
  in
  let files =
    match member "files" json with
    | Some (`List xs) -> Some (files_meta xs)
    | _ -> None
  and room =
    match member "room" json with
    | Some (`Assoc fields) -> Some (room_meta fields)
    | _ -> None
  in
  let text =
    match member "text" json with
    | Some (`String _ as t) when text_allowed ~rules ~channel json -> Some t
    | _ -> None
  in
  scalars message_scalars json
  @ opt "file_obj_id" (Option.bind (object_field "file" json) (scalar "id"))
  @ opt "bot_profile_app_id"
      (Option.bind (object_field "bot_profile" json) (scalar "app_id"))
  @ opt "item" item @ opt "files" files @ opt "room" room @ opt "text" text

let event ~rules json =
  let channel = string_field "channel" json in
  let nested key =
    opt key
      (Option.map
         (fun m -> `Assoc (message_fields ~rules ~channel m))
         (object_field key json))
  in
  `Assoc
    (message_fields ~rules ~channel json
    @ nested "message" @ nested "previous_message")

let record ~now ~rules json : Yojson.Safe.t =
  `Assoc
    ([ ("logged_at", `Float now); ("envelope", `Assoc (envelope json)) ]
    @ opt "event" (Option.map (event ~rules) (object_field "event" json)))

let append ?(max_bytes = default_max_bytes) ~path json =
  (match Unix.stat path with
  | { Unix.st_size; _ } when st_size >= max_bytes ->
      Sys.rename path (path ^ ".1")
  | _ -> ()
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  let oc = open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o600 path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc (Yojson.Safe.to_string json);
      output_char oc '\n';
      flush oc)

(** Reads the live config on every call, so toggling [event_log.enabled] takes
    effect on the next event. Never raises. *)
let observe ?path ~manager json =
  try
    match (Session.get_config manager).channels.slack with
    | Some { event_log = { enabled = true; capture_text_from = rules }; _ } ->
        ignore (Dot_dir.ensure ());
        append
          ~path:(Option.value path ~default:(default_path ()))
          (record ~now:(Unix.gettimeofday ()) ~rules json)
    | _ -> ()
  with exn ->
    Logs.warn (fun m ->
        m "Slack event log write failed: %s" (Printexc.to_string exn))
