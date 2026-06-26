(** Typed room-origin metadata for task, background, and ledger rows.

    Captures where a request originated: the connector (Slack, Teams, etc.),
    workspace/team and room identifiers, who requested it, and any enclosing
    thread or service context.  All fields are optional so the type works for
    CLI and API origins that may lack connector context. *)

type t = {
  connector : string option;
  workspace_id : string option;
  room_id : string option;
  requester_id : string option;
  requester_name : string option;
  source_message_id : string option;
  thread_id : string option;
  service_url : string option;
  profile_id : int option;
}

let make ?connector ?workspace_id ?room_id ?requester_id ?requester_name
    ?source_message_id ?thread_id ?service_url ?profile_id () =
  {
    connector;
    workspace_id;
    room_id;
    requester_id;
    requester_name;
    source_message_id;
    thread_id;
    service_url;
    profile_id;
  }

let empty =
  {
    connector = None;
    workspace_id = None;
    room_id = None;
    requester_id = None;
    requester_name = None;
    source_message_id = None;
    thread_id = None;
    service_url = None;
    profile_id = None;
  }

let is_empty (t : t) =
  t.connector = None && t.workspace_id = None && t.room_id = None
  && t.requester_id = None && t.requester_name = None
  && t.source_message_id = None && t.thread_id = None
  && t.service_url = None && t.profile_id = None

let json_of_opt_string = function
  | None -> `Null
  | Some s -> `String s

let json_of_opt_int = function None -> `Null | Some n -> `Int n

let to_json (t : t) : Yojson.Safe.t =
  `Assoc
    [
      ("connector", json_of_opt_string t.connector);
      ("workspace_id", json_of_opt_string t.workspace_id);
      ("room_id", json_of_opt_string t.room_id);
      ("requester_id", json_of_opt_string t.requester_id);
      ("requester_name", json_of_opt_string t.requester_name);
      ("source_message_id", json_of_opt_string t.source_message_id);
      ("thread_id", json_of_opt_string t.thread_id);
      ("service_url", json_of_opt_string t.service_url);
      ("profile_id", json_of_opt_int t.profile_id);
    ]

let to_json_string (t : t) = Yojson.Safe.to_string (to_json t)

let to_compact_json (t : t) : Yojson.Safe.t =
  let fields =
    [
      ("connector", Option.map (fun s -> `String s) t.connector);
      ("workspace_id", Option.map (fun s -> `String s) t.workspace_id);
      ("room_id", Option.map (fun s -> `String s) t.room_id);
      ("requester_id", Option.map (fun s -> `String s) t.requester_id);
      ("requester_name", Option.map (fun s -> `String s) t.requester_name);
      ( "source_message_id",
        Option.map (fun s -> `String s) t.source_message_id );
      ("thread_id", Option.map (fun s -> `String s) t.thread_id);
      ("service_url", Option.map (fun s -> `String s) t.service_url);
      ("profile_id", Option.map (fun n -> `Int n) t.profile_id);
    ]
    |> List.filter_map (fun (k, v) -> Option.map (fun v -> (k, v)) v)
  in
  `Assoc fields

let to_compact_json_string (t : t) = Yojson.Safe.to_string (to_compact_json t)

let opt_string_of_json key json =
  match List.assoc_opt key json with
  | Some (`String s) when s <> "" -> Some s
  | _ -> None

let opt_int_of_json key json =
  match List.assoc_opt key json with Some (`Int n) -> Some n | _ -> None

let of_json (json : Yojson.Safe.t) : (t, string) result =
  match json with
  | `Assoc pairs ->
      Ok
        {
          connector = opt_string_of_json "connector" pairs;
          workspace_id = opt_string_of_json "workspace_id" pairs;
          room_id = opt_string_of_json "room_id" pairs;
          requester_id = opt_string_of_json "requester_id" pairs;
          requester_name = opt_string_of_json "requester_name" pairs;
          source_message_id = opt_string_of_json "source_message_id" pairs;
          thread_id = opt_string_of_json "thread_id" pairs;
          service_url = opt_string_of_json "service_url" pairs;
          profile_id = opt_int_of_json "profile_id" pairs;
        }
  | `Null -> Ok empty
  | _ -> Error "room_origin: expected JSON object or null"

let of_json_string s =
  try
    let json = Yojson.Safe.from_string s in
    of_json json
  with Yojson.Json_error msg -> Error ("room_origin: " ^ msg)

let of_json_string_opt s =
  match of_json_string s with Ok t -> Some t | Error _ -> None

(** [from_room_session session] builds a [t] from a typed room-session,
    populating connector and the channel/sender fields.  Optional
    [?workspace_id], [?thread_id], [?service_url], and [?profile_id] are
    lifted from the caller context when available. *)
let from_room_session ?workspace_id ?thread_id ?service_url ?profile_id
    (session : Room_session.session) =
  {
    connector = Some (Room_session.channel_to_string session.channel);
    workspace_id;
    room_id = Some session.channel_id;
    requester_id = Some session.sender_id;
    requester_name = None;
    source_message_id = None;
    thread_id;
    service_url;
    profile_id;
  }

let connector_display_name = function
  | Some "slack" -> "Slack"
  | Some "teams" -> "Teams"
  | Some "discord" -> "Discord"
  | Some "telegram" -> "Telegram"
  | Some "web" -> "Web"
  | Some s -> s
  | None -> "CLI"

let display_summary (t : t) =
  let connector = connector_display_name t.connector in
  let room = match t.room_id with Some r -> r | None -> "-" in
  let requester =
    match t.requester_name with
    | Some name -> name
    | None -> (
        match t.requester_id with Some id -> id | None -> "-")
  in
  Printf.sprintf "%s room=%s requester=%s" connector room requester
