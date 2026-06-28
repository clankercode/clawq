(** Shared room delivery target and result types.

    Provides a connector-agnostic model for tracking message delivery across
    room-based connectors (Slack, Teams, Discord, etc.). The types capture the
    full delivery lifecycle from attempt through confirmation or failure. *)

(** {1 Delivery target} *)

type delivery_target = {
  room_id : string;
  thread_id : string option;
  reply_to_id : string option;
  service_url : string option;
  connector : string;
  message_id : string option;
  activity_id : string option;
}
(** Describes where and how a message should be delivered.

    - [room_id] is the target room/channel identifier.
    - [thread_id] is the optional thread scope for thread-aware connectors.
    - [reply_to_id] is the optional message being replied to.
    - [service_url] is the connector service endpoint (e.g. Teams Bot Framework
      service URL).
    - [connector] is the connector type name (e.g. "slack", "teams", "discord").
    - [message_id] is the connector-assigned message identifier after send.
    - [activity_id] is the connector-assigned activity identifier (Teams). *)

let make_target ?thread_id ?reply_to_id ?service_url ?message_id ?activity_id
    ~room_id ~connector () =
  {
    room_id;
    thread_id;
    reply_to_id;
    service_url;
    connector;
    message_id;
    activity_id;
  }

(** {1 Delivery state} *)

type delivery_state =
  | Attempted
  | Accepted
  | Confirmed
  | Unconfirmed
  | Failed of string
      (** Tracks the lifecycle of a delivery attempt.

          - [Attempted] means the delivery was initiated but no response yet.
          - [Accepted] means the connector acknowledged receipt.
          - [Confirmed] means the connector confirmed final delivery.
          - [Unconfirmed] means the connector did not confirm within timeout.
          - [Failed reason] means the delivery failed with the given error. *)

let state_to_string = function
  | Attempted -> "attempted"
  | Accepted -> "accepted"
  | Confirmed -> "confirmed"
  | Unconfirmed -> "unconfirmed"
  | Failed reason -> "failed:" ^ reason

let state_of_string s =
  match s with
  | "attempted" -> Some Attempted
  | "accepted" -> Some Accepted
  | "confirmed" -> Some Confirmed
  | "unconfirmed" -> Some Unconfirmed
  | _ -> (
      match String.split_on_char ':' s with
      | "failed" :: rest when rest <> [] ->
          Some (Failed (String.concat ":" rest))
      | _ -> None)

let is_terminal = function
  | Confirmed | Unconfirmed | Failed _ -> true
  | Attempted | Accepted -> false

let is_success = function Confirmed -> true | _ -> false

(** {1 Delivery result} *)

type delivery_result = {
  target : delivery_target;
  state : delivery_state;
  timestamp : float;
  error_detail : string option;
}
(** Combines a delivery target with its outcome state.

    - [target] is the delivery target being tracked.
    - [state] is the current delivery lifecycle state.
    - [timestamp] is the Unix time of the last state transition.
    - [error_detail] is an optional extended error description for [Failed]
      state, providing additional context beyond the state's error string. *)

let make_result ?error_detail ~target ~state ~timestamp () =
  { target; state; timestamp; error_detail }

let result_is_terminal (r : delivery_result) = is_terminal r.state
let result_is_success (r : delivery_result) = is_success r.state

(** {1 Unsupported connector fallbacks} *)

type unsupported_reason =
  | Unknown_connector
  | Missing_capability
  | Connector_disabled
      (** Reasons a connector may be unsupported for delivery. *)

let reason_to_string = function
  | Unknown_connector -> "unknown_connector"
  | Missing_capability -> "missing_capability"
  | Connector_disabled -> "connector_disabled"

let reason_of_string = function
  | "unknown_connector" -> Some Unknown_connector
  | "missing_capability" -> Some Missing_capability
  | "connector_disabled" -> Some Connector_disabled
  | _ -> None

let unsupported_target ~room_id ~connector ~reason () =
  make_target ~room_id ~connector ()

let unsupported_result ~room_id ~connector ~reason () =
  let target = unsupported_target ~room_id ~connector ~reason () in
  make_result ~target
    ~state:(Failed (reason_to_string reason))
    ~timestamp:(Unix.gettimeofday ()) ()

(** {1 Serialization} *)

let json_of_target (t : delivery_target) : Yojson.Safe.t =
  let fields =
    [ ("room_id", `String t.room_id); ("connector", `String t.connector) ]
    @ (match t.thread_id with
      | Some id -> [ ("thread_id", `String id) ]
      | None -> [])
    @ (match t.reply_to_id with
      | Some id -> [ ("reply_to_id", `String id) ]
      | None -> [])
    @ (match t.service_url with
      | Some url -> [ ("service_url", `String url) ]
      | None -> [])
    @ (match t.message_id with
      | Some id -> [ ("message_id", `String id) ]
      | None -> [])
    @
    match t.activity_id with
    | Some id -> [ ("activity_id", `String id) ]
    | None -> []
  in
  `Assoc fields

let target_of_json (json : Yojson.Safe.t) : (delivery_target, string) result =
  match json with
  | `Assoc pairs -> (
      let opt_str key =
        match List.assoc_opt key pairs with
        | Some (`String s) when s <> "" -> Some s
        | _ -> None
      in
      match List.assoc_opt "room_id" pairs with
      | Some (`String room_id) -> (
          match List.assoc_opt "connector" pairs with
          | Some (`String connector) ->
              Ok
                {
                  room_id;
                  thread_id = opt_str "thread_id";
                  reply_to_id = opt_str "reply_to_id";
                  service_url = opt_str "service_url";
                  connector;
                  message_id = opt_str "message_id";
                  activity_id = opt_str "activity_id";
                }
          | _ -> Error "delivery_target: missing connector")
      | _ -> Error "delivery_target: missing room_id")
  | _ -> Error "delivery_target: expected JSON object"

let json_of_state = function
  | Attempted -> `String "attempted"
  | Accepted -> `String "accepted"
  | Confirmed -> `String "confirmed"
  | Unconfirmed -> `String "unconfirmed"
  | Failed reason -> `Assoc [ ("failed", `String reason) ]

let state_of_json (json : Yojson.Safe.t) : (delivery_state, string) result =
  match json with
  | `String "attempted" -> Ok Attempted
  | `String "accepted" -> Ok Accepted
  | `String "confirmed" -> Ok Confirmed
  | `String "unconfirmed" -> Ok Unconfirmed
  | `Assoc [ ("failed", `String reason) ] -> Ok (Failed reason)
  | _ -> Error "delivery_state: invalid state"

let json_of_result (r : delivery_result) : Yojson.Safe.t =
  let fields =
    [
      ("target", json_of_target r.target);
      ("state", json_of_state r.state);
      ("timestamp", `Float r.timestamp);
    ]
    @
    match r.error_detail with
    | Some detail -> [ ("error_detail", `String detail) ]
    | None -> []
  in
  `Assoc fields

let result_of_json (json : Yojson.Safe.t) : (delivery_result, string) result =
  match json with
  | `Assoc pairs -> (
      match List.assoc_opt "target" pairs with
      | Some target_json -> (
          match target_of_json target_json with
          | Ok target -> (
              match List.assoc_opt "state" pairs with
              | Some state_json -> (
                  match state_of_json state_json with
                  | Ok state ->
                      let timestamp =
                        match List.assoc_opt "timestamp" pairs with
                        | Some (`Float f) -> f
                        | Some (`Int n) -> Float.of_int n
                        | _ -> 0.0
                      in
                      let error_detail =
                        match List.assoc_opt "error_detail" pairs with
                        | Some (`String s) when s <> "" -> Some s
                        | _ -> None
                      in
                      Ok { target; state; timestamp; error_detail }
                  | Error e -> Error e)
              | None -> Error "delivery_result: missing state")
          | Error e -> Error e)
      | None -> Error "delivery_result: missing target")
  | _ -> Error "delivery_result: expected JSON object"
