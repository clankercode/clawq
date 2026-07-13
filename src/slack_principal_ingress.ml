(** Slack Socket Mode / Events API ingress principal derivation
    (P21.M1.E1.T006).

    Pure validation of Socket Mode WSS JSON and fail-closed derivation of
    workspace-scoped immutable human identity. HTTP Events signing-secret
    verification is a separate trust path. *)

type human_identity = {
  team_id : string;
  enterprise_id : string option;
  user_id : string;
}

type envelope_meta = {
  envelope_id : string;
  envelope_type : string;
  accepts_response_payload : bool;
}

type verified_event = {
  envelope : envelope_meta;
  api_app_id : string option;
  team_id : string;
  enterprise_id : string option;
  event_type : string;
  channel_id : string option;
  event_ts : string option;
}

type outcome =
  | Human of {
      identity : human_identity;
      display_name : string option;
      event : verified_event;
      ack : Yojson.Safe.t;
    }
  | Hello of { app_id : string; num_connections : int option }
  | Disconnect of { reason : string option }
  | Ack_only of {
      envelope_id : string;
      envelope_type : string;
      reason : string;
      ack : Yojson.Safe.t;
    }
  | Bot_rejected of string
  | Replay of string
  | Invalid of string

type seen_set = { has : string -> bool; mark : string -> unit }

let default_max_skew_s = 300.0

(* ---- helpers ---- *)

let invalid msg = Invalid msg

let json_string_field json name =
  let open Yojson.Safe.Util in
  try match member name json with `String s -> Some s | `Null | _ -> None
  with _ -> None

let json_bool_field json name =
  let open Yojson.Safe.Util in
  try match member name json with `Bool b -> Some b | _ -> None
  with _ -> None

let json_int_field json name =
  let open Yojson.Safe.Util in
  try
    match member name json with
    | `Int i -> Some i
    | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
    | `Float f -> Some (int_of_float f)
    | _ -> None
  with _ -> None

let non_empty_opt = function
  | None -> None
  | Some s ->
      let t = String.trim s in
      if t = "" then None else Some t

let make_ack ~envelope_id = `Assoc [ ("envelope_id", `String envelope_id) ]

let human_identity_key (h : human_identity) =
  match h.enterprise_id with
  | Some eid ->
      Printf.sprintf "enterprise:%s:team:%s:user:%s" eid h.team_id h.user_id
  | None -> Printf.sprintf "team:%s:user:%s" h.team_id h.user_id

let workspace_scope (h : human_identity) =
  match h.enterprise_id with
  | Some eid -> Printf.sprintf "%s/%s" eid h.team_id
  | None -> h.team_id

let to_connector_actor_key (h : human_identity) =
  Principal_identity.make_connector_actor_key ~connector:Slack
    ~tenant_or_workspace:(workspace_scope h) ~immutable_user_id:h.user_id

let empty_seen_set () =
  let tbl : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  {
    has = (fun id -> Hashtbl.mem tbl id);
    mark = (fun id -> Hashtbl.replace tbl id ());
  }

(* ---- HTTP Events API signing secret (separate trust path) ---- *)

let verify_events_api_signature ?now ?(max_skew_s = default_max_skew_s)
    ~signing_secret ~timestamp ~body ~signature () =
  let secret = String.trim signing_secret in
  if secret = "" then Error "signing_secret must be non-empty"
  else
    let ts =
      try float_of_string (String.trim timestamp) with _ -> Float.nan
    in
    if not (Float.is_finite ts) then
      Error "timestamp is not a valid unix epoch seconds value"
    else
      let now = match now with Some t -> t | None -> Unix.gettimeofday () in
      if Float.abs (now -. ts) > max_skew_s then
        Error
          (Printf.sprintf
             "timestamp skew exceeds %.0fs (fail closed; HTTP Events cannot \
              inherit Socket Mode trust)"
             max_skew_s)
      else
        let basestring = "v0:" ^ String.trim timestamp ^ ":" ^ body in
        let expected =
          "v0=" ^ Digestif.SHA256.(hmac_string ~key:secret basestring |> to_hex)
        in
        let sig_trim = String.trim signature in
        if sig_trim = "" then Error "signature header is empty"
        else if not (Eqaf.equal expected sig_trim) then
          Error "Slack signing-secret HMAC verification failed"
        else Ok ()

(* ---- hello / disconnect ---- *)

let validate_hello ?expected_app_id json =
  match json_string_field json "type" with
  | Some "hello" -> (
      let open Yojson.Safe.Util in
      let app_id =
        try
          match member "connection_info" json with
          | `Assoc _ as ci -> json_string_field ci "app_id"
          | _ -> None
        with _ -> None
      in
      match non_empty_opt app_id with
      | None -> invalid "hello missing connection_info.app_id"
      | Some app_id -> (
          match expected_app_id with
          | Some exp when String.trim exp <> "" && not (String.equal app_id exp)
            ->
              invalid
                (Printf.sprintf
                   "hello app_id mismatch: got %s expected %s (fail closed)"
                   app_id exp)
          | _ ->
              let num_connections = json_int_field json "num_connections" in
              Hello { app_id; num_connections }))
  | Some other ->
      invalid
        (Printf.sprintf "validate_hello expected type=hello, got %s" other)
  | None -> invalid "hello message missing type"

(* ---- envelope identity extraction ---- *)

let extract_team_and_enterprise payload =
  let team_id =
    match non_empty_opt (json_string_field payload "team_id") with
    | Some t -> Some t
    | None -> (
        let
        (* interactive payloads nest team *)
        open
          Yojson.Safe.Util in
        try
          match member "team" payload with
          | `Assoc _ as team -> non_empty_opt (json_string_field team "id")
          | _ -> None
        with _ -> None)
  in
  let enterprise_id =
    match non_empty_opt (json_string_field payload "enterprise_id") with
    | Some e -> Some e
    | None -> (
        let open Yojson.Safe.Util in
        try
          match member "enterprise" payload with
          | `Assoc _ as ent -> non_empty_opt (json_string_field ent "id")
          | _ -> None
        with _ -> None)
  in
  (team_id, enterprise_id)

let check_namespace ~expected_team_id ~expected_enterprise_id ~team_id
    ~enterprise_id =
  match expected_team_id with
  | Some exp when String.trim exp <> "" && not (String.equal team_id exp) ->
      Error
        (Printf.sprintf "team_id mismatch: got %s expected %s (fail closed)"
           team_id exp)
  | _ -> (
      match expected_enterprise_id with
      | Some exp when String.trim exp <> "" -> (
          match enterprise_id with
          | None ->
              Error
                (Printf.sprintf
                   "expected enterprise_id %s but payload has none (fail \
                    closed)"
                   exp)
          | Some eid when not (String.equal eid exp) ->
              Error
                (Printf.sprintf
                   "enterprise_id mismatch: got %s expected %s (fail closed)"
                   eid exp)
          | Some _ -> Ok ())
      | _ -> Ok ())

let is_bot_event event =
  match non_empty_opt (json_string_field event "bot_id") with
  | Some _ -> true
  | None -> (
      match json_string_field event "subtype" with
      | Some "bot_message" -> true
      | _ -> false)

let extract_user_from_event event =
  (* Prefer immutable user id fields; never treat username/display as identity. *)
  match non_empty_opt (json_string_field event "user") with
  | Some u -> Ok (u, None)
  | None -> (
      match non_empty_opt (json_string_field event "user_id") with
      | Some u -> Ok (u, None)
      | None -> (
          let open Yojson.Safe.Util in
          let nested =
            try
              match member "user" event with
              | `Assoc _ as uobj ->
                  let id = non_empty_opt (json_string_field uobj "id") in
                  let name =
                    match non_empty_opt (json_string_field uobj "name") with
                    | Some n -> Some n
                    | None -> non_empty_opt (json_string_field uobj "username")
                  in
                  (id, name)
              | _ -> (None, None)
            with _ -> (None, None)
          in
          match nested with
          | Some id, display -> Ok (id, display)
          | None, Some _display ->
              Error
                "display-only identity rejected: missing immutable user_id \
                 (username/display_name cannot establish a Principal)"
          | None, None -> Error "event missing immutable user_id (fail closed)")
      )

let extract_user_from_interactive payload =
  let open Yojson.Safe.Util in
  try
    match member "user" payload with
    | `Assoc _ as uobj -> (
        match non_empty_opt (json_string_field uobj "id") with
        | Some id ->
            let display =
              match non_empty_opt (json_string_field uobj "name") with
              | Some n -> Some n
              | None -> non_empty_opt (json_string_field uobj "username")
            in
            Ok (id, display)
        | None ->
            Error
              "interactive payload user missing immutable id (display-only \
               rejected)")
    | `String s -> (
        match non_empty_opt (Some s) with
        | Some id -> Ok (id, None)
        | None -> Error "interactive payload user empty")
    | _ -> (
        match non_empty_opt (json_string_field payload "user_id") with
        | Some id ->
            let display =
              non_empty_opt (json_string_field payload "user_name")
            in
            Ok (id, display)
        | None -> Error "interactive/slash payload missing user_id")
  with _ -> Error "interactive payload user parse failed"

let channel_of_event event =
  match non_empty_opt (json_string_field event "channel") with
  | Some c -> Some c
  | None -> (
      let open Yojson.Safe.Util in
      try
        match member "channel" event with
        | `Assoc _ as ch -> non_empty_opt (json_string_field ch "id")
        | _ -> None
      with _ -> None)

let human_from_parts ~envelope ~api_app_id ~team_id ~enterprise_id ~event_type
    ~channel_id ~event_ts ~user_id ~display_name ~ack =
  let identity = { team_id; enterprise_id; user_id } in
  let verified =
    {
      envelope;
      api_app_id;
      team_id;
      enterprise_id;
      event_type;
      channel_id;
      event_ts;
    }
  in
  Human { identity; display_name; event = verified; ack }

let derive_from_event_callback ~envelope ~expected_app_id ~expected_team_id
    ~expected_enterprise_id ~payload ~ack =
  let api_app_id = non_empty_opt (json_string_field payload "api_app_id") in
  match (expected_app_id, api_app_id) with
  | Some exp, Some got when String.trim exp <> "" && not (String.equal got exp)
    ->
      invalid
        (Printf.sprintf "api_app_id mismatch: got %s expected %s (fail closed)"
           got exp)
  | Some exp, None when String.trim exp <> "" ->
      invalid "events_api payload missing api_app_id (fail closed)"
  | _ -> (
      match extract_team_and_enterprise payload with
      | None, _ -> invalid "events_api payload missing team_id (fail closed)"
      | Some team_id, enterprise_id -> (
          match
            check_namespace ~expected_team_id ~expected_enterprise_id ~team_id
              ~enterprise_id
          with
          | Error e -> invalid e
          | Ok () -> (
              let open Yojson.Safe.Util in
              let event = try member "event" payload with _ -> `Null in
              match event with
              | `Null | `Assoc [] ->
                  invalid "events_api payload missing event object"
              | event when is_bot_event event ->
                  Bot_rejected
                    "bot_id/bot_message cannot establish a human Principal"
              | event -> (
                  let event_type =
                    match json_string_field event "type" with
                    | Some t -> t
                    | None -> "unknown"
                  in
                  match extract_user_from_event event with
                  | Error e -> invalid e
                  | Ok (user_id, display_name) ->
                      let channel_id = channel_of_event event in
                      let event_ts =
                        match
                          non_empty_opt (json_string_field event "event_ts")
                        with
                        | Some t -> Some t
                        | None -> non_empty_opt (json_string_field event "ts")
                      in
                      human_from_parts ~envelope ~api_app_id ~team_id
                        ~enterprise_id ~event_type ~channel_id ~event_ts
                        ~user_id ~display_name ~ack))))

let derive_from_events_api ~envelope ~expected_app_id ~expected_team_id
    ~expected_enterprise_id ~payload ~ack =
  match json_string_field payload "type" with
  | Some "url_verification" ->
      Ack_only
        {
          envelope_id = envelope.envelope_id;
          envelope_type = envelope.envelope_type;
          reason = "url_verification is HTTP Events challenge, not identity";
          ack;
        }
  | Some "event_callback" | Some "" | None ->
      (* Socket Mode wraps Events API; type is often event_callback. *)
      derive_from_event_callback ~envelope ~expected_app_id ~expected_team_id
        ~expected_enterprise_id ~payload ~ack
  | Some other ->
      Ack_only
        {
          envelope_id = envelope.envelope_id;
          envelope_type = envelope.envelope_type;
          reason =
            Printf.sprintf "events_api payload type %s not identity" other;
          ack;
        }

let channel_of_interactive payload =
  let open Yojson.Safe.Util in
  try
    match member "channel" payload with
    | `Assoc _ as ch -> non_empty_opt (json_string_field ch "id")
    | `String s -> non_empty_opt (Some s)
    | _ -> non_empty_opt (json_string_field payload "channel_id")
  with _ -> non_empty_opt (json_string_field payload "channel_id")

let derive_from_interactive ~envelope ~expected_app_id ~expected_team_id
    ~expected_enterprise_id ~payload ~ack =
  let api_app_id = non_empty_opt (json_string_field payload "api_app_id") in
  match (expected_app_id, api_app_id) with
  | Some exp, Some got when String.trim exp <> "" && not (String.equal got exp)
    ->
      invalid
        (Printf.sprintf "api_app_id mismatch: got %s expected %s (fail closed)"
           got exp)
  | _ -> (
      match extract_team_and_enterprise payload with
      | None, _ -> invalid "interactive payload missing team_id (fail closed)"
      | Some team_id, enterprise_id -> (
          match
            check_namespace ~expected_team_id ~expected_enterprise_id ~team_id
              ~enterprise_id
          with
          | Error e -> invalid e
          | Ok () -> (
              let open Yojson.Safe.Util in
              let bot_on_message =
                try
                  match member "message" payload with
                  | `Assoc _ as msg -> is_bot_event msg
                  | _ -> false
                with _ -> false
              in
              if bot_on_message then
                Bot_rejected
                  "bot interactive message cannot establish a human Principal"
              else
                match extract_user_from_interactive payload with
                | Error e -> invalid e
                | Ok (user_id, display_name) ->
                    let channel_id = channel_of_interactive payload in
                    let event_type =
                      match json_string_field payload "type" with
                      | Some t -> t
                      | None -> envelope.envelope_type
                    in
                    human_from_parts ~envelope ~api_app_id ~team_id
                      ~enterprise_id ~event_type ~channel_id ~event_ts:None
                      ~user_id ~display_name ~ack)))

let claim_envelope_id (seen : seen_set option) envelope_id =
  match seen with
  | None -> Ok ()
  | Some s ->
      if s.has envelope_id then
        Error (Replay ("duplicate envelope_id (ack-once): " ^ envelope_id))
      else begin
        s.mark envelope_id;
        Ok ()
      end

let validate_envelope ?expected_app_id ?expected_team_id ?expected_enterprise_id
    ?seen json =
  let envelope_id = non_empty_opt (json_string_field json "envelope_id") in
  let envelope_type = non_empty_opt (json_string_field json "type") in
  match (envelope_id, envelope_type) with
  | None, _ -> invalid "envelope missing envelope_id (fail closed)"
  | _, None -> invalid "envelope missing type (fail closed)"
  | Some envelope_id, Some envelope_type -> (
      match claim_envelope_id seen envelope_id with
      | Error (Replay _ as r) -> r
      | Error _ -> invalid "envelope_id claim failed"
      | Ok () -> (
          let accepts_response_payload =
            match json_bool_field json "accepts_response_payload" with
            | Some b -> b
            | None -> false
          in
          let envelope =
            { envelope_id; envelope_type; accepts_response_payload }
          in
          let ack = make_ack ~envelope_id in
          let open Yojson.Safe.Util in
          let payload = try member "payload" json with _ -> `Null in
          match envelope_type with
          | "events_api" ->
              derive_from_events_api ~envelope ~expected_app_id
                ~expected_team_id ~expected_enterprise_id ~payload ~ack
          | "interactive" | "slash_commands" ->
              derive_from_interactive ~envelope ~expected_app_id
                ~expected_team_id ~expected_enterprise_id ~payload ~ack
          | "disconnect" ->
              (* disconnect may arrive as typed envelope in some clients *)
              let reason =
                match payload with
                | `Null -> json_string_field json "reason"
                | p -> (
                    match json_string_field p "reason" with
                    | Some r -> Some r
                    | None -> json_string_field json "reason")
              in
              Disconnect { reason = non_empty_opt reason }
          | other ->
              Ack_only
                {
                  envelope_id;
                  envelope_type = other;
                  reason =
                    Printf.sprintf
                      "envelope type %s acknowledged without human identity"
                      other;
                  ack;
                }))

let validate_socket_message ?expected_app_id ?expected_team_id
    ?expected_enterprise_id ?seen json =
  match json_string_field json "type" with
  | Some "hello" -> validate_hello ?expected_app_id json
  | Some "disconnect" when json_string_field json "envelope_id" = None ->
      let reason = non_empty_opt (json_string_field json "reason") in
      Disconnect { reason }
  | Some _ when json_string_field json "envelope_id" <> None ->
      validate_envelope ?expected_app_id ?expected_team_id
        ?expected_enterprise_id ?seen json
  | Some other ->
      invalid
        (Printf.sprintf
           "unrecognized Socket Mode message type %s without envelope_id" other)
  | None ->
      if json_string_field json "envelope_id" <> None then
        validate_envelope ?expected_app_id ?expected_team_id
          ?expected_enterprise_id ?seen json
      else invalid "Socket Mode message missing type and envelope_id"

let validate_socket_message_string ?expected_app_id ?expected_team_id
    ?expected_enterprise_id ?seen raw =
  try
    let json = Yojson.Safe.from_string raw in
    validate_socket_message ?expected_app_id ?expected_team_id
      ?expected_enterprise_id ?seen json
  with exn ->
    invalid ("JSON parse failed (fail closed): " ^ Printexc.to_string exn)
