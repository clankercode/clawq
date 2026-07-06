include Agent_profile

let ledger_room_id_for_session ~db session_key =
  try
    match Memory.get_session_channel ~db ~session_key with
    | Some (_channel, channel_id) when String.trim channel_id <> "" ->
        Some channel_id
    | _ -> (
        match Room_session.parse session_key with
        | Some session when String.trim session.Room_session.channel_id <> "" ->
            Some session.Room_session.channel_id
        | _ -> None)
  with _ -> None

let metadata_optional_int key = function
  | Some value -> [ (key, `Int value) ]
  | None -> []

let record_provider_ledger_event ?db ?session_key ~event_type ~provider ~model
    metadata_fields =
  match (db, session_key) with
  | Some db, Some session_key -> (
      match ledger_room_id_for_session ~db session_key with
      | None -> ()
      | Some room_id -> (
          let metadata =
            `Assoc
              ([ ("provider", `String provider); ("model", `String model) ]
              @ metadata_fields)
          in
          try
            ignore
              (Room_activity_ledger.append_now ~db ~room_id ~event_type
                 ~actor:provider ~metadata)
          with exn ->
            Logs.warn (fun m ->
                m "room_activity_ledger provider event failed: %s"
                  (Printexc.to_string exn))))
  | _ -> ()

let record_provider_request_event ?db ?session_key ~provider ~model ~messages ()
    =
  record_provider_ledger_event ?db ?session_key ~event_type:"provider_request"
    ~provider ~model
    [
      ("message_count", `Int (List.length messages));
      ( "estimated_prompt_tokens",
        `Int (Provider.estimate_messages_tokens messages) );
    ]

let record_provider_error_event ?db ?session_key ~provider ~model exn =
  record_provider_ledger_event ?db ?session_key ~event_type:"provider_error"
    ~provider ~model
    [ ("error", `String (Printexc.to_string exn)) ]

let record_provider_response_event ?db ?session_key ~provider ~model ?usage
    ?cost_usd ?latency_ms () =
  let usage_fields =
    match usage with
    | Some (prompt_tokens, completion_tokens, cached_tokens) ->
        [
          ("prompt_tokens", `Int prompt_tokens);
          ("completion_tokens", `Int completion_tokens);
          ("cached_tokens", `Int cached_tokens);
        ]
    | None -> []
  in
  let cost_field =
    match cost_usd with Some value -> `Float value | None -> `Null
  in
  record_provider_ledger_event ?db ?session_key ~event_type:"provider_response"
    ~provider ~model
    (usage_fields
    @ [ ("cost_usd", cost_field) ]
    @ metadata_optional_int "latency_ms" latency_ms)
