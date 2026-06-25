open Command_bridge_helpers

let heartbeat_status_text ~db ~config ~session_key =
  if not (Session.heartbeat_supported_session_key session_key) then
    Session.heartbeat_unsupported_reason session_key
  else
    let enabled = Memory.session_heartbeat_enabled ~db ~session_key in
    if config.Runtime_config.heartbeat.enabled then
      Printf.sprintf "Session %s: heartbeat = %s" session_key
        (if enabled then "on" else "off")
    else
      Printf.sprintf
        "Session %s: heartbeat = %s (global heartbeat disabled in config)"
        session_key
        (if enabled then "on" else "off")

let set_heartbeat_for_session ~db ~session_key ~enabled =
  if not (Session.heartbeat_supported_session_key session_key) then
    Error (Session.heartbeat_unsupported_reason session_key)
  else begin
    Memory.set_session_heartbeat ~db ~session_key ~enabled;
    Ok ()
  end

let session_list_columns =
  Table_format.
    [
      { header = "SESSION"; align = Left; min_width = 12; flex = true };
      { header = "STATE"; align = Left; min_width = 6; flex = false };
      { header = "CHANNEL"; align = Left; min_width = 7; flex = false };
      { header = "MSGS"; align = Right; min_width = 4; flex = false };
      { header = "ARCH"; align = Right; min_width = 4; flex = false };
      { header = "FLAGS"; align = Left; min_width = 5; flex = false };
      { header = "COST"; align = Right; min_width = 8; flex = false };
      { header = "LAST ACTIVE"; align = Left; min_width = 10; flex = false };
    ]

let format_session_list_table ~db sessions =
  let rows =
    List.map
      (fun (row : Memory.session_info) ->
        let channel =
          match row.channel with
          | Some value -> value
          | None -> (
              match Memory.parse_channel_from_session_key row.session_key with
              | Some value -> value
              | None -> "-")
        in
        let state = if row.turn = Some "agent" then "active" else "inactive" in
        let pending = Memory.queue_count ~db ~session_key:row.session_key in
        let flags =
          let parts = ref [] in
          if pending > 0 then
            parts := Printf.sprintf "pending:%d" pending :: !parts;
          if row.heartbeat_enabled then parts := "heartbeat" :: !parts;
          if row.keepalive_enabled then parts := "keepalive" :: !parts;
          String.concat " " (List.rev !parts)
        in
        let cost_info =
          Request_stats.summary_for_session ~db ~session_key:row.session_key
        in
        let cost =
          if cost_info.total_cost_usd > 0.0 then
            Printf.sprintf "$%.4f" cost_info.total_cost_usd
          else "-"
        in
        let last_active =
          match row.last_active with Some ts -> ts | None -> "-"
        in
        [
          row.session_key;
          state;
          channel;
          string_of_int row.message_count;
          string_of_int row.archived_epoch_count;
          flags;
          cost;
          last_active;
        ])
      sessions
  in
  Table_format.render session_list_columns rows

let cmd_session args =
  let db = get_db () in
  let config = get_config () in
  match args with
  | [] | [ "list" ] ->
      let sessions = Memory.list_session_infos ~db () in
      if sessions = [] then "No sessions found"
      else format_session_list_table ~db sessions
  | "list" :: rest -> (
      match parse_session_list_args rest with
      | Error msg -> msg
      | Ok parsed ->
          let sessions =
            Memory.list_session_infos ~db ?channel:parsed.channel
              ?prefix:parsed.prefix ~activity:parsed.activity
              ?only_main:parsed.only_main
              ~include_postmortem:parsed.include_postmortem ()
          in
          if sessions = [] then "No sessions matched"
          else format_session_list_table ~db sessions)
  | [ "archives" ] ->
      let rows = Memory.list_archive_sessions ~db () in
      if rows = [] then "No session archives found"
      else
        let columns =
          Table_format.
            [
              { header = "SESSION"; align = Left; min_width = 12; flex = true };
              {
                header = "ARCHIVES";
                align = Right;
                min_width = 8;
                flex = false;
              };
            ]
        in
        let data =
          List.map (fun (key, count) -> [ key; string_of_int count ]) rows
        in
        Table_format.render columns data
  | [ "archives"; session_key ] ->
      let rows = Memory.list_archives_for_session ~db ~session_key in
      if rows = [] then
        Printf.sprintf "No archives found for session %s" session_key
      else
        let columns =
          Table_format.
            [
              { header = "ID"; align = Right; min_width = 4; flex = false };
              {
                header = "ARCHIVED AT";
                align = Left;
                min_width = 10;
                flex = false;
              };
              { header = "MSGS"; align = Right; min_width = 4; flex = false };
              { header = "EPOCHS"; align = Right; min_width = 6; flex = false };
              { header = "RANGE"; align = Left; min_width = 10; flex = true };
            ]
        in
        let data =
          List.map
            (fun (info : Memory.session_archive_info) ->
              let range =
                match (info.first_message_at, info.last_message_at) with
                | Some first, Some last -> Printf.sprintf "%s..%s" first last
                | Some first, None -> first
                | _ -> "-"
              in
              [
                Printf.sprintf "#%d" info.archive_id;
                info.archived_at;
                string_of_int info.message_count;
                string_of_int info.epoch_count;
                range;
              ])
            rows
        in
        Table_format.render columns data
  | "archive" :: "show" :: id_str :: rest
  | "archives" :: "show" :: id_str :: rest -> (
      match int_of_string_opt id_str with
      | None ->
          Printf.sprintf
            "Error: archive ID must be an integer. Received: %S. Use 'session \
             archives SESSION' to list archive IDs."
            id_str
      | Some archive_id -> (
          match Memory.get_archive_info ~db ~archive_id with
          | None ->
              Printf.sprintf
                "Error: archive #%d not found. Use 'session archives SESSION' \
                 to list available archives."
                archive_id
          | Some info -> (
              match parse_session_show_args rest with
              | Error msg -> msg
              | Ok parsed ->
                  let all_rows = Memory.load_archive_messages ~db ~archive_id in
                  let total_messages = List.length all_rows in
                  let offset = parsed.offset in
                  let paged_rows =
                    let after_offset =
                      if offset > 0 then
                        let rec drop n lst =
                          if n <= 0 then lst
                          else
                            match lst with
                            | _ :: tl -> drop (n - 1) tl
                            | [] -> []
                        in
                        drop offset all_rows
                      else all_rows
                    in
                    match parsed.limit with
                    | Some limit ->
                        let rec take n acc = function
                          | _ when n <= 0 -> List.rev acc
                          | [] -> List.rev acc
                          | hd :: tl -> take (n - 1) (hd :: acc) tl
                        in
                        take limit [] after_offset
                    | None -> after_offset
                  in
                  let shown_count = List.length paged_rows in
                  let has_more = offset + shown_count < total_messages in
                  let paging_fields =
                    [ ("total_messages", `Int total_messages) ]
                    @ (if offset > 0 then [ ("offset", `Int offset) ] else [])
                    @ (match parsed.limit with
                      | Some n -> [ ("limit", `Int n) ]
                      | None -> [])
                    @ [ ("has_more", `Bool has_more) ]
                    @
                    if has_more then
                      [ ("next_offset", `Int (offset + shown_count)) ]
                    else []
                  in
                  let config = get_config () in
                  Yojson.Safe.pretty_to_string
                    (`Assoc
                       ([
                          ("archive_id", `Int info.archive_id);
                          ("session_key", `String info.session_key);
                          ("archived_at", `String info.archived_at);
                          ("epoch_count", `Int info.epoch_count);
                        ]
                       @ paging_fields
                       @ [
                           ( "messages",
                             `List
                               (List.mapi
                                  (fun i row ->
                                    raw_message_json config (offset + i) row)
                                  paged_rows) );
                         ])))))
  | [ "epochs"; session_key ] ->
      let epochs = Memory.list_session_epochs ~db ~session_key in
      if
        List.for_all
          (fun (e : Memory.session_epoch) -> e.message_count = 0)
          epochs
      then Printf.sprintf "No chat log found for session %s" session_key
      else
        Yojson.Safe.pretty_to_string
          (`Assoc
             [
               ("session_key", `String session_key);
               ("epochs", `List (List.map session_epoch_json epochs));
             ])
  | "show" :: session_key :: rest -> (
      match parse_session_show_args rest with
      | Error msg -> msg
      | Ok parsed -> (
          let epoch =
            match parsed.epoch with
            | Some value -> value
            | None -> Memory.Current
          in
          let epoch_label =
            match epoch with
            | Memory.Current -> `String "current"
            | Memory.Archived id -> `Int id
          in
          match Memory.load_epoch_messages ~db ~session_key ~epoch with
          | None -> Printf.sprintf "No epoch matched for session %s" session_key
          | Some rows ->
              let config = get_config () in
              let epochs = Memory.list_session_epochs ~db ~session_key in
              let archived =
                List.filter
                  (fun (e : Memory.session_epoch) -> not e.current)
                  epochs
              in
              let archived_epoch_count = List.length archived in
              let total_archived_messages =
                List.fold_left
                  (fun acc (e : Memory.session_epoch) -> acc + e.message_count)
                  0 archived
              in
              let total_messages = List.length rows in
              let offset = parsed.offset in
              let paged_rows =
                let after_offset =
                  if offset > 0 then
                    let rec drop n lst =
                      if n <= 0 then lst
                      else
                        match lst with _ :: tl -> drop (n - 1) tl | [] -> []
                    in
                    drop offset rows
                  else rows
                in
                match parsed.limit with
                | Some limit ->
                    let rec take n acc = function
                      | _ when n <= 0 -> List.rev acc
                      | [] -> List.rev acc
                      | hd :: tl -> take (n - 1) (hd :: acc) tl
                    in
                    take limit [] after_offset
                | None -> after_offset
              in
              let shown_count = List.length paged_rows in
              let has_more = offset + shown_count < total_messages in
              let paging_fields =
                [ ("total_messages", `Int total_messages) ]
                @ (if offset > 0 then [ ("offset", `Int offset) ] else [])
                @ (match parsed.limit with
                  | Some n -> [ ("limit", `Int n) ]
                  | None -> [])
                @ [ ("has_more", `Bool has_more) ]
                @
                if has_more then
                  [ ("next_offset", `Int (offset + shown_count)) ]
                else []
              in
              Yojson.Safe.pretty_to_string
                (`Assoc
                   ([
                      ("session_key", `String session_key);
                      ("epoch", epoch_label);
                      ( "system_prompt",
                        `String (session_show_system_prompt config) );
                      ("archived_epoch_count", `Int archived_epoch_count);
                      ("total_archived_messages", `Int total_archived_messages);
                    ]
                   @ paging_fields
                   @ [
                       ( "messages",
                         `List
                           (List.mapi
                              (fun i row ->
                                raw_message_json config (offset + i) row)
                              paged_rows) );
                     ]))))
  | [ "pending"; session_key ] ->
      let rows = Memory.queue_list ~db ~session_key in
      if rows = [] then
        Printf.sprintf "No pending inbound rows for session %s" session_key
      else
        Yojson.Safe.pretty_to_string
          (`Assoc
             [
               ("session_key", `String session_key);
               ("pending_count", `Int (List.length rows));
               ( "rows",
                 `List
                   (List.map
                      (fun (r : Memory.queue_row) ->
                        let payload_preview =
                          try
                            let json = Yojson.Safe.from_string r.payload_json in
                            let open Yojson.Safe.Util in
                            let msg =
                              json |> member "message" |> to_string_option
                            in
                            let bang =
                              json |> member "bang" |> to_bool_option
                            in
                            let preview =
                              match msg with
                              | Some s ->
                                  if String.length s > 80 then
                                    String.sub s 0 80 ^ "..."
                                  else s
                              | None -> "(no message field)"
                            in
                            let bang_str =
                              match bang with Some true -> " [bang]" | _ -> ""
                            in
                            preview ^ bang_str
                          with _ -> "(malformed payload)"
                        in
                        `Assoc
                          [
                            ("queue_id", `Int r.queue_id);
                            ( "state",
                              `String (Memory.queue_state_to_string r.state) );
                            ("attempt_count", `Int r.attempt_count);
                            ( "last_error",
                              match r.last_error with
                              | Some e -> `String e
                              | None -> `Null );
                            ("preview", `String payload_preview);
                            ("created_at", `String r.created_at);
                          ])
                      rows) );
             ])
  | "events" :: session_key :: rest -> (
      match parse_session_events_args rest with
      | Error msg -> msg
      | Ok parsed -> (
          let epoch =
            match parsed.ev_epoch with Some e -> e | None -> Memory.Current
          in
          let epoch_label =
            match epoch with
            | Memory.Current -> `String "current"
            | Memory.Archived id -> `Int id
          in
          match Memory.load_epoch_messages ~db ~session_key ~epoch with
          | None -> Printf.sprintf "No epoch found for session %s" session_key
          | Some rows ->
              let pending_count = Memory.queue_count ~db ~session_key in
              let event_rows = List.filter is_session_event_row rows in
              let filtered_rows =
                match parsed.ev_type with
                | None -> event_rows
                | Some wanted ->
                    List.filter
                      (fun row -> classify_event_message row = wanted)
                      event_rows
              in
              let preview_len = 200 in
              let content_preview s =
                if String.length s <= preview_len then s
                else String.sub s 0 preview_len ^ "..."
              in
              Yojson.Safe.pretty_to_string
                (`Assoc
                   [
                     ("session_key", `String session_key);
                     ("epoch", epoch_label);
                     ("pending_inbound_count", `Int pending_count);
                     ("event_count", `Int (List.length filtered_rows));
                     ( "events",
                       `List
                         (List.mapi
                            (fun i (row : Memory.raw_message) ->
                              `Assoc
                                [
                                  ("index", `Int i);
                                  ("id", `Int row.id);
                                  ("role", `String row.role);
                                  ( "event_type",
                                    `String (classify_event_message row) );
                                  ( "content_preview",
                                    `String (content_preview row.content) );
                                  ("created_at", `String row.created_at);
                                ])
                            filtered_rows) );
                   ])))
  | "inject" :: rest -> (
      let cwd, remaining =
        match rest with
        | "--cwd" :: path :: tl -> (Some path, tl)
        | _ -> (None, rest)
      in
      match remaining with
      | session_key :: message_parts -> (
          let session_key = Session.sanitize_session_key session_key in
          let message = String.concat " " message_parts in
          if String.trim message = "" then
            "Usage: clawq session inject [--cwd PATH] SESSION MESSAGE..."
          else
            let cwd_error =
              match cwd with
              | Some path ->
                  if not (Sys.file_exists path) then
                    Some
                      (Printf.sprintf "Error: --cwd path does not exist: %s"
                         path)
                  else if not (Sys.is_directory path) then
                    Some
                      (Printf.sprintf "Error: --cwd path is not a directory: %s"
                         path)
                  else None
              | None -> None
            in
            match cwd_error with
            | Some err -> err
            | None -> (
                match read_live_daemon_gateway () with
                | None ->
                    let is_bang =
                      String.length message > 0 && message.[0] = '!'
                    in
                    let payload_fields =
                      [ ("message", `String message); ("bang", `Bool is_bang) ]
                      @
                      match cwd with
                      | Some c -> [ ("cwd", `String c) ]
                      | None -> []
                    in
                    let payload_json =
                      Yojson.Safe.to_string (`Assoc payload_fields)
                    in
                    let queue_id =
                      Memory.queue_enqueue ~db ~session_key ~source:"cli"
                        ~payload_json
                    in
                    Printf.sprintf
                      "Queued message for session %s (queue_id=%d). No live \
                       daemon detected; startup replay will process it on next \
                       daemon start.%s"
                      session_key queue_id
                      (if is_bang then " (bang interrupt requested)" else "")
                | Some (host, port) -> (
                    let cfg = get_config () in
                    let body =
                      Yojson.Safe.to_string
                        (`Assoc
                           ([
                              ("session_key", `String session_key);
                              ("message", `String message);
                            ]
                           @
                           match cwd with
                           | Some c -> [ ("cwd", `String c) ]
                           | None -> []))
                    in
                    let result =
                      post_live_gateway_json ~cfg ~host ~port
                        ~path:"/session/inject" ~body
                    in
                    match result with
                    | Error msg ->
                        Printf.sprintf "Session inject failed: %s" msg
                    | Ok (status, resp_body) -> (
                        match status with
                        | 200 -> (
                            try
                              let json = Yojson.Safe.from_string resp_body in
                              let open Yojson.Safe.Util in
                              let queued = json |> member "queued" |> to_bool in
                              let response =
                                json |> member "response" |> to_string
                              in
                              if queued then
                                Printf.sprintf
                                  "Queued injected message for busy session \
                                   %s%s"
                                  session_key
                                  (if
                                     String.length message > 0
                                     && message.[0] = '!'
                                   then " (bang interrupt requested)"
                                   else "")
                              else
                                Printf.sprintf
                                  "Processed injected message for session %s\n\
                                   %s"
                                  session_key response
                            with _ ->
                              Printf.sprintf
                                "Session inject succeeded for %s but returned \
                                 an unexpected response: %s"
                                session_key resp_body)
                        | 401 | 403 ->
                            Printf.sprintf
                              "Session inject was rejected by the live gateway \
                               (%d): %s"
                              status
                              (match parse_json_error_body resp_body with
                              | Some msg -> msg
                              | None -> resp_body)
                        | _ ->
                            Printf.sprintf "Session inject failed (%d): %s"
                              status
                              (match parse_json_error_body resp_body with
                              | Some msg -> msg
                              | None -> resp_body)))))
      | _ -> "Usage: clawq session inject [--cwd PATH] SESSION MESSAGE...")
  | [ "compact"; session_key ] -> (
      match read_live_daemon_gateway () with
      | None -> "Error: no live daemon detected. Start `clawq agent` first."
      | Some (host, port) -> (
          let cfg = get_config () in
          let body =
            Yojson.Safe.to_string
              (`Assoc [ ("session_key", `String session_key) ])
          in
          let result =
            post_live_gateway_json ~cfg ~host ~port ~path:"/session/compact"
              ~body
          in
          match result with
          | Error msg -> Printf.sprintf "Session compact failed: %s" msg
          | Ok (status, resp_body) -> (
              match status with
              | 200 -> (
                  try
                    let json = Yojson.Safe.from_string resp_body in
                    let open Yojson.Safe.Util in
                    let compacted = json |> member "compacted" |> to_bool in
                    let message = json |> member "message" |> to_string in
                    let stats_str =
                      try
                        let stats = json |> member "stats" in
                        let percent =
                          stats |> member "context_usage_percent" |> to_int
                        in
                        let tokens =
                          stats |> member "estimated_tokens" |> to_int
                        in
                        let window =
                          stats |> member "context_window" |> to_int
                        in
                        Printf.sprintf " (Context usage: %d%% = %d/%d tokens)"
                          percent tokens window
                      with _ -> ""
                    in
                    if compacted then
                      Printf.sprintf "Session %s compacted successfully.\n%s%s"
                        session_key message stats_str
                    else
                      Printf.sprintf "Session %s: %s%s" session_key message
                        stats_str
                  with _ ->
                    Printf.sprintf
                      "Session compact succeeded for %s but returned an \
                       unexpected response: %s"
                      session_key resp_body)
              | 400 ->
                  Printf.sprintf "Session compact request invalid (400): %s"
                    (match parse_json_error_body resp_body with
                    | Some msg -> msg
                    | None -> resp_body)
              | 404 -> Printf.sprintf "Session '%s' not found (404)" session_key
              | 401 | 403 ->
                  Printf.sprintf
                    "Session compact was rejected by the live gateway (%d): %s"
                    status
                    (match parse_json_error_body resp_body with
                    | Some msg -> msg
                    | None -> resp_body)
              | _ ->
                  Printf.sprintf "Session compact failed (%d): %s" status
                    (match parse_json_error_body resp_body with
                    | Some msg -> msg
                    | None -> resp_body))))
  | "keepalive" :: session_key :: rest -> (
      match rest with
      | [] | [ "status" ] ->
          let infos =
            Memory.list_session_infos ~db ~prefix:session_key
              ~activity:Memory.Any ()
          in
          let enabled =
            match
              List.find_opt
                (fun (r : Memory.session_info) -> r.session_key = session_key)
                infos
            with
            | Some r -> r.keepalive_enabled
            | None -> false
          in
          Printf.sprintf "Session %s: keepalive = %s" session_key
            (if enabled then "on" else "off")
      | [ "on" ] ->
          Memory.set_session_keepalive ~db ~session_key ~enabled:true;
          Printf.sprintf "Keepalive enabled for session %s" session_key
      | [ "off" ] ->
          Memory.set_session_keepalive ~db ~session_key ~enabled:false;
          Printf.sprintf "Keepalive disabled for session %s" session_key
      | _ -> "Usage: clawq session keepalive SESSION [on|off|status]")
  | [ "keepalive" ] -> "Usage: clawq session keepalive SESSION [on|off|status]"
  | "heartbeat" :: session_key :: rest -> (
      match rest with
      | [] | [ "status" ] -> heartbeat_status_text ~db ~config ~session_key
      | [ "on" ] -> (
          match set_heartbeat_for_session ~db ~session_key ~enabled:true with
          | Ok () ->
              Printf.sprintf "Heartbeat enabled for session %s" session_key
          | Error err -> err)
      | [ "off" ] -> (
          match set_heartbeat_for_session ~db ~session_key ~enabled:false with
          | Ok () ->
              Printf.sprintf "Heartbeat disabled for session %s" session_key
          | Error err -> err)
      | _ -> "Usage: clawq session heartbeat SESSION [on|off|status]")
  | [ "heartbeat" ] -> "Usage: clawq session heartbeat SESSION [on|off|status]"
  | "model" :: session_key :: rest -> (
      let parse_set_args args =
        let skip =
          List.exists (fun a -> a = "--skip-validation" || a = "--no-test") args
        in
        let positional =
          List.filter
            (fun a -> a <> "--skip-validation" && a <> "--no-test")
            args
        in
        (skip, positional)
      in
      match rest with
      | [] | [ "get" ] | [ "status" ] -> (
          let model = Memory.get_session_model_override ~db ~session_key in
          match model with
          | Some m ->
              Printf.sprintf "Session %s: model override = %s" session_key m
          | None ->
              Printf.sprintf
                "Session %s: no model override (using global default: %s)"
                session_key config.agent_defaults.primary_model)
      | "set" :: set_args -> (
          let skip_validation, positional = parse_set_args set_args in
          match positional with
          | [ raw_model ] -> (
              let model = Models_catalog.resolve_alias_or_name raw_model in
              let provider, model_id, fmt = Models_catalog.split_name model in
              let canonical, hint =
                match fmt with
                | Models_catalog.Legacy ->
                    let canonical_id =
                      Option.value ~default:model_id
                        (Models_catalog.canonical_id ~provider model_id)
                    in
                    let c = provider ^ ":" ^ canonical_id in
                    ( c,
                      Printf.sprintf
                        "\nNote: normalized to canonical format \"%s\"." c )
                | Models_catalog.Canonical -> (
                    match Models_catalog.canonical_id ~provider model_id with
                    | Some canonical_id ->
                        let c = provider ^ ":" ^ canonical_id in
                        ( c,
                          Printf.sprintf
                            "\nNote: corrected model casing \"%s\" -> \"%s\"."
                            model c )
                    | None -> (model, ""))
                | Models_catalog.Plain -> (model, "")
              in
              let previous_override =
                Memory.get_session_model_override ~db ~session_key
              in
              let rollback_cmd =
                match previous_override with
                | Some prev ->
                    Printf.sprintf "clawq session model %s set %s" session_key
                      prev
                | None ->
                    Printf.sprintf "clawq session model %s clear" session_key
              in
              let previous_label =
                match previous_override with
                | Some prev -> Printf.sprintf "override=%s" prev
                | None ->
                    Printf.sprintf "no override (default: %s)"
                      config.agent_defaults.primary_model
              in
              let rollback_banner =
                Printf.sprintf
                  "Current session model: %s\n\
                   Rollback command if needed:\n\
                  \  %s\n"
                  previous_label rollback_cmd
              in
              let commit () =
                Memory.set_session_model_override ~db ~session_key
                  ~model:canonical;
                Printf.sprintf "%sModel override set for session %s: %s%s"
                  rollback_banner session_key canonical hint
              in
              if skip_validation then
                commit () ^ "\nNote: validation skipped (--skip-validation)."
              else
                let result =
                  Model_validation.validate_sync ~config ~model:canonical ()
                in
                match result with
                | Model_validation.Ok_validated -> commit ()
                | Model_validation.Error_msg msg ->
                    rollback_banner
                    ^ Model_validation.format_failure ~rollback_cmd msg)
          | _ ->
              "Usage: clawq session model SESSION set MODEL [--skip-validation]"
          )
      | [ "clear" ] ->
          Memory.clear_session_model_override ~db ~session_key;
          Printf.sprintf
            "Model override cleared for session %s (will use global default: \
             %s)"
            session_key config.agent_defaults.primary_model
      | _ ->
          "Usage: clawq session model SESSION [get|set MODEL \
           [--skip-validation]|clear]")
  | [ "model" ] -> "Usage: clawq session model SESSION [get|set MODEL|clear]"
  | "postmortems" :: rest ->
      let session_key, limit =
        let rec parse_args args sk lim =
          match args with
          | [] -> (sk, lim)
          | [ "--limit"; n ] -> (
              match int_of_string_opt n with
              | Some v -> (sk, v)
              | None -> (sk, lim))
          | "--limit" :: n :: tail -> (
              match int_of_string_opt n with
              | Some v -> parse_args tail sk v
              | None -> parse_args tail sk lim)
          | key :: tail when not (String.length key > 0 && key.[0] = '-') ->
              parse_args tail (Some key) lim
          | _ :: tail -> parse_args tail sk lim
        in
        parse_args rest None 20
      in
      let rows = Memory.list_postmortems ~db ?session_key ~limit () in
      if rows = [] then "No postmortems found."
      else
        let buf = Buffer.create 512 in
        List.iter
          (fun (p : Memory.postmortem) ->
            Buffer.add_string buf
              (Printf.sprintf
                 "postmortem id: %d\n\
                  session: %s\n\
                  created_at: %s\n\
                  pattern: %s\n\
                  correction: %s\n\
                  outcome: %s\n\
                  doc: %s\n\
                  ---\n"
                 p.id p.session_key p.created_at p.pattern p.correction_injected
                 (match p.outcome with Some s -> s | None -> "(pending)")
                 p.doc_path))
          rows;
        let s = Buffer.contents buf in
        (* trim trailing separator *)
        let len = String.length s in
        if len >= 4 && String.sub s (len - 4) 4 = "\n---\n" then
          String.sub s 0 (len - 4)
        else s
  | _ ->
      "Usage: clawq session <subcommand>\n\
      \  session list [--channel NAME] [--prefix PREFIX] [--active|--inactive] \
       [--main|--non-main]\n\
      \  session archives [SESSION]\n\
      \  session archive show ARCHIVE_ID [--offset N] [--limit N]\n\
      \  session epochs SESSION\n\
      \  session show SESSION [--epoch current|ID] [--offset N] [--limit N]\n\
      \  session pending SESSION\n\
      \  session events SESSION [--epoch current|ID] [--type TYPE]\n\
      \  session inject [--cwd PATH] SESSION MESSAGE...\n\
      \  session compact SESSION\n\
      \  session keepalive SESSION [on|off|status]\n\
      \  session heartbeat SESSION [on|off|status]\n\
      \  session model SESSION [get|set MODEL|clear]\n\
      \  session postmortems [SESSION] [--limit N]"

type background_add_args = {
  runner : Background_task.runner;
  model : string option;
  repo_path : string;
  branch : string option;
  agent_name : string option;
  prompt : string;
}

type background_wait_args = { id : int; timeout_seconds : float }

type background_logs_args = {
  id : int;
  lines : int;
  offset : int;
  follow : bool;
}

type delegate_args = {
  preferred_runner : Background_task.runner option;
  model : string option;
  repo_path : string option;
  branch : string option;
  goal : string;
  use_worktree : bool;
}

let path_is_git_repo path =
  Sys.command
    (Printf.sprintf "git -C %s rev-parse --is-inside-work-tree >/dev/null 2>&1"
       (Filename.quote path))
  = 0

let default_delegate_repo_path (cfg : Runtime_config.t) =
  let cwd = Sys.getcwd () in
  if path_is_git_repo cwd then cwd else Runtime_config.effective_workspace cfg

let parse_background_add_args args =
  let rec loop model branch agent_name positionals = function
    | [] -> (
        let positionals = List.rev positionals in
        match positionals with
        | runner_s :: repo_path :: prompt_parts -> (
            match Background_task.runner_of_string runner_s with
            | None ->
                Error
                  "Runner must be one of: codex, claude (or claude-code), \
                   kimi, gemini, opencode, cursor (or cursor-cli)"
            | Some runner ->
                let prompt = String.concat " " prompt_parts |> String.trim in
                if prompt = "" then Error "Prompt is required"
                else Ok { runner; model; repo_path; branch; agent_name; prompt }
            )
        | _ ->
            Error
              "Usage: clawq background add \
               <codex|claude|kimi|gemini|opencode|cursor> [--model <name>] \
               [--agent <name>] <repo> [--branch <name>] <prompt>")
    | "--model" :: value :: rest ->
        loop (Some value) branch agent_name positionals rest
    | "--branch" :: value :: rest ->
        loop model (Some value) agent_name positionals rest
    | "--agent" :: value :: rest ->
        loop model branch (Some value) positionals rest
    | arg :: rest -> loop model branch agent_name (arg :: positionals) rest
  in
  loop None None None [] args

let parse_background_wait_args args =
  let rec loop timeout id = function
    | [] -> (
        match id with
        | Some id -> Ok { id; timeout_seconds = timeout }
        | None ->
            Error "Usage: clawq background wait <id> [--timeout <seconds>]")
    | "--timeout" :: seconds :: rest -> (
        try loop (float_of_string seconds) id rest
        with _ -> Error "Timeout must be a number")
    | arg :: rest -> (
        match id with
        | Some _ ->
            Error "Usage: clawq background wait <id> [--timeout <seconds>]"
        | None -> (
            try loop timeout (Some (int_of_string arg)) rest
            with _ -> Error "Background task id must be an integer"))
  in
  loop 180.0 None args

let parse_background_logs_args args =
  let usage =
    "Usage: clawq background logs <id> [--lines <count>] [--offset <line>] \
     [--follow]"
  in
  let rec loop lines offset follow id = function
    | [] -> (
        match id with
        | Some id -> Ok { id; lines; offset; follow }
        | None -> Error usage)
    | "--lines" :: count :: rest -> (
        try loop (max 1 (int_of_string count)) offset follow id rest
        with _ -> Error "Log line count must be an integer")
    | "--offset" :: off :: rest -> (
        try loop lines (max 1 (int_of_string off)) follow id rest
        with _ -> Error "Offset must be a positive integer")
    | ("--follow" | "-f") :: rest -> loop lines offset true id rest
    | arg :: rest -> (
        match id with
        | Some _ -> Error usage
        | None -> (
            try loop lines offset follow (Some (int_of_string arg)) rest
            with _ -> Error "Background task id must be an integer"))
  in
  loop 40 0 false None args

let parse_delegate_args args =
  let rec loop preferred_runner model repo_path branch use_worktree positionals
      = function
    | [] ->
        let goal = String.concat " " (List.rev positionals) |> String.trim in
        if goal = "" then
          Error
            "Usage: clawq delegate [--runner \
             auto|kimi|opencode|codex|claude|gemini|cursor] [--model <name>] \
             [--repo <path>] [--branch <name>] [--no-worktree] <goal>"
        else
          Ok { preferred_runner; model; repo_path; branch; goal; use_worktree }
    | "--runner" :: value :: rest ->
        let value = String.lowercase_ascii (String.trim value) in
        let preferred_runner =
          if value = "" || value = "auto" then None
          else Background_task.runner_of_string value
        in
        if value <> "auto" && preferred_runner = None then
          Error
            "Runner must be one of: auto, codex, claude, kimi, gemini, \
             opencode, cursor"
        else
          loop preferred_runner model repo_path branch use_worktree positionals
            rest
    | "--model" :: value :: rest ->
        loop preferred_runner (Some value) repo_path branch use_worktree
          positionals rest
    | "--repo" :: value :: rest ->
        loop preferred_runner model (Some value) branch use_worktree positionals
          rest
    | "--branch" :: value :: rest ->
        loop preferred_runner model repo_path (Some value) use_worktree
          positionals rest
    | "--no-worktree" :: rest ->
        (* B649: opt out of git-worktree isolation so delegate accepts non-git
           paths (e.g. plain ~/.clawq/workspace runs). *)
        loop preferred_runner model repo_path branch false positionals rest
    | arg :: rest ->
        loop preferred_runner model repo_path branch use_worktree
          (arg :: positionals) rest
  in
  loop None None None None true [] args

type plan_start_args = {
  plan_prompt : string;
  plan_repo : string option;
  plan_runner : Background_task.runner option;
  plan_planner_model : string option;
  plan_reviewer_model : string option;
  plan_coder_model : string option;
  plan_max_plan_review_iters : int;
  plan_max_code_review_iters : int;
}

let parse_plan_start_args args =
  let rec loop prompt_parts repo runner planner_model reviewer_model coder_model
      max_plan_review max_code_review = function
    | [] ->
        let prompt = String.concat " " (List.rev prompt_parts) |> String.trim in
        if prompt = "" then
          Error
            "Usage: clawq plan start <PROMPT> [--repo PATH] [--runner NAME] \
             [--planner-model M] [--reviewer-model M] [--coder-model M] \
             [--max-plan-review-iters N] [--max-code-review-iters N] \
             [--no-plan-review] [--no-code-review]"
        else
          Ok
            {
              plan_prompt = prompt;
              plan_repo = repo;
              plan_runner = runner;
              plan_planner_model = planner_model;
              plan_reviewer_model = reviewer_model;
              plan_coder_model = coder_model;
              plan_max_plan_review_iters = max_plan_review;
              plan_max_code_review_iters = max_code_review;
            }
    | "--repo" :: v :: rest ->
        loop prompt_parts (Some v) runner planner_model reviewer_model
          coder_model max_plan_review max_code_review rest
    | "--runner" :: v :: rest ->
        let r = Background_task.runner_of_string v in
        loop prompt_parts repo r planner_model reviewer_model coder_model
          max_plan_review max_code_review rest
    | "--planner-model" :: v :: rest ->
        loop prompt_parts repo runner (Some v) reviewer_model coder_model
          max_plan_review max_code_review rest
    | "--reviewer-model" :: v :: rest ->
        loop prompt_parts repo runner planner_model (Some v) coder_model
          max_plan_review max_code_review rest
    | "--coder-model" :: v :: rest ->
        loop prompt_parts repo runner planner_model reviewer_model (Some v)
          max_plan_review max_code_review rest
    | "--max-plan-review-iters" :: v :: rest -> (
        try
          loop prompt_parts repo runner planner_model reviewer_model coder_model
            (int_of_string v) max_code_review rest
        with _ -> Error "--max-plan-review-iters requires an integer value")
    | "--max-code-review-iters" :: v :: rest -> (
        try
          loop prompt_parts repo runner planner_model reviewer_model coder_model
            max_plan_review (int_of_string v) rest
        with _ -> Error "--max-code-review-iters requires an integer value")
    | "--no-plan-review" :: rest ->
        loop prompt_parts repo runner planner_model reviewer_model coder_model 0
          max_code_review rest
    | "--no-code-review" :: rest ->
        loop prompt_parts repo runner planner_model reviewer_model coder_model
          max_plan_review 0 rest
    | arg :: rest ->
        loop (arg :: prompt_parts) repo runner planner_model reviewer_model
          coder_model max_plan_review max_code_review rest
  in
  loop [] None None None None None 3 3 args

let cmd_plan args =
  let cfg = get_config () in
  let db = get_db () in
  Plan_pipeline.init_schema db;
  Background_task.init_schema db;
  match args with
  | [] | [ "list" ] ->
      let pipelines = Plan_pipeline.list_pipelines ~db in
      Plan_pipeline.format_pipeline_list pipelines
      ^ "\n\n\
         Commands:\n\
        \  plan start <PROMPT> [--repo PATH] [--runner NAME]   - Start pipeline\n\
        \  plan list                                           - List pipelines\n\
        \  plan show <id>                                      - Show pipeline \
         details\n\
        \  plan logs <id> [--lines N]                          - Show stage logs\n\
        \  plan cancel <id>                                    - Cancel \
         pipeline"
  | "start" :: rest -> (
      match parse_plan_start_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed -> (
          let repo_path =
            match parsed.plan_repo with
            | Some p -> p
            | None -> default_delegate_repo_path cfg
          in
          let model_config : Plan_pipeline.model_config =
            {
              Plan_pipeline.planner_model = parsed.plan_planner_model;
              reviewer_model = parsed.plan_reviewer_model;
              coder_model = parsed.plan_coder_model;
              max_plan_review_iters = parsed.plan_max_plan_review_iters;
              max_code_review_iters = parsed.plan_max_code_review_iters;
            }
          in
          let runner_result =
            Background_task.resolve_runner ?preferred:parsed.plan_runner ()
          in
          match runner_result with
          | Error msg -> "Error: " ^ msg
          | Ok (runner, _) -> (
              let pipeline =
                Plan_pipeline.create ~db ~prompt:parsed.plan_prompt ~repo_path
                  ~model_config
              in
              Printf.printf
                "Started pipeline %d (stage: planning)\n\
                 Plan file: %s\n\
                 Use `clawq plan show %d` to check progress.\n"
                pipeline.Plan_pipeline.id
                (Plan_pipeline.plan_file_path pipeline)
                pipeline.Plan_pipeline.id;
              flush stdout;
              let result =
                Lwt_main.run
                  (Plan_pipeline.run_foreground ~db ~pipeline ~runner
                     ~on_progress:(fun s ->
                       print_endline s;
                       flush stdout)
                     ())
              in
              ignore result;
              match Plan_pipeline.get_pipeline ~db ~id:pipeline.id with
              | None -> "Pipeline complete."
              | Some p -> Plan_pipeline.format_pipeline_summary p)))
  | [ "show"; id_s ] -> (
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: pipeline id must be an integer"
      else
        match Plan_pipeline.get_pipeline ~db ~id with
        | None -> Printf.sprintf "No pipeline found with id %d" id
        | Some p -> Plan_pipeline.format_pipeline_summary p)
  | "logs" :: rest -> (
      let id, lines =
        let rec loop id lines = function
          | [] -> (id, lines)
          | "--lines" :: n :: rest -> (
              try loop id (int_of_string n) rest with _ -> loop id lines rest)
          | v :: rest -> (
              try loop (Some (int_of_string v)) lines rest
              with _ -> loop id lines rest)
        in
        loop None 50 rest
      in
      match id with
      | None -> "Usage: clawq plan logs <id> [--lines N]"
      | Some id -> (
          match Plan_pipeline.get_pipeline ~db ~id with
          | None -> Printf.sprintf "No pipeline found with id %d" id
          | Some p -> (
              match p.Plan_pipeline.current_bg_task_id with
              | None ->
                  Printf.sprintf
                    "Pipeline %d has no background task (stage: %s)." id
                    (Plan_pipeline.string_of_stage p.stage)
              | Some task_id -> (
                  match Background_task.get_task ~db ~id:task_id with
                  | None ->
                      Printf.sprintf "Background task %d not found." task_id
                  | Some task -> (
                      match
                        Background_task.log_excerpt ~offset:0 ~lines task
                      with
                      | Ok text -> text
                      | Error msg -> "Error: " ^ msg)))))
  | [ "cancel"; id_s ] -> (
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: pipeline id must be an integer"
      else
        match Plan_pipeline.cancel_pipeline ~db ~id with
        | Ok msg -> msg
        | Error msg -> "Error: " ^ msg)
  | _ ->
      "Usage: clawq plan <start|list|show|logs|cancel>\n\
      \  plan start <PROMPT> [--repo PATH] [--runner NAME]\n\
      \            [--planner-model M] [--reviewer-model M] [--coder-model M]\n\
      \            [--max-plan-review-iters N] [--max-code-review-iters N]\n\
      \            [--no-plan-review] [--no-code-review]\n\
      \  plan list                              - List all pipelines\n\
      \  plan show <id>                         - Show pipeline details\n\
      \  plan logs <id> [--lines N]             - Show stage logs\n\
      \  plan cancel <id>                       - Cancel pipeline"

let format_background_task_details (task : Background_task.task) =
  let add line acc = line :: acc in
  let lines = ref [] in
  lines := add (Printf.sprintf "id: %d" task.id) !lines;
  lines :=
    add
      (Printf.sprintf "runner: %s"
         (Background_task.string_of_runner task.runner))
      !lines;
  lines :=
    add
      (Printf.sprintf "status: %s"
         (Background_task.string_of_status task.status))
      !lines;
  let health = Background_task.diagnose_health task in
  (match health with
  | Background_task.Not_applicable -> ()
  | _ ->
      lines :=
        add
          (Printf.sprintf "health: %s"
             (Background_task.string_of_health health))
          !lines);
  lines := add (Printf.sprintf "repo: %s" task.repo_path) !lines;
  lines :=
    add
      (Printf.sprintf "branch: %s"
         (if task.branch = "" then "(auto)" else task.branch))
      !lines;
  lines := add (Printf.sprintf "created_at: %s" task.created_at) !lines;
  (match task.started_at with
  | Some value -> lines := add (Printf.sprintf "started_at: %s" value) !lines
  | None -> ());
  (match task.finished_at with
  | Some value -> lines := add (Printf.sprintf "finished_at: %s" value) !lines
  | None -> ());
  (match task.worktree_path with
  | Some value -> lines := add (Printf.sprintf "worktree: %s" value) !lines
  | None -> ());
  (match task.log_path with
  | Some value -> lines := add (Printf.sprintf "log: %s" value) !lines
  | None -> ());
  (match task.pid with
  | Some value -> lines := add (Printf.sprintf "pid: %d" value) !lines
  | None -> ());
  (match task.result_preview with
  | Some value when String.trim value <> "" ->
      lines := add (Printf.sprintf "result: %s" value) !lines
  | _ -> ());
  lines := add (Printf.sprintf "prompt: %s" task.prompt) !lines;
  String.concat "\n" (List.rev !lines)

let cmd_capabilities () =
  let cfg = get_config () in
  let caps = ref [] in
  let add s = caps := s :: !caps in
  (* Providers *)
  let active_providers =
    List.filter
      (fun (_, p) -> Runtime_config.is_key_set p.Runtime_config.api_key)
      cfg.providers
  in
  add
    (Printf.sprintf "  - LLM chat: %d provider(s) configured (%s)"
       (List.length active_providers)
       (if active_providers = [] then "none active"
        else String.concat ", " (List.map fst active_providers)));
  (* Channels *)
  if cfg.channels.cli then add "  - CLI channel: enabled";
  (match cfg.channels.telegram with
  | Some tg ->
      add
        (Printf.sprintf "  - Telegram channel: %d account(s)"
           (List.length tg.accounts))
  | None -> ());
  (* Gateway *)
  add
    (Printf.sprintf "  - HTTP gateway: %s:%d" cfg.gateway.host cfg.gateway.port);
  (* Memory *)
  add
    (Printf.sprintf "  - Memory: %s (FTS search: %s)" cfg.memory.backend
       (if cfg.memory.search_enabled then "enabled" else "disabled"));
  (* Tools *)
  if cfg.security.tools_enabled then begin
    let registry =
      match build_tool_registry ~db:(Some (get_db ())) cfg with
      | Some registry -> registry
      | None -> assert false
    in
    let tool_names = List.map (fun (t : Tool.t) -> t.name) registry.tools in
    add
      (Printf.sprintf "  - Tools: %d registered (%s)" (List.length tool_names)
         (String.concat ", " tool_names))
  end
  else add "  - Tools: disabled";
  (* MCP *)
  if cfg.mcp.enabled then begin
    let exposed =
      match cfg.mcp.exposed_tools with
      | None -> "all tools"
      | Some names -> String.concat ", " names
    in
    add (Printf.sprintf "  - MCP server: enabled (exposing: %s)" exposed)
  end
  else add "  - MCP server: disabled";
  (* Security *)
  add
    (Printf.sprintf
       "  - Security: workspace_only=%b audit=%b encrypt_secrets=%b"
       cfg.security.workspace_only cfg.security.audit_enabled
       cfg.security.encrypt_secrets);
  (* STT *)
  (match cfg.stt with
  | Some s -> add (Printf.sprintf "  - Voice/STT: %s (%s)" s.provider s.model)
  | None -> ());
  (* Cron *)
  add "  - Cron scheduler: available";
  (* Service management *)
  add "  - Service management: start/stop/restart/status";
  "Available capabilities:\n" ^ String.concat "\n" (List.rev !caps)

let known_auth_providers =
  [
    ("anthropic", "Anthropic Claude (native)");
    ("openai", "OpenAI (native)");
    ("gemini", "Google Gemini (native)");
    ("openai-codex", "OpenAI Codex / ChatGPT (OAuth or key)");
    ("zai_coding", "Z.AI coding endpoint");
    ("zai", "Z.AI general endpoint");
    ("mistral", "Mistral AI");
    ("xai", "xAI / Grok");
    ("groq", "Groq (fast inference)");
    ("deepseek", "DeepSeek");
    ("cohere", "Cohere");
    ("kimi_coding", "Kimi coding subscription");
    ("ollama", "Ollama (local, no key required)");
  ]

let is_known_provider name = List.mem_assoc name known_auth_providers

let provider_not_found_error provider_name =
  let cfg = get_config () in
  let configured_names = List.map fst cfg.providers in
  let extra =
    List.filter (fun n -> not (is_known_provider n)) configured_names
  in
  let all_names = List.map fst known_auth_providers @ extra in
  Printf.sprintf
    "Error: unknown provider '%s'. Valid providers: %s\n\
     Use 'clawq auth providers' to see providers with status."
    provider_name
    (String.concat ", " all_names)

let is_valid_set_key_provider provider_name =
  if is_known_provider provider_name then true
  else
    let cfg = get_config () in
    List.mem_assoc provider_name cfg.providers

let cmd_auth args =
  match args with
  | [ "codex-login" ] | [ "login"; "codex" ] -> (
      match Openai_codex_oauth.login () with
      | Ok creds ->
          Printf.sprintf "Codex login complete%s"
            (match creds.Runtime_config.email with
            | Some email -> Printf.sprintf " for %s" email
            | None -> "")
      | Error msg -> Printf.sprintf "Codex login failed: %s" msg)
  | [ "codex-login"; provider_name ] -> (
      match Openai_codex_oauth.login ~provider_name () with
      | Ok creds ->
          Printf.sprintf "%s: Codex login complete%s" provider_name
            (match creds.Runtime_config.email with
            | Some email -> Printf.sprintf " for %s" email
            | None -> "")
      | Error msg ->
          Printf.sprintf "%s: Codex login failed: %s" provider_name msg)
  | [ "codex-status" ] | [ "status"; "codex" ] -> Openai_codex_oauth.status ()
  | [ "codex-status"; provider_name ] ->
      Openai_codex_oauth.status ~provider_name ()
  | [ "codex-logout" ] | [ "logout"; "codex" ] -> Openai_codex_oauth.logout ()
  | [ "codex-logout"; provider_name ] ->
      Openai_codex_oauth.logout ~provider_name ()
  | [ "set-key"; provider_name; api_key ] -> (
      if not (is_valid_set_key_provider provider_name) then
        provider_not_found_error provider_name
      else
        let key = Printf.sprintf "providers.%s.api_key" provider_name in
        match Config_set.set_json_value key (`String api_key) with
        | Ok () ->
            Printf.sprintf "API key set for provider '%s': %s" provider_name
              (redact_key api_key)
        | Error err -> err)
  | [ "set-key"; provider_name ] -> (
      if not (is_valid_set_key_provider provider_name) then
        provider_not_found_error provider_name
      else
        let prompt =
          Printf.sprintf "Enter API key for provider '%s': " provider_name
        in
        match Tui_input.read_secret prompt with
        | Error msg -> msg
        | Ok api_key -> (
            let key = Printf.sprintf "providers.%s.api_key" provider_name in
            match Config_set.set_json_value key (`String api_key) with
            | Ok () ->
                Printf.sprintf "API key set for provider '%s': %s" provider_name
                  (redact_key api_key)
            | Error err -> err))
  | [ "set-key" ] ->
      "Usage: clawq auth set-key PROVIDER [API_KEY]\n\
       Example: clawq auth set-key anthropic sk-ant-...\n\
       Example: clawq auth set-key zai-coding\n\
       Omit API_KEY to enter it interactively (hidden input)."
  | [ "providers" ] | [ "list-providers" ] ->
      let cfg = get_config () in
      let configured_names = List.map fst cfg.providers in
      let extra =
        List.filter_map
          (fun name ->
            if is_known_provider name then None else Some (name, "configured"))
          configured_names
      in
      let all = known_auth_providers @ extra in
      let columns =
        Table_format.
          [
            { header = "PROVIDER"; align = Left; min_width = 8; flex = false };
            {
              header = "DESCRIPTION";
              align = Left;
              min_width = 10;
              flex = true;
            };
          ]
      in
      let tbl_rows =
        List.map
          (fun (name, desc) ->
            let suffix =
              if List.mem name configured_names then
                let p = List.assoc name cfg.providers in
                if Runtime_config.is_key_set p.api_key then " [key set]"
                else if Runtime_config.provider_has_codex_oauth p then
                  " [oauth]"
                else " [configured]"
              else ""
            in
            [ name; desc ^ suffix ])
          all
      in
      "Known providers (use with 'clawq auth set-key'):\n"
      ^ Table_format.render columns tbl_rows
  | [ "encrypt" ] ->
      if not (get_config ()).security.encrypt_secrets then
        "Secret encryption is disabled. Set security.encrypt_secrets to true \
         in config."
      else begin
        match Secret_store.get_master_key () with
        | Error msg -> Printf.sprintf "Error: %s" msg
        | Ok key ->
            let config_path = Dot_dir.config_path () in
            if not (Sys.file_exists config_path) then
              "No config file found at " ^ config_path
            else begin
              let json =
                try Ok (Yojson.Safe.from_file config_path)
                with exn -> Error exn
              in
              match json with
              | Error exn ->
                  Printf.sprintf "Failed to read config: %s"
                    (Printexc.to_string exn)
              | Ok json -> (
                  match Secret_store.encrypt_config_secrets ~key json with
                  | Error msg -> Printf.sprintf "Error: %s" msg
                  | Ok new_json -> (
                      try
                        let s =
                          Yojson.Safe.pretty_to_string ~std:true new_json
                        in
                        let oc = open_out config_path in
                        output_string oc s;
                        output_char oc '\n';
                        close_out oc;
                        "API keys encrypted in " ^ config_path
                      with exn ->
                        Printf.sprintf "Failed to write config: %s"
                          (Printexc.to_string exn)))
            end
      end
  | "pair" :: rest -> (
      let cfg = get_config () in
      let host = cfg.gateway.host in
      let port = cfg.gateway.port in
      let code =
        match rest with
        | c :: _ -> c
        | [] ->
            print_string "Enter OTP pairing code: ";
            flush stdout;
            input_line stdin
      in
      let url = Printf.sprintf "http://%s:%d/pair" host port in
      let body = `Assoc [ ("code", `String code) ] |> Yojson.Safe.to_string in
      let result =
        Lwt_main.run
          (Lwt.catch
             (fun () ->
               let open Lwt.Syntax in
               let* _status, resp_body =
                 Http_client.post_json ~uri:url ~headers:[] ~body
               in
               Lwt.return (Ok resp_body))
             (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
      in
      match result with
      | Error msg -> Printf.sprintf "Pairing request failed: %s" msg
      | Ok resp_body -> (
          try
            let json = Yojson.Safe.from_string resp_body in
            let open Yojson.Safe.Util in
            match json |> member "token" with
            | `String token ->
                let token_path = gateway_token_path () in
                (try save_gateway_token token
                 with exn ->
                   raise
                     (Failure
                        (Printf.sprintf "Failed to save token: %s"
                           (Printexc.to_string exn))));
                Printf.sprintf
                  "Paired successfully! Token saved to %s\nToken: %s" token_path
                  (redact_key token)
            | _ -> (
                match json |> member "error" with
                | `String err -> Printf.sprintf "Pairing failed: %s" err
                | _ -> Printf.sprintf "Unexpected response: %s" resp_body)
          with exn ->
            Printf.sprintf "Failed to parse response: %s\nBody: %s"
              (Printexc.to_string exn) resp_body))
  | _ ->
      let subcommands_csv =
        "set-key, providers, encrypt, codex-login, codex-status, codex-logout, \
         pair"
      in
      let cfg = get_config () in
      let status =
        match cfg.providers with
        | [] -> "No providers configured. No provider auth set."
        | providers ->
            let lines =
              List.map
                (fun (name, (p : Runtime_config.provider_config)) ->
                  let s =
                    if Runtime_config.is_key_set p.api_key then
                      redact_key p.api_key
                    else if Runtime_config.provider_has_codex_oauth p then
                      "codex-oauth configured"
                    else "not set"
                  in
                  Printf.sprintf "  %s: %s" name s)
                providers
            in
            "Provider auth status:\n" ^ String.concat "\n" lines
      in
      Printf.sprintf "%s\n\nAvailable subcommands: %s" status subcommands_csv

(* ── Structured output pipelines ───────────────────────────────────────── *)

let parse_pipeline_run_inputs rest =
  let rec loop acc = function
    | "--input" :: kv :: rest -> (
        match String.index_opt kv '=' with
        | Some i ->
            let key = String.sub kv 0 i in
            let value = String.sub kv (i + 1) (String.length kv - i - 1) in
            loop ((key, value) :: acc) rest
        | None -> loop acc rest)
    | _ :: rest -> loop acc rest
    | [] -> List.rev acc
  in
  loop [] rest

let cmd_pipeline args =
  let db = get_db () in
  Structured_pipeline.init_schema db;
  match args with
  | [] | [ "list" ] ->
      let pipelines = Structured_pipeline.discover_pipelines () in
      Structured_pipeline.format_pipeline_list pipelines
  | [ "show"; name ] -> (
      match Structured_pipeline.find_pipeline name with
      | None ->
          Printf.sprintf
            "Pipeline \"%s\" not found. Use 'clawq pipeline list' to see \
             available pipelines."
            name
      | Some def -> Structured_pipeline.pipeline_def_to_yaml def)
  | "run" :: name :: rest -> (
      match Structured_pipeline.find_pipeline name with
      | None ->
          Printf.sprintf
            "Pipeline \"%s\" not found. Use 'clawq pipeline list' to see \
             available pipelines."
            name
      | Some pipeline -> (
          let inputs = parse_pipeline_run_inputs rest in
          (* Validate required inputs *)
          let missing =
            List.filter_map
              (fun (key, (def : Structured_pipeline.input_def)) ->
                if
                  def.required
                  && (not (List.mem_assoc key inputs))
                  && def.default = None
                then Some key
                else None)
              pipeline.inputs
          in
          match missing with
          | _ :: _ ->
              Printf.sprintf
                "Missing required input(s): %s\n\
                 Usage: clawq pipeline run %s --input key=value ..."
                (String.concat ", " missing)
                name
          | [] -> (
              let config = get_config () in
              let tool_registry = build_tool_registry ~db:(Some db) config in
              let result =
                Lwt_main.run
                  (Structured_pipeline_run.run_pipeline ~db ~config ~pipeline
                     ~inputs ?tool_registry
                     ~on_progress:(fun s ->
                       print_endline s;
                       flush stdout)
                     ())
              in
              match result.Structured_pipeline.status with
              | Structured_pipeline.Completed ->
                  let outputs =
                    List.map
                      (fun (sr : Structured_pipeline.step_result) ->
                        Printf.sprintf "### %s\n```json\n%s\n```" sr.step_name
                          (Yojson.Safe.pretty_to_string sr.output_json))
                      result.step_results
                  in
                  Printf.sprintf "Pipeline \"%s\" completed (run #%d).\n\n%s"
                    name result.run_id
                    (String.concat "\n\n" outputs)
              | Structured_pipeline.Failed msg ->
                  Printf.sprintf "Pipeline \"%s\" failed (run #%d): %s" name
                    result.run_id msg
              | _ ->
                  Printf.sprintf "Pipeline \"%s\" run #%d status: unexpected"
                    name result.run_id)))
  | [ "validate"; name ] -> (
      match Structured_pipeline.find_pipeline name with
      | None -> Printf.sprintf "Pipeline \"%s\" not found." name
      | Some def -> (
          match Structured_pipeline.validate_pipeline_def def with
          | Ok () ->
              Printf.sprintf "Pipeline \"%s\" is valid (%d steps)." name
                (List.length def.steps)
          | Error errs ->
              Printf.sprintf "Pipeline \"%s\" has validation errors:\n%s" name
                (String.concat "\n" (List.map (fun e -> "  - " ^ e) errs))))
  | [ "create"; name ] -> (
      if not (Structured_pipeline.is_valid_pipeline_name name) then
        "Error: name must be alphanumeric with hyphens/underscores, max 64 \
         chars"
      else
        match Structured_pipeline.scaffold_pipeline ~name () with
        | Ok path ->
            Printf.sprintf
              "Created pipeline scaffold at %s\nEdit it to define your steps."
              path
        | Error msg -> Printf.sprintf "Error: %s" msg)
  | [ "wizard" ] ->
      if not (Unix.isatty Unix.stdin) then
        "Error: wizard requires an interactive terminal. Use 'clawq pipeline \
         create <name>' for non-interactive scaffolding."
      else begin
        Printf.printf "=== Pipeline Wizard ===\n\n";
        Printf.printf "Pipeline name: ";
        flush stdout;
        let name = Tui_input.read_line_clean "" in
        if not (Structured_pipeline.is_valid_pipeline_name name) then
          "Error: invalid pipeline name (alphanumeric, hyphens, underscores \
           only)"
        else begin
          Printf.printf "Description: ";
          flush stdout;
          let description = Tui_input.read_line_clean "" in
          (* Collect inputs *)
          let inputs = ref [] in
          let adding_inputs = ref true in
          while !adding_inputs do
            Printf.printf "\nAdd input parameter? (y/n): ";
            flush stdout;
            let ans = Tui_input.read_line_clean "" in
            if String.lowercase_ascii (String.trim ans) = "y" then begin
              Printf.printf "  Input name: ";
              flush stdout;
              let inp_name = Tui_input.read_line_clean "" in
              Printf.printf "  Description: ";
              flush stdout;
              let inp_desc = Tui_input.read_line_clean "" in
              Printf.printf "  Required? (y/n): ";
              flush stdout;
              let inp_req =
                String.lowercase_ascii
                  (String.trim (Tui_input.read_line_clean ""))
                = "y"
              in
              let inp_default =
                if not inp_req then begin
                  Printf.printf "  Default value (empty for none): ";
                  flush stdout;
                  let d = Tui_input.read_line_clean "" in
                  if d = "" then None else Some d
                end
                else None
              in
              inputs :=
                ( inp_name,
                  Structured_pipeline.
                    {
                      input_type = "string";
                      description = inp_desc;
                      required = inp_req;
                      default = inp_default;
                    } )
                :: !inputs
            end
            else adding_inputs := false
          done;
          (* Collect steps *)
          let steps = ref [] in
          let adding_steps = ref true in
          let step_num = ref 1 in
          while !adding_steps do
            Printf.printf "\nStep %d name (empty to finish): " !step_num;
            flush stdout;
            let sname = Tui_input.read_line_clean "" in
            if String.trim sname = "" then adding_steps := false
            else begin
              Printf.printf "  Step type (p=prompt, a=agent, default p): ";
              flush stdout;
              let stype = Tui_input.read_line_clean "" in
              let kind =
                if String.trim stype = "a" || String.trim stype = "agent" then begin
                  Printf.printf "  Task description (single line): ";
                  flush stdout;
                  let stask = Tui_input.read_line_clean "" in
                  Structured_pipeline.Agent_step
                    { task = stask; model = None; max_turns = None }
                end
                else begin
                  Printf.printf "  Prompt (single line): ";
                  flush stdout;
                  let sprompt = Tui_input.read_line_clean "" in
                  Printf.printf "  Max retries (default 1): ";
                  flush stdout;
                  let retries_s = Tui_input.read_line_clean "" in
                  let max_retries =
                    match int_of_string_opt (String.trim retries_s) with
                    | Some n when n >= 0 -> n
                    | _ -> 1
                  in
                  Structured_pipeline.Prompt_step
                    {
                      prompt = sprompt;
                      system_prompt = None;
                      model = None;
                      output_schema =
                        `Assoc
                          [
                            ("type", `String "object"); ("properties", `Assoc []);
                          ];
                      max_retries;
                    }
                end
              in
              steps := { Structured_pipeline.name = sname; kind } :: !steps;
              incr step_num
            end
          done;
          let def : Structured_pipeline.pipeline_def =
            {
              name;
              version = "1.0";
              description;
              inputs = List.rev !inputs;
              steps = List.rev !steps;
              source_path = "";
            }
          in
          let yaml = Structured_pipeline.pipeline_def_to_yaml def in
          Printf.printf "\n=== Generated Pipeline ===\n%s\n" yaml;
          Printf.printf "Save to ~/.clawq/pipelines/%s.yaml? (y/n): " name;
          flush stdout;
          let save_ans = Tui_input.read_line_clean "" in
          if String.lowercase_ascii (String.trim save_ans) = "y" then begin
            let dir = Structured_pipeline.ensure_pipelines_dir () in
            let path = Filename.concat dir (name ^ ".yaml") in
            let oc = open_out path in
            output_string oc yaml;
            close_out oc;
            Printf.sprintf "Saved pipeline to %s" path
          end
          else "Pipeline not saved."
        end
      end
  | "history" :: rest ->
      let pipeline_name =
        let rec find = function
          | "--pipeline" :: name :: _ -> Some name
          | _ :: rest -> find rest
          | [] -> None
        in
        find rest
      in
      let runs =
        Structured_pipeline.list_runs ~db ?pipeline_name ~limit:20 ()
      in
      Structured_pipeline.format_run_list runs
  | [ "result"; id_s ] -> (
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: run id must be a positive integer"
      else
        match Structured_pipeline.get_run ~db ~run_id:id with
        | None -> Printf.sprintf "Run #%d not found." id
        | Some run ->
            let steps = Structured_pipeline.get_run_steps ~db ~run_id:id in
            Structured_pipeline.format_run_detail ~run ~steps)
  | _ ->
      "Usage: clawq pipeline <subcommand>\n\n\
       Subcommands:\n\
      \  list                          List available pipelines\n\
      \  show <name>                   Show pipeline definition\n\
      \  run <name> [--input k=v ...]  Execute a pipeline\n\
      \  validate <name>               Validate pipeline definition\n\
      \  create <name>                 Scaffold a new pipeline YAML\n\
      \  wizard                        Interactive pipeline builder\n\
      \  history [--pipeline <name>]   List past runs\n\
      \  result <run-id>               Show run results"
