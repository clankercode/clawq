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

let rec cmd_session args =
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
  | "send" :: rest -> cmd_session ("inject" :: rest)
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
              let configured_providers = List.map fst config.providers in
              match
                Models_catalog.resolve_model_name_for_set
                  ~require_configured_provider:false ~configured_providers
                  raw_model
              with
              | Error err ->
                  if
                    String.length err >= 6
                    && String.lowercase_ascii (String.sub err 0 6) = "error:"
                  then err
                  else "Error: " ^ err
              | Ok resolved -> (
                  let canonical = resolved.Models_catalog.canonical_value in
                  let hint = resolved.Models_catalog.hint in
                  let previous_override =
                    Memory.get_session_model_override ~db ~session_key
                  in
                  let rollback_cmd =
                    match previous_override with
                    | Some prev ->
                        Printf.sprintf "clawq session model %s set %s"
                          session_key prev
                    | None ->
                        Printf.sprintf "clawq session model %s clear"
                          session_key
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
                  match
                    try
                      Model_discovery.validate_cached_model_allowed ~db
                        canonical
                    with _ -> None
                  with
                  | Some msg ->
                      if
                        String.length msg >= 6
                        && String.lowercase_ascii (String.sub msg 0 6)
                           = "error:"
                      then msg
                      else "Error: " ^ msg
                  | None -> (
                      if skip_validation then
                        commit ()
                        ^ "\nNote: validation skipped (--skip-validation)."
                      else
                        let result =
                          Model_validation.validate_sync ~config
                            ~model:canonical ()
                        in
                        match result with
                        | Model_validation.Ok_validated -> commit ()
                        | Model_validation.Error_msg msg ->
                            rollback_banner
                            ^ Model_validation.format_failure ~rollback_cmd msg)
                  ))
          | _ ->
              "Usage: clawq session model SESSION set MODEL [--skip-validation]\n\
               MODEL uses provider:model (e.g. anthropic:claude-sonnet-4-6).\n\
               Bare names resolve when unique; ambiguous names list candidates."
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
      \  session send [--cwd PATH] SESSION MESSAGE...\n\
      \  session compact SESSION\n\
      \  session keepalive SESSION [on|off|status]\n\
      \  session heartbeat SESSION [on|off|status]\n\
      \  session model SESSION [get|set MODEL|clear]\n\
      \  session postmortems [SESSION] [--limit N]"

(* Background-task arg parsing + delegate/plan helpers + task detail formatting *)
include Command_bridge_bgargs

(* Plan pipeline CLI (clawq plan ...) *)
include Command_bridge_plan

(* Capabilities + provider auth CLI (clawq auth ...) *)
include Command_bridge_auth

(* Structured output pipeline CLI (clawq pipeline ...) *)
include Command_bridge_pipeline
