let parse_availability uri =
  match Uri.get_query_param uri "availability" with
  | None -> Ok Models_catalog.Available
  | Some value -> (
      match Models_catalog.availability_filter_of_string value with
      | Some availability -> Ok availability
      | None ->
          Error
            (Printf.sprintf
               "invalid availability filter '%s'; use available, unavailable, \
                or all"
               value))

let provider_filter uri =
  match Uri.get_query_param uri "provider" with
  | Some p -> Some p
  | None -> None

let db_extras ~session_manager ~provider_filter ~availability =
  match Session.get_db session_manager with
  | None -> []
  | Some db ->
      Model_discovery.get_db_only_model_infos ~db ~provider_filter ~availability
        ()

let models_json ~session_manager uri =
  let provider_filter = provider_filter uri in
  match parse_availability uri with
  | Error msg -> Error msg
  | Ok availability ->
      let db_extras =
        db_extras ~session_manager ~provider_filter ~availability
      in
      Ok (Models_catalog.to_json ~provider_filter ~availability ~db_extras ())

let respond_models_json ~session_manager uri =
  match models_json ~session_manager uri with
  | Error msg ->
      Http_server_0_util.json_string_response ~status:`Bad_request
        (Yojson.Safe.to_string (`Assoc [ ("error", `String msg) ]))
  | Ok json ->
      Http_server_0_util.json_string_response (Yojson.Safe.to_string json)

let model_list_text ~session_manager ~provider ~availability =
  let db_extras =
    db_extras ~session_manager ~provider_filter:provider ~availability
  in
  let models =
    Models_catalog.to_plain_list ~provider_filter:provider ~availability
      ~db_extras ()
    |> String.split_on_char '\n'
    |> List.filter (fun s -> s <> "")
  in
  Slash_commands.format_model_list ~connector:Format_adapter.Plain ~models
    ~provider
