let skip_kinds =
  [
    "anthropic";
    "gemini";
    "deepseek";
    "cohere";
    "mistral";
    "kimi";
    "kimi_coding";
    "zai";
    "zai_coding";
    "minimax";
    "mimo";
    "openai-codex";
  ]

let default_base_url_for_kind = function
  | "groq" -> "https://api.groq.com/openai/v1"
  | "openrouter" -> "https://openrouter.ai/api/v1"
  | "ollama" -> "http://localhost:11434"
  | _ -> "https://api.openai.com/v1"

let should_skip_provider (pc : Runtime_config.provider_config) =
  match pc.kind with Some k -> List.mem k skip_kinds | None -> pc.api_key = ""

let is_ollama (pc : Runtime_config.provider_config) =
  match pc.kind with Some "ollama" -> true | _ -> false

let get_base_url (pc : Runtime_config.provider_config) =
  match pc.base_url with
  | Some u -> u
  | None -> (
      match pc.kind with
      | Some k -> default_base_url_for_kind k
      | None -> "https://api.openai.com/v1")

let check_ttl_hours ~db ~provider ~hours =
  let sql =
    "SELECT MAX(fetched_at) FROM models_cache WHERE provider = ? AND \
     fetched_at > datetime('now', ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT provider));
      ignore
        (Sqlite3.bind stmt 2
           (Sqlite3.Data.TEXT (Printf.sprintf "-%d hours" hours)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.NULL -> false
          | Sqlite3.Data.TEXT s when s = "" -> false
          | _ -> true)
      | _ -> false)

(* Returns true if a discovery attempt (success or failure) was recorded for
   this provider within the given number of hours. *)
let check_attempt_ttl ~db ~provider ~hours =
  let sql =
    "SELECT last_attempted_at FROM model_discovery_state WHERE provider = ? \
     AND last_attempted_at > datetime('now', ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT provider));
      ignore
        (Sqlite3.bind stmt 2
           (Sqlite3.Data.TEXT (Printf.sprintf "-%d hours" hours)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.NULL -> false
          | Sqlite3.Data.TEXT s when s = "" -> false
          | _ -> true)
      | _ -> false)

let record_attempt ~db ~provider ~error =
  let sql =
    "INSERT INTO model_discovery_state (provider, last_attempted_at, \
     last_error) VALUES (?, datetime('now'), ?) ON CONFLICT(provider) DO \
     UPDATE SET last_attempted_at = datetime('now'), last_error = \
     excluded.last_error"
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT provider));
        ignore
          (Sqlite3.bind stmt 2
             (match error with
             | None -> Sqlite3.Data.NULL
             | Some e -> Sqlite3.Data.TEXT e));
        ignore (Sqlite3.step stmt))
  with exn ->
    Logs.warn (fun m ->
        m "model_discovery: record_attempt error: %s" (Printexc.to_string exn))

let upsert_models ~db ~provider models =
  let sql =
    "INSERT OR REPLACE INTO models_cache (provider, model_id, fetched_at) \
     VALUES (?, ?, datetime('now'))"
  in
  let count = ref 0 in
  (try
     let stmt = Sqlite3.prepare db sql in
     Fun.protect
       ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
       (fun () ->
         List.iter
           (fun model_id ->
             ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT provider));
             ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT model_id));
             (match Sqlite3.step stmt with
             | Sqlite3.Rc.DONE -> incr count
             | rc ->
                 Logs.warn (fun m ->
                     m "models_cache upsert failed: %s"
                       (Sqlite3.Rc.to_string rc)));
             ignore (Sqlite3.reset stmt))
           models)
   with exn ->
     Logs.warn (fun m ->
         m "models_cache upsert error: %s" (Printexc.to_string exn)));
  !count

let upsert_model_rich ~db ~provider ~model_id ~display_name ~context_window
    ~supports_vision ~supports_tools ~supports_thinking ~source =
  let sql =
    "INSERT INTO models_cache (provider, model_id, display_name, \
     context_window, supports_vision, supports_tools, supports_thinking, \
     source, fetched_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, datetime('now')) ON \
     CONFLICT(provider, model_id) DO UPDATE SET display_name = \
     excluded.display_name, context_window = excluded.context_window, \
     supports_vision = excluded.supports_vision, supports_tools = \
     excluded.supports_tools, supports_thinking = excluded.supports_thinking, \
     source = excluded.source, fetched_at = excluded.fetched_at"
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT provider));
        ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT model_id));
        ignore
          (Sqlite3.bind stmt 3
             (match display_name with
             | None -> Sqlite3.Data.NULL
             | Some n -> Sqlite3.Data.TEXT n));
        ignore
          (Sqlite3.bind stmt 4
             (match context_window with
             | None -> Sqlite3.Data.NULL
             | Some n -> Sqlite3.Data.INT (Int64.of_int n)));
        ignore
          (Sqlite3.bind stmt 5
             (Sqlite3.Data.INT (if supports_vision then 1L else 0L)));
        ignore
          (Sqlite3.bind stmt 6
             (Sqlite3.Data.INT (if supports_tools then 1L else 0L)));
        ignore
          (Sqlite3.bind stmt 7
             (Sqlite3.Data.INT (if supports_thinking then 1L else 0L)));
        ignore (Sqlite3.bind stmt 8 (Sqlite3.Data.TEXT source));
        match Sqlite3.step stmt with
        | Sqlite3.Rc.DONE -> true
        | rc ->
            Logs.warn (fun m ->
                m "models_cache upsert_rich failed: %s"
                  (Sqlite3.Rc.to_string rc));
            false)
  with exn ->
    Logs.warn (fun m ->
        m "models_cache upsert_rich error: %s" (Printexc.to_string exn));
    false

let load_codex_file_models ?(path = None) ~db () =
  let file_path =
    match path with
    | Some p -> p
    | None ->
        let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
        Filename.concat (Filename.concat home ".codex") "models_cache.json"
  in
  if not (Sys.file_exists file_path) then (
    Logs.debug (fun m ->
        m "model_discovery: codex cache not found: %s" file_path);
    0)
  else
    try
      let contents = In_channel.with_open_text file_path In_channel.input_all in
      let json = Yojson.Safe.from_string contents in
      let open Yojson.Safe.Util in
      let models = json |> member "models" |> to_list in
      let count = ref 0 in
      List.iter
        (fun entry ->
          try
            let model_id = entry |> member "slug" |> to_string in
            let display_name =
              try Some (entry |> member "display_name" |> to_string)
              with _ -> None
            in
            let context_window =
              try Some (entry |> member "context_window" |> to_int)
              with _ -> None
            in
            let supports_vision =
              try
                let mods = entry |> member "input_modalities" |> to_list in
                List.exists
                  (fun m -> try to_string m = "image" with _ -> false)
                  mods
              with _ -> false
            in
            let supports_tools =
              try entry |> member "supports_parallel_tool_calls" |> to_bool
              with _ -> true
            in
            let supports_thinking =
              try
                let levels =
                  entry |> member "supported_reasoning_levels" |> to_list
                in
                levels <> []
              with _ -> false
            in
            if
              upsert_model_rich ~db ~provider:"openai-codex" ~model_id
                ~display_name ~context_window ~supports_vision ~supports_tools
                ~supports_thinking ~source:"codex-cli"
            then incr count
          with exn ->
            Logs.debug (fun m ->
                m "model_discovery: skip codex entry: %s"
                  (Printexc.to_string exn)))
        models;
      Logs.info (fun m ->
          m "model_discovery: loaded %d codex models from %s" !count file_path);
      !count
    with exn ->
      Logs.warn (fun m ->
          m "model_discovery: failed to read codex cache: %s"
            (Printexc.to_string exn));
      0

let fetch_openai_models ~base_url ~api_key =
  let open Lwt.Syntax in
  let uri = base_url ^ "/models" in
  let headers = [ ("Authorization", "Bearer " ^ api_key) ] in
  let* status, body = Http_client.get ~uri ~headers in
  if status = 200 then
    try
      let json = Yojson.Safe.from_string body in
      let open Yojson.Safe.Util in
      let data = json |> member "data" |> to_list in
      let ids =
        List.filter_map
          (fun m -> try Some (m |> member "id" |> to_string) with _ -> None)
          data
      in
      Lwt.return (Ok ids)
    with exn ->
      Lwt.return
        (Error (Printf.sprintf "parse error: %s" (Printexc.to_string exn)))
  else Lwt.return (Error (Printf.sprintf "HTTP %d" status))

let fetch_ollama_models ~base_url =
  let open Lwt.Syntax in
  let uri = base_url ^ "/api/tags" in
  let* status, body = Http_client.get ~uri ~headers:[] in
  if status = 200 then
    try
      let json = Yojson.Safe.from_string body in
      let open Yojson.Safe.Util in
      let models = json |> member "models" |> to_list in
      let names =
        List.filter_map
          (fun m -> try Some (m |> member "name" |> to_string) with _ -> None)
          models
      in
      Lwt.return (Ok names)
    with exn ->
      Lwt.return
        (Error (Printf.sprintf "parse error: %s" (Printexc.to_string exn)))
  else Lwt.return (Error (Printf.sprintf "HTTP %d" status))

let refresh_provider ~db ~provider_name
    ~(provider_config : Runtime_config.provider_config) =
  let open Lwt.Syntax in
  if should_skip_provider provider_config then Lwt.return (Ok 0)
  else
    let base_url = get_base_url provider_config in
    let* result =
      if is_ollama provider_config then fetch_ollama_models ~base_url
      else fetch_openai_models ~base_url ~api_key:provider_config.api_key
    in
    match result with
    | Error e ->
        Logs.warn (fun m ->
            m "model_discovery: %s refresh failed: %s" provider_name e);
        Lwt.return (Error e)
    | Ok model_ids ->
        let count = upsert_models ~db ~provider:provider_name model_ids in
        Logs.info (fun m ->
            m "model_discovery: %s refreshed %d models" provider_name count);
        Lwt.return (Ok count)

let maybe_refresh ?db ?(force = false) ~(config : Runtime_config.t) () =
  let open Lwt.Syntax in
  match db with
  | None ->
      Logs.debug (fun m -> m "model_discovery: no db handle, skipping refresh");
      Lwt.return_unit
  | Some db ->
      let providers = config.providers in
      let* () =
        Lwt_list.iter_s
          (fun (name, pc) ->
            if should_skip_provider pc then Lwt.return_unit
            else
              let fresh =
                (not force)
                && (check_ttl_hours ~db ~provider:name ~hours:12
                   || check_attempt_ttl ~db ~provider:name ~hours:12)
              in
              if fresh then (
                Logs.debug (fun m ->
                    m "model_discovery: %s cache fresh, skipping" name);
                Lwt.return_unit)
              else
                let* result =
                  Lwt.catch
                    (fun () ->
                      refresh_provider ~db ~provider_name:name
                        ~provider_config:pc)
                    (fun exn ->
                      Logs.warn (fun m ->
                          m "model_discovery: %s error: %s" name
                            (Printexc.to_string exn));
                      Lwt.return (Error "exception"))
                in
                let error =
                  match result with Ok _ -> None | Error e -> Some e
                in
                record_attempt ~db ~provider:name ~error;
                Lwt.return_unit)
          providers
      in
      let codex_fresh =
        (not force) && check_attempt_ttl ~db ~provider:"openai-codex" ~hours:12
      in
      (if not codex_fresh then
         let _count = load_codex_file_models ~db () in
         record_attempt ~db ~provider:"openai-codex" ~error:None);
      Lwt.return_unit

(* Returns (provider, model_id) pairs from models_cache that are not already in
   the compile-time catalog. Pass ~provider_filter to restrict to one provider. *)
let get_db_only_models ~db ~provider_filter =
  let rows = ref [] in
  (try
     let sql, bind_p =
       match provider_filter with
       | None ->
           ( "SELECT provider, model_id FROM models_cache ORDER BY provider, \
              model_id",
             None )
       | Some p ->
           ( "SELECT provider, model_id FROM models_cache WHERE provider = ? \
              ORDER BY provider, model_id",
             Some p )
     in
     let stmt = Sqlite3.prepare db sql in
     Fun.protect
       ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
       (fun () ->
         (match bind_p with
         | Some p -> ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT p))
         | None -> ());
         while Sqlite3.step stmt = Sqlite3.Rc.ROW do
           let provider =
             match Sqlite3.column stmt 0 with
             | Sqlite3.Data.TEXT s -> s
             | _ -> ""
           in
           let model_id =
             match Sqlite3.column stmt 1 with
             | Sqlite3.Data.TEXT s -> s
             | _ -> ""
           in
           if provider <> "" && model_id <> "" then
             rows := (provider, model_id) :: !rows
         done)
   with _ -> ());
  let catalog_set =
    let tbl = Hashtbl.create 64 in
    List.iter
      (fun m ->
        Hashtbl.replace tbl
          (m.Models_catalog.provider ^ "/" ^ m.Models_catalog.id)
          ())
      Models_catalog.known_models;
    tbl
  in
  List.filter
    (fun (p, m) -> not (Hashtbl.mem catalog_set (p ^ "/" ^ m)))
    (List.rev !rows)
