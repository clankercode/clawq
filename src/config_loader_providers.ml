open Config_loader_support

let providers_node json =
  let open Yojson.Safe.Util in
  let top = try json |> member "providers" with _ -> `Null in
  if top <> `Null then top
  else
    try json |> member "models" |> member "providers"
    with exn ->
      Logs.debug (fun m ->
          m "Config: 'models.providers' parse failed: %s"
            (Printexc.to_string exn));
      `Null

let parse_codex_oauth ~resolve_secret oauth =
  let open Yojson.Safe.Util in
  let access_token =
    oauth |> member "access_token" |> to_string |> resolve_secret
  in
  let refresh_token =
    oauth |> member "refresh_token" |> to_string |> resolve_secret
  in
  let expires_at_ms =
    try oauth |> member "expires_at_ms" |> to_int
    with _ ->
      let expires = oauth |> member "expires" |> to_int in
      expires
  in
  let account_id =
    try Some (oauth |> member "account_id" |> to_string) with _ -> None
  in
  let email =
    try Some (oauth |> member "email" |> to_string) with _ -> None
  in
  ({
     Runtime_config.access_token;
     refresh_token;
     expires_at_ms;
     account_id;
     email;
   }
    : Runtime_config.codex_oauth_config)

let parse_provider ~resolve_secret name v =
  let open Yojson.Safe.Util in
  let api_key =
    try v |> member "api_key" |> to_string |> resolve_secret with _ -> ""
  in
  let base_url =
    try Some (v |> member "base_url" |> to_string) with _ -> None
  in
  let kind =
    with_default
      ("providers." ^ name ^ ".kind")
      None
      (fun () -> Some (v |> member "kind" |> to_string))
  in
  let default_model =
    with_default
      ("providers." ^ name ^ ".default_model")
      None
      (fun () -> Some (v |> member "default_model" |> to_string))
  in
  let project_id =
    with_default
      ("providers." ^ name ^ ".project_id")
      None
      (fun () -> Some (v |> member "project_id" |> to_string))
  in
  let location =
    with_default
      ("providers." ^ name ^ ".location")
      None
      (fun () -> Some (v |> member "location" |> to_string))
  in
  let service_account_json =
    with_default
      ("providers." ^ name ^ ".service_account_json")
      None
      (fun () -> Some (v |> member "service_account_json" |> to_string))
  in
  let thinking_budget_tokens =
    with_default
      ("providers." ^ name ^ ".thinking_budget_tokens")
      None
      (fun () -> Some (v |> member "thinking_budget_tokens" |> to_int))
  in
  let oai_thinking_style =
    with_default
      ("providers." ^ name ^ ".oai_thinking_style")
      "none"
      (fun () -> v |> member "oai_thinking_style" |> to_string)
  in
  let codex_oauth =
    try Some (parse_codex_oauth ~resolve_secret (v |> member "codex_oauth"))
    with _ -> None
  in
  let quota_credentials_file =
    with_default
      ("providers." ^ name ^ ".quota_credentials_file")
      None
      (fun () -> Some (v |> member "quota_credentials_file" |> to_string))
  in
  let quota_threshold =
    with_default
      ("providers." ^ name ^ ".quota_threshold")
      None
      (fun () -> Some (v |> member "quota_threshold" |> to_float))
  in
  let quota_check_enabled =
    with_default
      ("providers." ^ name ^ ".quota_check_enabled")
      true
      (fun () -> v |> member "quota_check_enabled" |> to_bool)
  in
  let prompt_cache_retention =
    with_default
      ("providers." ^ name ^ ".prompt_cache_retention")
      (Some "24h")
      (fun () ->
        match v |> member "prompt_cache_retention" with
        | `Null -> None
        | `Bool false -> None
        | s -> Some (to_string s))
  in
  let http_timeout_s =
    with_default
      ("providers." ^ name ^ ".http_timeout_s")
      None
      (fun () ->
        match v |> member "http_timeout_s" with
        | `Null -> None
        | `Float f -> Some f
        | `Int i -> Some (float_of_int i)
        | _ -> None)
  in
  let max_output_tokens =
    with_default
      ("providers." ^ name ^ ".max_output_tokens")
      None
      (fun () ->
        match v |> member "max_output_tokens" with
        | `Null -> None
        | `Int i -> Some i
        | `Float f -> Some (int_of_float f)
        | _ -> None)
  in
  let quota_cache_ttl_s =
    with_default
      ("providers." ^ name ^ ".quota_cache_ttl_s")
      None
      (fun () ->
        match v |> member "quota_cache_ttl_s" with
        | `Null -> None
        | `Int i -> Some i
        | `Float f -> Some (int_of_float f)
        | _ -> None)
  in
  ({
     Runtime_config.api_key;
     kind;
     base_url;
     default_model;
     project_id;
     location;
     service_account_json;
     thinking_budget_tokens;
     oai_thinking_style;
     codex_oauth;
     quota_credentials_file;
     quota_threshold;
     quota_check_enabled;
     prompt_cache_retention;
     http_timeout_s;
     max_output_tokens;
     quota_cache_ttl_s;
   }
    : Runtime_config.provider_config)

let parse ~resolve_secret ~resolve_secrets json =
  let open Yojson.Safe.Util in
  let providers =
    try
      providers_node json |> to_assoc
      |> List.map (fun (name, v) ->
          (name, parse_provider ~resolve_secret name v))
    with exn ->
      Logs.warn (fun m ->
          m "Failed to parse providers config: %s" (Printexc.to_string exn));
      []
  in
  (* B697: backfill declared xiaomi providers and synthesize absent ones when a
     key is discoverable (env vars / ~/.mimo). No-op on the resolve_secrets=false
     display/round-trip path, so synthesized providers are never persisted. *)
  Xiaomi.augment_providers ~resolve_secrets providers
