open Provider_types

(* Provider kind detection and native dispatch *)

type provider_kind =
  | OpenAICompat
  | OpenAICodex
  | Anthropic
  | Ollama
  | Gemini
  | Vertex
  | Cohere
  | MiniMax

let string_contains = String_util.string_contains

let detect_kind ?(name = "") (p : Runtime_config.provider_config) =
  match p.kind with
  | Some "openai-codex" | Some "codex" -> OpenAICodex
  | Some "anthropic" -> Anthropic
  (* B617: Z.ai exposes a native anthropic-compat surface at
     https://api.z.ai/api/anthropic/v1/messages. Treating kind="zai_anthropic"
     as Anthropic routes through provider_anthropic with the base_url default
     below (provider.ml::default_base_url) — users get full tool_use/content
     block fidelity instead of the OpenAI-compat shim. *)
  | Some "zai_anthropic" | Some "zai-anthropic" -> Anthropic
  | Some "gemini" -> Gemini
  | Some "ollama" -> Ollama
  | Some "vertex" -> Vertex
  | Some "cohere" -> Cohere
  | Some "minimax" -> MiniMax
  (* B697: Xiaomi MiMo (public + token-plan regions) is OpenAI-compatible. *)
  | Some "xiaomi" -> OpenAICompat
  | Some "openai" -> OpenAICompat
  | Some _ | None ->
      let key = p.api_key in
      let url = String.lowercase_ascii (Option.value ~default:"" p.base_url) in
      let lname = String.lowercase_ascii name in
      if String.length key >= 7 && String.sub key 0 7 = "sk-ant-" then Anthropic
      else if String.length key >= 6 && String.sub key 0 6 = "AIzaSy" then
        Gemini
      else if
        string_contains url "localhost:11434" || string_contains url "ollama"
      then Ollama
      else if string_contains url "aiplatform.googleapis.com" then Vertex
      else if string_contains url "cohere.com" || lname = "cohere" then Cohere
      else if lname = "openai-codex" || lname = "codex" then OpenAICodex
      else if lname = "minimax" || string_contains url "minimax" then MiniMax
      else OpenAICompat

type complete_fn =
  config:Runtime_config.t ->
  provider:Runtime_config.provider_config ->
  model:string ->
  messages:message list ->
  ?tools:Yojson.Safe.t ->
  ?session_key:string ->
  unit ->
  completion_response Lwt.t

type stream_fn =
  config:Runtime_config.t ->
  provider:Runtime_config.provider_config ->
  model:string ->
  messages:message list ->
  ?tools:Yojson.Safe.t ->
  ?session_key:string ->
  on_chunk:(stream_event -> unit Lwt.t) ->
  unit ->
  completion_response Lwt.t

let native_complete : (provider_kind * complete_fn) list ref = ref []
let native_stream : (provider_kind * stream_fn) list ref = ref []

let register_native_complete kind fn =
  native_complete := (kind, fn) :: !native_complete

let register_native_stream kind fn =
  native_stream := (kind, fn) :: !native_stream

let default_base_url_for name =
  match Xiaomi.base_url_for name with
  | Some url -> url
  | None -> (
      match name with
      | "zai_coding" -> "https://api.z.ai/api/coding/paas/v4"
      | "zai" -> "https://api.z.ai/api/paas/v4"
      | "zai_anthropic" | "zai-anthropic" -> "https://api.z.ai/api/anthropic"
      | "mistral" -> "https://api.mistral.ai/v1"
      | "xai" | "x_ai" -> "https://api.x.ai/v1"
      | "deepseek" -> "https://api.deepseek.com/v1"
      | "cohere" -> "https://api.cohere.com"
      | "kimi_coding" | "kimi-code" -> "https://api.kimi.com/coding/v1"
      | "kimi" -> "https://api.moonshot.cn/v1"
      | "moonshot" -> "https://api.moonshot.cn/v1"
      | "minimax" -> "https://api.minimax.io"
      | _ -> "https://openrouter.ai/api/v1")

let strip_date_suffix = Model_utils.strip_date_suffix

let normalize_model_name s =
  String.lowercase_ascii (strip_date_suffix (String.trim s))

let codex_associated_models =
  [
    "gpt-5";
    "gpt-5-codex";
    "gpt-5-codex-mini";
    "gpt-5-mini";
    "gpt-5.1";
    "gpt-5.1-codex";
    "gpt-5.1-codex-mini";
    "gpt-5.1-codex-max";
    "gpt-5.2";
    "gpt-5.2-codex";
    "gpt-5.3-codex";
    "gpt-5.3-codex-spark";
    "gpt-5.4";
    "gpt-5.4-mini";
    "gpt-5.4-pro";
    "gpt-5.5";
  ]

let is_codex_associated_model norm =
  List.exists
    (fun prefix ->
      String.length norm >= String.length prefix
      && String.sub norm 0 (String.length prefix) = prefix)
    codex_associated_models

(* B635: models that hard-reject any temperature != 1. For these the
   OpenAI-compat request body must omit the `temperature` field entirely
   rather than send the configured default. Detected by name prefix on
   the normalized form. *)
let temperature_locked_to_one_prefixes =
  [ "o1"; "o3"; "o4"; "kimi-for-code"; "kimi-for-coding" ]

let model_requires_temperature_one model =
  let norm = normalize_model_name model in
  List.exists
    (fun prefix ->
      String.length norm >= String.length prefix
      && String.sub norm 0 (String.length prefix) = prefix)
    temperature_locked_to_one_prefixes

(* Routing needs credentials that can satisfy the next request, not just
   config-shaped auth. Codex providers require viable OAuth, and expired access
   tokens only remain usable when a refresh token is present. *)
let provider_has_routable_auth ~name (p : Runtime_config.provider_config) =
  match detect_kind ~name p with
  | OpenAICodex -> (
      match p.codex_oauth with
      | Some creds ->
          let health = Openai_codex_oauth.inspect_credentials creds in
          (health.has_access_token && not health.expired)
          || health.refresh_possible
      | None -> false)
  | _ -> Runtime_config.is_key_set p.api_key

let find_provider_for_model ~providers ~model_name =
  let norm = normalize_model_name model_name in
  let match_provider (name, (p : Runtime_config.provider_config)) =
    let norm_name = String.lowercase_ascii name in
    let nlen = String.length norm_name in
    if
      String.length norm >= nlen
      && String.sub norm 0 nlen = norm_name
      && (String.length norm = nlen
         ||
         let c = norm.[nlen] in
         c = '-' || c = ':' || c = '/')
      && provider_has_routable_auth ~name p
    then Some (name, p)
    else
      let is_codex_kind =
        match p.kind with
        | Some "openai-codex" | Some "codex" -> true
        | _ -> false
      in
      let codex_match =
        is_codex_kind
        && ((String.length norm >= 13 && String.sub norm 0 13 = "openai-codex")
           || is_codex_associated_model norm)
        && provider_has_routable_auth ~name p
      in
      if codex_match then Some (name, p)
      else
        match p.default_model with
        | Some dm ->
            let norm_dm = normalize_model_name dm in
            if norm = norm_dm && provider_has_routable_auth ~name p then
              Some (name, p)
            else None
        | None -> None
  in
  List.find_map match_provider providers

let select_provider ~(config : Runtime_config.t) ?preferred_provider
    ?(quota_states : (string * Provider_quota.provider_quota) list = []) () =
  let find_named name =
    List.find_opt (fun (n, _) -> n = name) config.providers
  in
  let with_key =
    List.filter
      (fun (name, p) -> provider_has_routable_auth ~name p)
      config.providers
  in
  let model_target =
    Runtime_config.effective_primary_target config.agent_defaults
  in
  let raw_model = String.trim config.agent_defaults.primary_model in
  let model_provider_preferred =
    match model_target.provider with
    | Some name -> find_named name
    | None -> None
  in
  let config_provider_preferred =
    match config.default_provider with
    | Some name -> (
        match find_named name with
        | Some (n, p) when provider_has_routable_auth ~name:n p -> Some (n, p)
        | _ -> None)
    | None -> None
  in
  let model_routed =
    match model_target.provider with
    | None ->
        find_provider_for_model ~providers:config.providers
          ~model_name:model_target.model
    | Some _ -> None
  in
  let fallback_provider_preferred =
    match preferred_provider with
    | Some name -> (
        match find_named name with
        | Some (n, p) when provider_has_routable_auth ~name:n p -> Some (n, p)
        | _ -> None)
    | None -> None
  in
  let chosen =
    match fallback_provider_preferred with
    | Some pair -> pair
    | None -> (
        match model_provider_preferred with
        | Some pair -> pair
        | None -> (
            match model_routed with
            | Some pair -> pair
            | None -> (
                match config_provider_preferred with
                | Some pair -> pair
                | None -> (
                    match with_key with
                    | (name, p) :: _ -> (name, p)
                    | [] -> (
                        match config.providers with
                        | (name, p) :: _ -> (name, p)
                        | [] ->
                            ("default", Runtime_config.default_provider_config))
                    ))))
  in
  let provider_name, provider = chosen in
  let preferred_override_selected =
    match preferred_provider with
    | Some name -> name = provider_name
    | None -> false
  in
  let model =
    match model_target.provider with
    | Some requested when requested = provider_name -> model_target.model
    | Some _ when preferred_override_selected -> model_target.model
    | Some _ -> raw_model
    | _ -> (
        if raw_model <> "" then raw_model
        else match provider.default_model with Some m -> m | None -> raw_model)
  in
  (* Quota-aware deprioritisation: if the selected provider is constrained and
     an unconstrained alternative exists, prefer the alternative. *)
  match quota_states with
  | [] -> (provider_name, provider, model)
  | qs -> (
      let threshold =
        Option.value ~default:0.85 provider.Runtime_config.quota_threshold
      in
      let is_cur_constrained =
        match List.assoc_opt provider_name qs with
        | Some pq ->
            Provider_quota.is_constrained ~threshold pq.Provider_quota.state
        | None -> false
      in
      if not is_cur_constrained then (provider_name, provider, model)
      else
        let unconstrained_alt =
          List.find_opt
            (fun (n, p) ->
              n <> provider_name
              && provider_has_routable_auth ~name:n p
              && p.Runtime_config.default_model <> None
              &&
              let t =
                Option.value ~default:0.85 p.Runtime_config.quota_threshold
              in
              match List.assoc_opt n qs with
              | Some pq ->
                  not
                    (Provider_quota.is_constrained ~threshold:t
                       pq.Provider_quota.state)
              | None -> true)
            config.providers
        in
        match unconstrained_alt with
        | None -> (provider_name, provider, model)
        | Some (alt_name, alt_p) ->
            let alt_model =
              match alt_p.Runtime_config.default_model with
              | Some m -> m
              | None -> model (* unreachable: filter requires default_model *)
            in
            Logs.info (fun m ->
                m
                  "[quota] deprioritized %s (constrained), routing to %s \
                   (model=%s)"
                  provider_name alt_name alt_model);
            (alt_name, alt_p, alt_model))

(* If the model id differs only in casing from a catalog entry, rewrite it to
   the catalog's canonical casing and warn once per request. APIs like MiniMax
   are case-sensitive on the model id, so this prevents an avoidable 404. *)
let normalize_model_casing ~provider_name model =
  match Models_catalog.canonical_id ~provider:provider_name model with
  | Some canonical ->
      Logs.warn (fun m ->
          m "model casing corrected: %s:%s -> %s:%s" provider_name model
            provider_name canonical);
      canonical
  | None -> model
