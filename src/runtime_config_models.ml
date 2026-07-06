type model_target = { provider : string option; model : string }

let effective_primary_target (ad : Runtime_config_types.agent_defaults) :
    model_target =
  let raw = String.trim ad.primary_model in
  let split_at delim =
    match String.index_opt raw delim with
    | Some i when i > 0 && i + 1 < String.length raw ->
        let provider = String.sub raw 0 i in
        let model = String.sub raw (i + 1) (String.length raw - i - 1) in
        Some { provider = Some provider; model }
    | _ -> None
  in
  match split_at ':' with
  | Some t -> t
  | None -> (
      match split_at '/' with
      | Some t -> t
      | None -> { provider = None; model = raw })

let strip_model_provider_prefix model =
  let try_strip delim =
    match String.index_opt model delim with
    | Some i when i > 0 && i + 1 < String.length model ->
        Some (String.sub model (i + 1) (String.length model - i - 1))
    | _ -> None
  in
  match try_strip ':' with
  | Some m -> m
  | None -> ( match try_strip '/' with Some m -> m | None -> model)

let context_window_table =
  [
    ("claude-opus-4-6", 200000);
    ("claude-sonnet-4-6", 200000);
    ("claude-haiku-4-5", 200000);
    ("claude-3.5-sonnet", 200000);
    ("claude-3.5-haiku", 200000);
    ("claude-3-opus", 200000);
    ("claude-3-sonnet", 200000);
    ("claude-3-haiku", 200000);
    ("gpt-4o", 128000);
    ("gpt-4o-mini", 128000);
    ("gpt-4-turbo", 128000);
    ("gpt-4", 8192);
    ("gpt-3.5-turbo", 16385);
    ("gpt-5.4", 272000);
    ("o1", 200000);
    ("o1-mini", 128000);
    ("o1-preview", 128000);
    ("o3", 200000);
    ("o3-mini", 200000);
    ("llama-3.3-70b", 128000);
    ("llama-3.1-405b", 128000);
    ("llama-3.1-70b", 128000);
    ("llama-3.1-8b", 128000);
    ("mistral-large", 128000);
    ("mixtral-8x7b", 32768);
    ("gemini-2.0-flash", 1048576);
    ("gemini-1.5-pro", 2097152);
    ("gemini-1.5-flash", 1048576);
    ("deepseek-v3", 128000);
    ("deepseek-r1", 128000);
    ("command-r-plus", 128000);
    ("command-r", 128000);
    ("minimax-m2", 204800);
    ("minimax-m2-highspeed", 204800);
    ("minimax-m2.1", 204800);
    ("minimax-m2.1-highspeed", 204800);
    ("minimax-m2.5", 204800);
    ("minimax-m2.5-highspeed", 204800);
    ("minimax-m2.7", 204800);
    ("minimax-m2.7-highspeed", 204800);
  ]

(* Operating ceilings for the runtime compaction budget. These cap the context
   window we will fill before compacting, even when a model advertises more (the
   API gets impractically slow past ~500k tokens). Keys are pre-normalized
   (lowercase, no provider prefix, no date suffix) and must be fully qualified so
   prefix matching does not over-capture (e.g. "gpt-5.5", never "gpt-5").
   Override per-model via the `model_context_limits` config map. *)
let default_model_context_caps =
  [
    ("gpt-5.5", 272000);
    ("minimax-m3", 512000);
    ("mimo-v2.5-pro", 512000);
    ("glm-5.2", 272000);
  ]

let normalize_model_name_for_context_lookup model_name =
  let norm =
    String.lowercase_ascii
      (Model_utils.strip_date_suffix (String.trim model_name))
  in
  strip_model_provider_prefix norm

let context_window_for_model ?(configured_limits = []) model_name =
  let bare = normalize_model_name_for_context_lookup model_name in
  let find_prefix hay needle =
    String.length hay >= String.length needle
    && String.sub hay 0 (String.length needle) = needle
  in
  (* exact match, then prefix match, over a name->value table *)
  let lookup table =
    match List.find_opt (fun (k, _) -> bare = k) table with
    | Some (_, v) -> Some v
    | None -> (
        match List.find_opt (fun (k, _) -> find_prefix bare k) table with
        | Some (_, v) -> Some v
        | None -> None)
  in
  let normalized_configured_limits =
    List.map
      (fun (name, limit) ->
        (normalize_model_name_for_context_lookup name, limit))
      configured_limits
  in
  match lookup normalized_configured_limits with
  | Some v -> Some v (* user override wins outright; may exceed a shipped cap *)
  | None -> (
      let base = lookup context_window_table in
      let cap = lookup default_model_context_caps in
      (* ceiling semantics: cap the advertised window when both are known *)
      match (base, cap) with
      | Some b, Some c -> Some (min b c)
      | None, Some c -> Some c
      | Some b, None -> Some b
      | None, None -> None)

let effective_primary_model (ad : Runtime_config_types.agent_defaults) =
  (effective_primary_target ad).model

let effective_primary_provider (ad : Runtime_config_types.agent_defaults) =
  (effective_primary_target ad).provider

let primary_model_deprecation_warning (ad : Runtime_config_types.agent_defaults)
    =
  Pmodel.deprecation_warning (Pmodel.parse_flexible ad.primary_model)

let default_provider_deprecation_warning (cfg : Runtime_config_types.t) =
  match cfg.default_provider with
  | None -> None
  | Some p ->
      Some
        (Printf.sprintf
           "WARNING: \"default_provider\" (\"%s\") is deprecated. The provider \
            is already embedded in \"agent_defaults.primary_model\" using the \
            \"provider:model\" format. Remove \"default_provider\" from your \
            config.json."
           p)
