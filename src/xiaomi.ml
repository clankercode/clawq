(* Xiaomi MiMo provider data — single source of truth. See xiaomi.mli.
   Mirrors the pi AI SDK (packages/ai/src/models.generated.ts): every model is
   reasoning=true, supports tools, api=openai-completions, thinkingFormat=
   deepseek (handled clawq-side by oai_thinking_style="reasoning_content" and
   model_requires_reasoning_content "mimo-"). *)

type model_spec = {
  id : string;
  display : string;
  context_window : int;
  max_tokens : int;
  supports_vision : bool;
  input_per_m : float;
  output_per_m : float;
  cache_read_per_m : float;
}

type provider_def = { name : string; base_url : string; env_vars : string list }

(* Prices are USD per 1M tokens (input / output / cache-read). *)
let flash =
  {
    id = "mimo-v2-flash";
    display = "MiMo-V2-Flash";
    context_window = 262144;
    max_tokens = 65536;
    supports_vision = false;
    input_per_m = 0.1;
    output_per_m = 0.3;
    cache_read_per_m = 0.01;
  }

let omni =
  {
    id = "mimo-v2-omni";
    display = "MiMo-V2-Omni";
    context_window = 262144;
    max_tokens = 131072;
    supports_vision = true;
    input_per_m = 0.4;
    output_per_m = 2.0;
    cache_read_per_m = 0.08;
  }

let pro =
  {
    id = "mimo-v2-pro";
    display = "MiMo-V2-Pro";
    context_window = 1048576;
    max_tokens = 131072;
    supports_vision = false;
    input_per_m = 1.0;
    output_per_m = 3.0;
    cache_read_per_m = 0.2;
  }

let v2_5 =
  {
    id = "mimo-v2.5";
    display = "MiMo-V2.5";
    context_window = 1048576;
    max_tokens = 131072;
    supports_vision = true;
    input_per_m = 0.4;
    output_per_m = 2.0;
    cache_read_per_m = 0.08;
  }

let v2_5_pro =
  {
    id = "mimo-v2.5-pro";
    display = "MiMo-V2.5-Pro";
    context_window = 1048576;
    max_tokens = 131072;
    supports_vision = false;
    input_per_m = 1.0;
    output_per_m = 3.0;
    cache_read_per_m = 0.2;
  }

let v2_5_pro_ultraspeed =
  {
    id = "mimo-v2.5-pro-ultraspeed";
    display = "MiMo-V2.5-Pro-UltraSpeed";
    context_window = 1048576;
    max_tokens = 131072;
    supports_vision = false;
    input_per_m = 1.305;
    output_per_m = 2.61;
    cache_read_per_m = 0.0108;
  }

(* Public xiaomi gets all 6 models; token-plan providers omit mimo-v2-flash
   (it stays on public/API-billing only — see pi xiaomi-models.test.ts). *)
let public_models = [ flash; omni; pro; v2_5; v2_5_pro; v2_5_pro_ultraspeed ]
let token_plan_models = [ omni; pro; v2_5; v2_5_pro; v2_5_pro_ultraspeed ]
let public_provider = "xiaomi"
let sgp_provider = "xiaomi-token-plan-sgp"

let providers =
  [
    {
      name = public_provider;
      base_url = "https://api.xiaomimimo.com/v1";
      env_vars = [ "XIAOMI_API_KEY" ];
    };
    {
      name = "xiaomi-token-plan-cn";
      base_url = "https://token-plan-cn.xiaomimimo.com/v1";
      env_vars = [ "XIAOMI_TOKEN_PLAN_CN_API_KEY" ];
    };
    {
      name = "xiaomi-token-plan-ams";
      base_url = "https://token-plan-ams.xiaomimimo.com/v1";
      env_vars = [ "XIAOMI_TOKEN_PLAN_AMS_API_KEY" ];
    };
    {
      name = sgp_provider;
      base_url = "https://token-plan-sgp.xiaomimimo.com/v1";
      env_vars = [ "XIAOMI_TOKEN_PLAN_SGP_API_KEY" ];
    };
  ]

let provider_names = List.map (fun p -> p.name) providers
let find_provider name = List.find_opt (fun p -> p.name = name) providers
let is_known_provider name = find_provider name <> None
let base_url_for name = Option.map (fun p -> p.base_url) (find_provider name)

let models_for name =
  if name = public_provider then public_models
  else if is_known_provider name then token_plan_models
  else []

let catalog_specs =
  List.concat_map
    (fun p -> List.map (fun s -> (p.name, s)) (models_for p.name))
    providers

(* Deduped by id: the public provider already enumerates all 6 distinct ids. *)
let pricing_specs = List.map (fun s -> (s.id, s)) public_models

let home_dir () =
  match Sys.getenv_opt "HOME" with Some h when h <> "" -> h | _ -> "/tmp"

let read_mimo_key () =
  let path = Filename.concat (home_dir ()) ".mimo" in
  if not (Sys.file_exists path) then None
  else
    try
      let ic = open_in_bin path in
      let len = in_channel_length ic in
      let raw = really_input_string ic len in
      close_in ic;
      let trimmed = String.trim raw in
      if trimmed = "" then None
      else
        (* Tolerate a JSON object holding the key; otherwise treat as raw. *)
        let from_json =
          match try Some (Yojson.Safe.from_string trimmed) with _ -> None with
          | Some (`Assoc _ as j) ->
              let open Yojson.Safe.Util in
              List.find_map
                (fun k ->
                  match
                    try Some (j |> member k |> to_string) with _ -> None
                  with
                  | Some v when String.trim v <> "" -> Some (String.trim v)
                  | _ -> None)
                [ "api_key"; "token"; "key" ]
          | _ -> None
        in
        match from_json with Some _ as r -> r | None -> Some trimmed
    with _ -> None

let resolve_api_key name =
  match find_provider name with
  | None -> None
  | Some p -> (
      let from_env =
        List.find_map
          (fun var ->
            match Sys.getenv_opt var with
            | Some v when String.trim v <> "" -> Some (String.trim v)
            | _ -> None)
          p.env_vars
      in
      match from_env with
      | Some _ as r -> r
      | None -> if name = sgp_provider then read_mimo_key () else None)

(* Backfill a declared xiaomi provider: only fill what's missing, and force
   reasoning thinking style when it's still the inert default. *)
let backfill name (pc : Runtime_config.provider_config) :
    Runtime_config.provider_config =
  let api_key =
    if Runtime_config.is_key_set pc.api_key then pc.api_key
    else match resolve_api_key name with Some k -> k | None -> pc.api_key
  in
  let base_url =
    match pc.base_url with Some _ -> pc.base_url | None -> base_url_for name
  in
  let kind = match pc.kind with Some _ -> pc.kind | None -> Some "xiaomi" in
  let oai_thinking_style =
    match pc.oai_thinking_style with
    | "" | "none" -> "reasoning_content"
    | s -> s
  in
  { pc with api_key; base_url; kind; oai_thinking_style }

let synthesize (p : provider_def) key : Runtime_config.provider_config =
  {
    Runtime_config.default_provider_config with
    api_key = key;
    kind = Some "xiaomi";
    base_url = Some p.base_url;
    oai_thinking_style = "reasoning_content";
  }

let augment_providers ~resolve_secrets existing =
  if not resolve_secrets then existing
  else
    let declared = List.map fst existing in
    let backfilled =
      List.map
        (fun (name, pc) ->
          if is_known_provider name then (name, backfill name pc) else (name, pc))
        existing
    in
    let synthesized =
      List.filter_map
        (fun p ->
          if List.mem p.name declared then None
          else
            match resolve_api_key p.name with
            | Some key -> Some (p.name, synthesize p key)
            | None -> None)
        providers
    in
    backfilled @ synthesized
