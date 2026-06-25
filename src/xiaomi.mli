(* Xiaomi MiMo provider data — single source of truth.

   Holds the four xiaomi providers (public + cn/ams/sgp token-plan), their base
   URLs and env vars, the neutral model specs, key resolution (env vars +
   ~/.mimo), and provider augmentation. Consumed by [models_catalog],
   [cost_tracker], [provider], [config_loader], and [model_discovery] so the
   data is never duplicated.

   Depends only on [Runtime_config] + stdlib (+ yojson for ~/.mimo parsing); no
   network, no [Models_catalog]/[Cost_tracker]/[Provider] deps -> no cycles. *)

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

(* The four known xiaomi providers, in canonical order:
   xiaomi (public), xiaomi-token-plan-cn/-ams/-sgp. *)
val providers : provider_def list
val provider_names : string list
val is_known_provider : string -> bool

(* Base URL for a known xiaomi provider name (None for unknown names). *)
val base_url_for : string -> string option

(* Model specs for a provider name. The public "xiaomi" provider gets all 6
   models; the three token-plan providers get the same 5 except mimo-v2-flash.
   [] for unknown names. *)
val models_for : string -> model_spec list

(* (provider_name, spec) for every (provider, model) pair, for Models_catalog. *)
val catalog_specs : (string * model_spec) list

(* (model_id, spec) deduped by id (6 distinct), for Cost_tracker. *)
val pricing_specs : (string * model_spec) list

(* Read ~/.mimo: trim; if it parses as a JSON object pull api_key/token/key,
   otherwise the raw trimmed text. None if absent/empty. *)
val read_mimo_key : unit -> string option

(* Resolve an API key for a provider name: try its env vars in order, then for
   the sgp provider fall back to ~/.mimo. None if nothing is discoverable. *)
val resolve_api_key : string -> string option

(* For each known xiaomi provider: backfill a declared entry's missing
   api_key (from [resolve_api_key]), base_url, kind (Some "xiaomi") and
   reasoning thinking style; and synthesize an absent provider when a key is
   discoverable. No-op when [resolve_secrets] = false (so secrets are never
   injected on the display/round-trip path, and synthesized providers are never
   persisted). *)
val augment_providers :
  resolve_secrets:bool ->
  (string * Runtime_config.provider_config) list ->
  (string * Runtime_config.provider_config) list
