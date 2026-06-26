let rec merge_json (original : Yojson.Safe.t) (complete : Yojson.Safe.t) :
    Yojson.Safe.t =
  match (original, complete) with
  | `Assoc orig_fields, `Assoc comp_fields ->
      let merged =
        List.map
          (fun (k, v) ->
            match List.assoc_opt k comp_fields with
            | Some cv -> (k, merge_json v cv)
            | None -> (k, v))
          orig_fields
      in
      let new_fields =
        List.filter
          (fun (k, _) -> not (List.mem_assoc k orig_fields))
          comp_fields
      in
      `Assoc (merged @ new_fields)
  | _ -> complete

(* [original_json] is the migrated, in-memory form used to seed the merge (so
   user keys survive backfill); [disk_json] is the raw, pre-migration content
   actually on disk. The write decision compares against [disk_json] so that
   migrations which only remove/rename keys (e.g. dropping the deprecated
   default_provider) are persisted even when they leave the merge output equal
   to the migrated form — otherwise the on-disk key would never be rewritten
   away (B701). *)
let backfill_config ~path ~original_json ~disk_json ~config =
  let complete_json = Runtime_config.to_json config in
  let merged = merge_json original_json complete_json in
  if merged <> disk_json then begin
    try
      let s = Yojson.Safe.pretty_to_string ~std:true merged in
      let oc = open_out path in
      output_string oc s;
      output_char oc '\n';
      close_out oc
    with _ -> ()
  end

let temperature_to_coq_units temperature =
  int_of_float (Float.round (temperature *. 100.0))

let coq_config_of_runtime (cfg : Runtime_config.t) : Clawq_core.clawqConfig =
  {
    Clawq_core.config_default_temperature =
      temperature_to_coq_units cfg.default_temperature;
    config_default_model = cfg.agent_defaults.primary_model;
    config_gateway =
      {
        Clawq_core.gateway_host = cfg.gateway.host;
        gateway_port = cfg.gateway.port;
        gateway_require_pairing = cfg.gateway.require_pairing;
      };
    config_memory =
      {
        Clawq_core.memory_backend = cfg.memory.backend;
        memory_search_enabled = cfg.memory.search_enabled;
        memory_vector_weight = cfg.memory.vector_weight;
        memory_keyword_weight = cfg.memory.keyword_weight;
      };
    config_security =
      {
        Clawq_core.security_workspace_only_cfg = cfg.security.workspace_only;
        security_audit_enabled_cfg = cfg.security.audit_enabled;
        security_encrypt_secrets_cfg = cfg.security.encrypt_secrets;
      };
  }

let coq_validation_view_of_json ~(json : Yojson.Safe.t)
    ~(config : Runtime_config.t) : Clawq_core.clawqConfig =
  let open Yojson.Safe.Util in
  let raw_default_temperature =
    try json |> member "default_temperature" |> to_float
    with _ -> config.default_temperature
  in
  let raw_gateway_port =
    try json |> member "gateway" |> member "port" |> to_int
    with _ -> config.gateway.port
  in
  let raw_vector_weight =
    try json |> member "memory" |> member "vector_weight" |> to_int
    with _ -> config.memory.vector_weight
  in
  let raw_keyword_weight =
    try json |> member "memory" |> member "keyword_weight" |> to_int
    with _ -> config.memory.keyword_weight
  in
  {
    (coq_config_of_runtime config) with
    Clawq_core.config_default_temperature =
      temperature_to_coq_units raw_default_temperature;
    config_gateway =
      {
        Clawq_core.gateway_host = config.gateway.host;
        gateway_port = raw_gateway_port;
        gateway_require_pairing = config.gateway.require_pairing;
      };
    config_memory =
      {
        Clawq_core.memory_backend = config.memory.backend;
        memory_search_enabled = config.memory.search_enabled;
        memory_vector_weight = raw_vector_weight;
        memory_keyword_weight = raw_keyword_weight;
      };
  }

let config_validation_issues (cfg : Clawq_core.clawqConfig) =
  let issues = ref [] in
  let gateway_port = cfg.config_gateway.gateway_port in
  let temperature = cfg.config_default_temperature in
  let vector_weight = cfg.config_memory.memory_vector_weight in
  let keyword_weight = cfg.config_memory.memory_keyword_weight in
  if
    vector_weight < 0 || vector_weight > 100 || keyword_weight < 0
    || keyword_weight > 100
    || not (Clawq_core.valid_weights cfg.config_memory)
  then issues := "memory weights" :: !issues;
  if
    gateway_port < 1 || gateway_port > 65535
    || not (Clawq_core.valid_port gateway_port)
  then issues := "gateway.port" :: !issues;
  if
    temperature < 0 || temperature > 200
    || not (Clawq_core.valid_temperature temperature)
  then issues := "default_temperature" :: !issues;
  List.rev !issues

let unique_issues issues =
  List.fold_left
    (fun acc issue -> if List.mem issue acc then acc else acc @ [ issue ])
    [] issues

let warn_invalid_config ~config_path issues =
  if issues <> [] then
    Printf.eprintf
      "WARNING: Config validation failed for %s: invalid %s (runtime defaults \
       may be substituted)\n\
       %!"
      config_path
      (String.concat ", " issues)

let default_path () = Dot_dir.config_path ()

(* Migrate the deprecated top-level "default_provider" key into the canonical
   "agent_defaults.primary_model" provider prefix, then drop it. (B701)

   default_provider is a routing fallback: in Provider.select_provider it is only
   consulted when primary_model carries no provider prefix. So:
   - If primary_model is bare ("model"), fold the provider in -> "provider:model",
     preserving the user's explicit provider choice in canonical form.
   - Otherwise primary_model already names a provider (and outranks
     default_provider), so the key is purely redundant and is simply removed.

   Doing this in migrate (before parse and backfill) means the deprecated key
   disappears from the parsed config and from disk on the next load, so its
   load-time deprecation warning stops firing — the self-heal that B588 intended
   but never achieved (merge_json had preserved the original-only key forever). *)
let migrate_default_provider (top : (string * Yojson.Safe.t) list) :
    (string * Yojson.Safe.t) list =
  match List.assoc_opt "default_provider" top with
  | Some (`String provider) when provider <> "" ->
      let drop_dp = List.filter (fun (k, _) -> k <> "default_provider") top in
      let model_is_bare m =
        m <> "" && (not (String.contains m ':')) && not (String.contains m '/')
      in
      let fold_into_agent_defaults = function
        | `Assoc ad_fields as ad -> (
            match List.assoc_opt "primary_model" ad_fields with
            | Some (`String m) when model_is_bare (String.trim m) ->
                let canonical = provider ^ ":" ^ String.trim m in
                `Assoc
                  (List.map
                     (fun (k, v) ->
                       if k = "primary_model" then (k, `String canonical)
                       else (k, v))
                     ad_fields)
            | _ -> ad)
        | other -> other
      in
      if List.mem_assoc "agent_defaults" drop_dp then
        List.map
          (fun (k, v) ->
            if k = "agent_defaults" then (k, fold_into_agent_defaults v)
            else (k, v))
          drop_dp
      else drop_dp
  | _ -> top

(* Rename legacy prefixed keys to canonical short names within sub-objects.
   Applied in-memory before parse and backfill so the canonical short names
   take effect immediately and the backfill pass will persist the clean form. *)
let migrate_config_json (json : Yojson.Safe.t) : Yojson.Safe.t =
  let migrate_keys renames = function
    | `Assoc fields ->
        let fields =
          List.fold_left
            (fun acc (old_key, new_key) ->
              if List.mem_assoc new_key acc then acc
              else
                match List.assoc_opt old_key acc with
                | None -> acc
                | Some v ->
                    let acc = List.filter (fun (k, _) -> k <> old_key) acc in
                    acc @ [ (new_key, v) ])
            fields renames
        in
        `Assoc fields
    | other -> other
  in
  let heartbeat_renames =
    [
      ("heartbeat_enabled", "enabled");
      ("heartbeat_interval_seconds", "interval_seconds");
      ("heartbeat_quiet_start", "quiet_start");
      ("heartbeat_quiet_end", "quiet_end");
    ]
  in
  let notify_renames =
    [ ("notify_channel", "channel"); ("notify_target", "target") ]
  in
  let error_watcher_renames =
    [ ("ec_enabled", "enabled"); ("ec_commit_tag", "commit_tag") ]
  in
  let summarizer_renames =
    [ ("summarizer_enabled", "enabled"); ("summarizer_model", "model") ]
  in
  match json with
  | `Assoc top ->
      let top = migrate_default_provider top in
      `Assoc
        (List.map
           (fun (k, v) ->
             match k with
             | "heartbeat" -> (k, migrate_keys heartbeat_renames v)
             | "notify" -> (k, migrate_keys notify_renames v)
             | "error_watcher" -> (k, migrate_keys error_watcher_renames v)
             | "summarizer" -> (k, migrate_keys summarizer_renames v)
             | _ -> (k, v))
           top)
  | other -> other
