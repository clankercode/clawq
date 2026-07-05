let with_default field_name default f =
  try f ()
  with exn ->
    Logs.debug (fun m ->
        m "Config field '%s' parse failed: %s (using default)" field_name
          (Printexc.to_string exn));
    default

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
let sanitize_credential_handle_providers ~(original : Yojson.Safe.t)
    ~(complete : Yojson.Safe.t) : Yojson.Safe.t =
  let open Yojson.Safe.Util in
  match
    (member "credential_handles" original, member "credential_handles" complete)
  with
  | `List orig_handles, `List comp_handles ->
      let sanitized =
        List.map2
          (fun orig comp ->
            let orig_provider = member "provider" orig in
            match orig_provider with
            | `Assoc fields -> (
                match List.assoc_opt "type" fields with
                | Some (`String "encrypted") -> (
                    match List.assoc_opt "cipher_text" fields with
                    | Some (`String ct) when not (Secret_store.is_encrypted ct)
                      ->
                        (* Replace malformed encrypted provider with
                           sanitized version from complete config *)
                        comp
                    | _ -> orig)
                | _ -> orig)
            | _ -> orig)
          orig_handles comp_handles
      in
      let orig_fields = match original with `Assoc f -> f | _ -> [] in
      `Assoc
        (List.map
           (fun (k, v) ->
             if k = "credential_handles" then (k, `List sanitized) else (k, v))
           orig_fields)
  | _ -> original

let backfill_config ~path ~original_json ~disk_json ~config =
  let complete_json = Runtime_config.to_json config in
  let sanitized_original =
    sanitize_credential_handle_providers ~original:original_json
      ~complete:complete_json
  in
  let merged = merge_json sanitized_original complete_json in
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

let access_bundle_string_list_fields =
  [
    "allowed_tools";
    "denied_tools";
    "codebase_grants";
    "mcp_servers";
    "skills";
    "repositories";
    "domains";
    "credential_handles";
    "memory_grants";
    "budget_refs";
  ]

(** Validate that instructions in an access bundle are a list of strings (legacy
    format), a list of objects, or a mixed list. Each object must have a
    non-empty "text" field; optional fields are validated for type. *)
let validate_instruction_shapes ~bundle_index fields : string list =
  match List.assoc_opt "instructions" fields with
  | None -> []
  | Some (`List items) ->
      items
      |> List.mapi (fun i item ->
          match item with
          | `String _ -> []
          | `Assoc obj_fields ->
              let text_issues =
                match List.assoc_opt "text" obj_fields with
                | Some (`String t) when t <> "" -> []
                | Some (`String _) ->
                    [
                      Printf.sprintf
                        "access_bundles[%d].instructions[%d].text must not be \
                         empty"
                        bundle_index i;
                    ]
                | _ ->
                    [
                      Printf.sprintf
                        "access_bundles[%d].instructions[%d] must have a \
                         'text' field"
                        bundle_index i;
                    ]
              in
              let policy_issues =
                match List.assoc_opt "edit_policy" obj_fields with
                | None | Some (`String ("locked" | "admin_only" | "open")) -> []
                | Some (`String invalid) ->
                    [
                      Printf.sprintf
                        "access_bundles[%d].instructions[%d].edit_policy '%s' \
                         must be locked, admin_only, or open"
                        bundle_index i invalid;
                    ]
                | Some _ ->
                    [
                      Printf.sprintf
                        "access_bundles[%d].instructions[%d].edit_policy must \
                         be a string"
                        bundle_index i;
                    ]
              in
              let enabled_issues =
                match List.assoc_opt "enabled" obj_fields with
                | None | Some (`Bool _) -> []
                | Some _ ->
                    [
                      Printf.sprintf
                        "access_bundles[%d].instructions[%d].enabled must be a \
                         boolean"
                        bundle_index i;
                    ]
              in
              text_issues @ policy_issues @ enabled_issues
          | _ ->
              [
                Printf.sprintf
                  "access_bundles[%d].instructions[%d] must be a string or an \
                   instruction object"
                  bundle_index i;
              ])
      |> List.flatten
  | Some _ ->
      [
        Printf.sprintf "access_bundles[%d].instructions must be a list"
          bundle_index;
      ]

let validate_access_bundle_json_shapes json : string list =
  match Yojson.Safe.Util.member "access_bundles" json with
  | `List bundles ->
      bundles
      |> List.mapi (fun index bundle ->
          match bundle with
          | `Assoc fields ->
              let string_list_issues =
                access_bundle_string_list_fields
                |> List.filter_map (fun field ->
                    match List.assoc_opt field fields with
                    | None -> None
                    | Some (`List values) ->
                        if
                          List.for_all
                            (function `String _ -> true | _ -> false)
                            values
                        then None
                        else
                          Some
                            (Printf.sprintf
                               "access_bundles[%d].%s must be a list of strings"
                               index field)
                    | Some _ ->
                        Some
                          (Printf.sprintf
                             "access_bundles[%d].%s must be a list of strings"
                             index field))
              in
              let repo_grants_issues =
                match List.assoc_opt "repo_grants" fields with
                | None -> []
                | Some (`List grants) ->
                    grants
                    |> List.mapi (fun gi g ->
                        match g with
                        | `Assoc gf ->
                            let repo_ok =
                              match List.assoc_opt "repo" gf with
                              | Some (`String _) -> true
                              | _ -> false
                            in
                            let caps_ok =
                              match List.assoc_opt "capabilities" gf with
                              | Some (`List caps) ->
                                  List.for_all
                                    (function `String _ -> true | _ -> false)
                                    caps
                              | None -> true
                              | _ -> false
                            in
                            (if not repo_ok then
                               [
                                 Printf.sprintf
                                   "access_bundles[%d].repo_grants[%d].repo \
                                    must be a string"
                                   index gi;
                               ]
                             else [])
                            @
                            if not caps_ok then
                              [
                                Printf.sprintf
                                  "access_bundles[%d].repo_grants[%d].capabilities \
                                   must be a list of strings"
                                  index gi;
                              ]
                            else []
                        | _ ->
                            [
                              Printf.sprintf
                                "access_bundles[%d].repo_grants[%d] must be an \
                                 object"
                                index gi;
                            ])
                    |> List.flatten
                | Some _ ->
                    [
                      Printf.sprintf
                        "access_bundles[%d].repo_grants must be a list" index;
                    ]
              in
              let egress_rules_issues =
                match List.assoc_opt "egress_rules" fields with
                | None -> []
                | Some (`List rules) ->
                    rules
                    |> List.mapi (fun ri r ->
                        match r with
                        | `Assoc rf ->
                            let host_ok =
                              match List.assoc_opt "host" rf with
                              | Some (`String _) -> true
                              | _ -> false
                            in
                            let action_ok =
                              match List.assoc_opt "action" rf with
                              | None -> true
                              | Some (`String ("allow" | "deny")) -> true
                              | _ -> false
                            in
                            let log_policy_ok =
                              match List.assoc_opt "log_policy" rf with
                              | None -> true
                              | Some (`String ("log" | "no_log")) -> true
                              | _ -> false
                            in
                            let path_ok =
                              match List.assoc_opt "path" rf with
                              | None | Some (`String _) -> true
                              | _ -> false
                            in
                            let method_ok =
                              match List.assoc_opt "method" rf with
                              | None | Some (`String _) -> true
                              | _ -> false
                            in
                            (if not host_ok then
                               [
                                 Printf.sprintf
                                   "access_bundles[%d].egress_rules[%d].host \
                                    must be a string"
                                   index ri;
                               ]
                             else [])
                            @ (if not action_ok then
                                 [
                                   Printf.sprintf
                                     "access_bundles[%d].egress_rules[%d].action \
                                      must be 'allow' or 'deny'"
                                     index ri;
                                 ]
                               else [])
                            @ (if not log_policy_ok then
                                 [
                                   Printf.sprintf
                                     "access_bundles[%d].egress_rules[%d].log_policy \
                                      must be 'log' or 'no_log'"
                                     index ri;
                                 ]
                               else [])
                            @ (if not path_ok then
                                 [
                                   Printf.sprintf
                                     "access_bundles[%d].egress_rules[%d].path \
                                      must be a string"
                                     index ri;
                                 ]
                               else [])
                            @
                            if not method_ok then
                              [
                                Printf.sprintf
                                  "access_bundles[%d].egress_rules[%d].method \
                                   must be a string"
                                  index ri;
                              ]
                            else []
                        | _ ->
                            [
                              Printf.sprintf
                                "access_bundles[%d].egress_rules[%d] must be \
                                 an object"
                                index ri;
                            ])
                    |> List.flatten
                | Some _ ->
                    [
                      Printf.sprintf
                        "access_bundles[%d].egress_rules must be a list" index;
                    ]
              in
              let instruction_issues =
                validate_instruction_shapes ~bundle_index:index fields
              in
              string_list_issues @ repo_grants_issues @ egress_rules_issues
              @ instruction_issues
          | _ -> [ Printf.sprintf "access_bundles[%d] must be an object" index ])
      |> List.flatten
  | `Null -> []
  | _ -> [ "access_bundles must be a list" ]

let validate_egress_json_shapes json : string list =
  let validate_rule prefix rule =
    match rule with
    | `Assoc fields ->
        let host_ok =
          match List.assoc_opt "host" fields with
          | Some (`String _) -> true
          | _ -> false
        in
        let action_ok =
          match List.assoc_opt "action" fields with
          | None -> true
          | Some (`String ("allow" | "deny")) -> true
          | _ -> false
        in
        let log_policy_ok =
          match List.assoc_opt "log_policy" fields with
          | None -> true
          | Some (`String ("log" | "no_log")) -> true
          | _ -> false
        in
        let path_ok =
          match List.assoc_opt "path" fields with
          | None | Some (`String _) -> true
          | _ -> false
        in
        let method_ok =
          match List.assoc_opt "method" fields with
          | None | Some (`String _) -> true
          | _ -> false
        in
        (if not host_ok then [ prefix ^ ".host must be a string" ] else [])
        @ (if not action_ok then
             [ prefix ^ ".action must be 'allow' or 'deny'" ]
           else [])
        @ (if not log_policy_ok then
             [ prefix ^ ".log_policy must be 'log' or 'no_log'" ]
           else [])
        @ (if not path_ok then [ prefix ^ ".path must be a string" ] else [])
        @ if not method_ok then [ prefix ^ ".method must be a string" ] else []
    | _ -> [ prefix ^ " must be an object" ]
  in
  match Yojson.Safe.Util.member "egress" json with
  | `Null -> []
  | `Assoc fields ->
      let strictness_issues =
        match List.assoc_opt "strictness" fields with
        | None -> []
        | Some (`String ("strict" | "permissive")) -> []
        | Some _ -> [ "egress.strictness must be 'strict' or 'permissive'" ]
      in
      let default_policy_issues =
        match List.assoc_opt "default_policy" fields with
        | None -> []
        | Some (`String ("deny" | "allow" | "strict" | "permissive")) -> []
        | Some _ ->
            [
              "egress.default_policy must be 'deny', 'allow', 'strict', or \
               'permissive'";
            ]
      in
      let allowlist_issues =
        match List.assoc_opt "default_allowlist" fields with
        | None -> []
        | Some (`List rules) ->
            rules
            |> List.mapi (fun i rule ->
                validate_rule
                  (Printf.sprintf "egress.default_allowlist[%d]" i)
                  rule)
            |> List.flatten
        | Some _ -> [ "egress.default_allowlist must be a list" ]
      in
      strictness_issues @ default_policy_issues @ allowlist_issues
  | _ -> [ "egress must be an object" ]

let validate_room_profile_access_bundle_json_shapes json : string list =
  match Yojson.Safe.Util.member "room_profiles" json with
  | `List profiles ->
      profiles
      |> List.mapi (fun index profile ->
          match profile with
          | `Assoc fields -> (
              match List.assoc_opt "access_bundle_ids" fields with
              | None -> []
              | Some (`List values)
                when List.for_all
                       (function `String _ -> true | _ -> false)
                       values ->
                  []
              | Some _ ->
                  [
                    Printf.sprintf
                      "room_profiles[%d].access_bundle_ids must be a list of \
                       strings"
                      index;
                  ])
          | _ -> [ Printf.sprintf "room_profiles[%d] must be an object" index ])
      |> List.flatten
  | `Null -> []
  | _ -> [ "room_profiles must be a list" ]

let validate_access_scope_json_shapes json : string list =
  match Yojson.Safe.Util.member "access_scopes" json with
  | `List scopes ->
      scopes
      |> List.mapi (fun index scope ->
          match scope with
          | `Assoc fields ->
              let level_issues =
                match List.assoc_opt "level" fields with
                | None -> []
                | Some (`String ("default" | "workspace" | "channel" | "room"))
                  ->
                    []
                | Some (`String _) ->
                    [
                      Printf.sprintf
                        "access_scopes[%d].level must be one of default, \
                         workspace, channel, room"
                        index;
                    ]
                | Some _ ->
                    [
                      Printf.sprintf "access_scopes[%d].level must be a string"
                        index;
                    ]
              in
              let selector_issues =
                [ "workspace"; "channel"; "room" ]
                |> List.filter_map (fun key ->
                    match List.assoc_opt key fields with
                    | None | Some `Null | Some (`String _) -> None
                    | Some _ ->
                        Some
                          (Printf.sprintf
                             "access_scopes[%d].%s must be a string" index key))
              in
              let bundle_issues =
                match List.assoc_opt "access_bundle_ids" fields with
                | None -> []
                | Some (`List values)
                  when List.for_all
                         (function `String _ -> true | _ -> false)
                         values ->
                    []
                | Some _ ->
                    [
                      Printf.sprintf
                        "access_scopes[%d].access_bundle_ids must be a list of \
                         strings"
                        index;
                    ]
              in
              level_issues @ selector_issues @ bundle_issues
          | _ -> [ Printf.sprintf "access_scopes[%d] must be an object" index ])
      |> List.flatten
  | `Null -> []
  | _ -> [ "access_scopes must be a list" ]

let fail_closed_access_bundle_id (cfg : Runtime_config.t) =
  let base = "__invalid_access_policy_validation__" in
  let existing =
    List.map (fun (b : Runtime_config.access_bundle) -> b.id) cfg.access_bundles
  in
  let rec pick n =
    let id = if n = 0 then base else Printf.sprintf "%s:%d" base n in
    if List.mem id existing then pick (n + 1) else id
  in
  pick 0

let fail_closed_profile invalid_bundle_id
    (profile : Runtime_config.room_profile) =
  if List.mem invalid_bundle_id profile.access_bundle_ids then profile
  else
    {
      profile with
      access_bundle_ids = profile.access_bundle_ids @ [ invalid_bundle_id ];
    }

let locked_profile_for_missing_binding invalid_bundle_id profile_id :
    Runtime_config.room_profile =
  {
    Runtime_config.id = profile_id;
    display_name = Some "Invalid room profile binding";
    model = Runtime_config.default.agent_defaults.primary_model;
    system_prompt = "";
    max_tool_iterations =
      Runtime_config.default.agent_defaults.max_tool_iterations;
    status = "active";
    allowed_tools = [];
    denied_tools = [];
    access_bundle_ids = [ invalid_bundle_id ];
    ambient_enabled = false;
    ambient_quiet_start = Ambient_policy.default_ambient_quiet_start;
    ambient_quiet_end = Ambient_policy.default_ambient_quiet_end;
    ambient_rate_limit_rph = 0;
  }

let fail_closed_access_policy (cfg : Runtime_config.t) =
  let invalid_bundle_id = fail_closed_access_bundle_id cfg in
  let profiles =
    List.map (fail_closed_profile invalid_bundle_id) cfg.room_profiles
  in
  let profile_ids =
    let tbl = Hashtbl.create (List.length profiles) in
    List.iter
      (fun (p : Runtime_config.room_profile) -> Hashtbl.replace tbl p.id ())
      profiles;
    tbl
  in
  let missing_binding_profiles =
    cfg.room_profile_bindings
    |> List.filter_map (fun (b : Runtime_config.room_profile_binding) ->
        if b.active && not (Hashtbl.mem profile_ids b.profile_id) then (
          Hashtbl.add profile_ids b.profile_id ();
          Some
            (locked_profile_for_missing_binding invalid_bundle_id b.profile_id))
        else None)
  in
  {
    cfg with
    Runtime_config.room_profiles = profiles @ missing_binding_profiles;
    access_scopes = [];
  }

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
