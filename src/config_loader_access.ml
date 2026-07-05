type t = {
  parsed_credential_handles : Runtime_config.credential_handle list;
  parsed_access_bundles : Runtime_config.access_bundle list;
  parsed_access_scopes : Runtime_config.access_scope list;
  parsed_egress : Runtime_config.egress_config;
}

let parse_credential_handles json =
  let open Yojson.Safe.Util in
  try
    json
    |> member "credential_handles"
    |> to_list
    |> List.map (fun ch ->
        let id = ch |> member "id" |> to_string in
        let status =
          try ch |> member "status" |> to_string with _ -> "active"
        in
        let description =
          try Some (ch |> member "description" |> to_string) with _ -> None
        in
        let provider_node = ch |> member "provider" in
        let provider_type =
          try provider_node |> member "type" |> to_string with _ -> "env_var"
        in
        let provider : Runtime_config.credential_provider =
          match provider_type with
          | "env_var" ->
              let name =
                try provider_node |> member "name" |> to_string with _ -> ""
              in
              Env_var { name }
          | "file" ->
              let path =
                try provider_node |> member "path" |> to_string with _ -> ""
              in
              File { path }
          | "encrypted" ->
              let cipher_text =
                try provider_node |> member "cipher_text" |> to_string
                with _ -> ""
              in
              if cipher_text = "" || not (Secret_store.is_encrypted cipher_text)
              then
                (* Fail closed: reject malformed encrypted providers *)
                Env_var { name = "__invalid_encrypted_" ^ id }
              else Encrypted { cipher_text }
          | "prompt" ->
              let description =
                try provider_node |> member "description" |> to_string
                with _ -> ""
              in
              Prompt { description }
          | _ ->
              (* Unknown provider type, default to env_var *)
              Env_var { name = "" }
        in
        ({ id; provider; description; status }
          : Runtime_config.credential_handle))
  with _ -> []

let parse_egress_rule (r : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let host = try r |> member "host" |> to_string with _ -> "*" in
  let path = try Some (r |> member "path" |> to_string) with _ -> None in
  let method_ = try Some (r |> member "method" |> to_string) with _ -> None in
  let action =
    try
      r |> member "action" |> to_string
      |> Runtime_config.egress_rule_action_of_string
      |> Option.value ~default:Runtime_config.Deny
    with _ -> Runtime_config.Deny
  in
  let log_policy =
    try
      r |> member "log_policy" |> to_string
      |> Runtime_config.egress_rule_log_policy_of_string
      |> Option.value ~default:Runtime_config.Log
    with _ -> Runtime_config.Log
  in
  ({ host; path; method_; action; log_policy } : Runtime_config.egress_rule)

let parse_egress_rules node =
  let open Yojson.Safe.Util in
  try node |> to_list |> List.map parse_egress_rule with _ -> []

let string_list node key =
  let open Yojson.Safe.Util in
  try node |> member key |> to_list |> List.map to_string with _ -> []

let parse_repo_grants b =
  let open Yojson.Safe.Util in
  try
    b |> member "repo_grants" |> to_list
    |> List.filter_map (fun g ->
        let repo =
          try Some (g |> member "repo" |> to_string) with _ -> None
        in
        let capabilities =
          try
            g |> member "capabilities" |> to_list
            |> List.filter_map (fun c ->
                Runtime_config.repo_capability_of_string (to_string c))
          with _ -> []
        in
        match repo with
        | Some r ->
            Some ({ repo = r; capabilities } : Runtime_config.repo_grant)
        | None -> None)
  with _ -> []

let parse_instructions b : Runtime_config.instruction_record list =
  match Yojson.Safe.Util.member "instructions" b with
  | `List items ->
      List.filter_map
        (fun item ->
          match item with
          | `String text ->
              Some (Runtime_config.default_instruction_record ~text ())
          | `Assoc _fields ->
              let open Yojson.Safe.Util in
              let text =
                try item |> member "text" |> to_string with _ -> ""
              in
              if text = "" then None
              else
                let source_scope =
                  try item |> member "source_scope" |> to_string
                  with _ -> "default"
                in
                let author =
                  try Some (item |> member "author" |> to_string)
                  with _ -> None
                in
                let enabled =
                  try item |> member "enabled" |> to_bool with _ -> true
                in
                let digest =
                  match
                    try Some (item |> member "digest" |> to_string)
                    with _ -> None
                  with
                  | Some _ as d -> d
                  | None -> Some Digestif.SHA256.(digest_string text |> to_hex)
                in
                let edit_policy =
                  try
                    item |> member "edit_policy" |> to_string
                    |> Runtime_config.instruction_edit_policy_of_string
                    |> Option.value ~default:Runtime_config.Open
                  with _ -> Runtime_config.Open
                in
                let locked =
                  match edit_policy with
                  | Runtime_config.Locked | Admin_only -> true
                  | Open -> false
                in
                Some
                  ({
                     text;
                     source_scope;
                     author;
                     enabled;
                     digest;
                     locked;
                     edit_policy;
                   }
                    : Runtime_config.instruction_record)
          | _ -> None)
        items
  | _ -> []

let parse_access_bundles json =
  let open Yojson.Safe.Util in
  try
    json |> member "access_bundles" |> to_list
    |> List.map (fun b ->
        let id = b |> member "id" |> to_string in
        let display_name =
          try Some (b |> member "display_name" |> to_string) with _ -> None
        in
        let system_prompt =
          try Some (b |> member "system_prompt" |> to_string) with _ -> None
        in
        let status =
          try b |> member "status" |> to_string with _ -> "active"
        in
        let repo_grants = parse_repo_grants b in
        let egress_rules = parse_egress_rules (b |> member "egress_rules") in
        ({
           id;
           display_name;
           system_prompt;
           allowed_tools = string_list b "allowed_tools";
           denied_tools = string_list b "denied_tools";
           codebase_grants = string_list b "codebase_grants";
           mcp_servers = string_list b "mcp_servers";
           skills = string_list b "skills";
           repositories = string_list b "repositories";
           repo_grants;
           domains = string_list b "domains";
           egress_rules;
           credential_handles = string_list b "credential_handles";
           instructions = parse_instructions b;
           memory_grants = string_list b "memory_grants";
           budget_refs = string_list b "budget_refs";
           status;
         }
          : Runtime_config.access_bundle))
  with _ -> []

let access_scope_level_of_string = function
  | "default" -> Some Runtime_config.Default
  | "workspace" -> Some Runtime_config.Workspace
  | "channel" -> Some Runtime_config.Channel
  | "room" -> Some Runtime_config.Room
  | _ -> None

let parse_access_scopes json =
  let open Yojson.Safe.Util in
  try
    json |> member "access_scopes" |> to_list
    |> List.map (fun s ->
        let id = s |> member "id" |> to_string in
        let level_raw = try s |> member "level" |> to_string with _ -> "" in
        let level_opt = access_scope_level_of_string level_raw in
        let level = Option.value ~default:Runtime_config.Default level_opt in
        let selector_malformed key =
          match member key s with `Null | `String _ -> false | _ -> true
        in
        let workspace =
          try Some (s |> member "workspace" |> to_string) with _ -> None
        in
        let channel =
          try Some (s |> member "channel" |> to_string) with _ -> None
        in
        let room =
          try Some (s |> member "room" |> to_string) with _ -> None
        in
        let status =
          if
            Option.is_none level_opt
            || selector_malformed "workspace"
            || selector_malformed "channel"
            || selector_malformed "room"
          then "deleted"
          else try s |> member "status" |> to_string with _ -> "active"
        in
        ({
           id;
           level;
           workspace;
           channel;
           room;
           access_bundle_ids = string_list s "access_bundle_ids";
           status;
         }
          : Runtime_config.access_scope))
  with _ -> []

let parse_egress ~(default : Runtime_config.t) json =
  let open Yojson.Safe.Util in
  try
    let e = json |> member "egress" in
    let strictness =
      let raw =
        try e |> member "strictness" |> to_string
        with _ -> (
          try e |> member "default_policy" |> to_string
          with _ ->
            Runtime_config.egress_strictness_to_string default.egress.strictness)
      in
      Runtime_config.egress_strictness_of_string raw
      |> Option.value ~default:default.egress.strictness
    in
    let default_allowlist =
      match e |> member "default_allowlist" with
      | `Null -> default.egress.default_allowlist
      | node -> parse_egress_rules node
    in
    ({ strictness; default_allowlist } : Runtime_config.egress_config)
  with _ -> default.egress

let parse ~(default : Runtime_config.t) json =
  {
    parsed_credential_handles = parse_credential_handles json;
    parsed_access_bundles = parse_access_bundles json;
    parsed_access_scopes = parse_access_scopes json;
    parsed_egress = parse_egress ~default json;
  }
