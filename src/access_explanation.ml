(* Redacted effective-access explanation API.
   Provides human-readable and structured explanations of the resolved
   access policy without exposing secret values. *)

type scope_info = {
  id : string;
  level : string;
  workspace : string option;
  channel : string option;
  room : string option;
  bundle_ids : string list;
  status : string;
}

type item_explanation = { value : string; sources : source list }
and source = { layer : string; source_id : string; field : string }

type credential_info = {
  id : string;
  description : string option;
  provider_type : string;
  status : string;
}

type egress_rule_explanation = {
  host : string;
  path : string option;
  method_ : string option;
  action : Runtime_config.egress_rule_action;
  log_policy : Runtime_config.egress_rule_log_policy;
  index : int;
}

type t = {
  scopes : scope_info list;
  allowed_tools : item_explanation list;
  denied_tools : item_explanation list;
  codebase_grants : item_explanation list;
  blocked_codebase_grants : item_explanation list;
  repo_grants : item_explanation list;
  blocked_repo_grants : item_explanation list;
  mcp_servers : item_explanation list;
  skills : item_explanation list;
  repositories : item_explanation list;
  domains : item_explanation list;
  credential_handles : credential_info list;
  instructions : string list;
  memory_grants : item_explanation list;
  budget_refs : item_explanation list;
  egress_rules : egress_rule_explanation list;
  summary : string;
}

let scope_level_label = Runtime_config.access_scope_level_label

let scope_to_info (scope : Runtime_config.access_scope) : scope_info =
  {
    id = scope.id;
    level = scope_level_label scope.level;
    workspace = scope.workspace;
    channel = scope.channel;
    room = scope.room;
    bundle_ids = scope.access_bundle_ids;
    status = scope.status;
  }

let provenance_to_source (p : Runtime_config.access_provenance) : source =
  { layer = p.layer; source_id = p.source_id; field = p.field }

let item_to_explanation (item : Runtime_config.effective_access_item) :
    item_explanation =
  {
    value = item.value;
    sources = List.map provenance_to_source item.provenance;
  }

let credential_provider_type_string = function
  | Runtime_config.Env_var _ -> "env_var"
  | File _ -> "file"
  | Encrypted _ -> "encrypted"
  | Prompt _ -> "prompt"

let credential_to_info (ch : Runtime_config.credential_handle) : credential_info
    =
  {
    id = ch.id;
    description = ch.description;
    provider_type = credential_provider_type_string ch.provider;
    status = ch.status;
  }

let redact_credential_handle (ch : Runtime_config.credential_handle) :
    credential_info =
  {
    id = ch.id;
    description = ch.description;
    provider_type = credential_provider_type_string ch.provider;
    status = ch.status;
  }

let generate_summary (explanation : t) : string =
  let tool_count = List.length explanation.allowed_tools in
  let deny_count = List.length explanation.denied_tools in
  let grant_count = List.length explanation.codebase_grants in
  let blocked_count = List.length explanation.blocked_codebase_grants in
  let repo_grant_count = List.length explanation.repo_grants in
  let blocked_repo_grant_count = List.length explanation.blocked_repo_grants in
  Printf.sprintf
    "tools:%d/%d grants:%d+%d servers:%d skills:%d repos:%d repo_grants:%d+%d \
     domains:%d credentials:%d instructions:%d egress_rules:%d"
    tool_count deny_count grant_count blocked_count
    (List.length explanation.mcp_servers)
    (List.length explanation.skills)
    (List.length explanation.repositories)
    repo_grant_count blocked_repo_grant_count
    (List.length explanation.domains)
    (List.length explanation.credential_handles)
    (List.length explanation.instructions)
    (List.length explanation.egress_rules)

let create ~(config : Runtime_config.t) ~session_key () : t =
  let access = Runtime_config.resolve_effective_access config ~session_key () in
  let matching_scopes =
    config.access_scopes
    |> List.filter (Runtime_config.scope_matches config ~session_key)
    |> Runtime_config.sort_scopes |> List.map scope_to_info
  in
  (* Only expose credential handles that are inherited through effective access *)
  let inherited_credential_ids =
    List.map
      (fun (item : Runtime_config.effective_access_item) -> item.value)
      access.credential_handles
  in
  let inherited_credential_handles =
    config.credential_handles
    |> List.filter (fun (ch : Runtime_config.credential_handle) ->
        Runtime_config.credential_handle_active ch
        && List.mem ch.id inherited_credential_ids)
    |> List.map redact_credential_handle
  in
  let instruction_texts =
    List.map
      (fun (item : Runtime_config.effective_access_item) -> item.value)
      access.instructions
  in
  let egress_rules =
    List.mapi
      (fun idx (rule : Runtime_config.egress_rule) ->
        {
          host = rule.host;
          path = rule.path;
          method_ = rule.method_;
          action = rule.action;
          log_policy = rule.log_policy;
          index = idx;
        })
      access.egress_rules
  in
  let explanation =
    {
      scopes = matching_scopes;
      allowed_tools = List.map item_to_explanation access.allowed_tools;
      denied_tools = List.map item_to_explanation access.denied_tools;
      codebase_grants = List.map item_to_explanation access.codebase_grants;
      blocked_codebase_grants =
        List.map item_to_explanation access.blocked_codebase_grants;
      repo_grants = List.map item_to_explanation access.repo_grants;
      blocked_repo_grants =
        List.map item_to_explanation access.blocked_repo_grants;
      mcp_servers = List.map item_to_explanation access.mcp_servers;
      skills = List.map item_to_explanation access.skills;
      repositories = List.map item_to_explanation access.repositories;
      domains = List.map item_to_explanation access.domains;
      credential_handles = inherited_credential_handles;
      instructions = instruction_texts;
      memory_grants = List.map item_to_explanation access.memory_grants;
      budget_refs = List.map item_to_explanation access.budget_refs;
      egress_rules;
      summary = "";
    }
  in
  { explanation with summary = generate_summary explanation }

let source_to_json (s : source) : Yojson.Safe.t =
  `Assoc
    [
      ("layer", `String s.layer);
      ("source_id", `String s.source_id);
      ("field", `String s.field);
    ]

let item_explanation_to_json (ie : item_explanation) : Yojson.Safe.t =
  `Assoc
    [
      ("value", `String ie.value);
      ("sources", `List (List.map source_to_json ie.sources));
    ]

let scope_info_to_json (si : scope_info) : Yojson.Safe.t =
  `Assoc
    [
      ("id", `String si.id);
      ("level", `String si.level);
      ( "workspace",
        match si.workspace with Some s -> `String s | None -> `Null );
      ("channel", match si.channel with Some s -> `String s | None -> `Null);
      ("room", match si.room with Some s -> `String s | None -> `Null);
      ("bundle_ids", `List (List.map (fun s -> `String s) si.bundle_ids));
      ("status", `String si.status);
    ]

let credential_info_to_json (ci : credential_info) : Yojson.Safe.t =
  `Assoc
    [
      ("id", `String ci.id);
      ( "description",
        match ci.description with Some s -> `String s | None -> `Null );
      ("provider_type", `String ci.provider_type);
      ("status", `String ci.status);
    ]

let egress_rule_explanation_to_json (er : egress_rule_explanation) :
    Yojson.Safe.t =
  `Assoc
    [
      ("host", `String er.host);
      ("path", match er.path with Some p -> `String p | None -> `Null);
      ("method_", match er.method_ with Some m -> `String m | None -> `Null);
      ( "action",
        `String (Runtime_config.egress_rule_action_to_string er.action) );
      ( "log_policy",
        `String (Runtime_config.egress_rule_log_policy_to_string er.log_policy)
      );
      ("index", `Int er.index);
    ]

let to_json (explanation : t) : Yojson.Safe.t =
  `Assoc
    [
      ("scopes", `List (List.map scope_info_to_json explanation.scopes));
      ( "allowed_tools",
        `List (List.map item_explanation_to_json explanation.allowed_tools) );
      ( "denied_tools",
        `List (List.map item_explanation_to_json explanation.denied_tools) );
      ( "codebase_grants",
        `List (List.map item_explanation_to_json explanation.codebase_grants) );
      ( "blocked_codebase_grants",
        `List
          (List.map item_explanation_to_json explanation.blocked_codebase_grants)
      );
      ( "mcp_servers",
        `List (List.map item_explanation_to_json explanation.mcp_servers) );
      ("skills", `List (List.map item_explanation_to_json explanation.skills));
      ( "repositories",
        `List (List.map item_explanation_to_json explanation.repositories) );
      ( "repo_grants",
        `List (List.map item_explanation_to_json explanation.repo_grants) );
      ( "blocked_repo_grants",
        `List
          (List.map item_explanation_to_json explanation.blocked_repo_grants) );
      ("domains", `List (List.map item_explanation_to_json explanation.domains));
      ( "credential_handles",
        `List (List.map credential_info_to_json explanation.credential_handles)
      );
      ( "instructions",
        `List (List.map (fun s -> `String s) explanation.instructions) );
      ( "memory_grants",
        `List (List.map item_explanation_to_json explanation.memory_grants) );
      ( "budget_refs",
        `List (List.map item_explanation_to_json explanation.budget_refs) );
      ( "egress_rules",
        `List
          (List.map egress_rule_explanation_to_json explanation.egress_rules) );
      ("summary", `String explanation.summary);
    ]

let format_source_short (s : source) : string =
  Printf.sprintf "%s:%s:%s" s.layer s.source_id s.field

let format_item_with_sources (ie : item_explanation) : string =
  let source_strs = List.map format_source_short ie.sources in
  Printf.sprintf "%s [%s]" ie.value (String.concat "; " source_strs)

let format_scope_short (si : scope_info) : string =
  let selectors = ref [] in
  (match si.workspace with
  | Some w -> selectors := ("workspace=" ^ w) :: !selectors
  | None -> ());
  (match si.channel with
  | Some c -> selectors := ("channel=" ^ c) :: !selectors
  | None -> ());
  (match si.room with
  | Some r -> selectors := ("room=" ^ r) :: !selectors
  | None -> ());
  Printf.sprintf "%s (%s) [%s] bundles:%s" si.id si.level
    (String.concat ", " (List.rev !selectors))
    (String.concat "," si.bundle_ids)

let format_credential_short (ci : credential_info) : string =
  Printf.sprintf "%s (%s) %s" ci.id ci.provider_type ci.status

let format_egress_rule_short (er : egress_rule_explanation) : string =
  let action_str = Runtime_config.egress_rule_action_to_string er.action in
  let log_str = Runtime_config.egress_rule_log_policy_to_string er.log_policy in
  let method_str = match er.method_ with Some m -> m | None -> "*" in
  let path_str = match er.path with Some p -> p | None -> "/*" in
  Printf.sprintf "[%d] %s %s %s -> %s (log: %s)" er.index method_str er.host
    path_str action_str log_str

let to_text (explanation : t) : string =
  let buf = Buffer.create 4096 in
  let add line =
    Buffer.add_string buf line;
    Buffer.add_char buf '\n'
  in
  add "=== Effective Access Explanation ===";
  add "";
  (* Scopes *)
  if explanation.scopes <> [] then begin
    add "Inherited Scopes:";
    List.iter
      (fun si -> add (Printf.sprintf "  - %s" (format_scope_short si)))
      explanation.scopes;
    add ""
  end;
  (* Allowed tools *)
  if explanation.allowed_tools <> [] then begin
    add "Allowed Tools:";
    List.iter
      (fun ie -> add (Printf.sprintf "  - %s" (format_item_with_sources ie)))
      explanation.allowed_tools;
    add ""
  end;
  (* Denied tools *)
  if explanation.denied_tools <> [] then begin
    add "Denied Tools:";
    List.iter
      (fun ie -> add (Printf.sprintf "  - %s" (format_item_with_sources ie)))
      explanation.denied_tools;
    add ""
  end;
  (* Codebase grants *)
  if explanation.codebase_grants <> [] then begin
    add "Codebase Grants:";
    List.iter
      (fun ie -> add (Printf.sprintf "  - %s" (format_item_with_sources ie)))
      explanation.codebase_grants;
    add ""
  end;
  (* Blocked codebase grants *)
  if explanation.blocked_codebase_grants <> [] then begin
    add "Blocked Codebase Grants:";
    List.iter
      (fun ie -> add (Printf.sprintf "  - %s" (format_item_with_sources ie)))
      explanation.blocked_codebase_grants;
    add ""
  end;
  (* MCP servers *)
  if explanation.mcp_servers <> [] then begin
    add "MCP Servers:";
    List.iter
      (fun ie -> add (Printf.sprintf "  - %s" (format_item_with_sources ie)))
      explanation.mcp_servers;
    add ""
  end;
  (* Skills *)
  if explanation.skills <> [] then begin
    add "Skills:";
    List.iter
      (fun ie -> add (Printf.sprintf "  - %s" (format_item_with_sources ie)))
      explanation.skills;
    add ""
  end;
  (* Repositories *)
  if explanation.repositories <> [] then begin
    add "Repositories:";
    List.iter
      (fun ie -> add (Printf.sprintf "  - %s" (format_item_with_sources ie)))
      explanation.repositories;
    add ""
  end;
  (* Repo grants *)
  if explanation.repo_grants <> [] then begin
    add "Repo Grants:";
    List.iter
      (fun ie -> add (Printf.sprintf "  - %s" (format_item_with_sources ie)))
      explanation.repo_grants;
    add ""
  end;
  (* Blocked repo grants *)
  if explanation.blocked_repo_grants <> [] then begin
    add "Blocked Repo Grants:";
    List.iter
      (fun ie -> add (Printf.sprintf "  - %s" (format_item_with_sources ie)))
      explanation.blocked_repo_grants;
    add ""
  end;
  (* Domains *)
  if explanation.domains <> [] then begin
    add "Domains:";
    List.iter
      (fun ie -> add (Printf.sprintf "  - %s" (format_item_with_sources ie)))
      explanation.domains;
    add ""
  end;
  (* Credential handles *)
  if explanation.credential_handles <> [] then begin
    add "Credential Handles:";
    List.iter
      (fun ci -> add (Printf.sprintf "  - %s" (format_credential_short ci)))
      explanation.credential_handles;
    add ""
  end;
  (* Instructions *)
  if explanation.instructions <> [] then begin
    add "Instructions:";
    List.iter
      (fun instr ->
        let digest = Digestif.SHA256.(digest_string instr |> to_hex) in
        add
          (Printf.sprintf "  - [%s] %s" (String.sub digest 0 8)
             (if String.length instr > 60 then String.sub instr 0 57 ^ "..."
              else instr)))
      explanation.instructions;
    add ""
  end;
  (* Memory grants *)
  if explanation.memory_grants <> [] then begin
    add "Memory Grants:";
    List.iter
      (fun ie -> add (Printf.sprintf "  - %s" (format_item_with_sources ie)))
      explanation.memory_grants;
    add ""
  end;
  (* Budget refs *)
  if explanation.budget_refs <> [] then begin
    add "Budget Refs:";
    List.iter
      (fun ie -> add (Printf.sprintf "  - %s" (format_item_with_sources ie)))
      explanation.budget_refs;
    add ""
  end;
  (* Egress rules *)
  if explanation.egress_rules <> [] then begin
    add "Egress Rules:";
    List.iter
      (fun er -> add (Printf.sprintf "  - %s" (format_egress_rule_short er)))
      explanation.egress_rules;
    add ""
  end;
  add (Printf.sprintf "Summary: %s" explanation.summary);
  Buffer.contents buf
