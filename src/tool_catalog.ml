(* Immutable per-turn Room Tool catalog. *)

type origin = Builtin | Skill | Mcp of string | Other of string

type entry = {
  canonical : string;
  aliases : string list;
  origin : origin;
  mcp_server : string option;
  schema_revision : string;
  deferred : bool;
  description : string;
  parameters_schema : Yojson.Safe.t;
  risk_level : Tool.risk_level;
}

type t = {
  id : string;
  revision : string;
  access_revision : string;
  room_id : string option;
  session_key : string option;
  created_at : string;
  entries : entry list;
}

let origin_to_string = function
  | Builtin -> "builtin"
  | Skill -> "skill"
  | Mcp server -> "mcp:" ^ server
  | Other s -> s

let schema_revision_of (schema : Yojson.Safe.t) =
  Digestif.SHA256.(digest_string (Yojson.Safe.to_string schema) |> to_hex)

let classify_origin ~skill_names (tool : Tool.t) : origin * string option =
  let name = tool.name in
  if List.mem name skill_names then (Skill, None)
  else if String.contains name '.' then
    (* Convention: mcp tools as server.remote_name *)
    match String.split_on_char '.' name with
    | server :: _ when server <> "" -> (Mcp server, Some server)
    | _ -> (Other "unknown", None)
  else (Builtin, None)

let entry_of_tool ~registry ~skill_names (tool : Tool.t) : entry =
  let canonical = tool.name in
  let all = Tool_registry.all_names registry canonical in
  let aliases = List.filter (fun n -> n <> canonical) all in
  let origin, mcp_server = classify_origin ~skill_names tool in
  {
    canonical;
    aliases;
    origin;
    mcp_server;
    schema_revision = schema_revision_of tool.parameters_schema;
    deferred = tool.deferred;
    description = tool.description;
    parameters_schema = tool.parameters_schema;
    risk_level = tool.risk_level;
  }

let content_revision (entries : entry list) =
  let parts =
    List.map
      (fun (e : entry) ->
        String.concat "|"
          [
            e.canonical;
            String.concat "," e.aliases;
            origin_to_string e.origin;
            Option.value e.mcp_server ~default:"";
            e.schema_revision;
            string_of_bool e.deferred;
          ])
      entries
  in
  Digestif.SHA256.(digest_string (String.concat "\n" parts) |> to_hex)

let generate_id ?(now = Unix.gettimeofday ()) () =
  Printf.sprintf "tcat_%d_%06d" (int_of_float now) (Random.int 1_000_000)

let freeze ~registry ?(allowed_tools = []) ?(denied_tools = [])
    ?(access_revision = "") ?room_id ?session_key ?(now = Unix.gettimeofday ())
    ?id () =
  let skill_names = registry.Tool_registry.skill_names in
  let tools = Tool_registry.list registry in
  let entries =
    tools
    |> List.filter_map (fun (tool : Tool.t) ->
        let equivalence_names = Tool_registry.all_names registry tool.name in
        if
          Tool_authz.is_allowed ~equivalence_names ~allowed_tools ~denied_tools
            ()
        then Some (entry_of_tool ~registry ~skill_names tool)
        else None)
  in
  let id = match id with Some i -> i | None -> generate_id ~now () in
  {
    id;
    revision = content_revision entries;
    access_revision;
    room_id;
    session_key;
    created_at = Time_util.iso8601_utc ~t:now ();
    entries;
  }

let freeze_from_snapshot ~registry ~snap ?room_id ?session_key ?now () =
  freeze ~registry ~allowed_tools:snap.Access_snapshot.allowed_tools
    ~denied_tools:snap.denied_tools ~access_revision:snap.config_hash ?room_id
    ?session_key:
      (match session_key with Some s -> Some s | None -> snap.session_key)
    ?now ()

let lookup (cat : t) name =
  List.find_opt
    (fun (e : entry) -> e.canonical = name || List.mem name e.aliases)
    cat.entries

let names (cat : t) = List.map (fun (e : entry) -> e.canonical) cat.entries
let contains cat name = Option.is_some (lookup cat name)

let equal_revision a b =
  a.revision = b.revision && a.access_revision = b.access_revision

let entry_count cat = List.length cat.entries

let risk_to_string = function
  | Tool.Low -> "low"
  | Medium -> "medium"
  | High -> "high"

let entry_to_openai_json (e : entry) =
  `Assoc
    [
      ("type", `String "function");
      ( "function",
        `Assoc
          [
            ("name", `String e.canonical);
            ("description", `String e.description);
            ("parameters", e.parameters_schema);
          ] );
    ]

let to_openai_json (cat : t) = `List (List.map entry_to_openai_json cat.entries)
