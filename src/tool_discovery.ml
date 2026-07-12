(* Portable discovery over frozen Tool_catalog. *)

type short_result = { identity : string; summary : string; deferred : bool }

type inspect_result = {
  identity : string;
  description : string;
  parameters_schema : Yojson.Safe.t;
  risk_level : string;
  deferred : bool;
  aliases : string list;
  origin : string;
}

let max_search_results = 5

let truncate_summary s =
  let max_len = 120 in
  if String.length s <= max_len then s else String.sub s 0 (max_len - 3) ^ "..."

let risk_level_to_string = function
  | Tool.Low -> "low"
  | Medium -> "medium"
  | High -> "high"

let portable_tool_defs : Yojson.Safe.t list =
  let fn name desc props required =
    `Assoc
      [
        ("type", `String "function");
        ( "function",
          `Assoc
            [
              ("name", `String name);
              ("description", `String desc);
              ( "parameters",
                `Assoc
                  [
                    ("type", `String "object");
                    ("properties", `Assoc props);
                    ("required", `List (List.map (fun s -> `String s) required));
                    ("additionalProperties", `Bool false);
                  ] );
            ] );
      ]
  in
  [
    fn "search_tools"
      "Search authorized tools by keyword. Returns at most 5 short results."
      [
        ( "query",
          `Assoc
            [
              ("type", `String "string"); ("description", `String "Search query");
            ] );
        ( "limit",
          `Assoc
            [
              ("type", `String "integer");
              ("description", `String "Max results (default 5, max 5)");
            ] );
      ]
      [ "query" ];
    fn "inspect_tool"
      "Inspect one authorized tool: full schema and risk policy."
      [
        ( "identity",
          `Assoc
            [
              ("type", `String "string");
              ("description", `String "Canonical tool identity");
            ] );
      ]
      [ "identity" ];
    fn "call_tool"
      "Call an authorized tool by canonical identity after reauthorization."
      [
        ( "identity",
          `Assoc
            [
              ("type", `String "string");
              ("description", `String "Canonical tool identity");
            ] );
        ( "arguments",
          `Assoc
            [
              ("type", `String "object");
              ("description", `String "Tool arguments object");
            ] );
      ]
      [ "identity"; "arguments" ];
  ]

let search_tools ~catalog ~query ?(limit = max_search_results) () =
  let limit = max 1 (min limit max_search_results) in
  Tool_catalog.search catalog ~query ~limit
  |> List.map (fun (e : Tool_catalog.entry) ->
      {
        identity = e.canonical;
        summary = truncate_summary e.description;
        deferred = e.deferred;
      })

let inspect_tool ~catalog ~identity =
  match Tool_catalog.authorize_invoke catalog ~tool_name:identity with
  | Error e -> Error e
  | Ok e ->
      Ok
        {
          identity = e.canonical;
          description = e.description;
          parameters_schema = e.parameters_schema;
          risk_level = risk_level_to_string e.risk_level;
          deferred = e.deferred;
          aliases = e.aliases;
          origin = Tool_catalog.origin_to_string e.origin;
        }

let call_tool_authorize ~catalog ~identity =
  Tool_catalog.authorize_invoke catalog ~tool_name:identity

let eager_entries (cat : Tool_catalog.t) =
  List.filter (fun (e : Tool_catalog.entry) -> not e.deferred) cat.entries

let has_deferred (cat : Tool_catalog.t) =
  List.exists (fun (e : Tool_catalog.entry) -> e.deferred) cat.entries

let entry_to_openai (e : Tool_catalog.entry) =
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

let provider_payload ~catalog ~prefer_native_search =
  let eager = List.map entry_to_openai (eager_entries catalog) in
  if (not (has_deferred catalog)) || prefer_native_search then
    (* No portable search needed, or caller wants native deferred path. *)
    if prefer_native_search && has_deferred catalog then
      Tool_catalog.to_openai_json_with_search catalog
    else `List eager
  else
    (* Portable path: eager tools + search/inspect/call only. Deferred schemas
       are not dumped into the provider payload. *)
    `List (eager @ portable_tool_defs)
