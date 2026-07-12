(* Vendor adapters for deferred tool discovery over frozen catalogs. *)

type vendor = OpenAI | Anthropic | Generic

let adapt = function
  | OpenAI ->
      (* OpenAI path: do not dump unselected deferred schemas. Use portable
         search/inspect/call for deferred discovery. *)
      fun ~catalog ->
        Tool_discovery.provider_payload ~catalog ~prefer_native_search:false
  | Anthropic ->
      (* Anthropic may receive authorized deferred definitions (full catalog
         of authorized tools). Denied tools are already filtered at freeze. *)
      fun ~catalog -> Tool_catalog.to_openai_json catalog
  | Generic ->
      fun ~catalog ->
        Tool_discovery.provider_payload ~catalog ~prefer_native_search:false

let collect_function_names (tools : Yojson.Safe.t) : string list =
  match tools with
  | `List items ->
      List.filter_map
        (function
          | `Assoc fields -> (
              match List.assoc_opt "function" fields with
              | Some (`Assoc ffields) -> (
                  match List.assoc_opt "name" ffields with
                  | Some (`String n) -> Some n
                  | _ -> None)
              | _ -> None)
          | _ -> None)
        items
  | _ -> []

let openai_excludes_unselected_deferred ~catalog =
  let payload = adapt OpenAI ~catalog in
  let names = collect_function_names payload in
  let deferred =
    List.filter_map
      (fun (e : Tool_catalog.entry) ->
        if e.deferred then Some e.canonical else None)
      catalog.entries
  in
  (* Deferred tool names must not appear as function definitions (portable
     tools search_tools/inspect/call are allowed). *)
  not (List.exists (fun d -> List.mem d names) deferred)

let anthropic_includes_only_authorized ~catalog ~denied_names =
  let payload = adapt Anthropic ~catalog in
  let names = collect_function_names payload in
  (not (List.exists (fun d -> List.mem d names) denied_names))
  && List.for_all
       (fun n ->
         n = "tool_search" || n = "search_tools" || n = "inspect_tool"
         || n = "call_tool"
         || Tool_catalog.contains catalog n)
       names
