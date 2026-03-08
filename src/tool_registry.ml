type t = { mutable tools : Tool.t list }

let create () = { tools = [] }
let register registry tool = registry.tools <- tool :: registry.tools

let replace registry tool =
  registry.tools <-
    tool :: List.filter (fun (t : Tool.t) -> t.name <> tool.name) registry.tools

let remove registry name =
  registry.tools <-
    List.filter (fun (t : Tool.t) -> t.name <> name) registry.tools

let find registry name =
  List.find_opt (fun (t : Tool.t) -> t.name = name) registry.tools

let list registry = List.rev registry.tools

let tool_to_openai_json (t : Tool.t) =
  `Assoc
    [
      ("type", `String "function");
      ( "function",
        `Assoc
          [
            ("name", `String t.name);
            ("description", `String t.description);
            ("parameters", t.parameters_schema);
          ] );
    ]

let tool_to_deferred_json (t : Tool.t) =
  `Assoc
    [
      ("type", `String "function");
      ( "function",
        `Assoc
          [ ("name", `String t.name); ("description", `String t.description) ]
      );
    ]

let tool_search_entry =
  `Assoc
    [
      ("type", `String "function");
      ( "function",
        `Assoc
          [
            ("name", `String "tool_search");
            ( "description",
              `String
                "Search for available tools by keyword. Use when you need a \
                 tool that isn't currently loaded." );
            ( "parameters",
              `Assoc
                [
                  ("type", `String "object");
                  ( "properties",
                    `Assoc
                      [
                        ( "query",
                          `Assoc
                            [
                              ("type", `String "string");
                              ( "description",
                                `String "Keywords to search for relevant tools"
                              );
                            ] );
                      ] );
                  ("required", `List [ `String "query" ]);
                  ("additionalProperties", `Bool false);
                ] );
          ] );
    ]

let to_openai_json registry =
  `List (List.map tool_to_openai_json registry.tools)

let to_openai_json_with_search registry =
  let entries =
    List.map
      (fun (t : Tool.t) ->
        if t.deferred then tool_to_deferred_json t else tool_to_openai_json t)
      registry.tools
  in
  let has_deferred =
    List.exists (fun (t : Tool.t) -> t.deferred) registry.tools
  in
  if has_deferred then `List (tool_search_entry :: entries) else `List entries

(* Search for tools by name/description keyword match. Used to resolve
   tool_search function calls from the model. *)
let search registry ~query =
  let q = String.lowercase_ascii query in
  let words = String.split_on_char ' ' q |> List.filter (fun s -> s <> "") in
  let score (t : Tool.t) =
    let name_l = String.lowercase_ascii t.name in
    let desc_l = String.lowercase_ascii t.description in
    let haystack = name_l ^ " " ^ desc_l in
    List.fold_left
      (fun acc w ->
        if String.length haystack >= String.length w then (
          let found = ref false in
          for i = 0 to String.length haystack - String.length w do
            if (not !found) && String.sub haystack i (String.length w) = w then
              found := true
          done;
          if !found then acc + 1 else acc)
        else acc)
      0 words
  in
  let deferred_tools =
    List.filter (fun (t : Tool.t) -> t.deferred) registry.tools
  in
  let scored =
    List.filter_map
      (fun t ->
        let s = score t in
        if s > 0 then Some (s, t) else None)
      deferred_tools
  in
  let sorted = List.sort (fun (s1, _) (s2, _) -> compare s2 s1) scored in
  List.map snd sorted
