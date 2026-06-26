(* Persistent memory + session-history tools.  Split out of
   tools_builtin_io.ml; re-exported via [include] there. *)

let memory_store ~db =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "key",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Unique key for the memory (required)" );
                  ] );
              ( "content",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "Content to store (required)");
                  ] );
              ( "category",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Category for the memory (default: general)" );
                  ] );
            ] );
        ("required", `List [ `String "key"; `String "content" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"memory_store" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "memory_store";
    description =
      "Store a persistent key-value memory that survives across sessions. \
       Overwrites if the key already exists. Use this when the user tells you \
       something to remember, or when you derive a stable fact (a preference, \
       a setup detail, a directive) that would help future turns. Keys should \
       be short, descriptive, and namespaced (e.g. `user:timezone`, \
       `project:default_branch`). To search stored memories later, use \
       `memory_recall`.";
    parameters_schema = schema;
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let key = try args |> member "key" |> to_string with _ -> "" in
        let content =
          try args |> member "content" |> to_string with _ -> ""
        in
        let category =
          try args |> member "category" |> to_string with _ -> "general"
        in
        if key = "" then
          Lwt.return (param_err "parameter 'key' must be a non-empty string")
        else if content = "" then
          Lwt.return
            (param_err "parameter 'content' must be a non-empty string")
        else begin
          Memory.store_core ~db ~key ~content ~category ();
          Lwt.return (Printf.sprintf "Stored memory: %s" key)
        end);
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let memory_recall ~db =
  let schema =
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
                    ("description", `String "Search query (required)");
                  ] );
              ( "limit",
                `Assoc
                  [
                    ("type", `String "integer");
                    ( "description",
                      `String "Maximum number of results (default: 5)" );
                  ] );
            ] );
        ("required", `List [ `String "query" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"memory_recall" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "memory_recall";
    description =
      "Search persistent memories by full-text query and return matching \
       key-content pairs. Use BEFORE answering questions about prior \
       conversations, preferences, or facts the user has shared — these are \
       stored via `memory_store` and outlive the current session. To enumerate \
       all keys, use `memory_list`. To delete one, use `memory_forget`.";
    parameters_schema = schema;
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let query = try args |> member "query" |> to_string with _ -> "" in
        let limit = try args |> member "limit" |> to_int with _ -> 5 in
        if query = "" then
          Lwt.return (param_err "parameter 'query' must be a non-empty string")
        else
          let results = Memory.recall_core ~db ~query ~limit in
          if results = [] then Lwt.return "No matching memories found"
          else
            let lines =
              List.map
                (fun (key, content, category) ->
                  Printf.sprintf "[%s] (%s): %s" key category content)
                results
            in
            Lwt.return (String.concat "\n" lines));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let memory_forget ~db =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "key",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "Key of the memory to remove (required)" );
                  ] );
            ] );
        ("required", `List [ `String "key" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"memory_forget" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "memory_forget";
    description =
      "Delete a persistent memory by its exact key. Returns success/failure; \
       silently no-ops if the key doesn't exist. To list keys first, use \
       `memory_list`. To overwrite a value rather than delete, call \
       `memory_store` with the same key.";
    parameters_schema = schema;
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let key = try args |> member "key" |> to_string with _ -> "" in
        if key = "" then
          Lwt.return (param_err "parameter 'key' must be a non-empty string")
        else
          let deleted = Memory.forget_core ~db ~key in
          if deleted then Lwt.return (Printf.sprintf "Deleted memory: %s" key)
          else Lwt.return (Printf.sprintf "No memory found with key: %s" key));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let memory_list ~db =
  {
    Tool.name = "memory_list";
    description =
      "List all persistent memory keys, optionally filtered by category. \
       Returns key + category + short content preview, NOT full contents — use \
       `memory_recall` to fetch full values by search. Lightweight: call this \
       when you need to see what's stored without paying the token cost of \
       full content.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "category",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Optional category filter (omit for all)" );
                    ] );
              ] );
          ("required", `List []);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let category =
          try args |> member "category" |> to_string with _ -> ""
        in
        let results = Memory.list_core ~db ~category () in
        if results = [] then Lwt.return "No memories found"
        else
          let lines =
            List.map
              (fun (key, content, cat) ->
                Printf.sprintf "[%s] (%s): %s" key cat content)
              results
          in
          Lwt.return (String.concat "\n" lines));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let history_search ~db =
  let schema =
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
                      `String "Text to search for in message history (required)"
                    );
                  ] );
              ( "limit",
                `Assoc
                  [
                    ("type", `String "integer");
                    ( "description",
                      `String "Maximum number of results (default: 10)" );
                  ] );
            ] );
        ("required", `List [ `String "query" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"history_search" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "history_search";
    description =
      "Search your own chat/session message history across current and \
       archived epochs. Returns matching messages with role, content snippet, \
       timestamp, and source epoch.";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let query = try args |> member "query" |> to_string with _ -> "" in
        let limit = try args |> member "limit" |> to_int with _ -> 10 in
        if query = "" then
          Lwt.return (param_err "parameter 'query' must be a non-empty string")
        else
          let session_key =
            match context with Some ctx -> ctx.Tool.session_key | None -> None
          in
          match session_key with
          | None -> Lwt.return "Error: no session context available"
          | Some sk ->
              let results =
                Memory.search_session_history ~db ~session_key:sk ~query ~limit
                  ()
              in
              if results = [] then Lwt.return "No matching messages found"
              else
                let lines =
                  List.map
                    (fun (r : Memory.history_search_result) ->
                      let snippet =
                        if String.length r.content > 200 then
                          String.sub r.content 0 200 ^ "..."
                        else r.content
                      in
                      Printf.sprintf "[%s] (%s) [%s]: %s" r.source r.role
                        r.created_at snippet)
                    results
                in
                Lwt.return (String.concat "\n" lines));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }
