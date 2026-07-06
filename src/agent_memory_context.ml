include Agent_1_context

let clip_memory_content content =
  if String.length content > 300 then String.sub content 0 300 ^ "..."
  else content

let scoped_memory_granted ~db ~scope_kind ~scope_key ?principal_kind
    ?principal_id ~capability () =
  match (principal_kind, principal_id) with
  | None, _ | _, None -> true
  | Some principal_kind, Some principal_id -> (
      match
        Memory.get_scope_by_kind_key ~db ~kind:scope_kind ~key:scope_key
      with
      | None -> false
      | Some scope ->
          let owns_scope =
            match
              (principal_kind, int_of_string_opt principal_id, scope.profile_id)
            with
            | "profile", Some profile_id, Some owner_id -> profile_id = owner_id
            | _ -> false
          in
          owns_scope
          || List.mem capability
               (Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind
                  ~principal_id))

let scoped_memory_strings ~db ~scope_kind ~scope_key ?principal_kind
    ?principal_id ~query () =
  if
    not
      (scoped_memory_granted ~db ~scope_kind ~scope_key ?principal_kind
         ?principal_id ~capability:"read" ())
  then []
  else
    let content_matches =
      Memory.query_scoped_memories ~db ~scope_kind ~scope_key
        ~content_search:query ~limit:10 ()
    in
    let all_when_no_content_match =
      if content_matches = [] then
        Memory.query_scoped_memories ~db ~scope_kind ~scope_key ~limit:10 ()
      else []
    in
    let rows = content_matches @ all_when_no_content_match in
    List.map
      (fun (m : Memory_types.scoped_memory) ->
        let content =
          match m.content with
          | Some c -> " " ^ clip_memory_content c
          | None -> ""
        in
        Printf.sprintf "[scoped:%s/%s#%d ref=%s]%s" m.scope_kind m.scope_key
          m.id m.reference content)
      rows

let inject_search_context ?scope_kind ?scope_key ?principal_kind ?principal_id
    agent ~db ~user_message =
  let open Lwt.Syntax in
  if agent.config.memory.search_enabled then
    Lwt.catch
      (fun () ->
        match (scope_kind, scope_key) with
        | Some scope_kind, Some scope_key ->
            let has_read_grant =
              scoped_memory_granted ~db ~scope_kind ~scope_key ?principal_kind
                ?principal_id ~capability:"read" ()
            in
            let scoped_message_results =
              if not has_read_grant then []
              else
                Memory.search ~db ~query:user_message ~scope_kind ~scope_key
                  ~limit:5 ()
                |> List.map (fun (m : Provider.message) ->
                    "[scoped-message:" ^ scope_kind ^ "/" ^ scope_key ^ "] "
                    ^ clip_memory_content m.content)
            in
            (* Vector search for scoped embeddings (if embedding provider is
               configured and room budget is not exceeded) *)
            let* scoped_vector_results =
              if not has_read_grant then Lwt.return []
              else if
                agent.config.memory.embedding_provider <> None
                || agent.config.memory.embedding_model <> None
              then
                (* Check room budget before making the embedding API call *)
                let budget_ok =
                  match
                    Memory.get_room_profile_binding ~db ~room_id:scope_key
                  with
                  | Some binding -> (
                      match
                        Room_budget.get_profile_budget ~db
                          ~profile_id:binding.profile_id
                      with
                      | Some state -> not state.Room_budget.limit_exceeded
                      | None -> true)
                  | None -> true
                in
                if not budget_ok then Lwt.return []
                else
                  Lwt.catch
                    (fun () ->
                      let* query_emb =
                        Vector.fetch_embedding ~config:agent.config
                          ~text:user_message
                      in
                      let results =
                        Vector.search ~db ~query_embedding:query_emb ~scope_kind
                          ~scope_key ~limit:5 ()
                      in
                      Lwt.return results)
                    (fun _exn -> Lwt.return [])
              else Lwt.return []
            in
            let scoped_rows =
              scoped_memory_strings ~db ~scope_kind ~scope_key ?principal_kind
                ?principal_id ~query:user_message ()
            in
            (* Merge keyword + vector results when vector results exist *)
            let keyword_strings =
              List.map
                (fun (s : string) ->
                  (* Strip provenance prefix for merge matching *)
                  match String.index_opt s ' ' with
                  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
                  | None -> s)
                scoped_message_results
            in
            let merged_strings =
              if scoped_vector_results = [] then scoped_message_results
              else
                let merged =
                  Vector.merge_results ~keyword_results:keyword_strings
                    ~vector_results:scoped_vector_results
                    ~keyword_weight:agent.config.memory.keyword_weight
                    ~vector_weight:agent.config.memory.vector_weight
                in
                List.map
                  (fun content ->
                    "[scoped-message:" ^ scope_kind ^ "/" ^ scope_key ^ "] "
                    ^ clip_memory_content content)
                  merged
            in
            (* Cross-scope retrieval: search granted sibling scopes *)
            let* granted_parts =
              if principal_kind = None || principal_id = None then Lwt.return []
              else
                let pk = Option.get principal_kind in
                let pid = Option.get principal_id in
                let granted_scopes =
                  Memory.list_scopes_granted_to_principal ~db ~principal_kind:pk
                    ~principal_id:pid ~capability:"read"
                in
                (* Filter out self — only retrieve from sibling scopes *)
                let siblings =
                  List.filter
                    (fun (s : Memory.memory_scope) ->
                      not (s.kind = scope_kind && s.key = scope_key))
                    granted_scopes
                in
                if siblings = [] then Lwt.return []
                else
                  (* Budget gate: skip cross-scope retrieval if current
                     room's budget is exceeded (mirrors B734 pattern) *)
                  let budget_ok =
                    match
                      Memory.get_room_profile_binding ~db ~room_id:scope_key
                    with
                    | Some binding -> (
                        match
                          Room_budget.get_profile_budget ~db
                            ~profile_id:binding.profile_id
                        with
                        | Some state -> not state.Room_budget.limit_exceeded
                        | None -> true)
                    | None -> true
                  in
                  if not budget_ok then Lwt.return []
                  else
                    let* all_granted =
                      Lwt_list.map_s
                        (fun (sibling : Memory.memory_scope) ->
                          (* FTS message search in sibling scope *)
                          let fts_results =
                            Memory.search ~db ~query:user_message
                              ~scope_kind:sibling.kind ~scope_key:sibling.key
                              ~limit:3 ()
                            |> List.map (fun (m : Provider.message) ->
                                Printf.sprintf "[granted:%s/%s] %s" sibling.kind
                                  sibling.key
                                  (clip_memory_content m.content))
                          in
                          (* Scoped memory strings from sibling scope *)
                          let scoped_mem_results =
                            Memory.query_scoped_memories ~db
                              ~scope_kind:sibling.kind ~scope_key:sibling.key
                              ~content_search:user_message ~limit:3 ()
                            |> List.map (fun (m : Memory.scoped_memory) ->
                                let content =
                                  match m.content with
                                  | Some c -> " " ^ clip_memory_content c
                                  | None -> ""
                                in
                                Printf.sprintf "[granted:%s/%s] %s" sibling.kind
                                  sibling.key content)
                          in
                          (* Vector search if embedding configured *)
                          let* vector_results =
                            if
                              agent.config.memory.embedding_provider <> None
                              || agent.config.memory.embedding_model <> None
                            then
                              Lwt.catch
                                (fun () ->
                                  let* query_emb =
                                    Vector.fetch_embedding ~config:agent.config
                                      ~text:user_message
                                  in
                                  let results =
                                    Vector.search ~db ~query_embedding:query_emb
                                      ~scope_kind:sibling.kind
                                      ~scope_key:sibling.key ~limit:3 ()
                                  in
                                  Lwt.return results)
                                (fun _exn -> Lwt.return [])
                            else Lwt.return []
                          in
                          (* Merge FTS + vector + scoped memory for this sibling *)
                          let kw_all = fts_results @ scoped_mem_results in
                          let merged =
                            if vector_results = [] then kw_all
                            else
                              let kw_stripped =
                                List.map
                                  (fun (s : string) ->
                                    match String.index_opt s ' ' with
                                    | Some i ->
                                        String.sub s (i + 1)
                                          (String.length s - i - 1)
                                    | None -> s)
                                  kw_all
                              in
                              let m =
                                Vector.merge_results
                                  ~keyword_results:kw_stripped ~vector_results
                                  ~keyword_weight:
                                    agent.config.memory.keyword_weight
                                  ~vector_weight:
                                    agent.config.memory.vector_weight
                              in
                              List.map
                                (fun content ->
                                  Printf.sprintf "[granted:%s/%s] %s"
                                    sibling.kind sibling.key
                                    (clip_memory_content content))
                                m
                          in
                          (* Emit ledger event if any results found *)
                          (if merged <> [] then
                             try
                               ignore
                                 (Room_activity_ledger.append_now ~db
                                    ~room_id:scope_key
                                    ~event_type:"cross_scope_context_injected"
                                    ~actor:"inject_search_context"
                                    ~metadata:
                                      (`Assoc
                                         [
                                           ( "source_scope_kind",
                                             `String sibling.kind );
                                           ( "source_scope_key",
                                             `String sibling.key );
                                           ( "result_count",
                                             `Int (List.length merged) );
                                         ])
                                   : Room_activity_ledger.event)
                             with _exn -> ());
                          Lwt.return merged)
                        siblings
                    in
                    Lwt.return (List.concat all_granted)
            in
            let parts = merged_strings @ scoped_rows @ granted_parts in
            if parts = [] then Lwt.return_unit
            else begin
              let context_msg =
                Provider.make_message ~role:"system"
                  ~content:
                    ("Relevant scoped memory context:\n"
                   ^ String.concat "\n" parts)
              in
              agent.history <- context_msg :: agent.history;
              Lwt.return_unit
            end
        | _ -> (
            (* Legacy routing fallback: only unprofiled or scope-less sessions
               should reach this branch. Profiled room/thread turns must pass
               scope_kind/scope_key and use the scoped branch above. *)
            (* TODO(scoped-memory-audit): keep this global message search as an
               explicit legacy fallback; do not route profiled rooms here. *)
            (* FTS keyword search *)
            let keyword_results =
              Memory.search ~db ~query:user_message ~limit:5 ()
            in
            let keyword_strings =
              List.map
                (fun (m : Provider.message) -> clip_memory_content m.content)
                keyword_results
            in
            (* Vector search (if embedding provider is configured) *)
            let* vector_strings =
              if
                agent.config.memory.embedding_provider <> None
                || agent.config.memory.embedding_model <> None
              then
                Lwt.catch
                  (fun () ->
                    let* query_emb =
                      Vector.fetch_embedding ~config:agent.config
                        ~text:user_message
                    in
                    let results =
                      Vector.search ~db ~query_embedding:query_emb ~limit:5 ()
                    in
                    Lwt.return results)
                  (fun _exn -> Lwt.return [])
              else Lwt.return []
            in
            (* Merge results *)
            let merged =
              if vector_strings = [] then keyword_strings
              else
                Vector.merge_results ~keyword_results:keyword_strings
                  ~vector_results:vector_strings
                  ~keyword_weight:agent.config.memory.keyword_weight
                  ~vector_weight:agent.config.memory.vector_weight
            in
            let top = List.filteri (fun i _ -> i < 3) merged in
            (* TODO(scoped-memory-audit): global core memory injection is a
               legacy fallback for unscoped turns; route to scoped memories if
               scope metadata becomes available here. *)
            (* Core memories: always include for awareness *)
            let core_items =
              let all = Memory.list_core ~db () in
              List.filteri (fun i _ -> i < 10) all
            in
            let core_strings =
              List.map
                (fun (key, content, category) ->
                  Printf.sprintf "[core:%s/%s] %s" category key content)
                core_items
            in
            match top @ core_strings with
            | [] -> Lwt.return_unit
            | parts ->
                let context_msg =
                  Provider.make_message ~role:"system"
                    ~content:
                      ("Relevant context from memory:\n"
                     ^ String.concat "\n" parts)
                in
                agent.history <- context_msg :: agent.history;
                Lwt.return_unit))
      (fun _ -> Lwt.return_unit)
  else Lwt.return_unit
