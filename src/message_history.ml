let collect_tool_call_ids msgs =
  let seen = Hashtbl.create 64 in
  List.fold_left
    (fun acc (m : Provider.message) ->
      if m.role = "assistant" && m.tool_calls <> [] then
        List.fold_left
          (fun acc (tc : Provider.tool_call) ->
            if Hashtbl.mem seen tc.id then acc
            else begin
              Hashtbl.add seen tc.id ();
              tc.id :: acc
            end)
          acc m.tool_calls
      else acc)
    [] msgs

let collect_tool_result_ids msgs =
  let seen = Hashtbl.create 64 in
  List.fold_left
    (fun acc (m : Provider.message) ->
      match m.tool_call_id with
      | Some id when m.role = "tool" ->
          if Hashtbl.mem seen id then acc
          else begin
            Hashtbl.add seen id ();
            id :: acc
          end
      | _ -> acc)
    [] msgs

(* Strip function_call entries from provider_response_items_json whose call_id
   is not in [kept_ids].  This keeps the raw provider payload consistent with
   the tool_calls list after integrity stripping. *)
let strip_provider_items_for_removed_calls provider_json_opt ~kept_ids =
  match provider_json_opt with
  | None -> None
  | Some json_str -> (
      try
        let arr = Yojson.Safe.from_string json_str in
        let items = match arr with `List items -> items | _ -> [ arr ] in
        let filtered =
          List.filter
            (fun item ->
              match item with
              | `Assoc fields -> (
                  match List.assoc_opt "type" fields with
                  | Some (`String "function_call") -> (
                      match List.assoc_opt "call_id" fields with
                      | Some (`String id) -> List.mem id kept_ids
                      | _ -> true)
                  | _ -> true)
              | _ -> true)
            items
        in
        if List.length filtered = List.length items then Some json_str
        else Some (Yojson.Safe.to_string (`List filtered))
      with _ -> provider_json_opt)

let ensure_tool_group_integrity msgs =
  let call_ids = collect_tool_call_ids msgs in
  let result_ids = collect_tool_result_ids msgs in
  msgs
  |> List.filter (fun (m : Provider.message) ->
      if m.role = "tool" then
        match m.tool_call_id with
        | Some id ->
            if List.mem id call_ids then true
            else begin
              Logs.warn (fun m' ->
                  m'
                    "[message_history] dropping orphan tool result for \
                     call_id=%s"
                    id);
              false
            end
        | None -> true
      else true)
  |> List.map (fun (m : Provider.message) ->
      if m.role = "assistant" && m.tool_calls <> [] then
        let kept =
          List.filter
            (fun (tc : Provider.tool_call) -> List.mem tc.id result_ids)
            m.tool_calls
        in
        let kept_ids = List.map (fun (tc : Provider.tool_call) -> tc.id) kept in
        {
          m with
          tool_calls = kept;
          provider_response_items_json =
            strip_provider_items_for_removed_calls
              m.provider_response_items_json ~kept_ids;
        }
      else m)
  (* B620 round 2/3: drop assistant messages that became fully empty after
     orphan-stripping (no content, no content_parts, no tool_calls, no
     provider_response_items_json, no thinking). Such messages serialize as
     content:"" which Anthropic rejects with "text content blocks must be
     non-empty". Codex (OpenAI Responses API) replays
     provider_response_items_json from such messages — those MUST be kept
     so their reasoning/output items round-trip. Similarly an assistant
     turn that emitted thinking-only content (rare but legal) must be
     preserved. *)
  |> List.filter (fun (m : Provider.message) ->
      let has_provider_items =
        match m.provider_response_items_json with
        | Some s when String.trim s <> "" && s <> "[]" -> true
        | _ -> false
      in
      let has_thinking =
        match m.thinking with
        | Some s when String.trim s <> "" -> true
        | _ -> false
      in
      not
        (m.role = "assistant" && m.content = "" && m.content_parts = []
       && m.tool_calls = [] && (not has_provider_items) && not has_thinking))

let adjust_split_for_tool_groups to_compact to_keep =
  let rec move_orphans compact keep =
    match keep with
    | (msg : Provider.message) :: rest when msg.role = "tool" ->
        move_orphans (compact @ [ msg ]) rest
    | _ -> (compact, keep)
  in
  move_orphans to_compact to_keep

let expand_keep_for_tool_groups to_compact to_keep =
  let rec loop compact keep =
    match keep with
    | (msg : Provider.message) :: _ when msg.role = "tool" -> (
        match List.rev compact with
        | prev :: rest_rev -> loop (List.rev rest_rev) (prev :: keep)
        | [] -> keep)
    | _ -> keep
  in
  loop to_compact to_keep

let sanitize_messages_for_flush (msgs : Provider.message list) :
    Provider.message list =
  let truncate_content s max_len =
    if String.length s <= max_len then s
    else
      let suffix = "... [truncated]" in
      let cut = max_len - String.length suffix in
      if cut <= 0 then String.sub s 0 max_len else String.sub s 0 cut ^ suffix
  in
  (* Phase 1: transform each message into system/user/assistant with no
     tool_calls, or drop it. *)
  let transformed =
    List.filter_map
      (fun (m : Provider.message) ->
        if m.role = "event" then None
        else
          let role =
            if m.role = "developer" || m.role = "tool" then "user" else m.role
          in
          let content =
            if m.role = "tool" then
              let prefix =
                match m.name with
                | Some n -> Printf.sprintf "[Tool result (%s)]: " n
                | None -> "[Tool result]: "
              in
              truncate_content (prefix ^ m.content) 2000
            else if m.role = "assistant" && m.content = "" && m.tool_calls <> []
            then
              let names =
                List.map
                  (fun (tc : Provider.tool_call) -> tc.function_name)
                  m.tool_calls
              in
              "[Called tools: " ^ String.concat ", " names ^ "]"
            else if m.content = "" && m.content_parts <> [] then
              "[Multimedia content]"
            else m.content
          in
          if content = "" || String.trim content = "" then
            (* Note: tool-role messages are always preserved because the prefix
               added above ensures content is never empty. *)
            None
          else
            Some
              (Provider.make_message ~role
                 ~content:(truncate_content content 2000)))
      msgs
  in
  (* Phase 2: merge consecutive same-role messages. *)
  let merged =
    List.fold_left
      (fun acc (m : Provider.message) ->
        match acc with
        | (prev : Provider.message) :: rest when prev.role = m.role ->
            { prev with content = prev.content ^ "\n" ^ m.content } :: rest
        | _ -> m :: acc)
      [] transformed
    |> List.rev
  in
  merged
