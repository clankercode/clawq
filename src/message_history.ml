let collect_tool_call_ids msgs =
  List.fold_left
    (fun acc (m : Provider.message) ->
      if m.role = "assistant" && m.tool_calls <> [] then
        List.fold_left
          (fun acc (tc : Provider.tool_call) ->
            if List.mem tc.id acc then acc else tc.id :: acc)
          acc m.tool_calls
      else acc)
    [] msgs

let collect_tool_result_ids msgs =
  List.fold_left
    (fun acc (m : Provider.message) ->
      match m.tool_call_id with
      | Some id when m.role = "tool" ->
          if List.mem id acc then acc else id :: acc
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
        let items =
          match arr with `List items -> items | _ -> [ arr ]
        in
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
        | Some id -> List.mem id call_ids
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
