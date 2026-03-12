let estimate_prev_assistant_tokens_from_history_delta ~history
    ~previous_request_history_len ~current_request_history_len =
  let estimate_message_tokens (m : Provider.message) =
    let estimate_tokens content = (String.length content + 3) / 4 in
    let content_tokens = estimate_tokens m.content in
    let tool_call_tokens =
      List.fold_left
        (fun acc (tc : Provider.tool_call) ->
          acc + estimate_tokens tc.function_name + estimate_tokens tc.arguments)
        0 m.tool_calls
    in
    content_tokens + tool_call_tokens
  in
  match previous_request_history_len with
  | None -> None
  | Some prev_len ->
      let added_messages = max 0 (current_request_history_len - prev_len) in
      if added_messages = 0 then None
      else
        let rec take n acc = function
          | _ when n <= 0 -> List.rev acc
          | [] -> List.rev acc
          | msg :: rest -> take (n - 1) (msg :: acc) rest
        in
        history |> take added_messages []
        |> List.filter (fun (msg : Provider.message) -> msg.role = "assistant")
        |> List.rev
        |> List.find_opt (fun _ -> true)
        |> Option.map estimate_message_tokens

let track_cost ~config ~(history : Provider.message list)
    ~last_request_history_len ~session_key ~db ~current_request_history_len
    response =
  let usage, model =
    match response with
    | Provider.Text { usage; model; _ } -> (usage, model)
    | Provider.ToolCalls { usage; model; _ } -> (usage, model)
  in
  match (usage, session_key) with
  | Some (pt, ct), Some sid -> (
      Cost_tracker.record_turn ~model ~prompt_tokens:pt ~completion_tokens:ct
        ~session_id:sid;
      Model_preferences.increment_usage model |> ignore;
      match db with
      | Some db ->
          let pname, _, _ = Provider.select_provider ~config () in
          let prev = Request_stats.get_prev_totals ~db ~session_key:sid in
          let added =
            match prev with
            | Some (prev_pt, prev_ct, _) ->
                let prev_assistant_tokens =
                  estimate_prev_assistant_tokens_from_history_delta ~history
                    ~previous_request_history_len:last_request_history_len
                    ~current_request_history_len
                  |> Option.value ~default:prev_ct
                in
                max 0 (pt - (prev_pt + prev_assistant_tokens))
            | None -> pt
          in
          let cache_hit =
            match prev with
            | Some (_, _, ts) when ts <> "" -> (
                try
                  let stmt =
                    Sqlite3.prepare db
                      "SELECT (strftime('%s', 'now') - strftime('%s', ?1)) < \
                       300"
                  in
                  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT ts));
                  Fun.protect
                    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                    (fun () ->
                      match Sqlite3.step stmt with
                      | Sqlite3.Rc.ROW -> (
                          match Sqlite3.column stmt 0 with
                          | Sqlite3.Data.INT 1L -> true
                          | _ -> false)
                      | _ -> false)
                with _ -> false)
            | _ -> false
          in
          let cost_usd_opt =
            match Cost_tracker.lookup_pricing model with
            | None -> None
            | Some _ ->
                Some
                  (Cost_tracker.calculate_cost_with_cache ~model
                     ~prompt_tokens:pt ~completion_tokens:ct
                     ~added_prompt_tokens:added ~cache_hit)
          in
          Request_stats.record ~db ~session_key:sid ~provider:pname ~model
            ~prompt_tokens:pt ~completion_tokens:ct ?cost_usd:cost_usd_opt
            ~added_prompt_tokens:added ()
      | None -> ())
  | _ -> ()
