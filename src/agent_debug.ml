let response_fields response =
  match response with
  | Provider.Text { usage; model; _ } -> (usage, model, 0)
  | Provider.ToolCalls { usage; model; calls; _ } ->
      (usage, model, List.length calls)

let notify ?on_llm_call_debug ~provider ~duration_s response =
  match on_llm_call_debug with
  | None -> Lwt.return_unit
  | Some send ->
      let usage, model, tool_call_count = response_fields response in
      send { Session_debug.provider; model; duration_s; usage; tool_call_count }
