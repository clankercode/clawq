(* F10 conformance oracles: compare Coq-extracted AgentLoop with native OCaml implementations *)

(* Convert Provider.message to Clawq_core.AgentLoop.message for conformance testing.
   Only preserves information needed for tool-group integrity: role, tool_calls,
   tool_call_id. Event messages are encoded into user messages with a sentinel so
   trim/integrity passes preserve their role across the Coq roundtrip. *)

let event_role_prefix = "__clawq_event__:"

(* Build a mapping from tool_call_id to function_name from provider messages.
   Used to restore tool names when converting back from Coq representation,
   since Coq ToolResultMsg does not preserve the name field. *)
let build_tool_name_map (messages : Provider.message list) :
    (string * string) list =
  List.filter_map
    (fun (m : Provider.message) ->
      match m.role with
      | "assistant" when m.tool_calls <> [] ->
          Some
            (List.map
               (fun (tc : Provider.tool_call) -> (tc.id, tc.function_name))
               m.tool_calls)
      | _ -> None)
    messages
  |> List.flatten

let provider_to_coq_message (m : Provider.message) :
    Clawq_core.AgentLoop.message =
  match m.role with
  | "event" -> Clawq_core.AgentLoop.UserMsg (event_role_prefix ^ m.content)
  | "user" -> Clawq_core.AgentLoop.UserMsg m.content
  | "assistant" when m.tool_calls <> [] ->
      let coq_calls =
        List.map
          (fun (tc : Provider.tool_call) ->
            {
              Clawq_core.AgentLoop.tc_id = tc.id;
              Clawq_core.AgentLoop.tc_name = tc.function_name;
            })
          m.tool_calls
      in
      Clawq_core.AgentLoop.AssistantToolCallsMsg coq_calls
  | "assistant" -> Clawq_core.AgentLoop.AssistantMsg m.content
  | "tool" -> (
      match m.tool_call_id with
      | Some id -> Clawq_core.AgentLoop.ToolResultMsg (id, m.content)
      | None -> Clawq_core.AgentLoop.ToolResultMsg ("unknown", m.content))
  | _ -> Clawq_core.AgentLoop.UserMsg m.content

(* Convert Clawq_core.AgentLoop.message back to Provider.message.
   Uses minimal/dummy values for fields not present in Coq type. *)

let coq_to_provider_message (m : Clawq_core.AgentLoop.message) :
    Provider.message =
  match m with
  | Clawq_core.AgentLoop.UserMsg content ->
      if String.starts_with ~prefix:event_role_prefix content then
        let prefix_len = String.length event_role_prefix in
        let event_content =
          String.sub content prefix_len (String.length content - prefix_len)
        in
        Provider.make_message ~role:"event" ~content:event_content
      else Provider.make_message ~role:"user" ~content
  | Clawq_core.AgentLoop.AssistantMsg content ->
      Provider.make_message ~role:"assistant" ~content
  | Clawq_core.AgentLoop.AssistantToolCallsMsg calls ->
      let provider_calls =
        List.map
          (fun (tc : Clawq_core.AgentLoop.tool_call) ->
            {
              Provider.id = tc.tc_id;
              Provider.function_name = tc.tc_name;
              Provider.arguments = "{}";
            })
          calls
      in
      {
        Provider.role = "assistant";
        Provider.content = "";
        Provider.content_parts = [];
        Provider.tool_calls = provider_calls;
        Provider.tool_call_id = None;
        Provider.name = None;
        Provider.provider_response_items_json = None;
        Provider.thinking = None;
      }
  | Clawq_core.AgentLoop.ToolResultMsg (id, content) ->
      {
        Provider.role = "tool";
        Provider.content;
        Provider.content_parts = [];
        Provider.tool_calls = [];
        Provider.tool_call_id = Some id;
        Provider.name = None;
        Provider.provider_response_items_json = None;
        Provider.thinking = None;
      }

(* Convert a list of messages *)
let provider_to_coq_history msgs = List.map provider_to_coq_message msgs
let coq_to_provider_history msgs = List.map coq_to_provider_message msgs

(* Variant of coq_to_provider_message that restores tool names from a mapping.
   Used for production roundtrips where tool name preservation is required. *)
let coq_to_provider_message_with_names ~tool_name_map
    (m : Clawq_core.AgentLoop.message) : Provider.message =
  match m with
  | Clawq_core.AgentLoop.UserMsg content ->
      if String.starts_with ~prefix:event_role_prefix content then
        let prefix_len = String.length event_role_prefix in
        let event_content =
          String.sub content prefix_len (String.length content - prefix_len)
        in
        Provider.make_message ~role:"event" ~content:event_content
      else Provider.make_message ~role:"user" ~content
  | Clawq_core.AgentLoop.AssistantMsg content ->
      Provider.make_message ~role:"assistant" ~content
  | Clawq_core.AgentLoop.AssistantToolCallsMsg calls ->
      let provider_calls =
        List.map
          (fun (tc : Clawq_core.AgentLoop.tool_call) ->
            {
              Provider.id = tc.tc_id;
              Provider.function_name = tc.tc_name;
              Provider.arguments = "{}";
            })
          calls
      in
      {
        Provider.role = "assistant";
        Provider.content = "";
        Provider.content_parts = [];
        Provider.tool_calls = provider_calls;
        Provider.tool_call_id = None;
        Provider.name = None;
        Provider.provider_response_items_json = None;
        Provider.thinking = None;
      }
  | Clawq_core.AgentLoop.ToolResultMsg (id, content) ->
      let name = List.assoc_opt id tool_name_map in
      {
        Provider.role = "tool";
        Provider.content;
        Provider.content_parts = [];
        Provider.tool_calls = [];
        Provider.tool_call_id = Some id;
        Provider.name;
        Provider.provider_response_items_json = None;
        Provider.thinking = None;
      }

(* Convert Coq history back to provider messages, restoring tool names from
   the original provider messages. This is the production entry point that
   preserves tool names through the Coq roundtrip. *)
let coq_to_provider_history_with_names ~original_messages coq_messages =
  let tool_name_map = build_tool_name_map original_messages in
  List.map (coq_to_provider_message_with_names ~tool_name_map) coq_messages

(* Conformance oracle for ensure_tool_group_integrity.
   Returns (coq_result, native_result, equal) where equal indicates structural equality. *)

let conformance_ensure_tool_group_integrity messages =
  let native_result = Message_history.ensure_tool_group_integrity messages in
  let coq_input = provider_to_coq_history messages in
  let coq_output = Clawq_core.AgentLoop.ensure_tool_group_integrity coq_input in
  let coq_result =
    coq_to_provider_history_with_names ~original_messages:messages coq_output
  in
  let native_coq = provider_to_coq_history native_result in
  let equal = coq_output = native_coq in
  (coq_result, native_result, equal)

(* Conformance oracle for collect_tool_call_ids.
   Returns (coq_result, native_result, equal). *)

let conformance_collect_tool_call_ids messages =
  let native_result = Message_history.collect_tool_call_ids messages in
  let coq_input = provider_to_coq_history messages in
  let coq_result = Clawq_core.AgentLoop.collect_tool_call_ids coq_input in
  (* Compare sorted lists - ordering may differ between Coq and OCaml *)
  let sort_ids = List.sort String.compare in
  let equal = sort_ids coq_result = sort_ids native_result in
  (coq_result, native_result, equal)

(* Conformance oracle for collect_tool_result_ids.
   Returns (coq_result, native_result, equal). *)

let conformance_collect_tool_result_ids messages =
  let native_result = Message_history.collect_tool_result_ids messages in
  let coq_input = provider_to_coq_history messages in
  let coq_result = Clawq_core.AgentLoop.collect_tool_result_ids coq_input in
  (* Compare sorted lists - ordering may differ between Coq and OCaml *)
  let sort_ids = List.sort String.compare in
  let equal = sort_ids coq_result = sort_ids native_result in
  (coq_result, native_result, equal)

(* Conformance oracle for trim_history.
   Note: The OCaml Agent.trim_history modifies in place and also calls ensure_tool_group_integrity.
   For conformance testing, we test the pure trimming logic only. *)

let conformance_trim_history max messages =
  let native_result =
    let len = List.length messages in
    if len > max then
      let trimmed = List.filteri (fun i _ -> i < max) messages in
      Message_history.ensure_tool_group_integrity trimmed
    else messages
  in
  let coq_input = provider_to_coq_history messages in
  let coq_trimmed = Clawq_core.AgentLoop.trim_history max coq_input in
  let coq_output =
    Clawq_core.AgentLoop.ensure_tool_group_integrity coq_trimmed
  in
  let coq_result =
    coq_to_provider_history_with_names ~original_messages:messages coq_output
  in
  let native_coq = provider_to_coq_history native_result in
  let equal = coq_output = native_coq in
  (coq_result, native_result, equal)

(* Conformance oracle for force_compress_history.
   Similar to trim_history but with fixed keep count. *)

let conformance_force_compress_history keep messages =
  let native_result =
    let recent = List.filteri (fun i _ -> i < keep) messages in
    Message_history.ensure_tool_group_integrity recent
  in
  let coq_input = provider_to_coq_history messages in
  let coq_compressed =
    Clawq_core.AgentLoop.force_compress_history keep coq_input
  in
  let coq_output =
    Clawq_core.AgentLoop.ensure_tool_group_integrity coq_compressed
  in
  let coq_result =
    coq_to_provider_history_with_names ~original_messages:messages coq_output
  in
  let native_coq = provider_to_coq_history native_result in
  let equal = coq_output = native_coq in
  (coq_result, native_result, equal)

(* Assertion helpers for tests *)

let assert_conformance name (coq, native, equal) =
  if not equal then (
    Printf.eprintf "CONFORMANCE FAILURE: %s\n" name;
    Printf.eprintf "  Coq result:   %d messages\n" (List.length coq);
    Printf.eprintf "  Native result: %d messages\n" (List.length native);
    failwith (Printf.sprintf "Conformance check failed: %s" name))
  else Printf.printf "CONFORMANCE OK: %s\n" name
