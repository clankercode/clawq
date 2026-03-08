type settings = { show_thinking : bool; show_tool_calls : bool }
type t = { thinking_buf : Buffer.t; content_buf : Buffer.t }

let create () =
  { thinking_buf = Buffer.create 256; content_buf = Buffer.create 1024 }

let truncate_text ?(max_chars = 800) text =
  if String.length text <= max_chars then text
  else String.sub text 0 max_chars ^ "..."

let tool_start_message ~name ~arguments =
  if String.trim arguments = "" then "Tool call: " ^ name
  else Printf.sprintf "Tool call: %s\n%s" name arguments

let tool_result_message ~name ~result ~is_error =
  let prefix = if is_error then "Tool error" else "Tool result" in
  Printf.sprintf "%s: %s\n%s" prefix name (truncate_text result)

let thinking_message text = "Thinking:\n" ^ text

let on_chunk t ~(settings : settings) ~notify = function
  | Provider.ThinkingDelta text ->
      if settings.show_thinking then Buffer.add_string t.thinking_buf text;
      Lwt.return_unit
  | Provider.Delta text ->
      Buffer.add_string t.content_buf text;
      Lwt.return_unit
  | Provider.ToolStart { name; arguments; _ } ->
      if settings.show_tool_calls then
        notify (tool_start_message ~name ~arguments)
      else Lwt.return_unit
  | Provider.ToolResult { name; result; is_error; _ } ->
      if settings.show_tool_calls then
        notify (tool_result_message ~name ~result ~is_error)
      else Lwt.return_unit
  | Provider.ToolCallDelta _ | Provider.ToolOutputDelta _ | Provider.Done ->
      Lwt.return_unit

let thinking_text t = Buffer.contents t.thinking_buf
let content_text t = Buffer.contents t.content_buf
