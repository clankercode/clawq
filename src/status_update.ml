(** Lifecycle event emitted by the handler when a tool call starts or completes.
    Connectors use this for side effects (reactions, detail accumulation, error
    notifications) without duplicating status-rendering logic. *)
type tool_event =
  | Tool_started of { id : string; name : string; summary : string option }
  | Tool_completed of {
      id : string;
      name : string;
      result : string;
      is_error : bool;
      summary : string option;
      duration_secs : float option;
    }

type error_detail = {
  id : string;
  name : string;
  emoji : string;
  summary : string option;
  duration_secs : float option;
  result : string;
}
(** Error detail payload emitted for failed tools. Connectors use this to send
    standalone error messages with full context. *)

type handler = {
  on_chunk : Provider.stream_event -> unit Lwt.t;
  finalize : unit -> unit Lwt.t;
  get_thinking : unit -> string;
  reset : unit -> unit Lwt.t;
      (** Finalize the current status group and start a fresh one. Used when a
          mid-turn user message arrives so that subsequent tool calls get
          visually separated from the pre-injection batch. *)
  on_tool_event : tool_event -> unit Lwt.t;
      (** Called after Status_message has processed a ToolStart or ToolResult.
          Default: no-op. *)
  on_error_detail : error_detail -> unit Lwt.t;
      (** Called for failed tools with full context for standalone error
          rendering. Default: no-op. *)
}

type strategy = Consolidated | Individual | Buffered

(** Build stream-visibility settings for a turn. Low-volume rooms suppress
    thinking and tool start/success chatter but still surface tool errors as
    alerts ([show_tool_calls=true] with notify flags off). *)
let visibility_settings ~(agent_defaults : Runtime_config.agent_defaults)
    ?(low_volume = false) () : Stream_visibility.settings =
  if low_volume then
    {
      show_thinking = false;
      show_tool_calls = true;
      notify_tool_starts = false;
      notify_tool_successes = false;
    }
  else
    {
      show_thinking = agent_defaults.show_thinking;
      show_tool_calls = agent_defaults.show_tool_calls;
      notify_tool_starts = false;
      notify_tool_successes = true;
    }

(** Whether tool status should be presented at all for this turn. Low-volume
    rooms still deliver error alerts via [visibility_settings] but never use
    consolidated/buffered multi-tool status cards. *)
let shows_tool_status ~(agent_defaults : Runtime_config.agent_defaults)
    ?(low_volume = false) () =
  (not low_volume) && agent_defaults.show_tool_calls

let select_strategy ~(agent_defaults : Runtime_config.agent_defaults)
    ~capabilities ?(low_volume = false) () =
  if low_volume then Individual
  else if
    agent_defaults.show_tool_calls
    && agent_defaults.tool_status_mode = "consolidated"
  then
    match capabilities with
    | Some (caps : Connector_capabilities.t) -> (
        match caps.can_edit with
        | Connector_capabilities.Edit_in_place | Delete_and_resend ->
            Consolidated
        | No_edit -> Buffered)
    | None -> Consolidated
  else Individual

let no_op_tool_event _ = Lwt.return_unit
let no_op_error_detail _ = Lwt.return_unit

let make_handler ~strategy ~notifier_factory ~notify
    ~(agent_defaults : Runtime_config.agent_defaults) ~parse_mode
    ?(low_volume = false) ?(on_tool_event = no_op_tool_event)
    ?(on_error_detail = no_op_error_detail) () =
  match strategy with
  | Consolidated -> (
      match notifier_factory with
      | Some factory ->
          let sm = ref (factory ()) in
          let thinking_buf = Buffer.create 256 in
          let on_chunk = function
            | Provider.ToolStart { id; name; arguments } ->
                let open Lwt.Syntax in
                let summary =
                  Stream_visibility.summarize_tool_arguments ~name arguments
                in
                let* () = Status_message.tool_start !sm ~id ~name ~summary in
                on_tool_event (Tool_started { id; name; summary })
            | Provider.ToolResult { id; name; result; is_error } ->
                let open Lwt.Syntax in
                let* () =
                  Status_message.tool_result !sm ~id ~name ~result ~is_error
                in
                let info = Status_message.get_tool_info !sm ~id in
                let summary =
                  Option.bind info (fun (e : Status_message.tool_entry) ->
                      e.summary)
                in
                let duration_secs =
                  Option.bind info (fun (e : Status_message.tool_entry) ->
                      Option.map (fun fin -> fin -. e.started_at) e.finished_at)
                in
                let* () =
                  on_tool_event
                    (Tool_completed
                       { id; name; result; is_error; summary; duration_secs })
                in
                if is_error then begin
                  let emoji =
                    Option.fold ~none:"\xE2\x9C\x97"
                      ~some:(fun (e : Status_message.tool_entry) -> e.emoji)
                      info
                  in
                  on_error_detail
                    { id; name; emoji; summary; duration_secs; result }
                end
                else Lwt.return_unit
            | Provider.ThinkingDelta text ->
                if (not low_volume) && agent_defaults.show_thinking then begin
                  Buffer.add_string thinking_buf text;
                  Status_message.update_thinking !sm text
                end
                else Lwt.return_unit
            | Provider.ToolOutputDelta { id; chunk } ->
                Status_message.tool_output_delta !sm ~id ~chunk
            | Provider.Delta _ | Provider.ToolCallDelta _ | Provider.Done ->
                Lwt.return_unit
          in
          let finalize () = Status_message.finalize !sm in
          let get_thinking () = Buffer.contents thinking_buf in
          let reset () =
            let open Lwt.Syntax in
            let* () = Status_message.finalize !sm in
            sm := factory ();
            Buffer.clear thinking_buf;
            Lwt.return_unit
          in
          {
            on_chunk;
            finalize;
            get_thinking;
            reset;
            on_tool_event;
            on_error_detail;
          }
      | None ->
          (* Fall back to Individual if no factory available *)
          let visibility = Stream_visibility.create () in
          let settings = visibility_settings ~agent_defaults ~low_volume () in
          let on_chunk chunk =
            Stream_visibility.on_chunk visibility ~settings ~notify chunk
          in
          let finalize () = Lwt.return_unit in
          let get_thinking () = Stream_visibility.thinking_text visibility in
          let reset () = Lwt.return_unit in
          {
            on_chunk;
            finalize;
            get_thinking;
            reset;
            on_tool_event;
            on_error_detail;
          })
  | Individual ->
      let visibility = Stream_visibility.create () in
      let settings = visibility_settings ~agent_defaults ~low_volume () in
      let on_chunk chunk =
        Stream_visibility.on_chunk visibility ~settings ~notify chunk
      in
      let finalize () = Lwt.return_unit in
      let get_thinking () = Stream_visibility.thinking_text visibility in
      let reset () = Lwt.return_unit in
      {
        on_chunk;
        finalize;
        get_thinking;
        reset;
        on_tool_event;
        on_error_detail;
      }
  | Buffered ->
      let thinking_buf = Buffer.create 256 in
      let tool_events = ref [] in
      let on_chunk = function
        | Provider.ToolStart { id; name; arguments } ->
            if low_volume then Lwt.return_unit
            else
              let summary =
                Stream_visibility.summarize_tool_arguments ~name arguments
              in
              tool_events := `Start (id, name, summary) :: !tool_events;
              Lwt.return_unit
        | Provider.ToolResult { id; name; result; is_error } ->
            (* Low-volume: only keep failures (alerts); drop routine successes. *)
            if low_volume && not is_error then Lwt.return_unit
            else begin
              tool_events :=
                `Result (id, name, result, is_error) :: !tool_events;
              Lwt.return_unit
            end
        | Provider.ThinkingDelta text ->
            if (not low_volume) && agent_defaults.show_thinking then
              Buffer.add_string thinking_buf text;
            Lwt.return_unit
        | Provider.Delta _ | Provider.ToolCallDelta _
        | Provider.ToolOutputDelta _ | Provider.Done ->
            Lwt.return_unit
      in
      let finalize () =
        let events = List.rev !tool_events in
        if events = [] then Lwt.return_unit
        else
          let connector = Format_adapter.of_parse_mode parse_mode in
          let buf = Buffer.create 256 in
          let successes = ref 0 in
          let failures = ref 0 in
          List.iter
            (function
              | `Result (_id, name, result, is_error) ->
                  let emoji = Stream_visibility.tool_emoji name in
                  if is_error then begin
                    incr failures;
                    let detail =
                      Stream_visibility.truncate_text ~max_chars:200 result
                    in
                    Buffer.add_string buf
                      (Printf.sprintf "\xE2\x9C\x97 %s %s \xE2\x80\x94 %s\n"
                         emoji
                         (Format_adapter.bold connector name)
                         (Format_adapter.italic connector detail))
                  end
                  else begin
                    incr successes;
                    let preview =
                      Stream_visibility.summarize_tool_result ~name result
                    in
                    let preview_part =
                      match preview with
                      | Some p ->
                          Printf.sprintf " \xE2\x86\x92 %s"
                            (Format_adapter.italic connector p)
                      | None -> ""
                    in
                    Buffer.add_string buf
                      (Printf.sprintf "\xE2\x9C\x93 %s %s%s\n" emoji
                         (Format_adapter.bold connector name)
                         preview_part)
                  end
              | `Start _ -> ())
            events;
          let result = Buffer.contents buf in
          let trimmed =
            let len = String.length result in
            if len > 0 && result.[len - 1] = '\n' then
              String.sub result 0 (len - 1)
            else result
          in
          if trimmed <> "" then notify trimmed else Lwt.return_unit
      in
      let get_thinking () = Buffer.contents thinking_buf in
      let reset () =
        tool_events := [];
        Buffer.clear thinking_buf;
        Lwt.return_unit
      in
      {
        on_chunk;
        finalize;
        get_thinking;
        reset;
        on_tool_event;
        on_error_detail;
      }
