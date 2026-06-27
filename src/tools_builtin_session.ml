(* Session/summary-oriented agent tool definitions extracted from
   tools_builtin_util.ml: thread_summary, unsummarize, send_to_session.
   Re-exported via `include Tools_builtin_session`. *)

let thread_summary ~db ~(config : Runtime_config.t) =
  {
    Tool.name = "thread_summary";
    description =
      "Get a concise dot-point summary of what a session is working on. \
       Focuses on recent activity. Useful for understanding a thread at a \
       glance.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "session_id",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Session key to summarize (required)" );
                    ] );
              ] );
          ("required", `List [ `String "session_id" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Lwt.Syntax in
        let open Yojson.Safe.Util in
        let session_id =
          try args |> member "session_id" |> to_string with _ -> ""
        in
        if session_id = "" then
          Lwt.return
            "Error: session_id is required. Provide the session key to \
             summarize (e.g., \"default\" or a channel-specific session key)."
        else
          let all_msgs = Memory.load_history ~db ~session_key:session_id in
          if all_msgs = [] then
            Lwt.return
              (Printf.sprintf
                 "No messages found for session '%s'. Check the session key or \
                  use 'session list' to see active sessions."
                 session_id)
          else
            let n = List.length all_msgs in
            let window = min 40 n in
            let recent = List.filteri (fun i _ -> i >= n - window) all_msgs in
            let conversation =
              List.map
                (fun (m : Provider.message) ->
                  let snippet =
                    if String.length m.content > 800 then
                      String.sub m.content 0 800 ^ "..."
                    else m.content
                  in
                  Printf.sprintf "[%s]: %s" m.role snippet)
                recent
              |> String.concat "\n"
            in
            let prompt =
              "Summarize what this session is working on. Be concise. Use \
               dot-point form. Focus on the most recent activity and current \
               state. Max 10 bullet points.\n\n" ^ conversation
            in
            let obs_config = Session_observer.observer_config_for ~config in
            let messages =
              [
                Provider.make_message ~role:"system"
                  ~content:
                    "You are a session summarizer. Output a concise dot-point \
                     summary of the session's current focus and recent \
                     actions. No preamble.";
                Provider.make_message ~role:"user" ~content:prompt;
              ]
            in
            Lwt.catch
              (fun () ->
                let* response =
                  Provider.complete ~config:obs_config ~messages ()
                in
                match response with
                | Provider.Text { content; _ } ->
                    Lwt.return (String.trim content)
                | Provider.ToolCalls _ ->
                    Lwt.return
                      "Summary unavailable (unexpected tool call response)")
              (fun exn ->
                Lwt.return
                  ("Error generating summary: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let unsummarize ~db =
  {
    Tool.name = "unsummarize";
    description =
      "Retrieve the original (unsummarized) content of a previously summarized \
       tool result. Use this when you need the full output that was \
       automatically summarized. Usually the summary is sufficient — only call \
       this when you need exact text, specific line ranges, or data the \
       summary explicitly notes was omitted.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "summary_id",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "The summary ID, e.g. sum_abc123def456 (required)" );
                    ] );
                ( "lines",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "Max lines to return (default: 100)" );
                    ] );
                ( "offset",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Line offset to start from (default: 0). Ignored \
                           when head_and_tail=true." );
                    ] );
                ( "with_context",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Include context available during summarization \
                           (default: false)" );
                    ] );
                ( "head_and_tail",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Return first and last N lines instead of contiguous \
                           slice (default: false)" );
                    ] );
              ] );
          ("required", `List [ `String "summary_id" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let summary_id =
          try args |> member "summary_id" |> to_string with _ -> ""
        in
        if summary_id = "" then
          Lwt.return
            "Error: parameter \"summary_id\" is required. Provide the summary \
             ID from the [Auto-summarized] header (e.g., sum_abc123def456)."
        else
          let lines = try args |> member "lines" |> to_int with _ -> 100 in
          let offset = try args |> member "offset" |> to_int with _ -> 0 in
          let with_context =
            try args |> member "with_context" |> to_bool with _ -> false
          in
          let head_and_tail =
            try args |> member "head_and_tail" |> to_bool with _ -> false
          in
          match Summary_store.find ~db ~summary_id with
          | None ->
              Lwt.return
                (Printf.sprintf
                   "Error: summary ID %S not found. The original content may \
                    have been purged (TTL expired) or the ID may be incorrect. \
                    Check the summary_id from the [Auto-summarized] header."
                   summary_id)
          | Some record ->
              let all_lines =
                String.split_on_char '\n' record.original_content
                |> Array.of_list
              in
              let total_lines = Array.length all_lines in
              let lines = max 1 (min lines total_lines) in
              let result_text =
                if head_and_tail then
                  if total_lines <= lines * 2 then
                    (* Content fits — return all *)
                    record.original_content
                  else
                    let head =
                      Array.sub all_lines 0 lines
                      |> Array.to_list |> String.concat "\n"
                    in
                    let tail =
                      Array.sub all_lines (total_lines - lines) lines
                      |> Array.to_list |> String.concat "\n"
                    in
                    let skipped = total_lines - (lines * 2) in
                    Printf.sprintf "%s\n--- (skipped %d lines) ---\n%s" head
                      skipped tail
                else
                  let offset = max 0 (min offset (total_lines - 1)) in
                  let avail = total_lines - offset in
                  let n = min lines avail in
                  Array.sub all_lines offset n
                  |> Array.to_list |> String.concat "\n"
              in
              let from_line, to_line =
                if head_and_tail then
                  if total_lines <= lines * 2 then (0, total_lines - 1)
                  else (0, total_lines - 1) (* head + skipped + tail *)
                else
                  let offset = max 0 (min offset (total_lines - 1)) in
                  let avail = total_lines - offset in
                  let n = min lines avail in
                  (offset, offset + n - 1)
              in
              let header =
                if head_and_tail && total_lines > lines * 2 then
                  Printf.sprintf
                    "[Original for %s: %d lines, %d bytes, showing lines 0-%d \
                     and %d-%d]"
                    summary_id total_lines record.original_bytes (lines - 1)
                    (total_lines - lines) (total_lines - 1)
                else
                  Printf.sprintf
                    "[Original for %s: %d lines, %d bytes, showing lines %d-%d]"
                    summary_id total_lines record.original_bytes from_line
                    to_line
              in
              let context_section =
                if with_context && record.context_snippet <> "" then
                  Printf.sprintf "\n\n[Context at summarization time:]\n%s"
                    record.context_snippet
                else ""
              in
              Lwt.return
                (Printf.sprintf "%s\n%s%s" header result_text context_section));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

(* B679: send_to_session — cross-session messaging agent tool.
   Allows an agent in one session to send a message to another session's
   channel without waking the target agent (by default). Used for cron-
   driven briefings to deliver results to a DM session. *)
let send_to_session ~(session_mgr : Session.t option) ?(db = None) () =
  (* Rate limit state: session_key -> (count, window_start). Max 20 per hour. *)
  let rate_limit_state : (string, int * float) Hashtbl.t = Hashtbl.create 32 in
  let max_sends_per_hour = 20 in
  let window_seconds = 3600.0 in
  let check_rate_limit ~caller_key =
    let now = Unix.gettimeofday () in
    match Hashtbl.find_opt rate_limit_state caller_key with
    | None ->
        Hashtbl.replace rate_limit_state caller_key (1, now);
        true
    | Some (count, window_start) ->
        if now -. window_start > window_seconds then (
          Hashtbl.replace rate_limit_state caller_key (1, now);
          true)
        else if count >= max_sends_per_hour then false
        else (
          Hashtbl.replace rate_limit_state caller_key (count + 1, window_start);
          true)
  in
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "session_id",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String
                        "Target session key to send the message to (required)"
                    );
                  ] );
              ( "message",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "Message text to send (required)");
                  ] );
              ( "wake_agent",
                `Assoc
                  [
                    ("type", `String "boolean");
                    ( "description",
                      `String
                        "If true, trigger the target session's agent loop. \
                         Default: false (silent delivery)." );
                  ] );
              ( "store_in_history",
                `Assoc
                  [
                    ("type", `String "boolean");
                    ( "description",
                      `String
                        "If true, persist the message in the target session's \
                         chat history so the agent sees it on next wake. \
                         Default: true." );
                  ] );
            ] );
        ("required", `List [ `String "session_id"; `String "message" ]);
        ("additionalProperties", `Bool false);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"send_to_session" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "send_to_session";
    description =
      "Send a message to another session's channel. By default the message \
       arrives silently without waking the target session's agent (like a \
       notification). Set wake_agent=true to trigger the agent loop. Use \
       store_in_history=true (default) to persist the message so the target \
       agent sees it on next wake. Rate limited to 20 sends per hour per \
       caller session.";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        let open Lwt.Syntax in
        let open Yojson.Safe.Util in
        let caller_key =
          match context with Some ctx -> ctx.Tool.session_key | None -> None
        in
        (* Rate limit check *)
        let* rate_err =
          match caller_key with
          | Some key ->
              if not (check_rate_limit ~caller_key:key) then
                Lwt.return
                  (Printf.sprintf
                     "Error: rate limit exceeded (max %d sends per hour). Wait \
                      before sending more cross-session messages."
                     max_sends_per_hour)
              else Lwt.return ""
          | None -> Lwt.return ""
        in
        if rate_err <> "" then Lwt.return rate_err
        else
          let session_id =
            try args |> member "session_id" |> to_string with _ -> ""
          in
          let message =
            try args |> member "message" |> to_string with _ -> ""
          in
          if session_id = "" then
            Lwt.return
              (param_err "parameter 'session_id' must be a non-empty string")
          else if message = "" then
            Lwt.return
              (param_err "parameter 'message' must be a non-empty string")
          else
            let wake_agent =
              try args |> member "wake_agent" |> to_bool with _ -> false
            in
            let store_in_history =
              try args |> member "store_in_history" |> to_bool with _ -> true
            in
            match session_mgr with
            | None ->
                Lwt.return
                  "Error: no session manager available (send_to_session \
                   requires a live daemon)."
            | Some mgr -> (
                let sanitized_id = Session.sanitize_session_key session_id in
                (* Check target session channel routing *)
                let channel_info =
                  match db with
                  | Some db ->
                      Memory.get_session_channel ~db ~session_key:sanitized_id
                  | None -> None
                in
                (match channel_info with
                | Some _ -> ()
                | None ->
                    (* Also try parsing from key format directly *)
                    ());
                let channel, channel_id =
                  match Restart_notify.parse_channel_from_key sanitized_id with
                  | Some (ch, ch_id) -> (Some ch, Some ch_id)
                  | None -> (None, None)
                in
                if wake_agent then
                  (* Full Session.turn — wakes agent in target session *)
                  Lwt.catch
                    (fun () ->
                      let open Lwt.Syntax in
                      let* response =
                        Session.turn mgr ~key:sanitized_id ~message ?channel
                          ?channel_id ()
                      in
                      if Session.should_suppress_response response then
                        Lwt.return "Message queued for busy target session."
                      else
                        Lwt.return
                          "Message sent and target session agent triggered.")
                    (fun exn ->
                      Lwt.return
                        (Printf.sprintf "Error sending to session %s: %s"
                           sanitized_id (Printexc.to_string exn)))
                else
                  (* Silent delivery via notifier (does not wake agent) *)
                  let notify_opt =
                    match
                      Session_core.find_silent_channel_notifier mgr
                        ~key:sanitized_id
                    with
                    | Some _ as s -> s
                    | None ->
                        Session_core.find_registered_notifier mgr
                          ~key:sanitized_id
                  in
                  match notify_opt with
                  | Some notify ->
                      Lwt.catch
                        (fun () ->
                          let open Lwt.Syntax in
                          let* () = notify message in
                          (* Optionally persist to message history *)
                          let* () =
                            if store_in_history then
                              match db with
                              | Some db ->
                                  let msg =
                                    Provider.make_message ~role:"event"
                                      ~content:
                                        ("[cross-session message]\n" ^ message)
                                  in
                                  Memory.store_message ~db
                                    ~session_key:sanitized_id msg;
                                  Lwt.return_unit
                              | None -> Lwt.return_unit
                            else Lwt.return_unit
                          in
                          Lwt.return
                            (Printf.sprintf
                               "Message sent silently to session %s. Agent in \
                                target session will see this on next wake \
                                (store_in_history=%b)."
                               sanitized_id store_in_history))
                        (fun exn ->
                          Lwt.return
                            (Printf.sprintf "Error sending to session %s: %s"
                               sanitized_id (Printexc.to_string exn)))
                  | None ->
                      (* No notifier registered — try Session.turn which will
                       use channel routing if available *)
                      Lwt.catch
                        (fun () ->
                          let open Lwt.Syntax in
                          let* response =
                            Session.turn mgr ~key:sanitized_id ~message ?channel
                              ?channel_id ()
                          in
                          let* () =
                            if store_in_history then
                              match db with
                              | Some db ->
                                  let msg =
                                    Provider.make_message ~role:"event"
                                      ~content:
                                        ("[cross-session message]\n" ^ message)
                                  in
                                  Memory.store_message ~db
                                    ~session_key:sanitized_id msg;
                                  Lwt.return_unit
                              | None -> Lwt.return_unit
                            else Lwt.return_unit
                          in
                          Lwt.return
                            (Printf.sprintf
                               "Message sent to session %s via direct turn (no \
                                channel notifier registered)."
                               sanitized_id))
                        (fun exn ->
                          Lwt.return
                            (Printf.sprintf
                               "Error: could not send to session %s — no \
                                channel notifier registered and Session.turn \
                                failed: %s"
                               sanitized_id (Printexc.to_string exn)))));
    invoke_stream = None;
    risk_level = Tool.Medium;
    deferred = false;
  }
