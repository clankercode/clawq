type summarize_result =
  | Passthrough of string
  | Summarized of { content : string; summary_id : string; model : string }
  | Fallback_truncated of string

let default_system_prompt =
  "You are a tool-output summarizer for an AI assistant. Summarize the tool \
   output below while strictly preserving:\n\
   - All identifiers (IDs, keys, hashes, paths, URLs)\n\
   - Error messages and status codes verbatim\n\
   - Numerical values, counts, and statistics\n\
   - Structural relationships (parent/child, hierarchies)\n\
   - Names and labels\n\n\
   Do NOT add commentary or interpretation. Output ONLY the summary.\n\
   If the content is too complex or specialized for accurate summarization, \
   reply with exactly: ESCALATE"

let truncate_for_history s ~max_chars =
  if String.length s <= max_chars then s
  else
    let omitted = String.length s - max_chars in
    String.sub s 0 max_chars
    ^ Printf.sprintf
        "\n\n[truncated %d chars to keep context within model limits]" omitted

let serialize_context_messages (history : Provider.message list) ~max_msgs =
  let take_first n lst =
    let rec aux acc k = function
      | [] -> List.rev acc
      | _ when k = 0 -> List.rev acc
      | x :: rest -> aux (x :: acc) (k - 1) rest
    in
    aux [] n lst
  in
  (* history is newest-first; take first max_msgs, then reverse to chronological *)
  let recent = take_first max_msgs history in
  let chronological = List.rev recent in
  List.map
    (fun (m : Provider.message) ->
      let content_short =
        if String.length m.content > 500 then String.sub m.content 0 500 ^ "..."
        else m.content
      in
      Printf.sprintf "[%s]: %s" m.role content_short)
    chronological
  |> String.concat "\n"

let default_envelope_template_lines =
  [
    "[Auto-summarized: id={sum_id}, tool={tool_name}, model={model}, original: \
     {orig_lines} lines / {orig_bytes} bytes / ~{orig_tokens} tokens, summary: \
     {sum_lines} lines / {sum_bytes} bytes / ~{sum_tokens} tokens, at: \
     {timestamp}]";
    "";
    "{summary}";
    "";
    "[Use unsummarize(summary_id=\"{sum_id}\") to retrieve original content]";
    "[Use unsummarize(summary_id=\"{sum_id}\", head_and_tail=true) for \
     first+last 100 lines]";
  ]

let default_envelope_template =
  String.concat "\n" default_envelope_template_lines

let render_envelope_template ~summary_id ~tool_name ~model ~orig_lines
    ~orig_bytes ~orig_tokens ~sum_lines ~sum_bytes ~sum_tokens ~timestamp
    ~summary ~template =
  let replacements =
    [
      ("{sum_id}", summary_id);
      ("{tool_name}", tool_name);
      ("{model}", model);
      ("{orig_lines}", string_of_int orig_lines);
      ("{orig_bytes}", string_of_int orig_bytes);
      ("{orig_tokens}", string_of_int orig_tokens);
      ("{sum_lines}", string_of_int sum_lines);
      ("{sum_bytes}", string_of_int sum_bytes);
      ("{sum_tokens}", string_of_int sum_tokens);
      ("{timestamp}", timestamp);
      ("{summary}", summary);
    ]
  in
  let replace_placeholder acc (k, v) =
    let buf = Buffer.create (String.length acc) in
    let klen = String.length k in
    let slen = String.length acc in
    let i = ref 0 in
    while !i <= slen - klen do
      if String.sub acc !i klen = k then begin
        Buffer.add_string buf v;
        i := !i + klen
      end
      else begin
        Buffer.add_char buf acc.[!i];
        incr i
      end
    done;
    while !i < slen do
      Buffer.add_char buf acc.[!i];
      incr i
    done;
    Buffer.contents buf
  in
  List.fold_left replace_placeholder template replacements

let build_envelope ~summary_id ~tool_name ~model ~orig_lines ~orig_bytes
    ~orig_tokens ~sum_lines ~sum_bytes ~sum_tokens ~timestamp ~summary ~template
    =
  render_envelope_template ~summary_id ~tool_name ~model ~orig_lines ~orig_bytes
    ~orig_tokens ~sum_lines ~sum_bytes ~sum_tokens ~timestamp ~summary
    ~template:(Option.value template ~default:default_envelope_template)

let summarizer_config_for ~(config : Runtime_config.t) (pm : Pmodel.t) =
  {
    config with
    default_provider = None;
    agent_defaults =
      { config.agent_defaults with primary_model = Pmodel.to_string pm };
  }

let call_summarizer ?on_llm_call_debug ~config ~pm ~system_prompt ~user_content
    () =
  let open Lwt.Syntax in
  let override = summarizer_config_for ~config pm in
  let messages =
    [
      Provider.make_message ~role:"system" ~content:system_prompt;
      Provider.make_message ~role:"user" ~content:user_content;
    ]
  in
  let provider, _, _ = Provider.select_provider ~config:override () in
  let started_at = Unix.gettimeofday () in
  let* response = Provider.complete ~config:override ~messages () in
  let duration_s = Unix.gettimeofday () -. started_at in
  let* () =
    Agent_debug.notify ?on_llm_call_debug ~provider ~duration_s response
  in
  match response with
  | Provider.Text { content; usage; model; _ } ->
      Lwt.return (Ok (String.trim content, usage, model))
  | Provider.ToolCalls { model; usage; _ } ->
      (* Unexpected tool calls — treat as failure *)
      Lwt.return (Ok ("", usage, model))

let now_iso () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (1900 + tm.tm_year)
    (1 + tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let maybe_summarize ~(config : Runtime_config.t) ~(db : Sqlite3.db option)
    ~(session_key : string option) ~(tool_name : string)
    ~(history : Provider.message list) ~(original : string) ?on_llm_call_debug
    () =
  let open Lwt.Syntax in
  let sc = config.summarizer in
  (* Check if disabled *)
  if not sc.enabled then Lwt.return (Passthrough original)
  else if
    (* Check exclusion list *)
    List.mem tool_name sc.excluded_tools
  then begin
    Logs.debug (fun m -> m "[summarizer] skipping excluded tool %s" tool_name);
    Lwt.return (Passthrough original)
  end
  else if
    (* Check threshold *)
    String.length original <= sc.threshold_chars
  then begin
    Logs.debug (fun m ->
        m "[summarizer] passthrough: %s output (%d chars) below threshold (%d)"
          tool_name (String.length original) sc.threshold_chars);
    Lwt.return (Passthrough original)
  end
  else
    match (db, session_key) with
    | None, _ | _, None ->
        Logs.debug (fun m ->
            m "[summarizer] no db/session, falling back to truncation");
        Lwt.return
          (Fallback_truncated
             (truncate_for_history original ~max_chars:sc.p2_max_chars))
    | Some db, Some session_key -> (
        let summary_id = Summary_store.generate_id () in
        let p1_content =
          if String.length original > sc.p1_max_chars then
            String.sub original 0 sc.p1_max_chars
          else original
        in
        let context_snippet =
          serialize_context_messages history
            ~max_msgs:sc.context_window_messages
        in
        let workspace = Runtime_config.effective_workspace config in
        let system_prompt =
          match Agent_template.resolve "summarizer" with
          | Some tmpl when tmpl.system_prompt <> "" -> tmpl.system_prompt
          | _ ->
              let prompt_data =
                Agent_prompt_loader.load ~workspace ~agent_name:"summarizer"
                  ~default:default_system_prompt
              in
              prompt_data.system_prompt
        in
        let n_lines = Summary_store.count_lines p1_content in
        let n_bytes = String.length p1_content in
        let user_content =
          let ctx_section =
            if context_snippet = "" then ""
            else Printf.sprintf "<context>\n%s\n</context>\n" context_snippet
          in
          Printf.sprintf
            "%s<tool name=%S output_lines=%S output_bytes=%S>\n%s\n</tool>"
            ctx_section tool_name (string_of_int n_lines)
            (string_of_int n_bytes) p1_content
        in
        let try_summarize pm =
          Lwt.catch
            (fun () ->
              call_summarizer ?on_llm_call_debug ~config ~pm ~system_prompt
                ~user_content ())
            (fun exn ->
              Logs.warn (fun m ->
                  m "[summarizer] LLM call failed: %s" (Printexc.to_string exn));
              Lwt.return (Error (Printexc.to_string exn)))
        in
        let* result = try_summarize sc.model in
        let* final_result =
          match result with
          | Ok (text, usage, model_used)
            when String.starts_with ~prefix:"ESCALATE" (String.trim text) -> (
              (* Escalation requested *)
              let escalation_pm =
                match sc.escalation_model with
                | Some pm -> pm
                | None ->
                    (* Fall back to primary model *)
                    Pmodel.parse_exn config.agent_defaults.primary_model
              in
              Logs.info (fun m ->
                  m "[summarizer] escalating from %s to %s"
                    (Pmodel.to_string sc.model)
                    (Pmodel.to_string escalation_pm));
              let* esc_result = try_summarize escalation_pm in
              match esc_result with
              | Ok (esc_text, esc_usage, esc_model)
                when String.length esc_text > 0
                     && not
                          (String.starts_with ~prefix:"ESCALATE"
                             (String.trim esc_text)) ->
                  Lwt.return (Ok (esc_text, esc_usage, esc_model))
              | Ok _ ->
                  (* Double escalation or empty — record usage from first attempt *)
                  ignore usage;
                  ignore model_used;
                  Lwt.return (Error "double escalation failure")
              | Error _ as e -> Lwt.return e)
          | other -> Lwt.return other
        in
        match final_result with
        | Ok (summary_text, _usage, model_used)
          when String.length summary_text > 0 ->
            let timestamp = now_iso () in
            let orig_lines = Summary_store.count_lines original in
            let orig_bytes = String.length original in
            let orig_tokens = Summary_store.estimate_tokens original in
            let sum_lines = Summary_store.count_lines summary_text in
            let sum_bytes = String.length summary_text in
            let sum_tokens = Summary_store.estimate_tokens summary_text in
            let record : Summary_store.summary_record =
              {
                summary_id;
                session_key;
                tool_name;
                original_content = p1_content;
                summary_content = summary_text;
                context_snippet;
                original_bytes = orig_bytes;
                original_lines = orig_lines;
                original_tokens_est = orig_tokens;
                summary_bytes = sum_bytes;
                summary_lines = sum_lines;
                summary_tokens_est = sum_tokens;
                model_used;
                created_at = timestamp;
              }
            in
            (try Summary_store.store ~db record
             with exn ->
               Logs.warn (fun m ->
                   m "[summarizer] failed to store summary %s: %s" summary_id
                     (Printexc.to_string exn)));
            Logs.info (fun m ->
                m
                  "[summarizer] session=%s tool=%s original=%dB summary=%dB \
                   model=%s id=%s"
                  session_key tool_name orig_bytes sum_bytes model_used
                  summary_id);
            let envelope =
              build_envelope ~summary_id ~tool_name ~model:model_used
                ~orig_lines ~orig_bytes ~orig_tokens ~sum_lines ~sum_bytes
                ~sum_tokens ~timestamp ~summary:summary_text
                ~template:sc.envelope_template
            in
            Lwt.return
              (Summarized { content = envelope; summary_id; model = model_used })
        | Ok _ | Error _ ->
            Logs.warn (fun m ->
                m "[summarizer] falling back to truncation for tool=%s"
                  tool_name);
            Lwt.return
              (Fallback_truncated
                 (truncate_for_history original ~max_chars:sc.p2_max_chars)))
