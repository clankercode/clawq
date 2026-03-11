let process_tool_result ~(config : Runtime_config.t) ~(db : Sqlite3.db option)
    ~(session_key : string option) ~(tool_name : string)
    ~(history : Provider.message list) ~(raw_result : string) =
  let open Lwt.Syntax in
  let sc = config.summarizer in
  (* Stage 1: p1 truncation is handled inside maybe_summarize *)
  (* Stage 2: summarization *)
  let* summarize_result =
    Summarizer.maybe_summarize ~config ~db ~session_key ~tool_name ~history
      ~original:raw_result ()
  in
  (* Stage 3: p2 truncation — ensure final result fits history limit *)
  let final =
    match summarize_result with
    | Summarizer.Passthrough s ->
        Summarizer.truncate_for_history s ~max_chars:sc.p2_max_chars
    | Summarizer.Summarized { content; _ } ->
        Summarizer.truncate_for_history content ~max_chars:sc.p2_max_chars
    | Summarizer.Fallback_truncated s ->
        (* Already truncated to p2 inside the summarizer *)
        s
  in
  Lwt.return final
