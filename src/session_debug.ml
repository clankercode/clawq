type llm_call = {
  provider : string;
  model : string;
  duration_s : float;
  usage : (int * int * int) option;
  tool_call_count : int;
}

let bool_int enabled = if enabled then 1L else 0L

let set_enabled ~db ~session_key ~enabled =
  let sql =
    "INSERT INTO session_state (session_key, debug_enabled) VALUES (?, ?) ON \
     CONFLICT(session_key) DO UPDATE SET debug_enabled = \
     excluded.debug_enabled"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (bool_int enabled)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          Logs.warn (fun m ->
              m "Failed to set session debug mode: %s" (Sqlite3.Rc.to_string rc)))

let enabled ~db ~session_key =
  let sql =
    "SELECT COALESCE(debug_enabled, 0) FROM session_state WHERE session_key = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> n <> 0L
          | _ -> false)
      | _ -> false)

let format_usage = function
  | None ->
      "tokens=unknown prompt=unknown output+reasoning=unknown cached=unknown \
       cached_pct=unknown"
  | Some (prompt_tokens, completion_tokens, cached_tokens) ->
      let total_tokens = prompt_tokens + completion_tokens in
      let cached_pct =
        if prompt_tokens <= 0 then 0.0
        else 100.0 *. float_of_int cached_tokens /. float_of_int prompt_tokens
      in
      Printf.sprintf
        "tokens=%d prompt=%d output+reasoning=%d cached=%d cached_pct=%.0f%%"
        total_tokens prompt_tokens completion_tokens cached_tokens cached_pct

let format_llm_call call =
  Printf.sprintf
    "debug: llm provider=%s model=%s duration=%.2fs %s tool_calls=%d"
    call.provider call.model call.duration_s (format_usage call.usage)
    call.tool_call_count
