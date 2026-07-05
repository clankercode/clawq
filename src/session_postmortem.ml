let spawn_postmortem_agent_fn :
    (Session_core.t ->
    stuck_history:Provider.message list ->
    session_key:string ->
    reason:string ->
    ?db:Sqlite3.db ->
    unit ->
    unit Lwt.t)
    ref =
  ref (fun _mgr ~stuck_history:_ ~session_key:_ ~reason:_ ?db:_ () ->
      Lwt.return_unit)

(* B612 round 2: derive a stable pattern fingerprint from the reason string.
   Each known Stuck_detector prefix has its own differentiator: for tool-based
   signals it is the tool name; for SameErrorString it is the first 60 chars of
   the error message; for NearMaxIters there is no differentiator, so the prefix
   alone collapses repeats. *)
let pattern_key_for_reason (reason : string) : string =
  let starts_with prefix =
    String.length reason >= String.length prefix
    && String.sub reason 0 (String.length prefix) = prefix
  in
  let extract_quoted_after anchor =
    match Str.search_forward (Str.regexp_string anchor) reason 0 with
    | exception Not_found -> None
    | i -> (
        let start = i + String.length anchor in
        let len = String.length reason in
        if start >= len || reason.[start] <> '"' then None
        else
          let body_start = start + 1 in
          match String.index_from_opt reason body_start '"' with
          | None -> None
          | Some stop ->
              let s = String.sub reason body_start (stop - body_start) in
              if String.trim s = "" then None else Some s)
  in
  let suffix_after anchor =
    match Str.search_forward (Str.regexp_string anchor) reason 0 with
    | exception Not_found -> None
    | i ->
        let start = i + String.length anchor in
        let len = String.length reason in
        if start >= len then None
        else
          let tail = String.sub reason start (len - start) in
          let tail = String.trim tail in
          if tail = "" then None else Some tail
  in
  let truncate s n = if String.length s <= n then s else String.sub s 0 n in
  if starts_with "ConsecutiveErrors" then
    let tool = extract_quoted_after "from " in
    "ConsecutiveErrors:" ^ Option.value tool ~default:"unknown"
  else if starts_with "RepeatedToolCall" then
    let tool = extract_quoted_after ": " in
    "RepeatedToolCall:" ^ Option.value tool ~default:"unknown"
  else if starts_with "SameErrorString" then
    let msg = suffix_after "times: " in
    let key_suffix =
      match msg with None -> "unknown" | Some m -> truncate m 60
    in
    "SameErrorString:" ^ key_suffix
  else if starts_with "NearMaxIters" then "NearMaxIters"
  else truncate reason 60

let spawn_postmortem_agent mgr ~stuck_history ~session_key ~reason ?db () =
  let pm_cfg = mgr.Session_core.config.postmortem in
  if not pm_cfg.enabled then begin
    Logs.info (fun m ->
        m
          "postmortem disabled in config; suppressing launch (session=%s, \
           reason=%s)"
          session_key reason);
    Lwt.return_unit
  end
  else
    let root_key = Session_core.root_postmortem_session_key session_key in
    if root_key <> session_key then begin
      Logs.warn (fun m ->
          m
            "Suppressing recursive postmortem launch for session %s (root=%s, \
             reason=%s)"
            session_key root_key reason);
      Lwt.return_unit
    end
    else
      let pattern = pattern_key_for_reason reason in
      let breaker_key = (root_key, pattern) in
      if Hashtbl.mem mgr.Session_core.postmortem_circuit_breakers breaker_key
      then begin
        Logs.warn (fun m ->
            m
              "Postmortem circuit breaker open for session %s pattern %s; \
               suppressing additional launch (reason=%s)"
              root_key pattern reason);
        Lwt.return_unit
      end
      else begin
        Hashtbl.replace mgr.Session_core.postmortem_circuit_breakers breaker_key
          ();
        !spawn_postmortem_agent_fn mgr ~stuck_history ~session_key ~reason ?db
          ()
      end
