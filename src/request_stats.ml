type summary = {
  total_cost_usd : float;
  total_prompt_tokens : int;
  total_completion_tokens : int;
  total_added_prompt_tokens : int;
  total_cached_tokens : int;
  total_turns : int;
}

type session_summary = {
  session_key : string;
  summary : summary;
  first_request : string;
  last_request : string;
}

type model_summary = { model : string; provider : string; summary : summary }

let zero_summary =
  {
    total_cost_usd = 0.0;
    total_prompt_tokens = 0;
    total_completion_tokens = 0;
    total_added_prompt_tokens = 0;
    total_cached_tokens = 0;
    total_turns = 0;
  }

let record ~db ~session_key ?message_id ~provider ~model ~prompt_tokens
    ~completion_tokens ?cost_usd ?added_prompt_tokens ?cached_tokens () =
  let sql =
    "INSERT INTO request_stats (session_key, message_id, provider, model, \
     prompt_tokens, completion_tokens, cost_usd, added_prompt_tokens, \
     cached_tokens) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
        ignore
          (Sqlite3.bind stmt 2
             (match message_id with
             | Some id -> Sqlite3.Data.INT (Int64.of_int id)
             | None -> Sqlite3.Data.NULL));
        ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT provider));
        ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT model));
        ignore
          (Sqlite3.bind stmt 5 (Sqlite3.Data.INT (Int64.of_int prompt_tokens)));
        ignore
          (Sqlite3.bind stmt 6
             (Sqlite3.Data.INT (Int64.of_int completion_tokens)));
        ignore
          (Sqlite3.bind stmt 7
             (match cost_usd with
             | Some c -> Sqlite3.Data.FLOAT c
             | None -> Sqlite3.Data.NULL));
        ignore
          (Sqlite3.bind stmt 8
             (match added_prompt_tokens with
             | Some a -> Sqlite3.Data.INT (Int64.of_int a)
             | None -> Sqlite3.Data.NULL));
        ignore
          (Sqlite3.bind stmt 9
             (match cached_tokens with
             | Some c -> Sqlite3.Data.INT (Int64.of_int c)
             | None -> Sqlite3.Data.NULL));
        match Sqlite3.step stmt with
        | Sqlite3.Rc.DONE ->
            Logs.debug (fun m ->
                m "request_stats: recorded %s/%s pt=%d ct=%d" provider model
                  prompt_tokens completion_tokens)
        | rc ->
            Logs.warn (fun m ->
                m "request_stats insert failed: %s" (Sqlite3.Rc.to_string rc)))
  with exn ->
    Logs.warn (fun m ->
        m "request_stats record error: %s" (Printexc.to_string exn))

let get_prev_totals ~db ~session_key =
  let sql =
    "SELECT prompt_tokens, completion_tokens, requested_at FROM request_stats \
     WHERE session_key = ? ORDER BY id DESC LIMIT 1"
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let pt =
              Sqlite3.column stmt 0 |> Sqlite3.Data.to_int
              |> Option.value ~default:0
            in
            let ct =
              Sqlite3.column stmt 1 |> Sqlite3.Data.to_int
              |> Option.value ~default:0
            in
            let ts =
              match Sqlite3.column stmt 2 with
              | Sqlite3.Data.TEXT s -> s
              | _ -> ""
            in
            Some (pt, ct, ts)
        | _ -> None)
  with _ -> None

let read_summary_row stmt =
  let cost =
    match Sqlite3.column stmt 0 with Sqlite3.Data.FLOAT f -> f | _ -> 0.0
  in
  let pt =
    Sqlite3.column stmt 1 |> Sqlite3.Data.to_int |> Option.value ~default:0
  in
  let ct =
    Sqlite3.column stmt 2 |> Sqlite3.Data.to_int |> Option.value ~default:0
  in
  let apt =
    Sqlite3.column stmt 3 |> Sqlite3.Data.to_int |> Option.value ~default:0
  in
  let cached =
    Sqlite3.column stmt 4 |> Sqlite3.Data.to_int |> Option.value ~default:0
  in
  let turns =
    Sqlite3.column stmt 5 |> Sqlite3.Data.to_int |> Option.value ~default:0
  in
  {
    total_cost_usd = cost;
    total_prompt_tokens = pt;
    total_completion_tokens = ct;
    total_added_prompt_tokens = apt;
    total_cached_tokens = cached;
    total_turns = turns;
  }

let summary_sql_cols =
  "COALESCE(SUM(cost_usd), 0.0), COALESCE(SUM(prompt_tokens), 0), \
   COALESCE(SUM(completion_tokens), 0), COALESCE(SUM(added_prompt_tokens), 0), \
   COALESCE(SUM(cached_tokens), 0), COUNT(*)"

let total_summary ~db =
  let sql = "SELECT " ^ summary_sql_cols ^ " FROM request_stats" in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> read_summary_row stmt
        | _ -> zero_summary)
  with _ -> zero_summary

let summary_for_session ~db ~session_key =
  let sql =
    "SELECT " ^ summary_sql_cols ^ " FROM request_stats WHERE session_key = ?"
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> read_summary_row stmt
        | _ -> zero_summary)
  with _ -> zero_summary

let is_valid_date_expr s =
  (* Whitelist: must be a datetime() or date() call with 'now' and safe args *)
  let len = String.length s in
  let starts_with prefix =
    let plen = String.length prefix in
    len >= plen && String.sub s 0 plen = prefix
  in
  let has_substr sub =
    let slen = String.length sub in
    let rec go i =
      if i + slen > len then false
      else if String.sub s i slen = sub then true
      else go (i + 1)
    in
    go 0
  in
  (starts_with "datetime('now'," || starts_with "date('now',")
  && len > 0
  && s.[len - 1] = ')'
  && (not (has_substr ";"))
  && (not (has_substr "' UNION"))
  && not (has_substr "' OR")

let resolve_since ~db since =
  if not (is_valid_date_expr since) then (
    Logs.warn (fun m ->
        m "resolve_since: rejecting non-whitelisted expr: %s" since);
    since)
  else
    try
      let stmt = Sqlite3.prepare db ("SELECT " ^ since) in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> (
              match Sqlite3.column stmt 0 with
              | Sqlite3.Data.TEXT s -> s
              | _ -> since)
          | _ -> since)
    with _ -> since

let summary_for_period ~db ~since =
  let resolved_since = resolve_since ~db since in
  let sql =
    "SELECT " ^ summary_sql_cols ^ " FROM request_stats WHERE requested_at >= ?"
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT resolved_since));
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> read_summary_row stmt
        | _ -> zero_summary)
  with _ -> zero_summary

let summary_by_session ~db =
  let sql =
    "SELECT session_key, " ^ summary_sql_cols
    ^ ", MIN(requested_at), MAX(requested_at) FROM request_stats GROUP BY \
       session_key ORDER BY COALESCE(SUM(cost_usd), 0.0) DESC"
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        let rows = ref [] in
        let rec loop () =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW ->
              let sk =
                match Sqlite3.column stmt 0 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              let cost =
                match Sqlite3.column stmt 1 with
                | Sqlite3.Data.FLOAT f -> f
                | _ -> 0.0
              in
              let pt =
                Sqlite3.column stmt 2 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let ct =
                Sqlite3.column stmt 3 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let apt =
                Sqlite3.column stmt 4 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let cached =
                Sqlite3.column stmt 5 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let turns =
                Sqlite3.column stmt 6 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let first_req =
                match Sqlite3.column stmt 7 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              let last_req =
                match Sqlite3.column stmt 8 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              rows :=
                {
                  session_key = sk;
                  summary =
                    {
                      total_cost_usd = cost;
                      total_prompt_tokens = pt;
                      total_completion_tokens = ct;
                      total_added_prompt_tokens = apt;
                      total_cached_tokens = cached;
                      total_turns = turns;
                    };
                  first_request = first_req;
                  last_request = last_req;
                }
                :: !rows;
              loop ()
          | _ -> ()
        in
        loop ();
        List.rev !rows)
  with _ -> []

let summary_by_model ~db =
  let sql =
    "SELECT model, provider, " ^ summary_sql_cols
    ^ " FROM request_stats GROUP BY model, provider ORDER BY \
       COALESCE(SUM(cost_usd), 0.0) DESC"
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        let rows = ref [] in
        let rec loop () =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW ->
              let m =
                match Sqlite3.column stmt 0 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              let prov =
                match Sqlite3.column stmt 1 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              let cost =
                match Sqlite3.column stmt 2 with
                | Sqlite3.Data.FLOAT f -> f
                | _ -> 0.0
              in
              let pt =
                Sqlite3.column stmt 3 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let ct =
                Sqlite3.column stmt 4 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let apt =
                Sqlite3.column stmt 5 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let cached =
                Sqlite3.column stmt 6 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let turns =
                Sqlite3.column stmt 7 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              rows :=
                {
                  model = m;
                  provider = prov;
                  summary =
                    {
                      total_cost_usd = cost;
                      total_prompt_tokens = pt;
                      total_completion_tokens = ct;
                      total_added_prompt_tokens = apt;
                      total_cached_tokens = cached;
                      total_turns = turns;
                    };
                }
                :: !rows;
              loop ()
          | _ -> ()
        in
        loop ();
        List.rev !rows)
  with _ -> []

let summary_by_model_for_period ~db ~since =
  let resolved_since = resolve_since ~db since in
  let sql =
    "SELECT model, provider, " ^ summary_sql_cols
    ^ " FROM request_stats WHERE requested_at >= ? GROUP BY model, provider \
       ORDER BY COALESCE(SUM(cost_usd), 0.0) DESC"
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT resolved_since));
        let rows = ref [] in
        let rec loop () =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW ->
              let m =
                match Sqlite3.column stmt 0 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              let prov =
                match Sqlite3.column stmt 1 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              let cost =
                match Sqlite3.column stmt 2 with
                | Sqlite3.Data.FLOAT f -> f
                | _ -> 0.0
              in
              let pt =
                Sqlite3.column stmt 3 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let ct =
                Sqlite3.column stmt 4 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let apt =
                Sqlite3.column stmt 5 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let cached =
                Sqlite3.column stmt 6 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let turns =
                Sqlite3.column stmt 7 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              rows :=
                {
                  model = m;
                  provider = prov;
                  summary =
                    {
                      total_cost_usd = cost;
                      total_prompt_tokens = pt;
                      total_completion_tokens = ct;
                      total_added_prompt_tokens = apt;
                      total_cached_tokens = cached;
                      total_turns = turns;
                    };
                }
                :: !rows;
              loop ()
          | _ -> ()
        in
        loop ();
        List.rev !rows)
  with _ -> []

let summary_by_provider ~db =
  let sql =
    "SELECT provider, " ^ summary_sql_cols
    ^ " FROM request_stats GROUP BY provider ORDER BY COALESCE(SUM(cost_usd), \
       0.0) DESC"
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        let rows = ref [] in
        let rec loop () =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW ->
              let prov =
                match Sqlite3.column stmt 0 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              let cost =
                match Sqlite3.column stmt 1 with
                | Sqlite3.Data.FLOAT f -> f
                | _ -> 0.0
              in
              let pt =
                Sqlite3.column stmt 2 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let ct =
                Sqlite3.column stmt 3 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let apt =
                Sqlite3.column stmt 4 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let cached =
                Sqlite3.column stmt 5 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let turns =
                Sqlite3.column stmt 6 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              rows :=
                ( prov,
                  {
                    total_cost_usd = cost;
                    total_prompt_tokens = pt;
                    total_completion_tokens = ct;
                    total_added_prompt_tokens = apt;
                    total_cached_tokens = cached;
                    total_turns = turns;
                  } )
                :: !rows;
              loop ()
          | _ -> ()
        in
        loop ();
        List.rev !rows)
  with _ -> []

let format_tokens n =
  if n >= 1_000_000 then Printf.sprintf "%.1fM" (float_of_int n /. 1_000_000.0)
  else if n >= 1_000 then Printf.sprintf "%.1fK" (float_of_int n /. 1_000.0)
  else string_of_int n
