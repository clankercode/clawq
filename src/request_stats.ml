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

let unique_strings values =
  List.fold_left
    (fun acc value ->
      if String.trim value = "" || List.mem value acc then acc else value :: acc)
    [] values
  |> List.rev

let channel_id_from_session_key session_key =
  match String.index_opt session_key ':' with
  | None -> None
  | Some idx when idx + 1 < String.length session_key ->
      Some
        (String.sub session_key (idx + 1) (String.length session_key - idx - 1))
  | Some _ -> None

let room_profile_binding_profile_id ~db room_id =
  try
    Option.map
      (fun (b : Memory.room_profile_binding) -> b.profile_id)
      (Memory.get_room_profile_binding ~db ~room_id)
  with _ -> None

let profile_id_by_name ~db name =
  try
    Option.map
      (fun (p : Memory.room_profile) -> p.id)
      (Memory.get_room_profile_by_name ~db ~name)
  with _ -> None

let profile_has_binding ~db profile_id =
  try
    Memory.list_room_profile_bindings_all ~db
    |> List.exists (fun (b : Memory.room_profile_binding) ->
        b.profile_id = profile_id)
  with _ -> false

let direct_room_candidates ~db ~session_key =
  let channel_candidates =
    try
      match Memory.get_session_channel ~db ~session_key with
      | Some (channel, channel_id) -> [ channel ^ ":" ^ channel_id; channel_id ]
      | None -> []
    with _ -> []
  in
  unique_strings
    ((session_key :: channel_candidates)
    @
    match channel_id_from_session_key session_key with
    | Some channel_id -> [ channel_id ]
    | None -> [])

let infer_profile_id ~db ~session_key =
  let direct =
    direct_room_candidates ~db ~session_key
    |> List.find_map (room_profile_binding_profile_id ~db)
  in
  match direct with
  | Some _ as found -> found
  | None -> (
      match Room_session.parse_child_thread_key session_key with
      | Some child -> (
          match profile_id_by_name ~db child.profile_id with
          | None -> None
          | Some profile_id ->
              let room_candidates =
                unique_strings
                  [ child.connector ^ ":" ^ child.room_id; child.room_id ]
              in
              let matches =
                room_candidates
                |> List.exists (fun room_id ->
                    room_profile_binding_profile_id ~db room_id
                    = Some profile_id)
              in
              if matches then Some profile_id else None)
      | None -> (
          match Room_session.parse_routine_key session_key with
          | Some routine -> (
              match profile_id_by_name ~db routine.profile_id with
              | Some profile_id when profile_has_binding ~db profile_id ->
                  Some profile_id
              | _ -> None)
          | None -> None))

let record ~db ~session_key ?message_id ?profile_id ~provider ~model
    ~prompt_tokens ~completion_tokens ?cost_usd ?added_prompt_tokens
    ?cached_tokens ?latency_ms () =
  let profile_id =
    match profile_id with
    | Some _ -> profile_id
    | None -> infer_profile_id ~db ~session_key
  in
  let sql =
    "INSERT INTO request_stats (session_key, message_id, profile_id, provider, \
     model, prompt_tokens, completion_tokens, cost_usd, added_prompt_tokens, \
     cached_tokens, latency_ms) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
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
        ignore
          (Sqlite3.bind stmt 3
             (match profile_id with
             | Some id -> Sqlite3.Data.INT (Int64.of_int id)
             | None -> Sqlite3.Data.NULL));
        ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT provider));
        ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT model));
        ignore
          (Sqlite3.bind stmt 6 (Sqlite3.Data.INT (Int64.of_int prompt_tokens)));
        ignore
          (Sqlite3.bind stmt 7
             (Sqlite3.Data.INT (Int64.of_int completion_tokens)));
        ignore
          (Sqlite3.bind stmt 8
             (match cost_usd with
             | Some c -> Sqlite3.Data.FLOAT c
             | None -> Sqlite3.Data.NULL));
        ignore
          (Sqlite3.bind stmt 9
             (match added_prompt_tokens with
             | Some a -> Sqlite3.Data.INT (Int64.of_int a)
             | None -> Sqlite3.Data.NULL));
        ignore
          (Sqlite3.bind stmt 10
             (match cached_tokens with
             | Some c -> Sqlite3.Data.INT (Int64.of_int c)
             | None -> Sqlite3.Data.NULL));
        ignore
          (Sqlite3.bind stmt 11
             (match latency_ms with
             | Some ms -> Sqlite3.Data.INT (Int64.of_int ms)
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
  (* Strict whitelist: only allow datetime('now', '...') or date('now', '...')
     where the modifier contains only safe chars (no SQL metacharacters). *)
  let len = String.length s in
  let starts_with prefix =
    let plen = String.length prefix in
    len >= plen && String.sub s 0 plen = prefix
  in
  (starts_with "datetime('now', '" || starts_with "date('now', '")
  && len > 0
  && s.[len - 1] = ')'
  (* Extract the modifier between the inner quotes *)
  &&
  let inner_start =
    if starts_with "datetime('now', '" then String.length "datetime('now', '"
    else String.length "date('now', '"
  in
  (* Find the closing inner quote *)
  let rec find_quote i =
    if i >= len then None
    else if s.[i] = '\'' then Some i
    else find_quote (i + 1)
  in
  match find_quote inner_start with
  | None -> false
  | Some quote_pos ->
      let modifier = String.sub s inner_start (quote_pos - inner_start) in
      (* Only allow: alphanumeric, space, +, - *)
      String.length modifier > 0
      && String.for_all
           (fun c ->
             (c >= 'a' && c <= 'z')
             || (c >= 'A' && c <= 'Z')
             || (c >= '0' && c <= '9')
             || c = ' ' || c = '+' || c = '-')
           modifier

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

let summary_query ~db ?profile_id ?since ?until () =
  let filters = ref [] in
  let params = ref [] in
  let add_filter sql data =
    filters := sql :: !filters;
    params := data :: !params
  in
  (match profile_id with
  | Some id -> add_filter "profile_id = ?" (Sqlite3.Data.INT (Int64.of_int id))
  | None -> ());
  (match since with
  | Some since ->
      add_filter "requested_at >= ?"
        (Sqlite3.Data.TEXT (resolve_since ~db since))
  | None -> ());
  (match until with
  | Some until ->
      add_filter "requested_at < ?"
        (Sqlite3.Data.TEXT (resolve_since ~db until))
  | None -> ());
  let where_clause =
    match List.rev !filters with
    | [] -> ""
    | filters -> " WHERE " ^ String.concat " AND " filters
  in
  let sql =
    "SELECT " ^ summary_sql_cols ^ " FROM request_stats" ^ where_clause
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        List.rev !params
        |> List.iteri (fun i data -> ignore (Sqlite3.bind stmt (i + 1) data));
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> read_summary_row stmt
        | _ -> zero_summary)
  with _ -> zero_summary

let summary_for_period ~db ~since = summary_query ~db ~since ()

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
