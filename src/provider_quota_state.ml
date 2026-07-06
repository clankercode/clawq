type window_state = {
  used_pct : float;
  resets_at : float option;
  window_duration_s : float option;
}

type quota_state =
  | Known of {
      session : window_state option;
      weekly : window_state option;
      monthly : window_state option;
    }
  | Unknown of string

type provider_quota = {
  provider_name : string;
  state : quota_state;
  fetched_at : float;
}

type history_entry = {
  h_id : int;
  h_provider : string;
  h_state : quota_state;
  h_fetched_at : float;
  h_recorded_at : string;
}

(* In-memory TTL cache: provider name -> latest quota *)
let cache : (string, provider_quota) Hashtbl.t = Hashtbl.create 8
let cache_ttl_s = ref 300.0
let set_cache_ttl s = cache_ttl_s := float_of_int s

(* Optional DB handle for persistent quota cache across process invocations. *)
let db_handle : Sqlite3.db option ref = ref None
let loaded_from_db = ref false

let set_db db =
  db_handle := Some db;
  loaded_from_db := false;
  try
    ignore
      (Sqlite3.exec db
         "CREATE TABLE IF NOT EXISTS quota_cache (provider TEXT PRIMARY KEY, \
          state_json TEXT NOT NULL, fetched_at REAL NOT NULL)")
  with _ -> ()

let reset_for_test () =
  Hashtbl.reset cache;
  loaded_from_db := false;
  db_handle := None

(* ── JSON serialization for quota_state ─────────────────────────────────── *)

let window_state_to_json ws =
  let resets = match ws.resets_at with None -> `Null | Some f -> `Float f in
  let dur =
    match ws.window_duration_s with None -> `Null | Some f -> `Float f
  in
  `Assoc
    [
      ("used_pct", `Float ws.used_pct);
      ("resets_at", resets);
      ("window_duration_s", dur);
    ]

let window_state_of_json j =
  try
    let used_pct = Yojson.Safe.Util.(j |> member "used_pct" |> to_float) in
    let resets_at =
      try Some Yojson.Safe.Util.(j |> member "resets_at" |> to_float)
      with _ -> None
    in
    let window_duration_s =
      try Some Yojson.Safe.Util.(j |> member "window_duration_s" |> to_float)
      with _ -> None
    in
    Some { used_pct; resets_at; window_duration_s }
  with _ -> None

let opt_window_of_json = function `Null -> None | j -> window_state_of_json j

let quota_state_to_json = function
  | Unknown msg -> `Assoc [ ("kind", `String "unknown"); ("msg", `String msg) ]
  | Known { session; weekly; monthly } ->
      let opt w =
        match w with None -> `Null | Some ws -> window_state_to_json ws
      in
      `Assoc
        [
          ("kind", `String "known");
          ("session", opt session);
          ("weekly", opt weekly);
          ("monthly", opt monthly);
        ]

let quota_state_of_json j =
  try
    let kind = Yojson.Safe.Util.(j |> member "kind" |> to_string) in
    if kind = "unknown" then
      Some (Unknown Yojson.Safe.Util.(j |> member "msg" |> to_string))
    else if kind = "known" then
      Some
        (Known
           {
             session =
               opt_window_of_json Yojson.Safe.Util.(j |> member "session");
             weekly = opt_window_of_json Yojson.Safe.Util.(j |> member "weekly");
             monthly =
               opt_window_of_json Yojson.Safe.Util.(j |> member "monthly");
           })
    else None
  with _ -> None

(* ── DB persistence ──────────────────────────────────────────────────────── *)

let store_to_db ?(replace = true) db pq =
  try
    let state_json = Yojson.Safe.to_string (quota_state_to_json pq.state) in
    let sql =
      if replace then
        "INSERT OR REPLACE INTO quota_cache (provider, state_json, fetched_at) \
         VALUES (?, ?, ?)"
      else
        "INSERT OR IGNORE INTO quota_cache (provider, state_json, fetched_at) \
         VALUES (?, ?, ?)"
    in
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT pq.provider_name));
        ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT state_json));
        ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.FLOAT pq.fetched_at));
        ignore (Sqlite3.step stmt))
  with _ -> ()

let record_history db pq =
  match pq.state with
  | Unknown s when String.length s >= 12 && String.sub s 0 12 = "fetch_failed"
    ->
      ()
  | _ -> (
      try
        let state_json = Yojson.Safe.to_string (quota_state_to_json pq.state) in
        let stmt =
          Sqlite3.prepare db
            "INSERT INTO quota_history (provider, state_json, fetched_at) \
             VALUES (?, ?, ?)"
        in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT pq.provider_name));
            ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT state_json));
            ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.FLOAT pq.fetched_at));
            ignore (Sqlite3.step stmt))
      with _ -> ())

let read_history_row stmt =
  let h_id =
    match Sqlite3.column stmt 0 with
    | Sqlite3.Data.INT n -> Int64.to_int n
    | _ -> raise Exit
  in
  let h_provider =
    match Sqlite3.column stmt 1 with
    | Sqlite3.Data.TEXT s -> s
    | _ -> raise Exit
  in
  let state_json =
    match Sqlite3.column stmt 2 with
    | Sqlite3.Data.TEXT s -> s
    | _ -> raise Exit
  in
  let h_fetched_at =
    match Sqlite3.column stmt 3 with
    | Sqlite3.Data.FLOAT f -> f
    | Sqlite3.Data.INT n -> Int64.to_float n
    | _ -> raise Exit
  in
  let h_recorded_at =
    match Sqlite3.column stmt 4 with
    | Sqlite3.Data.TEXT s -> s
    | _ -> raise Exit
  in
  let json = Yojson.Safe.from_string state_json in
  match quota_state_of_json json with
  | Some h_state ->
      Some { h_id; h_provider; h_state; h_fetched_at; h_recorded_at }
  | None -> None

let collect_history_rows stmt =
  let rec loop acc =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match read_history_row stmt with
        | Some entry -> loop (entry :: acc)
        | None -> loop acc)
    | _ -> List.rev acc
  in
  loop []

let history_for_provider ~db ~provider ?since ?limit () =
  let base =
    "SELECT id, provider, state_json, fetched_at, recorded_at FROM \
     quota_history WHERE provider = ?"
  in
  let sql, binds =
    match since with
    | None ->
        ( base ^ " ORDER BY recorded_at DESC",
          [ (1, Sqlite3.Data.TEXT provider) ] )
    | Some ts ->
        ( base ^ " AND fetched_at >= ? ORDER BY recorded_at DESC",
          [ (1, Sqlite3.Data.TEXT provider); (2, Sqlite3.Data.FLOAT ts) ] )
  in
  let sql =
    match limit with Some n -> sql ^ " LIMIT " ^ string_of_int n | None -> sql
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        List.iter (fun (i, v) -> ignore (Sqlite3.bind stmt i v)) binds;
        collect_history_rows stmt)
  with _ -> []

let history_all ~db ?since ?limit () =
  let base =
    "SELECT id, provider, state_json, fetched_at, recorded_at FROM \
     quota_history"
  in
  let sql, binds =
    match since with
    | None -> (base ^ " ORDER BY recorded_at DESC", [])
    | Some ts ->
        ( base ^ " WHERE fetched_at >= ? ORDER BY recorded_at DESC",
          [ (1, Sqlite3.Data.FLOAT ts) ] )
  in
  let sql =
    match limit with Some n -> sql ^ " LIMIT " ^ string_of_int n | None -> sql
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        List.iter (fun (i, v) -> ignore (Sqlite3.bind stmt i v)) binds;
        collect_history_rows stmt)
  with _ -> []

let history_entry_to_json entry =
  `Assoc
    [
      ("id", `Int entry.h_id);
      ("provider", `String entry.h_provider);
      ("state", quota_state_to_json entry.h_state);
      ("fetched_at", `Float entry.h_fetched_at);
      ("recorded_at", `String entry.h_recorded_at);
    ]

let purge_history ~db ~before () =
  try
    let stmt =
      Sqlite3.prepare db "DELETE FROM quota_history WHERE fetched_at < ?"
    in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.FLOAT before));
        ignore (Sqlite3.step stmt);
        Sqlite3.changes db)
  with _ -> 0

let load_from_db db =
  try
    let stmt =
      Sqlite3.prepare db
        "SELECT provider, state_json, fetched_at FROM quota_cache"
    in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        let rec loop () =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW ->
              (try
                 let provider =
                   match Sqlite3.column stmt 0 with
                   | Sqlite3.Data.TEXT s -> s
                   | _ -> raise Exit
                 in
                 let state_json =
                   match Sqlite3.column stmt 1 with
                   | Sqlite3.Data.TEXT s -> s
                   | _ -> raise Exit
                 in
                 let fetched_at =
                   match Sqlite3.column stmt 2 with
                   | Sqlite3.Data.FLOAT f -> f
                   | Sqlite3.Data.INT n -> Int64.to_float n
                   | _ -> raise Exit
                 in
                 let json = Yojson.Safe.from_string state_json in
                 match quota_state_of_json json with
                 | Some state ->
                     Hashtbl.replace cache provider
                       { provider_name = provider; state; fetched_at }
                 | None -> ()
               with _ -> ());
              loop ()
          | _ -> ()
        in
        loop ())
  with _ -> ()

let ensure_loaded_from_db () =
  if not !loaded_from_db then begin
    loaded_from_db := true;
    match !db_handle with None -> () | Some db -> load_from_db db
  end

let get_cached name =
  ensure_loaded_from_db ();
  match Hashtbl.find_opt cache name with
  | None -> None
  | Some pq ->
      let age = Unix.gettimeofday () -. pq.fetched_at in
      if age < !cache_ttl_s then Some pq else None

(* Like get_cached but with an explicit TTL override. *)
let get_cached_with_ttl ~ttl_s name =
  ensure_loaded_from_db ();
  match Hashtbl.find_opt cache name with
  | None -> None
  | Some pq ->
      let age = Unix.gettimeofday () -. pq.fetched_at in
      if age < float_of_int ttl_s then Some pq else None

let get_all_cached () =
  ensure_loaded_from_db ();
  let now = Unix.gettimeofday () in
  Hashtbl.fold
    (fun _name pq acc ->
      let age = now -. pq.fetched_at in
      if age < !cache_ttl_s then (pq.provider_name, pq) :: acc else acc)
    cache []

let store_result pq =
  (match pq.state with
  | Known _ -> Hashtbl.replace cache pq.provider_name pq
  | Unknown s when String.length s >= 12 && String.sub s 0 12 = "fetch_failed"
    ->
      (* On fetch failure, keep existing stale entry if present *)
      if not (Hashtbl.mem cache pq.provider_name) then
        Hashtbl.replace cache pq.provider_name pq
  | Unknown _ -> Hashtbl.replace cache pq.provider_name pq);
  match !db_handle with
  | None -> ()
  | Some db -> (
      match pq.state with
      | Unknown s
        when String.length s >= 12 && String.sub s 0 12 = "fetch_failed" ->
          (* Mirror in-memory: don't overwrite an existing entry in DB *)
          store_to_db ~replace:false db pq
      | _ ->
          store_to_db db pq;
          record_history db pq)

(* Pace-aware constrained check for a single window *)
let is_window_constrained ~threshold w =
  let u = w.used_pct /. 100.0 in
  if u >= threshold then true
  else
    match (w.resets_at, w.window_duration_s) with
    | Some ts, Some dur ->
        let now = Unix.gettimeofday () in
        let window_start = ts -. dur in
        let elapsed = now -. window_start in
        if elapsed <= 0.0 || dur <= 0.0 then false
        else
          let time_elapsed_pct = elapsed /. dur *. 100.0 in
          if time_elapsed_pct < 1.0 then false
          else
            let pace_ratio = w.used_pct /. time_elapsed_pct in
            pace_ratio >= 1.5 && w.used_pct >= 50.0
    | _ -> false

(* Returns true when a provider should be deprioritised due to quota pressure.
   Unknown providers are never considered constrained. *)
let is_constrained ?(threshold = 0.85) state =
  match state with
  | Unknown _ -> false
  | Known { session; weekly; monthly } ->
      let check = function
        | None -> false
        | Some w -> is_window_constrained ~threshold w
      in
      check session || check weekly || check monthly

let format_time_remaining_s s =
  let s = int_of_float s in
  if s < 60 then Printf.sprintf "%ds" s
  else if s < 3600 then Printf.sprintf "%dm" (s / 60)
  else if s < 86400 then Printf.sprintf "%dh%02dm" (s / 3600) (s mod 3600 / 60)
  else Printf.sprintf "%dd%dh" (s / 86400) (s mod 86400 / 3600)

let window_to_string label window =
  match window with
  | None -> ""
  | Some { used_pct; resets_at; _ } ->
      let remaining =
        match resets_at with
        | None -> ""
        | Some ts ->
            let r = ts -. Unix.gettimeofday () in
            if r > 0.0 then
              Printf.sprintf " (%s left)" (format_time_remaining_s r)
            else ""
      in
      Printf.sprintf "%s=%.0f%%%s" label used_pct remaining

let to_summary_string pq =
  match pq.state with
  | Unknown s -> Printf.sprintf "%s\tUnknown (%s)" pq.provider_name s
  | Known { session; weekly; monthly } ->
      let parts =
        List.filter_map
          (fun s -> if s = "" then None else Some s)
          [
            window_to_string "session" session;
            window_to_string "weekly" weekly;
            window_to_string "monthly" monthly;
          ]
      in
      Printf.sprintf "%s\t%s" pq.provider_name (String.concat "  " parts)

(* Build a notice string to inject into system prompt when provider is >= threshold
   usage. Returns None when not applicable. *)
let quota_notice ?(threshold = 0.70) pq =
  match pq.state with
  | Unknown _ -> None
  | Known { session; weekly; monthly } ->
      let any_triggered = function
        | None -> false
        | Some { used_pct; _ } -> used_pct /. 100.0 >= threshold
      in
      if any_triggered session || any_triggered weekly || any_triggered monthly
      then
        let parts =
          List.filter_map
            (fun (label, w) ->
              match w with
              | Some { used_pct; resets_at; _ }
                when used_pct /. 100.0 >= threshold ->
                  let remaining =
                    match resets_at with
                    | None -> ""
                    | Some ts ->
                        let r = ts -. Unix.gettimeofday () in
                        if r > 0.0 then
                          Printf.sprintf " (%s left)"
                            (format_time_remaining_s r)
                        else ""
                  in
                  Some (Printf.sprintf "%s %.0f%%%s" label used_pct remaining)
              | _ -> None)
            [ ("session", session); ("weekly", weekly); ("monthly", monthly) ]
        in
        Some
          (Printf.sprintf "[quota] %s: %s — prefer concise responses"
             pq.provider_name (String.concat ", " parts))
      else None

(* Returns the status label for CLI display *)
let status_label ?(threshold = 0.85) pq =
  match pq.state with
  | Unknown _ -> "unknown"
  | Known _ ->
      if is_constrained ~threshold pq.state then "constrained" else "ok"
