type usage = {
  prompt_tokens : int;
  completion_tokens : int;
  total_tokens : int;
  cost_usd : float;
  turns : int;
}

type state = {
  profile_id : int;
  token_limit : int;
  cost_limit_usd : float;
  current_usage : usage;
  reset_period : string;
  period_started_at : string;
  token_limit_exceeded : bool;
  cost_limit_exceeded : bool;
  limit_exceeded : bool;
  created_at : string;
  updated_at : string;
}

let exec_exn db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "SQLite error: %s (sql: %s)" (Sqlite3.Rc.to_string rc)
           sql)

let init_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS room_budgets (\n\
    \     profile_id INTEGER PRIMARY KEY,\n\
    \     token_limit INTEGER NOT NULL CHECK(token_limit >= 0),\n\
    \     cost_limit_usd REAL NOT NULL CHECK(cost_limit_usd >= 0.0),\n\
    \     reset_period TEXT NOT NULL,\n\
    \     period_started_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     updated_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     FOREIGN KEY (profile_id) REFERENCES room_profiles(id) ON DELETE \
     CASCADE\n\
    \   )"

let init_profile_budget ~db ~profile_id ~token_limit ~cost_limit_usd
    ~reset_period ?period_started_at () =
  let sql =
    "INSERT INTO room_budgets (profile_id, token_limit, cost_limit_usd, \
     reset_period, period_started_at) VALUES (?, ?, ?, ?, COALESCE(?, \
     datetime('now'))) ON CONFLICT(profile_id) DO UPDATE SET token_limit = \
     excluded.token_limit, cost_limit_usd = excluded.cost_limit_usd, \
     reset_period = excluded.reset_period, updated_at = datetime('now')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int profile_id)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int token_limit)));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.FLOAT cost_limit_usd));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT reset_period));
      ignore
        (Sqlite3.bind stmt 5
           (match period_started_at with
           | Some ts -> Sqlite3.Data.TEXT ts
           | None -> Sqlite3.Data.NULL));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "init_profile_budget failed: %s"
               (Sqlite3.Rc.to_string rc)))

let int_col stmt idx =
  Sqlite3.column stmt idx |> Sqlite3.Data.to_int |> Option.value ~default:0

let float_col stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.FLOAT f -> f
  | Sqlite3.Data.INT n -> Int64.to_float n
  | _ -> 0.0

let text_col stmt idx =
  match Sqlite3.column stmt idx with Sqlite3.Data.TEXT s -> s | _ -> ""

let current_usage ~db ~profile_id ~period_started_at =
  let sql =
    "SELECT COALESCE(SUM(prompt_tokens), 0), COALESCE(SUM(completion_tokens), \
     0), COALESCE(SUM(cost_usd), 0.0), COUNT(*) FROM request_stats WHERE \
     profile_id = ? AND requested_at >= ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int profile_id)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT period_started_at));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let prompt_tokens = int_col stmt 0 in
          let completion_tokens = int_col stmt 1 in
          {
            prompt_tokens;
            completion_tokens;
            total_tokens = prompt_tokens + completion_tokens;
            cost_usd = float_col stmt 2;
            turns = int_col stmt 3;
          }
      | _ ->
          {
            prompt_tokens = 0;
            completion_tokens = 0;
            total_tokens = 0;
            cost_usd = 0.0;
            turns = 0;
          })

let get_profile_budget ~db ~profile_id =
  let sql =
    "SELECT profile_id, token_limit, cost_limit_usd, reset_period, \
     period_started_at, created_at, updated_at FROM room_budgets WHERE \
     profile_id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int profile_id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let profile_id = int_col stmt 0 in
          let token_limit = int_col stmt 1 in
          let cost_limit_usd = float_col stmt 2 in
          let reset_period = text_col stmt 3 in
          let period_started_at = text_col stmt 4 in
          let current_usage =
            current_usage ~db ~profile_id ~period_started_at
          in
          let token_limit_exceeded = current_usage.total_tokens > token_limit in
          let cost_limit_exceeded = current_usage.cost_usd > cost_limit_usd in
          Some
            {
              profile_id;
              token_limit;
              cost_limit_usd;
              current_usage;
              reset_period;
              period_started_at;
              token_limit_exceeded;
              cost_limit_exceeded;
              limit_exceeded = token_limit_exceeded || cost_limit_exceeded;
              created_at = text_col stmt 5;
              updated_at = text_col stmt 6;
            }
      | _ -> None)

let reset_profile_budget ~db ~profile_id ?period_started_at () =
  let sql =
    "UPDATE room_budgets SET period_started_at = COALESCE(?, datetime('now')), \
     updated_at = datetime('now') WHERE profile_id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (match period_started_at with
           | Some ts -> Sqlite3.Data.TEXT ts
           | None -> Sqlite3.Data.NULL));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int profile_id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db > 0
      | rc ->
          failwith
            (Printf.sprintf "reset_profile_budget failed: %s"
               (Sqlite3.Rc.to_string rc)))

(* Reservations are intentionally in-memory. Request_stats remains the durable
   source of completed usage; reservations only close the race between
   concurrent provider calls within this process. *)
type reservation_slot = {
  mutex : Lwt_mutex.t;
  condition : unit Lwt_condition.t;
  mutable reserved_tokens : int;
  mutable reserved_cost_usd : float;
}

let reservation_slots : (int * int, reservation_slot) Hashtbl.t =
  Hashtbl.create 16

let reservation_slots_mutex = Lwt_mutex.create ()

let reservation_slot_for_profile db profile_id =
  let key = (Hashtbl.hash db, profile_id) in
  Lwt_mutex.with_lock reservation_slots_mutex (fun () ->
      match Hashtbl.find_opt reservation_slots key with
      | Some slot -> Lwt.return slot
      | None ->
          let slot =
            {
              mutex = Lwt_mutex.create ();
              condition = Lwt_condition.create ();
              reserved_tokens = 0;
              reserved_cost_usd = 0.0;
            }
          in
          Hashtbl.add reservation_slots key slot;
          Lwt.return slot)

let reservation_would_exceed state slot ~estimated_tokens ~estimated_cost_usd =
  let projected_tokens =
    state.current_usage.total_tokens + slot.reserved_tokens + estimated_tokens
  in
  let projected_cost =
    state.current_usage.cost_usd +. slot.reserved_cost_usd +. estimated_cost_usd
  in
  projected_tokens > state.token_limit || projected_cost > state.cost_limit_usd

let reservation_impossible state ~estimated_tokens ~estimated_cost_usd =
  estimated_tokens > state.token_limit
  || estimated_cost_usd > state.cost_limit_usd

let release_reservation slot ~tokens ~cost_usd released () =
  if not !released then begin
    released := true;
    slot.reserved_tokens <- max 0 (slot.reserved_tokens - tokens);
    slot.reserved_cost_usd <- max 0.0 (slot.reserved_cost_usd -. cost_usd);
    Lwt_condition.broadcast slot.condition ()
  end

let reserve_profile_budget ~db ~profile_id ~estimated_tokens ~estimated_cost_usd
    =
  let open Lwt.Syntax in
  let estimated_tokens = max 0 estimated_tokens in
  let estimated_cost_usd = max 0.0 estimated_cost_usd in
  let* slot = reservation_slot_for_profile db profile_id in
  Lwt_mutex.with_lock slot.mutex (fun () ->
      let rec wait_for_budget () =
        match get_profile_budget ~db ~profile_id with
        | None -> Lwt.return (Ok (fun () -> ()))
        | Some state
          when state.limit_exceeded
               || reservation_impossible state ~estimated_tokens
                    ~estimated_cost_usd ->
            Lwt.return (Error state)
        | Some state
          when not
                 (reservation_would_exceed state slot ~estimated_tokens
                    ~estimated_cost_usd) ->
            slot.reserved_tokens <- slot.reserved_tokens + estimated_tokens;
            slot.reserved_cost_usd <-
              slot.reserved_cost_usd +. estimated_cost_usd;
            let released = ref false in
            Lwt.return
              (Ok
                 (release_reservation slot ~tokens:estimated_tokens
                    ~cost_usd:estimated_cost_usd released))
        | Some state ->
            if slot.reserved_tokens > 0 || slot.reserved_cost_usd > 0.0 then
              let* () = Lwt_condition.wait ~mutex:slot.mutex slot.condition in
              wait_for_budget ()
            else Lwt.return (Error state)
      in
      wait_for_budget ())

let budget_exceeded_message state =
  Printf.sprintf
    "budget exceeded for room profile %d: current usage is %d tokens, %.6f \
     USD; limits are %d tokens and %.6f USD"
    state.profile_id state.current_usage.total_tokens
    state.current_usage.cost_usd state.token_limit state.cost_limit_usd

let with_profile_budget_reservation ~db ~profile_id ~estimated_tokens
    ~estimated_cost_usd f =
  let open Lwt.Syntax in
  let* reservation =
    reserve_profile_budget ~db ~profile_id ~estimated_tokens ~estimated_cost_usd
  in
  match reservation with
  | Error state -> Lwt.fail_with (budget_exceeded_message state)
  | Ok release ->
      Lwt.finalize f (fun () ->
          release ();
          Lwt.return_unit)
