(** Delivery outbox diagnostics, metrics, and repair helpers (P19.M3.E3.T003).
*)

module O = Github_delivery_outbox

type metrics = {
  pending : int;
  in_flight : int;
  succeeded : int;
  dead_letter : int;
  superseded : int;
}

let default_stale_in_flight_seconds = 300.0

(** Parse ISO-8601 UTC "YYYY-MM-DDTHH:MM:SSZ" (optional fractional seconds) to
    Unix epoch. Returns [None] on malformed input. *)
let parse_iso8601_utc_opt s =
  try
    let s = String.trim s in
    let len = String.length s in
    let s =
      if len > 0 && s.[len - 1] = 'Z' then String.sub s 0 (len - 1) else s
    in
    let date_part, time_part =
      match String.split_on_char 'T' s with
      | [ d; t ] -> (d, t)
      | _ -> failwith "no T"
    in
    let year, month, day =
      match String.split_on_char '-' date_part with
      | [ y; m; d ] -> (int_of_string y, int_of_string m, int_of_string d)
      | _ -> failwith "bad date"
    in
    let time_part =
      match String.split_on_char '.' time_part with
      | [ t; _ ] -> t
      | _ -> time_part
    in
    let hour, minute, second =
      match String.split_on_char ':' time_part with
      | [ h; m; s ] -> (int_of_string h, int_of_string m, int_of_string s)
      | _ -> failwith "bad time"
    in
    let is_leap y = (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0 in
    let days_in_month y m =
      match m with
      | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
      | 4 | 6 | 9 | 11 -> 30
      | 2 -> if is_leap y then 29 else 28
      | _ -> 30
    in
    let year_days = ref 0 in
    for y = 1970 to year - 1 do
      year_days := !year_days + if is_leap y then 366 else 365
    done;
    let month_days = ref 0 in
    for m = 1 to month - 1 do
      month_days := !month_days + days_in_month year m
    done;
    let days = !year_days + !month_days + (day - 1) in
    Some
      ((float_of_int days *. 86_400.)
      +. (float_of_int hour *. 3_600.)
      +. (float_of_int minute *. 60.)
      +. float_of_int second)
  with _ -> None

let metrics ~db ?room_id () =
  O.ensure_schema db;
  let count status =
    match O.count_status ~db ~status ?room_id () with
    | Ok n -> Ok n
    | Error e -> Error e
  in
  match count O.Pending with
  | Error e -> Error e
  | Ok pending -> (
      match count O.In_flight with
      | Error e -> Error e
      | Ok in_flight -> (
          match count O.Succeeded with
          | Error e -> Error e
          | Ok succeeded -> (
              match count O.Dead_letter with
              | Error e -> Error e
              | Ok dead_letter -> (
                  match count O.Superseded with
                  | Error e -> Error e
                  | Ok superseded ->
                      Ok
                        {
                          pending;
                          in_flight;
                          succeeded;
                          dead_letter;
                          superseded;
                        }))))

let diagnose ~db ?room_id () =
  O.ensure_schema db;
  let now = Unix.gettimeofday () in
  let lines = ref [] in
  let push s = lines := s :: !lines in
  (match metrics ~db ?room_id () with
  | Error e -> push (Printf.sprintf "metrics_error: %s" e)
  | Ok m ->
      push (Printf.sprintf "pending: %d" m.pending);
      push (Printf.sprintf "in_flight: %d" m.in_flight);
      push (Printf.sprintf "succeeded: %d" m.succeeded);
      push (Printf.sprintf "dead_letter: %d" m.dead_letter);
      push (Printf.sprintf "superseded: %d" m.superseded));
  (match O.oldest_pending_created_at ~db ?room_id () with
  | Error e -> push (Printf.sprintf "oldest_pending_error: %s" e)
  | Ok None -> push "oldest_pending_age_seconds: n/a"
  | Ok (Some created_at) -> (
      match parse_iso8601_utc_opt created_at with
      | None ->
          push
            (Printf.sprintf "oldest_pending_created_at: %s (age unparsed)"
               created_at)
      | Some t ->
          let age = max 0. (now -. t) in
          push (Printf.sprintf "oldest_pending_age_seconds: %.0f" age);
          push (Printf.sprintf "oldest_pending_created_at: %s" created_at)));
  (match O.list_dead_letters ~db ?room_id ~limit:3 () with
  | Error e -> push (Printf.sprintf "dead_letter_list_error: %s" e)
  | Ok [] -> push "dead_letter_samples: (none)"
  | Ok entries ->
      List.iter
        (fun (e : O.entry) ->
          let err =
            match e.last_error with
            | None -> "-"
            | Some s ->
                let s = String.trim s in
                if String.length s <= 80 then s else String.sub s 0 80 ^ "..."
          in
          push
            (Printf.sprintf
               "dead_letter sample: id=%s room=%s item=%s attempts=%d error=%s"
               e.id e.room_id e.item_key e.attempts err))
        entries);
  List.rev !lines

let repair_stale_in_flight ~db
    ?(older_than_seconds = default_stale_in_flight_seconds)
    ?(now = Unix.gettimeofday ()) () =
  O.ensure_schema db;
  let older_than_seconds = max 0. older_than_seconds in
  let cutoff = Time_util.iso8601_utc ~t:(now -. older_than_seconds) () in
  (* Stuck = still in_flight and last due time is at/before the cutoff. *)
  let sql =
    {|UPDATE github_delivery_outbox
        SET status = 'pending'
      WHERE status = 'in_flight'
        AND next_attempt_at <= ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT cutoff));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok (Sqlite3.changes db)
  | rc ->
      Error
        (Printf.sprintf "repair_stale_in_flight failed: %s (%s)"
           (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))

let requeue_dead_letter ~db ~id ?(now = Unix.gettimeofday ()) () =
  O.ensure_schema db;
  if String.trim id = "" then Error "id must be non-empty"
  else
    let next_at = Time_util.iso8601_utc ~t:now () in
    let sql =
      {|UPDATE github_delivery_outbox
          SET status = 'pending',
              next_attempt_at = ?,
              dead_lettered_at = NULL
        WHERE id = ? AND status = 'dead_letter'|}
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT next_at));
    ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT id));
    let rc = Sqlite3.step stmt in
    ignore (Sqlite3.finalize stmt);
    match rc with
    | Sqlite3.Rc.DONE ->
        if Sqlite3.changes db > 0 then Ok ()
        else
          Error
            (Printf.sprintf
               "requeue_dead_letter: no dead_letter outbox row for id %s" id)
    | rc ->
        Error
          (Printf.sprintf "requeue_dead_letter failed: %s (%s)"
             (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
