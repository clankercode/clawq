let expire_job_inline ~db ~name =
  let sql = "UPDATE cron_jobs SET enabled = 0 WHERE name = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
      ignore (Sqlite3.step stmt))

(* B630/B632: identical-output detection. When a bg task that was triggered
   by a cron job completes, hash its output and stash it on the cron_runs
   row. If the most recent N runs for that job all share the same non-empty
   hash, the cron is producing a degenerate identical loop — disable it and
   warn so the user can fix the underlying cause (empty config, prompt
   shortcut, etc.) instead of letting it burn tokens hourly. *)
let identical_output_disable_threshold = 5

let normalize_output_for_hash s =
  (* Collapse whitespace so trivial formatting jitter doesn't defeat
     identical-output detection. *)
  let buf = Buffer.create (String.length s) in
  let last_ws = ref true in
  String.iter
    (fun c ->
      if c = ' ' || c = '\t' || c = '\n' || c = '\r' then begin
        if not !last_ws then Buffer.add_char buf ' ';
        last_ws := true
      end
      else begin
        Buffer.add_char buf c;
        last_ws := false
      end)
    s;
  String.trim (Buffer.contents buf)

let hash_output s =
  let normalized = normalize_output_for_hash s in
  if normalized = "" then None
  else Some (Digest.to_hex (Digest.string normalized))

let lookup_run_id_for_bg_task ~db ~bg_task_id =
  let sql =
    "SELECT id, job_name FROM cron_runs WHERE bg_task_id = ? ORDER BY id DESC \
     LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int bg_task_id)));
      if Sqlite3.step stmt = Sqlite3.Rc.ROW then
        match (Sqlite3.column stmt 0, Sqlite3.column stmt 1) with
        | Sqlite3.Data.INT rid, Sqlite3.Data.TEXT job_name ->
            Some (Int64.to_int rid, job_name)
        | _ -> None
      else None)

let last_n_output_hashes ~db ~job_name ~n =
  let sql =
    "SELECT output_hash FROM cron_runs WHERE job_name = ? AND output_hash IS \
     NOT NULL ORDER BY id DESC LIMIT ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT job_name));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int n)));
      let rec loop acc =
        if Sqlite3.step stmt = Sqlite3.Rc.ROW then
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT h -> loop (h :: acc)
          | _ -> loop acc
        else acc
      in
      loop [])

let update_run_output_hash ~db ~run_id ~hash =
  let sql = "UPDATE cron_runs SET output_hash = ? WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT hash));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int run_id)));
      ignore (Sqlite3.step stmt))

(* Public: scan whether the last `threshold` runs all share the same non-empty
   output hash. Returns Some hash when the loop is detected. *)
let detect_identical_output_loop ~db ~job_name ~threshold =
  let hashes = last_n_output_hashes ~db ~job_name ~n:threshold in
  if List.length hashes < threshold then None
  else
    match hashes with
    | [] -> None
    | h :: rest when List.for_all (fun h' -> h' = h) rest -> Some h
    | _ -> None

(* B665: shared hash-and-detect step. Called from both bg-task completion
   (mark_run_output) and the inline cron-tick path (mark_run_output_by_run_id)
   so degenerate-loop detection fires regardless of how the cron job
   executed. *)
let hash_and_detect_loop ~db ~run_id ~job_name ~output =
  match hash_output output with
  | None -> ()
  | Some hash -> (
      update_run_output_hash ~db ~run_id ~hash;
      match
        detect_identical_output_loop ~db ~job_name
          ~threshold:identical_output_disable_threshold
      with
      | None -> ()
      | Some _ ->
          expire_job_inline ~db ~name:job_name;
          Logs.warn (fun m ->
              m
                "Cron job %S disabled after %d consecutive identical outputs — \
                 investigate empty config / prompt shortcuts. Re-enable with \
                 `clawq cron enable %s` after fixing."
                job_name identical_output_disable_threshold job_name))

(* Called from the bg-task completion path (daemon_util) so the scheduler can
   record output for cron-triggered runs and disable degenerate loops. Safe to
   call with a non-cron task id — it just returns None when no row matches. *)
let table_exists ~db ~name =
  let sql = "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
      Sqlite3.step stmt = Sqlite3.Rc.ROW)

let mark_run_output ~db ~bg_task_id ~output =
  if not (table_exists ~db ~name:"cron_runs") then None
  else
    match lookup_run_id_for_bg_task ~db ~bg_task_id with
    | None -> None
    | Some (run_id, job_name) ->
        hash_and_detect_loop ~db ~run_id ~job_name ~output;
        Some job_name

(* B665: called from the inline cron tick (where we already have run_id and
   job_name in scope) so cron jobs that never spawn a Background_task — i.e.,
   the normal scheduled tick path — also get their output hashed and the
   consecutive-identical-output safeguard applied. *)
let mark_run_output_by_run_id ~db ~run_id ~job_name ~output =
  hash_and_detect_loop ~db ~run_id ~job_name ~output
