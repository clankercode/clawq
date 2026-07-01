(** Workflow run trigger model.

    Tracks pipeline-based workflow runs triggered from rooms or CLI commands.
    Each workflow run maps to a structured pipeline execution and an associated
    background task, with progress reported back to the originating room.

    Trigger sources: room commands or manual CLI. *)

(** {1 Types} *)

(** What triggered this workflow run. *)
type trigger_source =
  | Room_command of { room_id : string; requester_id : string }
      (** Triggered by a room slash command *)
  | Manual  (** Manually triggered via CLI *)

(** Status of a workflow run. *)
type run_status =
  | Pending  (** Queued but not yet started *)
  | Running  (** Currently executing *)
  | Completed  (** Finished successfully *)
  | Failed  (** Finished with error *)

type workflow_run = {
  id : int;
  pipeline_name : string;  (** Name of the structured pipeline *)
  pipeline_version : string;  (** Pipeline version at time of trigger *)
  inputs : (string * string) list;  (** Pipeline input values *)
  trigger_source : trigger_source;
  status : run_status;
  task_id : int option;  (** Associated background task ID *)
  room_id : string;  (** Originating room ID *)
  requester_id : string;  (** Who triggered the run *)
  result_preview : string option;  (** Summary of results *)
  error_message : string option;  (** Error details if failed *)
  created_at : string;
  started_at : string option;
  finished_at : string option;
}
(** A workflow run record with full context. *)

(** {1 Serialization} *)

let trigger_source_to_json = function
  | Room_command { room_id; requester_id } ->
      `Assoc
        [
          ("type", `String "room_command");
          ("room_id", `String room_id);
          ("requester_id", `String requester_id);
        ]
  | Manual -> `Assoc [ ("type", `String "manual") ]

let trigger_source_of_json json =
  let open Yojson.Safe.Util in
  let typ =
    json |> member "type" |> to_string_option |> Option.value ~default:""
  in
  match typ with
  | "room_command" ->
      let room_id =
        json |> member "room_id" |> to_string_option |> Option.value ~default:""
      in
      let requester_id =
        json |> member "requester_id" |> to_string_option
        |> Option.value ~default:""
      in
      Room_command { room_id; requester_id }
  | "manual" -> Manual
  | _ -> Manual

let run_status_to_string = function
  | Pending -> "pending"
  | Running -> "running"
  | Completed -> "completed"
  | Failed -> "failed"

let run_status_of_string = function
  | "pending" -> Pending
  | "running" -> Running
  | "completed" -> Completed
  | "failed" -> Failed
  | _ -> Pending

let inputs_to_json inputs =
  `Assoc (List.map (fun (k, v) -> (k, `String v)) inputs)

let inputs_of_json json =
  let open Yojson.Safe.Util in
  match json with
  | `Assoc pairs ->
      List.filter_map
        (fun (k, v) ->
          match to_string_option v with Some s -> Some (k, s) | None -> None)
        pairs
  | _ -> []

let workflow_run_to_json run =
  `Assoc
    [
      ("id", `Int run.id);
      ("pipeline_name", `String run.pipeline_name);
      ("pipeline_version", `String run.pipeline_version);
      ("inputs", inputs_to_json run.inputs);
      ("trigger_source", trigger_source_to_json run.trigger_source);
      ( "status",
        `String
          (match run.status with
          | Pending -> "pending"
          | Running -> "running"
          | Completed -> "completed"
          | Failed -> "failed") );
      ("task_id", match run.task_id with Some id -> `Int id | None -> `Null);
      ("room_id", `String run.room_id);
      ("requester_id", `String run.requester_id);
      ( "result_preview",
        match run.result_preview with Some s -> `String s | None -> `Null );
      ( "error_message",
        match run.error_message with Some s -> `String s | None -> `Null );
      ("created_at", `String run.created_at);
      ( "started_at",
        match run.started_at with Some s -> `String s | None -> `Null );
      ( "finished_at",
        match run.finished_at with Some s -> `String s | None -> `Null );
    ]

let workflow_run_to_string run =
  Yojson.Safe.to_string (workflow_run_to_json run)

(** {1 Database schema} *)

let exec_exn db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "workflow_run_trigger schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

(** Create the workflow_runs table. Idempotent via IF NOT EXISTS. *)
let init_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS workflow_runs (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     pipeline_name TEXT NOT NULL,\n\
    \     pipeline_version TEXT NOT NULL,\n\
    \     inputs_json TEXT NOT NULL,\n\
    \     trigger_source TEXT NOT NULL,\n\
    \     status TEXT NOT NULL DEFAULT 'pending',\n\
    \     task_id INTEGER,\n\
    \     room_id TEXT NOT NULL,\n\
    \     requester_id TEXT NOT NULL,\n\
    \     result_preview TEXT,\n\
    \     error_message TEXT,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     started_at TEXT,\n\
    \     finished_at TEXT\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_workflow_runs_status ON \
     workflow_runs(status)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_workflow_runs_room ON \
     workflow_runs(room_id)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_workflow_runs_task ON \
     workflow_runs(task_id)"

(** {1 Database helpers} *)

let text_column stmt idx =
  match Sqlite3.column stmt idx with Sqlite3.Data.TEXT s -> s | _ -> ""

let int_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | _ -> 0

let opt_int_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT n -> Some (Int64.to_int n)
  | Sqlite3.Data.NULL -> None
  | _ -> None

let opt_text_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.TEXT s -> Some s
  | Sqlite3.Data.NULL -> None
  | _ -> None

let select_columns =
  "id, pipeline_name, pipeline_version, inputs_json, trigger_source, status, \
   task_id, room_id, requester_id, result_preview, error_message, created_at, \
   started_at, finished_at"

let workflow_run_of_stmt stmt =
  let inputs_json_str = text_column stmt 3 in
  let inputs =
    try inputs_of_json (Yojson.Safe.from_string inputs_json_str) with _ -> []
  in
  let trigger_json_str = text_column stmt 4 in
  let trigger_source =
    try trigger_source_of_json (Yojson.Safe.from_string trigger_json_str)
    with _ -> Manual
  in
  {
    id = int_column stmt 0;
    pipeline_name = text_column stmt 1;
    pipeline_version = text_column stmt 2;
    inputs;
    trigger_source;
    status = run_status_of_string (text_column stmt 5);
    task_id = opt_int_column stmt 6;
    room_id = text_column stmt 7;
    requester_id = text_column stmt 8;
    result_preview = opt_text_column stmt 9;
    error_message = opt_text_column stmt 10;
    created_at = text_column stmt 11;
    started_at = opt_text_column stmt 12;
    finished_at = opt_text_column stmt 13;
  }

let bind_params stmt params =
  List.iteri
    (fun i value -> ignore (Sqlite3.bind stmt (i + 1) value : Sqlite3.Rc.t))
    params

(** {1 CRUD operations} *)

(** [find_by_id ~db ~id] retrieves a workflow run by its ID. *)
let find_by_id ~db ~id =
  let sql =
    Printf.sprintf "SELECT %s FROM workflow_runs WHERE id = ?" select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.INT (Int64.of_int id) ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (workflow_run_of_stmt stmt)
      | _ -> None)

(** [create ~db ~pipeline_name ~pipeline_version ~inputs ~trigger_source
     ~room_id ~requester_id ()] creates a new workflow run in pending state. *)
let create ~db ~pipeline_name ~pipeline_version ~inputs ~trigger_source ~room_id
    ~requester_id () =
  let inputs_json = Yojson.Safe.to_string (inputs_to_json inputs) in
  let trigger_json =
    Yojson.Safe.to_string (trigger_source_to_json trigger_source)
  in
  let sql =
    "INSERT INTO workflow_runs (pipeline_name, pipeline_version, inputs_json, \
     trigger_source, status, room_id, requester_id) VALUES (?, ?, ?, ?, \
     'pending', ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT pipeline_name;
          Sqlite3.Data.TEXT pipeline_version;
          Sqlite3.Data.TEXT inputs_json;
          Sqlite3.Data.TEXT trigger_json;
          Sqlite3.Data.TEXT room_id;
          Sqlite3.Data.TEXT requester_id;
        ];
      ignore (Sqlite3.step stmt : Sqlite3.Rc.t));
  let id = Int64.to_int (Sqlite3.last_insert_rowid db) in
  match find_by_id ~db ~id with
  | Some run ->
      (* Record backlink from room to workflow run *)
      Room_github_backlinks.record_triggered_run ~db ~repo:pipeline_name
        ~pr_number:0 ~github_item_type:Workflow_run ~room_id
        ~room_item_type:Workflow_run_room ~room_item_id:(string_of_int run.id)
        ();
      run
  | None ->
      failwith "workflow_run_trigger create: record not found after insert"

(** [set_running ~db ~id ~task_id] marks a workflow run as running and
    associates it with a background task. Returns true if updated. *)
let set_running ~db ~id ~task_id =
  let sql =
    "UPDATE workflow_runs SET status = 'running', task_id = ?, started_at = \
     datetime('now') WHERE id = ? AND status = 'pending'"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.INT (Int64.of_int task_id);
          Sqlite3.Data.INT (Int64.of_int id);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db > 0
      | rc ->
          failwith
            (Printf.sprintf "workflow_run_trigger set_running failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [set_completed ~db ~id ~result_preview] marks a workflow run as completed.
*)
let set_completed ~db ~id ?result_preview () =
  let sql =
    "UPDATE workflow_runs SET status = 'completed', result_preview = ?, \
     finished_at = datetime('now') WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          (match result_preview with
          | Some s -> Sqlite3.Data.TEXT s
          | None -> Sqlite3.Data.NULL);
          Sqlite3.Data.INT (Int64.of_int id);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db > 0
      | rc ->
          failwith
            (Printf.sprintf "workflow_run_trigger set_completed failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [set_failed ~db ~id ~error_message] marks a workflow run as failed. *)
let set_failed ~db ~id ~error_message =
  let sql =
    "UPDATE workflow_runs SET status = 'failed', error_message = ?, \
     finished_at = datetime('now') WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [ Sqlite3.Data.TEXT error_message; Sqlite3.Data.INT (Int64.of_int id) ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db > 0
      | rc ->
          failwith
            (Printf.sprintf "workflow_run_trigger set_failed failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [find_by_task_id ~db ~task_id] finds a workflow run by its associated
    background task ID. *)
let find_by_task_id ~db ~task_id =
  let sql =
    Printf.sprintf "SELECT %s FROM workflow_runs WHERE task_id = ?"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.INT (Int64.of_int task_id) ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (workflow_run_of_stmt stmt)
      | _ -> None)

(** [sync_from_background_task ~db ~task_id ~status ~result_preview
     ~error_message] synchronizes the workflow run status from a completed
    background task. Called when a background task finishes so the workflow run
    reflects the final state. *)
let sync_from_background_task ~db ~task_id ~status ~result_preview
    ~error_message =
  match find_by_task_id ~db ~task_id with
  | None -> false
  | Some run ->
      if run.status <> Running then false
      else
        let ok =
          match status with
          | `Succeeded -> set_completed ~db ~id:run.id ?result_preview ()
          | `Failed ->
              let msg =
                Option.value error_message ~default:"Background task failed"
              in
              set_failed ~db ~id:run.id ~error_message:msg
        in
        ok

(** [find_pending ~db ?limit ()] returns pending workflow runs. *)
let find_pending ~db ?(limit = 50) () =
  let sql =
    Printf.sprintf
      "SELECT %s FROM workflow_runs WHERE status = 'pending' ORDER BY \
       created_at ASC LIMIT ?"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  let runs = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.INT (Int64.of_int limit) ];
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            runs := workflow_run_of_stmt stmt :: !runs;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "workflow_run_trigger find_pending failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !runs

(** [find_by_room ~db ~room_id ?limit ()] returns workflow runs for a room. *)
let find_by_room ~db ~room_id ?(limit = 20) () =
  let sql =
    Printf.sprintf
      "SELECT %s FROM workflow_runs WHERE room_id = ? ORDER BY created_at DESC \
       LIMIT ?"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  let runs = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [ Sqlite3.Data.TEXT room_id; Sqlite3.Data.INT (Int64.of_int limit) ];
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            runs := workflow_run_of_stmt stmt :: !runs;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "workflow_run_trigger find_by_room failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !runs

(** [count_by_status ~db ()] returns counts of workflow runs by status. *)
let count_by_status ~db () =
  let sql = "SELECT status, COUNT(*) FROM workflow_runs GROUP BY status" in
  let stmt = Sqlite3.prepare db sql in
  let counts = Hashtbl.create 4 in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let status = text_column stmt 0 in
            let count = int_column stmt 1 in
            Hashtbl.replace counts status count;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "workflow_run_trigger count_by_status failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  counts

(** {1 Prompt assembly} *)

(** [build_workflow_prompt ~pipeline ~inputs ()] assembles the enriched prompt
    for a workflow run background task. Includes pipeline metadata, step
    descriptions, and input values. The background agent is expected to execute
    the pipeline steps in order. *)
let build_workflow_prompt ~(pipeline : Structured_pipeline.pipeline_def) ~inputs
    () =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Printf.sprintf "You are executing the \"%s\" workflow pipeline (v%s).\n\n"
       pipeline.name pipeline.version);
  if pipeline.description <> "" then
    Buffer.add_string buf
      (Printf.sprintf "Description: %s\n\n" pipeline.description);
  (* Inputs *)
  if inputs <> [] then begin
    Buffer.add_string buf "## Inputs\n";
    List.iter
      (fun (key, value) ->
        Buffer.add_string buf (Printf.sprintf "- %s: %s\n" key value))
      inputs
  end;
  (* Step descriptions *)
  if pipeline.steps <> [] then begin
    Buffer.add_string buf "\n## Steps\n";
    List.iteri
      (fun i (step : Structured_pipeline.step) ->
        Buffer.add_string buf (Printf.sprintf "%d. **%s**" (i + 1) step.name);
        match step.kind with
        | Structured_pipeline.Prompt_step { prompt; _ } ->
            let truncated =
              if String.length prompt > 200 then String.sub prompt 0 197 ^ "..."
              else prompt
            in
            Buffer.add_string buf (Printf.sprintf " (prompt): %s\n" truncated)
        | Pipeline_step { pipeline = sub; _ } ->
            Buffer.add_string buf (Printf.sprintf " (sub-pipeline: %s)\n" sub)
        | Agent_step { task; _ } ->
            let truncated =
              if String.length task > 200 then String.sub task 0 197 ^ "..."
              else task
            in
            Buffer.add_string buf (Printf.sprintf " (agent): %s\n" truncated))
      pipeline.steps
  end;
  Buffer.add_string buf
    "\n\
     ## Execution Contract\n\
     - Execute each step in order, using the outputs from previous steps.\n\
     - For prompt steps: generate structured output matching the expected \
     schema.\n\
     - For agent steps: perform the described task and report results.\n\
     - If a step fails, report the error and stop execution.\n\
     - Summarize the final results at the end.\n\
     - Commit all changes before reporting completion.";
  Buffer.contents buf

(** {1 Input validation} *)

(** [validate_and_resolve_inputs ~pipeline ~inputs ()] validates required inputs
    and applies defaults. Returns [Ok effective_inputs] or [Error msg] if
    required inputs are missing. *)
let validate_and_resolve_inputs ~(pipeline : Structured_pipeline.pipeline_def)
    ~inputs () =
  match Structured_pipeline.validate_pipeline_def pipeline with
  | Error errs ->
      Error
        (Printf.sprintf "Pipeline \"%s\" has validation errors:\n%s"
           pipeline.name
           (String.concat "\n" (List.map (fun e -> "  - " ^ e) errs)))
  | Ok () -> (
      let missing =
        List.filter_map
          (fun (key, (def : Structured_pipeline.input_def)) ->
            if
              def.required
              && (not (List.mem_assoc key inputs))
              && def.default = None
            then Some key
            else None)
          pipeline.inputs
      in
      match missing with
      | _ :: _ ->
          Error
            (Printf.sprintf "Missing required input(s): %s"
               (String.concat ", " missing))
      | [] ->
          let effective_inputs =
            List.map
              (fun (key, (def : Structured_pipeline.input_def)) ->
                match List.assoc_opt key inputs with
                | Some v -> (key, v)
                | None -> (
                    match def.default with
                    | Some d -> (key, d)
                    | None -> (key, "")))
              pipeline.inputs
            @ List.filter
                (fun (k, _) -> not (List.mem_assoc k pipeline.inputs))
                inputs
          in
          Ok effective_inputs)

(** {1 Formatting} *)

let format_workflow_run (run : workflow_run) =
  let status_str = run_status_to_string run.status in
  let trigger_str =
    match run.trigger_source with
    | Room_command { room_id; _ } -> Printf.sprintf "room:%s" room_id
    | Manual -> "manual"
  in
  Printf.sprintf "[%s] %s v%s (%s) %s" status_str run.pipeline_name
    run.pipeline_version trigger_str
    (match run.result_preview with
    | Some s -> String.sub s 0 (min 60 (String.length s))
    | None -> "")

let format_workflow_run_list runs =
  if runs = [] then "No workflow runs found."
  else
    let lines =
      List.map
        (fun run -> Printf.sprintf "  #%d %s" run.id (format_workflow_run run))
        runs
    in
    "Workflow runs:\n" ^ String.concat "\n" lines
