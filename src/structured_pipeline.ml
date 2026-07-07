(* structured_pipeline.ml - Pipeline parsing, discovery, DB, and formatting *)

include Structured_pipeline_types

(* ── Paths ─────────────────────────────────────────────────────────────── *)

let pipelines_dir () = Dot_dir.sub "pipelines"

let ensure_pipelines_dir () =
  let dir = pipelines_dir () in
  (try
     if not (Sys.file_exists dir) then begin
       let parent = Dot_dir.path () in
       (try if not (Sys.file_exists parent) then Sys.mkdir parent 0o755
        with _ -> ());
       Sys.mkdir dir 0o755
     end
   with _ -> ());
  dir

(* ── Name validation ───────────────────────────────────────────────────── *)

let is_valid_pipeline_name name =
  name <> ""
  && String.length name <= 64
  &&
  let ok = ref true in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> ()
      | _ -> ok := false)
    name;
  !ok

(* ── YAML loading ──────────────────────────────────────────────────────── *)

(* Convert Yaml.value to Yojson.Safe.t *)
let rec yaml_value_to_yojson (v : Yaml.value) : Yojson.Safe.t =
  match v with
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Float f ->
      if Float.is_integer f && Float.is_finite f then `Int (int_of_float f)
      else `Float f
  | `String s -> `String s
  | `A items -> `List (List.map yaml_value_to_yojson items)
  | `O pairs ->
      `Assoc (List.map (fun (k, v) -> (k, yaml_value_to_yojson v)) pairs)

let load_yaml_file path =
  try
    let ic = open_in path in
    let content =
      Fun.protect
        (fun () ->
          let len = in_channel_length ic in
          let buf = Bytes.create len in
          really_input ic buf 0 len;
          Bytes.to_string buf)
        ~finally:(fun () -> close_in ic)
    in
    match Yaml.of_string content with
    | Ok yaml_value -> Ok (yaml_value_to_yojson yaml_value)
    | Error (`Msg msg) -> Error (Printf.sprintf "YAML parse error: %s" msg)
  with exn ->
    Error (Printf.sprintf "Failed to read %s: %s" path (Printexc.to_string exn))

(* ── JSON helpers ──────────────────────────────────────────────────────── *)

let string_of key pairs =
  match List.assoc_opt key pairs with Some (`String s) -> Some s | _ -> None

let assoc_of key pairs =
  match List.assoc_opt key pairs with Some (`Assoc a) -> Some a | _ -> None

let list_of key pairs =
  match List.assoc_opt key pairs with Some (`List l) -> Some l | _ -> None

let bool_of key pairs =
  match List.assoc_opt key pairs with Some (`Bool b) -> Some b | _ -> None

(* ── Pipeline parsing ──────────────────────────────────────────────────── *)

let parse_input_def (json : Yojson.Safe.t) =
  match json with
  | `Assoc pairs ->
      let input_type =
        match string_of "type" pairs with Some t -> t | None -> "string"
      in
      let description =
        match string_of "description" pairs with Some d -> d | None -> ""
      in
      let required =
        match bool_of "required" pairs with Some b -> b | None -> false
      in
      let default = string_of "default" pairs in
      Ok { input_type; description; required; default }
  | _ -> Error "input definition must be an object"

let parse_step (json : Yojson.Safe.t) =
  match json with
  | `Assoc pairs -> (
      match string_of "name" pairs with
      | None -> Error "step must have a \"name\" field"
      | Some name ->
          if string_of "pipeline" pairs <> None then
            (* Pipeline step *)
            let pipeline =
              match string_of "pipeline" pairs with Some p -> p | None -> ""
            in
            let input_map =
              match assoc_of "input_map" pairs with
              | Some im ->
                  List.filter_map
                    (fun (k, v) ->
                      match v with `String s -> Some (k, s) | _ -> None)
                    im
              | None -> []
            in
            Ok { name; kind = Pipeline_step { pipeline; input_map } }
          else if string_of "task" pairs <> None then
            (* Agent step *)
            let task =
              match string_of "task" pairs with Some t -> t | None -> ""
            in
            let model = string_of "model" pairs in
            let max_turns =
              match List.assoc_opt "max_turns" pairs with
              | Some (`Int n) -> Some n
              | Some (`Float f) -> Some (int_of_float f)
              | _ -> None
            in
            Ok { name; kind = Agent_step { task; model; max_turns } }
          else
            (* Prompt step *)
            let prompt =
              match string_of "prompt" pairs with Some p -> p | None -> ""
            in
            let system_prompt = string_of "system_prompt" pairs in
            let model = string_of "model" pairs in
            let output_schema =
              match List.assoc_opt "output_schema" pairs with
              | Some s -> s
              | None -> `Assoc [ ("type", `String "object") ]
            in
            let max_retries =
              match List.assoc_opt "max_retries" pairs with
              | Some (`Int n) -> n
              | Some (`Float f) -> int_of_float f
              | _ -> 1
            in
            Ok
              {
                name;
                kind =
                  Prompt_step
                    { prompt; system_prompt; model; output_schema; max_retries };
              })
  | _ -> Error "step must be an object"

let parse_pipeline_def ?(source_path = "") (json : Yojson.Safe.t) =
  match json with
  | `Assoc pairs -> (
      match string_of "name" pairs with
      | None -> Error "pipeline definition must have a \"name\" field"
      | Some name ->
          let version =
            match string_of "version" pairs with Some v -> v | None -> "1.0"
          in
          let description =
            match string_of "description" pairs with Some d -> d | None -> ""
          in
          let inputs, input_errors =
            match assoc_of "inputs" pairs with
            | Some input_pairs ->
                List.fold_left
                  (fun (acc, errs) (key, v) ->
                    match parse_input_def v with
                    | Ok def -> ((key, def) :: acc, errs)
                    | Error msg ->
                        (acc, Printf.sprintf "input '%s': %s" key msg :: errs))
                  ([], []) input_pairs
            | None -> ([], [])
          in
          let steps, step_errors =
            match list_of "steps" pairs with
            | Some step_list ->
                List.fold_left
                  (fun (acc, errs) s ->
                    match parse_step s with
                    | Ok step -> (step :: acc, errs)
                    | Error msg -> (acc, ("step: " ^ msg) :: errs))
                  ([], []) step_list
            | None -> ([], [])
          in
          let all_errors = input_errors @ step_errors in
          if all_errors <> [] then
            Error (String.concat "; " (List.rev all_errors))
          else
            Ok
              {
                name;
                version;
                description;
                inputs = List.rev inputs;
                steps = List.rev steps;
                source_path;
              })
  | _ -> Error "pipeline definition must be a JSON object"

let load_pipeline path =
  let loader =
    if Filename.check_suffix path ".yaml" || Filename.check_suffix path ".yml"
    then load_yaml_file
    else fun p ->
      try Ok (Yojson.Safe.from_file p)
      with exn -> Error (Printexc.to_string exn)
  in
  match loader path with
  | Error e -> Error e
  | Ok json -> parse_pipeline_def ~source_path:path json

(* ── Discovery ─────────────────────────────────────────────────────────── *)

let builtins : (unit -> pipeline_def) list ref = ref []
let register_builtin f = builtins := f :: !builtins

let builtin_research_report =
  Structured_pipeline_builtins.builtin_research_report

let builtin_build_review_carm =
  Structured_pipeline_builtins.builtin_build_review_carm

let builtin_plan_build_review_carm =
  Structured_pipeline_builtins.builtin_plan_build_review_carm

let () = List.iter register_builtin Structured_pipeline_builtins.all

let discover_user_pipelines () =
  let dir = pipelines_dir () in
  if Sys.file_exists dir && Sys.is_directory dir then
    let files = Sys.readdir dir |> Array.to_list in
    List.filter_map
      (fun f ->
        if
          Filename.check_suffix f ".yaml"
          || Filename.check_suffix f ".yml"
          || Filename.check_suffix f ".json"
        then
          let path = Filename.concat dir f in
          match load_pipeline path with Ok def -> Some def | Error _ -> None
        else None)
      files
  else []

let discover_pipelines () =
  let user = discover_user_pipelines () in
  let builtin = List.map (fun f -> f ()) !builtins in
  user @ builtin

let find_pipeline name =
  let name_lower = String.lowercase_ascii name in
  List.find_opt
    (fun (p : pipeline_def) -> String.lowercase_ascii p.name = name_lower)
    (discover_pipelines ())

(* ── Validation ────────────────────────────────────────────────────────── *)

let validate_pipeline_def (def : pipeline_def) =
  let errors = ref [] in
  let add msg = errors := msg :: !errors in
  if not (is_valid_pipeline_name def.name) then
    add "name must be alphanumeric with hyphens/underscores, max 64 chars";
  if def.steps = [] then add "pipeline must have at least one step";
  (* unique step names *)
  let seen = Hashtbl.create 8 in
  List.iter
    (fun (s : step) ->
      if Hashtbl.mem seen s.name then
        add (Printf.sprintf "duplicate step name \"%s\"" s.name)
      else Hashtbl.add seen s.name true)
    def.steps;
  (* validate step schemas *)
  List.iter
    (fun (s : step) ->
      match s.kind with
      | Prompt_step { output_schema; prompt; _ } ->
          (match
             Structured_pipeline_schema.validate_schema_itself output_schema
           with
          | Ok () -> ()
          | Error msg ->
              add
                (Printf.sprintf "step \"%s\" has invalid output_schema: %s"
                   s.name msg));
          if String.trim prompt = "" then
            add (Printf.sprintf "step \"%s\" has empty prompt" s.name)
      | Pipeline_step { pipeline = p; _ } ->
          if String.trim p = "" then
            add
              (Printf.sprintf "step \"%s\" references empty pipeline name"
                 s.name)
      | Agent_step { task; _ } ->
          if String.trim task = "" then
            add (Printf.sprintf "step \"%s\" has empty task" s.name))
    def.steps;
  match List.rev !errors with [] -> Ok () | errs -> Error errs

(* ── Template substitution ─────────────────────────────────────────────── *)

(* Note: template variables are delimited by {{ and }}. Nested braces
   within variable expressions are not supported (e.g., {{json: {"key":
   "val"}}} would match the inner }} prematurely). Escaped \{{ is also
   not handled. If you need literal braces in output, use step outputs
   that contain the desired text. *)
(* Note: input_map keys are static; only values support {{template}}
   substitution. Dynamic key references are not supported. *)
let substitute_template template ~inputs ~step_outputs =
  let buf = Buffer.create (String.length template) in
  let len = String.length template in
  let i = ref 0 in
  while !i < len do
    if !i + 1 < len && template.[!i] = '{' && template.[!i + 1] = '{' then begin
      (* Find closing }} *)
      let start = !i + 2 in
      match String.index_from_opt template start '}' with
      | Some j when j + 1 < len && template.[j + 1] = '}' ->
          let var = String.trim (String.sub template start (j - start)) in
          let replacement =
            if String.length var > 6 && String.sub var 0 6 = "input." then
              let key = String.sub var 6 (String.length var - 6) in
              match List.assoc_opt key inputs with
              | Some v -> v
              | None -> "{{" ^ var ^ "}}"
            else
              (* step_name or step_name.field *)
              let step_name, field =
                match String.index_opt var '.' with
                | Some dot ->
                    ( String.sub var 0 dot,
                      Some
                        (String.sub var (dot + 1) (String.length var - dot - 1))
                    )
                | None -> (var, None)
              in
              match List.assoc_opt step_name step_outputs with
              | Some (json : Yojson.Safe.t) -> (
                  match field with
                  | None -> Yojson.Safe.to_string json
                  | Some f -> (
                      match json with
                      | `Assoc pairs -> (
                          match List.assoc_opt f pairs with
                          | Some (`String s) -> s
                          | Some v -> Yojson.Safe.to_string v
                          | None -> "{{" ^ var ^ "}}")
                      | _ -> "{{" ^ var ^ "}}"))
              | None -> "{{" ^ var ^ "}}"
          in
          Buffer.add_string buf replacement;
          i := j + 2
      | _ ->
          Buffer.add_char buf template.[!i];
          incr i
    end
    else begin
      Buffer.add_char buf template.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* ── DB operations ─────────────────────────────────────────────────────── *)

let init_schema db =
  let exec sql = Sql_util.exec_exn db sql in
  exec
    "CREATE TABLE IF NOT EXISTS structured_pipeline_runs (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  pipeline_name TEXT NOT NULL,\n\
    \  pipeline_version TEXT,\n\
    \  inputs_json TEXT,\n\
    \  status TEXT NOT NULL DEFAULT 'running',\n\
    \  error_msg TEXT,\n\
    \  started_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  finished_at TEXT,\n\
    \  total_elapsed_s REAL\n\
     )";
  exec
    "CREATE TABLE IF NOT EXISTS structured_pipeline_step_results (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  run_id INTEGER NOT NULL REFERENCES structured_pipeline_runs(id),\n\
    \  step_name TEXT NOT NULL,\n\
    \  step_index INTEGER NOT NULL,\n\
    \  output_json TEXT,\n\
    \  output_raw TEXT,\n\
    \  model_used TEXT,\n\
    \  attempts INTEGER NOT NULL DEFAULT 1,\n\
    \  elapsed_s REAL,\n\
    \  prompt_tokens INTEGER,\n\
    \  completion_tokens INTEGER,\n\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
     )"

let insert_run ~db ~pipeline_name ~pipeline_version ~inputs =
  let inputs_json =
    `Assoc (List.map (fun (k, v) -> (k, `String v)) inputs)
    |> Yojson.Safe.to_string
  in
  let sql =
    "INSERT INTO structured_pipeline_runs (pipeline_name, pipeline_version, \
     inputs_json) VALUES (?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind_text stmt 1 pipeline_name);
      ignore (Sqlite3.bind_text stmt 2 pipeline_version);
      ignore (Sqlite3.bind_text stmt 3 inputs_json);
      ignore (Sqlite3.step stmt));
  Int64.to_int (Sqlite3.last_insert_rowid db)

let update_run_status ~db ~run_id ~status ?error_msg ?elapsed_s () =
  let status_s =
    match status with
    | Running -> "running"
    | Completed -> "completed"
    | Failed _ -> "failed"
    | Cancelled -> "cancelled"
  in
  let err =
    match (error_msg, status) with
    | Some m, _ -> Some m
    | None, Failed m -> Some m
    | _ -> None
  in
  let sql =
    "UPDATE structured_pipeline_runs SET status = ?, error_msg = ?, \
     finished_at = datetime('now'), total_elapsed_s = ? WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind_text stmt 1 status_s);
      (match err with
      | Some e -> ignore (Sqlite3.bind_text stmt 2 e)
      | None -> ignore (Sqlite3.bind stmt 2 Sqlite3.Data.NULL));
      (match elapsed_s with
      | Some e -> ignore (Sqlite3.bind_double stmt 3 e)
      | None -> ignore (Sqlite3.bind stmt 3 Sqlite3.Data.NULL));
      ignore (Sqlite3.bind_int stmt 4 run_id);
      ignore (Sqlite3.step stmt))

let add_step_result ~db ~run_id ~step_index ~(result : step_result) =
  let sql =
    "INSERT INTO structured_pipeline_step_results (run_id, step_name, \
     step_index, output_json, output_raw, model_used, attempts, elapsed_s, \
     prompt_tokens, completion_tokens) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind_int stmt 1 run_id);
      ignore (Sqlite3.bind_text stmt 2 result.step_name);
      ignore (Sqlite3.bind_int stmt 3 step_index);
      ignore
        (Sqlite3.bind_text stmt 4 (Yojson.Safe.to_string result.output_json));
      ignore (Sqlite3.bind_text stmt 5 result.output_raw);
      ignore (Sqlite3.bind_text stmt 6 result.model_used);
      ignore (Sqlite3.bind_int stmt 7 result.attempts);
      ignore (Sqlite3.bind_double stmt 8 result.elapsed_s);
      (match result.tokens with
      | Some (pt, ct) ->
          ignore (Sqlite3.bind_int stmt 9 pt);
          ignore (Sqlite3.bind_int stmt 10 ct)
      | None ->
          ignore (Sqlite3.bind stmt 9 Sqlite3.Data.NULL);
          ignore (Sqlite3.bind stmt 10 Sqlite3.Data.NULL));
      ignore (Sqlite3.step stmt))

type db_run = {
  id : int;
  pipeline_name : string;
  pipeline_version : string;
  inputs_json : string;
  status : string;
  error_msg : string option;
  started_at : string;
  finished_at : string option;
  total_elapsed_s : float option;
}

let list_runs ~db ?pipeline_name ~limit () =
  let sql =
    match pipeline_name with
    | Some _ ->
        "SELECT id, pipeline_name, pipeline_version, inputs_json, status, \
         error_msg, started_at, finished_at, total_elapsed_s FROM \
         structured_pipeline_runs WHERE pipeline_name = ? ORDER BY id DESC \
         LIMIT ?"
    | None ->
        "SELECT id, pipeline_name, pipeline_version, inputs_json, status, \
         error_msg, started_at, finished_at, total_elapsed_s FROM \
         structured_pipeline_runs ORDER BY id DESC LIMIT ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      (match pipeline_name with
      | Some name ->
          ignore (Sqlite3.bind_text stmt 1 name);
          ignore (Sqlite3.bind_int stmt 2 limit)
      | None -> ignore (Sqlite3.bind_int stmt 1 limit));
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let id = Sqlite3.column_int stmt 0 in
        let pname = Sqlite3.column_text stmt 1 in
        let pversion = Sqlite3.column_text stmt 2 in
        let inputs_json = Sqlite3.column_text stmt 3 in
        let status = Sqlite3.column_text stmt 4 in
        let error_msg =
          match Sqlite3.column stmt 5 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let started_at = Sqlite3.column_text stmt 6 in
        let finished_at =
          match Sqlite3.column stmt 7 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let total_elapsed_s =
          match Sqlite3.column stmt 8 with
          | Sqlite3.Data.FLOAT f -> Some f
          | _ -> None
        in
        rows :=
          {
            id;
            pipeline_name = pname;
            pipeline_version = pversion;
            inputs_json;
            status;
            error_msg;
            started_at;
            finished_at;
            total_elapsed_s;
          }
          :: !rows
      done;
      List.rev !rows)

let get_run ~db ~run_id =
  let sql =
    "SELECT id, pipeline_name, pipeline_version, inputs_json, status, \
     error_msg, started_at, finished_at, total_elapsed_s FROM \
     structured_pipeline_runs WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind_int stmt 1 run_id);
      if Sqlite3.step stmt = Sqlite3.Rc.ROW then
        let id = Sqlite3.column_int stmt 0 in
        let pipeline_name = Sqlite3.column_text stmt 1 in
        let pipeline_version = Sqlite3.column_text stmt 2 in
        let inputs_json = Sqlite3.column_text stmt 3 in
        let status = Sqlite3.column_text stmt 4 in
        let error_msg =
          match Sqlite3.column stmt 5 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let started_at = Sqlite3.column_text stmt 6 in
        let finished_at =
          match Sqlite3.column stmt 7 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let total_elapsed_s =
          match Sqlite3.column stmt 8 with
          | Sqlite3.Data.FLOAT f -> Some f
          | _ -> None
        in
        Some
          {
            id;
            pipeline_name;
            pipeline_version;
            inputs_json;
            status;
            error_msg;
            started_at;
            finished_at;
            total_elapsed_s;
          }
      else None)

type db_step_result = {
  step_name : string;
  step_index : int;
  output_json : string;
  output_raw : string;
  model_used : string;
  attempts : int;
  elapsed_s : float;
}

let get_run_steps ~db ~run_id =
  let sql =
    "SELECT step_name, step_index, output_json, output_raw, model_used, \
     attempts, elapsed_s FROM structured_pipeline_step_results WHERE run_id = \
     ? ORDER BY step_index ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind_int stmt 1 run_id);
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let step_name = Sqlite3.column_text stmt 0 in
        let step_index = Sqlite3.column_int stmt 1 in
        let output_json = Sqlite3.column_text stmt 2 in
        let output_raw = Sqlite3.column_text stmt 3 in
        let model_used = Sqlite3.column_text stmt 4 in
        let attempts = Sqlite3.column_int stmt 5 in
        let elapsed_s =
          match Sqlite3.column stmt 6 with
          | Sqlite3.Data.FLOAT f -> f
          | _ -> 0.0
        in
        rows :=
          {
            step_name;
            step_index;
            output_json;
            output_raw;
            model_used;
            attempts;
            elapsed_s;
          }
          :: !rows
      done;
      List.rev !rows)

(* ── Scaffold ──────────────────────────────────────────────────────────── *)

let scaffold_pipeline ~name () =
  let dir = ensure_pipelines_dir () in
  let path = Filename.concat dir (name ^ ".yaml") in
  if Sys.file_exists path then
    Error (Printf.sprintf "File already exists: %s" path)
  else begin
    let content =
      Printf.sprintf
        {|name: %s
version: "1.0"
description: "TODO: describe what this pipeline does"
inputs:
  topic:
    type: string
    description: "Main input topic"
    required: true
steps:
  - name: analyze
    prompt: |
      Analyze the following topic: {{input.topic}}
      Return a JSON object with "summary" (string) and "key_points" (array of strings).
    output_schema:
      type: object
      properties:
        summary:
          type: string
        key_points:
          type: array
          items:
            type: string
      required:
        - summary
        - key_points
    max_retries: 2
|}
        name
    in
    let oc = open_out path in
    output_string oc content;
    close_out oc;
    Ok path
  end

(* ── Pipeline def to YAML string ───────────────────────────────────────── *)

let pipeline_def_to_yaml (def : pipeline_def) =
  let buf = Buffer.create 512 in
  let add fmt = Printf.bprintf buf fmt in
  add "name: %s\n" def.name;
  add "version: \"%s\"\n" def.version;
  add "description: \"%s\"\n" (String.escaped def.description);
  if def.inputs <> [] then begin
    add "inputs:\n";
    List.iter
      (fun (key, (inp : input_def)) ->
        add "  %s:\n" key;
        add "    type: %s\n" inp.input_type;
        if inp.description <> "" then
          add "    description: \"%s\"\n" (String.escaped inp.description);
        if inp.required then add "    required: true\n";
        match inp.default with
        | Some d -> add "    default: %s\n" d
        | None -> ())
      def.inputs
  end;
  if def.steps <> [] then begin
    add "steps:\n";
    List.iter
      (fun (s : step) ->
        add "  - name: %s\n" s.name;
        match s.kind with
        | Prompt_step
            { prompt; system_prompt; model; output_schema; max_retries } ->
            add "    prompt: |\n";
            let lines = String.split_on_char '\n' prompt in
            List.iter (fun line -> add "      %s\n" line) lines;
            (match system_prompt with
            | Some sp ->
                add "    system_prompt: |\n";
                let sp_lines = String.split_on_char '\n' sp in
                List.iter (fun line -> add "      %s\n" line) sp_lines
            | None -> ());
            (match model with Some m -> add "    model: %s\n" m | None -> ());
            add "    output_schema:\n";
            let schema_str =
              Yojson.Safe.pretty_to_string ~std:true output_schema
            in
            let schema_lines = String.split_on_char '\n' schema_str in
            List.iter (fun line -> add "      %s\n" line) schema_lines;
            if max_retries <> 1 then add "    max_retries: %d\n" max_retries
        | Pipeline_step { pipeline; input_map } ->
            add "    pipeline: %s\n" pipeline;
            if input_map <> [] then begin
              add "    input_map:\n";
              List.iter
                (fun (k, v) -> add "      %s: \"%s\"\n" k (String.escaped v))
                input_map
            end
        | Agent_step { task; model; max_turns } -> (
            add "    task: |\n";
            let lines = String.split_on_char '\n' task in
            List.iter (fun line -> add "      %s\n" line) lines;
            (match model with Some m -> add "    model: %s\n" m | None -> ());
            match max_turns with
            | Some n -> add "    max_turns: %d\n" n
            | None -> ()))
      def.steps
  end;
  Buffer.contents buf

(* ── Formatting helpers for CLI ────────────────────────────────────────── *)

let format_pipeline_list pipelines =
  if pipelines = [] then "No pipelines found."
  else
    let columns =
      Table_format.
        [
          { header = "NAME"; align = Left; min_width = 10; flex = false };
          { header = "VERSION"; align = Left; min_width = 5; flex = false };
          { header = "STEPS"; align = Right; min_width = 3; flex = false };
          { header = "DESCRIPTION"; align = Left; min_width = 10; flex = true };
          { header = "SOURCE"; align = Left; min_width = 8; flex = false };
        ]
    in
    let rows =
      List.map
        (fun (p : pipeline_def) ->
          [
            p.name;
            p.version;
            string_of_int (List.length p.steps);
            p.description;
            (if p.source_path = "(builtin)" then "(builtin)"
             else Filename.basename p.source_path);
          ])
        pipelines
    in
    "Pipelines:\n" ^ Table_format.render columns rows

let format_run_list runs =
  if runs = [] then "No pipeline runs found."
  else
    let columns =
      Table_format.
        [
          { header = "ID"; align = Right; min_width = 2; flex = false };
          { header = "PIPELINE"; align = Left; min_width = 10; flex = false };
          { header = "STATUS"; align = Left; min_width = 6; flex = false };
          { header = "ELAPSED"; align = Right; min_width = 5; flex = false };
          { header = "STARTED"; align = Left; min_width = 16; flex = false };
        ]
    in
    let rows =
      List.map
        (fun (r : db_run) ->
          [
            string_of_int r.id;
            r.pipeline_name;
            r.status;
            (match r.total_elapsed_s with
            | Some e -> Printf.sprintf "%.1fs" e
            | None -> "-");
            r.started_at;
          ])
        runs
    in
    "Pipeline runs:\n" ^ Table_format.render columns rows

let format_run_detail ~(run : db_run) ~steps =
  let buf = Buffer.create 512 in
  let add fmt = Printf.bprintf buf fmt in
  add "# Pipeline Run #%d\n\n" run.id;
  add "**Pipeline:** %s v%s\n" run.pipeline_name run.pipeline_version;
  add "**Status:** %s\n" run.status;
  add "**Started:** %s\n" run.started_at;
  (match run.finished_at with
  | Some f -> add "**Finished:** %s\n" f
  | None -> ());
  (match run.total_elapsed_s with
  | Some e -> add "**Elapsed:** %.1fs\n" e
  | None -> ());
  add "**Inputs:** %s\n\n" run.inputs_json;
  (match run.error_msg with Some e -> add "**Error:** %s\n\n" e | None -> ());
  if steps <> [] then begin
    add "## Step Results\n\n";
    List.iter
      (fun (sr : db_step_result) ->
        add "### %s (step %d, %d attempt(s), %.1fs)\n\n" sr.step_name
          sr.step_index sr.attempts sr.elapsed_s;
        add "**Model:** %s\n\n" sr.model_used;
        let output =
          if String.length sr.output_json > 500 then
            String.sub sr.output_json 0 497 ^ "..."
          else sr.output_json
        in
        add "```json\n%s\n```\n\n" output)
      steps
  end;
  Buffer.contents buf
