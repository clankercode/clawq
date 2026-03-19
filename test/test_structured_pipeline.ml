(* test_structured_pipeline.ml — Tests for structured output pipelines *)

let mk_db () =
  let db = Sqlite3.db_open ":memory:" in
  Structured_pipeline.init_schema db;
  db

(* ── Schema validation tests ───────────────────────────────────────────── *)

let test_schema_validate_string_type () =
  let schema = `Assoc [ ("type", `String "string") ] in
  Alcotest.(check bool)
    "string matches" true
    (Structured_pipeline_schema.validate ~schema ~value:(`String "hello")
    = Ok ());
  Alcotest.(check bool)
    "int fails" true
    (Result.is_error
       (Structured_pipeline_schema.validate ~schema ~value:(`Int 42)))

let test_schema_validate_integer_type () =
  let schema = `Assoc [ ("type", `String "integer") ] in
  Alcotest.(check bool)
    "int matches" true
    (Structured_pipeline_schema.validate ~schema ~value:(`Int 42) = Ok ());
  Alcotest.(check bool)
    "float integer matches" true
    (Structured_pipeline_schema.validate ~schema ~value:(`Float 42.0) = Ok ());
  Alcotest.(check bool)
    "float non-integer fails" true
    (Result.is_error
       (Structured_pipeline_schema.validate ~schema ~value:(`Float 42.5)))

let test_schema_validate_object_required () =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ("name", `Assoc [ ("type", `String "string") ]);
              ("age", `Assoc [ ("type", `String "integer") ]);
            ] );
        ("required", `List [ `String "name" ]);
      ]
  in
  Alcotest.(check bool)
    "valid object" true
    (Structured_pipeline_schema.validate ~schema
       ~value:(`Assoc [ ("name", `String "Alice"); ("age", `Int 30) ])
    = Ok ());
  let result =
    Structured_pipeline_schema.validate ~schema
      ~value:(`Assoc [ ("age", `Int 30) ])
  in
  Alcotest.(check bool) "missing required field" true (Result.is_error result)

let test_schema_validate_array () =
  let schema =
    `Assoc
      [
        ("type", `String "array");
        ("items", `Assoc [ ("type", `String "string") ]);
        ("minItems", `Int 1);
      ]
  in
  Alcotest.(check bool)
    "valid array" true
    (Structured_pipeline_schema.validate ~schema
       ~value:(`List [ `String "a"; `String "b" ])
    = Ok ());
  let result = Structured_pipeline_schema.validate ~schema ~value:(`List []) in
  Alcotest.(check bool)
    "empty array fails minItems" true (Result.is_error result)

let test_schema_validate_enum () =
  let schema =
    `Assoc
      [
        ("type", `String "string");
        ("enum", `List [ `String "a"; `String "b"; `String "c" ]);
      ]
  in
  Alcotest.(check bool)
    "enum match" true
    (Structured_pipeline_schema.validate ~schema ~value:(`String "b") = Ok ());
  Alcotest.(check bool)
    "enum mismatch" true
    (Result.is_error
       (Structured_pipeline_schema.validate ~schema ~value:(`String "d")))

let test_schema_validate_min_max_length () =
  let schema =
    `Assoc
      [
        ("type", `String "string"); ("minLength", `Int 2); ("maxLength", `Int 5);
      ]
  in
  Alcotest.(check bool)
    "valid length" true
    (Structured_pipeline_schema.validate ~schema ~value:(`String "abc") = Ok ());
  Alcotest.(check bool)
    "too short" true
    (Result.is_error
       (Structured_pipeline_schema.validate ~schema ~value:(`String "a")));
  Alcotest.(check bool)
    "too long" true
    (Result.is_error
       (Structured_pipeline_schema.validate ~schema ~value:(`String "abcdef")))

let test_schema_validate_numeric_range () =
  let schema =
    `Assoc
      [ ("type", `String "integer"); ("minimum", `Int 1); ("maximum", `Int 10) ]
  in
  Alcotest.(check bool)
    "in range" true
    (Structured_pipeline_schema.validate ~schema ~value:(`Int 5) = Ok ());
  Alcotest.(check bool)
    "below minimum" true
    (Result.is_error
       (Structured_pipeline_schema.validate ~schema ~value:(`Int 0)));
  Alcotest.(check bool)
    "above maximum" true
    (Result.is_error
       (Structured_pipeline_schema.validate ~schema ~value:(`Int 11)))

let test_schema_validate_nested () =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "sections",
                `Assoc
                  [
                    ("type", `String "array");
                    ( "items",
                      `Assoc
                        [
                          ("type", `String "object");
                          ( "properties",
                            `Assoc
                              [
                                ( "heading",
                                  `Assoc [ ("type", `String "string") ] );
                              ] );
                          ("required", `List [ `String "heading" ]);
                        ] );
                  ] );
            ] );
        ("required", `List [ `String "sections" ]);
      ]
  in
  let valid =
    `Assoc
      [
        ( "sections",
          `List
            [
              `Assoc [ ("heading", `String "Intro") ];
              `Assoc [ ("heading", `String "Body") ];
            ] );
      ]
  in
  Alcotest.(check bool)
    "nested valid" true
    (Structured_pipeline_schema.validate ~schema ~value:valid = Ok ());
  let invalid =
    `Assoc
      [ ("sections", `List [ `Assoc [ ("not_heading", `String "oops") ] ]) ]
  in
  Alcotest.(check bool)
    "nested invalid" true
    (Result.is_error
       (Structured_pipeline_schema.validate ~schema ~value:invalid))

let test_schema_additional_properties () =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [ ("name", `Assoc [ ("type", `String "string") ]) ] );
        ("additionalProperties", `Bool false);
      ]
  in
  Alcotest.(check bool)
    "no extra props" true
    (Structured_pipeline_schema.validate ~schema
       ~value:(`Assoc [ ("name", `String "ok") ])
    = Ok ());
  Alcotest.(check bool)
    "extra prop fails" true
    (Result.is_error
       (Structured_pipeline_schema.validate ~schema
          ~value:(`Assoc [ ("name", `String "ok"); ("extra", `Int 1) ])))

let test_schema_validate_itself () =
  let valid =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [ ("name", `Assoc [ ("type", `String "string") ]) ] );
      ]
  in
  Alcotest.(check bool)
    "valid schema" true
    (Structured_pipeline_schema.validate_schema_itself valid = Ok ());
  let invalid = `Assoc [ ("type", `String "unicorn") ] in
  Alcotest.(check bool)
    "invalid type in schema" true
    (Result.is_error
       (Structured_pipeline_schema.validate_schema_itself invalid))

let test_schema_summary () =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ("title", `Assoc [ ("type", `String "string") ]);
              ("count", `Assoc [ ("type", `String "integer") ]);
            ] );
        ("required", `List [ `String "title" ]);
      ]
  in
  let summary = Structured_pipeline_schema.schema_summary schema in
  Alcotest.(check bool)
    "has object" true
    (String_util.contains summary "object");
  Alcotest.(check bool) "has title" true (String_util.contains summary "title");
  Alcotest.(check bool)
    "has required" true
    (String_util.contains summary "required")

(* ── Pipeline parsing tests ────────────────────────────────────────────── *)

let test_parse_pipeline_json () =
  let json =
    `Assoc
      [
        ("name", `String "test-pipe");
        ("version", `String "1.0");
        ("description", `String "A test pipeline");
        ( "inputs",
          `Assoc
            [
              ( "topic",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "Main topic");
                    ("required", `Bool true);
                  ] );
            ] );
        ( "steps",
          `List
            [
              `Assoc
                [
                  ("name", `String "step1");
                  ("prompt", `String "Do thing with {{input.topic}}");
                  ( "output_schema",
                    `Assoc
                      [
                        ("type", `String "object");
                        ( "properties",
                          `Assoc
                            [
                              ("result", `Assoc [ ("type", `String "string") ]);
                            ] );
                      ] );
                  ("max_retries", `Int 2);
                ];
            ] );
      ]
  in
  match Structured_pipeline.parse_pipeline_def json with
  | Error msg -> Alcotest.failf "parse failed: %s" msg
  | Ok def -> (
      Alcotest.(check string) "name" "test-pipe" def.name;
      Alcotest.(check string) "version" "1.0" def.version;
      Alcotest.(check int) "inputs count" 1 (List.length def.inputs);
      Alcotest.(check int) "steps count" 1 (List.length def.steps);
      let step = List.hd def.steps in
      Alcotest.(check string) "step name" "step1" step.name;
      match step.kind with
      | Structured_pipeline.Prompt_step { max_retries; _ } ->
          Alcotest.(check int) "max_retries" 2 max_retries
      | _ -> Alcotest.fail "expected Prompt_step")

let test_parse_pipeline_step () =
  let json =
    `Assoc
      [
        ("name", `String "composed");
        ("version", `String "1.0");
        ("description", `String "test");
        ("inputs", `Assoc []);
        ( "steps",
          `List
            [
              `Assoc
                [
                  ("name", `String "sub");
                  ("pipeline", `String "research-report");
                  ("input_map", `Assoc [ ("topic", `String "{{input.topic}}") ]);
                ];
            ] );
      ]
  in
  match Structured_pipeline.parse_pipeline_def json with
  | Error msg -> Alcotest.failf "parse failed: %s" msg
  | Ok def -> (
      Alcotest.(check int) "1 step" 1 (List.length def.steps);
      let step = List.hd def.steps in
      match step.kind with
      | Structured_pipeline.Pipeline_step { pipeline; input_map } ->
          Alcotest.(check string) "pipeline" "research-report" pipeline;
          Alcotest.(check int) "input_map count" 1 (List.length input_map)
      | _ -> Alcotest.fail "expected Pipeline_step")

let test_parse_agent_step () =
  let json =
    `Assoc
      [
        ("name", `String "agent-test");
        ("version", `String "1.0");
        ("description", `String "test agent step");
        ("inputs", `Assoc [ ("task", `Assoc [ ("type", `String "string") ]) ]);
        ( "steps",
          `List
            [
              `Assoc
                [
                  ("name", `String "build");
                  ("task", `String "Implement {{input.task}}");
                  ("max_turns", `Int 50);
                ];
            ] );
      ]
  in
  match Structured_pipeline.parse_pipeline_def json with
  | Error msg -> Alcotest.failf "parse failed: %s" msg
  | Ok def -> (
      Alcotest.(check int) "1 step" 1 (List.length def.steps);
      let step = List.hd def.steps in
      Alcotest.(check string) "step name" "build" step.name;
      match step.kind with
      | Structured_pipeline.Agent_step { task; model; max_turns } ->
          Alcotest.(check string) "task" "Implement {{input.task}}" task;
          Alcotest.(check bool) "no model" true (model = None);
          Alcotest.(check (option int)) "max_turns" (Some 50) max_turns
      | _ -> Alcotest.fail "expected Agent_step")

let test_parse_invalid_pipeline () =
  let json = `String "not an object" in
  Alcotest.(check bool)
    "parse fails" true
    (Result.is_error (Structured_pipeline.parse_pipeline_def json));
  let no_name = `Assoc [ ("description", `String "missing name") ] in
  Alcotest.(check bool)
    "no name fails" true
    (Result.is_error (Structured_pipeline.parse_pipeline_def no_name))

(* ── Template substitution tests ───────────────────────────────────────── *)

let test_template_input_substitution () =
  let result =
    Structured_pipeline.substitute_template
      "Topic: {{input.topic}}, depth: {{input.depth}}"
      ~inputs:[ ("topic", "AI Safety"); ("depth", "deep") ]
      ~step_outputs:[]
  in
  Alcotest.(check string)
    "inputs substituted" "Topic: AI Safety, depth: deep" result

let test_template_step_output_substitution () =
  let step_json = `Assoc [ ("title", `String "My Title"); ("count", `Int 5) ] in
  let result =
    Structured_pipeline.substitute_template
      "Title: {{outline.title}}, full: {{outline}}" ~inputs:[]
      ~step_outputs:[ ("outline", step_json) ]
  in
  Alcotest.(check bool)
    "has title" true
    (String_util.contains result "My Title");
  Alcotest.(check bool)
    "has full json" true
    (String_util.contains result "\"title\"")

let test_template_missing_var () =
  let result =
    Structured_pipeline.substitute_template "Hello {{input.missing}}" ~inputs:[]
      ~step_outputs:[]
  in
  Alcotest.(check string)
    "missing var preserved" "Hello {{input.missing}}" result

(* ── JSON extraction tests ─────────────────────────────────────────────── *)

let test_extract_json_pure () =
  let result =
    Structured_pipeline_run.extract_json_from_response {|{"result": "hello"}|}
  in
  match result with
  | Ok json ->
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "result" "hello"
        (json |> member "result" |> to_string)
  | Error msg -> Alcotest.failf "extraction failed: %s" msg

let test_extract_json_code_fenced () =
  let result =
    Structured_pipeline_run.extract_json_from_response
      "Here is the result:\n```json\n{\"result\": \"hello\"}\n```\nDone!"
  in
  match result with
  | Ok json ->
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "result" "hello"
        (json |> member "result" |> to_string)
  | Error msg -> Alcotest.failf "extraction failed: %s" msg

let test_extract_json_embedded () =
  let result =
    Structured_pipeline_run.extract_json_from_response
      "Some preamble text {\"key\": \"value\"} and trailing text"
  in
  match result with
  | Ok json ->
      let open Yojson.Safe.Util in
      Alcotest.(check string) "key" "value" (json |> member "key" |> to_string)
  | Error msg -> Alcotest.failf "extraction failed: %s" msg

let test_extract_json_no_json () =
  let result =
    Structured_pipeline_run.extract_json_from_response "Just plain text"
  in
  Alcotest.(check bool) "no json" true (Result.is_error result)

(* ── Pipeline validation tests ─────────────────────────────────────────── *)

let test_validate_valid_pipeline () =
  let def = Structured_pipeline.builtin_research_report () in
  match Structured_pipeline.validate_pipeline_def def with
  | Ok () -> ()
  | Error errs ->
      Alcotest.failf "builtin should be valid: %s" (String.concat "; " errs)

let test_validate_duplicate_step_names () =
  let def : Structured_pipeline.pipeline_def =
    {
      name = "dup-test";
      version = "1.0";
      description = "test";
      inputs = [];
      steps =
        [
          {
            name = "step1";
            kind =
              Prompt_step
                {
                  prompt = "do thing";
                  system_prompt = None;
                  model = None;
                  output_schema = `Assoc [ ("type", `String "object") ];
                  max_retries = 0;
                };
          };
          {
            name = "step1";
            kind =
              Prompt_step
                {
                  prompt = "do other thing";
                  system_prompt = None;
                  model = None;
                  output_schema = `Assoc [ ("type", `String "object") ];
                  max_retries = 0;
                };
          };
        ];
      source_path = "";
    }
  in
  match Structured_pipeline.validate_pipeline_def def with
  | Ok () -> Alcotest.fail "should have failed for duplicate names"
  | Error errs ->
      Alcotest.(check bool)
        "has dup error" true
        (List.exists (fun e -> String_util.contains e "duplicate") errs)

let test_validate_empty_steps () =
  let def : Structured_pipeline.pipeline_def =
    {
      name = "empty";
      version = "1.0";
      description = "test";
      inputs = [];
      steps = [];
      source_path = "";
    }
  in
  Alcotest.(check bool)
    "empty steps invalid" true
    (Result.is_error (Structured_pipeline.validate_pipeline_def def))

(* ── DB tests ──────────────────────────────────────────────────────────── *)

let test_db_init_and_insert () =
  let db = mk_db () in
  let run_id =
    Structured_pipeline.insert_run ~db ~pipeline_name:"test-pipe"
      ~pipeline_version:"1.0"
      ~inputs:[ ("topic", "AI") ]
  in
  Alcotest.(check bool) "run_id > 0" true (run_id > 0);
  let runs = Structured_pipeline.list_runs ~db ~limit:10 () in
  Alcotest.(check int) "1 run" 1 (List.length runs);
  let run = List.hd runs in
  Alcotest.(check string) "pipeline name" "test-pipe" run.pipeline_name;
  Alcotest.(check string) "status" "running" run.status

let test_db_update_status () =
  let db = mk_db () in
  let run_id =
    Structured_pipeline.insert_run ~db ~pipeline_name:"test"
      ~pipeline_version:"1.0" ~inputs:[]
  in
  Structured_pipeline.update_run_status ~db ~run_id
    ~status:Structured_pipeline.Completed ~elapsed_s:5.0 ();
  let runs = Structured_pipeline.list_runs ~db ~limit:10 () in
  let run = List.hd runs in
  Alcotest.(check string) "completed" "completed" run.status;
  Alcotest.(check bool)
    "elapsed" true
    (match run.total_elapsed_s with Some e -> e > 4.0 | None -> false)

let test_db_step_results () =
  let db = mk_db () in
  let run_id =
    Structured_pipeline.insert_run ~db ~pipeline_name:"test"
      ~pipeline_version:"1.0" ~inputs:[]
  in
  let sr : Structured_pipeline.step_result =
    {
      step_name = "step1";
      output_json = `Assoc [ ("key", `String "val") ];
      output_raw = "{\"key\": \"val\"}";
      model_used = "test-model";
      attempts = 2;
      elapsed_s = 1.5;
      tokens = Some (100, 50);
    }
  in
  Structured_pipeline.add_step_result ~db ~run_id ~step_index:0 ~result:sr;
  let steps = Structured_pipeline.get_run_steps ~db ~run_id in
  Alcotest.(check int) "1 step" 1 (List.length steps);
  let step = List.hd steps in
  Alcotest.(check string) "step name" "step1" step.step_name;
  Alcotest.(check int) "attempts" 2 step.attempts;
  Alcotest.(check string) "model" "test-model" step.model_used

let test_db_list_by_pipeline () =
  let db = mk_db () in
  ignore
    (Structured_pipeline.insert_run ~db ~pipeline_name:"pipe-a"
       ~pipeline_version:"1.0" ~inputs:[]);
  ignore
    (Structured_pipeline.insert_run ~db ~pipeline_name:"pipe-b"
       ~pipeline_version:"1.0" ~inputs:[]);
  ignore
    (Structured_pipeline.insert_run ~db ~pipeline_name:"pipe-a"
       ~pipeline_version:"1.0" ~inputs:[]);
  let all = Structured_pipeline.list_runs ~db ~limit:10 () in
  Alcotest.(check int) "3 total" 3 (List.length all);
  let pipe_a =
    Structured_pipeline.list_runs ~db ~pipeline_name:"pipe-a" ~limit:10 ()
  in
  Alcotest.(check int) "2 for pipe-a" 2 (List.length pipe_a)

(* ── Pipeline name validation ──────────────────────────────────────────── *)

let test_valid_pipeline_names () =
  Alcotest.(check bool)
    "simple" true
    (Structured_pipeline.is_valid_pipeline_name "test");
  Alcotest.(check bool)
    "hyphen" true
    (Structured_pipeline.is_valid_pipeline_name "my-pipe");
  Alcotest.(check bool)
    "underscore" true
    (Structured_pipeline.is_valid_pipeline_name "my_pipe");
  Alcotest.(check bool)
    "empty" false
    (Structured_pipeline.is_valid_pipeline_name "");
  Alcotest.(check bool)
    "space" false
    (Structured_pipeline.is_valid_pipeline_name "my pipe");
  Alcotest.(check bool)
    "dot" false
    (Structured_pipeline.is_valid_pipeline_name "my.pipe")

(* ── YAML to pipeline round-trip ───────────────────────────────────────── *)

let test_pipeline_to_yaml () =
  let def = Structured_pipeline.builtin_research_report () in
  let yaml = Structured_pipeline.pipeline_def_to_yaml def in
  Alcotest.(check bool)
    "has name" true
    (String_util.contains yaml "name: research-report");
  Alcotest.(check bool) "has steps" true (String_util.contains yaml "steps:");
  Alcotest.(check bool) "has outline" true (String_util.contains yaml "outline")

(* ── Builtin pipeline ─────────────────────────────────────────────────── *)

let test_builtin_research_report () =
  let def = Structured_pipeline.builtin_research_report () in
  Alcotest.(check string) "name" "research-report" def.name;
  Alcotest.(check int) "3 steps" 3 (List.length def.steps);
  Alcotest.(check int) "2 inputs" 2 (List.length def.inputs);
  let _, topic_def = List.hd def.inputs in
  Alcotest.(check bool) "topic required" true topic_def.required

let test_builtin_build_review_carm () =
  let def = Structured_pipeline.builtin_build_review_carm () in
  Alcotest.(check string) "name" "build-review-carm" def.name;
  Alcotest.(check int) "3 steps" 3 (List.length def.steps);
  Alcotest.(check int) "1 input" 1 (List.length def.inputs);
  List.iter
    (fun (s : Structured_pipeline.step) ->
      match s.kind with
      | Structured_pipeline.Agent_step _ -> ()
      | _ -> Alcotest.failf "step %s should be Agent_step" s.name)
    def.steps;
  match Structured_pipeline.validate_pipeline_def def with
  | Ok () -> ()
  | Error errs -> Alcotest.failf "should be valid: %s" (String.concat "; " errs)

let test_builtin_plan_build_review_carm () =
  let def = Structured_pipeline.builtin_plan_build_review_carm () in
  Alcotest.(check string) "name" "plan-build-review-carm" def.name;
  Alcotest.(check int) "4 steps" 4 (List.length def.steps);
  Alcotest.(check int) "1 input" 1 (List.length def.inputs);
  List.iter
    (fun (s : Structured_pipeline.step) ->
      match s.kind with
      | Structured_pipeline.Agent_step _ -> ()
      | _ -> Alcotest.failf "step %s should be Agent_step" s.name)
    def.steps;
  match Structured_pipeline.validate_pipeline_def def with
  | Ok () -> ()
  | Error errs -> Alcotest.failf "should be valid: %s" (String.concat "; " errs)

(* ── Discovery ─────────────────────────────────────────────────────────── *)

let test_discover_includes_builtins () =
  let pipelines = Structured_pipeline.discover_pipelines () in
  List.iter
    (fun name ->
      let found =
        List.exists
          (fun (p : Structured_pipeline.pipeline_def) -> p.name = name)
          pipelines
      in
      Alcotest.(check bool) (name ^ " found") true found)
    [ "research-report"; "build-review-carm"; "plan-build-review-carm" ]

(* ── Format helpers ────────────────────────────────────────────────────── *)

let test_format_pipeline_list () =
  let pipelines = Structured_pipeline.discover_pipelines () in
  let output = Structured_pipeline.format_pipeline_list pipelines in
  Alcotest.(check bool)
    "has Pipelines" true
    (String_util.contains output "Pipelines:");
  Alcotest.(check bool)
    "has research-report" true
    (String_util.contains output "research-report")

let test_format_empty_list () =
  let output = Structured_pipeline.format_pipeline_list [] in
  Alcotest.(check string) "no pipelines" "No pipelines found." output

let test_format_run_list () =
  let db = mk_db () in
  ignore
    (Structured_pipeline.insert_run ~db ~pipeline_name:"test"
       ~pipeline_version:"1.0" ~inputs:[]);
  let runs = Structured_pipeline.list_runs ~db ~limit:10 () in
  let output = Structured_pipeline.format_run_list runs in
  Alcotest.(check bool)
    "has runs" true
    (String_util.contains output "Pipeline runs:")

let test_format_run_detail () =
  let db = mk_db () in
  let run_id =
    Structured_pipeline.insert_run ~db ~pipeline_name:"test"
      ~pipeline_version:"1.0"
      ~inputs:[ ("topic", "AI") ]
  in
  let sr : Structured_pipeline.step_result =
    {
      step_name = "step1";
      output_json = `Assoc [ ("key", `String "val") ];
      output_raw = "{\"key\": \"val\"}";
      model_used = "test-model";
      attempts = 1;
      elapsed_s = 1.0;
      tokens = None;
    }
  in
  Structured_pipeline.add_step_result ~db ~run_id ~step_index:0 ~result:sr;
  Structured_pipeline.update_run_status ~db ~run_id
    ~status:Structured_pipeline.Completed ~elapsed_s:1.0 ();
  let runs = Structured_pipeline.list_runs ~db ~limit:10 () in
  let run = List.hd runs in
  let steps = Structured_pipeline.get_run_steps ~db ~run_id in
  let output = Structured_pipeline.format_run_detail ~run ~steps in
  Alcotest.(check bool)
    "has title" true
    (String_util.contains output "Pipeline Run #");
  Alcotest.(check bool) "has step" true (String_util.contains output "step1")

(* ── Suite ─────────────────────────────────────────────────────────────── *)

let suite =
  [
    (* Schema validation *)
    Alcotest.test_case "schema: string type" `Quick
      test_schema_validate_string_type;
    Alcotest.test_case "schema: integer type" `Quick
      test_schema_validate_integer_type;
    Alcotest.test_case "schema: object required" `Quick
      test_schema_validate_object_required;
    Alcotest.test_case "schema: array" `Quick test_schema_validate_array;
    Alcotest.test_case "schema: enum" `Quick test_schema_validate_enum;
    Alcotest.test_case "schema: min/maxLength" `Quick
      test_schema_validate_min_max_length;
    Alcotest.test_case "schema: numeric range" `Quick
      test_schema_validate_numeric_range;
    Alcotest.test_case "schema: nested" `Quick test_schema_validate_nested;
    Alcotest.test_case "schema: additionalProperties" `Quick
      test_schema_additional_properties;
    Alcotest.test_case "schema: validate_schema_itself" `Quick
      test_schema_validate_itself;
    Alcotest.test_case "schema: summary" `Quick test_schema_summary;
    (* Pipeline parsing *)
    Alcotest.test_case "parse: valid JSON pipeline" `Quick
      test_parse_pipeline_json;
    Alcotest.test_case "parse: pipeline step ref" `Quick
      test_parse_pipeline_step;
    Alcotest.test_case "parse: agent step" `Quick test_parse_agent_step;
    Alcotest.test_case "parse: invalid pipeline" `Quick
      test_parse_invalid_pipeline;
    (* Template substitution *)
    Alcotest.test_case "template: input substitution" `Quick
      test_template_input_substitution;
    Alcotest.test_case "template: step output" `Quick
      test_template_step_output_substitution;
    Alcotest.test_case "template: missing var" `Quick test_template_missing_var;
    (* JSON extraction *)
    Alcotest.test_case "extract: pure JSON" `Quick test_extract_json_pure;
    Alcotest.test_case "extract: code fenced" `Quick
      test_extract_json_code_fenced;
    Alcotest.test_case "extract: embedded" `Quick test_extract_json_embedded;
    Alcotest.test_case "extract: no JSON" `Quick test_extract_json_no_json;
    (* Validation *)
    Alcotest.test_case "validate: builtin valid" `Quick
      test_validate_valid_pipeline;
    Alcotest.test_case "validate: duplicate steps" `Quick
      test_validate_duplicate_step_names;
    Alcotest.test_case "validate: empty steps" `Quick test_validate_empty_steps;
    (* DB *)
    Alcotest.test_case "db: init and insert" `Quick test_db_init_and_insert;
    Alcotest.test_case "db: update status" `Quick test_db_update_status;
    Alcotest.test_case "db: step results" `Quick test_db_step_results;
    Alcotest.test_case "db: list by pipeline" `Quick test_db_list_by_pipeline;
    (* Name validation *)
    Alcotest.test_case "name: validation" `Quick test_valid_pipeline_names;
    (* YAML output *)
    Alcotest.test_case "yaml: pipeline to yaml" `Quick test_pipeline_to_yaml;
    (* Builtins *)
    Alcotest.test_case "builtin: research-report" `Quick
      test_builtin_research_report;
    Alcotest.test_case "builtin: build-review-carm" `Quick
      test_builtin_build_review_carm;
    Alcotest.test_case "builtin: plan-build-review-carm" `Quick
      test_builtin_plan_build_review_carm;
    (* Discovery *)
    Alcotest.test_case "discover: includes builtins" `Quick
      test_discover_includes_builtins;
    (* Format *)
    Alcotest.test_case "format: pipeline list" `Quick test_format_pipeline_list;
    Alcotest.test_case "format: empty list" `Quick test_format_empty_list;
    Alcotest.test_case "format: run list" `Quick test_format_run_list;
    Alcotest.test_case "format: run detail" `Quick test_format_run_detail;
  ]
