(* Built-in structured pipeline definitions. *)

open Structured_pipeline_types

let builtin_research_report () =
  {
    name = "research-report";
    version = "1.0";
    description = "Multi-step research report: outline, research, draft, review";
    inputs =
      [
        ( "topic",
          {
            input_type = "string";
            description = "Research topic";
            required = true;
            default = None;
          } );
        ( "depth",
          {
            input_type = "string";
            description = "shallow|medium|deep";
            required = false;
            default = Some "medium";
          } );
      ];
    steps =
      [
        {
          name = "outline";
          kind =
            Prompt_step
              {
                prompt =
                  "Create a detailed outline for a research report on \
                   {{input.topic}} at {{input.depth}} depth.\n\
                   Return a JSON object with \"title\" (string) and \
                   \"sections\" (array of objects with \"heading\" (string) \
                   and \"points\" (array of strings)).";
                system_prompt = None;
                model = None;
                output_schema =
                  `Assoc
                    [
                      ("type", `String "object");
                      ( "properties",
                        `Assoc
                          [
                            ("title", `Assoc [ ("type", `String "string") ]);
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
                                                `Assoc
                                                  [ ("type", `String "string") ]
                                              );
                                              ( "points",
                                                `Assoc
                                                  [
                                                    ("type", `String "array");
                                                    ( "items",
                                                      `Assoc
                                                        [
                                                          ( "type",
                                                            `String "string" );
                                                        ] );
                                                  ] );
                                            ] );
                                        ( "required",
                                          `List
                                            [
                                              `String "heading";
                                              `String "points";
                                            ] );
                                      ] );
                                ] );
                          ] );
                      ("required", `List [ `String "title"; `String "sections" ]);
                    ];
                max_retries = 2;
              };
        };
        {
          name = "draft";
          kind =
            Prompt_step
              {
                prompt =
                  "Write a research report based on this outline:\n\
                   {{outline}}\n\n\
                   The topic is: {{input.topic}}\n\
                   Return a JSON object with \"report\" (string, the full \
                   report text) and \"word_count\" (integer).";
                system_prompt = None;
                model = None;
                output_schema =
                  `Assoc
                    [
                      ("type", `String "object");
                      ( "properties",
                        `Assoc
                          [
                            ("report", `Assoc [ ("type", `String "string") ]);
                            ( "word_count",
                              `Assoc [ ("type", `String "integer") ] );
                          ] );
                      ("required", `List [ `String "report" ]);
                    ];
                max_retries = 1;
              };
        };
        {
          name = "review";
          kind =
            Prompt_step
              {
                prompt =
                  "Review this research report draft and provide feedback:\n\
                   {{draft.report}}\n\n\
                   Return a JSON object with \"quality_score\" (integer 1-10), \
                   \"strengths\" (array of strings), \"weaknesses\" (array of \
                   strings), and \"suggestions\" (array of strings).";
                system_prompt = None;
                model = None;
                output_schema =
                  `Assoc
                    [
                      ("type", `String "object");
                      ( "properties",
                        `Assoc
                          [
                            ( "quality_score",
                              `Assoc
                                [
                                  ("type", `String "integer");
                                  ("minimum", `Int 1);
                                  ("maximum", `Int 10);
                                ] );
                            ( "strengths",
                              `Assoc
                                [
                                  ("type", `String "array");
                                  ( "items",
                                    `Assoc [ ("type", `String "string") ] );
                                ] );
                            ( "weaknesses",
                              `Assoc
                                [
                                  ("type", `String "array");
                                  ( "items",
                                    `Assoc [ ("type", `String "string") ] );
                                ] );
                            ( "suggestions",
                              `Assoc
                                [
                                  ("type", `String "array");
                                  ( "items",
                                    `Assoc [ ("type", `String "string") ] );
                                ] );
                          ] );
                      ( "required",
                        `List [ `String "quality_score"; `String "strengths" ]
                      );
                    ];
                max_retries = 1;
              };
        };
      ];
    source_path = "(builtin)";
  }

let builtin_build_review_carm () =
  {
    name = "build-review-carm";
    version = "1.0";
    description =
      "Build (implement), review-and-fix, then commit all and rebase master";
    inputs =
      [
        ( "task",
          {
            input_type = "string";
            description = "Description of what to implement";
            required = true;
            default = None;
          } );
      ];
    steps =
      [
        {
          name = "build";
          kind =
            Agent_step
              {
                task =
                  "Implement the following task:\n\n\
                   {{input.task}}\n\n\
                   Work until the implementation is complete. Make sure the \
                   code compiles and basic tests pass.";
                model = None;
                max_turns = None;
              };
        };
        {
          name = "review";
          kind =
            Agent_step
              {
                task =
                  "Load and run the /review-and-fix skill on the current work. \
                   Review the changes against the original task, fix any \
                   issues found, and re-review until it passes.";
                model = None;
                max_turns = None;
              };
        };
        {
          name = "carm";
          kind =
            Agent_step
              {
                task =
                  "Load and run the /carm skill to commit all changes and \
                   rebase master.";
                model = None;
                max_turns = None;
              };
        };
      ];
    source_path = "(builtin)";
  }

let builtin_plan_build_review_carm () =
  {
    name = "plan-build-review-carm";
    version = "1.0";
    description =
      "Plan, build (implement), review-and-fix, then commit all and rebase \
       master";
    inputs =
      [
        ( "task",
          {
            input_type = "string";
            description = "Description of what to plan and implement";
            required = true;
            default = None;
          } );
      ];
    steps =
      [
        {
          name = "plan";
          kind =
            Agent_step
              {
                task =
                  "Create a detailed implementation plan for the following \
                   task:\n\n\
                   {{input.task}}\n\n\
                   Read the relevant code, understand the architecture, and \
                   produce a step-by-step plan. Use the /super-plan skill if \
                   appropriate.";
                model = None;
                max_turns = None;
              };
        };
        {
          name = "build";
          kind =
            Agent_step
              {
                task =
                  "Implement the plan from the previous step. The original \
                   task was:\n\n\
                   {{input.task}}\n\n\
                   Work until the implementation is complete. Make sure the \
                   code compiles and basic tests pass.";
                model = None;
                max_turns = None;
              };
        };
        {
          name = "review";
          kind =
            Agent_step
              {
                task =
                  "Load and run the /review-and-fix skill on the current work. \
                   Review the changes against the original task, fix any \
                   issues found, and re-review until it passes.";
                model = None;
                max_turns = None;
              };
        };
        {
          name = "carm";
          kind =
            Agent_step
              {
                task =
                  "Load and run the /carm skill to commit all changes and \
                   rebase master.";
                model = None;
                max_turns = None;
              };
        };
      ];
    source_path = "(builtin)";
  }

let all =
  [
    builtin_research_report;
    builtin_build_review_carm;
    builtin_plan_build_review_carm;
  ]
