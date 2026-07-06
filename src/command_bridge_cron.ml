open Command_bridge_helpers

let cmd_cron args =
  match args with
  | "list" :: flags | ([] as flags) ->
      let show_prompt = List.mem "--prompt" flags || List.mem "-p" flags in
      let db = get_db () in
      Scheduler.init_schema db;
      let jobs = Scheduler.list_jobs ~db in
      if jobs = [] then
        "No cron jobs configured. Use 'clawq cron add' to create one."
      else
        let columns =
          let base =
            [
              Table_format.
                { header = "NAME"; align = Left; min_width = 4; flex = false };
              { header = "SESSION"; align = Left; min_width = 7; flex = false };
              { header = "SCHEDULE"; align = Left; min_width = 8; flex = false };
              { header = "ENABLED"; align = Left; min_width = 3; flex = false };
              { header = "EPH"; align = Left; min_width = 3; flex = false };
              { header = "EXPIRES"; align = Left; min_width = 3; flex = false };
            ]
          in
          if show_prompt then
            base
            @ [
                Table_format.
                  {
                    header = "PROMPT";
                    align = Left;
                    min_width = 10;
                    flex = true;
                  };
              ]
          else base
        in
        let rows =
          List.map
            (fun (j : Scheduler.job) ->
              let base =
                [
                  j.name;
                  (match Scheduler.job_routine_target j with
                  | Some target -> Printf.sprintf "%s (%s)" j.session_key target
                  | None -> j.session_key);
                  j.schedule_str;
                  (if j.enabled then "yes" else "no");
                  (if j.ephemeral then "yes" else "no");
                  (match j.expires_at with Some ea -> ea | None -> "-");
                ]
              in
              if show_prompt then base @ [ j.message ] else base)
            jobs
        in
        Format_adapter.bold Format_adapter.Plain "Cron Jobs"
        ^ "\n\n"
        ^ Format_adapter.render_table Format_adapter.Plain ~max_width:80 columns
            rows
  | [ "show"; name ] -> (
      let db = get_db () in
      Scheduler.init_schema db;
      match Scheduler.get_job ~db ~name with
      | None -> Printf.sprintf "No cron job found with name '%s'." name
      | Some (job : Scheduler.job) ->
          let connector = Format_adapter.Plain in
          let runs = Scheduler.get_history ~db ~name ~limit:5 in
          let doc =
            [
              Content_dsl.Paragraph
                [ Bold "Cron Job"; Text " — "; Code job.name ];
              Paragraph [ Text "Session: "; Code job.session_key ];
            ]
            @ (match Scheduler.job_routine_target job with
              | Some target ->
                  [
                    Content_dsl.Paragraph
                      [ Text "Routine target: "; Code target ];
                  ]
              | None -> [])
            @ [
                Content_dsl.Paragraph
                  [ Text "Schedule: "; Code job.schedule_str ];
                Paragraph
                  [
                    Text "Enabled: "; Text (if job.enabled then "yes" else "no");
                  ];
                Paragraph
                  [
                    Text "Ephemeral: ";
                    Text (if job.ephemeral then "yes" else "no");
                  ];
                Paragraph
                  [
                    Text "Expires: ";
                    Text
                      (match job.expires_at with
                      | Some ea -> ea
                      | None -> "never");
                  ];
              ]
            @ (match job.agent_name with
              | Some agent ->
                  [ Content_dsl.Paragraph [ Text "Agent: "; Code agent ] ]
              | None -> [])
            @ [
                Content_dsl.Separator;
                Paragraph [ Bold "Message" ];
                CodeBlock { language = None; content = job.message };
              ]
            @
            if runs = [] then
              [ Content_dsl.Paragraph [ Italic "No run history." ] ]
            else
              let show_target =
                List.exists
                  (fun (r : Scheduler.run) ->
                    Scheduler.run_routine_target r <> None)
                  runs
              in
              let history_columns =
                let base =
                  Table_format.
                    [
                      {
                        header = "ID";
                        align = Right;
                        min_width = 2;
                        flex = false;
                      };
                      {
                        header = "STARTED";
                        align = Left;
                        min_width = 19;
                        flex = false;
                      };
                      {
                        header = "STATUS";
                        align = Left;
                        min_width = 6;
                        flex = false;
                      };
                    ]
                in
                let target =
                  if show_target then
                    Table_format.
                      [
                        {
                          header = "TARGET";
                          align = Left;
                          min_width = 6;
                          flex = false;
                        };
                      ]
                  else []
                in
                base @ target
                @ Table_format.
                    [
                      {
                        header = "PREVIEW";
                        align = Left;
                        min_width = 10;
                        flex = true;
                      };
                    ]
              in
              let history_rows =
                List.map
                  (fun (r : Scheduler.run) ->
                    let preview =
                      match r.result_preview with
                      | Some p when String.length p > 40 ->
                          String.sub p 0 37 ^ "..."
                      | Some p -> p
                      | None -> ""
                    in
                    let base =
                      [ string_of_int r.run_id; r.started_at; r.status ]
                    in
                    let target =
                      if show_target then
                        [
                          (match Scheduler.run_routine_target r with
                          | Some target -> target
                          | None -> "-");
                        ]
                      else []
                    in
                    base @ target @ [ preview ])
                  runs
              in
              [
                Content_dsl.Separator;
                Paragraph [ Bold "Recent Runs" ];
                Paragraph
                  [
                    Text
                      (Format_adapter.render_table connector ~max_width:70
                         history_columns history_rows);
                  ];
              ]
          in
          Content_dsl.render_document connector doc)
  | "add" :: name :: session_key :: schedule :: message -> (
      let db = get_db () in
      Scheduler.init_schema db;
      let ephemeral = List.mem "--ephemeral" message in
      let message = List.filter (fun s -> s <> "--ephemeral") message in
      let rec extract_ttl acc = function
        | "--ttl" :: v :: rest -> (Some v, List.rev_append acc rest)
        | x :: rest -> extract_ttl (x :: acc) rest
        | [] -> (None, List.rev acc)
      in
      let ttl, message = extract_ttl [] message in
      let msg = String.concat " " message in
      match
        Scheduler.add_job ~db ~name ~session_key ~message:msg ~schedule
          ~ephemeral ?ttl ()
      with
      | Ok () -> Printf.sprintf "Added cron job '%s'" name
      | Error e -> Printf.sprintf "Error: %s" e)
  | [ "remove"; name ] ->
      let db = get_db () in
      Scheduler.init_schema db;
      if Scheduler.remove_job ~db ~name then
        Printf.sprintf "Removed job '%s'" name
      else Printf.sprintf "No job found with name '%s'" name
  | "history" :: name :: _ | "runs" :: name :: _ ->
      let db = get_db () in
      Scheduler.init_schema db;
      let runs = Scheduler.get_history ~db ~name ~limit:10 in
      if runs = [] then Printf.sprintf "No run history for '%s'" name
      else
        let show_target =
          List.exists
            (fun (r : Scheduler.run) -> Scheduler.run_routine_target r <> None)
            runs
        in
        let columns =
          let base =
            Table_format.
              [
                { header = "ID"; align = Right; min_width = 2; flex = false };
                {
                  header = "STARTED";
                  align = Left;
                  min_width = 19;
                  flex = false;
                };
                { header = "STATUS"; align = Left; min_width = 6; flex = false };
              ]
          in
          let target =
            if show_target then
              Table_format.
                [
                  {
                    header = "TARGET";
                    align = Left;
                    min_width = 6;
                    flex = false;
                  };
                ]
            else []
          in
          base @ target
          @ Table_format.
              [
                {
                  header = "PREVIEW";
                  align = Left;
                  min_width = 10;
                  flex = true;
                };
              ]
        in
        let rows =
          List.map
            (fun (r : Scheduler.run) ->
              let base = [ string_of_int r.run_id; r.started_at; r.status ] in
              let target =
                if show_target then
                  [
                    (match Scheduler.run_routine_target r with
                    | Some target -> target
                    | None -> "-");
                  ]
                else []
              in
              base @ target
              @ [ (match r.result_preview with Some p -> p | None -> "") ])
            runs
        in
        Format_adapter.bold Format_adapter.Plain
          (Printf.sprintf "Run History — %s" name)
        ^ "\n\n"
        ^ Format_adapter.render_table Format_adapter.Plain ~max_width:80 columns
            rows
  | [ "runs" ] ->
      let db = get_db () in
      Scheduler.init_schema db;
      let runs = Scheduler.list_runs ~db ~limit:20 () in
      if runs = [] then "No run history."
      else
        let show_target =
          List.exists
            (fun (r : Scheduler.run) -> Scheduler.run_routine_target r <> None)
            runs
        in
        let columns =
          let base =
            Table_format.
              [
                { header = "ID"; align = Right; min_width = 2; flex = false };
                { header = "JOB"; align = Left; min_width = 3; flex = false };
                {
                  header = "STARTED";
                  align = Left;
                  min_width = 19;
                  flex = false;
                };
                { header = "STATUS"; align = Left; min_width = 6; flex = false };
              ]
          in
          let target =
            if show_target then
              Table_format.
                [
                  {
                    header = "TARGET";
                    align = Left;
                    min_width = 6;
                    flex = false;
                  };
                ]
            else []
          in
          base @ target
          @ Table_format.
              [
                {
                  header = "PREVIEW";
                  align = Left;
                  min_width = 10;
                  flex = true;
                };
              ]
        in
        let rows =
          List.map
            (fun (r : Scheduler.run) ->
              let base =
                [ string_of_int r.run_id; r.job_name; r.started_at; r.status ]
              in
              let target =
                if show_target then
                  [
                    (match Scheduler.run_routine_target r with
                    | Some target -> target
                    | None -> "-");
                  ]
                else []
              in
              base @ target
              @ [ (match r.result_preview with Some p -> p | None -> "") ])
            runs
        in
        Format_adapter.bold Format_adapter.Plain "Run History"
        ^ "\n\n"
        ^ Format_adapter.render_table Format_adapter.Plain ~max_width:80 columns
            rows
  | [ "trigger"; name ] | [ "run"; name ] -> (
      let db = get_db () in
      Scheduler.init_schema db;
      Background_task.init_schema db;
      match Scheduler.trigger_job ~db ~name () with
      | Ok task_id ->
          Printf.sprintf
            "Triggered cron job '%s' — enqueued as background task %d.\n\
             Use 'clawq background show %d' to check progress."
            name task_id task_id
      | Error e -> Printf.sprintf "Error: %s" e)
  (* B587: explicit enable/disable so operators can pause a misbehaving cron
     without removing it (and losing its schedule + prompt). *)
  | [ "disable"; name ] -> (
      let db = get_db () in
      Scheduler.init_schema db;
      match Scheduler.get_job ~db ~name with
      | None -> Printf.sprintf "No cron job found with name '%s'." name
      | Some j when not j.enabled ->
          Printf.sprintf "Cron job '%s' is already disabled." name
      | Some _ -> (
          match Scheduler.toggle_job ~db ~name with
          | Ok () -> Printf.sprintf "Disabled cron job '%s'." name
          | Error e -> Printf.sprintf "Error: %s" e))
  | [ "enable"; name ] -> (
      let db = get_db () in
      Scheduler.init_schema db;
      match Scheduler.get_job ~db ~name with
      | None -> Printf.sprintf "No cron job found with name '%s'." name
      | Some j when j.enabled ->
          Printf.sprintf "Cron job '%s' is already enabled." name
      | Some _ -> (
          match Scheduler.toggle_job ~db ~name with
          | Ok () -> Printf.sprintf "Enabled cron job '%s'." name
          | Error e -> Printf.sprintf "Error: %s" e))
  | _ ->
      "Usage: clawq cron \
       <list|show|add|remove|enable|disable|trigger|history|runs>\n\
      \  cron list [--prompt|-p]                      - List all jobs \
       (--prompt shows prompt text)\n\
      \  cron show <name>                             - Show job details\n\
      \  cron add <name> <session> <schedule> <msg> [--ephemeral] [--ttl \
       <duration>] - Add a job\n\
      \  cron remove <name>                           - Remove a job\n\
      \  cron enable <name>                           - Enable a paused job\n\
      \  cron disable <name>                          - Pause job (keeps \
       schedule + prompt)\n\
      \  cron trigger <name>                          - Trigger a job \
       immediately\n\
      \  cron history <name>                          - Show run history\n\
      \  cron runs [name]                             - Show all run history\n\
       Schedule format: \"every 5m\" (interval) or standard 5-field cron (e.g. \
       \"0 9 * * 1-5\" for weekdays at 9am)\n\
       TTL duration: e.g. 24h, 7d, 30m (job auto-disables after this time)"
