let bg_shell_status () =
  {
    Tool.name = "bg_shell_status";
    description =
      "Check the status of a background shell job. Returns current status, \
       elapsed time, and last 20 lines of output log.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "id",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "Background shell job ID");
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        match Bg_shell.find id with
        | None ->
            Lwt.return
              (Printf.sprintf
                 "Error: no background shell job with id=%d. Use \
                  bg_shell_status to list active jobs, or check that the job \
                  ID is correct."
                 id)
        | Some job ->
            let status = Bg_shell.status_string job in
            let tail = Bg_shell.tail_log job ~lines:20 in
            let result =
              Printf.sprintf
                "Background shell job #%d\n\
                 Command: %s\n\
                 Status: %s\n\n\
                 Last 20 lines of output:\n\
                 %s"
                job.id job.command status tail
            in
            Lwt.return result);
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let bg_shell_wait () =
  {
    Tool.name = "bg_shell_wait";
    description =
      "Wait for a background shell job to complete. Returns the job result \
       when done, or a timeout/interrupt message if the wait is cut short.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "id",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "Background shell job ID");
                    ] );
                ( "timeout_seconds",
                  `Assoc
                    [
                      ("type", `String "number");
                      ( "description",
                        `String "Maximum seconds to wait (default 110, max 110)"
                      );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let timeout_seconds =
          try
            let v = args |> member "timeout_seconds" |> to_float in
            Float.min v 110.0
          with _ -> 110.0
        in
        let interrupt_check =
          match context with Some c -> c.Tool.interrupt_check | None -> None
        in
        match Bg_shell.find id with
        | None ->
            Lwt.return
              (Printf.sprintf
                 "Error: no background shell job with id=%d. Check the job ID \
                  is correct."
                 id)
        | Some _job -> (
            let open Lwt.Syntax in
            let* result =
              Bg_shell.wait_job ~id ~timeout_seconds ?interrupt_check ()
            in
            match result with
            | Bg_shell.Done job ->
                let tail = Bg_shell.tail_log job ~lines:50 in
                Lwt.return
                  (Printf.sprintf
                     "Background shell job #%d completed.\n\
                      Command: %s\n\
                      Status: %s\n\n\
                      Output (last 50 lines):\n\
                      %s\n\n\
                      For full output, use bg_shell_result with id=%d"
                     job.id job.command
                     (Bg_shell.status_string job)
                     tail job.id)
            | Bg_shell.Timeout ->
                Lwt.return
                  (Printf.sprintf
                     "Timed out waiting for background shell job #%d after \
                      %.0fs. The job is still running. Call bg_shell_wait \
                      again to continue waiting, or bg_shell_status to check \
                      progress."
                     id timeout_seconds)
            | Bg_shell.Interrupted ->
                Lwt.return
                  (Printf.sprintf
                     "Interrupted while waiting for background shell job #%d. \
                      The job is still running. Use bg_shell_wait with id=%d \
                      to resume waiting, or bg_shell_status to check progress."
                     id id)));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let bg_shell_result () =
  {
    Tool.name = "bg_shell_result";
    description =
      "Get the result and log output of a completed background shell job. \
       Optionally window output with head/tail parameters.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "id",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "Background shell job ID");
                    ] );
                ( "head",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "Show only the first N lines of output" );
                    ] );
                ( "tail",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "Show only the last N lines of output" );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let head =
          try Some (args |> member "head" |> to_int) with _ -> None
        in
        let tail =
          try Some (args |> member "tail" |> to_int) with _ -> None
        in
        match Bg_shell.find id with
        | None ->
            Lwt.return
              (Printf.sprintf
                 "Error: no background shell job with id=%d. Check the job ID \
                  is correct."
                 id)
        | Some job -> (
            match job.status with
            | Bg_shell.Running ->
                Lwt.return
                  (Printf.sprintf
                     "Error: background shell job #%d is still running. Use \
                      bg_shell_wait with id=%d to wait for completion, or \
                      bg_shell_status for current progress."
                     id id)
            | _ ->
                let output = Bg_shell.read_log job ?head ?tail () in
                let status = Bg_shell.status_string job in
                Lwt.return
                  (Printf.sprintf
                     "Background shell job #%d\n\
                      Command: %s\n\
                      Status: %s\n\n\
                      Output:\n\
                      %s"
                     job.id job.command status output)));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let tools () = [ bg_shell_status (); bg_shell_wait (); bg_shell_result () ]
