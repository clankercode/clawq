let transcript_tool ~db =
  {
    Tool.name = "background_task_transcript";
    description =
      "Read a bounded transcript for a background/native subagent task. \
       Prefers the stable local session history, then ACP history, then the \
       task log file. Regex filtering is applied before output caps; oversized \
       results are exported as searchable JSONL instead of being returned \
       inline.";
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
                      ( "description",
                        `String "Background task id whose transcript to read" );
                    ] );
                ( "regex",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional OCaml Str regex. Matching is applied to \
                           role + content before line caps." );
                    ] );
                ( "max_lines",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Optional inline line cap. Defaults to 200 and hard \
                           clamps at 300." );
                    ] );
                ( "export",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "When true, also write the filtered transcript to \
                           JSONL and include the export path." );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let regex =
          try
            match args |> member "regex" with
            | `String s when String.trim s <> "" -> Some s
            | _ -> None
          with _ -> None
        in
        let max_lines =
          try
            match args |> member "max_lines" with
            | `Int n -> Some n
            | `Intlit s -> Some (int_of_string s)
            | _ -> None
          with _ -> None
        in
        let export = try args |> member "export" |> to_bool with _ -> false in
        if id < 0 then
          Lwt.return "Error: id is required and must be a non-negative integer."
        else
          Lwt.return
            (Background_task_transcript.render ?regex ?max_lines ~export ~db ~id
               ()));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }
