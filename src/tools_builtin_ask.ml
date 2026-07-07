type question_type =
  | Single_select of { options : string list }
  | Multi_select of { options : string list }
  | Text of { placeholder : string option }
  | Number of { min : int option; max : int option }
  | Confirm
  | Rating of { min : int; max : int }
  | File_upload of { accept : string option }
  | Date of { include_time : bool }

type question_item = {
  question : string;
  qtype : question_type;
  request_notes : bool;
      (** B594: model opts in via "notes": true on the question. When false
          (default), the daemon does NOT show the "Add notes?" prompt after the
          answer, even for single_select / multi_select / etc. *)
}

type question_result = {
  question : string;
  answer : string;
  notes : string option;
}

let parse_questions (args : Yojson.Safe.t) : question_item list =
  let open Yojson.Safe.Util in
  let qs = try args |> member "questions" |> to_list with _ -> [] in
  List.map
    (fun q ->
      let question = try q |> member "question" |> to_string with _ -> "" in
      let qtype_str = try q |> member "type" |> to_string with _ -> "text" in
      let qtype =
        match qtype_str with
        | "single_select" ->
            let options =
              try q |> member "options" |> to_list |> List.map to_string
              with _ -> []
            in
            Single_select { options }
        | "multi_select" ->
            let options =
              try q |> member "options" |> to_list |> List.map to_string
              with _ -> []
            in
            Multi_select { options }
        | "number" ->
            let min = try Some (q |> member "min" |> to_int) with _ -> None in
            let max = try Some (q |> member "max" |> to_int) with _ -> None in
            Number { min; max }
        | "confirm" -> Confirm
        | "rating" ->
            let min = try q |> member "min" |> to_int with _ -> 1 in
            let max = try q |> member "max" |> to_int with _ -> 5 in
            Rating { min; max }
        | "file_upload" ->
            let accept =
              try Some (q |> member "accept" |> to_string) with _ -> None
            in
            File_upload { accept }
        | "date" ->
            let include_time =
              try q |> member "include_time" |> to_bool with _ -> false
            in
            Date { include_time }
        | _ ->
            let placeholder =
              try Some (q |> member "placeholder" |> to_string) with _ -> None
            in
            Text { placeholder }
      in
      let request_notes =
        try q |> member "notes" |> to_bool with _ -> false
      in
      { question; qtype; request_notes })
    qs

let serialize_question_results (results : question_result list) : string =
  let open Yojson.Safe in
  to_string
    (`List
       (List.map
          (fun r ->
            `Assoc
              ([
                 ("question", `String r.question); ("answer", `String r.answer);
               ]
              @
              match r.notes with
              | Some n -> [ ("notes", `String n) ]
              | None -> []))
          results))

let question_items_to_json (items : question_item list) : string =
  let open Yojson.Safe in
  let qtype_to_json = function
    | Single_select { options } ->
        [
          ("type", `String "single_select");
          ("options", `List (List.map (fun s -> `String s) options));
        ]
    | Multi_select { options } ->
        [
          ("type", `String "multi_select");
          ("options", `List (List.map (fun s -> `String s) options));
        ]
    | Text { placeholder } ->
        ("type", `String "text")
        ::
        (match placeholder with
        | Some p -> [ ("placeholder", `String p) ]
        | None -> [])
    | Number { min; max } -> (
        ("type", `String "number")
        :: (match min with Some n -> [ ("min", `Int n) ] | None -> [])
        @ match max with Some n -> [ ("max", `Int n) ] | None -> [])
    | Confirm -> [ ("type", `String "confirm") ]
    | Rating { min; max } ->
        [ ("type", `String "rating"); ("min", `Int min); ("max", `Int max) ]
    | File_upload { accept } ->
        ("type", `String "file_upload")
        ::
        (match accept with
        | Some a -> [ ("accept", `String a) ]
        | None -> [])
    | Date { include_time } ->
        [ ("type", `String "date"); ("include_time", `Bool include_time) ]
  in
  to_string
    (`List
       (List.map
          (fun (qi : question_item) ->
            let base =
              ("question", `String qi.question) :: qtype_to_json qi.qtype
            in
            let with_notes =
              if qi.request_notes then base @ [ ("notes", `Bool true) ]
              else base
            in
            `Assoc with_notes)
          items))

let ask_user_question_schema =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ( "questions",
              `Assoc
                [
                  ("type", `String "array");
                  ( "description",
                    `String
                      "Array of questions to ask the user sequentially \
                       (required). Each question is sent one-at-a-time; the \
                       tool blocks until all are answered." );
                  ( "items",
                    `Assoc
                      [
                        ("type", `String "object");
                        ( "properties",
                          `Assoc
                            [
                              ( "type",
                                `Assoc
                                  [
                                    ("type", `String "string");
                                    ( "description",
                                      `String "Question type (required)" );
                                    ( "enum",
                                      `List
                                        [
                                          `String "single_select";
                                          `String "multi_select";
                                          `String "text";
                                          `String "number";
                                          `String "confirm";
                                          `String "rating";
                                          `String "file_upload";
                                          `String "date";
                                        ] );
                                  ] );
                              ( "question",
                                `Assoc
                                  [
                                    ("type", `String "string");
                                    ( "description",
                                      `String
                                        "The question text to display \
                                         (required)" );
                                  ] );
                              ( "options",
                                `Assoc
                                  [
                                    ("type", `String "array");
                                    ( "items",
                                      `Assoc [ ("type", `String "string") ] );
                                    ( "description",
                                      `String
                                        "Options for single_select/multi_select"
                                    );
                                  ] );
                              ( "placeholder",
                                `Assoc
                                  [
                                    ("type", `String "string");
                                    ( "description",
                                      `String "Hint for text input" );
                                  ] );
                              ( "min",
                                `Assoc
                                  [
                                    ("type", `String "integer");
                                    ( "description",
                                      `String "Min value for number/rating" );
                                  ] );
                              ( "max",
                                `Assoc
                                  [
                                    ("type", `String "integer");
                                    ( "description",
                                      `String "Max value for number/rating" );
                                  ] );
                              ( "accept",
                                `Assoc
                                  [
                                    ("type", `String "string");
                                    ( "description",
                                      `String "MIME type hint for file_upload"
                                    );
                                  ] );
                              ( "include_time",
                                `Assoc
                                  [
                                    ("type", `String "boolean");
                                    ( "description",
                                      `String
                                        "Include time in date picker (default \
                                         false)" );
                                  ] );
                              ( "notes",
                                `Assoc
                                  [
                                    ("type", `String "boolean");
                                    ( "description",
                                      `String
                                        "Set true to prompt the user for \
                                         optional follow-up notes after they \
                                         answer (default false). Skipped \
                                         automatically for free-form types \
                                         (text, file_upload) and for binary \
                                         types (confirm, rating)." );
                                  ] );
                            ] );
                        ( "required",
                          `List [ `String "type"; `String "question" ] );
                      ] );
                ] );
          ] );
      ("required", `List [ `String "questions" ]);
    ]

let ask_user_question
    ~(ask_fn :
       (session_key:string ->
       questions:question_item list ->
       question_result list Lwt.t)
       option) =
  {
    Tool.name = "ask_user_question";
    description =
      "Ask the user one or more clarifying questions and wait for answers. \
       Questions are sent sequentially; each blocks until answered. Supports \
       types: single_select, multi_select, text, number, confirm, rating, \
       file_upload, date. Returns JSON array of {question, answer, notes?}. \
       Only available in interactive channel sessions (Telegram, Discord, \
       Slack, web).";
    parameters_schema = ask_user_question_schema;
    invoke =
      (fun ?context args ->
        match (ask_fn, context) with
        | None, _ ->
            Lwt.return
              "Error: ask_user_question is only available in interactive \
               channel sessions (Telegram, Discord, Slack, web). This tool \
               requires a channel notifier to send questions and receive \
               replies."
        | _, None ->
            Lwt.return
              "Error: no invoke context provided. This is an internal error — \
               the tool requires a session context to function."
        | Some fn, Some ctx -> (
            match ctx.Tool.session_key with
            | None ->
                Lwt.return
                  "Error: no session key in context. ask_user_question \
                   requires an active session to identify the user channel."
            | Some sk ->
                let questions = parse_questions args in
                if questions = [] then
                  Lwt.return
                    "Error: questions array is empty. Provide at least one \
                     question object with 'type' and 'question' fields."
                else
                  Lwt.catch
                    (fun () ->
                      let open Lwt.Syntax in
                      let* results = fn ~session_key:sk ~questions in
                      Lwt.return (serialize_question_results results))
                    (fun exn ->
                      Lwt.return
                        (Printf.sprintf
                           "Error: question cancelled or failed: %s"
                           (Printexc.to_string exn)))));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }
