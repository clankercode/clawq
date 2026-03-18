(* Question presentation strategies for ask_user_question.
   Selects the best UX per connector — inline buttons on Telegram,
   Adaptive Cards on Teams, formatted text elsewhere. *)

type render_strategy = Rich_buttons | Rich_poll | Formatted_text | Plain_text
type rendered_question = RichMessage of Rich_message.t | TextMessage of string

let select_strategy ~(capabilities : Connector_capabilities.t option)
    ~has_rich_notifier (qtype : Tools_builtin.question_type) =
  let can_buttons =
    has_rich_notifier
    &&
    match capabilities with
    | Some c -> Connector_capabilities.supports_rich_questions c
    | None -> false
  in
  let strategy =
    match qtype with
    | Tools_builtin.Single_select { options } ->
        if can_buttons && List.length options <= 8 then Rich_buttons
        else Formatted_text
    | Tools_builtin.Confirm ->
        if can_buttons then Rich_buttons else Formatted_text
    | Tools_builtin.Rating { min; max } ->
        if can_buttons && max - min + 1 <= 5 then Rich_buttons
        else Formatted_text
    | Tools_builtin.Multi_select _ ->
        if can_buttons then Rich_poll else Formatted_text
    | Tools_builtin.Text _ | Tools_builtin.Number _
    | Tools_builtin.File_upload _ | Tools_builtin.Date _ ->
        Formatted_text
  in
  Logs.debug (fun m ->
      m "Question_presenter: strategy=%s has_rich=%b"
        (match strategy with
        | Rich_buttons -> "Rich_buttons"
        | Rich_poll -> "Rich_poll"
        | Formatted_text -> "Formatted_text"
        | Plain_text -> "Plain_text")
        has_rich_notifier);
  strategy

(* Generate a short unique callback ID prefix from session key *)
let short_hash s =
  let h = Hashtbl.hash s in
  Printf.sprintf "%08x" (h land 0x7FFFFFFF)

let make_callback_id ~session_key ~question_index ~option_index =
  Printf.sprintf "auq:%s:%d:%d" (short_hash session_key) question_index
    option_index

let parse_callback_id s =
  match String.split_on_char ':' s with
  | [ "auq"; _hash; qi; oi ] -> (
      try Some (int_of_string qi, int_of_string oi) with _ -> None)
  | _ -> None

(* Build Rich_message.TextWithButtons for single_select / confirm / rating *)
let build_buttons_message ~session_key ~question_index ~text
    ~(options : (int * string) list) =
  let max_per_row = 3 in
  let buttons =
    List.mapi
      (fun i (_num, label) ->
        Rich_message.
          {
            label;
            callback_id =
              make_callback_id ~session_key ~question_index ~option_index:i;
          })
      options
  in
  let rec chunk n lst =
    if lst = [] then []
    else
      let take, rest =
        let rec split acc i = function
          | [] -> (List.rev acc, [])
          | x :: xs ->
              if i >= n then (List.rev acc, x :: xs)
              else split (x :: acc) (i + 1) xs
        in
        split [] 0 lst
      in
      take :: chunk n rest
  in
  let button_rows = chunk max_per_row buttons in
  Rich_message.TextWithButtons { text; button_rows }

(* Build Rich_message.Poll for multi_select *)
let build_poll_message ~question ~(options : string list) =
  Rich_message.Poll { question; options; allows_multiple = true }

(* Render a question using Content_dsl for formatted text output *)
let render_formatted_text ~(connector : Format_adapter.connector) ~index ~total
    (qi : Tools_builtin.question_item) =
  let prefix =
    if total > 1 then Printf.sprintf "[Question %d/%d] " (index + 1) total
    else ""
  in
  let question_text = prefix ^ qi.question in
  let hint, options, instruction =
    match qi.qtype with
    | Tools_builtin.Single_select { options } ->
        let opts = List.mapi (fun i o -> (i + 1, o)) options in
        (None, opts, Some "Reply with number or text")
    | Tools_builtin.Multi_select { options } ->
        let opts = List.mapi (fun i o -> (i + 1, o)) options in
        (None, opts, Some "Reply with numbers separated by commas, e.g. 1,3")
    | Tools_builtin.Confirm -> (None, [], Some "Reply yes/no")
    | Tools_builtin.Rating { min; max } ->
        ( None,
          [],
          Some (Printf.sprintf "Reply with a number from %d to %d" min max) )
    | Tools_builtin.Number { min; max } ->
        let constraint_str =
          match (min, max) with
          | Some lo, Some hi -> Printf.sprintf " (between %d and %d)" lo hi
          | Some lo, None -> Printf.sprintf " (minimum %d)" lo
          | None, Some hi -> Printf.sprintf " (maximum %d)" hi
          | None, None -> ""
        in
        (None, [], Some (Printf.sprintf "Reply with a number%s" constraint_str))
    | Tools_builtin.File_upload { accept } ->
        let hint_text =
          match accept with
          | Some mime -> Printf.sprintf "Accepted: %s" mime
          | None -> ""
        in
        ( (if hint_text <> "" then Some hint_text else None),
          [],
          Some "Upload a file" )
    | Tools_builtin.Date { include_time } ->
        let fmt = if include_time then "YYYY-MM-DD HH:MM" else "YYYY-MM-DD" in
        (None, [], Some (Printf.sprintf "Reply with a date in %s format" fmt))
    | Tools_builtin.Text { placeholder } -> (placeholder, [], None)
  in
  let doc =
    [ Content_dsl.QuestionBlock { question_text; hint; options; instruction } ]
  in
  Content_dsl.render_document connector doc

(* Render a question as plain text (no formatting) — matches original daemon.ml *)
let render_plain_text ~index ~total (qi : Tools_builtin.question_item) =
  let msg =
    match qi.qtype with
    | Tools_builtin.Single_select { options } ->
        let opts =
          List.mapi (fun i o -> Printf.sprintf "%d. %s" (i + 1) o) options
        in
        Printf.sprintf "%s\n%s\n(Reply with number or text)" qi.question
          (String.concat "\n" opts)
    | Tools_builtin.Multi_select { options } ->
        let opts =
          List.mapi (fun i o -> Printf.sprintf "%d. %s" (i + 1) o) options
        in
        Printf.sprintf
          "%s\n%s\n(Reply with numbers separated by commas, e.g. 1,3)"
          qi.question (String.concat "\n" opts)
    | Tools_builtin.Confirm -> Printf.sprintf "%s\n(Reply yes/no)" qi.question
    | Tools_builtin.Rating { min; max } ->
        Printf.sprintf "%s\n(Reply with a number from %d to %d)" qi.question min
          max
    | Tools_builtin.Number { min; max } ->
        let constraint_str =
          match (min, max) with
          | Some lo, Some hi -> Printf.sprintf " (between %d and %d)" lo hi
          | Some lo, None -> Printf.sprintf " (minimum %d)" lo
          | None, Some hi -> Printf.sprintf " (maximum %d)" hi
          | None, None -> ""
        in
        Printf.sprintf "%s\n(Reply with a number%s)" qi.question constraint_str
    | Tools_builtin.File_upload { accept } ->
        let hint =
          match accept with
          | Some mime -> Printf.sprintf " (%s)" mime
          | None -> ""
        in
        Printf.sprintf "%s\n(Upload a file%s)" qi.question hint
    | Tools_builtin.Date { include_time } ->
        let fmt = if include_time then "YYYY-MM-DD HH:MM" else "YYYY-MM-DD" in
        Printf.sprintf "%s\n(Reply with a date in %s format)" qi.question fmt
    | Tools_builtin.Text { placeholder } ->
        let hint =
          match placeholder with
          | Some p -> Printf.sprintf "\n(Hint: %s)" p
          | None -> ""
        in
        qi.question ^ hint
  in
  if total > 1 then Printf.sprintf "[Question %d/%d] %s" (index + 1) total msg
  else msg

(* Main render function *)
let render_question ~strategy ~(connector : Format_adapter.connector)
    ~session_key ~index ~total (qi : Tools_builtin.question_item) =
  match strategy with
  | Rich_buttons -> (
      let prefix =
        if total > 1 then Printf.sprintf "[Question %d/%d] " (index + 1) total
        else ""
      in
      let text = prefix ^ qi.question in
      match qi.qtype with
      | Tools_builtin.Single_select { options } ->
          let numbered = List.mapi (fun i o -> (i + 1, o)) options in
          RichMessage
            (build_buttons_message ~session_key ~question_index:index ~text
               ~options:numbered)
      | Tools_builtin.Confirm ->
          RichMessage
            (build_buttons_message ~session_key ~question_index:index ~text
               ~options:[ (1, "Yes"); (2, "No") ])
      | Tools_builtin.Rating { min; max } ->
          let nums =
            List.init
              (max - min + 1)
              (fun i ->
                let n = min + i in
                (n, string_of_int n))
          in
          RichMessage
            (build_buttons_message ~session_key ~question_index:index ~text
               ~options:nums)
      | _ -> TextMessage (render_formatted_text ~connector ~index ~total qi))
  | Rich_poll -> (
      match qi.qtype with
      | Tools_builtin.Multi_select { options } ->
          let prefix =
            if total > 1 then Printf.sprintf "[Q%d/%d] " (index + 1) total
            else ""
          in
          RichMessage
            (build_poll_message ~question:(prefix ^ qi.question) ~options)
      | _ -> TextMessage (render_formatted_text ~connector ~index ~total qi))
  | Formatted_text ->
      TextMessage (render_formatted_text ~connector ~index ~total qi)
  | Plain_text -> TextMessage (render_plain_text ~index ~total qi)

(* Extract callback_id -> answer_text mappings from a RichMessage *)
let extract_callback_answers = function
  | Rich_message.TextWithButtons { button_rows; _ } ->
      List.concat_map
        (fun row ->
          List.map
            (fun (btn : Rich_message.button) -> (btn.callback_id, btn.label))
            row)
        button_rows
  | Rich_message.Poll { options; _ } ->
      List.mapi (fun i opt -> (Printf.sprintf "poll_opt_%d" i, opt)) options
  | Rich_message.Text _ | Rich_message.FileAttachment _ -> []

(* Build a Teams Adaptive Card JSON for multi_select with ChoiceSet *)
let build_teams_poll_card ~question ~(options : string list) =
  let choices =
    List.map
      (fun opt -> `Assoc [ ("title", `String opt); ("value", `String opt) ])
      options
  in
  `Assoc
    [
      ("type", `String "message");
      ( "attachments",
        `List
          [
            `Assoc
              [
                ( "contentType",
                  `String "application/vnd.microsoft.card.adaptive" );
                ( "content",
                  `Assoc
                    [
                      ("type", `String "AdaptiveCard");
                      ( "$schema",
                        `String
                          "http://adaptivecards.io/schemas/adaptive-card.json"
                      );
                      ("version", `String "1.3");
                      ( "body",
                        `List
                          [
                            `Assoc
                              [
                                ("type", `String "TextBlock");
                                ("text", `String question);
                                ("wrap", `Bool true);
                                ("weight", `String "Bolder");
                              ];
                            `Assoc
                              [
                                ("type", `String "Input.ChoiceSet");
                                ("id", `String "clawq_question_answer");
                                ("isMultiSelect", `Bool true);
                                ("style", `String "expanded");
                                ("choices", `List choices);
                              ];
                          ] );
                      ( "actions",
                        `List
                          [
                            `Assoc
                              [
                                ("type", `String "Action.Submit");
                                ("title", `String "Submit");
                              ];
                          ] );
                    ] );
              ];
          ] );
    ]

(* Build a Teams Adaptive Card from Rich_message buttons *)
let build_teams_card_from_buttons ~text ~button_rows =
  let actions =
    List.concat_map
      (fun row ->
        List.map
          (fun (btn : Rich_message.button) ->
            `Assoc
              [
                ("type", `String "Action.Submit");
                ("title", `String btn.label);
                ("data", `Assoc [ ("clawq_question_answer", `String btn.label) ]);
              ])
          row)
      button_rows
  in
  `Assoc
    [
      ("type", `String "message");
      ( "attachments",
        `List
          [
            `Assoc
              [
                ( "contentType",
                  `String "application/vnd.microsoft.card.adaptive" );
                ( "content",
                  `Assoc
                    [
                      ("type", `String "AdaptiveCard");
                      ( "$schema",
                        `String
                          "http://adaptivecards.io/schemas/adaptive-card.json"
                      );
                      ("version", `String "1.3");
                      ( "body",
                        `List
                          [
                            `Assoc
                              [
                                ("type", `String "TextBlock");
                                ("text", `String text);
                                ("wrap", `Bool true);
                                ("weight", `String "Bolder");
                              ];
                          ] );
                      ("actions", `List actions);
                    ] );
              ];
          ] );
    ]
