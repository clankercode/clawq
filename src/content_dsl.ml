type inline =
  | Text of string
  | Bold of string
  | Italic of string
  | Code of string
  | Link of { text : string; url : string }
  | Emoji of string

type tool_state = Done | Failed | Running | Pending

type block =
  | Paragraph of inline list
  | CodeBlock of { language : string option; content : string }
  | ToolEntry of {
      emoji : string;
      name : string;
      summary : string option;
      state : tool_state;
      timing : string option;
      preview : string option;
      error_detail : string option;
      connector_char : string option;
    }
  | ProgressBar of { filled : int; total : int; done_count : int }
  | CollapsedTools of { count : int }
  | ToolSummary of {
      total : int;
      emoji_breakdown : string;
      parallel_indicator : string;
      total_time : string;
    }
  | Separator
  | ThinkingPreview of string

type document = block list

let render_inline c = function
  | Text s -> Format_adapter.escape c s
  | Bold s -> Format_adapter.bold c (Format_adapter.escape c s)
  | Italic s -> Format_adapter.italic c (Format_adapter.escape c s)
  | Code s -> Format_adapter.code c (Format_adapter.escape c s)
  | Link { text; url } -> Format_adapter.link c ~text ~url
  | Emoji s -> s

let render_block c = function
  | Paragraph inlines -> String.concat "" (List.map (render_inline c) inlines)
  | CodeBlock { language; content } -> (
      let lang = match language with Some l -> l | None -> "" in
      match c with
      | Format_adapter.Telegram_html ->
          "<pre>" ^ Format_adapter.escape c content ^ "</pre>"
      | _ -> "```" ^ lang ^ "\n" ^ content ^ "\n```")
  | ToolEntry
      {
        emoji;
        name;
        summary;
        state;
        timing;
        preview;
        error_detail;
        connector_char;
      } -> (
      let prefix = match connector_char with Some ch -> ch | None -> "" in
      match state with
      | Done ->
          let summary_part =
            match summary with
            | Some s ->
                Printf.sprintf " \xE2\x80\x94 %s"
                  (Format_adapter.code c (Format_adapter.escape c s))
            | None -> ""
          in
          let preview_part =
            match preview with
            | Some p ->
                Printf.sprintf " \xE2\x86\x92 %s"
                  (Format_adapter.italic c (Format_adapter.escape c p))
            | None -> ""
          in
          let timing_part =
            match timing with
            | Some t -> " " ^ Format_adapter.escape c t
            | None -> ""
          in
          Printf.sprintf "%s\xE2\x9C\x93 %s %s%s%s%s" prefix emoji
            (Format_adapter.bold c (Format_adapter.escape c name))
            summary_part preview_part timing_part
      | Failed ->
          let summary_part =
            match summary with
            | Some s ->
                Printf.sprintf " \xE2\x80\x94 %s"
                  (Format_adapter.code c (Format_adapter.escape c s))
            | None -> ""
          in
          let timing_part =
            match timing with
            | Some t -> " " ^ Format_adapter.escape c t
            | None -> ""
          in
          let lb = Format_adapter.line_break c in
          let error_part =
            match error_detail with
            | Some err ->
                Printf.sprintf "%s  \xE2\x94\x94 %s" lb
                  (Format_adapter.italic c (Format_adapter.escape c err))
            | None -> ""
          in
          Printf.sprintf "%s\xE2\x9C\x97 %s %s%s%s%s" prefix emoji
            (Format_adapter.bold c (Format_adapter.escape c name))
            summary_part timing_part error_part
      | Running ->
          let summary_part =
            match summary with
            | Some s ->
                Printf.sprintf " \xE2\x80\x94 %s"
                  (Format_adapter.code c (Format_adapter.escape c s))
            | None -> ""
          in
          let timing_part =
            match timing with
            | Some t -> " " ^ Format_adapter.escape c t
            | None -> ""
          in
          Printf.sprintf "%s\xE2\x97\x89 %s %s%s%s" prefix emoji
            (Format_adapter.bold c (Format_adapter.escape c name))
            summary_part timing_part
      | Pending -> Printf.sprintf "%s\xE2\x97\x8B %s %s" prefix emoji name)
  | ProgressBar { filled; total; done_count } ->
      let bar_width = 8 in
      let fill_count =
        if total > 0 then done_count * bar_width / total else 0
      in
      let empty = bar_width - fill_count in
      let repeat n s =
        let buf = Buffer.create (n * String.length s) in
        for _ = 1 to n do
          Buffer.add_string buf s
        done;
        Buffer.contents buf
      in
      let bar =
        repeat fill_count "\xE2\x96\x93" ^ repeat empty "\xE2\x96\x91"
      in
      Printf.sprintf "%s %d/%d" bar done_count total
  | CollapsedTools { count } ->
      Printf.sprintf "\xE2\x9C\x93 %d tools completed" count
  | ToolSummary { total; emoji_breakdown; parallel_indicator; total_time } ->
      let lb = Format_adapter.line_break c in
      Printf.sprintf
        "\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81%s\xF0\x9F\x9B\xA0\xEF\xB8\x8F \
         %d tools \xC2\xB7 %s%s \xC2\xB7 %s"
        lb total emoji_breakdown parallel_indicator total_time
  | Separator ->
      "\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81"
  | ThinkingPreview text ->
      Printf.sprintf "\xF0\x9F\x92\xAD %s"
        (Format_adapter.italic c
           (Format_adapter.escape c
              (Stream_visibility.truncate_text ~max_chars:200 text)))

let render_document connector doc =
  let lines = List.map (render_block connector) doc in
  String.concat (Format_adapter.line_break connector) lines
