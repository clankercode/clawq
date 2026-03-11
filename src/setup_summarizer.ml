(* setup_summarizer.ml — Interactive setup wizard for autosummarizer config *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_model s =
  let trimmed = String.trim s in
  match Pmodel.parse trimmed with Ok _ as ok -> ok | Error _ as err -> err

let validate_positive_int s =
  match int_of_string_opt (String.trim s) with
  | None -> Error "Not a valid integer."
  | Some n when n <= 0 -> Error "Value must be positive (> 0)."
  | Some n -> Ok n

let validate_non_negative_int s =
  match int_of_string_opt (String.trim s) with
  | None -> Error "Not a valid integer."
  | Some n when n < 0 -> Error "Value must be non-negative (>= 0)."
  | Some n -> Ok n

let validate_tool_name s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "Tool name cannot be empty."
  else if String.to_seq trimmed |> Seq.exists (fun c -> c = ' ' || c = '\t')
  then Error "Tool name must not contain whitespace."
  else Ok trimmed

let validate_threshold_chars s =
  match validate_positive_int s with
  | Error _ as err -> err
  | Ok n ->
      let warning =
        if n < 500 then
          Some "Warning: very low threshold may cause excessive summarization."
        else if n > 50000 then
          Some
            "Warning: very high threshold; most results will never be \
             summarized."
        else None
      in
      Ok (n, warning)

let validate_p1_max_chars ~p2_max s =
  match validate_positive_int s with
  | Error _ as err -> err
  | Ok n when n <= p2_max ->
      Error
        (Printf.sprintf "P1 max (%d) must be greater than P2 max (%d)." n p2_max)
  | Ok n -> Ok n

let validate_p2_max_chars ~p1_max s =
  match validate_positive_int s with
  | Error _ as err -> err
  | Ok n when n >= p1_max ->
      Error
        (Printf.sprintf "P2 max (%d) must be less than P1 max (%d)." n p1_max)
  | Ok n -> Ok n

(* ── JSON builder ────────────────────────────────────────────────── *)

let build_summarizer_json ~(sc : Runtime_config.summarizer_config) =
  let fields =
    [
      ("enabled", `Bool sc.summarizer_enabled);
      ("model", `String (Pmodel.to_string sc.summarizer_model));
      ( "escalation_model",
        match sc.escalation_model with
        | None -> `Null
        | Some m -> `String (Pmodel.to_string m) );
      ("threshold_chars", `Int sc.threshold_chars);
      ("p1_max_chars", `Int sc.p1_max_chars);
      ("p2_max_chars", `Int sc.p2_max_chars);
      ("context_window_messages", `Int sc.context_window_messages);
      ("excluded_tools", `List (List.map (fun s -> `String s) sc.excluded_tools));
      ("max_age_days", `Int sc.max_age_days);
      ( "envelope_template",
        match sc.envelope_template with None -> `Null | Some t -> `String t );
    ]
  in
  `Assoc [ ("summarizer", `Assoc fields) ]

(* ── Post-setup instructions ─────────────────────────────────────── *)

let post_setup_instructions ~(sc : Runtime_config.summarizer_config) =
  let model_str = Pmodel.to_string sc.summarizer_model in
  let esc_str =
    match sc.escalation_model with
    | None -> "(none)"
    | Some m -> Pmodel.to_string m
  in
  Printf.sprintf
    {|
  Autosummarizer Setup Summary
  ============================

  Enabled:              %s
  Model:                %s
  Escalation model:     %s
  Threshold:            %d chars
  P1 max:               %d chars
  P2 max:               %d chars
  Context window:       %d messages
  Max age:              %d days

  How it works:
    Tool results exceeding the threshold are automatically summarized
    before being added to conversation history. The original content
    is preserved and can be recovered using the "unsummarize" tool.

  Next steps:
    1. Start or restart the daemon:  clawq daemon start
    2. Summarization happens automatically during agent turns
    3. Use "unsummarize" tool to recover original content when needed
|}
    (if sc.summarizer_enabled then "yes" else "no")
    model_str esc_str sc.threshold_chars sc.p1_max_chars sc.p2_max_chars
    sc.context_window_messages sc.max_age_days

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.summarizer
  with _ -> Runtime_config.default_summarizer_config

(* ── TUI drawing ─────────────────────────────────────────────────── *)

let draw_dashboard ~(sc : Runtime_config.summarizer_config) =
  let open Setup_common in
  let w = terminal_width () in
  clear_screen ();
  Printf.printf "\n";
  let enabled_str =
    if sc.summarizer_enabled then green "enabled" else dim "disabled"
  in
  let model_str = Pmodel.to_string sc.summarizer_model in
  let esc_str =
    match sc.escalation_model with
    | None -> dim "(none)"
    | Some m -> cyan (Pmodel.to_string m)
  in
  let excluded_str =
    if sc.excluded_tools = [] then dim "(none)"
    else String.concat ", " sc.excluded_tools
  in
  let envelope_str =
    match sc.envelope_template with
    | None -> dim "(not set)"
    | Some _ -> green "set"
  in
  draw_box ~width:w
    [
      bold " Autosummarizer Configuration ";
      dim
        "  Automatically summarizes large tool results to reduce context usage.";
      "";
      Printf.sprintf "  Enabled:           %s" enabled_str;
      Printf.sprintf "  Model:             %s" (cyan model_str);
      Printf.sprintf "  Escalation model:  %s" esc_str;
      Printf.sprintf "  Threshold:         %d chars" sc.threshold_chars;
      Printf.sprintf "  P1 max:            %d chars" sc.p1_max_chars;
      Printf.sprintf "  P2 max:            %d chars" sc.p2_max_chars;
      Printf.sprintf "  Context window:    %d messages"
        sc.context_window_messages;
      Printf.sprintf "  Excluded tools:    %s" excluded_str;
      Printf.sprintf "  Max age:           %d days" sc.max_age_days;
      Printf.sprintf "  Envelope template: %s" envelope_str;
      "";
    ];
  print_docs_link "https://clawq.org/configuration/#summarizer";
  Printf.printf "\n";
  draw_separator ~width:w

(* ── Save helper ─────────────────────────────────────────────────── *)

let save_summarizer_config ~(sc : Runtime_config.summarizer_config) =
  let open Setup_common in
  let json = build_summarizer_json ~sc in
  let full_json =
    match load_config_json () with
    | Some existing -> deep_merge_json existing json
    | None -> json
  in
  match write_config_json full_json with
  | Ok path ->
      print_success (Printf.sprintf "Saved to %s" path);
      true
  | Error e ->
      print_error (Printf.sprintf "Failed to write config: %s" e);
      false

(* ── Main menu loop ──────────────────────────────────────────────── *)

let run () =
  match Setup_common.check_tty () with
  | Error e -> e
  | Ok () ->
      let existing = load_existing () in
      let sc = ref existing in
      let dirty = ref false in
      let quit = ref false in
      while not !quit do
        draw_dashboard ~sc:!sc;
        let options =
          [
            ( "e",
              Printf.sprintf "Toggle enabled (%s)"
                (if !sc.summarizer_enabled then "currently on"
                 else "currently off") );
            ( "m",
              Printf.sprintf "Set model (currently: %s)"
                (Pmodel.to_string !sc.summarizer_model) );
            ("x", "Set escalation model (empty to clear)");
            ( "t",
              Printf.sprintf "Set threshold chars (currently: %d)"
                !sc.threshold_chars );
            ( "1",
              Printf.sprintf "Set P1 max chars (currently: %d)" !sc.p1_max_chars
            );
            ( "2",
              Printf.sprintf "Set P2 max chars (currently: %d)" !sc.p2_max_chars
            );
            ( "c",
              Printf.sprintf "Set context window messages (currently: %d)"
                !sc.context_window_messages );
            ("a", "Add excluded tool");
            ("r", "Remove excluded tool");
            ( "d",
              Printf.sprintf "Set max age days (currently: %d)" !sc.max_age_days
            );
            ("v", "Set envelope template (empty to clear)");
            ("i", "Show post-setup instructions");
          ]
          @
          if !dirty then [ ("s", Setup_common.bold "Save configuration") ]
          else []
        in
        let choice =
          Setup_common.prompt_menu ~title:"Actions" ~options
            ~shortcut_exit:"q/Enter" ()
        in
        match String.lowercase_ascii choice with
        | "q" | "" ->
            if !dirty then begin
              let save =
                Setup_common.prompt_yn
                  ~prompt:"You have unsaved changes. Save before exiting?"
                  ~default:true ()
              in
              if save then begin
                ignore (save_summarizer_config ~sc:!sc);
                quit := true
              end
              else quit := true
            end
            else quit := true
        | "e" ->
            sc := { !sc with summarizer_enabled = not !sc.summarizer_enabled };
            dirty := true
        | "m" ->
            let rec get_model () =
              let s =
                Setup_common.prompt_string ~prompt:"Model (provider:model)"
                  ~default:(Pmodel.to_string !sc.summarizer_model)
                  ()
              in
              match validate_model s with
              | Ok m ->
                  sc := { !sc with summarizer_model = m };
                  dirty := true
              | Error e ->
                  Setup_common.print_warning e;
                  get_model ()
            in
            get_model ()
        | "x" ->
            let s =
              Setup_common.prompt_string
                ~prompt:"Escalation model (empty to clear)"
                ~default:
                  (match !sc.escalation_model with
                  | None -> ""
                  | Some m -> Pmodel.to_string m)
                ()
            in
            let trimmed = String.trim s in
            if trimmed = "" then begin
              sc := { !sc with escalation_model = None };
              dirty := true
            end
            else
              let rec get_esc_model input =
                match validate_model input with
                | Ok m ->
                    sc := { !sc with escalation_model = Some m };
                    dirty := true
                | Error e ->
                    Setup_common.print_warning e;
                    let s2 =
                      Setup_common.prompt_string
                        ~prompt:"Escalation model (empty to clear)" ~default:""
                        ()
                    in
                    let t2 = String.trim s2 in
                    if t2 = "" then begin
                      sc := { !sc with escalation_model = None };
                      dirty := true
                    end
                    else get_esc_model t2
              in
              get_esc_model trimmed
        | "t" ->
            let rec get_threshold () =
              let s =
                Setup_common.prompt_string ~prompt:"Threshold (chars)"
                  ~default:(string_of_int !sc.threshold_chars)
                  ()
              in
              match validate_threshold_chars s with
              | Ok (n, warning) ->
                  (match warning with
                  | Some w -> Setup_common.print_warning w
                  | None -> ());
                  sc := { !sc with threshold_chars = n };
                  dirty := true
              | Error e ->
                  Setup_common.print_warning e;
                  get_threshold ()
            in
            get_threshold ()
        | "1" ->
            let rec get_p1 () =
              let s =
                Setup_common.prompt_string ~prompt:"P1 max (chars)"
                  ~default:(string_of_int !sc.p1_max_chars)
                  ()
              in
              match validate_p1_max_chars ~p2_max:!sc.p2_max_chars s with
              | Ok n ->
                  sc := { !sc with p1_max_chars = n };
                  dirty := true
              | Error e ->
                  Setup_common.print_warning e;
                  get_p1 ()
            in
            get_p1 ()
        | "2" ->
            let rec get_p2 () =
              let s =
                Setup_common.prompt_string ~prompt:"P2 max (chars)"
                  ~default:(string_of_int !sc.p2_max_chars)
                  ()
              in
              match validate_p2_max_chars ~p1_max:!sc.p1_max_chars s with
              | Ok n ->
                  sc := { !sc with p2_max_chars = n };
                  dirty := true
              | Error e ->
                  Setup_common.print_warning e;
                  get_p2 ()
            in
            get_p2 ()
        | "c" ->
            let rec get_ctx () =
              let s =
                Setup_common.prompt_string ~prompt:"Context window (messages)"
                  ~default:(string_of_int !sc.context_window_messages)
                  ()
              in
              match validate_non_negative_int s with
              | Ok n ->
                  sc := { !sc with context_window_messages = n };
                  dirty := true
              | Error e ->
                  Setup_common.print_warning e;
                  get_ctx ()
            in
            get_ctx ()
        | "a" ->
            let rec get_tool () =
              let s =
                Setup_common.prompt_string ~prompt:"Tool name to exclude"
                  ~default:"" ()
              in
              match validate_tool_name s with
              | Ok name ->
                  if List.mem name !sc.excluded_tools then
                    Setup_common.print_warning
                      (Printf.sprintf "%S is already excluded." name)
                  else begin
                    sc :=
                      {
                        !sc with
                        excluded_tools = !sc.excluded_tools @ [ name ];
                      };
                    dirty := true
                  end
              | Error e ->
                  Setup_common.print_warning e;
                  get_tool ()
            in
            get_tool ()
        | "r" ->
            if !sc.excluded_tools = [] then
              Setup_common.print_warning "No excluded tools to remove."
            else begin
              Printf.printf "\n  Excluded tools:\n";
              List.iteri
                (fun i name -> Printf.printf "    %d. %s\n" (i + 1) name)
                !sc.excluded_tools;
              Printf.printf "\n";
              let s =
                Setup_common.prompt_string
                  ~prompt:"Number to remove (or empty to cancel)" ~default:"" ()
              in
              match int_of_string_opt (String.trim s) with
              | Some idx when idx >= 1 && idx <= List.length !sc.excluded_tools
                ->
                  let tools =
                    List.filteri (fun i _ -> i <> idx - 1) !sc.excluded_tools
                  in
                  sc := { !sc with excluded_tools = tools };
                  dirty := true
              | _ ->
                  if String.trim s <> "" then
                    Setup_common.print_warning "Invalid selection."
            end
        | "d" ->
            let rec get_days () =
              let s =
                Setup_common.prompt_string ~prompt:"Max age (days)"
                  ~default:(string_of_int !sc.max_age_days)
                  ()
              in
              match validate_positive_int s with
              | Ok n ->
                  sc := { !sc with max_age_days = n };
                  dirty := true
              | Error e ->
                  Setup_common.print_warning e;
                  get_days ()
            in
            get_days ()
        | "v" ->
            let open Setup_common in
            let placeholders =
              [
                ("{summary}", "The summary text");
                ("{sum_id}", "Summary ID (for unsummarize)");
                ("{tool_name}", "Tool that produced the result");
                ("{model}", "Model used for summarization");
                ("{orig_lines}", "Original line count");
                ("{orig_bytes}", "Original byte count");
                ("{orig_tokens}", "Original token estimate");
                ("{sum_lines}", "Summary line count");
                ("{sum_bytes}", "Summary byte count");
                ("{sum_tokens}", "Summary token estimate");
                ("{timestamp}", "When summarization occurred");
              ]
            in
            Printf.printf "\n";
            Printf.printf "  %s\n" (bold "Current envelope template:");
            (match !sc.envelope_template with
            | None -> Printf.printf "    %s\n" (dim "(using built-in default)")
            | Some t ->
                let lines = String.split_on_char '\n' t in
                List.iter
                  (fun line -> Printf.printf "    %s\n" (cyan line))
                  lines);
            Printf.printf "\n";
            Printf.printf "  %s\n"
              (bold "The envelope template wraps each summarized tool result.");
            Printf.printf
              "  Use %s for newlines. Leave empty to use the built-in default.\n\n"
              (cyan "\\n");
            Printf.printf "  %s\n" (bold "Available placeholders:");
            let max_ph_len =
              List.fold_left
                (fun acc (ph, _) -> max acc (String.length ph))
                0 placeholders
            in
            List.iter
              (fun (ph, desc) ->
                let pad = String.make (max_ph_len - String.length ph) ' ' in
                Printf.printf "    %s%s  %s\n" (green ph) pad (dim desc))
              placeholders;
            Printf.printf "\n  %s\n" (bold "Default (when not set):");
            List.iter
              (fun line -> Printf.printf "    %s\n" (dim line))
              [
                "[Auto-summarized: id={sum_id}, tool={tool_name}, model={model}";
                " original: {orig_lines} lines / {orig_bytes} bytes / \
                 ~{orig_tokens} tokens";
                " summary: {sum_lines} lines / {sum_bytes} bytes / \
                 ~{sum_tokens} tokens";
                " at: {timestamp}]";
                "{summary}";
                "[Use unsummarize(summary_id=\"{sum_id}\") to retrieve \
                 original]";
              ];
            Printf.printf "\n";
            let current_default =
              match !sc.envelope_template with
              | None -> ""
              | Some t -> String_util.escape_newlines t
            in
            let rec get_template default =
              let s =
                Setup_common.prompt_string
                  ~prompt:"Envelope template (empty to clear)" ~default ()
              in
              let trimmed = String.trim s in
              if trimmed = "" then None
              else if not (String_util.contains trimmed "{summary}") then begin
                Setup_common.print_error
                  "Template must contain {summary} placeholder.";
                get_template trimmed
              end
              else Some (String_util.unescape_newlines trimmed)
            in
            let value = get_template current_default in
            sc := { !sc with envelope_template = value };
            dirty := true
        | "i" ->
            let instructions = post_setup_instructions ~sc:!sc in
            Printf.printf "%s" instructions;
            Setup_common.press_enter_to_continue ()
        | "s" when !dirty ->
            if save_summarizer_config ~sc:!sc then dirty := false;
            Setup_common.press_enter_to_continue ()
        | s ->
            Setup_common.print_warning (Printf.sprintf "Unknown option: %s" s);
            Setup_common.press_enter_to_continue ()
      done;
      if !dirty then "Exited with unsaved changes."
      else if !sc.summarizer_enabled then
        Printf.sprintf "Summarizer setup complete. Model: %s."
          (Pmodel.to_string !sc.summarizer_model)
      else "Summarizer setup complete (summarizer disabled)."
