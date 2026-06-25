(* setup_summarizer.ml — Interactive setup wizard for autosummarizer config *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_model s =
  let trimmed = String.trim s in
  match Pmodel.parse trimmed with Ok _ as ok -> ok | Error _ as err -> err

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
  match Setup_common.validate_positive_int s with
  | Error _ as err -> err
  | Ok s ->
      let n = int_of_string (String.trim s) in
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

(* ── JSON builder ────────────────────────────────────────────────── *)

let build_summarizer_json ~(sc : Runtime_config.summarizer_config) =
  let fields =
    [
      ("enabled", `Bool sc.enabled);
      ("model", `String (Pmodel.to_string sc.model));
      ( "escalation_model",
        match sc.escalation_model with
        | None -> `Null
        | Some m -> `String (Pmodel.to_string m) );
      ("threshold_chars", `Int sc.threshold_chars);
      ("p1_max_chars", `Int sc.p1_max_chars);
      ("p2_max_chars", `Int sc.p2_max_chars);
      ("context_window_messages", `Int sc.context_window_messages);
      ("excluded_tools", Setup_common.json_string_list sc.excluded_tools);
      ("max_age_days", `Int sc.max_age_days);
      ( "envelope_template",
        match sc.envelope_template with None -> `Null | Some t -> `String t );
    ]
  in
  Setup_common.build_section_json ~section_name:"summarizer" fields

(* ── Post-setup instructions ─────────────────────────────────────── *)

let post_setup_instructions ~(sc : Runtime_config.summarizer_config) =
  let model_str = Pmodel.to_string sc.model in
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
    (if sc.enabled then "yes" else "no")
    model_str esc_str sc.threshold_chars sc.p1_max_chars sc.p2_max_chars
    sc.context_window_messages sc.max_age_days

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  Setup_common.load_config_field_or
    ~default:Runtime_config.default_summarizer_config (fun cfg ->
      cfg.summarizer)

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let d = load_existing () in
  (* Mutable state for excluded_tools and envelope_template, managed by
     extra_actions rather than Setup_tui fields *)
  let excluded_tools_ref = ref d.excluded_tools in
  let envelope_template_ref = ref d.envelope_template in
  let enabled =
    Setup_tui.make_bool_field ~key:"e" ~label:"Enabled"
      ~menu_label:"Toggle enabled"
      ~description:"Enable or disable the autosummarizer." ~default:d.enabled ()
  in
  let model =
    Setup_tui.make_field ~key:"m" ~label:"Model" ~menu_label:"Set model"
      ~description:"Model for summarization (provider:model format)."
      ~validate:(fun s ->
        match validate_model s with
        | Ok m -> Ok (Pmodel.to_string m)
        | Error e -> Error e)
      ~default:(Pmodel.to_string d.model) ()
  in
  let escalation_model =
    Setup_tui.make_field ~key:"x" ~label:"Escalation Model"
      ~menu_label:"Set escalation model"
      ~description:
        "Fallback model for large summaries (provider:model). Leave empty for \
         none."
      ~validate:(fun s ->
        let trimmed = String.trim s in
        if trimmed = "" then Ok ""
        else
          match validate_model trimmed with
          | Ok m -> Ok (Pmodel.to_string m)
          | Error e -> Error e)
      ~default:
        (match d.escalation_model with
        | None -> ""
        | Some m -> Pmodel.to_string m)
      ()
  in
  let threshold_chars =
    Setup_tui.make_int_field ~key:"t" ~label:"Threshold (chars)"
      ~menu_label:"Set threshold chars"
      ~description:
        "Tool results exceeding this character count will be summarized."
      ~validate:(fun s ->
        match validate_threshold_chars s with
        | Ok (_, Some w) ->
            Setup_common.print_warning w;
            Ok s
        | Ok (_, None) -> Ok s
        | Error e -> Error e)
      ~default:d.threshold_chars ()
  in
  let p1_max_chars =
    Setup_tui.make_int_field ~key:"1" ~label:"P1 Max (chars)"
      ~menu_label:"Set P1 max chars"
      ~description:
        "Maximum characters for the primary (detailed) summary. Must be > P2 \
         max."
      ~validate:Setup_common.validate_positive_int ~default:d.p1_max_chars ()
  in
  let p2_max_chars =
    Setup_tui.make_int_field ~key:"2" ~label:"P2 Max (chars)"
      ~menu_label:"Set P2 max chars"
      ~description:
        "Maximum characters for the secondary (compact) summary. Must be < P1 \
         max."
      ~validate:Setup_common.validate_positive_int ~default:d.p2_max_chars ()
  in
  let context_window =
    Setup_tui.make_int_field ~key:"c" ~label:"Context Window (messages)"
      ~menu_label:"Set context window"
      ~description:
        "Number of recent messages to include as context for summarization."
      ~validate:(fun s ->
        match validate_non_negative_int s with
        | Ok _ -> Ok s
        | Error e -> Error e)
      ~default:d.context_window_messages ()
  in
  let max_age_days =
    Setup_tui.make_int_field ~key:"d" ~label:"Max Age (days)"
      ~menu_label:"Set max age days"
      ~description:"Maximum age in days for stored summaries."
      ~validate:Setup_common.validate_positive_int ~default:d.max_age_days ()
  in
  let add_excluded_tool () =
    let rec get_tool () =
      let s =
        Setup_common.prompt_string ~prompt:"Tool name to exclude" ~default:"" ()
      in
      match validate_tool_name s with
      | Ok name ->
          if List.mem name !excluded_tools_ref then
            Setup_common.print_warning
              (Printf.sprintf "%S is already excluded." name)
          else excluded_tools_ref := !excluded_tools_ref @ [ name ]
      | Error e ->
          Setup_common.print_warning e;
          get_tool ()
    in
    get_tool ()
  in
  let remove_excluded_tool () =
    if !excluded_tools_ref = [] then
      Setup_common.print_warning "No excluded tools to remove."
    else begin
      Printf.printf "\n  Excluded tools:\n";
      List.iteri
        (fun i name -> Printf.printf "    %d. %s\n" (i + 1) name)
        !excluded_tools_ref;
      Printf.printf "\n";
      let s =
        Setup_common.prompt_string
          ~prompt:"Number to remove (or empty to cancel)" ~default:"" ()
      in
      match int_of_string_opt (String.trim s) with
      | Some idx when idx >= 1 && idx <= List.length !excluded_tools_ref ->
          excluded_tools_ref :=
            List.filteri (fun i _ -> i <> idx - 1) !excluded_tools_ref
      | _ ->
          if String.trim s <> "" then
            Setup_common.print_warning "Invalid selection."
    end
  in
  let set_envelope_template () =
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
    (match !envelope_template_ref with
    | None -> Printf.printf "    %s\n" (dim "(using built-in default)")
    | Some t ->
        let lines = String.split_on_char '\n' t in
        List.iter (fun line -> Printf.printf "    %s\n" (cyan line)) lines);
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
      Summarizer.default_envelope_template_lines;
    Printf.printf "\n";
    let current_default =
      match !envelope_template_ref with
      | None -> ""
      | Some t -> String_util.escape_newlines t
    in
    let rec get_template default =
      let s =
        Setup_common.prompt_string ~prompt:"Envelope template (empty to clear)"
          ~default ()
      in
      let trimmed = String.trim s in
      if trimmed = "" then None
      else if not (String_util.contains trimmed "{summary}") then begin
        Setup_common.print_error "Template must contain {summary} placeholder.";
        get_template trimmed
      end
      else Some (String_util.unescape_newlines trimmed)
    in
    envelope_template_ref := get_template current_default
  in
  let build_sc () : Runtime_config.summarizer_config =
    let model_str = Setup_tui.get_str model in
    let esc_str = Setup_tui.get_str escalation_model in
    {
      enabled = Setup_tui.get_bool enabled;
      model =
        (match Pmodel.parse model_str with
        | Ok m -> m
        | Error _ ->
            Logs.warn (fun m ->
                m "H5: invalid summarizer model '%s', falling back to default"
                  model_str);
            d.model);
      escalation_model =
        (if esc_str = "" then None
         else
           match Pmodel.parse esc_str with
           | Ok m -> Some m
           | Error _ ->
               Logs.warn (fun m ->
                   m "H5: invalid summarizer escalation model '%s', ignoring"
                     esc_str);
               None);
      threshold_chars = Setup_tui.get_int threshold_chars;
      p1_max_chars = Setup_tui.get_int p1_max_chars;
      p2_max_chars = Setup_tui.get_int p2_max_chars;
      context_window_messages = Setup_tui.get_int context_window;
      excluded_tools = !excluded_tools_ref;
      max_age_days = Setup_tui.get_int max_age_days;
      envelope_template = !envelope_template_ref;
    }
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Autosummarizer Configuration ";
      docs_url = "https://clawq.org/configuration/#summarizer";
      fields =
        [
          enabled;
          model;
          escalation_model;
          threshold_chars;
          p1_max_chars;
          p2_max_chars;
          context_window;
          max_age_days;
        ];
      extra_actions =
        [
          ("a", "Add excluded tool", add_excluded_tool);
          ("r", "Remove excluded tool", remove_excluded_tool);
          ("v", "Set envelope template", set_envelope_template);
        ];
      build_json = (fun () -> build_summarizer_json ~sc:(build_sc ()));
      pre_save_check =
        (fun () ->
          let p1 = Setup_tui.get_int p1_max_chars in
          let p2 = Setup_tui.get_int p2_max_chars in
          if p1 <= p2 then
            Error
              (Printf.sprintf "P1 max (%d) must be greater than P2 max (%d)." p1
                 p2)
          else Ok ());
      post_instructions = (fun () -> post_setup_instructions ~sc:(build_sc ()));
    }
  in
  Setup_tui.run_wizard spec
