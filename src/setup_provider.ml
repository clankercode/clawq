(* setup_provider.ml — Setup wizard for AI provider configuration *)

(* ── Pure validation functions (tested) ──────────────────────────── *)

let validate_provider_name s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "Provider name cannot be empty."
  else
    let valid =
      String.to_seq trimmed
      |> Seq.for_all (fun c ->
          (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_' || c = '-')
    in
    if valid then Ok trimmed
    else
      Error
        "Provider name must contain only lowercase letters, digits, hyphens, \
         and underscores."

let validate_api_key s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "API key cannot be empty." else Ok trimmed

(* ── JSON builder ────────────────────────────────────────────────── *)

let build_provider_json ~name ~api_key ~base_url ~default_model =
  let fields =
    [ ("api_key", `String api_key) ]
    @ (if base_url = "" then [] else [ ("base_url", `String base_url) ])
    @
    if default_model = "" then []
    else [ ("default_model", `String default_model) ]
  in
  `Assoc [ ("providers", `Assoc [ (name, `Assoc fields) ]) ]

(* ── Post-setup instructions ─────────────────────────────────────── *)

let post_setup_instructions =
  {|
  Provider Setup Instructions
  ===========================

  Common providers and their API key sources:

    openai-codex   https://platform.openai.com/api-keys
    anthropic      https://console.anthropic.com/settings/keys
    ollama         (no key needed; set base_url to http://localhost:11434)
    cohere         https://dashboard.cohere.com/api-keys
    gemini         https://aistudio.google.com/apikey
    mistral        https://console.mistral.ai/api-keys
    groq           https://console.groq.com/keys
    openrouter     https://openrouter.ai/keys

  After adding a provider, set it as your default model:
    clawq models set-default <provider>:<model>

  For OpenAI-compatible endpoints (Ollama, LM Studio, etc.):
    Set base_url to the server's base URL (e.g. http://localhost:11434)

  Full documentation: https://clawq.org/providers/
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing_providers () =
  Setup_common.load_config_field_or ~default:[] (fun cfg -> cfg.providers)

(* ── Draw provider list ──────────────────────────────────────────── *)

let draw_provider_list providers =
  let open Setup_common in
  let w = terminal_width () in
  clear_screen ();
  Printf.printf "\n";
  let lines =
    [ bold " AI Provider Configuration "; "" ]
    @ (if providers = [] then [ dim "  (no providers configured)" ]
       else
         List.mapi
           (fun i (name, (pc : Runtime_config.provider_config)) ->
             let key_str =
               if pc.api_key = "" then dim "(no key)"
               else green (Tui_input.redact pc.api_key)
             in
             let url_str =
               match pc.base_url with
               | None -> ""
               | Some u -> Printf.sprintf "  [%s]" (dim u)
             in
             Printf.sprintf "  %s  %s  %s%s"
               (cyan (string_of_int (i + 1)))
               (bold name) key_str url_str)
           providers)
    @ [ "" ]
  in
  draw_box ~width:w lines;
  print_docs_link "https://clawq.org/providers/";
  Printf.printf "\n";
  draw_separator ~width:w

(* ── Prompt for a single provider ───────────────────────────────── *)

let prompt_add_provider () =
  let open Setup_common in
  Printf.printf "\n";
  Printf.printf "  %s\n\n"
    (bold "Add / update a provider (e.g. openai-codex, anthropic, ollama)");
  let rec get_name () =
    let s = prompt_string ~prompt:"Provider name" ~default:"" () in
    match validate_provider_name s with
    | Ok n -> n
    | Error e ->
        print_warning e;
        get_name ()
  in
  let name = get_name () in
  let api_key =
    match prompt_secret ~prompt:"API key (Enter to skip)" () with
    | Ok s -> ( match validate_api_key s with Ok k -> k | Error _ -> "")
    | Error _ -> ""
  in
  let base_url =
    let rec get_url () =
      let s =
        prompt_string ~prompt:"Base URL (empty for default)" ~default:"" ()
      in
      match Setup_common.validate_url s with
      | Ok u -> u
      | Error e ->
          print_warning e;
          get_url ()
    in
    get_url ()
  in
  let default_model =
    prompt_string ~prompt:"Default model (empty to skip)" ~default:"" ()
  in
  (name, api_key, base_url, default_model)

(* ── Save helper ─────────────────────────────────────────────────── *)

let save_providers providers =
  let provider_assoc =
    List.map
      (fun (name, (pc : Runtime_config.provider_config)) ->
        let fields =
          [ ("api_key", `String pc.api_key) ]
          @ (match pc.base_url with
            | Some u -> [ ("base_url", `String u) ]
            | None -> [])
          @
          match pc.default_model with
          | Some m -> [ ("default_model", `String m) ]
          | None -> []
        in
        (name, `Assoc fields))
      providers
  in
  let new_providers = `Assoc [ ("providers", `Assoc provider_assoc) ] in
  let cp = Setup_common.config_path () in
  Setup_common.ensure_config_dir ();
  let final =
    if Sys.file_exists cp then
      try
        let existing = Yojson.Safe.from_file cp in
        (* Replace providers entirely, keep everything else *)
        let without_providers =
          match existing with
          | `Assoc fields ->
              `Assoc (List.filter (fun (k, _) -> k <> "providers") fields)
          | other -> other
        in
        Setup_common.deep_merge_json without_providers new_providers
      with _ -> new_providers
    else new_providers
  in
  match Setup_common.write_json_file cp final with
  | Ok path ->
      Setup_common.print_success (Printf.sprintf "Saved to %s" path);
      true
  | Error e ->
      Setup_common.print_error (Printf.sprintf "Failed to write config: %s" e);
      false

(* ── Main menu loop ──────────────────────────────────────────────── *)

let run () =
  match Setup_common.check_tty () with
  | Error e -> e
  | Ok () ->
      let providers = ref (load_existing_providers ()) in
      let dirty = ref false in
      let quit = ref false in
      while not !quit do
        draw_provider_list !providers;
        let options =
          [ ("a", "Add / update provider") ]
          @ (if !providers <> [] then [ ("r", "Remove provider") ] else [])
          @ [ ("h", "Show setup instructions") ]
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
                ignore (save_providers !providers);
                quit := true
              end
              else quit := true
            end
            else quit := true
        | "a" ->
            let name, api_key, base_url, default_model =
              prompt_add_provider ()
            in
            let pc =
              {
                Runtime_config.default_provider_config with
                api_key;
                base_url = (if base_url = "" then None else Some base_url);
                default_model =
                  (if default_model = "" then None else Some default_model);
              }
            in
            (* Replace if exists, else append *)
            let existing_names = List.map fst !providers in
            if List.mem name existing_names then
              providers :=
                List.map
                  (fun (n, p) -> if n = name then (n, pc) else (n, p))
                  !providers
            else providers := !providers @ [ (name, pc) ];
            dirty := true;
            Setup_common.press_enter_to_continue ()
        | "r" ->
            if !providers = [] then
              Setup_common.print_warning "No providers to remove."
            else begin
              Printf.printf "\n  Providers:\n";
              List.iteri
                (fun i (name, _) ->
                  Printf.printf "    %s. %s\n"
                    (Setup_common.cyan (string_of_int (i + 1)))
                    name)
                !providers;
              Printf.printf "\n";
              let s =
                Setup_common.prompt_string
                  ~prompt:"Number to remove (or empty to cancel)" ~default:"" ()
              in
              (match int_of_string_opt (String.trim s) with
              | Some idx when idx >= 1 && idx <= List.length !providers ->
                  let name = fst (List.nth !providers (idx - 1)) in
                  let confirm =
                    Setup_common.prompt_yn
                      ~prompt:
                        (Printf.sprintf
                           "Remove provider '%s'? This cannot be undone." name)
                      ~default:false ()
                  in
                  if confirm then (
                    providers :=
                      List.filteri (fun i _ -> i <> idx - 1) !providers;
                    dirty := true;
                    Setup_common.print_success
                      (Printf.sprintf "Removed provider '%s'." name))
                  else Setup_common.print_warning "Cancelled."
              | _ ->
                  if String.trim s <> "" then
                    Setup_common.print_warning "Invalid selection.");
              Setup_common.press_enter_to_continue ()
            end
        | "h" ->
            Printf.printf "%s" post_setup_instructions;
            Setup_common.press_enter_to_continue ()
        | "s" when !dirty ->
            if save_providers !providers then dirty := false;
            Setup_common.press_enter_to_continue ()
        | s ->
            Setup_common.print_warning (Printf.sprintf "Unknown option: %s" s);
            Setup_common.press_enter_to_continue ()
      done;
      if !dirty then "Exited with unsaved changes."
      else "Provider setup complete."
