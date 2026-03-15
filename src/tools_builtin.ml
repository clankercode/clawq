include Tools_builtin_util
include Tools_builtin_io
include Tools_builtin_net

let generate_callback_id ~index ~label =
  let nonce = Printf.sprintf "%f_%d" (Unix.gettimeofday ()) (Random.bits ()) in
  let hash = Digest.to_hex (Digest.string (label ^ nonce)) in
  Printf.sprintf "cb_%d_%s" index (String.sub hash 0 8)

let send_message ~(send_fn : (text:string -> unit Lwt.t) option)
    ~(rich_send_fn :
       (session_key:string -> Rich_message.t -> Rich_message.send_result Lwt.t)
       option) =
  {
    Tool.name = "send_message";
    description =
      "Send a message to the user immediately via the current session, or via \
       the configured notification channel (Telegram, Discord, etc.) if no \
       session is active. Use when asked to notify, alert, or message the \
       user. Optionally include inline keyboard buttons for user choices.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "text",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Message text to send");
                    ] );
                ( "buttons",
                  `Assoc
                    [
                      ("type", `String "array");
                      ( "description",
                        `String
                          "Optional inline keyboard buttons. Each is an object \
                           with 'label' (display text). When clicked, the \
                           selected label is sent back as a user message." );
                      ( "items",
                        `Assoc
                          [
                            ("type", `String "object");
                            ( "properties",
                              `Assoc
                                [
                                  ( "label",
                                    `Assoc [ ("type", `String "string") ] );
                                ] );
                            ("required", `List [ `String "label" ]);
                          ] );
                    ] );
              ] );
          ("required", `List [ `String "text" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let text = try args |> member "text" |> to_string with _ -> "" in
        if text = "" then Lwt.return "Error: text is required"
        else
          let buttons =
            try
              args |> member "buttons" |> to_list
              |> List.map (fun b -> b |> member "label" |> to_string)
            with _ -> []
          in
          let session_key =
            match context with Some ctx -> ctx.Tool.session_key | None -> None
          in
          if buttons <> [] then
            let button_objs =
              List.mapi
                (fun i label ->
                  Rich_message.
                    {
                      label;
                      callback_id = generate_callback_id ~index:i ~label;
                    })
                buttons
            in
            let callback_ids =
              List.map
                (fun (b : Rich_message.button) -> b.callback_id)
                button_objs
            in
            (* v1: all buttons in a single row; multi-row layout not yet
               exposed in the tool schema *)
            let msg =
              Rich_message.TextWithButtons
                { text; button_rows = [ button_objs ] }
            in
            match (rich_send_fn, session_key) with
            | Some rsf, Some sk ->
                Lwt.catch
                  (fun () ->
                    let open Lwt.Syntax in
                    let* result = rsf ~session_key:sk msg in
                    let ids =
                      String.concat ", " result.Rich_message.callback_ids
                    in
                    Lwt.return
                      (Printf.sprintf
                         "Message sent with %d button(s). message_id=%s \
                          callback_ids=[%s]"
                         (List.length buttons) result.message_id ids))
                  (fun exn ->
                    Lwt.return
                      ("Error sending rich message: " ^ Printexc.to_string exn))
            | _ -> (
                (* Fallback: render buttons as text *)
                let fallback_text = Rich_message.to_fallback_text msg in
                match send_fn with
                | Some f ->
                    Lwt.catch
                      (fun () ->
                        let open Lwt.Syntax in
                        let* () = f ~text:fallback_text in
                        let ids = String.concat ", " callback_ids in
                        Lwt.return
                          (Printf.sprintf
                             "Message sent (buttons rendered as text). \
                              callback_ids=[%s]"
                             ids))
                      (fun exn ->
                        Lwt.return
                          ("Error sending message: " ^ Printexc.to_string exn))
                | None ->
                    Lwt.return
                      "Error: no active session notifier or configured \
                       notification channel.")
          else
            match (rich_send_fn, session_key) with
            | Some rsf, Some sk ->
                Lwt.catch
                  (fun () ->
                    let open Lwt.Syntax in
                    let* _result =
                      rsf ~session_key:sk (Rich_message.Text text)
                    in
                    Lwt.return "Message sent")
                  (fun exn ->
                    Lwt.return
                      ("Error sending message: " ^ Printexc.to_string exn))
            | _ -> (
                match send_fn with
                | None ->
                    Lwt.return
                      "Error: no active session notifier or configured \
                       notification channel."
                | Some f ->
                    Lwt.catch
                      (fun () ->
                        let open Lwt.Syntax in
                        let* () = f ~text in
                        Lwt.return "Message sent")
                      (fun exn ->
                        Lwt.return
                          ("Error sending message: " ^ Printexc.to_string exn))));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let send_poll
    ~(rich_send_fn :
       (session_key:string -> Rich_message.t -> Rich_message.send_result Lwt.t)
       option) ~(send_fn : (text:string -> unit Lwt.t) option) =
  {
    Tool.name = "send_poll";
    description =
      "Send a poll to the user via the current channel. On Telegram, this \
       creates a native poll; on other channels it renders as a text question. \
       The user's vote is routed back as a message.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "question",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "The poll question");
                    ] );
                ( "options",
                  `Assoc
                    [
                      ("type", `String "array");
                      ( "description",
                        `String "Poll options (2-10 items required)" );
                      ("items", `Assoc [ ("type", `String "string") ]);
                      ("minItems", `Int 2);
                      ("maxItems", `Int 10);
                    ] );
                ( "allows_multiple",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Whether users can select multiple options (default: \
                           false)" );
                    ] );
              ] );
          ("required", `List [ `String "question"; `String "options" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let question =
          try args |> member "question" |> to_string with _ -> ""
        in
        let options =
          try args |> member "options" |> to_list |> List.map to_string
          with _ -> []
        in
        let allows_multiple =
          try args |> member "allows_multiple" |> to_bool with _ -> false
        in
        if question = "" then Lwt.return "Error: question is required"
        else if List.length options < 2 then
          Lwt.return "Error: at least 2 options are required"
        else if List.length options > 10 then
          Lwt.return "Error: at most 10 options are allowed"
        else
          let session_key =
            match context with Some ctx -> ctx.Tool.session_key | None -> None
          in
          let msg = Rich_message.Poll { question; options; allows_multiple } in
          match (rich_send_fn, session_key) with
          | Some rsf, Some sk ->
              Lwt.catch
                (fun () ->
                  let open Lwt.Syntax in
                  let* result = rsf ~session_key:sk msg in
                  Lwt.return
                    (Printf.sprintf "Poll sent. message_id=%s"
                       result.Rich_message.message_id))
                (fun exn ->
                  Lwt.return ("Error sending poll: " ^ Printexc.to_string exn))
          | _ -> (
              let fallback_text = Rich_message.to_fallback_text msg in
              match send_fn with
              | Some f ->
                  Lwt.catch
                    (fun () ->
                      let open Lwt.Syntax in
                      let* () = f ~text:fallback_text in
                      Lwt.return "Poll sent (rendered as text)")
                    (fun exn ->
                      Lwt.return
                        ("Error sending poll: " ^ Printexc.to_string exn))
              | None ->
                  Lwt.return
                    "Error: no active session notifier or configured \
                     notification channel."));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let doc_write ~workspace ~workspace_files =
  let known_files = String.concat ", " workspace_files in
  {
    Tool.name = "doc_write";
    description =
      Printf.sprintf
        "Write or update a workspace document in the clawq workspace \
         directory. These documents persist across sessions and are injected \
         into the system prompt. Known effective files: %s. You may also \
         create new files but they will only appear in the prompt if added to \
         the workspace_files config."
        known_files;
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "filename",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          ("Filename to write (e.g. TOOLS.md, MEMORY.md). \
                            Known files: " ^ known_files) );
                    ] );
                ( "content",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Content to write");
                    ] );
                ( "append",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "If true, append to existing file instead of \
                           overwriting (default: false)" );
                    ] );
              ] );
          ("required", `List [ `String "filename"; `String "content" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let filename =
          try args |> member "filename" |> to_string with _ -> ""
        in
        let content =
          try args |> member "content" |> to_string with _ -> ""
        in
        let append = try args |> member "append" |> to_bool with _ -> false in
        if filename = "" then Lwt.return "Error: filename is required"
        else if not (Prompt_builder.safe_prompt_filename filename) then
          Lwt.return "Error: invalid filename (must not contain .., /, or \\)"
        else if content = "" then Lwt.return "Error: content is required"
        else
          let path = Filename.concat workspace filename in
          let is_known = List.mem filename workspace_files in
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let* () =
                if append then
                  let* existing =
                    Lwt.catch
                      (fun () ->
                        Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read)
                      (fun _ -> Lwt.return "")
                  in
                  Lwt_io.with_file ~mode:Lwt_io.Output path (fun oc ->
                      Lwt_io.write oc (existing ^ content))
                else
                  Lwt_io.with_file ~mode:Lwt_io.Output path (fun oc ->
                      Lwt_io.write oc content)
              in
              let action = if append then "Appended to" else "Written" in
              let note =
                if is_known then
                  " (active workspace file — will appear in system prompt)"
                else
                  " (not in workspace_files list — add to config for prompt \
                   injection)"
              in
              Lwt.return
                (Printf.sprintf "%s %d bytes to %s%s" action
                   (String.length content) path note))
            (fun exn ->
              Lwt.return ("Error writing document: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let compact_history ~compact_fn =
  {
    Tool.name = "compact_history";
    description =
      "Compact (summarize) older conversation history to free up context \
       window space. Useful for clearing out old patterns or lots of junk \
       data, and should be done proactively. The agent will be forked and the \
       clone asked to save everything necessary to memory, so compaction can \
       safely be run at any time without fear of losing unremembered things. \
       Returns token usage before and after compaction.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ("properties", `Assoc []);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context _args ->
        match context with
        | Some { Tool.session_key = Some key; _ } -> compact_fn ~session_key:key
        | _ ->
            Lwt.return
              "Error: compact_history requires a session context. This tool is \
               only available during daemon sessions.");
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let models_tool ~(config : Runtime_config.t) ?session_mgr () =
  let current_model () =
    Runtime_config.effective_primary_model config.agent_defaults
  in
  let set_model ?session_key model =
    match Models_catalog.find_by_full_name model with
    | Some _ -> (
        match session_mgr with
        | Some mgr -> (
            let provider, model_id, fmt = Models_catalog.split_name model in
            let hint =
              match fmt with
              | Models_catalog.Legacy ->
                  Printf.sprintf "\nHint: use %s:%s format instead of %s/%s."
                    provider model_id provider model_id
              | _ -> ""
            in
            let cfg = Session.get_config mgr in
            let provider_in_config = List.mem_assoc provider cfg.providers in
            let warn =
              if not provider_in_config then
                Printf.sprintf
                  "\n\
                   Warning: provider '%s' not found in config. Add it to your \
                   config.json to use this model."
                  provider
              else ""
            in
            match session_key with
            | Some key ->
                Session.set_session_model mgr ~key ~model;
                Printf.sprintf
                  "Model set to: %s (provider: %s)%s%s\n\
                   Persisted for this session across restarts. Use 'models \
                   set-default' to change the global default."
                  model_id provider hint warn
            | None ->
                "Error: session key not available; cannot set session model.")
        | None ->
            "Error: no active session available; session-scoped model changes \
             require a live session. Use the CLI 'models set-default' command \
             to change the persistent default.")
    | None ->
        Printf.sprintf
          "Error: model '%s' not found in catalog. Use 'models list' to see \
           available models. Format: provider:model-name (e.g., \
           openai:gpt-5.4)"
          model
  in
  {
    Tool.name = "models";
    description =
      "List available LLM models, get the current model, or set the model for \
       this session. Models are specified in provider:model format (e.g., \
       anthropic:claude-sonnet-4-6, openai:gpt-5.4). Use 'list' to discover \
       available models.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "action",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Action to perform: 'list' (show available models), \
                           'get' (show current model), or 'set' (change model)"
                      );
                      ( "enum",
                        `List [ `String "list"; `String "get"; `String "set" ]
                      );
                    ] );
                ( "model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Model name for 'set' action (provider:model format, \
                           e.g., openai:gpt-5.4)" );
                    ] );
                ( "provider",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Filter by provider for 'list' action (e.g., \
                           'openai', 'anthropic')" );
                    ] );
              ] );
          ("required", `List [ `String "action" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let action = try args |> member "action" |> to_string with _ -> "" in
        let session_key =
          match context with Some ctx -> ctx.Tool.session_key | None -> None
        in
        match action with
        | "list" ->
            let provider_filter =
              try Some (args |> member "provider" |> to_string) with _ -> None
            in
            Lwt.return (Models_catalog.to_plain_list ~provider_filter ())
        | "get" ->
            let model =
              match (session_mgr, session_key) with
              | Some mgr, Some key ->
                  Session.get_session_effective_model mgr ~key
              | _ -> current_model ()
            in
            Lwt.return (Printf.sprintf "Current model: %s" model)
        | "set" ->
            let model =
              try args |> member "model" |> to_string with _ -> ""
            in
            if model = "" then
              Lwt.return
                "Error: model parameter is required for 'set' action. Specify \
                 a model in provider:model format (e.g., openai:gpt-5.4). Use \
                 'models list' to see available models."
            else Lwt.return (set_model ?session_key model)
        | _ ->
            Lwt.return
              "Error: action must be 'list', 'get', or 'set'. Use 'list' to \
               see available models, 'get' to see the current model, or 'set' \
               to change the model.");
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let provider_usage_tool ~(config : Runtime_config.t) =
  Provider_quota.set_cache_ttl config.quota_cache_ttl_s;
  {
    Tool.name = "provider_usage";
    description =
      "Check quota and usage information for configured LLM providers. Shows \
       session, weekly, and monthly usage limits when available. Use 'list' to \
       see all providers, or 'get' with a provider name for details.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "action",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Action: 'list' (all providers) or 'get' (specific \
                           provider details)" );
                      ("enum", `List [ `String "list"; `String "get" ]);
                    ] );
                ( "provider",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Provider name for 'get' action (e.g., 'openai', \
                           'anthropic')" );
                    ] );
                ( "refresh",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Force refresh quota data from provider APIs \
                           (default: use cache if < 60s old)" );
                    ] );
              ] );
          ("required", `List [ `String "action" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Lwt.Syntax in
        let open Yojson.Safe.Util in
        let action = try args |> member "action" |> to_string with _ -> "" in
        let refresh =
          try args |> member "refresh" |> to_bool with _ -> false
        in
        let format_quota (name, pq) =
          let sess, week, mon =
            match pq.Provider_quota.state with
            | Provider_quota.Unknown msg -> (msg, "-", "-")
            | Provider_quota.Known { session; weekly; monthly } ->
                let fmt_pct = function
                  | None -> "-"
                  | Some w -> Printf.sprintf "%.0f%%" w.Provider_quota.used_pct
                in
                (fmt_pct session, fmt_pct weekly, fmt_pct monthly)
          in
          Printf.sprintf "%s\t%s\t%s\t%s" name sess week mon
        in
        match action with
        | "list" ->
            let* results =
              if refresh then
                let* refreshed = Provider_quota.refresh_all ~config () in
                Lwt.return
                  (List.map
                     (fun pq -> (pq.Provider_quota.provider_name, pq))
                     refreshed)
              else Lwt.return (Provider_quota.get_all_cached ())
            in
            if results = [] then
              if refresh then Lwt.return "No providers configured."
              else
                Lwt.return
                  "No cached quota data. Set refresh=true to fetch current \
                   data from provider APIs."
            else
              let header = "Provider\tSession\tWeekly\tMonthly" in
              let lines = List.map format_quota results in
              Lwt.return (String.concat "\n" (header :: lines))
        | "get" -> (
            let provider =
              try args |> member "provider" |> to_string with _ -> ""
            in
            if provider = "" then
              Lwt.return
                "Error: provider parameter is required for 'get' action. \
                 Specify a provider name (e.g., 'openai', 'anthropic'). Use \
                 'provider_usage list' to see available providers."
            else
              match Provider_quota.get_cached provider with
              | Some pq -> Lwt.return (Provider_quota.to_summary_string pq)
              | None ->
                  if refresh then
                    let* refreshed = Provider_quota.refresh_all ~config () in
                    let results =
                      List.map
                        (fun pq -> (pq.Provider_quota.provider_name, pq))
                        refreshed
                    in
                    match
                      List.find_opt (fun (n, _) -> n = provider) results
                    with
                    | Some (_, pq) ->
                        Lwt.return (Provider_quota.to_summary_string pq)
                    | None ->
                        Lwt.return
                          (Printf.sprintf
                             "Provider '%s' not found. Use 'provider_usage \
                              list' to see available providers."
                             provider)
                  else
                    Lwt.return
                      (Printf.sprintf
                         "No cached data for provider '%s'. Set refresh=true \
                          to fetch current data, or use 'provider_usage list' \
                          to see available providers."
                         provider))
        | _ ->
            Lwt.return
              "Error: action must be 'list' or 'get'. Use 'list' to see all \
               providers, or 'get' with a provider name for details.");
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

type question_type =
  | Single_select of { options : string list }
  | Multi_select of { options : string list }
  | Text of { placeholder : string option }
  | Number of { min : int option; max : int option }
  | Confirm
  | Rating of { min : int; max : int }
  | File_upload of { accept : string option }
  | Date of { include_time : bool }

type question_item = { question : string; qtype : question_type }

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
      { question; qtype })
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
            `Assoc (("question", `String qi.question) :: qtype_to_json qi.qtype))
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
                      "Array of questions to ask the user sequentially. Each \
                       question is sent one-at-a-time; the tool blocks until \
                       all are answered." );
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
                                      `String "The question text to display" );
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

let register_all ~(config : Runtime_config.t) ~sandbox ?(db = None)
    ?(send_fn = None) ?(rich_send_fn = None) ?(session_mgr = None) registry =
  let workspace_only = config.security.workspace_only in
  let workspace = Runtime_config.effective_workspace config in
  let extra_allowed_paths = config.security.extra_allowed_paths in
  Tool_registry.register registry
    (shell_exec ~workspace ~workspace_only
       ~allowed_commands:default_shell_allowlist ~extra_allowed_paths ~sandbox);
  Tool_registry.register registry
    (file_read ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (file_write ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (file_append ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (file_edit ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (file_edit_lines ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry (http_get ~workspace_only);
  Tool_registry.register registry
    (glob ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (list_dir ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (grep ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry (http_request ~workspace_only);
  Tool_registry.register registry (web_fetch ~workspace_only);
  List.iter (Tool_registry.register registry) (bg_shell_tools ());
  Tool_registry.register registry (git_operations ~workspace);
  if config.web_search <> None then
    Tool_registry.register registry (web_search ~config);
  (match config.zai_mcp with
  | Some cfg when cfg.websearch_enabled ->
      Tool_registry.register registry (zai_websearch ~config)
  | _ -> ());
  (match config.zai_mcp with
  | Some cfg when cfg.webfetch_enabled ->
      Tool_registry.register registry (zai_webfetch ~config)
  | _ -> ());
  Tool_registry.register registry (models_tool ~config ?session_mgr ());
  Tool_registry.register registry (provider_usage_tool ~config);
  (match send_fn with
  | Some _ ->
      Tool_registry.register registry (send_message ~send_fn ~rich_send_fn);
      Tool_registry.register registry (send_poll ~rich_send_fn ~send_fn)
  | None -> ());
  if config.stt <> None then
    Tool_registry.register registry (transcribe ~config);
  Tool_registry.register registry
    (doc_write ~workspace ~workspace_files:config.prompt.workspace_files);
  match db with
  | Some db ->
      Tool_registry.register registry (memory_store ~db);
      Tool_registry.register registry (memory_recall ~db);
      Tool_registry.register registry (memory_forget ~db);
      Tool_registry.register registry (memory_list ~db);
      Tool_registry.register registry (history_search ~db);
      Tool_registry.register registry (thread_summary ~db ~config);
      Tool_registry.register registry (unsummarize ~db);
      Background_task.init_schema db;
      Task_tree.init_schema db;
      Plan_pipeline.init_schema db;
      Tool_registry.register registry (Task_tree.tool ~db ());
      Tool_registry.register registry
        (Background_task.enqueue_tool_with_notify ~notify_cfg:config.notify ~db);
      Tool_registry.register registry (Background_task.list_tool ~db);
      Tool_registry.register registry (Background_task.wait_tool ~db);
      Tool_registry.register registry (Background_task.logs_tool ~db);
      Tool_registry.register registry (Background_task.resume_tool ~db);
      Tool_registry.register registry (Background_task.message_tool ~db);
      Tool_registry.register registry
        (Background_task.delegate_tool_with_notify ~db
           ~default_repo_path:workspace ~notify_cfg:config.notify ());
      Tool_registry.register registry (Background_task.cancel_tool ~db);
      Tool_registry.register registry (Background_task.recover_tool ~db);
      Tool_registry.register registry (Worktree_merge.finalize_tool ~db);
      Tool_registry.register registry
        (Plan_pipeline.start_tool ~db ~default_repo_path:workspace);
      Tool_registry.register registry (Plan_pipeline.status_tool ~db);
      Tool_registry.register registry (Plan_pipeline.list_tool ~db);
      Tool_registry.register registry (Plan_pipeline.logs_tool ~db);
      Tool_registry.register registry (Plan_pipeline.cancel_tool ~db)
  | None -> ()
