(* Messaging tools: send_message, send_poll, send_file.

   Split out of tools_builtin.ml. References to path helpers are fully
   qualified (Tools_builtin_io / Tools_builtin_util) since this module does
   not [include] those modules. *)

let generate_callback_id ~index ~label =
  let nonce = Printf.sprintf "%d_%d" (Random.bits ()) (Random.bits ()) in
  let hash = Digest.to_hex (Digest.string (label ^ nonce)) in
  Printf.sprintf "cb_%d_%s" index (String.sub hash 0 8)

let send_message ~(send_fn : (text:string -> unit Lwt.t) option)
    ~(rich_send_fn :
       (session_key:string -> Rich_message.t -> Rich_message.send_result Lwt.t)
       option) =
  let schema =
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
                    ("description", `String "Message text to send (required)");
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
                                  `Assoc
                                    [
                                      ("type", `String "string");
                                      ( "description",
                                        `String "Button display text (required)"
                                      );
                                    ] );
                              ] );
                          ("required", `List [ `String "label" ]);
                        ] );
                  ] );
            ] );
        ("required", `List [ `String "text" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"send_message" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "send_message";
    description =
      "Send a message to the user immediately via the current session, or via \
       the configured notification channel (Telegram, Discord, etc.) if no \
       session is active. Use when asked to notify, alert, or message the \
       user. Optionally include inline keyboard buttons for user choices.";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let text = try args |> member "text" |> to_string with _ -> "" in
        if text = "" then
          Lwt.return (param_err "parameter 'text' must be a non-empty string")
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
                          callback_ids=[%s]. Delivered to the user — do NOT \
                          repeat the same information in the assistant reply; \
                          await a button selection or next user message."
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
                              callback_ids=[%s]. Delivered to the user — do \
                              NOT repeat the same information in the assistant \
                              reply."
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
                    Lwt.return
                      "Message sent. Delivered to the user — do NOT repeat the \
                       same information in the assistant reply.")
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
                        Lwt.return
                          "Message sent. Delivered to the user — do NOT repeat \
                           the same information in the assistant reply.")
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
  let question_param =
    Tool_param.required ~name:"question"
      ~description:"The poll question (required)"
      (Tool_param.string ~non_empty:true ())
  in
  let options_param =
    Tool_param.required ~name:"options"
      ~description:"Poll options, 2-10 items (required)"
      (Tool_param.string_array ~min_items:2 ~max_items:10 ())
  in
  let allows_multiple_param =
    Tool_param.defaulted ~on_invalid:`Use_default ~name:"allows_multiple"
      ~description:"Whether users can select multiple options (default: false)"
      ~default:false Tool_param.boolean
  in
  let schema =
    Tool_param.object_schema
      [
        Tool_param.pack question_param;
        Tool_param.pack options_param;
        Tool_param.pack allows_multiple_param;
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"send_poll" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "send_poll";
    description =
      "Send a poll to the user via the current channel. On Telegram, this \
       creates a native poll; on other channels it renders as a text question. \
       The user's vote is routed back as a message.";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        match
          ( Tool_param.parse question_param args,
            Tool_param.parse options_param args,
            Tool_param.parse allows_multiple_param args )
        with
        | Error detail, _, _ | _, Error detail, _ | _, _, Error detail ->
            Lwt.return (param_err detail)
        | Ok question, Ok options, Ok allows_multiple -> (
            let session_key =
              match context with
              | Some ctx -> ctx.Tool.session_key
              | None -> None
            in
            let msg =
              Rich_message.Poll { question; options; allows_multiple }
            in
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
                       notification channel.")));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let guess_content_type filename =
  let ext =
    match Filename.extension filename with
    | "" -> ""
    | e -> String.lowercase_ascii (String.sub e 1 (String.length e - 1))
  in
  match ext with
  | "txt" -> "text/plain"
  | "json" -> "application/json"
  | "csv" -> "text/csv"
  | "html" | "htm" -> "text/html"
  | "xml" -> "application/xml"
  | "pdf" -> "application/pdf"
  | "png" -> "image/png"
  | "jpg" | "jpeg" -> "image/jpeg"
  | "gif" -> "image/gif"
  | "svg" -> "image/svg+xml"
  | "webp" -> "image/webp"
  | "zip" -> "application/zip"
  | "gz" | "gzip" -> "application/gzip"
  | "tar" -> "application/x-tar"
  | "md" -> "text/markdown"
  | "py" -> "text/x-python"
  | "ml" | "mli" -> "text/x-ocaml"
  | "js" -> "text/javascript"
  | "ts" -> "text/typescript"
  | "css" -> "text/css"
  | "yaml" | "yml" -> "text/yaml"
  | "toml" -> "application/toml"
  | "sh" -> "text/x-shellscript"
  | "sql" -> "application/sql"
  | "log" -> "text/plain"
  | _ -> "application/octet-stream"

let send_file ~workspace ~workspace_only ~extra_allowed_paths
    ~(send_fn : (text:string -> unit Lwt.t) option)
    ~(rich_send_fn :
       (session_key:string -> Rich_message.t -> Rich_message.send_result Lwt.t)
       option)
    ~(store_file :
       (content:string ->
       content_type:string ->
       filename:string ->
       string option)
       option) =
  {
    Tool.name = "send_file";
    description =
      "Send a file to the user via the current channel. On Telegram, the file \
       is uploaded natively; on other channels a download link is sent. A \
       download link is always sent as a separate message. Provide either a \
       workspace file path or inline content.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Path to a workspace file to send. Mutually \
                           exclusive with 'content'." );
                    ] );
                ( "content",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Inline file content to send. Mutually exclusive \
                           with 'path'. Requires 'filename'." );
                    ] );
                ( "filename",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Filename for the file. Required with 'content', \
                           optional with 'path' (defaults to basename)." );
                    ] );
                ( "content_type",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "MIME type (e.g., 'text/plain', 'application/pdf'). \
                           Auto-detected from extension if omitted." );
                    ] );
                ( "description",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Description shown to the user with the file."
                      );
                    ] );
              ] );
          (* No required fields: the (path | content) constraint is
             enforced at runtime since JSON Schema cannot express "exactly
             one of these". *)
          ("required", `List []);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path =
          try
            match args |> member "path" with
            | `Null -> None
            | v -> Some (to_string v)
          with _ -> None
        in
        let inline_content =
          try
            match args |> member "content" with
            | `Null -> None
            | v -> Some (to_string v)
          with _ -> None
        in
        let explicit_filename =
          try
            match args |> member "filename" with
            | `Null -> None
            | v -> Some (to_string v)
          with _ -> None
        in
        let content_type_param =
          try
            match args |> member "content_type" with
            | `Null -> None
            | v -> Some (to_string v)
          with _ -> None
        in
        let description =
          try
            match args |> member "description" with
            | `Null -> ""
            | v -> to_string v
          with _ -> ""
        in
        match (path, inline_content) with
        | None, None ->
            Lwt.return
              "Error: either 'path' or 'content' is required. Use 'path' to \
               send an existing workspace file, or 'content' to send inline \
               data (requires 'filename')."
        | Some _, Some _ ->
            Lwt.return
              "Error: 'path' and 'content' are mutually exclusive. Provide \
               exactly one: 'path' for an existing file, or 'content' for \
               inline data."
        | Some file_path, None -> (
            let resolved =
              if Filename.is_relative file_path then
                let cwd =
                  match context with
                  | Some ctx -> (
                      match ctx.Tool.effective_cwd with
                      | Some d -> d
                      | None -> workspace)
                  | None -> workspace
                in
                Filename.concat cwd file_path
              else file_path
            in
            if
              workspace_only
              && not
                   (Tools_builtin_io.is_path_allowed ~workspace ~workspace_only
                      ~extra_allowed_paths resolved)
            then
              Lwt.return
                "Error: path is outside workspace. Use an absolute path within \
                 the workspace, or check 'extra_allowed_paths' in config."
            else
              match Tools_builtin_util.canonicalize_for_read resolved with
              | Error msg -> Lwt.return msg
              | Ok canonical_path ->
                  if
                    workspace_only
                    && not
                         (Tools_builtin_io.is_path_allowed ~workspace
                            ~workspace_only ~extra_allowed_paths canonical_path)
                  then
                    Lwt.return
                      "Error: path resolves outside workspace. The resolved \
                       path is outside the allowed workspace boundaries."
                  else
                    Lwt.catch
                      (fun () ->
                        let open Lwt.Syntax in
                        let* content =
                          Lwt_io.with_file ~mode:Lwt_io.Input canonical_path
                            Lwt_io.read
                        in
                        let filename =
                          match explicit_filename with
                          | Some f -> f
                          | None -> Filename.basename canonical_path
                        in
                        let content_type =
                          match content_type_param with
                          | Some ct -> ct
                          | None -> guess_content_type filename
                        in
                        let size = String.length content in
                        match store_file with
                        | None ->
                            Lwt.return
                              "Error: no public base URL configured (tunnel \
                               not active). A download link cannot be \
                               generated. Configure a tunnel or set \
                               'public_base_url' to enable file sending."
                        | Some store_fn -> (
                            let download_url =
                              store_fn ~content ~content_type ~filename
                            in
                            match download_url with
                            | None ->
                                Lwt.return
                                  "Error: no public base URL configured \
                                   (tunnel not active). A download link cannot \
                                   be generated. Configure a tunnel or set \
                                   'public_base_url' to enable file sending."
                            | Some _ ->
                                let attachment =
                                  Rich_message.FileAttachment
                                    {
                                      filename;
                                      content;
                                      content_type;
                                      description;
                                      download_url;
                                    }
                                in
                                let session_key =
                                  match context with
                                  | Some ctx -> ctx.Tool.session_key
                                  | None -> None
                                in
                                let* upload_warning =
                                  match (rich_send_fn, session_key) with
                                  | Some rsf, Some sk ->
                                      Lwt.catch
                                        (fun () ->
                                          let* _result =
                                            rsf ~session_key:sk attachment
                                          in
                                          Lwt.return "")
                                        (fun exn ->
                                          Lwt.return
                                            (Printf.sprintf
                                               " (native upload failed: %s)"
                                               (Printexc.to_string exn)))
                                  | _ -> Lwt.return ""
                                in
                                let* () =
                                  match (send_fn, download_url) with
                                  | Some f, Some url ->
                                      let desc =
                                        if description <> "" then description
                                        else filename
                                      in
                                      f ~text:(desc ^ "\n\nDownload: " ^ url)
                                  | _ -> Lwt.return_unit
                                in
                                let url_str =
                                  match download_url with
                                  | Some u -> u
                                  | None -> "(no URL)"
                                in
                                Lwt.return
                                  (Printf.sprintf
                                     "File sent: %s (%d bytes). Download: %s%s"
                                     filename size url_str upload_warning)))
                      (fun exn ->
                        Lwt.return
                          (Printf.sprintf
                             "Error: could not read file '%s': %s. Check that \
                              the path exists and is readable."
                             file_path (Printexc.to_string exn))))
        | None, Some content -> (
            match explicit_filename with
            | None | Some "" ->
                Lwt.return
                  "Error: 'filename' is required when using 'content'. Specify \
                   a filename (e.g., 'report.csv') so the recipient knows what \
                   the file is."
            | Some filename -> (
                let content_type =
                  match content_type_param with
                  | Some ct -> ct
                  | None -> guess_content_type filename
                in
                let size = String.length content in
                match store_file with
                | None ->
                    Lwt.return
                      "Error: no public base URL configured (tunnel not \
                       active). A download link cannot be generated. Configure \
                       a tunnel or set 'public_base_url' to enable file \
                       sending."
                | Some store_fn -> (
                    let download_url =
                      store_fn ~content ~content_type ~filename
                    in
                    match download_url with
                    | None ->
                        Lwt.return
                          "Error: no public base URL configured (tunnel not \
                           active). A download link cannot be generated. \
                           Configure a tunnel or set 'public_base_url' to \
                           enable file sending."
                    | Some _ ->
                        let attachment =
                          Rich_message.FileAttachment
                            {
                              filename;
                              content;
                              content_type;
                              description;
                              download_url;
                            }
                        in
                        let session_key =
                          match context with
                          | Some ctx -> ctx.Tool.session_key
                          | None -> None
                        in
                        let open Lwt.Syntax in
                        let* upload_warning =
                          match (rich_send_fn, session_key) with
                          | Some rsf, Some sk ->
                              Lwt.catch
                                (fun () ->
                                  let* _result =
                                    rsf ~session_key:sk attachment
                                  in
                                  Lwt.return "")
                                (fun exn ->
                                  Lwt.return
                                    (Printf.sprintf
                                       " (native upload failed: %s)"
                                       (Printexc.to_string exn)))
                          | _ -> Lwt.return ""
                        in
                        let* () =
                          match (send_fn, download_url) with
                          | Some f, Some url ->
                              let desc =
                                if description <> "" then description
                                else filename
                              in
                              f ~text:(desc ^ "\n\nDownload: " ^ url)
                          | _ -> Lwt.return_unit
                        in
                        let url_str =
                          match download_url with
                          | Some u -> u
                          | None -> "(no URL)"
                        in
                        Lwt.return
                          (Printf.sprintf
                             "File sent: %s (%d bytes). Download: %s%s" filename
                             size url_str upload_warning)))));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }
