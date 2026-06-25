(* tools_builtin_browser.ml — Browser automation tool *)

let action_enum =
  `List
    [
      `String "navigate";
      `String "click";
      `String "type";
      `String "screenshot";
      `String "content";
      `String "evaluate";
      `String "load_script";
      `String "list_scripts";
      `String "unload_script";
      `String "wait";
      `String "close";
      `String "new_tab";
      `String "switch_tab";
      `String "close_tab";
      `String "list_tabs";
      `String "navigate_and_extract";
      `String "fill_form";
      `String "snapshot_all";
      `String "run_script";
      `String "perform";
    ]

let parameters_schema =
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
                  ("enum", action_enum);
                  ("description", `String "Browser action to perform");
                ] );
            ( "actions",
              `Assoc
                [
                  ("type", `String "array");
                  ( "description",
                    `String
                      "Batch mode: array of action objects executed \
                       sequentially (stops on first error)" );
                  ("items", `Assoc [ ("type", `String "object") ]);
                ] );
            ( "url",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "URL for navigate/new_tab");
                ] );
            ( "selector",
              `Assoc
                [
                  ("type", `String "string");
                  ( "description",
                    `String
                      "CSS selector for click/type/wait/content/screenshot" );
                ] );
            ( "text",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "Text for type action");
                ] );
            ( "javascript",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "JS code for evaluate/load_script");
                ] );
            ( "autoload",
              `Assoc
                [
                  ("type", `String "boolean");
                  ( "description",
                    `String
                      "For load_script: persist across navigations (default \
                       true)" );
                ] );
            ( "name",
              `Assoc
                [
                  ("type", `String "string");
                  ( "description",
                    `String
                      "For load_script/unload_script: script label for \
                       management" );
                ] );
            ( "full_page",
              `Assoc
                [
                  ("type", `String "boolean");
                  ("description", `String "Full page screenshot (default true)");
                ] );
            ( "timeout",
              `Assoc
                [
                  ("type", `String "number");
                  ("description", `String "Timeout in seconds (default 30)");
                ] );
            ( "tab",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "Tab name (default: current tab)");
                ] );
            ( "fields",
              `Assoc
                [
                  ("type", `String "object");
                  ( "description",
                    `String "For fill_form: {selector: value} pairs" );
                  ("additionalProperties", `Assoc [ ("type", `String "string") ]);
                ] );
            ( "instructions",
              `Assoc
                [
                  ("type", `String "string");
                  ( "description",
                    `String "Natural language instructions for perform action"
                  );
                ] );
          ] );
      ("required", `List [ `String "action" ]);
    ]

let get_str key args =
  try Some (Yojson.Safe.Util.member key args |> Yojson.Safe.Util.to_string)
  with _ -> None

let get_bool key args default =
  try Yojson.Safe.Util.member key args |> Yojson.Safe.Util.to_bool
  with _ -> default

let get_float key args default =
  try Yojson.Safe.Util.member key args |> Yojson.Safe.Util.to_float
  with _ -> default

let ensure_browser ~config ~session_key =
  let configured_path = config.Runtime_config.browser.chromium_path in
  Cdp_client.get_or_launch ?configured_path ~session_key ()

let maybe_switch_tab browser args =
  match get_str "tab" args with
  | Some tab_name when tab_name <> browser.Cdp_client.current_page -> (
      match Cdp_client.switch_tab browser ~name:tab_name with
      | Ok () -> Ok ()
      | Error e -> Error e)
  | _ -> Ok ()

let execute_single_action ~config ~session_key args =
  let open Lwt.Syntax in
  let action = get_str "action" args in
  let timeout_s =
    get_float "timeout" args config.Runtime_config.browser.default_timeout_s
  in
  match action with
  | None ->
      Lwt.return
        "Error: 'action' parameter is required. Use one of: navigate, click, \
         type, screenshot, content, evaluate, load_script, list_scripts, \
         unload_script, wait, close, new_tab, switch_tab, close_tab, \
         list_tabs, navigate_and_extract, fill_form, snapshot_all, run_script, \
         perform."
  | Some "close" ->
      let* () = Cdp_client.close_session ~session_key () in
      Lwt.return "Browser session closed."
  | Some "list_tabs" -> (
      match Hashtbl.find_opt Cdp_client.pool session_key with
      | None -> Lwt.return "No active browser session."
      | Some browser ->
          let tabs = Cdp_client.list_tabs browser in
          let tab_strs =
            List.map
              (fun (name, _tid, is_current) ->
                Printf.sprintf "%s%s" name
                  (if is_current then " (current)" else ""))
              tabs
          in
          Lwt.return
            (Printf.sprintf "Open tabs:\n%s" (String.concat "\n" tab_strs)))
  | Some "list_scripts" -> (
      match Hashtbl.find_opt Cdp_client.pool session_key with
      | None -> Lwt.return "No active browser session."
      | Some browser ->
          let scripts = Cdp_client.list_scripts browser in
          if scripts = [] then Lwt.return "No autoloaded scripts."
          else
            let lines =
              List.map
                (fun (name, id) -> Printf.sprintf "  %s (id=%s)" name id)
                scripts
            in
            Lwt.return
              (Printf.sprintf "Autoloaded scripts:\n%s"
                 (String.concat "\n" lines)))
  | Some act -> (
      let* browser = ensure_browser ~config ~session_key in
      (* Maybe switch tab *)
      match maybe_switch_tab browser args with
      | Error e -> Lwt.return ("Error: " ^ e)
      | Ok () -> (
          match act with
          | "navigate" -> (
              match get_str "url" args with
              | None ->
                  Lwt.return
                    "Error: 'url' parameter is required for navigate. Example: \
                     browser(action=\"navigate\", url=\"https://example.com\")"
              | Some url ->
                  (* Reject non-http/https schemes *)
                  let is_safe_scheme =
                    let lower = String.lowercase_ascii url in
                    String.starts_with ~prefix:"http://" lower
                    || String.starts_with ~prefix:"https://" lower
                    || String.starts_with ~prefix:"data:" lower
                    || not (String.contains url ':')
                  in
                  if not is_safe_scheme then
                    Lwt.return
                      (Printf.sprintf
                         "Error: rejected URL scheme in %S. Only http://, \
                          https://, and data: URLs are allowed."
                         url)
                  else
                    let* () = Cdp_client.navigate browser ~url ~timeout_s () in
                    let* title_r =
                      Cdp_client.evaluate browser
                        ~expression:"document.title + ' - ' + location.href" ()
                    in
                    let title =
                      match title_r with Ok s -> s | Error _ -> url
                    in
                    Lwt.return (Printf.sprintf "Navigated to: %s" title))
          | "click" -> (
              match get_str "selector" args with
              | None ->
                  Lwt.return
                    "Error: 'selector' parameter is required for click. \
                     Example: browser(action=\"click\", \
                     selector=\"#submit-btn\")"
              | Some selector -> (
                  let* r = Cdp_client.click browser ~selector ~timeout_s () in
                  match r with
                  | Ok s -> Lwt.return (Printf.sprintf "Clicked: %s" s)
                  | Error e -> Lwt.return ("Error: " ^ e)))
          | "type" -> (
              match (get_str "selector" args, get_str "text" args) with
              | None, _ ->
                  Lwt.return "Error: 'selector' parameter is required for type."
              | _, None ->
                  Lwt.return "Error: 'text' parameter is required for type."
              | Some selector, Some text -> (
                  let* r =
                    Cdp_client.fill browser ~selector ~text ~timeout_s ()
                  in
                  match r with
                  | Ok _ ->
                      Lwt.return
                        (Printf.sprintf "Typed %S into %s" text selector)
                  | Error e -> Lwt.return ("Error: " ^ e)))
          | "screenshot" ->
              let selector = get_str "selector" args in
              let full_page = get_bool "full_page" args true in
              let* path =
                Cdp_client.screenshot browser ?selector ~full_page ~timeout_s ()
              in
              Lwt.return (Printf.sprintf "Screenshot saved: %s" path)
          | "content" ->
              let selector = get_str "selector" args in
              let* content =
                Cdp_client.get_content browser ?selector ~timeout_s ()
              in
              Lwt.return content
          | "evaluate" -> (
              match get_str "javascript" args with
              | None ->
                  Lwt.return
                    "Error: 'javascript' parameter is required for evaluate. \
                     Example: browser(action=\"evaluate\", \
                     javascript=\"document.title\")"
              | Some js -> (
                  let* r =
                    Cdp_client.evaluate browser ~expression:js ~timeout_s ()
                  in
                  match r with
                  | Ok s -> Lwt.return s
                  | Error e -> Lwt.return ("Error: " ^ e)))
          | "load_script" -> (
              match get_str "javascript" args with
              | None ->
                  Lwt.return
                    "Error: 'javascript' parameter is required for load_script."
              | Some source -> (
                  let name = get_str "name" args in
                  let autoload = get_bool "autoload" args true in
                  let* r =
                    Cdp_client.load_script browser ~source ?name ~autoload
                      ~timeout_s ()
                  in
                  match r with
                  | Ok s -> Lwt.return s
                  | Error e -> Lwt.return ("Error: " ^ e)))
          | "unload_script" -> (
              match get_str "name" args with
              | None ->
                  Lwt.return
                    "Error: 'name' parameter is required for unload_script."
              | Some name -> (
                  let* r =
                    Cdp_client.unload_script browser ~name ~timeout_s ()
                  in
                  match r with
                  | Ok s -> Lwt.return s
                  | Error e -> Lwt.return ("Error: " ^ e)))
          | "wait" -> (
              match get_str "selector" args with
              | None ->
                  Lwt.return "Error: 'selector' parameter is required for wait."
              | Some selector -> (
                  let wait_timeout = get_float "timeout" args timeout_s in
                  let* r =
                    Cdp_client.wait_for_selector browser ~selector
                      ~timeout_s:wait_timeout ()
                  in
                  match r with
                  | Ok () ->
                      Lwt.return (Printf.sprintf "Element %s found" selector)
                  | Error e -> Lwt.return ("Error: " ^ e)))
          | "new_tab" -> (
              match get_str "url" args with
              | None ->
                  Lwt.return "Error: 'url' parameter is required for new_tab."
              | Some url ->
                  let is_safe_scheme =
                    let lower = String.lowercase_ascii url in
                    String.starts_with ~prefix:"http://" lower
                    || String.starts_with ~prefix:"https://" lower
                    || String.starts_with ~prefix:"data:" lower
                    || not (String.contains url ':')
                  in
                  if not is_safe_scheme then
                    Lwt.return
                      (Printf.sprintf
                         "Error: rejected URL scheme in %S. Only http://, \
                          https://, and data: URLs are allowed."
                         url)
                  else
                    let name = get_str "tab" args in
                    let* tab_name =
                      Cdp_client.create_tab browser ?name ~url ~timeout_s ()
                    in
                    Lwt.return
                      (Printf.sprintf "Opened new tab %S with URL %s" tab_name
                         url))
          | "switch_tab" -> (
              match get_str "tab" args with
              | None ->
                  Lwt.return
                    "Error: 'tab' parameter is required for switch_tab."
              | Some name -> (
                  match Cdp_client.switch_tab browser ~name with
                  | Ok () ->
                      Lwt.return (Printf.sprintf "Switched to tab %S" name)
                  | Error e -> Lwt.return ("Error: " ^ e)))
          | "close_tab" -> (
              match get_str "tab" args with
              | None ->
                  Lwt.return "Error: 'tab' parameter is required for close_tab."
              | Some name -> (
                  let* r = Cdp_client.close_tab browser ~name ~timeout_s () in
                  match r with
                  | Ok () -> Lwt.return (Printf.sprintf "Tab %S closed" name)
                  | Error e -> Lwt.return ("Error: " ^ e)))
          (* Workflow actions *)
          | "navigate_and_extract" -> (
              match get_str "url" args with
              | None ->
                  Lwt.return
                    "Error: 'url' parameter is required for \
                     navigate_and_extract."
              | Some url ->
                  let is_safe_scheme =
                    let lower = String.lowercase_ascii url in
                    String.starts_with ~prefix:"http://" lower
                    || String.starts_with ~prefix:"https://" lower
                    || String.starts_with ~prefix:"data:" lower
                    || not (String.contains url ':')
                  in
                  if not is_safe_scheme then
                    Lwt.return
                      (Printf.sprintf
                         "Error: rejected URL scheme in %S. Only http://, \
                          https://, and data: URLs are allowed."
                         url)
                  else
                    let* () = Cdp_client.navigate browser ~url ~timeout_s () in
                    let* content =
                      Cdp_client.get_content browser ~timeout_s ()
                    in
                    Lwt.return content)
          | "fill_form" -> (
              let fields =
                try
                  Yojson.Safe.Util.member "fields" args
                  |> Yojson.Safe.Util.to_assoc
                  |> List.filter_map (fun (k, v) ->
                      try Some (k, Yojson.Safe.Util.to_string v)
                      with _ -> None)
                with _ -> []
              in
              match fields with
              | [] ->
                  Lwt.return
                    "Error: 'fields' parameter is required for fill_form. \
                     Example: browser(action=\"fill_form\", fields={\"#name\": \
                     \"John\", \"#email\": \"j@x.com\"})"
              | _ ->
                  let* results =
                    Lwt_list.map_s
                      (fun (selector, text) ->
                        let* r =
                          Cdp_client.fill browser ~selector ~text ~timeout_s ()
                        in
                        match r with
                        | Ok _ ->
                            Lwt.return
                              (Printf.sprintf "  %s = %S" selector text)
                        | Error e ->
                            Lwt.return
                              (Printf.sprintf "  %s: Error: %s" selector e))
                      fields
                  in
                  Lwt.return
                    (Printf.sprintf "Form filled:\n%s"
                       (String.concat "\n" results)))
          | "snapshot_all" -> (
              match get_str "url" args with
              | None ->
                  Lwt.return
                    "Error: 'url' parameter is required for snapshot_all."
              | Some url ->
                  let is_safe_scheme =
                    let lower = String.lowercase_ascii url in
                    String.starts_with ~prefix:"http://" lower
                    || String.starts_with ~prefix:"https://" lower
                    || String.starts_with ~prefix:"data:" lower
                    || not (String.contains url ':')
                  in
                  if not is_safe_scheme then
                    Lwt.return
                      (Printf.sprintf
                         "Error: rejected URL scheme in %S. Only http://, \
                          https://, and data: URLs are allowed."
                         url)
                  else
                    let* () = Cdp_client.navigate browser ~url ~timeout_s () in
                    let* path = Cdp_client.screenshot browser ~timeout_s () in
                    let* content =
                      Cdp_client.get_content browser ~timeout_s ()
                    in
                    Lwt.return
                      (Printf.sprintf "Screenshot: %s\n\nPage content:\n%s" path
                         content))
          | "run_script" -> (
              match (get_str "url" args, get_str "javascript" args) with
              | None, _ ->
                  Lwt.return
                    "Error: 'url' parameter is required for run_script."
              | _, None ->
                  Lwt.return
                    "Error: 'javascript' parameter is required for run_script."
              | Some url, Some js -> (
                  let is_safe_scheme =
                    let lower = String.lowercase_ascii url in
                    String.starts_with ~prefix:"http://" lower
                    || String.starts_with ~prefix:"https://" lower
                    || String.starts_with ~prefix:"data:" lower
                    || not (String.contains url ':')
                  in
                  if not is_safe_scheme then
                    Lwt.return
                      (Printf.sprintf
                         "Error: rejected URL scheme in %S. Only http://, \
                          https://, and data: URLs are allowed."
                         url)
                  else
                    let* () = Cdp_client.navigate browser ~url ~timeout_s () in
                    let* r =
                      Cdp_client.evaluate browser ~expression:js ~timeout_s ()
                    in
                    match r with
                    | Ok s -> Lwt.return s
                    | Error e -> Lwt.return ("Error: " ^ e)))
          | "perform" -> (
              match get_str "instructions" args with
              | None ->
                  Lwt.return
                    "Error: 'instructions' parameter is required for perform. \
                     Provide natural language instructions for what to do on \
                     the page."
              | Some instructions ->
                  let* result =
                    Browser_agent.execute_instruction ~config ~browser
                      ~instructions ()
                  in
                  Lwt.return (Browser_agent.format_result result))
          | other ->
              Lwt.return
                (Printf.sprintf
                   "Error: unknown action %S. Use one of: navigate, click, \
                    type, screenshot, content, evaluate, load_script, \
                    list_scripts, unload_script, wait, close, new_tab, \
                    switch_tab, close_tab, list_tabs, navigate_and_extract, \
                    fill_form, snapshot_all, run_script, perform."
                   other)))

let execute_batch ~config ~session_key actions_json =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let actions = try to_list actions_json with _ -> [] in
  let results = Buffer.create 256 in
  let rec run_actions i = function
    | [] -> Lwt.return_unit
    | action :: rest ->
        let* result = execute_single_action ~config ~session_key action in
        Buffer.add_string results
          (Printf.sprintf "Step %d: %s\n" (i + 1) result);
        let is_error =
          String.length result >= 6 && String.sub result 0 6 = "Error:"
        in
        if is_error then Lwt.return_unit else run_actions (i + 1) rest
  in
  let* () = run_actions 0 actions in
  Lwt.return (Buffer.contents results)

let browser ~workspace_only ~(config : Runtime_config.t) =
  {
    Tool.name = "browser";
    description =
      "Interact with web pages via a headless browser. The 'action' parameter \
       is required. Supports navigation, clicking, typing, screenshots, JS \
       evaluation, multi-tab management, and LLM-powered instruction \
       execution. Actions: navigate, click, type, screenshot, content, \
       evaluate, load_script, list_scripts, unload_script, wait, close, \
       new_tab, switch_tab, close_tab, list_tabs. Workflows: \
       navigate_and_extract, fill_form, snapshot_all, run_script, perform.";
    parameters_schema;
    invoke =
      (fun ?context args ->
        if workspace_only then
          Lwt.return
            "Error: browser tool is disabled in workspace-only mode. Set \
             security.workspace_only to false in config to enable browser \
             automation."
        else
          let session_key =
            match context with
            | Some ctx -> (
                match ctx.Tool.session_key with
                | Some k -> k
                | None -> "default")
            | None -> "default"
          in
          let has_batch =
            try
              match Yojson.Safe.Util.member "actions" args with
              | `Null -> false
              | `List _ -> true
              | _ -> false
            with _ -> false
          in
          if has_batch then
            let actions_json = Yojson.Safe.Util.member "actions" args in
            execute_batch ~config ~session_key actions_json
          else execute_single_action ~config ~session_key args);
    invoke_stream = None;
    risk_level = Tool.High;
    deferred = false;
  }
