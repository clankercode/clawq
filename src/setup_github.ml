(* setup_github.ml — Interactive setup wizard for GitHub webhook integration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_repo_name s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "Repository name cannot be empty."
  else
    match String.split_on_char '/' trimmed with
    | [ owner; repo ] when owner <> "" && repo <> "" -> Ok trimmed
    | _ -> Error "Repository name must be in 'owner/repo' format."

let pat_has_known_prefix s =
  String.length s >= 4
  && (String.sub s 0 4 = "ghp_"
     || (String.length s >= 11 && String.sub s 0 11 = "github_pat_"))

let validate_pat s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "PAT cannot be empty." else Ok trimmed

let default_webhook_path repo_name =
  match String.split_on_char '/' repo_name with
  | [ _owner; repo ] -> "/github/webhook/" ^ repo
  | _ -> "/github/webhook/default"

let repo_to_json (r : Runtime_config.github_repo_config) =
  let fields =
    [
      ("name", `String r.name);
      ("webhook_secret", `String r.webhook_secret);
      ("webhook_path", `String r.webhook_path);
      ("allow_users", `List (List.map (fun s -> `String s) r.allow_users));
      ("react_to", `List (List.map (fun s -> `String s) r.react_to));
      ("include_pr_files", `Bool r.include_pr_files);
    ]
  in
  let fields =
    match r.agent_name with
    | Some name -> fields @ [ ("agent_name", `String name) ]
    | None -> fields
  in
  `Assoc fields

let build_full_github_json ~pat_token ~repos =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "github",
              `Assoc
                [
                  ( "auth",
                    `Assoc
                      [ ("type", `String "pat"); ("token", `String pat_token) ]
                  );
                  ("repos", `List (List.map repo_to_json repos));
                ] );
          ] );
    ]

let build_github_json ~pat_token ~repo_name ~webhook_secret ~webhook_path
    ~react_to ~allow_users ~include_pr_files ~agent_name =
  let repo : Runtime_config.github_repo_config =
    {
      name = repo_name;
      webhook_secret;
      webhook_path;
      agent_name;
      allow_users;
      react_to;
      include_pr_files;
    }
  in
  build_full_github_json ~pat_token ~repos:[ repo ]

let post_setup_instructions ~repo_name ~webhook_path ~webhook_secret
    ~gateway_port ~tunnel_url =
  let settings_url =
    match String.split_on_char '/' repo_name with
    | [ owner; repo ] ->
        Printf.sprintf "https://github.com/%s/%s/settings/hooks/new" owner repo
    | _ -> "https://github.com/<owner>/<repo>/settings/hooks/new"
  in
  let base_url =
    match tunnel_url with
    | Some url -> url
    | None -> Printf.sprintf "http://localhost:%d" gateway_port
  in
  let webhook_url = base_url ^ webhook_path in
  let tunnel_note =
    match tunnel_url with
    | None ->
        "\n\
        \    Note: You are using localhost. For GitHub to reach your server,\n\
        \    set up a tunnel first: clawq tunnel start\n"
    | Some _ -> ""
  in
  Printf.sprintf
    {|
  Complete GitHub webhook setup for %s:

    1. Start clawq with the HTTP gateway enabled.
    2. Ensure the webhook URL below is reachable from GitHub.%s
    3. Go to:         %s
    4. Payload URL:   %s
    5. Content type:  application/json
    6. Secret:        %s
    7. Events:        Select "Let me select individual events" and check:
                      - Pull requests
                      - Issue comments
                      - Pull request review comments
                      - Pull request reviews
    8. Active:        checked
    9. Click "Add webhook"

    Verify locally:
      - `clawq service status` should show the gateway is running
      - `tail -f ~/.clawq/daemon.log` should show GitHub webhook deliveries
      - Look for /github/webhook/... requests and `GitHub hooks:` log lines

    Hook automation:
      - Add markdown hook files under ~/.clawq/workspace/gh-hooks/
      - Match on repo/event and optional fields such as action, status,
        conclusion, branch, head_sha, and workflow_run_id
      - `workflow_run` failures can match `status: completed` and
        `conclusion: failure`

    Full documentation: https://clawq.org/channels/
|}
    repo_name tunnel_note settings_url webhook_url webhook_secret

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.github
  with _ -> None

(* ── TUI drawing ─────────────────────────────────────────────────── *)

let draw_dashboard ~pat_token ~repos =
  let open Setup_common in
  let w = terminal_width () in
  clear_screen ();
  Printf.printf "\n";
  draw_box ~width:w
    [
      bold " GitHub Webhook Configuration ";
      "";
      Printf.sprintf "  PAT:  %s"
        (if pat_token = "" then dim "(not set)"
         else green (Tui_input.redact pat_token));
      "";
    ];
  print_docs_link "https://clawq.org/channels/";
  Printf.printf "\n";
  if repos = [] then (
    Printf.printf "  %s\n" (dim "  No repositories configured yet.");
    Printf.printf "  %s\n\n"
      (dim "  Add one to start receiving GitHub webhooks."))
  else (
    Printf.printf "  %s  %s\n" (bold "  Repositories")
      (dim (Printf.sprintf "(%d)" (List.length repos)));
    Printf.printf "\n";
    List.iteri
      (fun i (r : Runtime_config.github_repo_config) ->
        let idx = cyan (Printf.sprintf "  [%d]" (i + 1)) in
        Printf.printf "  %s  %s\n" idx (bold r.name);
        print_kv ~indent:10 "path" r.webhook_path;
        print_kv ~indent:10 "secret" (Tui_input.redact r.webhook_secret);
        print_kv ~indent:10 "events"
          (if r.react_to = [] then "all" else String.concat ", " r.react_to);
        print_kv ~indent:10 "users" (String.concat ", " r.allow_users);
        print_kv ~indent:10 "PR files"
          (if r.include_pr_files then "yes" else "no");
        (match r.agent_name with
        | Some n -> print_kv ~indent:10 "agent" n
        | None -> ());
        Printf.printf "\n")
      repos);
  draw_separator ~width:w

(* ── Repo editor (add or edit) ───────────────────────────────────── *)

let prompt_repo_fields ?(existing : Runtime_config.github_repo_config option) ()
    =
  let open Setup_common in
  (* Repo name *)
  let default_name =
    match existing with Some r -> Some r.name | None -> None
  in
  let rec get_name () =
    let name =
      prompt_string ~prompt:"Repository (owner/repo)" ?default:default_name ()
    in
    match validate_repo_name name with
    | Ok n -> n
    | Error e ->
        print_warning e;
        get_name ()
  in
  let repo_name = get_name () in

  (* Webhook secret *)
  let webhook_secret =
    match existing with
    | Some r ->
        let keep =
          prompt_yn
            ~prompt:
              (Printf.sprintf "Keep existing webhook secret? (%s)"
                 (Tui_input.redact r.webhook_secret))
            ~default:true ()
        in
        if keep then r.webhook_secret
        else
          let gen =
            prompt_yn ~prompt:"Generate new random secret?" ~default:true ()
          in
          if gen then (
            let s = generate_random_hex 32 in
            Printf.printf "    Generated: %s\n" s;
            s)
          else prompt_string ~prompt:"Webhook secret" ()
    | None ->
        let gen =
          prompt_yn ~prompt:"Generate random webhook secret?" ~default:true ()
        in
        if gen then (
          let s = generate_random_hex 32 in
          Printf.printf "    Generated: %s\n" s;
          s)
        else prompt_string ~prompt:"Webhook secret" ()
  in

  (* Webhook path *)
  let default_path =
    match existing with
    | Some r -> r.webhook_path
    | None -> default_webhook_path repo_name
  in
  let webhook_path =
    prompt_string ~prompt:"Webhook URL path" ~default:default_path ()
  in

  (* Event types *)
  let default_react_to =
    match existing with Some r -> r.react_to | None -> []
  in
  Printf.printf "\n";
  Printf.printf "  %s Event filter %s\n" (cyan "?")
    (dim "(leave empty = all events; comma-separated to restrict)");
  let react_to_default =
    if default_react_to = [] then "all" else String.concat "," default_react_to
  in
  let react_to_input =
    prompt_string ~prompt:"Events" ~default:react_to_default ()
  in
  let react_to =
    if react_to_input = "all" || react_to_input = "" then []
    else
      String.split_on_char ',' react_to_input
      |> List.map String.trim
      |> List.filter (fun s -> s <> "")
  in

  (* Allowed users *)
  let default_users =
    match existing with Some r -> r.allow_users | None -> [ "*" ]
  in
  let users_default = String.concat "," default_users in
  let users_input =
    prompt_string ~prompt:"Allowed users (* = all)" ~default:users_default ()
  in
  let allow_users =
    String.split_on_char ',' users_input
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  let allow_users = if allow_users = [] then [ "*" ] else allow_users in

  (* Include PR files *)
  let default_pr_files =
    match existing with Some r -> r.include_pr_files | None -> true
  in
  let include_pr_files =
    prompt_yn ~prompt:"Include changed files list in PR context?"
      ~default:default_pr_files ()
  in

  (* Agent name *)
  let default_agent =
    match existing with Some { agent_name = Some n; _ } -> Some n | _ -> None
  in
  let agent_name =
    let s =
      prompt_string ~prompt:"Agent name (optional, Enter to skip)"
        ?default:default_agent ()
    in
    if s = "" then None else Some s
  in

  ({
     Runtime_config.name = repo_name;
     webhook_secret;
     webhook_path;
     agent_name;
     allow_users;
     react_to;
     include_pr_files;
   }
    : Runtime_config.github_repo_config)

(* ── PAT management ──────────────────────────────────────────────── *)

let prompt_new_pat () =
  let open Setup_common in
  Printf.printf "\n";
  Printf.printf "  %s\n"
    (dim "Create a PAT at: https://github.com/settings/tokens");
  Printf.printf "  %s\n\n"
    (dim "Required scopes: repo (private repos) or public_repo");
  let rec loop () =
    match prompt_secret ~prompt:"GitHub PAT" () with
    | Ok pat -> (
        match validate_pat pat with
        | Ok p ->
            if not (pat_has_known_prefix p) then
              print_warning
                "Token doesn't start with 'ghp_' or 'github_pat_'. May still \
                 work.";
            p
        | Error e ->
            print_error e;
            loop ())
    | Error e ->
        print_error e;
        loop ()
  in
  loop ()

let prompt_pat_change ~current_pat =
  let open Setup_common in
  Printf.printf "\n";
  if current_pat <> "" then (
    Printf.printf "  Current PAT: %s\n\n" (green (Tui_input.redact current_pat));
    let change = prompt_yn ~prompt:"Change PAT?" ~default:false () in
    if not change then current_pat else prompt_new_pat ())
  else prompt_new_pat ()

(* ── Save helper ─────────────────────────────────────────────────── *)

let save_github_config ~pat_token ~repos =
  let open Setup_common in
  let json = build_full_github_json ~pat_token ~repos in
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
      let pat_token =
        ref
          (match existing with
          | Some { auth = Runtime_config.GithubPat t; _ } -> t
          | None -> "")
      in
      let repos =
        ref (match existing with Some { repos = r; _ } -> r | None -> [])
      in
      let dirty = ref false in
      let quit = ref false in
      while not !quit do
        draw_dashboard ~pat_token:!pat_token ~repos:!repos;
        let n_repos = List.length !repos in
        let options =
          [ ("p", "Set / update PAT") ]
          @ (if n_repos > 0 then
               [ ("e", Printf.sprintf "Edit a repository (1-%d)" n_repos) ]
             else [])
          @ [ ("a", "Add a new repository") ]
          @ (if n_repos > 0 then
               [ ("r", Printf.sprintf "Remove a repository (1-%d)" n_repos) ]
             else [])
          @ (if n_repos > 0 then
               [ ("i", "Show webhook setup instructions for a repo") ]
             else [])
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
            if !dirty then
              let save =
                Setup_common.prompt_yn
                  ~prompt:"You have unsaved changes. Save before exiting?"
                  ~default:true ()
              in
              if save then begin
                if !pat_token = "" then (
                  Setup_common.print_warning
                    "PAT is not set. Set it before saving.";
                  Setup_common.press_enter_to_continue ())
                else (
                  ignore
                    (save_github_config ~pat_token:!pat_token ~repos:!repos);
                  quit := true)
              end
              else quit := true
            else quit := true
        | "p" ->
            pat_token := prompt_pat_change ~current_pat:!pat_token;
            dirty := true
        | "a" ->
            Printf.printf "\n  %s\n\n" (Setup_common.bold "Add Repository");
            let repo = prompt_repo_fields () in
            repos := !repos @ [ repo ];
            dirty := true;
            Setup_common.print_success (Printf.sprintf "Added %s" repo.name);
            Setup_common.press_enter_to_continue ()
        | "e" when n_repos > 0 -> (
            let p = Printf.sprintf "\n  Which repo to edit? (1-%d): " n_repos in
            let idx_str = String.trim (Tui_input.read_line_clean p) in
            match int_of_string_opt idx_str with
            | Some idx when idx >= 1 && idx <= n_repos ->
                let existing_repo = List.nth !repos (idx - 1) in
                Printf.printf "\n  %s %s\n\n"
                  (Setup_common.bold "Editing")
                  (Setup_common.cyan existing_repo.name);
                let updated = prompt_repo_fields ~existing:existing_repo () in
                repos :=
                  List.mapi
                    (fun i r -> if i = idx - 1 then updated else r)
                    !repos;
                dirty := true;
                Setup_common.print_success
                  (Printf.sprintf "Updated %s" updated.name);
                Setup_common.press_enter_to_continue ()
            | _ ->
                Setup_common.print_warning "Invalid selection.";
                Setup_common.press_enter_to_continue ())
        | "r" when n_repos > 0 -> (
            let p =
              Printf.sprintf "\n  Which repo to remove? (1-%d): " n_repos
            in
            let idx_str = String.trim (Tui_input.read_line_clean p) in
            match int_of_string_opt idx_str with
            | Some idx when idx >= 1 && idx <= n_repos ->
                let name = (List.nth !repos (idx - 1)).name in
                let confirm =
                  Setup_common.prompt_yn
                    ~prompt:(Printf.sprintf "Remove %s?" name)
                    ~default:false ()
                in
                if confirm then (
                  repos := List.filteri (fun i _ -> i <> idx - 1) !repos;
                  dirty := true;
                  Setup_common.print_success (Printf.sprintf "Removed %s" name))
                else Printf.printf "  Cancelled.\n";
                Setup_common.press_enter_to_continue ()
            | _ ->
                Setup_common.print_warning "Invalid selection.";
                Setup_common.press_enter_to_continue ())
        | "i" when n_repos > 0 -> (
            let p =
              Printf.sprintf "\n  Show instructions for repo (1-%d): " n_repos
            in
            let idx_str = String.trim (Tui_input.read_line_clean p) in
            match int_of_string_opt idx_str with
            | Some idx when idx >= 1 && idx <= n_repos ->
                let r = List.nth !repos (idx - 1) in
                let cfg =
                  try Config_loader.load () with _ -> Runtime_config.default
                in
                let gateway_port = cfg.gateway.port in
                let tunnel_url =
                  if cfg.tunnel.enabled && String.trim cfg.tunnel.url <> "" then
                    Some cfg.tunnel.url
                  else None
                in
                let instructions =
                  post_setup_instructions ~repo_name:r.name
                    ~webhook_path:r.webhook_path
                    ~webhook_secret:r.webhook_secret ~gateway_port ~tunnel_url
                in
                Printf.printf "%s" instructions;
                Setup_common.press_enter_to_continue ()
            | _ ->
                Setup_common.print_warning "Invalid selection.";
                Setup_common.press_enter_to_continue ())
        | "s" when !dirty ->
            if !pat_token = "" then (
              Setup_common.print_warning "PAT is required before saving.";
              pat_token := prompt_new_pat ();
              dirty := true);
            if !pat_token <> "" then (
              if save_github_config ~pat_token:!pat_token ~repos:!repos then
                dirty := false;
              Setup_common.press_enter_to_continue ())
        | s -> (
            (* Allow typing a number to jump straight to editing that repo *)
            match int_of_string_opt s with
            | Some idx when idx >= 1 && idx <= n_repos ->
                let existing_repo = List.nth !repos (idx - 1) in
                Printf.printf "\n  %s %s\n\n"
                  (Setup_common.bold "Editing")
                  (Setup_common.cyan existing_repo.name);
                let updated = prompt_repo_fields ~existing:existing_repo () in
                repos :=
                  List.mapi
                    (fun i r -> if i = idx - 1 then updated else r)
                    !repos;
                dirty := true;
                Setup_common.print_success
                  (Printf.sprintf "Updated %s" updated.name);
                Setup_common.press_enter_to_continue ()
            | _ ->
                Setup_common.print_warning
                  (Printf.sprintf "Unknown option: %s" s);
                Setup_common.press_enter_to_continue ())
      done;
      if !dirty then "Exited with unsaved changes."
      else
        let n = List.length !repos in
        if n = 0 then "GitHub setup complete (no repos configured)."
        else
          Printf.sprintf "GitHub setup complete. %d repo%s configured." n
            (if n = 1 then "" else "s")
