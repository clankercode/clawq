(* setup_cron.ml — DB-backed interactive wizard for cron job management *)

(* ── Pure validation functions (tested) ──────────────────────────── *)

let validate_job_name s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Job name cannot be empty. Use a short identifier (e.g. 'daily-report', \
       'hourly-check')."
  else if String.to_seq trimmed |> Seq.exists (fun c -> c = ' ' || c = '\t')
  then
    Error
      (Printf.sprintf
         "Job name must not contain whitespace (got: '%s'). Use hyphens or \
          underscores instead."
         trimmed)
  else Ok trimmed

let validate_schedule s =
  match Scheduler.parse_schedule (String.trim s) with
  | Ok _ -> Ok (String.trim s)
  | Error e ->
      Error
        (Printf.sprintf
           "Invalid schedule '%s': %s\n\
           \  Examples: 'every 30m', 'every 2h', '0 9 * * 1' (Mon 9am), '0 */4 \
            * * *' (every 4h)"
           (String.trim s) e)

let validate_message s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Message cannot be empty. This is the text sent to the agent when the \
       job fires (e.g. 'Generate daily summary')."
  else Ok trimmed

(* ── Display helpers ─────────────────────────────────────────────── *)

let draw_jobs_dashboard jobs =
  let open Setup_common in
  let w = terminal_width () in
  clear_screen ();
  Printf.printf "\n";
  let lines =
    [ bold " Cron Job Manager "; "" ]
    @ (if jobs = [] then [ dim "  (no jobs configured)" ]
       else
         List.map
           (fun (j : Scheduler.job) ->
             let status =
               if j.enabled then green "enabled" else dim "disabled"
             in
             let expires =
               match j.expires_at with
               | Some ea -> " expires:" ^ ea
               | None -> ""
             in
             Printf.sprintf "  %s  %s  %s  %s%s"
               (cyan (string_of_int j.id))
               (bold j.name) status (dim j.schedule_str) (dim expires))
           jobs)
    @ [ "" ]
  in
  draw_box ~width:w lines;
  print_docs_link "https://clawq.org/features/#cron";
  Printf.printf "\n";
  draw_separator ~width:w

(* ── Job list display ────────────────────────────────────────────── *)

let print_jobs_table jobs =
  let open Setup_common in
  if jobs = [] then Printf.printf "\n  %s\n" (dim "(no jobs configured)")
  else begin
    Printf.printf "\n";
    Printf.printf "  %s\n"
      (bold
         (Printf.sprintf "  %-4s  %-20s  %-8s  %-25s  %-19s  %s" "ID" "Name"
            "Status" "Schedule" "Expires" "Session"));
    Printf.printf "\n";
    List.iter
      (fun (j : Scheduler.job) ->
        let status = if j.enabled then green "enabled " else dim "disabled" in
        let expires = match j.expires_at with Some ea -> ea | None -> "-" in
        Printf.printf "  %-4d  %-20s  %s  %-25s  %-19s  %s\n" j.id
          (if String.length j.name > 20 then String.sub j.name 0 17 ^ "..."
           else j.name)
          status
          (if String.length j.schedule_str > 25 then
             String.sub j.schedule_str 0 22 ^ "..."
           else j.schedule_str)
          expires j.session_key)
      jobs
  end;
  Printf.printf "\n"

(* ── Prompt helpers ──────────────────────────────────────────────── *)

let pick_job jobs prompt_text =
  let open Setup_common in
  print_jobs_table jobs;
  let s = prompt_string ~prompt:prompt_text ~default:"" () in
  match int_of_string_opt (String.trim s) with
  | Some id -> (
      let found = List.find_opt (fun (j : Scheduler.job) -> j.id = id) jobs in
      match found with
      | Some j -> Ok j
      | None -> Error (Printf.sprintf "No job with ID %d." id))
  | None -> (
      if String.trim s = "" then Error "Cancelled."
      else
        (* Try by name *)
        let found =
          List.find_opt (fun (j : Scheduler.job) -> j.name = String.trim s) jobs
        in
        match found with
        | Some j -> Ok j
        | None -> Error (Printf.sprintf "No job named '%s'." (String.trim s)))

(* ── Actions ─────────────────────────────────────────────────────── *)

let action_add ~db =
  let open Setup_common in
  Printf.printf "\n";
  Printf.printf "  %s\n\n" (bold "Add Cron Job");
  let rec get_name () =
    let s = prompt_string ~prompt:"Job name" ~default:"" () in
    match validate_job_name s with
    | Ok n -> n
    | Error e ->
        print_warning e;
        get_name ()
  in
  let name = get_name () in
  let session_key = prompt_string ~prompt:"Session key" ~default:"default" () in
  let rec get_schedule () =
    Printf.printf "  %s\n"
      (dim
         "Examples: every 30m, every 2h, 0 9 * * 1 (Mon 9am), 0 */4 * * * \
          (every 4h)");
    let s = prompt_string ~prompt:"Schedule" ~default:"" () in
    match validate_schedule s with
    | Ok sched -> sched
    | Error e ->
        print_warning e;
        get_schedule ()
  in
  let schedule = get_schedule () in
  let rec get_message () =
    let s = prompt_string ~prompt:"Message to send" ~default:"" () in
    match validate_message s with
    | Ok m -> m
    | Error e ->
        print_warning e;
        get_message ()
  in
  let message = get_message () in
  Printf.printf "  %s\n"
    (dim "Examples: 24h, 7d, 30m (blank for no TTL — job runs indefinitely)");
  let ttl =
    let s = prompt_string ~prompt:"TTL (optional)" ~default:"" () in
    let trimmed = String.trim s in
    if trimmed = "" then None
    else
      match Scheduler.parse_duration_seconds trimmed with
      | Ok _ -> Some trimmed
      | Error e ->
          print_warning (Printf.sprintf "Invalid TTL: %s (skipping)" e);
          None
  in
  match Scheduler.add_job ~db ~name ~session_key ~message ~schedule ?ttl () with
  | Ok () -> print_success (Printf.sprintf "Added job '%s'." name)
  | Error e -> print_error e

let action_edit ~db jobs =
  let open Setup_common in
  Printf.printf "\n";
  Printf.printf "  %s\n\n" (bold "Edit Cron Job");
  match pick_job jobs "Job ID or name to edit" with
  | Error e -> print_warning e
  | Ok job -> (
      Printf.printf "\n  Editing: %s\n\n" (bold job.name);
      let rec get_schedule () =
        Printf.printf "  %s\n"
          (dim
             "Examples: every 30m, every 2h, 0 9 * * 1 (Mon 9am), 0 */4 * * * \
              (every 4h)");
        let s =
          prompt_string ~prompt:"New schedule (Enter to keep)"
            ~default:job.schedule_str ()
        in
        if s = job.schedule_str then None
        else
          match validate_schedule s with
          | Ok sched -> Some sched
          | Error e ->
              print_warning e;
              get_schedule ()
      in
      let new_schedule = get_schedule () in
      let new_message =
        let s =
          prompt_string ~prompt:"New message (Enter to keep)"
            ~default:job.message ()
        in
        if s = job.message then None
        else
          match validate_message s with
          | Ok m -> Some m
          | Error e ->
              print_warning e;
              None
      in
      Printf.printf "  %s\n"
        (dim
           (Printf.sprintf "Current expires_at: %s"
              (match job.expires_at with Some ea -> ea | None -> "none")));
      Printf.printf "  %s\n"
        (dim "Examples: 24h, 7d, 30m, \"none\" to clear (blank to keep current)");
      let new_ttl =
        let s =
          prompt_string ~prompt:"New TTL (Enter to keep)" ~default:"" ()
        in
        let trimmed = String.trim s in
        if trimmed = "" then None
        else if String.lowercase_ascii trimmed = "none" then Some "none"
        else
          match Scheduler.parse_duration_seconds trimmed with
          | Ok _ -> Some trimmed
          | Error e ->
              print_warning (Printf.sprintf "Invalid TTL: %s (skipping)" e);
              None
      in
      if new_schedule = None && new_message = None && new_ttl = None then
        print_warning "Nothing changed."
      else
        match
          Scheduler.update_job ~db ~name:job.name ?schedule:new_schedule
            ?message:new_message ?ttl:new_ttl ()
        with
        | Ok () -> print_success (Printf.sprintf "Updated job '%s'." job.name)
        | Error e -> print_error e)

let action_remove ~db jobs =
  let open Setup_common in
  Printf.printf "\n";
  Printf.printf "  %s\n\n" (bold "Remove Cron Job");
  match pick_job jobs "Job ID or name to remove" with
  | Error e -> print_warning e
  | Ok job ->
      Printf.printf "\n  Remove job '%s'? This cannot be undone.\n"
        (bold job.name);
      let confirm = prompt_yn ~prompt:"Confirm removal" ~default:false () in
      if confirm then begin
        let removed = Scheduler.remove_job ~db ~name:job.name in
        if removed then
          print_success (Printf.sprintf "Removed job '%s'." job.name)
        else print_error (Printf.sprintf "Job '%s' not found." job.name)
      end
      else print_warning "Cancelled."

let action_toggle ~db jobs =
  let open Setup_common in
  Printf.printf "\n";
  Printf.printf "  %s\n\n" (bold "Toggle Job Enabled");
  match pick_job jobs "Job ID or name to toggle" with
  | Error e -> print_warning e
  | Ok job -> (
      match Scheduler.toggle_job ~db ~name:job.name with
      | Ok () ->
          let new_state = if job.enabled then "disabled" else "enabled" in
          print_success
            (Printf.sprintf "Job '%s' is now %s." job.name new_state)
      | Error e -> print_error e)

(* ── Main menu loop ──────────────────────────────────────────────── *)

let run () =
  match Setup_common.check_tty () with
  | Error e -> e
  | Ok () ->
      let db_path = Dot_dir.db_path () in
      let db = Sqlite3.db_open db_path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          Scheduler.init_schema db;
          let quit = ref false in
          while not !quit do
            let jobs = Scheduler.list_jobs ~db in
            draw_jobs_dashboard jobs;
            let options =
              [ ("a", "Add job") ]
              @ (if jobs <> [] then
                   [
                     ("e", "Edit job");
                     ("r", "Remove job");
                     ("t", "Toggle enabled/disabled");
                     ("l", "List jobs (table view)");
                   ]
                 else [])
              @ [ ("h", "Show schedule examples") ]
            in
            let choice =
              Setup_common.prompt_menu ~title:"Actions" ~options
                ~shortcut_exit:"q/Enter" ()
            in
            match String.lowercase_ascii choice with
            | "q" | "" -> quit := true
            | "a" ->
                action_add ~db;
                Setup_common.press_enter_to_continue ()
            | "e" ->
                action_edit ~db jobs;
                Setup_common.press_enter_to_continue ()
            | "r" ->
                action_remove ~db jobs;
                Setup_common.press_enter_to_continue ()
            | "t" ->
                action_toggle ~db jobs;
                Setup_common.press_enter_to_continue ()
            | "l" ->
                print_jobs_table jobs;
                Setup_common.press_enter_to_continue ()
            | "h" ->
                Printf.printf
                  {|
  Schedule format examples:

    Human-readable (clawq extension):
      every 30m         Every 30 minutes
      every 2h          Every 2 hours
      every 1d          Every day

    Standard cron (minute hour day month weekday):
      0 9 * * *         Daily at 9:00 AM
      0 9 * * 1         Every Monday at 9:00 AM
      0 */4 * * *       Every 4 hours
      30 8 * * 1-5      Weekdays at 8:30 AM
      0 0 1 * *         First of every month at midnight
      0 9,17 * * *      At 9 AM and 5 PM daily

    Weekday numbers: 0=Sun, 1=Mon, 2=Tue, 3=Wed, 4=Thu, 5=Fri, 6=Sat

  Session key:
    The 'session' field determines which agent session receives the message.
    Use 'default' for the default session, or a custom session name.

  Message:
    The message text the agent receives when the job fires.
    Examples: "Generate daily standup summary"
              "Check for new GitHub issues and summarize"

  TTL (optional):
    Auto-disable the job after this duration.
    Examples: 24h (1 day), 7d (1 week), 30m (30 minutes)
    Leave blank for no TTL (job runs indefinitely).

|};
                Setup_common.press_enter_to_continue ()
            | s ->
                Setup_common.print_warning
                  (Printf.sprintf "Unknown option: %s" s);
                Setup_common.press_enter_to_continue ()
          done);
      "Cron job setup complete."
