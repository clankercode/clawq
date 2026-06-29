(** Teams Adaptive Card renderer for room-origin background task progress.

    Renders a checklist of task steps as an evolving Adaptive Card with:
    - Status header with icon and color coding
    - Checklist items with state indicators
    - Action controls (retry, logs, finalize links)
    - Artifact links (transcript, session)

    Raw prompts and logs are never shown by default — only structured status. *)

open Room_progress_checklist

(** {1 Status styling} *)

type status_style = {
  color : string;  (** Hex color for header background *)
  icon : string;  (** Emoji icon for status *)
  label : string;  (** Human-readable status label *)
}
(** Color and icon for the card header based on task progress state. *)

let style_of_state = function
  | Planned ->
      { color = "#6B7280"; icon = "\xF0\x9F\x93\x8B"; label = "Planned" }
  | Current ->
      { color = "#3B82F6"; icon = "\xF0\x9F\x94\x84"; label = "In Progress" }
  | Blocked ->
      {
        color = "#EF4444";
        icon = "\xE2\x9A\xA0\xEF\xB8\x8F";
        label = "Blocked";
      }
  | Done -> { color = "#10B981"; icon = "\xE2\x9C\x85"; label = "Done" }
  | Final ->
      { color = "#8B5CF6"; icon = "\xF0\x9F\x8F\x81"; label = "Complete" }

(** Derive overall status style from a list of checklist items. Uses the "worst"
    non-terminal state: Blocked > Current > Planned > Done. *)
let overall_style (items : checklist_item list) =
  let has_blocked = List.exists (fun i -> i.state = Blocked) items in
  let has_current = List.exists (fun i -> i.state = Current) items in
  let has_planned = List.exists (fun i -> i.state = Planned) items in
  let has_done =
    List.exists (fun i -> i.state = Done || i.state = Final) items
  in
  if has_blocked then style_of_state Blocked
  else if has_current then style_of_state Current
  else if has_planned then style_of_state Planned
  else if has_done then style_of_state Done
  else style_of_state Planned

(** {1 Item rendering} *)

(** Icon for a single checklist item state. *)
let item_icon = function
  | Planned -> "\xE2\xAC\x9C" (* ⬜ white large square *)
  | Current -> "\xF0\x9F\x94\x84" (* 🔄 clockwise arrows *)
  | Blocked -> "\xF0\x9F\x9A\xAB" (* 🚫 prohibited *)
  | Done -> "\xE2\x9C\x85" (* ✅ check mark *)
  | Final -> "\xF0\x9F\x8F\x81" (* 🏁 checkered flag *)

(** Build a TextBlock for a single checklist item. *)
let render_item_element (item : checklist_item) =
  let icon = item_icon item.state in
  let state_label =
    match item.state with
    | Current -> " *(working)*"
    | Blocked -> " *(blocked)*"
    | _ -> ""
  in
  let links =
    let parts = ref [] in
    (match item.transcript_url with
    | Some url when String.trim url <> "" ->
        parts := Url_sanitize.safe_teams_link url "transcript" :: !parts
    | _ -> ());
    (match item.session_url with
    | Some url when String.trim url <> "" ->
        parts := Url_sanitize.safe_teams_link url "session" :: !parts
    | _ -> ());
    (match item.session_record_id with
    | Some id_val when String.trim id_val <> "" ->
        let record_url = Printf.sprintf "/session-records/%s" id_val in
        parts := Url_sanitize.safe_teams_link record_url "record" :: !parts
    | _ -> ());
    match !parts with [] -> "" | ps -> " — " ^ String.concat " | " ps
  in
  let text = Printf.sprintf "%s %s%s%s" icon item.title state_label links in
  `Assoc
    [
      ("type", `String "TextBlock");
      ("text", `String text);
      ("wrap", `Bool true);
      ("spacing", `String "Small");
    ]

(** {1 Summary rendering} *)

(** Render a compact summary line showing item counts by state. *)
let render_summary_line (items : checklist_item list) =
  let counts =
    List.fold_left
      (fun acc (item : checklist_item) ->
        let key = string_of_item_state item.state in
        let current = try List.assoc key acc with Not_found -> 0 in
        (key, current + 1) :: List.remove_assoc key acc)
      [] items
  in
  let parts =
    [ "final"; "done"; "current"; "blocked"; "planned" ]
    |> List.filter_map (fun key ->
        match List.assoc_opt key counts with
        | Some n when n > 0 ->
            let icon =
              match item_state_of_string key with
              | Some s -> item_icon s
              | None -> ""
            in
            Some (Printf.sprintf "%s %d %s" icon n key)
        | _ -> None)
  in
  match parts with [] -> "No items yet" | _ -> String.concat "  " parts

(** {1 Action controls} *)

type task_actions = {
  task_id : int;
  show_retry : bool;
  show_logs : bool;
  show_finalize : bool;
  show_inspect : bool;
  show_continue : bool;
  show_cancel : bool;
  log_path : string option;
}
(** Build action buttons for a task.
    - [retry] shown when task failed
    - [logs] shown when task has a log path
    - [finalize] shown when task has dirty worktree
    - [inspect] shown to view task details
    - [continue] shown when task can be resumed
    - [cancel] shown when task is running or queued *)

type room_policy_check = tool_name:string -> bool
(** Room policy check function type. Returns [true] if the tool is allowed. *)

(** Default room policy check that allows everything. *)
let default_room_policy_check ~tool_name:_ = true

let render_actions ?(room_policy = default_room_policy_check)
    (actions : task_actions) =
  let buttons = ref [] in
  if actions.show_retry && room_policy ~tool_name:"background_task_enqueue" then
    buttons :=
      `Assoc
        [
          ("type", `String "Action.Submit");
          ("title", `String "Retry Task");
          ( "data",
            `Assoc
              [
                ( "msteams",
                  `Assoc
                    [
                      ("type", `String "imBack");
                      ( "value",
                        `String
                          (Printf.sprintf "/background retry %d" actions.task_id)
                      );
                    ] );
              ] );
        ]
      :: !buttons;
  if actions.show_logs && room_policy ~tool_name:"background_task_logs" then
    buttons :=
      `Assoc
        [
          ("type", `String "Action.Submit");
          ("title", `String "View Logs");
          ( "data",
            `Assoc
              [
                ( "msteams",
                  `Assoc
                    [
                      ("type", `String "imBack");
                      ( "value",
                        `String
                          (Printf.sprintf "/background logs %d" actions.task_id)
                      );
                    ] );
              ] );
        ]
      :: !buttons;
  if actions.show_finalize && room_policy ~tool_name:"background_finalize" then
    buttons :=
      `Assoc
        [
          ("type", `String "Action.Submit");
          ("title", `String "Finalize");
          ( "data",
            `Assoc
              [
                ( "msteams",
                  `Assoc
                    [
                      ("type", `String "imBack");
                      ( "value",
                        `String
                          (Printf.sprintf "/background finalize %d"
                             actions.task_id) );
                    ] );
              ] );
        ]
      :: !buttons;
  if actions.show_inspect && room_policy ~tool_name:"background_task_list" then
    buttons :=
      `Assoc
        [
          ("type", `String "Action.Submit");
          ("title", `String "Inspect");
          ( "data",
            `Assoc
              [
                ( "msteams",
                  `Assoc
                    [
                      ("type", `String "imBack");
                      ( "value",
                        `String
                          (Printf.sprintf "/background show %d" actions.task_id)
                      );
                    ] );
              ] );
        ]
      :: !buttons;
  if actions.show_continue && room_policy ~tool_name:"background_task_resume"
  then
    buttons :=
      `Assoc
        [
          ("type", `String "Action.Submit");
          ("title", `String "Continue");
          ( "data",
            `Assoc
              [
                ( "msteams",
                  `Assoc
                    [
                      ("type", `String "imBack");
                      ( "value",
                        `String
                          (Printf.sprintf "/background resume %d"
                             actions.task_id) );
                    ] );
              ] );
        ]
      :: !buttons;
  if actions.show_cancel && room_policy ~tool_name:"background_task_cancel" then
    buttons :=
      `Assoc
        [
          ("type", `String "Action.Submit");
          ("title", `String "Cancel");
          ( "data",
            `Assoc
              [
                ( "msteams",
                  `Assoc
                    [
                      ("type", `String "imBack");
                      ( "value",
                        `String
                          (Printf.sprintf "/background cancel %d"
                             actions.task_id) );
                    ] );
              ] );
        ]
      :: !buttons;
  match !buttons with
  | [] -> []
  | btns ->
      [ `Assoc [ ("type", `String "ActionSet"); ("actions", `List btns) ] ]

(** {1 Full card construction} *)

(** Task outcome status for terminal state styling. *)
type task_outcome = Succeeded | Failed | DirtyWorktree | Cancelled

let style_of_outcome = function
  | Succeeded ->
      { color = "#10B981"; icon = "\xE2\x9C\x85"; label = "Succeeded" }
  | Failed -> { color = "#EF4444"; icon = "\xE2\x9D\x8C"; label = "Failed" }
  | DirtyWorktree ->
      {
        color = "#F59E0B";
        icon = "\xE2\x9A\xA0\xEF\xB8\x8F";
        label = "Dirty Worktree";
      }
  | Cancelled ->
      { color = "#6B7280"; icon = "\xF0\x9F\x9A\xAB"; label = "Cancelled" }

(** Build a complete Adaptive Card JSON for a room-origin progress update.

    Parameters:
    - [~task_id] the background task ID
    - [~task_label] short description of the task (e.g. "claude repo=foo
      branch=main")
    - [~items] the checklist items to render
    - [~actions] optional action controls to show
    - [~elapsed] optional elapsed time string
    - [~summary] optional override summary text
    - [~task_outcome] optional terminal task outcome for styling *)
let build_card ~task_id ~task_label ~items ?(actions = None) ?elapsed ?summary
    ?task_outcome ?room_policy () =
  let style =
    match task_outcome with
    | Some outcome -> style_of_outcome outcome
    | None -> overall_style items
  in
  let header_text =
    Printf.sprintf "%s Task #%d: %s" style.icon task_id task_label
  in
  let summary_text =
    match summary with Some s -> s | None -> render_summary_line items
  in
  let elapsed_text =
    match elapsed with Some e -> Printf.sprintf "Elapsed: %s" e | None -> ""
  in
  let body_elements =
    [
      `Assoc
        [
          ("type", `String "TextBlock");
          ("text", `String header_text);
          ("size", `String "Large");
          ("weight", `String "Bolder");
          ("wrap", `Bool true);
          ( "color",
            `String
              (match style.color with
              | "#3B82F6" -> "Accent"
              | "#10B981" -> "Good"
              | "#EF4444" -> "Attention"
              | _ -> "Default") );
        ];
      `Assoc
        [
          ("type", `String "TextBlock");
          ("text", `String summary_text);
          ("spacing", `String "Medium");
          ("wrap", `Bool true);
          ("isSubtle", `Bool true);
        ];
    ]
  in
  let body_elements =
    if elapsed_text <> "" then
      body_elements
      @ [
          `Assoc
            [
              ("type", `String "TextBlock");
              ("text", `String elapsed_text);
              ("size", `String "Small");
              ("isSubtle", `Bool true);
            ];
        ]
    else body_elements
  in
  (* Add separator before checklist *)
  let body_elements =
    body_elements
    @ [
        `Assoc
          [
            ("type", `String "Container");
            ("style", `String "emphasis");
            ("spacing", `String "Medium");
            ("items", `List (List.map render_item_element items));
          ];
      ]
  in
  (* Add action controls if present *)
  let body_elements =
    match actions with
    | Some act -> body_elements @ render_actions ?room_policy act
    | None -> body_elements
  in
  (* Wrap in the Bot Framework attachment envelope *)
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
                      ("body", `List body_elements);
                    ] );
              ];
          ] );
    ]

(** {1 Edit support} *)

(** Build an updated card JSON for editing an existing progress message. Same
    structure as [build_card] but without the envelope wrapper — used when
    editing an existing Adaptive Card in place. *)
let build_update_card ~task_id ~task_label ~items ?(actions = None) ?elapsed
    ?summary ?task_outcome ?room_policy () =
  let style =
    match task_outcome with
    | Some outcome -> style_of_outcome outcome
    | None -> overall_style items
  in
  let header_text =
    Printf.sprintf "%s Task #%d: %s" style.icon task_id task_label
  in
  let summary_text =
    match summary with Some s -> s | None -> render_summary_line items
  in
  let elapsed_text =
    match elapsed with Some e -> Printf.sprintf "Elapsed: %s" e | None -> ""
  in
  let body_elements =
    [
      `Assoc
        [
          ("type", `String "TextBlock");
          ("text", `String header_text);
          ("size", `String "Large");
          ("weight", `String "Bolder");
          ("wrap", `Bool true);
          ( "color",
            `String
              (match style.color with
              | "#3B82F6" -> "Accent"
              | "#10B981" -> "Good"
              | "#EF4444" -> "Attention"
              | _ -> "Default") );
        ];
      `Assoc
        [
          ("type", `String "TextBlock");
          ("text", `String summary_text);
          ("spacing", `String "Medium");
          ("wrap", `Bool true);
          ("isSubtle", `Bool true);
        ];
    ]
  in
  let body_elements =
    if elapsed_text <> "" then
      body_elements
      @ [
          `Assoc
            [
              ("type", `String "TextBlock");
              ("text", `String elapsed_text);
              ("size", `String "Small");
              ("isSubtle", `Bool true);
            ];
        ]
    else body_elements
  in
  let body_elements =
    body_elements
    @ [
        `Assoc
          [
            ("type", `String "Container");
            ("style", `String "emphasis");
            ("spacing", `String "Medium");
            ("items", `List (List.map render_item_element items));
          ];
      ]
  in
  let body_elements =
    match actions with
    | Some act -> body_elements @ render_actions ?room_policy act
    | None -> body_elements
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
                      ("body", `List body_elements);
                    ] );
              ];
          ] );
    ]

(** {1 Fallback text} *)

(** Build a plain text fallback for connectors that don't support Adaptive
    Cards. Shows the same checklist but as markdown text. *)
let build_fallback_text ~task_label ~(items : checklist_item list) ?summary () =
  let style = overall_style items in
  let summary_text =
    match summary with Some s -> s | None -> render_summary_line items
  in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add (Printf.sprintf "%s %s" style.icon task_label);
  add summary_text;
  add "";
  List.iter (fun (item : checklist_item) -> add (render_item item)) items;
  String.concat "\n" (List.rev !lines)
