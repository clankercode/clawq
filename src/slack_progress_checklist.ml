(** Slack mrkdwn renderer for room-origin background task progress checklists.

    Renders checklist items as Slack-formatted messages with:
    - Status emoji icons per item state
    - Completion ratio summary
    - Transcript and session links in Slack mrkdwn format
    - Blocked items with short generic status (no secret leakage)

    Uses Slack mrkdwn syntax: *bold*, _italic_, [links](url). *)

open Room_progress_checklist

(** {1 Status icons} *)

(** Emoji icon for a checklist item state, suitable for Slack. *)
let item_icon = function
  | Planned -> "\xE2\xAC\x9C\xEF\xB8\x8F" (* ⬜ white square *)
  | Current -> "\xF0\x9F\x94\x84" (* 🔄 clockwise arrows *)
  | Blocked -> "\xF0\x9F\x9A\xAB" (* 🚫 prohibited *)
  | Done -> "\xE2\x9C\x85" (* ✅ check mark *)
  | Final -> "\xF0\x9F\x8F\x81" (* 🏁 checkered flag *)

(** Emoji icon for an overall status based on worst non-terminal state. *)
let overall_icon (items : checklist_item list) =
  let has_blocked = List.exists (fun i -> i.state = Blocked) items in
  let has_current = List.exists (fun i -> i.state = Current) items in
  let has_planned = List.exists (fun i -> i.state = Planned) items in
  let has_done =
    List.exists (fun i -> i.state = Done || i.state = Final) items
  in
  if has_blocked then item_icon Blocked
  else if has_current then item_icon Current
  else if has_planned then item_icon Planned
  else if has_done then item_icon Done
  else item_icon Planned

(** {1 Item rendering} *)

(** Render a single checklist item as a Slack mrkdwn line.

    Format: {v :icon: *Title* (working) — [transcript](url) | [session](url) v}

    Blocked items include a generic "(blocked)" indicator without exposing
    internal dependency details or secrets. *)
let render_item (item : checklist_item) =
  let buf = Buffer.create 128 in
  Buffer.add_string buf (item_icon item.state);
  Buffer.add_char buf ' ';
  (* Bold the title *)
  Buffer.add_char buf '*';
  Buffer.add_string buf item.title;
  Buffer.add_char buf '*';
  (* State label for active/blocked items *)
  (match item.state with
  | Current -> Buffer.add_string buf " (working)"
  | Blocked -> Buffer.add_string buf " (blocked)"
  | _ -> ());
  (* Links using Slack mrkdwn format *)
  let links = ref [] in
  (match item.transcript_url with
  | Some url when String.trim url <> "" ->
      links := Printf.sprintf "<%s|transcript>" url :: !links
  | _ -> ());
  (match item.session_url with
  | Some url when String.trim url <> "" ->
      links := Printf.sprintf "<%s|session>" url :: !links
  | _ -> ());
  (match !links with
  | [] -> ()
  | ps ->
      Buffer.add_string buf " — ";
      Buffer.add_string buf (String.concat " | " ps));
  Buffer.contents buf

(** {1 Summary rendering} *)

(** Compute counts by state from a checklist item list. *)
let count_by_state (items : checklist_item list) =
  List.fold_left
    (fun acc (item : checklist_item) ->
      let key = string_of_item_state item.state in
      let current = try List.assoc key acc with Not_found -> 0 in
      (key, current + 1) :: List.remove_assoc key acc)
    [] items

(** Render a compact summary line with completion ratio and state counts.

    Format: {v :overall_icon: 3/5 done | 1 current, 1 blocked v} *)
let render_summary (items : checklist_item list) =
  if items = [] then "(no items)"
  else
    let total = List.length items in
    let counts = count_by_state items in
    let done_count =
      (try List.assoc "done" counts with Not_found -> 0)
      + try List.assoc "final" counts with Not_found -> 0
    in
    let icon = overall_icon items in
    let ratio = Printf.sprintf "%d/%d done" done_count total in
    let state_parts =
      [ "current"; "blocked"; "planned" ]
      |> List.filter_map (fun key ->
          match List.assoc_opt key counts with
          | Some n when n > 0 -> Some (Printf.sprintf "%d %s" n key)
          | _ -> None)
    in
    let detail =
      match state_parts with [] -> "" | ps -> " | " ^ String.concat ", " ps
    in
    Printf.sprintf "%s %s%s" icon ratio detail

(** {1 Full message rendering} *)

(** Render a complete Slack progress checklist message.

    Parameters:
    - [~task_label] short description of the task
    - [~items] the checklist items to render
    - [~elapsed] optional elapsed time string *)
let render_checklist ~task_label ?elapsed (items : checklist_item list) =
  let buf = Buffer.create 512 in
  (* Header *)
  let icon = overall_icon items in
  Buffer.add_string buf icon;
  Buffer.add_char buf ' ';
  Buffer.add_char buf '*';
  Buffer.add_string buf task_label;
  Buffer.add_char buf '*';
  Buffer.add_char buf '\n';
  (* Summary with completion ratio *)
  Buffer.add_string buf (render_summary items);
  (* Elapsed time *)
  (match elapsed with
  | Some e when String.trim e <> "" ->
      Buffer.add_string buf (Printf.sprintf " • %s" e)
  | _ -> ());
  Buffer.add_char buf '\n';
  Buffer.add_string buf "\n";
  (* Checklist items *)
  List.iter
    (fun item ->
      Buffer.add_string buf (render_item item);
      Buffer.add_char buf '\n')
    items;
  Buffer.contents buf

(** Render a final/completion message for a terminal task.

    Adds outcome indicator and optional action hints. *)
let render_final ~task_label ?elapsed ?summary ?task_status
    (items : checklist_item list) =
  let buf = Buffer.create 512 in
  (* Header with outcome *)
  let outcome_icon =
    match task_status with
    | Some "succeeded" -> "\xE2\x9C\x85"
    | Some "failed" -> "\xE2\x9D\x8C"
    | Some "dirty_worktree" -> "\xE2\x9A\xA0\xEF\xB8\x8F"
    | Some "cancelled" -> "\xF0\x9F\x9A\xAB"
    | _ -> overall_icon items
  in
  Buffer.add_string buf outcome_icon;
  Buffer.add_char buf ' ';
  Buffer.add_char buf '*';
  Buffer.add_string buf task_label;
  Buffer.add_char buf '*';
  Buffer.add_char buf '\n';
  (* Summary *)
  (match summary with
  | Some s when String.trim s <> "" -> Buffer.add_string buf s
  | _ -> Buffer.add_string buf (render_summary items));
  (* Elapsed *)
  (match elapsed with
  | Some e when String.trim e <> "" ->
      Buffer.add_string buf (Printf.sprintf " • %s" e)
  | _ -> ());
  Buffer.add_char buf '\n';
  Buffer.add_string buf "\n";
  (* Checklist items *)
  List.iter
    (fun item ->
      Buffer.add_string buf (render_item item);
      Buffer.add_char buf '\n')
    items;
  Buffer.contents buf
