(** Deliver progress updates to origin rooms for room-origin background tasks.

    When a room-origin background task changes state, this module formats a
    progress message and delivers it to the room where the request originated.
    For connectors with edit-in-place support, it edits the same message. For
    unsupported connectors, it sends a sparse fallback (new message per update).

    The module is connector-agnostic: callers provide [send] and [edit]
    callbacks. Thread-aware delivery is used when a [thread_id] is present in
    the task's origin metadata. *)

open Background_task_0_format

(** In-memory map from task ID to the progress message identifier, used for
    edit-in-place delivery. *)
let progress_msg_ids : (int, string) Hashtbl.t = Hashtbl.create 16

(** Clear the tracked progress message ID for a task. Call when the task reaches
    a terminal state and no further updates are expected. *)
let clear_progress_msg_id ~task_id = Hashtbl.remove progress_msg_ids task_id

(** Extract a short label from a task's prompt for display. Uses the first
    non-empty line, truncated to 80 characters. *)
let short_prompt_label (task : task) =
  let first_line =
    match String.split_on_char '\n' (String.trim task.prompt) with
    | line :: _ when String.trim line <> "" -> String.trim line
    | _ -> ""
  in
  if first_line = "" then task_label task
  else if String.length first_line > 80 then String.sub first_line 0 77 ^ "..."
  else first_line

(** Format a progress update message for a room-origin task. Produces a
    human-readable string suitable for posting to a room or thread. *)
let format_progress_message (task : task) =
  let state_str =
    match effective_progress_state task with
    | Some ps -> string_of_progress_state ps
    | None -> string_of_status task.status
  in
  let label = short_prompt_label task in
  Printf.sprintf "[%s] %s" state_str label

(** Deliver a progress update to the origin room for a task. Parses the task's
    origin metadata to determine room and thread. Uses edit-in-place when a
    previous progress message exists; otherwise sends a new message.

    Parameters:
    - [~send] send a message to a room, optionally in a thread. Returns the
      message identifier (e.g. Slack ts, Discord message_id). Return [""] or
      ["0"] if the ID is unavailable.
    - [~edit] edit an existing message in place. Should raise on failure so the
      fallback path can send a fresh message.
    - [~task] the background task with origin metadata *)
let deliver_progress_update
    ~(send :
       room_id:string ->
       ?thread_id:string ->
       text:string ->
       unit ->
       string Lwt.t)
    ~(edit : room_id:string -> msg_id:string -> text:string -> unit Lwt.t)
    ~(task : task) () =
  let open Lwt.Syntax in
  let text = format_progress_message task in
  let origin = Option.bind task.origin_json Room_origin.of_json_string_opt in
  let room_id =
    match origin with
    | Some o -> Option.value o.room_id ~default:""
    | None -> Option.value task.channel_id ~default:""
  in
  let thread_id =
    match origin with Some o -> o.thread_id | None -> task.thread_id
  in
  if room_id = "" then Lwt.return_unit
  else
    match Hashtbl.find_opt progress_msg_ids task.id with
    | Some msg_id ->
        (* Edit existing progress message in place *)
        Lwt.catch
          (fun () ->
            let* () = edit ~room_id ~msg_id ~text in
            Lwt.return_unit)
          (fun _exn ->
            (* Edit failed — message may have been deleted. Send a new one. *)
            let* new_id = send ~room_id ?thread_id ~text () in
            if new_id <> "" && new_id <> "0" then
              Hashtbl.replace progress_msg_ids task.id new_id;
            Lwt.return_unit)
    | None ->
        (* First progress message — send and record the ID *)
        let* msg_id = send ~room_id ?thread_id ~text () in
        if msg_id <> "" && msg_id <> "0" then
          Hashtbl.replace progress_msg_ids task.id msg_id;
        Lwt.return_unit
