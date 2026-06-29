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
let extract_room_id_and_thread (task : task) =
  let origin = Option.bind task.origin_json Room_origin.of_json_string_opt in
  let room_id =
    match origin with
    | Some o -> Option.value o.room_id ~default:""
    | None -> Option.value task.channel_id ~default:""
  in
  let thread_id =
    match origin with Some o -> o.thread_id | None -> task.thread_id
  in
  (room_id, thread_id)

(** Result of a room delivery attempt. *)
type delivery_result = Delivered | Delivery_failed of string | Skipped

let is_valid_message_id msg_id =
  let trimmed = String.trim msg_id in
  trimmed <> "" && trimmed <> "0"

let activity_id_of_message_id = function
  | Some msg_id when is_valid_message_id msg_id -> Some msg_id
  | _ -> None

(** Extract connector, room_id, and thread_id from a task's origin metadata. *)
let connector_and_room_of_task (task : task) =
  let origin = Option.bind task.origin_json Room_origin.of_json_string_opt in
  let connector =
    match origin with
    | Some o -> Option.value o.Room_origin.connector ~default:""
    | None -> Option.value task.channel ~default:""
  in
  let room_id, thread_id = extract_room_id_and_thread task in
  (connector, room_id, thread_id)

type lifecycle_ctx = {
  db : Sqlite3.db;
  room_id : string;
  connector : string;
  tracking_id : string;
  task_id : int;
  thread_id : string option;
}
(** Lifecycle context for Teams delivery tracking. When provided to
    [send_or_edit], records granular lifecycle state transitions. *)

(** [send_or_edit] performs the actual send/edit cycle and returns the result
    along with the resolved message ID. A delivery is [Delivered] only when a
    valid message ID is returned (non-empty, not "0"). Empty or placeholder IDs
    indicate a failed delivery (e.g. Teams returning empty activity ID on
    transient errors).

    When [?lifecycle_ctx] is provided (Teams connector), records granular
    lifecycle states: Attempted on send, Transport_accepted on 2xx,
    Message_id_recorded on valid ID, Edit_failed on edit exception,
    Fallback_sent on fallback send, User_visible_unconfirmed on empty ID. *)
let send_or_edit ~send ~edit ~room_id ?thread_id ~text ~task_id ?lifecycle_ctx
    () =
  let open Lwt.Syntax in
  let invalid_message_id_error msg_id =
    if String.trim msg_id = "" then "empty message ID from connector"
    else "placeholder message ID from connector"
  in
  let record state ?error ?message_id () =
    match lifecycle_ctx with
    | Some ctx ->
        Teams_delivery_lifecycle.record_lifecycle ~db:ctx.db
          ~room_id:ctx.room_id ~connector:ctx.connector
          ~tracking_id:ctx.tracking_id ~state ~task_id:ctx.task_id
          ?thread_id:ctx.thread_id ?error ?message_id ()
    | None -> ()
  in
  let send_new ?invalid_error ?(is_fallback = false) () =
    record Teams_delivery_lifecycle.Generated ();
    record Teams_delivery_lifecycle.Attempted ();
    let* msg_id = send ~room_id ?thread_id ~text () in
    record Teams_delivery_lifecycle.Transport_accepted ();
    if is_fallback then record Teams_delivery_lifecycle.Fallback_sent ();
    if is_valid_message_id msg_id then begin
      record Teams_delivery_lifecycle.Message_id_recorded ~message_id:msg_id ();
      Hashtbl.replace progress_msg_ids task_id msg_id;
      Lwt.return (Delivered, Some msg_id)
    end
    else begin
      let trimmed = String.trim msg_id in
      if trimmed = "" || trimmed = "0" then
        record Teams_delivery_lifecycle.User_visible_unconfirmed ();
      let err =
        match invalid_error with
        | Some err -> err
        | None -> invalid_message_id_error msg_id
      in
      if trimmed <> "" && trimmed <> "0" then
        record Teams_delivery_lifecycle.Failed ~error:err ();
      Lwt.return (Delivery_failed err, None)
    end
  in
  match Hashtbl.find_opt progress_msg_ids task_id with
  | Some msg_id when is_valid_message_id msg_id ->
      Lwt.catch
        (fun () ->
          let* () = edit ~room_id ~msg_id ~text in
          record Teams_delivery_lifecycle.Generated ();
          record Teams_delivery_lifecycle.Attempted ();
          record Teams_delivery_lifecycle.Transport_accepted ();
          record Teams_delivery_lifecycle.Message_id_recorded ~message_id:msg_id
            ();
          Lwt.return (Delivered, Some msg_id))
        (fun exn ->
          let edit_err = Printexc.to_string exn in
          record Teams_delivery_lifecycle.Edit_failed ~error:edit_err ();
          send_new ~is_fallback:true ())
  | Some _ ->
      Hashtbl.remove progress_msg_ids task_id;
      send_new ()
  | None -> send_new ()

let deliver_progress_update
    ~(send :
       room_id:string ->
       ?thread_id:string ->
       text:string ->
       unit ->
       string Lwt.t)
    ~(edit : room_id:string -> msg_id:string -> text:string -> unit Lwt.t)
    ?(db : Sqlite3.db option) ~(task : task) () =
  let open Lwt.Syntax in
  let text = format_progress_message task in
  let connector, room_id, thread_id = connector_and_room_of_task task in
  if room_id = "" then Lwt.return_unit
  else begin
    let existing_activity_id =
      activity_id_of_message_id (Hashtbl.find_opt progress_msg_ids task.id)
    in
    (* Record delivery attempt in ledger *)
    (match db with
    | Some db ->
        ignore
          (Room_activity_ledger.record_delivery_attempt ~db ~room_id ~connector
             ~task_id:task.id ?thread_id ?activity_id:existing_activity_id ())
    | None -> ());
    (* Generate lifecycle tracking ID for Teams connectors *)
    let lifecycle_ctx =
      match db with
      | Some db when connector = "teams" ->
          let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
          Teams_delivery_lifecycle.record_scheduled ~db ~room_id ~connector
            ~tracking_id ~task_id:task.id ?thread_id
            ?activity_id:existing_activity_id ();
          Some
            {
              db;
              room_id;
              connector;
              tracking_id;
              task_id = task.id;
              thread_id;
            }
      | _ -> None
    in
    Lwt.catch
      (fun () ->
        let* result, msg_id =
          send_or_edit ~send ~edit ~room_id ?thread_id ~text ~task_id:task.id
            ?lifecycle_ctx ()
        in
        let activity_id = activity_id_of_message_id msg_id in
        (* Record delivery outcome in ledger *)
        (match (db, result) with
        | Some db, Delivered ->
            ignore
              (Room_activity_ledger.record_delivery_success ~db ~room_id
                 ~connector ~task_id:task.id
                 ~message_id:(Option.value msg_id ~default:"")
                 ?thread_id ?activity_id ())
        | Some db, Delivery_failed err ->
            ignore
              (Room_activity_ledger.record_delivery_failure ~db ~room_id
                 ~connector ~task_id:task.id ~error:err ?thread_id ?activity_id
                 ())
        | Some _, Skipped -> ()
        | None, _ -> ());
        Lwt.return_unit)
      (fun exn ->
        let err = Printexc.to_string exn in
        (* Record exception-based failure in ledger *)
        (match (db, lifecycle_ctx) with
        | Some db, Some ctx ->
            Teams_delivery_lifecycle.record_failed ~db ~room_id ~connector
              ~tracking_id:ctx.tracking_id ~task_id:task.id ~error:err
              ?thread_id ?activity_id:existing_activity_id ();
            ignore
              (Room_activity_ledger.record_delivery_failure ~db ~room_id
                 ~connector ~task_id:task.id ~error:err ?thread_id
                 ?activity_id:existing_activity_id ())
        | Some db, None ->
            ignore
              (Room_activity_ledger.record_delivery_failure ~db ~room_id
                 ~connector ~task_id:task.id ~error:err ?thread_id
                 ?activity_id:existing_activity_id ())
        | None, _ -> ());
        Lwt.return_unit)
  end

(** Format a concise final message for a terminal room-origin task. Unlike
    [format_progress_message], this includes the result preview, merge status,
    and actionable hints. Intended for edit-in-place on the last progress
    message so the room sees a single self-contained final state.

    Never includes raw logs or full task output — only truncated previews. *)
let format_final_message ?summary (task : task) =
  let status_word =
    match task.status with
    | Succeeded -> "Succeeded"
    | Failed -> "Failed"
    | DirtyWorktree -> "Dirty worktree"
    | Cancelled -> "Cancelled"
    | Queued -> "Queued"
    | Running -> "Running"
  in
  let label = task_label task in
  let elapsed = elapsed_string task in
  let merge_suffix = merge_status_suffix task in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add (Printf.sprintf "[%s%s] %s" status_word merge_suffix label);
  add (Printf.sprintf "%s (%s)" (task_label task) elapsed);
  (* Summary: prefer caller-provided, then use generic status. Never use
     result_preview directly as it can contain raw output. *)
  (match summary with
  | Some s -> add (Printf.sprintf "Summary: %s" (preview_text_n 120 s))
  | None ->
      add
        (match task.status with
        | Succeeded -> "Summary: Task completed successfully"
        | Failed -> "Summary: Task failed"
        | DirtyWorktree -> "Summary: Task finished with uncommitted changes"
        | Cancelled -> "Summary: Task was cancelled"
        | _ -> "Summary: Task in progress"));
  (* Hints — concise, actionable *)
  (match task.status with
  | Failed ->
      add
        (Printf.sprintf "Hint: `background retry %d` or `background logs %d`"
           task.id task.id)
  | DirtyWorktree ->
      add (Printf.sprintf "Hint: `background finalize %d`" task.id)
  | Succeeded -> ( match finalize_hint task with Some h -> add h | None -> ())
  | _ -> ());
  String.concat "\n" (List.rev !lines)

(** Deliver a final completion/failure message to the origin room for a terminal
    task. Edits the existing progress message in place when possible, then
    clears the tracked message ID.

    Returns [Delivered] on success, [Delivery_failed reason] on send/edit
    failure, or [Skipped] when no room_id is available. Callers should record
    the result durably. *)
let deliver_final_message ?summary
    ~(send :
       room_id:string ->
       ?thread_id:string ->
       text:string ->
       unit ->
       string Lwt.t)
    ~(edit : room_id:string -> msg_id:string -> text:string -> unit Lwt.t)
    ?(db : Sqlite3.db option) ~(task : task) () : delivery_result Lwt.t =
  let open Lwt.Syntax in
  let connector, room_id, thread_id = connector_and_room_of_task task in
  if room_id = "" then begin
    clear_progress_msg_id ~task_id:task.id;
    Lwt.return Skipped
  end
  else begin
    let text = format_final_message ?summary task in
    let existing_activity_id =
      activity_id_of_message_id (Hashtbl.find_opt progress_msg_ids task.id)
    in
    (* Record delivery attempt in ledger *)
    (match db with
    | Some db ->
        ignore
          (Room_activity_ledger.record_delivery_attempt ~db ~room_id ~connector
             ~task_id:task.id ?thread_id ?activity_id:existing_activity_id ())
    | None -> ());
    (* Generate lifecycle tracking ID for Teams connectors *)
    let lifecycle_ctx =
      match db with
      | Some db when connector = "teams" ->
          let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
          Teams_delivery_lifecycle.record_scheduled ~db ~room_id ~connector
            ~tracking_id ~task_id:task.id ?thread_id
            ?activity_id:existing_activity_id ();
          Some
            {
              db;
              room_id;
              connector;
              tracking_id;
              task_id = task.id;
              thread_id;
            }
      | _ -> None
    in
    Lwt.catch
      (fun () ->
        let* result, msg_id =
          send_or_edit ~send ~edit ~room_id ?thread_id ~text ~task_id:task.id
            ?lifecycle_ctx ()
        in
        clear_progress_msg_id ~task_id:task.id;
        let activity_id = activity_id_of_message_id msg_id in
        (* Record delivery outcome in ledger *)
        (match (db, result) with
        | Some db, Delivered ->
            ignore
              (Room_activity_ledger.record_delivery_success ~db ~room_id
                 ~connector ~task_id:task.id
                 ~message_id:(Option.value msg_id ~default:"")
                 ?thread_id ?activity_id ())
        | Some db, Delivery_failed err ->
            ignore
              (Room_activity_ledger.record_delivery_failure ~db ~room_id
                 ~connector ~task_id:task.id ~error:err ?thread_id ?activity_id
                 ())
        | Some _, Skipped -> ()
        | None, _ -> ());
        Lwt.return result)
      (fun exn ->
        clear_progress_msg_id ~task_id:task.id;
        let err = Printexc.to_string exn in
        (* Record exception-based failure in ledger *)
        (match (db, lifecycle_ctx) with
        | Some db, Some ctx ->
            Teams_delivery_lifecycle.record_failed ~db ~room_id ~connector
              ~tracking_id:ctx.tracking_id ~task_id:task.id ~error:err
              ?thread_id ?activity_id:existing_activity_id ();
            ignore
              (Room_activity_ledger.record_delivery_failure ~db ~room_id
                 ~connector ~task_id:task.id ~error:err ?thread_id
                 ?activity_id:existing_activity_id ())
        | Some db, None ->
            ignore
              (Room_activity_ledger.record_delivery_failure ~db ~room_id
                 ~connector ~task_id:task.id ~error:err ?thread_id
                 ?activity_id:existing_activity_id ())
        | None, _ -> ());
        Lwt.return (Delivery_failed err))
  end

(** {1 Adaptive Card delivery} *)

(** [send_or_edit_card] performs send/edit cycle for Adaptive Cards. Falls back
    to text if adaptive card callbacks are not available.

    When [?lifecycle_ctx] is provided (Teams connector), records granular
    lifecycle states for card sends, edits, and fallbacks. *)
let send_or_edit_card ~send_card ~edit_card ~send_text ~edit_text ~room_id
    ?thread_id ~card ~fallback_text ~task_id ?lifecycle_ctx () =
  let open Lwt.Syntax in
  let record state ?error ?message_id () =
    match lifecycle_ctx with
    | Some ctx ->
        Teams_delivery_lifecycle.record_lifecycle ~db:ctx.db
          ~room_id:ctx.room_id ~connector:ctx.connector
          ~tracking_id:ctx.tracking_id ~state ~task_id:ctx.task_id
          ?thread_id:ctx.thread_id ?error ?message_id ()
    | None -> ()
  in
  match (send_card, edit_card) with
  | Some send_card_fn, _ -> (
      match Hashtbl.find_opt progress_msg_ids task_id with
      | Some msg_id when is_valid_message_id msg_id -> (
          match edit_card with
          | Some edit_card_fn ->
              Lwt.catch
                (fun () ->
                  let* () = edit_card_fn ~room_id ~msg_id ~card () in
                  record Teams_delivery_lifecycle.Generated ();
                  record Teams_delivery_lifecycle.Attempted ();
                  record Teams_delivery_lifecycle.Transport_accepted ();
                  record Teams_delivery_lifecycle.Message_id_recorded
                    ~message_id:msg_id ();
                  Lwt.return (Delivered, Some msg_id))
                (fun exn ->
                  (* Edit failed, send new card *)
                  let edit_err = Printexc.to_string exn in
                  record Teams_delivery_lifecycle.Edit_failed ~error:edit_err ();
                  record Teams_delivery_lifecycle.Generated ();
                  record Teams_delivery_lifecycle.Attempted ();
                  let* new_id = send_card_fn ~room_id ?thread_id ~card () in
                  record Teams_delivery_lifecycle.Transport_accepted ();
                  if is_valid_message_id new_id then begin
                    record Teams_delivery_lifecycle.Fallback_sent ();
                    record Teams_delivery_lifecycle.Message_id_recorded
                      ~message_id:new_id ();
                    Hashtbl.replace progress_msg_ids task_id new_id;
                    Lwt.return (Delivered, Some new_id)
                  end
                  else begin
                    record Teams_delivery_lifecycle.User_visible_unconfirmed ();
                    Lwt.return
                      ( Delivery_failed
                          (Printf.sprintf
                             "card send failed after edit error: %s" edit_err),
                        None )
                  end)
          | None ->
              (* No edit support, send new card *)
              record Teams_delivery_lifecycle.Generated ();
              record Teams_delivery_lifecycle.Attempted ();
              let* new_id = send_card_fn ~room_id ?thread_id ~card () in
              record Teams_delivery_lifecycle.Transport_accepted ();
              if is_valid_message_id new_id then begin
                record Teams_delivery_lifecycle.Message_id_recorded
                  ~message_id:new_id ();
                Hashtbl.replace progress_msg_ids task_id new_id;
                Lwt.return (Delivered, Some new_id)
              end
              else begin
                record Teams_delivery_lifecycle.User_visible_unconfirmed ();
                Lwt.return (Delivery_failed "empty card message ID", None)
              end)
      | _ ->
          (* No existing message, send new card *)
          record Teams_delivery_lifecycle.Generated ();
          record Teams_delivery_lifecycle.Attempted ();
          let* new_id = send_card_fn ~room_id ?thread_id ~card () in
          record Teams_delivery_lifecycle.Transport_accepted ();
          if is_valid_message_id new_id then begin
            record Teams_delivery_lifecycle.Message_id_recorded
              ~message_id:new_id ();
            Hashtbl.replace progress_msg_ids task_id new_id;
            Lwt.return (Delivered, Some new_id)
          end
          else begin
            record Teams_delivery_lifecycle.User_visible_unconfirmed ();
            Lwt.return (Delivery_failed "empty card message ID", None)
          end)
  | None, _ ->
      (* No adaptive card support, fall back to text *)
      send_or_edit ~send:send_text ~edit:edit_text ~room_id ?thread_id
        ~text:fallback_text ~task_id ?lifecycle_ctx ()

(** Deliver a progress update using Adaptive Cards when available. Falls back to
    plain text for connectors without card support. Returns [true] if the card
    was delivered successfully, [false] otherwise. *)
let deliver_progress_update_with_card
    ~(send :
       room_id:string ->
       ?thread_id:string ->
       text:string ->
       unit ->
       string Lwt.t)
    ~(edit : room_id:string -> msg_id:string -> text:string -> unit Lwt.t)
    ?send_adaptive_card ?edit_adaptive_card ?(db : Sqlite3.db option)
    ?(format_checklist :
       (task_label:string ->
       elapsed:string ->
       Room_progress_checklist.checklist_item list ->
       string)
       option) ~(checklist_items : Room_progress_checklist.checklist_item list)
    ?room_policy ~(task : task) () : bool Lwt.t =
  let open Lwt.Syntax in
  let connector, room_id, thread_id = connector_and_room_of_task task in
  if room_id = "" then Lwt.return false
  else begin
    let task_label = task_label task in
    let elapsed = elapsed_string task in
    (* Build the adaptive card *)
    let card =
      Teams_progress_card.build_card ~task_id:task.id ~task_label
        ~items:checklist_items ~elapsed ?room_policy ()
    in
    let fallback_text =
      match format_checklist with
      | Some f -> f ~task_label ~elapsed checklist_items
      | None ->
          Teams_progress_card.build_fallback_text ~task_label
            ~items:checklist_items ()
    in
    let existing_activity_id =
      activity_id_of_message_id (Hashtbl.find_opt progress_msg_ids task.id)
    in
    (* Record delivery attempt in ledger *)
    (match db with
    | Some db ->
        ignore
          (Room_activity_ledger.record_delivery_attempt ~db ~room_id ~connector
             ~task_id:task.id ?thread_id ?activity_id:existing_activity_id ())
    | None -> ());
    (* Generate lifecycle tracking ID for Teams connectors *)
    let lifecycle_ctx =
      match db with
      | Some db when connector = "teams" ->
          let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
          Teams_delivery_lifecycle.record_scheduled ~db ~room_id ~connector
            ~tracking_id ~task_id:task.id ?thread_id
            ?activity_id:existing_activity_id ();
          Some
            {
              db;
              room_id;
              connector;
              tracking_id;
              task_id = task.id;
              thread_id;
            }
      | _ -> None
    in
    Lwt.catch
      (fun () ->
        let* result, msg_id =
          send_or_edit_card ~send_card:send_adaptive_card
            ~edit_card:edit_adaptive_card ~send_text:send ~edit_text:edit
            ~room_id ?thread_id ~card ~fallback_text ~task_id:task.id
            ?lifecycle_ctx ()
        in
        let activity_id = activity_id_of_message_id msg_id in
        (* Record delivery outcome in ledger *)
        (match (db, result) with
        | Some db, Delivered ->
            ignore
              (Room_activity_ledger.record_delivery_success ~db ~room_id
                 ~connector ~task_id:task.id
                 ~message_id:(Option.value msg_id ~default:"")
                 ?thread_id ?activity_id ())
        | Some db, Delivery_failed err ->
            ignore
              (Room_activity_ledger.record_delivery_failure ~db ~room_id
                 ~connector ~task_id:task.id ~error:err ?thread_id ?activity_id
                 ())
        | Some _, Skipped -> ()
        | None, _ -> ());
        Lwt.return (result = Delivered))
      (fun exn ->
        let err = Printexc.to_string exn in
        (match (db, lifecycle_ctx) with
        | Some db, Some ctx ->
            Teams_delivery_lifecycle.record_failed ~db ~room_id ~connector
              ~tracking_id:ctx.tracking_id ~task_id:task.id ~error:err
              ?thread_id ?activity_id:existing_activity_id ();
            ignore
              (Room_activity_ledger.record_delivery_failure ~db ~room_id
                 ~connector ~task_id:task.id ~error:err ?thread_id
                 ?activity_id:existing_activity_id ())
        | Some db, None ->
            ignore
              (Room_activity_ledger.record_delivery_failure ~db ~room_id
                 ~connector ~task_id:task.id ~error:err ?thread_id
                 ?activity_id:existing_activity_id ())
        | None, _ -> ());
        Lwt.return false)
  end

(** Deliver a final completion/failure message using Adaptive Cards when
    available. Falls back to plain text for connectors without card support. *)
let deliver_final_message_with_card ?summary
    ~(send :
       room_id:string ->
       ?thread_id:string ->
       text:string ->
       unit ->
       string Lwt.t)
    ~(edit : room_id:string -> msg_id:string -> text:string -> unit Lwt.t)
    ?send_adaptive_card ?edit_adaptive_card ?(db : Sqlite3.db option)
    ?(format_checklist :
       (task_label:string ->
       ?summary:string ->
       ?task_status:string ->
       Room_progress_checklist.checklist_item list ->
       string)
       option) ~(checklist_items : Room_progress_checklist.checklist_item list)
    ?room_policy ~(task_actions : Teams_progress_card.task_actions option)
    ~(task : task) () : delivery_result Lwt.t =
  let open Lwt.Syntax in
  let connector, room_id, thread_id = connector_and_room_of_task task in
  if room_id = "" then begin
    clear_progress_msg_id ~task_id:task.id;
    Lwt.return Skipped
  end
  else begin
    let task_label = task_label task in
    let elapsed = elapsed_string task in
    (* Summary: prefer caller-provided, then use generic status message.
       Never use result_preview directly as it can contain raw output. *)
    let summary_text =
      match summary with
      | Some s -> Some (preview_text_n 200 s)
      | None ->
          Some
            (match task.status with
            | Succeeded -> "Task completed successfully"
            | Failed -> "Task failed"
            | DirtyWorktree -> "Task finished with uncommitted changes"
            | Cancelled -> "Task was cancelled"
            | _ -> "Task in progress")
    in
    (* Determine task outcome for terminal state styling *)
    let task_outcome =
      match task.status with
      | Succeeded -> Some Teams_progress_card.Succeeded
      | Failed -> Some Teams_progress_card.Failed
      | DirtyWorktree -> Some Teams_progress_card.DirtyWorktree
      | Cancelled -> Some Teams_progress_card.Cancelled
      | _ -> None
    in
    (* Build the final adaptive card *)
    let card =
      Teams_progress_card.build_card ~task_id:task.id ~task_label
        ~items:checklist_items ~actions:task_actions ~elapsed
        ?summary:summary_text ?task_outcome ?room_policy ()
    in
    let fallback_text =
      match format_checklist with
      | Some f ->
          let task_status_str =
            match task.status with
            | Succeeded -> Some "succeeded"
            | Failed -> Some "failed"
            | DirtyWorktree -> Some "dirty_worktree"
            | Cancelled -> Some "cancelled"
            | _ -> None
          in
          f ~task_label ?summary ?task_status:task_status_str checklist_items
      | None -> format_final_message ?summary task
    in
    let existing_activity_id =
      activity_id_of_message_id (Hashtbl.find_opt progress_msg_ids task.id)
    in
    (* Record delivery attempt in ledger *)
    (match db with
    | Some db ->
        ignore
          (Room_activity_ledger.record_delivery_attempt ~db ~room_id ~connector
             ~task_id:task.id ?thread_id ?activity_id:existing_activity_id ())
    | None -> ());
    (* Generate lifecycle tracking ID for Teams connectors *)
    let lifecycle_ctx =
      match db with
      | Some db when connector = "teams" ->
          let tracking_id = Teams_delivery_lifecycle.generate_tracking_id () in
          Teams_delivery_lifecycle.record_scheduled ~db ~room_id ~connector
            ~tracking_id ~task_id:task.id ?thread_id
            ?activity_id:existing_activity_id ();
          Some
            {
              db;
              room_id;
              connector;
              tracking_id;
              task_id = task.id;
              thread_id;
            }
      | _ -> None
    in
    Lwt.catch
      (fun () ->
        let* result, msg_id =
          send_or_edit_card ~send_card:send_adaptive_card
            ~edit_card:edit_adaptive_card ~send_text:send ~edit_text:edit
            ~room_id ?thread_id ~card ~fallback_text ~task_id:task.id
            ?lifecycle_ctx ()
        in
        clear_progress_msg_id ~task_id:task.id;
        let activity_id = activity_id_of_message_id msg_id in
        (* Record delivery outcome in ledger *)
        (match (db, result) with
        | Some db, Delivered ->
            ignore
              (Room_activity_ledger.record_delivery_success ~db ~room_id
                 ~connector ~task_id:task.id
                 ~message_id:(Option.value msg_id ~default:"")
                 ?thread_id ?activity_id ())
        | Some db, Delivery_failed err ->
            ignore
              (Room_activity_ledger.record_delivery_failure ~db ~room_id
                 ~connector ~task_id:task.id ~error:err ?thread_id ?activity_id
                 ())
        | Some _, Skipped -> ()
        | None, _ -> ());
        Lwt.return result)
      (fun exn ->
        clear_progress_msg_id ~task_id:task.id;
        let err = Printexc.to_string exn in
        (match (db, lifecycle_ctx) with
        | Some db, Some ctx ->
            Teams_delivery_lifecycle.record_failed ~db ~room_id ~connector
              ~tracking_id:ctx.tracking_id ~task_id:task.id ~error:err
              ?thread_id ?activity_id:existing_activity_id ();
            ignore
              (Room_activity_ledger.record_delivery_failure ~db ~room_id
                 ~connector ~task_id:task.id ~error:err ?thread_id
                 ?activity_id:existing_activity_id ())
        | Some db, None ->
            ignore
              (Room_activity_ledger.record_delivery_failure ~db ~room_id
                 ~connector ~task_id:task.id ~error:err ?thread_id
                 ?activity_id:existing_activity_id ())
        | None, _ -> ());
        Lwt.return (Delivery_failed err))
  end
