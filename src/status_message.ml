type notifier = {
  send : ?parse_mode:string -> string -> string Lwt.t;
      (** Send a message, return its id *)
  edit : string -> ?parse_mode:string -> string -> string option Lwt.t;
      (** [edit msg_id ?parse_mode text] edits message with given id. Returns
          [None] for a normal in-place edit, or [Some new_id] if the
          implementation moved the message (delete + resend) and the caller
          should adopt [new_id] as the new message id. *)
  delete : string -> unit Lwt.t;  (** [delete msg_id] deletes a message *)
}
(** Operations for sending/editing/deleting messages on a channel *)

(** Tool execution state *)
type tool_state = Pending | Running | Done | Failed

type tool_entry = {
  id : string;
  name : string;
  emoji : string;
  summary : string option;
  state : tool_state;
  started_at : float;
  finished_at : float option;
  is_error : bool;
  error_detail : string option;
  result_preview : string option;
  output_tail : string option;
}
(** A tracked tool call *)

type t = {
  notifier : notifier;
  connector : Format_adapter.connector;
  mutable msg_id : string option;
  tools : (string, tool_entry) Hashtbl.t;
  tool_order : string Queue.t;
  mutable total_started : int;
  mutable total_done : int;
  mutable total_failed : int;
  mutable first_tool_at : float option;
  mutable last_edit : float;
  mutable parallel_batch_size : int;
  pending_edit : unit Lwt_condition.t;
  debounce_interval : float;
  mutable heartbeat_cancel : (unit Lwt.t * unit Lwt.u) option;
  mutable thinking_text : string;
  mutable finalized : bool;
  edit_mutex : Lwt_mutex.t;
  mutable pending_rerender : bool;
}
(** The consolidated status message state *)

let is_valid_notifier_message_id id =
  let trimmed = String.trim id in
  trimmed <> "" && trimmed <> "0"

let create ?(debounce_interval = 0.5) ~notifier ~parse_mode () =
  {
    notifier;
    connector = Format_adapter.of_parse_mode parse_mode;
    msg_id = None;
    tools = Hashtbl.create 16;
    tool_order = Queue.create ();
    total_started = 0;
    total_done = 0;
    total_failed = 0;
    first_tool_at = None;
    last_edit = 0.0;
    parallel_batch_size = 0;
    pending_edit = Lwt_condition.create ();
    debounce_interval;
    heartbeat_cancel = None;
    thinking_text = "";
    finalized = false;
    edit_mutex = Lwt_mutex.create ();
    pending_rerender = false;
  }

let format_duration secs =
  if secs < 1.0 then
    let ms = int_of_float (secs *. 1000.0) in
    Printf.sprintf "%d ms" ms
  else if secs < 10.0 then Printf.sprintf "%.3f s" secs
  else Printf.sprintf "%ds" (int_of_float secs)

let format_duration_opt secs =
  let s = format_duration secs in
  if s = "0 ms" then None else Some s

let fmt_bold t text =
  Format_adapter.bold t.connector (Format_adapter.escape t.connector text)

let fmt_italic t text =
  Format_adapter.italic t.connector (Format_adapter.escape t.connector text)

let fmt_code t text =
  Format_adapter.code t.connector (Format_adapter.escape t.connector text)

let fmt_plain t text = Format_adapter.escape t.connector text

let to_document t =
  let total = Hashtbl.length t.tools in
  if total = 0 then
    if t.thinking_text <> "" then
      [ Content_dsl.ThinkingPreview t.thinking_text ]
    else []
  else
    let entries_in_order =
      Queue.fold (fun acc id -> id :: acc) [] t.tool_order |> List.rev
    in
    let done_and_failed = ref [] in
    let running = ref [] in
    let pending = ref [] in
    List.iter
      (fun id ->
        match Hashtbl.find_opt t.tools id with
        | None -> ()
        | Some entry -> (
            match entry.state with
            | Done | Failed -> done_and_failed := entry :: !done_and_failed
            | Running -> running := entry :: !running
            | Pending -> pending := entry :: !pending))
      entries_in_order;
    let all_done_list = List.rev !done_and_failed in
    let running_list = List.rev !running in
    let pending_list = List.rev !pending in
    let all_done =
      List.length running_list = 0
      && List.length pending_list = 0
      && total = t.total_done + t.total_failed
    in
    let n_done = List.length all_done_list in
    let collapsed_count, visible_done =
      if n_done > 8 && not t.finalized then
        let to_collapse = n_done - 2 in
        let visible =
          List.filteri (fun i _ -> i >= to_collapse) all_done_list
        in
        (to_collapse, visible)
      else (0, all_done_list)
    in
    let doc = ref [] in
    if collapsed_count > 0 then
      doc := Content_dsl.CollapsedTools { count = collapsed_count } :: !doc;
    List.iter
      (fun (entry : tool_entry) ->
        match entry.state with
        | Done ->
            let timing =
              match entry.finished_at with
              | Some fin -> format_duration_opt (fin -. entry.started_at)
              | None -> None
            in
            doc :=
              Content_dsl.ToolEntry
                {
                  emoji = entry.emoji;
                  name = entry.name;
                  summary = entry.summary;
                  state = Content_dsl.Done;
                  timing;
                  preview = entry.result_preview;
                  error_detail = None;
                  connector_char = None;
                }
              :: !doc
        | Failed ->
            let timing =
              match entry.finished_at with
              | Some fin -> format_duration_opt (fin -. entry.started_at)
              | None -> None
            in
            doc :=
              Content_dsl.ToolEntry
                {
                  emoji = entry.emoji;
                  name = entry.name;
                  summary = entry.summary;
                  state = Content_dsl.Failed;
                  timing;
                  preview = None;
                  error_detail = entry.error_detail;
                  connector_char = None;
                }
              :: !doc
        | _ -> ())
      visible_done;
    let active_items = running_list @ pending_list in
    let n_active = List.length active_items in
    let active_idx = ref 0 in
    List.iter
      (fun (entry : tool_entry) ->
        incr active_idx;
        let connector_char =
          if !active_idx = n_active then "\xE2\x94\x97 " else "\xE2\x94\xA3 "
        in
        let elapsed = Unix.gettimeofday () -. entry.started_at in
        let timing =
          if elapsed > 2.0 then Some (format_duration elapsed ^ "...") else None
        in
        doc :=
          Content_dsl.ToolEntry
            {
              emoji = entry.emoji;
              name = entry.name;
              summary = entry.summary;
              state = Content_dsl.Running;
              timing;
              preview = None;
              error_detail = None;
              connector_char = Some connector_char;
            }
          :: !doc)
      running_list;
    List.iter
      (fun (entry : tool_entry) ->
        incr active_idx;
        let connector_char =
          if !active_idx = n_active then "\xE2\x94\x97 " else "\xE2\x94\xA3 "
        in
        doc :=
          Content_dsl.ToolEntry
            {
              emoji = entry.emoji;
              name = entry.name;
              summary = None;
              state = Content_dsl.Pending;
              timing = None;
              preview = None;
              error_detail = None;
              connector_char = Some connector_char;
            }
          :: !doc)
      pending_list;
    if List.length running_list > 0 && total > 1 then begin
      let done_count = t.total_done + t.total_failed in
      doc := Content_dsl.ProgressBar { filled = 0; total; done_count } :: !doc
    end;
    if all_done && total >= 4 then begin
      let emoji_counts = Hashtbl.create 8 in
      List.iter
        (fun id ->
          match Hashtbl.find_opt t.tools id with
          | None -> ()
          | Some entry ->
              let cur =
                match Hashtbl.find_opt emoji_counts entry.emoji with
                | Some n -> n
                | None -> 0
              in
              Hashtbl.replace emoji_counts entry.emoji (cur + 1))
        entries_in_order;
      let emoji_breakdown =
        Hashtbl.fold
          (fun emoji count acc ->
            Printf.sprintf "%s\xC3\x97%d" emoji count :: acc)
          emoji_counts []
        |> List.sort String.compare |> String.concat " "
      in
      let total_time =
        let last_finish =
          Hashtbl.fold
            (fun _ (entry : tool_entry) acc ->
              match entry.finished_at with Some f -> max acc f | None -> acc)
            t.tools 0.0
        in
        match t.first_tool_at with
        | Some start when last_finish > start ->
            format_duration (last_finish -. start)
        | Some start -> format_duration (Unix.gettimeofday () -. start)
        | None -> "0s"
      in
      let parallel_indicator =
        if t.parallel_batch_size > 1 then " \xC2\xB7 \xF0\x9F\x94\x80" else ""
      in
      doc :=
        Content_dsl.ToolSummary
          { total; emoji_breakdown; parallel_indicator; total_time }
        :: !doc
    end;
    List.rev !doc

let render t =
  let doc = to_document t in
  if doc = [] then "" else Content_dsl.render_document t.connector doc

let has_active_tools t =
  Hashtbl.fold
    (fun _ (e : tool_entry) acc ->
      acc
      || match e.state with Running | Pending -> true | Done | Failed -> false)
    t.tools false

(* Debounced send or edit, serialized with a mutex to prevent duplicate sends.
   When callers arrive while an edit is in-flight, they mark a pending rerender
   instead of queueing up — the mutex holder drains pending rerenders before
   releasing, so rapid-fire updates coalesce without leaving a stale queued
   render behind. *)
let send_or_edit t =
  if Lwt_mutex.is_locked t.edit_mutex then begin
    t.pending_rerender <- true;
    Lwt.return_unit
  end
  else
    Lwt_util.with_lock_timeout ~label:"status_edit"
      ~fatal_timeout:Lwt_util.short_fatal_timeout t.edit_mutex (fun () ->
        let open Lwt.Syntax in
        let do_send_or_edit () =
          let now = Unix.gettimeofday () in
          let elapsed = now -. t.last_edit in
          let* () =
            if elapsed < t.debounce_interval && t.msg_id <> None then
              let delay = t.debounce_interval -. elapsed in
              Lwt_unix.sleep delay
            else Lwt.return_unit
          in
          let text = render t in
          if text = "" then Lwt.return_unit
          else
            match t.msg_id with
            | None ->
                let* id =
                  t.notifier.send
                    ~parse_mode:(Format_adapter.parse_mode_string t.connector)
                    text
                in
                if is_valid_notifier_message_id id then t.msg_id <- Some id;
                t.last_edit <- Unix.gettimeofday ();
                Lwt.return_unit
            | Some id ->
                let* new_id_opt =
                  t.notifier.edit id
                    ~parse_mode:(Format_adapter.parse_mode_string t.connector)
                    text
                in
                (match new_id_opt with
                | Some new_id when is_valid_notifier_message_id new_id ->
                    t.msg_id <- Some new_id
                | Some _ -> ()
                | None -> ());
                t.last_edit <- Unix.gettimeofday ();
                Lwt.return_unit
        in
        let rec drain () =
          let* () = do_send_or_edit () in
          if t.pending_rerender then begin
            t.pending_rerender <- false;
            drain ()
          end
          else Lwt.return_unit
        in
        drain ())

(* Idempotent heartbeat manager: starts a heartbeat if any tools are Running
   and none exists, cancels it if no tools are Running. Safe to call multiple
   times — must be called BEFORE any yield point (send_or_edit) so the
   heartbeat is registered before concurrent tool_result can fire. *)
let ensure_heartbeat t =
  let has_running =
    Hashtbl.fold
      (fun _ (e : tool_entry) acc -> acc || e.state = Running)
      t.tools false
  in
  if has_running then begin
    (* Need a heartbeat — start one if not already active *)
    if t.heartbeat_cancel = None && t.debounce_interval > 0.0 then begin
      let cancel_p, cancel_u = Lwt.wait () in
      t.heartbeat_cancel <- Some (cancel_p, cancel_u);
      Lwt.async (fun () ->
          let rec loop () =
            let open Lwt.Syntax in
            (* Use Lwt.choose (not Lwt.pick) to avoid cancelling cancel_p *)
            let* () = Lwt.choose [ Lwt_unix.sleep 5.0; cancel_p ] in
            if Lwt.is_sleeping cancel_p then
              let still_running =
                Hashtbl.fold
                  (fun _ (e : tool_entry) acc -> acc || e.state = Running)
                  t.tools false
              in
              if still_running then
                let* () = send_or_edit t in
                loop ()
              else begin
                t.heartbeat_cancel <- None;
                Lwt.return_unit
              end
            else begin
              t.heartbeat_cancel <- None;
              Lwt.return_unit
            end
          in
          loop ())
    end
  end
  else begin
    (* No running tools — cancel any existing heartbeat *)
    match t.heartbeat_cancel with
    | Some (cancel_p, u) ->
        if Lwt.is_sleeping cancel_p then Lwt.wakeup_later u ();
        t.heartbeat_cancel <- None
    | None -> ()
  end

let tool_start t ~id ~name ~summary =
  let open Lwt.Syntax in
  let now = Unix.gettimeofday () in
  let emoji = Stream_visibility.tool_emoji name in
  let entry =
    {
      id;
      name;
      emoji;
      summary;
      state = Running;
      started_at = now;
      finished_at = None;
      is_error = false;
      error_detail = None;
      result_preview = None;
      output_tail = None;
    }
  in
  Hashtbl.replace t.tools id entry;
  Queue.push id t.tool_order;
  t.total_started <- t.total_started + 1;
  (match t.first_tool_at with
  | None -> t.first_tool_at <- Some now
  | Some _ -> ());
  (* Track parallel execution: count currently running tools *)
  let running_count =
    Hashtbl.fold
      (fun _ (e : tool_entry) acc ->
        match e.state with Running -> acc + 1 | _ -> acc)
      t.tools 0
  in
  if running_count > t.parallel_batch_size then
    t.parallel_batch_size <- running_count;
  (* Ensure heartbeat is active BEFORE yielding to send_or_edit,
     so concurrent tool_result can cancel it *)
  ensure_heartbeat t;
  let* () = send_or_edit t in
  Lwt.return_unit

let last_n_lines ~n text =
  let lines = String.split_on_char '\n' (String.trim text) in
  let len = List.length lines in
  if len <= n then lines
  else
    let to_drop = len - n in
    let rec drop k = function
      | [] -> []
      | _ :: rest when k > 0 -> drop (k - 1) rest
      | l -> l
    in
    drop to_drop lines

let tool_result t ~id ~name:_ ~result ~is_error =
  let open Lwt.Syntax in
  let now = Unix.gettimeofday () in
  (match Hashtbl.find_opt t.tools id with
  | Some entry ->
      let error_detail =
        if is_error then
          Some (Stream_visibility.truncate_text ~max_chars:200 result)
        else None
      in
      let result_preview =
        if is_error then None
        else Stream_visibility.summarize_tool_result ~name:entry.name result
      in
      let output_tail =
        match entry.name with
        | "shell_exec" when not is_error ->
            let trimmed = String.trim result in
            if trimmed = "" then None
            else
              let tail = last_n_lines ~n:4 trimmed in
              let text =
                String.concat "\n"
                  (List.map
                     (fun l -> Stream_visibility.truncate_text ~max_chars:60 l)
                     tail)
              in
              Some text
        | _ -> None
      in
      let updated =
        {
          entry with
          state = (if is_error then Failed else Done);
          finished_at = Some now;
          is_error;
          error_detail;
          result_preview;
          output_tail;
        }
      in
      Hashtbl.replace t.tools id updated;
      if is_error then t.total_failed <- t.total_failed + 1
      else t.total_done <- t.total_done + 1
  | None ->
      (* Tool wasn't tracked via tool_start; ignore *)
      ());
  (* Cancel or preserve heartbeat based on current running state *)
  ensure_heartbeat t;
  let* () = send_or_edit t in
  Lwt.return_unit

let update_thinking t text =
  t.thinking_text <- text;
  if t.msg_id <> None || String.length text > 20 then send_or_edit t
  else Lwt.return_unit

let finalize t =
  if t.finalized then Lwt.return_unit
  else begin
    (* Cancel any remaining heartbeat *)
    (match t.heartbeat_cancel with
    | Some (cancel_p, u) ->
        if Lwt.is_sleeping cancel_p then Lwt.wakeup_later u ();
        t.heartbeat_cancel <- None
    | None -> ());
    let open Lwt.Syntax in
    let total = Hashtbl.length t.tools in
    if total = 0 then (
      match t.msg_id with
      | Some id ->
          t.finalized <- true;
          let* () = t.notifier.delete id in
          t.msg_id <- None;
          Lwt.return_unit
      | None ->
          t.finalized <- true;
          Lwt.return_unit)
    else if total >= 4 || has_active_tools t then (
      t.finalized <- true;
      send_or_edit t)
    else (
      t.finalized <- true;
      Lwt.return_unit)
  end

let get_tool_info t ~id = Hashtbl.find_opt t.tools id
