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
  if secs < 10.0 then Printf.sprintf "%.1fs" secs
  else Printf.sprintf "%ds" (int_of_float secs)

let fmt_bold t text =
  Format_adapter.bold t.connector (Format_adapter.escape t.connector text)

let fmt_italic t text =
  Format_adapter.italic t.connector (Format_adapter.escape t.connector text)

let fmt_code t text =
  Format_adapter.code t.connector (Format_adapter.escape t.connector text)

let fmt_plain t text = Format_adapter.escape t.connector text

let render t =
  let buf = Buffer.create 256 in
  let total = Hashtbl.length t.tools in
  if total = 0 then
    if t.thinking_text <> "" then
      Printf.sprintf "\xF0\x9F\x92\xAD %s"
        (fmt_italic t
           (Stream_visibility.truncate_text ~max_chars:200 t.thinking_text))
    else ""
  else
    let entries_in_order =
      Queue.fold (fun acc id -> id :: acc) [] t.tool_order |> List.rev
    in
    (* Separate entries by state *)
    let completed = ref [] in
    let failed = ref [] in
    let running = ref [] in
    let pending = ref [] in
    List.iter
      (fun id ->
        match Hashtbl.find_opt t.tools id with
        | None -> ()
        | Some entry -> (
            match entry.state with
            | Done -> completed := entry :: !completed
            | Failed -> failed := entry :: !failed
            | Running -> running := entry :: !running
            | Pending -> pending := entry :: !pending))
      entries_in_order;
    let completed = List.rev !completed in
    let failed = List.rev !failed in
    let running = List.rev !running in
    let pending = List.rev !pending in
    let all_done =
      List.length running = 0
      && List.length pending = 0
      && total = t.total_done + t.total_failed
    in
    (* Collapsing: if more than 8 completed, collapse all but last 2 *)
    let n_completed = List.length completed in
    let collapsed_count, visible_completed =
      if n_completed > 8 && not t.finalized then
        let to_collapse = n_completed - 2 in
        let visible = List.filteri (fun i _ -> i >= to_collapse) completed in
        (to_collapse, visible)
      else (0, completed)
    in
    (* Render collapsed line *)
    if collapsed_count > 0 then
      Buffer.add_string buf
        (Printf.sprintf "\xE2\x9C\x93 %d tools completed\n" collapsed_count);
    (* Render visible completed *)
    List.iter
      (fun (entry : tool_entry) ->
        let timing =
          match entry.finished_at with
          | Some fin ->
              let dur = fin -. entry.started_at in
              if dur > 1.0 then " " ^ format_duration dur else ""
          | None -> ""
        in
        let summary_part =
          match entry.summary with
          | Some s -> Printf.sprintf " \xE2\x80\x94 %s" (fmt_code t s)
          | None -> ""
        in
        let preview_part =
          match entry.result_preview with
          | Some p -> Printf.sprintf " \xE2\x86\x92 %s" (fmt_italic t p)
          | None -> ""
        in
        Buffer.add_string buf
          (Printf.sprintf "\xE2\x9C\x93 %s %s%s%s%s\n" entry.emoji
             (fmt_bold t entry.name) summary_part preview_part
             (fmt_plain t timing)))
      visible_completed;
    (* Render failed (always expanded) *)
    List.iter
      (fun (entry : tool_entry) ->
        let summary_part =
          match entry.summary with
          | Some s -> Printf.sprintf " \xE2\x80\x94 %s" (fmt_code t s)
          | None -> ""
        in
        let error_part =
          match entry.error_detail with
          | Some err -> Printf.sprintf "\n  \xE2\x94\x94 %s" (fmt_italic t err)
          | None -> ""
        in
        Buffer.add_string buf
          (Printf.sprintf "\xE2\x9C\x97 %s %s%s%s\n" entry.emoji
             (fmt_bold t entry.name) summary_part error_part))
      failed;
    (* Render running with box drawing *)
    let active_items = running @ pending in
    let n_active = List.length active_items in
    let active_idx = ref 0 in
    List.iter
      (fun (entry : tool_entry) ->
        incr active_idx;
        let connector =
          if !active_idx = n_active then "\xE2\x94\x97 " (* ┗ *)
          else "\xE2\x94\xA3 " (* ┣ *)
        in
        let summary_part =
          match entry.summary with
          | Some s -> Printf.sprintf " \xE2\x80\x94 %s" (fmt_code t s)
          | None -> ""
        in
        let elapsed = Unix.gettimeofday () -. entry.started_at in
        let timing =
          if elapsed > 5.0 then " " ^ format_duration elapsed ^ "..." else ""
        in
        Buffer.add_string buf
          (Printf.sprintf "%s\xE2\x97\x89 %s %s%s%s\n" connector entry.emoji
             (fmt_bold t entry.name) summary_part (fmt_plain t timing)))
      running;
    List.iter
      (fun (entry : tool_entry) ->
        incr active_idx;
        let connector =
          if !active_idx = n_active then "\xE2\x94\x97 " (* ┗ *)
          else "\xE2\x94\xA3 " (* ┣ *)
        in
        Buffer.add_string buf
          (Printf.sprintf "%s\xE2\x97\x8B %s %s\n" connector entry.emoji
             entry.name))
      pending;
    (* Progress bar + counter *)
    (if List.length running > 0 && total > 1 then
       let done_count = t.total_done + t.total_failed in
       let bar_width = 8 in
       let filled = if total > 0 then done_count * bar_width / total else 0 in
       let empty = bar_width - filled in
       let repeat n s =
         let buf = Buffer.create (n * String.length s) in
         for _ = 1 to n do
           Buffer.add_string buf s
         done;
         Buffer.contents buf
       in
       let bar = repeat filled "\xE2\x96\x93" ^ repeat empty "\xE2\x96\x91" in
       Buffer.add_string buf (Printf.sprintf "%s %d/%d\n" bar done_count total));
    (* Summary footer when all done and total >= 4 *)
    if all_done && total >= 4 then (
      (* Build emoji breakdown *)
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
        match t.first_tool_at with
        | Some start -> format_duration (Unix.gettimeofday () -. start)
        | None -> "0s"
      in
      let parallel_indicator =
        if t.parallel_batch_size > 1 then " \xC2\xB7 \xF0\x9F\x94\x80" else ""
      in
      Buffer.add_string buf
        (Printf.sprintf
           "\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\xE2\x94\x81\n\
            \xF0\x9F\x9B\xA0\xEF\xB8\x8F %d tools \xC2\xB7 %s%s \xC2\xB7 %s"
           total emoji_breakdown parallel_indicator total_time));
    (* Trim trailing newline *)
    let result = Buffer.contents buf in
    let len = String.length result in
    if len > 0 && result.[len - 1] = '\n' then String.sub result 0 (len - 1)
    else result

(* Debounced send or edit, serialized with a mutex to prevent duplicate sends.
   When callers arrive while an edit is in-flight, they mark a pending rerender
   instead of queueing up — the mutex holder drains pending rerenders before
   releasing, so rapid-fire updates coalesce into a single API call. *)
let send_or_edit t =
  if Lwt_mutex.is_locked t.edit_mutex then begin
    t.pending_rerender <- true;
    Lwt.return_unit
  end
  else
    Lwt_mutex.with_lock t.edit_mutex (fun () ->
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
                t.msg_id <- Some id;
                t.last_edit <- Unix.gettimeofday ();
                Lwt.return_unit
            | Some id ->
                let* new_id_opt =
                  t.notifier.edit id
                    ~parse_mode:(Format_adapter.parse_mode_string t.connector)
                    text
                in
                (match new_id_opt with
                | Some new_id -> t.msg_id <- Some new_id
                | None -> ());
                t.last_edit <- Unix.gettimeofday ();
                Lwt.return_unit
        in
        let* () = do_send_or_edit () in
        (* If updates arrived while we held the lock, do one final render
           to pick up the latest state. No loop — just the most recent. *)
        if t.pending_rerender then begin
          t.pending_rerender <- false;
          do_send_or_edit ()
        end
        else Lwt.return_unit)

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
  let* () = send_or_edit t in
  (* Cancel any existing heartbeat *)
  (match t.heartbeat_cancel with
  | Some (cancel_p, u) ->
      if Lwt.is_sleeping cancel_p then Lwt.wakeup_later u ();
      t.heartbeat_cancel <- None
  | None -> ());
  (* Start new heartbeat if debounce > 0 *)
  if t.debounce_interval > 0.0 then begin
    let cancel_p, cancel_u = Lwt.wait () in
    t.heartbeat_cancel <- Some (cancel_p, cancel_u);
    Lwt.async (fun () ->
        let rec loop () =
          let open Lwt.Syntax in
          let* () = Lwt.pick [ Lwt_unix.sleep 5.0; cancel_p ] in
          if Lwt.is_sleeping cancel_p then
            let* () = send_or_edit t in
            loop ()
          else Lwt.return_unit
        in
        loop ())
  end;
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
  (* Cancel heartbeat *)
  (match t.heartbeat_cancel with
  | Some (cancel_p, u) ->
      if Lwt.is_sleeping cancel_p then Lwt.wakeup_later u ();
      t.heartbeat_cancel <- None
  | None -> ());
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
  let* () = send_or_edit t in
  Lwt.return_unit

let update_thinking t text =
  t.thinking_text <- text;
  if t.msg_id <> None || String.length text > 20 then send_or_edit t
  else Lwt.return_unit

let finalize t =
  let open Lwt.Syntax in
  let total = Hashtbl.length t.tools in
  if total = 0 then
    match t.msg_id with
    | Some id ->
        let* () = t.notifier.delete id in
        t.msg_id <- None;
        Lwt.return_unit
    | None -> Lwt.return_unit
  else if total >= 4 then (
    t.finalized <- true;
    let text = render t in
    match t.msg_id with
    | Some id ->
        let* new_id_opt =
          t.notifier.edit id
            ~parse_mode:(Format_adapter.parse_mode_string t.connector)
            text
        in
        (match new_id_opt with
        | Some new_id -> t.msg_id <- Some new_id
        | None -> ());
        Lwt.return_unit
    | None -> Lwt.return_unit)
  else (
    t.finalized <- true;
    Lwt.return_unit)
