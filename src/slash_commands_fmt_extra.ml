(** Extended format helpers for less-frequently-used slash commands.

    Extracted from {!Slash_commands_fmt} to keep that module within size limits.
    [include]-d back into {!Slash_commands_fmt} so all names remain accessible
    under the [Slash_commands] namespace. *)

type cron_action =
  | CronList
  | CronAdd of {
      name : string;
      schedule : string;
      message : string;
      ttl : string option;
    }
  | CronEdit of {
      name : string;
      schedule : string option;
      message : string option;
      ttl : string option;
    }
  | CronRemove of string
  | CronShow of string
  | CronHistory of string option
  | CronTrigger of string
  | CronHelp

type memories_action = { oldest : bool; page : int }

type room_memory_action =
  | RoomMemoryList
  | RoomMemoryShow of string
  | RoomMemorySave of string * string
  | RoomMemoryCorrect of string * string
  | RoomMemoryForget of string * string list

let risk_level_string (r : Tool.risk_level) =
  match r with Low -> "Low" | Medium -> "Medium" | High -> "High"

let extract_params (schema : Yojson.Safe.t) : (string * string * bool) list =
  let open Yojson.Safe.Util in
  let props = try schema |> member "properties" |> to_assoc with _ -> [] in
  let required =
    try schema |> member "required" |> to_list |> List.map to_string
    with _ -> []
  in
  List.map
    (fun (name, v) ->
      let typ = try v |> member "type" |> to_string with _ -> "string" in
      let is_required = List.mem name required in
      (name, typ, is_required))
    props

let truncate_description desc max_len =
  if String.length desc <= max_len then desc
  else String.sub desc 0 (max_len - 3) ^ "..."

let items_per_menu_page = 9

let paginate_items items page =
  let total = List.length items in
  let total_pages =
    max 1 ((total + items_per_menu_page - 1) / items_per_menu_page)
  in
  let page = max 1 (min page total_pages) in
  let start_idx = (page - 1) * items_per_menu_page in
  let page_items =
    List.filteri
      (fun i _ -> i >= start_idx && i < start_idx + items_per_menu_page)
      items
  in
  (page_items, page, total_pages)

let pagination_footer ~connector ~cmd page total_pages =
  if total_pages <= 1 then ""
  else
    let prev =
      if page > 1 then
        Format_adapter.code connector (Printf.sprintf "%s %d" cmd (page - 1))
        ^ " << "
      else ""
    in
    let next =
      if page < total_pages then
        " >> "
        ^ Format_adapter.code connector (Printf.sprintf "%s %d" cmd (page + 1))
      else ""
    in
    Printf.sprintf "\n\nPage %d/%d  %s%s" page total_pages prev next

let format_cron_usage ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/cron"
  ^ " [list/show/add/edit/remove/history]\n  "
  ^ Format_adapter.code connector "/cron"
  ^ "                                    \xe2\x80\x94 List all cron jobs\n  "
  ^ Format_adapter.code connector "/cron list"
  ^ "                               \xe2\x80\x94 List all cron jobs\n  "
  ^ Format_adapter.code connector "/cron show <name>"
  ^ "                        \xe2\x80\x94 Show job details\n  "
  ^ Format_adapter.code connector
      "/cron add <name> <schedule> <message> [--ttl <duration>]"
  ^ " \xe2\x80\x94 Create a cron job\n  "
  ^ Format_adapter.code connector
      "/cron edit <name> --schedule <expr> [--ttl <duration>]"
  ^ " \xe2\x80\x94 Edit schedule\n  "
  ^ Format_adapter.code connector
      "/cron edit <name> --message <text> [--ttl <duration>]"
  ^ "  \xe2\x80\x94 Edit message\n  "
  ^ Format_adapter.code connector "/cron remove <name>"
  ^ "                      \xe2\x80\x94 Remove a cron job\n  "
  ^ Format_adapter.code connector "/cron trigger <name>"
  ^ "                     \xe2\x80\x94 Trigger a job immediately\n  "
  ^ Format_adapter.code connector "/cron history [name]"
  ^ "                     \xe2\x80\x94 Show recent run history\n\n\
     Schedule formats: cron expression (e.g. "
  ^ Format_adapter.code connector "\"*/5 * * * *\""
  ^ ") or interval (e.g. "
  ^ Format_adapter.code connector "\"every 30m\""
  ^ ")"

let format_cron_confirm ~connector action name =
  Format_adapter.bold connector (String.capitalize_ascii action)
  ^ " cron job "
  ^ Format_adapter.code connector (Printf.sprintf "'%s'" name)
  ^ "."

(* ── Existing format: tools ────────────────────────────────────────────── *)

let format_tool_plain buf (t : Tool.t) =
  Buffer.add_char buf '\n';
  Buffer.add_string buf
    (Printf.sprintf "%s [%s]\n" t.name (risk_level_string t.risk_level));
  Buffer.add_string buf (Printf.sprintf "  %s\n" t.description);
  let params = extract_params t.parameters_schema in
  if params <> [] then
    let param_strs =
      List.map
        (fun (name, typ, req) ->
          if req then Printf.sprintf "%s* (%s)" name typ
          else Printf.sprintf "%s (%s)" name typ)
        params
    in
    Buffer.add_string buf
      (Printf.sprintf "  Args: %s\n" (String.concat ", " param_strs))

let format_tools_plain (tools : Tool.t list) (skills : Tool.t list)
    (agents : Agent_template.t list) : string =
  let sort_tools ts =
    List.sort (fun (a : Tool.t) b -> String.compare a.name b.name) ts
  in
  let sorted = sort_tools tools in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (Printf.sprintf "Tools (%d):\n" (List.length sorted));
  List.iter (format_tool_plain buf) sorted;
  if skills <> [] then begin
    let sorted_skills = sort_tools skills in
    Buffer.add_string buf
      (Printf.sprintf "\n\nSkills (%d):\n" (List.length sorted_skills));
    List.iter (format_tool_plain buf) sorted_skills
  end;
  if agents <> [] then begin
    Buffer.add_string buf
      (Printf.sprintf "\n\nAgents (%d):\n" (List.length agents));
    List.iter
      (fun (t : Agent_template.t) ->
        Buffer.add_string buf
          (Printf.sprintf "\n@%s\n  %s\n" t.name t.description))
      agents
  end;
  Buffer.contents buf

let format_tool_telegram buf (t : Tool.t) =
  let params = extract_params t.parameters_schema in
  let param_str =
    if params = [] then ""
    else
      let names =
        List.map (fun (name, _, req) -> if req then name ^ "*" else name) params
      in
      " <code>" ^ String.concat " " names ^ "</code>"
  in
  Buffer.add_string buf (Printf.sprintf "<b>%s</b>%s\n" t.name param_str);
  Buffer.add_string buf (truncate_description t.description 60 ^ "\n\n")

let format_tools_telegram (tools : Tool.t list) (skills : Tool.t list)
    (agents : Agent_template.t list) : string =
  let sort_tools ts =
    List.sort (fun (a : Tool.t) b -> String.compare a.name b.name) ts
  in
  let sorted = sort_tools tools in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Printf.sprintf "<b>Tools (%d)</b>\n\n" (List.length sorted));
  if sorted <> [] then begin
    Buffer.add_string buf "<blockquote expandable>\n";
    List.iter (format_tool_telegram buf) sorted;
    Buffer.add_string buf "</blockquote>"
  end;
  if skills <> [] then begin
    let sorted_skills = sort_tools skills in
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Printf.sprintf "<b>Skills (%d)</b>\n\n" (List.length sorted_skills));
    Buffer.add_string buf "<blockquote expandable>\n";
    List.iter (format_tool_telegram buf) sorted_skills;
    Buffer.add_string buf "</blockquote>"
  end;
  if agents <> [] then begin
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Printf.sprintf "<b>Agents (%d)</b>\n\n" (List.length agents));
    Buffer.add_string buf "<blockquote expandable>\n";
    List.iter
      (fun (t : Agent_template.t) ->
        Buffer.add_string buf
          (Printf.sprintf "<b>@%s</b>\n%s\n\n" t.name t.description))
      agents;
    Buffer.add_string buf "</blockquote>"
  end;
  Buffer.contents buf

let format_tools_table ~connector (tools : Tool.t list) (skills : Tool.t list)
    (agents : Agent_template.t list) =
  let sort_tools ts =
    List.sort (fun (a : Tool.t) b -> String.compare a.name b.name) ts
  in
  let columns =
    Table_format.
      [
        { header = "Tool"; align = Left; min_width = 0; flex = false };
        { header = "Risk"; align = Left; min_width = 0; flex = false };
        { header = "Description"; align = Left; min_width = 0; flex = true };
      ]
  in
  let tool_rows =
    List.map
      (fun (t : Tool.t) ->
        [
          t.name;
          risk_level_string t.risk_level;
          truncate_description t.description 60;
        ])
      (sort_tools tools)
  in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Format_adapter.bold connector
       (Printf.sprintf "Tools (%d)" (List.length tools)));
  Buffer.add_string buf "\n\n";
  Buffer.add_string buf
    (Format_adapter.render_table connector ~max_width:80 columns tool_rows);
  if skills <> [] then begin
    let skill_rows =
      List.map
        (fun (t : Tool.t) ->
          [
            t.name;
            risk_level_string t.risk_level;
            truncate_description t.description 60;
          ])
        (sort_tools skills)
    in
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.bold connector
         (Printf.sprintf "Skills (%d)" (List.length skills)));
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.render_table connector ~max_width:80 columns skill_rows)
  end;
  if agents <> [] then begin
    let agent_columns =
      Table_format.
        [
          { header = "Name"; align = Left; min_width = 0; flex = false };
          { header = "Description"; align = Left; min_width = 0; flex = true };
        ]
    in
    let agent_rows =
      List.map
        (fun (t : Agent_template.t) ->
          [ "@" ^ t.name; truncate_description t.description 60 ])
        agents
    in
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.bold connector
         (Printf.sprintf "Agents (%d)" (List.length agents)));
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.render_table connector ~max_width:80 agent_columns
         agent_rows)
  end;
  Buffer.contents buf

let format_tools ~connector tools skills agents =
  match connector with
  | Format_adapter.Telegram_html -> format_tools_telegram tools skills agents
  | Format_adapter.Plain -> format_tools_plain tools skills agents
  | _ -> format_tools_table ~connector tools skills agents

(* ── Memories ──────────────────────────────────────────────────────────── *)

let format_memories_usage ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/memories"
  ^ " [oldest/newest] [page]\n  "
  ^ Format_adapter.code connector "/memories"
  ^ "          \xe2\x80\x94 List memories, most recently updated first\n  "
  ^ Format_adapter.code connector "/memories oldest"
  ^ "   \xe2\x80\x94 List memories, oldest first\n  "
  ^ Format_adapter.code connector "/memories 2"
  ^ "        \xe2\x80\x94 Show page 2\n\nRoom memory (current channel):\n  "
  ^ Format_adapter.code connector "/memory list"
  ^ "            \xe2\x80\x94 List room memories\n  "
  ^ Format_adapter.code connector "/memory show <id>"
  ^ "       \xe2\x80\x94 Show room memory details\n  "
  ^ Format_adapter.code connector "/memory save <ref> <content>"
  ^ "\n  "
  ^ Format_adapter.code connector "/memory correct <id> <content>"
  ^ "\n  "
  ^ Format_adapter.code connector "/memory forget <id> [reason]"

(* Collapse whitespace/newlines to a single line, then truncate for preview. *)
let memory_preview content =
  let buf = Buffer.create (String.length content) in
  let prev_space = ref false in
  String.iter
    (fun c ->
      let c = match c with '\n' | '\r' | '\t' -> ' ' | _ -> c in
      if c = ' ' then begin
        if not !prev_space then Buffer.add_char buf ' ';
        prev_space := true
      end
      else begin
        Buffer.add_char buf c;
        prev_space := false
      end)
    content;
  truncate_description (String.trim (Buffer.contents buf)) 60

let format_unix_date seconds =
  if seconds <= 0 then "-"
  else
    let tm = Unix.gmtime (float_of_int seconds) in
    Printf.sprintf "%04d-%02d-%02d" (1900 + tm.tm_year) (1 + tm.tm_mon)
      tm.tm_mday

let format_memories ~connector ~db { oldest; page } =
  let items = Memory.list_core_with_meta ~db ~oldest () in
  if items = [] then "No memories stored yet."
  else
    let page_items, page, total_pages = paginate_items items page in
    let lines =
      List.map
        (fun (key, content, cat, updated) ->
          Format_adapter.bold connector (Format_adapter.escape connector key)
          ^ " ["
          ^ Format_adapter.escape connector cat
          ^ "] " ^ format_unix_date updated ^ "\n"
          ^ Format_adapter.escape connector (memory_preview content))
        page_items
    in
    let header =
      Format_adapter.bold connector
        (Printf.sprintf "Memories (%d total)" (List.length items))
    in
    let order_label =
      if oldest then " \xe2\x80\x94 oldest first"
      else " \xe2\x80\x94 newest first"
    in
    let cmd = if oldest then "/memories oldest" else "/memories" in
    header ^ order_label ^ "\n\n" ^ String.concat "\n\n" lines
    ^ pagination_footer ~connector ~cmd page total_pages

(* ── Room Memory ───────────────────────────────────────────────────────── *)

(** Resolve room_id from channel_id, ensuring it has a memory scope. *)
let resolve_room_scope ~db ~cfg ~channel_id ~is_admin () =
  let reconcile_error =
    try
      ignore (Memory.reconcile_room_profiles ~db ~config:cfg);
      None
    with exn ->
      Some
        (Printf.sprintf "Error reconciling room profile bindings: %s"
           (Printexc.to_string exn))
  in
  match reconcile_error with
  | Some msg -> Error msg
  | None -> (
      let binding = Memory.get_room_profile_binding ~db ~room_id:channel_id in
      match binding with
      | None ->
          Error
            "No room profile binding found for this channel. Bind a room \
             profile first."
      | Some b -> (
          match
            Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:channel_id
          with
          | Some scope ->
              (* Update scope profile_id if it's missing or differs from \
                 current binding *)
              let needs_update =
                match scope.profile_id with
                | None -> true
                | Some existing_pid -> existing_pid <> b.profile_id
              in
              if needs_update then begin
                let stmt =
                  Sqlite3.prepare db
                    "UPDATE memory_scopes SET profile_id = ?, updated_at = \
                     datetime('now') WHERE id = ?"
                in
                Fun.protect
                  ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                  (fun () ->
                    ignore
                      (Sqlite3.bind stmt 1
                         (Sqlite3.Data.INT (Int64.of_int b.profile_id)));
                    ignore
                      (Sqlite3.bind stmt 2
                         (Sqlite3.Data.INT (Int64.of_int scope.id)));
                    match Sqlite3.step stmt with
                    | Sqlite3.Rc.DONE -> ()
                    | rc ->
                        failwith
                          (Printf.sprintf
                             "update room memory scope owner failed: %s"
                             (Sqlite3.Rc.to_string rc)));
                Ok
                  (Option.value ~default:scope
                     (Memory.get_scope ~db ~id:scope.id))
              end
              else Ok scope
          | None ->
              Ok
                (Memory.create_scope ~db ~kind:"room" ~key:channel_id
                   ~profile_id:b.profile_id ~provenance:"channel" ())))

(** Check if the user has the required capability for the room scope. *)
let check_room_grant ~db ~cfg ~channel_id ~is_admin ~capability () =
  match resolve_room_scope ~db ~cfg ~channel_id ~is_admin () with
  | Error _ as err -> err
  | Ok scope -> (
      if is_admin then Ok scope
      else
        let binding = Memory.get_room_profile_binding ~db ~room_id:channel_id in
        match binding with
        | Some b ->
            let is_owner =
              match scope.profile_id with
              | Some pid -> pid = b.profile_id
              | None -> false
            in
            if is_owner then Ok scope
            else
              let grants =
                Memory.resolve_grants ~db ~scope_id:scope.id
                  ~principal_kind:"profile"
                  ~principal_id:(string_of_int b.profile_id)
              in
              if List.mem capability grants then Ok scope
              else
                Error
                  (Printf.sprintf
                     "Access denied: you do not have '%s' capability for this \
                      channel."
                     capability)
        | None ->
            let grants =
              Memory.resolve_grants ~db ~scope_id:scope.id
                ~principal_kind:"room" ~principal_id:channel_id
            in
            if List.mem capability grants then Ok scope
            else
              Error
                (Printf.sprintf
                   "Access denied: you do not have '%s' capability for this \
                    channel."
                   capability))

let format_room_memory_list ~connector ~db ~channel_id =
  let memories =
    Memory.query_scoped_memories ~db ~scope_kind:"room" ~scope_key:channel_id
      ~limit:100 ()
  in
  if memories = [] then Printf.sprintf "No memories found for this channel."
  else
    let columns =
      Table_format.
        [
          { header = "ID"; align = Right; min_width = 4; flex = false };
          { header = "REFERENCE"; align = Left; min_width = 12; flex = false };
          { header = "CONTENT"; align = Left; min_width = 20; flex = true };
          { header = "PROVENANCE"; align = Left; min_width = 8; flex = false };
          { header = "UPDATED"; align = Left; min_width = 16; flex = false };
        ]
    in
    let rows =
      List.map
        (fun (m : Memory.scoped_memory) ->
          let content_preview =
            match m.content with
            | Some c when String.length c > 60 -> String.sub c 0 57 ^ "..."
            | Some c -> c
            | None -> "(empty)"
          in
          [
            string_of_int m.id;
            Format_adapter.escape connector m.reference;
            Format_adapter.escape connector content_preview;
            Format_adapter.escape connector m.provenance;
            m.updated_at;
          ])
        memories
    in
    Format_adapter.bold connector "Channel Memories"
    ^ "\n\n"
    ^ Format_adapter.render_table connector ~max_width:100 columns rows

let format_room_memory_show ~connector ~db ~channel_id memory_id_str =
  match int_of_string_opt memory_id_str with
  | None ->
      Printf.sprintf "Error: '%s' is not a valid memory ID."
        (Format_adapter.escape connector memory_id_str)
  | Some memory_id -> (
      match Memory.get_scoped_memory ~db ~id:memory_id with
      | None -> Printf.sprintf "Memory #%d not found." memory_id
      | Some m when m.scope_kind <> "room" || m.scope_key <> channel_id ->
          Printf.sprintf "Memory #%d does not belong to this channel." memory_id
      | Some m ->
          let lines = ref [] in
          let add s = lines := s :: !lines in
          add (Printf.sprintf "Channel:    %s" channel_id);
          add (Printf.sprintf "ID:         %d" m.id);
          add
            (Printf.sprintf "Reference:  %s"
               (Format_adapter.escape connector m.reference));
          add
            (Printf.sprintf "Provenance: %s"
               (Format_adapter.escape connector m.provenance));
          add (Printf.sprintf "Created:    %s" m.created_at);
          add (Printf.sprintf "Updated:    %s" m.updated_at);
          (match m.content with
          | Some c ->
              add "";
              add "--- Content ---";
              add (Format_adapter.escape connector c)
          | None -> add "Content:    (empty)");
          (match m.redacted_at with
          | Some ts ->
              add "";
              add (Printf.sprintf "Redacted:   %s" ts);
              Option.iter
                (fun r ->
                  add
                    (Printf.sprintf "Reason:     %s"
                       (Format_adapter.escape connector r)))
                m.redaction_reason
          | None -> ());
          String.concat "\n" (List.rev !lines))

let format_room_memory_save ~connector ~db ~cfg ~channel_id ~is_admin ~reference
    ~content =
  if reference = "" then "Error: memory reference is required."
  else if content = "" then "Error: memory content is required."
  else
    match
      check_room_grant ~db ~cfg ~channel_id ~is_admin ~capability:"write" ()
    with
    | Error msg -> msg
    | Ok scope -> (
        let provenance = if is_admin then "admin-channel" else "channel" in
        let ledger ~room_id ~event_type ~actor ~metadata =
          ignore
            (Room_activity_ledger.append_now ~db ~room_id ~event_type ~actor
               ~metadata)
        in
        try
          let m =
            Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference
              ~content ~provenance ~ledger ()
          in
          Printf.sprintf "Saved memory '%s' (ID: %d) for this channel."
            (Format_adapter.escape connector m.reference)
            m.id
        with exn ->
          Printf.sprintf "Error saving memory: %s" (Printexc.to_string exn))

let format_room_memory_correct ~connector ~db ~cfg ~channel_id ~is_admin
    ~memory_id_str ~content =
  match int_of_string_opt memory_id_str with
  | None ->
      Printf.sprintf "Error: '%s' is not a valid memory ID."
        (Format_adapter.escape connector memory_id_str)
  | Some memory_id -> (
      match
        check_room_grant ~db ~cfg ~channel_id ~is_admin ~capability:"write" ()
      with
      | Error msg -> msg
      | Ok _scope -> (
          match Memory.get_scoped_memory ~db ~id:memory_id with
          | None -> Printf.sprintf "Memory #%d not found." memory_id
          | Some m when m.scope_kind <> "room" || m.scope_key <> channel_id ->
              Printf.sprintf "Memory #%d does not belong to this channel."
                memory_id
          | Some m when m.redacted_at <> None ->
              Printf.sprintf "Memory #%d is redacted and cannot be corrected."
                memory_id
          | Some _ -> (
              if content = "" then "Error: new memory content is required."
              else
                let provenance =
                  if is_admin then "corrected:admin-channel"
                  else "corrected:channel"
                in
                let ledger ~room_id ~event_type ~actor ~metadata =
                  ignore
                    (Room_activity_ledger.append_now ~db ~room_id ~event_type
                       ~actor ~metadata)
                in
                match
                  Memory.correct_scoped_memory ~db ~id:memory_id ~content
                    ~provenance ~ledger ()
                with
                | None ->
                    Printf.sprintf "Error: failed to correct memory #%d."
                      memory_id
                | Some updated ->
                    Printf.sprintf
                      "Corrected memory #%d '%s' for this channel.\n\
                       Old provenance preserved in correction trail."
                      updated.id
                      (Format_adapter.escape connector updated.reference))))

let format_room_memory_forget ~connector ~db ~cfg ~channel_id ~is_admin
    ~memory_id_str ~flags =
  match int_of_string_opt memory_id_str with
  | None ->
      Printf.sprintf "Error: '%s' is not a valid memory ID."
        (Format_adapter.escape connector memory_id_str)
  | Some memory_id -> (
      match
        check_room_grant ~db ~cfg ~channel_id ~is_admin ~capability:"write" ()
      with
      | Error msg -> msg
      | Ok _scope -> (
          match Memory.get_scoped_memory ~db ~id:memory_id with
          | None -> Printf.sprintf "Memory #%d not found." memory_id
          | Some m when m.scope_kind <> "room" || m.scope_key <> channel_id ->
              Printf.sprintf "Memory #%d does not belong to this channel."
                memory_id
          | Some m ->
              let hard_purge = List.mem "--hard" flags in
              let reason =
                let non_flags =
                  List.filter
                    (fun s -> not (String.starts_with ~prefix:"--" s))
                    flags
                in
                match non_flags with
                | [] -> "user request"
                | parts -> String.concat " " parts
              in
              let ledger ~room_id ~event_type ~actor ~metadata =
                ignore
                  (Room_activity_ledger.append_now ~db ~room_id ~event_type
                     ~actor ~metadata)
              in
              if hard_purge then
                if is_admin then
                  if Memory.delete_scoped_memory ~db ~id:memory_id ~ledger ()
                  then
                    Printf.sprintf
                      "Hard-purged memory #%d '%s' for this channel." memory_id
                      (Format_adapter.escape connector m.reference)
                  else
                    Printf.sprintf "Error: failed to hard-purge memory #%d."
                      memory_id
                else "Error: hard purge requires admin privileges."
              else if m.redacted_at <> None then
                Printf.sprintf
                  "Memory #%d is already redacted. Use --hard to purge."
                  memory_id
              else if
                Memory.redact_scoped_memory ~db ~id:memory_id ~reason ~ledger ()
              then
                Printf.sprintf
                  "Forgot (redacted) memory #%d '%s' for this channel."
                  memory_id
                  (Format_adapter.escape connector m.reference)
              else
                Printf.sprintf "Error: failed to redact memory #%d." memory_id))

let format_room_memories ~connector ~db ~cfg ~channel_id ~is_admin action =
  match action with
  | RoomMemoryList -> (
      match
        check_room_grant ~db ~cfg ~channel_id ~is_admin ~capability:"list" ()
      with
      | Error msg -> msg
      | Ok _ -> format_room_memory_list ~connector ~db ~channel_id)
  | RoomMemoryShow id -> (
      match
        check_room_grant ~db ~cfg ~channel_id ~is_admin ~capability:"read" ()
      with
      | Error msg -> msg
      | Ok _ -> format_room_memory_show ~connector ~db ~channel_id id)
  | RoomMemorySave (reference, content) ->
      format_room_memory_save ~connector ~db ~cfg ~channel_id ~is_admin
        ~reference ~content
  | RoomMemoryCorrect (id, content) ->
      format_room_memory_correct ~connector ~db ~cfg ~channel_id ~is_admin
        ~memory_id_str:id ~content
  | RoomMemoryForget (id, flags) ->
      format_room_memory_forget ~connector ~db ~cfg ~channel_id ~is_admin
        ~memory_id_str:id ~flags

(* ── Existing format: cron ─────────────────────────────────────────────── *)

let format_cron ~connector ~db ~session_key action =
  Scheduler.init_schema db;
  match action with
  | CronHelp -> format_cron_usage ~connector
  | CronShow name -> (
      match Scheduler.get_job ~db ~name with
      | None -> Printf.sprintf "No cron job found with name '%s'." name
      | Some (job : Scheduler.job) ->
          ignore session_key;
          let runs = Scheduler.get_history ~db ~name ~limit:5 in
          let doc =
            [
              Content_dsl.Paragraph
                [ Bold "Cron Job"; Text " — "; Code job.name ];
              Paragraph [ Text "Session: "; Code job.session_key ];
            ]
            @ (match Scheduler.job_routine_target job with
              | Some target ->
                  [
                    Content_dsl.Paragraph
                      [ Text "Routine target: "; Code target ];
                  ]
              | None -> [])
            @ [
                Content_dsl.Paragraph
                  [ Text "Schedule: "; Code job.schedule_str ];
                Paragraph
                  [
                    Text "Enabled: "; Text (if job.enabled then "yes" else "no");
                  ];
                Paragraph
                  [
                    Text "Expires: ";
                    Text
                      (match job.expires_at with
                      | Some ea -> ea
                      | None -> "never");
                  ];
              ]
            @ (match job.agent_name with
              | Some agent ->
                  [ Content_dsl.Paragraph [ Text "Agent: "; Code agent ] ]
              | None -> [])
            @ [
                Content_dsl.Separator;
                Paragraph [ Bold "Message" ];
                CodeBlock { language = None; content = job.message };
              ]
            @
            if runs = [] then
              [ Content_dsl.Paragraph [ Italic "No run history." ] ]
            else
              let history_columns =
                Table_format.
                  [
                    {
                      header = "ID";
                      align = Right;
                      min_width = 2;
                      flex = false;
                    };
                    {
                      header = "STARTED";
                      align = Left;
                      min_width = 19;
                      flex = false;
                    };
                    {
                      header = "STATUS";
                      align = Left;
                      min_width = 6;
                      flex = false;
                    };
                    {
                      header = "PREVIEW";
                      align = Left;
                      min_width = 10;
                      flex = true;
                    };
                  ]
              in
              let history_rows =
                List.map
                  (fun (r : Scheduler.run) ->
                    let preview =
                      match r.result_preview with
                      | Some p when String.length p > 40 ->
                          String.sub p 0 37 ^ "..."
                      | Some p -> p
                      | None -> ""
                    in
                    let status =
                      match Scheduler.run_routine_target r with
                      | Some target -> r.status ^ " " ^ target
                      | None -> r.status
                    in
                    [ string_of_int r.run_id; r.started_at; status; preview ])
                  runs
              in
              [ Content_dsl.Separator; Paragraph [ Bold "Recent Runs" ] ]
              @ [
                  Content_dsl.Paragraph
                    [
                      Text
                        (Format_adapter.render_table connector ~max_width:70
                           history_columns history_rows);
                    ];
                ]
          in
          Content_dsl.render_document connector doc)
  | CronList ->
      let jobs = Scheduler.list_jobs ~db in
      if jobs = [] then
        "No cron jobs configured. Use 'clawq cron add' to create one."
      else
        let columns =
          Table_format.
            [
              { header = "NAME"; align = Left; min_width = 4; flex = false };
              { header = "SESSION"; align = Left; min_width = 7; flex = false };
              { header = "SCHEDULE"; align = Left; min_width = 8; flex = false };
              { header = "ENABLED"; align = Left; min_width = 3; flex = false };
              { header = "EXPIRES"; align = Left; min_width = 3; flex = false };
              { header = "MESSAGE"; align = Left; min_width = 10; flex = true };
            ]
        in
        let rows =
          List.map
            (fun (j : Scheduler.job) ->
              let msg_preview =
                if String.length j.message > 40 then
                  String.sub j.message 0 37 ^ "..."
                else j.message
              in
              [
                j.name;
                (match Scheduler.job_routine_target j with
                | Some target -> Printf.sprintf "%s (%s)" j.session_key target
                | None -> j.session_key);
                j.schedule_str;
                (if j.enabled then "yes" else "no");
                (match j.expires_at with Some ea -> ea | None -> "-");
                msg_preview;
              ])
            jobs
        in
        Format_adapter.bold connector "Cron Jobs"
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:80 columns rows
  | CronAdd { name; schedule; message; ttl } -> (
      match
        Scheduler.add_job ~db ~name ~session_key ~message ~schedule ?ttl ()
      with
      | Ok () -> format_cron_confirm ~connector "added" name
      | Error e ->
          Format_adapter.bold connector "Error"
          ^ ": "
          ^ Format_adapter.escape connector e)
  | CronEdit { name; schedule; message; ttl } -> (
      match Scheduler.update_job ~db ~name ?schedule ?message ?ttl () with
      | Ok () -> format_cron_confirm ~connector "updated" name
      | Error e ->
          Format_adapter.bold connector "Error"
          ^ ": "
          ^ Format_adapter.escape connector e
      | exception Invalid_argument e ->
          Format_adapter.bold connector "Error"
          ^ ": "
          ^ Format_adapter.escape connector e)
  | CronTrigger name -> (
      Background_task.init_schema db;
      match Scheduler.trigger_job ~db ~name () with
      | Ok task_id ->
          format_cron_confirm ~connector "triggered" name
          ^ " Enqueued as background task "
          ^ Format_adapter.code connector (string_of_int task_id)
          ^ "."
      | Error e ->
          Format_adapter.bold connector "Error"
          ^ ": "
          ^ Format_adapter.escape connector e)
  | CronRemove name ->
      if Scheduler.remove_job ~db ~name then
        format_cron_confirm ~connector "removed" name
      else
        "No job found with name "
        ^ Format_adapter.code connector (Printf.sprintf "'%s'" name)
        ^ "."
  | CronHistory job_name ->
      let runs =
        match job_name with
        | Some name -> Scheduler.get_history ~db ~name ~limit:10
        | None -> Scheduler.list_runs ~db ~limit:20 ()
      in
      if runs = [] then
        match job_name with
        | Some name -> Printf.sprintf "No run history for '%s'." name
        | None -> "No run history."
      else
        let columns =
          Table_format.
            [
              { header = "ID"; align = Right; min_width = 2; flex = false };
              { header = "JOB"; align = Left; min_width = 3; flex = false };
              { header = "STARTED"; align = Left; min_width = 19; flex = false };
              { header = "STATUS"; align = Left; min_width = 6; flex = false };
              { header = "PREVIEW"; align = Left; min_width = 10; flex = true };
            ]
        in
        let rows =
          List.map
            (fun (r : Scheduler.run) ->
              let preview =
                match r.result_preview with
                | Some p when String.length p > 40 -> String.sub p 0 37 ^ "..."
                | Some p -> p
                | None -> ""
              in
              let status =
                match Scheduler.run_routine_target r with
                | Some target -> r.status ^ " " ^ target
                | None -> r.status
              in
              [
                string_of_int r.run_id;
                r.job_name;
                r.started_at;
                status;
                preview;
              ])
            runs
        in
        let title =
          match job_name with
          | Some name -> Printf.sprintf "Run History \xe2\x80\x94 %s" name
          | None -> "Run History"
        in
        Format_adapter.bold connector title
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:80 columns rows
