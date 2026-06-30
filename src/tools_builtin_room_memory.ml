(** Agent tools for room-scoped memory operations.

    These tools allow agents to list, search, save, correct, and forget memories
    scoped to the current room. Each tool automatically resolves the room
    context from the session key and enforces access control. *)

(** Resolve room_id from session context. Tries multiple strategies: 1. Direct
    channel_id from DB session 2. Room_id parsed from session_key format
    "channel:room-id" *)
let resolve_room_id_for_context ~db (context : Tool.invoke_context) =
  match context.session_key with
  | None -> None
  | Some session_key -> (
      (* Try getting channel_id from session DB *)
      match Memory.get_session_channel ~db ~session_key with
      | Some (_channel, channel_id) when String.trim channel_id <> "" ->
          Some channel_id
      | _ -> (
          (* Parse from session_key format *)
          match String.index_opt session_key ':' with
          | Some idx when idx + 1 < String.length session_key ->
              let room_id =
                String.sub session_key (idx + 1)
                  (String.length session_key - idx - 1)
              in
              if String.trim room_id <> "" then Some room_id else None
          | _ -> None))

(** Ensure a memory scope exists for the room. Creates one if needed and the
    room has a profile binding. Backfills missing owner if scope exists but
    profile_id is None and a binding exists. Returns [Ok scope] or [Error msg].
*)
let ensure_room_scope ~db ~room_id =
  match Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:room_id with
  | Some scope -> (
      (* Backfill missing owner if binding exists *)
      match Memory.get_room_profile_binding ~db ~room_id with
      | Some binding -> (
          match scope.profile_id with
          | Some _ -> Ok scope
          | None ->
              (* Backfill: update scope with binding's profile_id *)
              let stmt =
                Sqlite3.prepare db
                  "UPDATE memory_scopes SET profile_id = ?, updated_at = \
                   datetime('now') WHERE id = ? AND profile_id IS NULL"
              in
              Fun.protect
                ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                (fun () ->
                  ignore
                    (Sqlite3.bind stmt 1
                       (Sqlite3.Data.INT (Int64.of_int binding.profile_id)));
                  ignore
                    (Sqlite3.bind stmt 2
                       (Sqlite3.Data.INT (Int64.of_int scope.id)));
                  match Sqlite3.step stmt with
                  | Sqlite3.Rc.DONE -> ()
                  | rc ->
                      failwith
                        (Printf.sprintf
                           "backfill room memory scope owner failed: %s"
                           (Sqlite3.Rc.to_string rc)));
              Ok
                (Option.value ~default:scope
                   (Memory.get_scope ~db ~id:scope.id)))
      | None -> Ok scope)
  | None -> (
      match Memory.get_room_profile_binding ~db ~room_id with
      | Some binding ->
          Ok
            (Memory.create_scope ~db ~kind:"room" ~key:room_id
               ~profile_id:binding.profile_id ~provenance:"agent-tool" ())
      | None ->
          Error
            (Printf.sprintf
               "No memory scope or profile binding found for room '%s'. Bind a \
                room profile first."
               room_id))

(** Check if the principal has the required capability for the room scope.
    Supports: 1. Owner (bound profile owns scope) 2. Profile grant to bound
    profile 3. Direct room grant when no profile binding *)
let check_room_access ~db ~room_id ~capability =
  match ensure_room_scope ~db ~room_id with
  | Error _ as err -> err
  | Ok scope -> (
      match Memory.get_room_profile_binding ~db ~room_id with
      | Some binding ->
          let is_owner =
            match scope.profile_id with
            | Some pid -> pid = binding.profile_id
            | None -> false
          in
          if is_owner then Ok scope
          else
            let grants =
              Memory.resolve_grants ~db ~scope_id:scope.id
                ~principal_kind:"profile"
                ~principal_id:(string_of_int binding.profile_id)
            in
            if List.mem capability grants then Ok scope
            else
              Error
                (Printf.sprintf
                   "Access denied: room '%s' does not have '%s' capability."
                   room_id capability)
      | None ->
          (* No profile binding: check direct room grant *)
          let grants =
            Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind:"room"
              ~principal_id:room_id
          in
          if List.mem capability grants then Ok scope
          else
            Error
              (Printf.sprintf
                 "Access denied: room '%s' does not have '%s' capability."
                 room_id capability))

(** Determine the principal kind and id for visibility checks. If the scope has
    a bound profile, use it as the principal (so Private memories owned by the
    profile are visible). Otherwise fall back to the room id. *)
let visibility_principal ~(scope : Memory.memory_scope) ~room_id =
  match scope.profile_id with
  | Some profile_id -> ("profile", string_of_int profile_id)
  | None -> ("room", room_id)

(** Clip memory content for preview in tool responses. *)
let clip_content content max_len =
  if String.length content <= max_len then content
  else String.sub content 0 (max_len - 3) ^ "..."

(** [room_memory_list] tool: list memories for the current room. *)
let room_memory_list ~db =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "limit",
                `Assoc
                  [
                    ("type", `String "integer");
                    ( "description",
                      `String
                        "Maximum number of memories to return (default: 20, \
                         max: 100)" );
                  ] );
            ] );
        ("required", `List []);
      ]
  in
  {
    Tool.name = "room_memory_list";
    description =
      "List memories stored for the current room/channel. Returns memory IDs, \
       references, content previews, and timestamps. Use this to discover what \
       the room has remembered. To see full content of a specific memory, use \
       room_memory_show.";
    parameters_schema = schema;
    invoke =
      (fun ?context _args ->
        match context with
        | None ->
            Lwt.return
              "Error: no invoke context available. This tool requires a \
               session context."
        | Some ctx -> (
            match resolve_room_id_for_context ~db ctx with
            | None ->
                Lwt.return
                  "Error: could not determine room context from session. This \
                   tool is only available in room-scoped sessions."
            | Some room_id -> (
                match check_room_access ~db ~room_id ~capability:"list" with
                | Error msg -> Lwt.return ("Error: " ^ msg)
                | Ok scope -> (
                    let open Yojson.Safe.Util in
                    let limit_result =
                      try
                        let v = _args |> member "limit" |> to_int in
                        if v < 1 || v > 100 then
                          Error "Error: limit must be between 1 and 100."
                        else Ok v
                      with _ -> Ok 20
                    in
                    match limit_result with
                    | Error msg -> Lwt.return msg
                    | Ok limit ->
                        let memories =
                          Memory.query_scoped_memories ~db ~scope_kind:"room"
                            ~scope_key:room_id ~limit ()
                        in
                        (* Filter by visibility: only show memories the caller can see *)
                        let scope_profile_id =
                          Option.map string_of_int scope.profile_id
                        in
                        let principal_kind, principal_id =
                          visibility_principal ~scope ~room_id
                        in
                        let visible_memories =
                          List.filter
                            (fun (m : Memory_types.scoped_memory) ->
                              Memory.can_see_memory ~db ~scoped_mem:m
                                ~principal_kind ~principal_id ~scope_profile_id)
                            memories
                        in
                        if visible_memories = [] then
                          Lwt.return
                            (Printf.sprintf
                               "No memories found for this room (%s)." room_id)
                        else
                          let lines =
                            List.map
                              (fun (m : Memory_types.scoped_memory) ->
                                let preview =
                                  match m.content with
                                  | Some c -> clip_content c 80
                                  | None -> "(empty)"
                                in
                                let vis =
                                  match m.visibility with
                                  | Memory_types.Public -> ""
                                  | v ->
                                      Printf.sprintf " [%s]"
                                        (Memory_types.visibility_to_string v)
                                in
                                Printf.sprintf "#%d [%s]%s %s (updated: %s)"
                                  m.id m.reference vis preview m.updated_at)
                              visible_memories
                          in
                          Lwt.return
                            (Printf.sprintf "Room memories (%s):\n%s" room_id
                               (String.concat "\n" lines))))));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

(** [room_memory_show] tool: show full content of a specific room memory. *)
let room_memory_show ~db =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "memory_id",
                `Assoc
                  [
                    ("type", `String "integer");
                    ( "description",
                      `String "ID of the memory to show (required)" );
                  ] );
            ] );
        ("required", `List [ `String "memory_id" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"room_memory_show"
      ~parameters_schema:schema ~detail
  in
  {
    Tool.name = "room_memory_show";
    description =
      "Show the full content of a specific room memory by its ID. Use \
       room_memory_list first to discover available memory IDs.";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        match context with
        | None ->
            Lwt.return
              "Error: no invoke context available. This tool requires a \
               session context."
        | Some ctx -> (
            match resolve_room_id_for_context ~db ctx with
            | None ->
                Lwt.return
                  "Error: could not determine room context from session."
            | Some room_id -> (
                match check_room_access ~db ~room_id ~capability:"read" with
                | Error msg -> Lwt.return ("Error: " ^ msg)
                | Ok scope -> (
                    let open Yojson.Safe.Util in
                    let memory_id =
                      try args |> member "memory_id" |> to_int with _ -> -1
                    in
                    if memory_id <= 0 then
                      Lwt.return
                        (param_err "memory_id must be a positive integer")
                    else
                      match Memory.get_scoped_memory ~db ~id:memory_id with
                      | None ->
                          Lwt.return
                            (Printf.sprintf "Memory #%d not found." memory_id)
                      | Some m
                        when m.scope_kind <> "room" || m.scope_key <> room_id ->
                          Lwt.return
                            (Printf.sprintf
                               "Memory #%d does not belong to this room."
                               memory_id)
                      | Some m ->
                          (* Check visibility before showing content *)
                          let scope_profile_id =
                            Option.map string_of_int scope.profile_id
                          in
                          let principal_kind, principal_id =
                            visibility_principal ~scope ~room_id
                          in
                          if
                            not
                              (Memory.can_see_memory ~db ~scoped_mem:m
                                 ~principal_kind ~principal_id ~scope_profile_id)
                          then
                            Lwt.return
                              (Printf.sprintf
                                 "Memory #%d is not visible to this room \
                                  (visibility: %s)."
                                 memory_id
                                 (Memory_types.visibility_to_string m.visibility))
                          else
                            let lines = ref [] in
                            let add s = lines := s :: !lines in
                            add (Printf.sprintf "Room:       %s" room_id);
                            add (Printf.sprintf "ID:         %d" m.id);
                            add (Printf.sprintf "Reference:  %s" m.reference);
                            add
                              (Printf.sprintf "Visibility: %s"
                                 (Memory_types.visibility_to_string m.visibility));
                            add (Printf.sprintf "Provenance: %s" m.provenance);
                            add (Printf.sprintf "Created:    %s" m.created_at);
                            add (Printf.sprintf "Updated:    %s" m.updated_at);
                            (match m.content with
                            | Some c ->
                                add "";
                                add "--- Content ---";
                                add c
                            | None -> add "Content:    (empty)");
                            (match m.redacted_at with
                            | Some ts ->
                                add "";
                                add (Printf.sprintf "Redacted:   %s" ts);
                                Option.iter
                                  (fun r ->
                                    add (Printf.sprintf "Reason:     %s" r))
                                  m.redaction_reason
                            | None -> ());
                            Lwt.return (String.concat "\n" (List.rev !lines))))));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

(** [room_memory_save] tool: save or update a room memory. *)
let no_op_ledger : Memory.ledger_fn =
 fun ~room_id:_ ~event_type:_ ~actor:_ ~metadata:_ -> ()

let room_memory_save ~db ~ledger =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "reference",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String
                        "Short reference key for the memory (required). Should \
                         be descriptive and unique within the room (e.g., \
                         'project-goal', 'user-preference')." );
                  ] );
              ( "content",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "Content to store (required)");
                  ] );
              ( "visibility",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String
                        "Visibility level: 'public' (default, visible to all \
                         room members), 'private' (owner only), 'team' \
                         (explicit grant set)." );
                    ( "enum",
                      `List
                        [ `String "public"; `String "private"; `String "team" ]
                    );
                  ] );
            ] );
        ("required", `List [ `String "reference"; `String "content" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"room_memory_save"
      ~parameters_schema:schema ~detail
  in
  {
    Tool.name = "room_memory_save";
    description =
      "Save or update a memory for the current room. If a memory with the same \
       reference already exists, it will be updated (upsert). Use this when \
       the user tells you something to remember about this room, or when you \
       derive a stable fact specific to this room.";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        match context with
        | None ->
            Lwt.return
              "Error: no invoke context available. This tool requires a \
               session context."
        | Some ctx -> (
            match resolve_room_id_for_context ~db ctx with
            | None ->
                Lwt.return
                  "Error: could not determine room context from session."
            | Some room_id -> (
                match check_room_access ~db ~room_id ~capability:"write" with
                | Error msg -> Lwt.return ("Error: " ^ msg)
                | Ok scope ->
                    let open Yojson.Safe.Util in
                    let reference =
                      try args |> member "reference" |> to_string with _ -> ""
                    in
                    let content =
                      try args |> member "content" |> to_string with _ -> ""
                    in
                    if reference = "" then
                      Lwt.return
                        (param_err
                           "parameter 'reference' must be a non-empty string")
                    else if content = "" then
                      Lwt.return
                        (param_err
                           "parameter 'content' must be a non-empty string")
                    else
                      let provenance = "agent-tool" in
                      let visibility =
                        try
                          match args |> member "visibility" |> to_string with
                          | "public" -> Memory_types.Public
                          | "private" -> Memory_types.Private
                          | "team" -> Memory_types.Team
                          | _ -> Memory_types.Public
                        with _ -> Memory_types.Public
                      in
                      Lwt.catch
                        (fun () ->
                          let m =
                            Memory.upsert_scoped_memory ~db ~scope_id:scope.id
                              ~reference ~content ~provenance ~visibility
                              ~ledger ()
                          in
                          let vis_str =
                            match m.visibility with
                            | Memory_types.Public -> ""
                            | v ->
                                Printf.sprintf " (visibility: %s)"
                                  (Memory_types.visibility_to_string v)
                          in
                          Lwt.return
                            (Printf.sprintf
                               "Saved memory '%s' (ID: %d) for room %s.%s"
                               m.reference m.id room_id vis_str))
                        (fun exn ->
                          Lwt.return
                            (Printf.sprintf "Error saving memory: %s"
                               (Printexc.to_string exn))))));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

(** [room_memory_correct] tool: correct an existing room memory. *)
let room_memory_correct ~db ~ledger =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "memory_id",
                `Assoc
                  [
                    ("type", `String "integer");
                    ( "description",
                      `String "ID of the memory to correct (required)" );
                  ] );
              ( "content",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String "New content for the memory (required)" );
                  ] );
            ] );
        ("required", `List [ `String "memory_id"; `String "content" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"room_memory_correct"
      ~parameters_schema:schema ~detail
  in
  {
    Tool.name = "room_memory_correct";
    description =
      "Correct the content of an existing room memory. The old content is \
       preserved in the correction trail. Use this when a memory needs to be \
       updated with more accurate information.";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        match context with
        | None ->
            Lwt.return
              "Error: no invoke context available. This tool requires a \
               session context."
        | Some ctx -> (
            match resolve_room_id_for_context ~db ctx with
            | None ->
                Lwt.return
                  "Error: could not determine room context from session."
            | Some room_id -> (
                match check_room_access ~db ~room_id ~capability:"write" with
                | Error msg -> Lwt.return ("Error: " ^ msg)
                | Ok _scope -> (
                    let open Yojson.Safe.Util in
                    let memory_id =
                      try args |> member "memory_id" |> to_int with _ -> -1
                    in
                    let content =
                      try args |> member "content" |> to_string with _ -> ""
                    in
                    if memory_id <= 0 then
                      Lwt.return
                        (param_err "memory_id must be a positive integer")
                    else if content = "" then
                      Lwt.return
                        (param_err
                           "parameter 'content' must be a non-empty string")
                    else
                      match Memory.get_scoped_memory ~db ~id:memory_id with
                      | None ->
                          Lwt.return
                            (Printf.sprintf "Memory #%d not found." memory_id)
                      | Some m
                        when m.scope_kind <> "room" || m.scope_key <> room_id ->
                          Lwt.return
                            (Printf.sprintf
                               "Memory #%d does not belong to this room."
                               memory_id)
                      | Some m when m.redacted_at <> None ->
                          Lwt.return
                            (Printf.sprintf
                               "Memory #%d is redacted and cannot be corrected."
                               memory_id)
                      | Some _ -> (
                          let provenance = "corrected:agent-tool" in
                          match
                            Memory.correct_scoped_memory ~db ~id:memory_id
                              ~content ~provenance ~ledger ()
                          with
                          | None ->
                              Lwt.return
                                (Printf.sprintf
                                   "Error: failed to correct memory #%d."
                                   memory_id)
                          | Some updated ->
                              Lwt.return
                                (Printf.sprintf
                                   "Corrected memory #%d '%s' for room %s.\n\
                                    Old provenance preserved in correction \
                                    trail."
                                   updated.id updated.reference room_id))))));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

(** [room_memory_forget] tool: redact (soft-delete) a room memory. *)
let room_memory_forget ~db ~ledger =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "memory_id",
                `Assoc
                  [
                    ("type", `String "integer");
                    ( "description",
                      `String "ID of the memory to forget (required)" );
                  ] );
              ( "reason",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String
                        "Optional reason for forgetting (default: 'agent \
                         request')" );
                  ] );
            ] );
        ("required", `List [ `String "memory_id" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"room_memory_forget"
      ~parameters_schema:schema ~detail
  in
  {
    Tool.name = "room_memory_forget";
    description =
      "Forget (redact) a room memory. The memory is soft-deleted and marked \
       with a redaction timestamp and reason. Use this when the user asks to \
       remove a memory or when a memory is no longer relevant.";
    parameters_schema = schema;
    invoke =
      (fun ?context args ->
        match context with
        | None ->
            Lwt.return
              "Error: no invoke context available. This tool requires a \
               session context."
        | Some ctx -> (
            match resolve_room_id_for_context ~db ctx with
            | None ->
                Lwt.return
                  "Error: could not determine room context from session."
            | Some room_id -> (
                match check_room_access ~db ~room_id ~capability:"write" with
                | Error msg -> Lwt.return ("Error: " ^ msg)
                | Ok _scope -> (
                    let open Yojson.Safe.Util in
                    let memory_id =
                      try args |> member "memory_id" |> to_int with _ -> -1
                    in
                    let reason =
                      try args |> member "reason" |> to_string
                      with _ -> "agent request"
                    in
                    if memory_id <= 0 then
                      Lwt.return
                        (param_err "memory_id must be a positive integer")
                    else
                      match Memory.get_scoped_memory ~db ~id:memory_id with
                      | None ->
                          Lwt.return
                            (Printf.sprintf "Memory #%d not found." memory_id)
                      | Some m
                        when m.scope_kind <> "room" || m.scope_key <> room_id ->
                          Lwt.return
                            (Printf.sprintf
                               "Memory #%d does not belong to this room."
                               memory_id)
                      | Some m when m.redacted_at <> None ->
                          Lwt.return
                            (Printf.sprintf "Memory #%d is already redacted."
                               memory_id)
                      | Some _ ->
                          if
                            Memory.redact_scoped_memory ~db ~id:memory_id
                              ~reason ~ledger ()
                          then
                            Lwt.return
                              (Printf.sprintf
                                 "Forgot (redacted) memory #%d for room %s."
                                 memory_id room_id)
                          else
                            Lwt.return
                              (Printf.sprintf
                                 "Error: failed to redact memory #%d." memory_id)
                    ))));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

(** Register all room memory tools. *)
let register_room_memory_tools ~db ?(ledger = no_op_ledger) registry =
  Tool_registry.register registry (room_memory_list ~db);
  Tool_registry.register registry (room_memory_show ~db);
  Tool_registry.register registry (room_memory_save ~db ~ledger);
  Tool_registry.register registry (room_memory_correct ~db ~ledger);
  Tool_registry.register registry (room_memory_forget ~db ~ledger)
