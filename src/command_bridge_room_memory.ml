open Command_bridge_helpers

let admin_env_var = "CLAWQ_ADMIN"

let is_admin_cli () =
  match Sys.getenv_opt admin_env_var with
  | Some v -> v = "1" || v = "true"
  | None -> false

let require_admin () =
  if is_admin_cli () then None
  else
    Some
      "Error: this command requires admin privileges. Set CLAWQ_ADMIN=1 in \
       your environment."

let require_admin_audited ~room_id ~action =
  match require_admin () with
  | Some _ as err ->
      (try
         let db = get_db () in
         Room_activity_ledger.init_schema db;
         ignore
           (Room_activity_ledger.append_now ~db ~room_id
              ~event_type:"admin_denied" ~actor:"cli"
              ~metadata:
                (`Assoc
                   [
                     ("action", `String action);
                     ("error", `String "requires CLAWQ_ADMIN");
                   ]))
       with _ -> ());
      err
  | None -> None

let cmd_rooms_memory (cfg : Runtime_config.t) args =
  let db = get_db () in
  let is_admin = is_admin_cli () in
  let reconcile_error =
    try
      ignore (Memory.reconcile_room_profiles ~db ~config:cfg);
      None
    with exn ->
      Some
        (Printf.sprintf "Error reconciling room profile bindings: %s"
           (Printexc.to_string exn))
  in
  let room_display_name room_id =
    match Memory.get_room_profile_for_room ~db ~room_id with
    | Some profile -> Printf.sprintf "%s (%s)" room_id profile.name
    | None -> room_id
  in
  let get_room_binding ~room_id =
    match reconcile_error with
    | Some msg -> Error msg
    | None -> Ok (Memory.get_room_profile_binding ~db ~room_id)
  in
  let attach_scope_profile_if_missing ~(scope : Memory.memory_scope) ~profile_id
      =
    match scope.profile_id with
    | Some _ -> scope
    | None ->
        let stmt =
          Sqlite3.prepare db
            "UPDATE memory_scopes SET profile_id = ?, updated_at = \
             datetime('now') WHERE id = ? AND profile_id IS NULL"
        in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            ignore
              (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int profile_id)));
            ignore
              (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int scope.id)));
            match Sqlite3.step stmt with
            | Sqlite3.Rc.DONE -> ()
            | rc ->
                failwith
                  (Printf.sprintf "update room memory scope owner failed: %s"
                     (Sqlite3.Rc.to_string rc)));
        Option.value ~default:scope (Memory.get_scope ~db ~id:scope.id)
  in
  let visibility_principal ~(scope : Memory.memory_scope) ~room_id =
    match scope.profile_id with
    | Some profile_id -> ("profile", string_of_int profile_id)
    | None -> ("room", room_id)
  in
  let ensure_room_scope ~room_id =
    match get_room_binding ~room_id with
    | Error msg -> Error msg
    | Ok binding -> (
        match Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:room_id with
        | Some scope ->
            let scope =
              match binding with
              | Some binding ->
                  attach_scope_profile_if_missing ~scope
                    ~profile_id:binding.profile_id
              | None -> scope
            in
            Ok scope
        | None -> (
            match binding with
            | Some binding ->
                Ok
                  (Memory.create_scope ~db ~kind:"room" ~key:room_id
                     ~profile_id:binding.profile_id ~provenance:"cli" ())
            | None when is_admin ->
                Ok
                  (Memory.create_scope ~db ~kind:"room" ~key:room_id
                     ~provenance:"cli" ())
            | None ->
                Error
                  (Printf.sprintf "No memory scope found for room '%s'." room_id)
            ))
  in
  let check_grant ~room_id ~capability =
    match get_room_binding ~room_id with
    | Error msg -> Error msg
    | Ok None -> (
        (* No binding: check if scope exists and if grants allow access *)
        match Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:room_id with
        | None ->
            Error
              (Printf.sprintf "No memory scope found for room '%s'." room_id)
        | Some scope ->
            if is_admin then Ok scope
            else
              let grants =
                Memory.resolve_grants ~db ~scope_id:scope.id
                  ~principal_kind:"room" ~principal_id:room_id
              in
              if List.mem capability grants then Ok scope
              else
                Error
                  (Printf.sprintf
                     "Access denied: room '%s' does not have '%s' capability."
                     room_id capability))
    | Ok (Some binding) -> (
        (* Has binding: owner has implicit access, or check grants *)
        match Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:room_id with
        | None ->
            Error
              (Printf.sprintf "No memory scope found for room '%s'." room_id)
        | Some scope ->
            let scope =
              attach_scope_profile_if_missing ~scope
                ~profile_id:binding.profile_id
            in
            if is_admin then Ok scope
            else
              let is_owner =
                match scope.profile_id with
                | Some profile_id -> profile_id = binding.profile_id
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
                       room_id capability))
  in
  match args with
  | [ "list"; room_id ] -> (
      match check_grant ~room_id ~capability:"list" with
      | Error msg -> msg
      | Ok scope ->
          let memories =
            Memory.query_scoped_memories ~db ~scope_kind:"room"
              ~scope_key:room_id ~limit:100 ()
          in
          let scope_profile_id = Option.map string_of_int scope.profile_id in
          let principal_kind, principal_id =
            visibility_principal ~scope ~room_id
          in
          let visible_memories =
            List.filter
              (fun (m : Memory.scoped_memory) ->
                Memory.can_see_memory ~db ~scoped_mem:m ~principal_kind
                  ~principal_id ~scope_profile_id)
              memories
          in
          if visible_memories = [] then
            Printf.sprintf "No memories found for room '%s'."
              (room_display_name room_id)
          else
            let columns =
              Table_format.
                [
                  { header = "ID"; align = Right; min_width = 4; flex = false };
                  {
                    header = "REFERENCE";
                    align = Left;
                    min_width = 12;
                    flex = false;
                  };
                  {
                    header = "VISIBILITY";
                    align = Left;
                    min_width = 8;
                    flex = false;
                  };
                  {
                    header = "CONTENT";
                    align = Left;
                    min_width = 20;
                    flex = true;
                  };
                  {
                    header = "PROVENANCE";
                    align = Left;
                    min_width = 8;
                    flex = false;
                  };
                  {
                    header = "UPDATED";
                    align = Left;
                    min_width = 16;
                    flex = false;
                  };
                ]
            in
            let rows =
              List.map
                (fun (m : Memory.scoped_memory) ->
                  let content_preview =
                    match m.content with
                    | Some c when String.length c > 80 ->
                        String.sub c 0 80 ^ "..."
                    | Some c -> c
                    | None -> "(empty)"
                  in
                  [
                    string_of_int m.id;
                    m.reference;
                    Memory.visibility_to_string m.visibility;
                    content_preview;
                    m.provenance;
                    m.updated_at;
                  ])
                visible_memories
            in
            let table_output =
              Format_adapter.bold Format_adapter.Plain
                (Printf.sprintf "Room Memories: %s" (room_display_name room_id))
              ^ "\n\n"
              ^ Format_adapter.render_table Format_adapter.Plain ~max_width:100
                  columns rows
            in
            (* Add scope grants summary for admins *)
            if is_admin then
              match
                Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:room_id
              with
              | Some scope ->
                  let scope_grants =
                    Memory.list_grants ~db ~scope_id:scope.id
                  in
                  if scope_grants <> [] then
                    let grant_lines =
                      List.map
                        (fun (g : Memory.scope_grant) ->
                          Printf.sprintf "  %s:%s -> %s" g.principal_kind
                            g.principal_id g.capability)
                        scope_grants
                    in
                    table_output ^ "\n\nScope Grants:\n"
                    ^ String.concat "\n" grant_lines
                  else table_output
              | None -> table_output
            else table_output)
  | [ "show"; room_id; memory_id_str ] -> (
      match check_grant ~room_id ~capability:"read" with
      | Error msg -> msg
      | Ok scope -> (
          match int_of_string_opt memory_id_str with
          | None ->
              Printf.sprintf "Error: '%s' is not a valid memory ID."
                memory_id_str
          | Some memory_id -> (
              match Memory.get_scoped_memory ~db ~id:memory_id with
              | None -> Printf.sprintf "Memory #%d not found." memory_id
              | Some m when m.scope_kind <> "room" || m.scope_key <> room_id ->
                  Printf.sprintf "Memory #%d does not belong to room '%s'."
                    memory_id room_id
              | Some m ->
                  let scope_profile_id =
                    Option.map string_of_int scope.profile_id
                  in
                  let principal_kind, principal_id =
                    visibility_principal ~scope ~room_id
                  in
                  if
                    not
                      (Memory.can_see_memory ~db ~scoped_mem:m ~principal_kind
                         ~principal_id ~scope_profile_id)
                  then
                    Printf.sprintf
                      "Memory #%d is not visible to room '%s' (visibility: %s)."
                      memory_id room_id
                      (Memory.visibility_to_string m.visibility)
                  else
                    let lines = ref [] in
                    let add s = lines := s :: !lines in
                    add
                      (Printf.sprintf "Room:       %s"
                         (room_display_name room_id));
                    add (Printf.sprintf "ID:         %d" m.id);
                    add (Printf.sprintf "Reference:  %s" m.reference);
                    add
                      (Printf.sprintf "Visibility: %s"
                         (Memory.visibility_to_string m.visibility));
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
                          (fun r -> add (Printf.sprintf "Reason:     %s" r))
                          m.redaction_reason
                    | None -> ());
                    (* Show team grants for admins only. *)
                    if is_admin then begin
                      match m.visibility with
                      | Memory.Team ->
                          let grants =
                            Memory.list_team_grants ~db ~memory_id:m.id
                          in
                          if grants <> [] then begin
                            add "";
                            add "--- Team Grants ---";
                            List.iter
                              (fun (g : Memory.team_grant) ->
                                add
                                  (Printf.sprintf "  %s:%s (granted %s)"
                                     g.principal_kind g.principal_id
                                     g.granted_at))
                              grants
                          end
                      | _ -> ()
                    end;
                    (* Show scope grants for admins *)
                    if is_admin then begin
                      match
                        Memory.get_scope_by_kind_key ~db ~kind:"room"
                          ~key:room_id
                      with
                      | Some scope ->
                          let scope_grants =
                            Memory.list_grants ~db ~scope_id:scope.id
                          in
                          if scope_grants <> [] then begin
                            add "";
                            add "--- Scope Grants ---";
                            List.iter
                              (fun (g : Memory.scope_grant) ->
                                let expiry =
                                  match g.expires_at with
                                  | Some ts -> Printf.sprintf " (expires %s)" ts
                                  | None -> ""
                                in
                                add
                                  (Printf.sprintf "  %s:%s -> %s%s"
                                     g.principal_kind g.principal_id
                                     g.capability expiry))
                              scope_grants
                          end
                      | None -> ()
                    end;
                    String.concat "\n" (List.rev !lines))))
  | "save" :: room_id :: reference :: content_parts -> (
      if reference = "" then "Error: memory reference is required."
      else if content_parts = [] then "Error: memory content is required."
      else
        let visibility, content_parts =
          let rec extract_vis acc = function
            | "--visibility" :: v :: rest ->
                ( Some
                    (match v with
                    | "public" -> Memory.Public
                    | "private" -> Memory.Private
                    | "team" -> Memory.Team
                    | other ->
                        failwith
                          (Printf.sprintf
                             "invalid visibility '%s' (use public, private, or \
                              team)"
                             other)),
                  List.rev_append acc rest )
            | x :: rest -> extract_vis (x :: acc) rest
            | [] -> (None, List.rev acc)
          in
          extract_vis [] content_parts
        in
        if content_parts = [] then "Error: memory content is required."
        else
          match ensure_room_scope ~room_id with
          | Error msg -> msg
          | Ok _scope -> (
              match check_grant ~room_id ~capability:"write" with
              | Error msg -> msg
              | Ok scope -> (
                  let content = String.concat " " content_parts in
                  let provenance = if is_admin then "admin-cli" else "cli" in
                  try
                    let m =
                      Memory.upsert_scoped_memory ~db ~scope_id:scope.id
                        ~reference ~content ~provenance ?visibility ()
                    in
                    let vis_str =
                      match m.visibility with
                      | Memory.Public -> ""
                      | v ->
                          Printf.sprintf " (visibility: %s)"
                            (Memory.visibility_to_string v)
                    in
                    Printf.sprintf "Saved memory '%s' (ID: %d) for room '%s'.%s"
                      m.reference m.id
                      (room_display_name room_id)
                      vis_str
                  with
                  | Failure msg -> Printf.sprintf "Error: %s" msg
                  | exn ->
                      Printf.sprintf "Error saving memory: %s"
                        (Printexc.to_string exn))))
  | "save" :: _ ->
      "Usage: clawq rooms memory save <room_id> <reference> <content> \
       [--visibility V]"
  | "correct" :: room_id :: memory_id_str :: content_parts -> (
      match check_grant ~room_id ~capability:"write" with
      | Error msg -> msg
      | Ok _scope -> (
          match int_of_string_opt memory_id_str with
          | None ->
              Printf.sprintf "Error: '%s' is not a valid memory ID."
                memory_id_str
          | Some memory_id -> (
              match Memory.get_scoped_memory ~db ~id:memory_id with
              | None -> Printf.sprintf "Memory #%d not found." memory_id
              | Some m when m.scope_kind <> "room" || m.scope_key <> room_id ->
                  Printf.sprintf "Memory #%d does not belong to room '%s'."
                    memory_id room_id
              | Some m when m.redacted_at <> None ->
                  Printf.sprintf
                    "Memory #%d is redacted and cannot be corrected." memory_id
              | Some _ -> (
                  if content_parts = [] then
                    "Error: new memory content is required."
                  else
                    let content = String.concat " " content_parts in
                    let provenance =
                      if is_admin then "corrected:admin-cli"
                      else "corrected:cli"
                    in
                    match
                      Memory.correct_scoped_memory ~db ~id:memory_id ~content
                        ~provenance ()
                    with
                    | None ->
                        Printf.sprintf "Error: failed to correct memory #%d."
                          memory_id
                    | Some updated ->
                        Printf.sprintf
                          "Corrected memory #%d '%s' for room '%s'.\n\
                           Old provenance preserved in correction trail."
                          updated.id updated.reference
                          (room_display_name room_id)))))
  | "correct" :: _ ->
      "Usage: clawq rooms memory correct <room_id> <id> <new_content>"
  | "forget" :: room_id :: memory_id_str :: flags -> (
      match check_grant ~room_id ~capability:"write" with
      | Error msg -> msg
      | Ok _scope -> (
          match int_of_string_opt memory_id_str with
          | None ->
              Printf.sprintf "Error: '%s' is not a valid memory ID."
                memory_id_str
          | Some memory_id -> (
              match Memory.get_scoped_memory ~db ~id:memory_id with
              | None -> Printf.sprintf "Memory #%d not found." memory_id
              | Some m when m.scope_kind <> "room" || m.scope_key <> room_id ->
                  Printf.sprintf "Memory #%d does not belong to room '%s'."
                    memory_id room_id
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
                  if hard_purge then begin
                    match
                      require_admin_audited ~room_id ~action:"memory_hard_purge"
                    with
                    | Some err -> err
                    | None ->
                        if Memory.delete_scoped_memory ~db ~id:memory_id then
                          Printf.sprintf
                            "Hard-purged memory #%d '%s' for room '%s'."
                            memory_id m.reference
                            (room_display_name room_id)
                        else
                          Printf.sprintf
                            "Error: failed to hard-purge memory #%d." memory_id
                  end
                  else if m.redacted_at <> None then
                    Printf.sprintf
                      "Memory #%d is already redacted. Use --hard to purge."
                      memory_id
                  else if
                    Memory.redact_scoped_memory ~db ~id:memory_id ~reason ()
                  then
                    Printf.sprintf
                      "Forgot (redacted) memory #%d '%s' for room '%s'. \
                       Content is now hidden."
                      memory_id m.reference
                      (room_display_name room_id)
                  else
                    Printf.sprintf "Error: failed to redact memory #%d."
                      memory_id)))
  | "forget" :: _ ->
      "Usage: clawq rooms memory forget <room_id> <id> [-- --hard] [reason]\n\
       Note: use -- separator before flags when invoking from CLI."
  | "team-grant" :: "add" :: room_id :: memory_id_str :: principal_kind
    :: principal_id :: _ -> (
      match require_admin () with
      | Some err -> err
      | None -> (
          match int_of_string_opt memory_id_str with
          | None ->
              Printf.sprintf "Error: '%s' is not a valid memory ID."
                memory_id_str
          | Some memory_id -> (
              match Memory.get_scoped_memory ~db ~id:memory_id with
              | None -> Printf.sprintf "Memory #%d not found." memory_id
              | Some m when m.scope_kind <> "room" || m.scope_key <> room_id ->
                  Printf.sprintf "Memory #%d does not belong to room '%s'."
                    memory_id room_id
              | Some m when m.visibility <> Memory.Team ->
                  Printf.sprintf
                    "Memory #%d has visibility '%s', not 'team'. Team grants \
                     only apply to team-visible memories."
                    memory_id
                    (Memory.visibility_to_string m.visibility)
              | Some _ -> (
                  match
                    Memory.add_team_grant ~db ~memory_id ~principal_kind
                      ~principal_id
                  with
                  | true ->
                      Printf.sprintf
                        "Added team grant for '%s:%s' on memory #%d."
                        principal_kind principal_id memory_id
                  | false ->
                      Printf.sprintf
                        "Team grant already exists for '%s:%s' on memory #%d."
                        principal_kind principal_id memory_id))))
  | "team-grant" :: "remove" :: room_id :: memory_id_str :: principal_kind
    :: principal_id :: _ -> (
      match require_admin () with
      | Some err -> err
      | None -> (
          match int_of_string_opt memory_id_str with
          | None ->
              Printf.sprintf "Error: '%s' is not a valid memory ID."
                memory_id_str
          | Some memory_id -> (
              match Memory.get_scoped_memory ~db ~id:memory_id with
              | None -> Printf.sprintf "Memory #%d not found." memory_id
              | Some m when m.scope_kind <> "room" || m.scope_key <> room_id ->
                  Printf.sprintf "Memory #%d does not belong to room '%s'."
                    memory_id room_id
              | Some _ ->
                  if
                    Memory.remove_team_grant ~db ~memory_id ~principal_kind
                      ~principal_id
                  then
                    Printf.sprintf
                      "Removed team grant for '%s:%s' on memory #%d."
                      principal_kind principal_id memory_id
                  else
                    Printf.sprintf
                      "No team grant found for '%s:%s' on memory #%d."
                      principal_kind principal_id memory_id)))
  | "team-grant" :: "list" :: room_id :: memory_id_str :: _ -> (
      match require_admin () with
      | Some err -> err
      | None -> (
          match int_of_string_opt memory_id_str with
          | None ->
              Printf.sprintf "Error: '%s' is not a valid memory ID."
                memory_id_str
          | Some memory_id -> (
              match Memory.get_scoped_memory ~db ~id:memory_id with
              | None -> Printf.sprintf "Memory #%d not found." memory_id
              | Some m when m.scope_kind <> "room" || m.scope_key <> room_id ->
                  Printf.sprintf "Memory #%d does not belong to room '%s'."
                    memory_id room_id
              | Some _ ->
                  let grants = Memory.list_team_grants ~db ~memory_id in
                  if grants = [] then
                    Printf.sprintf "No team grants for memory #%d." memory_id
                  else
                    let lines =
                      List.map
                        (fun (g : Memory.team_grant) ->
                          Printf.sprintf "  %s:%s (granted %s)" g.principal_kind
                            g.principal_id g.granted_at)
                        grants
                    in
                    Printf.sprintf "Team grants for memory #%d:\n%s" memory_id
                      (String.concat "\n" lines))))
  | "team-grant" :: _ ->
      "Usage: clawq rooms memory team-grant <add|remove|list> <room_id> \
       <memory_id> [principal_kind principal_id]"
  | "grant"
    :: ("add" | "create")
    :: room_id :: principal_kind :: principal_id :: capability :: _ -> (
      match require_admin () with
      | Some err -> err
      | None -> (
          (* Resolve profile name to ID if principal_kind is "profile" *)
          let resolved_principal_id =
            if principal_kind = "profile" then
              match Memory.get_room_profile_by_name ~db ~name:principal_id with
              | Some profile -> string_of_int profile.id
              | None -> principal_id
            else principal_id
          in
          (* Get or create scope *)
          let scope_result =
            match
              Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:room_id
            with
            | Some scope -> Ok scope
            | None ->
                (* Create scope for new room *)
                let binding = Memory.get_room_profile_binding ~db ~room_id in
                let profile_id =
                  Option.map
                    (fun (b : Memory.room_profile_binding) -> b.profile_id)
                    binding
                in
                Ok
                  (Memory.create_scope ~db ~kind:"room" ~key:room_id ?profile_id
                     ~provenance:"cli" ())
          in
          match scope_result with
          | Error msg -> Printf.sprintf "Error: %s" msg
          | Ok scope -> (
              match
                Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
                  ~principal_kind ~principal_id:resolved_principal_id
                  ~capability ()
              with
              | Ok () ->
                  Printf.sprintf
                    "Added grant '%s' for '%s:%s' on room '%s' scope #%d."
                    capability principal_kind principal_id room_id scope.id
              | Error msg -> Printf.sprintf "Error: %s" msg)))
  | "grant"
    :: ("remove" | "revoke")
    :: room_id :: principal_kind :: principal_id :: capability :: _ -> (
      match require_admin () with
      | Some err -> err
      | None -> (
          (* Resolve profile name to ID if principal_kind is "profile" *)
          let resolved_principal_id =
            if principal_kind = "profile" then
              match Memory.get_room_profile_by_name ~db ~name:principal_id with
              | Some profile -> string_of_int profile.id
              | None -> principal_id
            else principal_id
          in
          match Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:room_id with
          | None ->
              Printf.sprintf "No memory scope found for room '%s'." room_id
          | Some scope -> (
              match
                Memory.revoke_access ~db ~is_admin:true ~scope_id:scope.id
                  ~principal_kind ~principal_id:resolved_principal_id
                  ~capability ()
              with
              | Ok 0 ->
                  Printf.sprintf
                    "No grant '%s' found for '%s:%s' on room '%s' scope #%d."
                    capability principal_kind principal_id room_id scope.id
              | Ok _ ->
                  Printf.sprintf
                    "Removed grant '%s' for '%s:%s' on room '%s' scope #%d."
                    capability principal_kind principal_id room_id scope.id
              | Error msg -> Printf.sprintf "Error: %s" msg)))
  | "grant" :: "list" :: room_id :: _ -> (
      match require_admin () with
      | Some err -> err
      | None -> (
          match Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:room_id with
          | None ->
              Printf.sprintf "No memory scope found for room '%s'." room_id
          | Some scope ->
              let grants = Memory.list_grants ~db ~scope_id:scope.id in
              if grants = [] then
                Printf.sprintf "No grants for room '%s' scope #%d." room_id
                  scope.id
              else
                let lines =
                  List.map
                    (fun (g : Memory.scope_grant) ->
                      let expiry =
                        match g.expires_at with
                        | Some ts -> Printf.sprintf " (expires %s)" ts
                        | None -> ""
                      in
                      Printf.sprintf "  %s:%s -> %s%s (by %s:%s, %s)"
                        g.principal_kind g.principal_id g.capability expiry
                        g.grantor_kind g.grantor_id g.created_at)
                    grants
                in
                Printf.sprintf "Grants for room '%s' scope #%d:\n%s" room_id
                  scope.id (String.concat "\n" lines)))
  | "grant" :: _ ->
      "Usage: clawq rooms memory grant <add|create|remove|revoke|list> \
       <room_id> [principal_kind principal_id capability]"
  | _ ->
      "Usage: clawq rooms memory \
       <list|show|save|correct|forget|team-grant|grant> <room_id> [args...]\n\n\
       Subcommands:\n\
      \  list <room_id>              List memories in a room scope\n\
      \  show <room_id> <id>         Show details of a specific memory\n\
      \  save <room_id> <ref> <content> [--visibility V]\n\
      \                              Save or update a room-scoped memory\n\
      \  correct <room_id> <id> <content>\n\
      \                              Correct a memory (preserves old provenance)\n\
      \  forget <room_id> <id> [-- --hard] [reason]\n\
      \                              Forget (redact) a memory (admin: --hard \
       for purge)\n\
      \  team-grant <add|remove|list> <room_id> <id> [kind id]\n\
      \                              Manage team grants (admin-only)\n\
      \  grant <add|create|remove|revoke|list> <room_id> [kind id cap]\n\
      \                              Manage scope grants (admin-only)"
