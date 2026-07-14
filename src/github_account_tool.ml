(** Narrow agent tool: redacted GitHub account status only (P21.M4.E1.T001).

    Returns redacted account + preference inspection for the current Principal.
    Never exposes vault tokens, sealed ciphertext, vault row ids, authorization
    URLs, device codes, or callback errors. Authorization continuations and
    lifecycle mutations are out of scope for agents; they happen through the CLI
    (or the configured Connector / app) and are delivered privately via
    {!Github_user_auth_delivery}.

    Use this tool when an agent needs to know "which GitHub accounts does the
    current Principal have and what is their authorization status?" without
    touching credentials. *)

module Surf = Github_account_admin_surface
module P = Principal_identity

let schema_version = 1
let principal_env_var = "CLAWQ_PRINCIPAL_ID"

let current_principal_id () =
  match Sys.getenv_opt principal_env_var with
  | Some raw -> (
      let trimmed = String.trim raw in
      if trimmed = "" then
        Error
          (Printf.sprintf
             "github_account tool: %s must be set to a non-empty Principal id."
             principal_env_var)
      else
        match P.principal_id_of_string trimmed with
        | Ok pid -> Ok pid
        | Error e ->
            Error
              (Printf.sprintf "github_account tool: invalid Principal id: %s" e)
      )
  | None ->
      Error
        (Printf.sprintf
           "github_account tool: %s must be set so the tool can scope \
            inspection to the current Principal."
           principal_env_var)

let render_inspect (inspect : Surf.account_inspect) =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "GitHub account status (redacted)\n";
  Buffer.add_string buf
    (Printf.sprintf "  principal: %s\n  kind:      %s\n" inspect.principal_id
       inspect.surface_kind);
  Buffer.add_string buf
    (Printf.sprintf "  accounts:  %d\n" (List.length inspect.accounts));
  List.iter
    (fun (a : Surf.redacted_account) ->
      Buffer.add_string buf
        (Printf.sprintf
           "    - %s (%s) host=%s app=%d user=%Ld status=%s vault=%s \
            lineage=%s revision=%d\n"
           a.binding_id
           (Option.value ~default:"<unknown>" a.login)
           a.host a.app_id a.github_user_id a.authorization_status
           (if a.vault_attached then "attached" else "none")
           a.lineage_id a.revision))
    inspect.accounts;
  Buffer.add_string buf
    (Printf.sprintf "  preferences: %d\n" (List.length inspect.preferences));
  List.iter
    (fun (p : Surf.redacted_preference) ->
      Buffer.add_string buf
        (Printf.sprintf "    - %s binding=%s lineage=%s\n" p.scope
           (Option.value ~default:"<unset>" p.binding_id)
           (Option.value ~default:"<unset>" p.lineage_id)))
    inspect.preferences;
  Buffer.contents buf

let tool ~(db : Sqlite3.db) : Tool.t =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "binding_id",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String
                        "Optional binding id to inspect a single binding (with \
                         historical snapshots). Omit for the current \
                         Principal's full redacted account list." );
                  ] );
            ] );
        ("required", `List []);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"github_account" ~parameters_schema:schema
      ~detail
  in
  let open Yojson.Safe.Util in
  let get_binding_id args =
    try
      match args |> member "binding_id" with
      | `String s ->
          let trimmed = String.trim s in
          if trimmed = "" then Ok None else Ok (Some trimmed)
      | `Null -> Ok None
      | _ ->
          Error
            "github_account: parameter \"binding_id\" must be a string when \
             provided"
    with _ -> Ok None
  in
  {
    Tool.name = "github_account";
    description =
      "Inspect the current Principal's redacted GitHub account status. Returns \
       binding ids, logins, host/App/numeric GitHub user id, authorization \
       status (Pending/Authorized/Disabled/Revoked/Unlinked), vault-attached \
       flag (presence only), revision, lineage id, and stored Principal \
       preferences. Never exposes vault tokens, sealed ciphertext, vault row \
       ids, authorization URLs, device codes, or callback errors. Use this \
       when an agent needs to know which GitHub account the current Principal \
       is using; for lifecycle changes (link, relink, unlink) run the matching \
       `clawq github account ...` CLI command.";
    parameters_schema = schema;
    invoke =
      (fun ?context:_ args ->
        match current_principal_id () with
        | Error e -> Lwt.return e
        | Ok principal_id -> (
            match get_binding_id args with
            | Error e -> Lwt.return (param_err e)
            | Ok binding_id -> (
                match Surf.make_self_service ~principal_id () with
                | Error e -> Lwt.return ("Error: " ^ e)
                | Ok surface -> (
                    Surf.ensure_schema db;
                    match binding_id with
                    | None -> (
                        match Surf.inspect_accounts ~db ~surface () with
                        | Error e -> Lwt.return ("Error: " ^ e)
                        | Ok inspect -> Lwt.return (render_inspect inspect))
                    | Some id -> (
                        match
                          Surf.inspect_account ~db ~surface ~binding_id:id ()
                        with
                        | Error e -> Lwt.return ("Error: " ^ e)
                        | Ok (account, _snaps) ->
                            let buf = Buffer.create 128 in
                            Buffer.add_string buf
                              (Printf.sprintf
                                 "GitHub account %s (redacted)\n\
                                 \  principal:    %s\n\
                                 \  host/app:     %s/%d\n\
                                 \  user_id:      %Ld\n\
                                 \  login:        %s\n\
                                 \  status:       %s\n\
                                 \  revision:     %d\n\
                                 \  vault:        %s\n\
                                 \  lineage:      %s\n"
                                 account.binding_id
                                 (P.principal_id_to_string principal_id)
                                 account.host account.app_id
                                 account.github_user_id
                                 (Option.value ~default:"<unknown>"
                                    account.login)
                                 account.authorization_status account.revision
                                 (if account.vault_attached then "attached"
                                  else "none")
                                 account.lineage_id);
                            Lwt.return (Buffer.contents buf))))));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let disabled_tool : Tool.t =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ("properties", `Assoc []);
        ("required", `List []);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"github_account" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "github_account";
    description =
      "Disabled in the minimal build. Use the full `clawq` binary for redacted \
       GitHub account status.";
    parameters_schema = schema;
    invoke =
      (fun ?context:_ _args ->
        Lwt.return
          (param_err
             "github_account tool is disabled in the minimal build. Use the \
              full clawq binary for redacted GitHub account status."));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }
