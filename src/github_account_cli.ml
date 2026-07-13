(** Full-build command bridge for Principal-owned GitHub account lifecycle:
    list, status, preference selection (use), and link/relink/unlink flows
    (P21.M4.E1.T001).

    Every redacted surface path goes through {!Github_account_admin_surface};
    private authorization continuations route through
    {!Github_user_auth_delivery} so authorization URLs, device codes, and
    callbacks never appear on shared Room paths or in redacted summaries. *)

module Surf = Github_account_admin_surface
module P = Principal_identity
module B = Github_account_binding
module Pref = Github_account_preference
module D = Github_user_auth_delivery

(* -------------------------------------------------------------------------- *)
(* Principal resolution                                                        *)
(* -------------------------------------------------------------------------- *)

let principal_env_var = "CLAWQ_PRINCIPAL_ID"

let cli_principal_id () =
  match Sys.getenv_opt principal_env_var with
  | Some raw ->
      let trimmed = String.trim raw in
      if trimmed = "" then
        Error
          (Printf.sprintf
             "Error: %s must be set to a non-empty Principal id so the GitHub \
              account surface can be scoped to the current Principal."
             principal_env_var)
      else (
        match P.principal_id_of_string trimmed with
        | Ok pid -> Ok pid
        | Error e -> Error ("Error: invalid Principal id: " ^ e))
  | None ->
      Error
        (Printf.sprintf
           "Error: %s is required so `clawq github account ...` commands act \
            on the current Principal. Set %s=<principal-id> (see \
            `clawq principal list`)."
           principal_env_var principal_env_var)

(* -------------------------------------------------------------------------- *)
(* Arg helpers                                                                 *)
(* -------------------------------------------------------------------------- *)

let value_after flag args =
  let rec loop = function
    | key :: value :: _ when key = flag -> Some value
    | _ :: rest -> loop rest
    | [] -> None
  in
  loop args

let non_empty_trim label value =
  let v = String.trim value in
  if v = "" then Error (Printf.sprintf "Error: %s must be non-empty" label)
  else Ok v

(* -------------------------------------------------------------------------- *)
(* Self-service surface (current Principal only)                              *)
(* -------------------------------------------------------------------------- *)

let make_self_service ~db principal_id =
  Surf.ensure_schema db;
  Surf.make_self_service ~principal_id ()

(* -------------------------------------------------------------------------- *)
(* Redacted output                                                             *)
(* -------------------------------------------------------------------------- *)

let format_account_line (a : Surf.redacted_account) =
  Printf.sprintf
    "- %s (binding %s)\n    host=%s app=%d user=%Ld login=%s status=%s \
     revision=%d vault=%s lineage=%s"
    (match a.login with Some l -> l | None -> "<unknown>")
    a.binding_id a.host a.app_id a.github_user_id
    (Option.value ~default:"<none>" a.login)
    a.authorization_status a.revision
    (if a.vault_attached then "attached" else "none") a.lineage_id

let format_preference_line (p : Surf.redacted_preference) =
  Printf.sprintf "  %s binding=%s lineage=%s (rev=%d, updated=%s)"
    p.scope
    (Option.value ~default:"<unset>" p.binding_id)
    (Option.value ~default:"<unset>" p.lineage_id)
    p.revision p.updated_at

let render_inspect (inspect : Surf.account_inspect) =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "GitHub account surface (redacted)\n";
  Buffer.add_string buf (Printf.sprintf "  principal: %s\n" inspect.principal_id);
  (match inspect.admin_principal_id with
  | Some id ->
      Buffer.add_string buf (Printf.sprintf "  admin:     %s\n" id);
      (match inspect.admin_reason with
      | Some reason ->
          Buffer.add_string buf (Printf.sprintf "  reason:    %s\n" reason)
      | None -> ())
  | None -> ());
  Buffer.add_string buf (Printf.sprintf "  kind:      %s\n" inspect.surface_kind);
  Buffer.add_string buf
    (Printf.sprintf "  accounts:  %d\n"
       (List.length inspect.accounts));
  List.iter (fun a ->
      Buffer.add_char buf '\n';
      Buffer.add_string buf (format_account_line a);
      Buffer.add_char buf '\n')
    inspect.accounts;
  Buffer.add_string buf
    (Printf.sprintf "\n  preferences: %d\n"
       (List.length inspect.preferences));
  List.iter
    (fun p ->
      Buffer.add_string buf (format_preference_line p);
      Buffer.add_char buf '\n')
    inspect.preferences;
  Buffer.add_string buf "\nNotes:\n";
  List.iter
    (fun note ->
      Buffer.add_string buf "  - ";
      Buffer.add_string buf note;
      Buffer.add_char buf '\n')
    inspect.notes;
  Buffer.contents buf

let render_single_account ~principal_id (a : Surf.redacted_account)
    (snaps : Surf.redacted_snapshot list) =
  let buf = Buffer.create 256 in
  Buffer.add_string buf
    (Printf.sprintf "GitHub account %s (principal %s)\n" a.binding_id
       principal_id);
  Buffer.add_string buf
    (Printf.sprintf "  lineage:    %s\n" a.lineage_id);
  Buffer.add_string buf
    (Printf.sprintf "  host/app:   %s/%d\n" a.host a.app_id);
  Buffer.add_string buf
    (Printf.sprintf "  user_id:    %Ld\n" a.github_user_id);
  Buffer.add_string buf
    (Printf.sprintf "  login:      %s\n"
       (Option.value ~default:"<unknown>" a.login));
  Buffer.add_string buf
    (Printf.sprintf "  status:     %s\n" a.authorization_status);
  Buffer.add_string buf
    (Printf.sprintf "  revision:   %d\n" a.revision);
  Buffer.add_string buf
    (Printf.sprintf "  vault:      %s\n"
       (if a.vault_attached then "attached" else "none"));
  Buffer.add_string buf
    (Printf.sprintf "  created:    %s\n" a.created_at);
  Buffer.add_string buf
    (Printf.sprintf "  updated:    %s\n" a.updated_at);
  if snaps = [] then Buffer.add_string buf "  snapshots:  0\n"
  else begin
    Buffer.add_string buf
      (Printf.sprintf "  snapshots:  %d\n" (List.length snaps));
    List.iter
      (fun (s : Surf.redacted_snapshot) ->
        Buffer.add_string buf
          (Printf.sprintf
             "    - %s @ %s (%s) prior_status=%s prior_login=%s\n"
             s.snapshot_id s.created_at
             (match s.principal_id_at_snapshot with
              | p when p = principal_id -> "owner=current"
              | p -> "owner=" ^ p)
             (Option.value ~default:"-" s.authorization_status_at_snapshot)
             (Option.value ~default:"-" s.login_at_snapshot)))
      snaps
  end;
  Buffer.contents buf

(* -------------------------------------------------------------------------- *)
(* Subcommands                                                                 *)
(* -------------------------------------------------------------------------- *)

let cmd_list ~db ~principal_id () =
  match make_self_service ~db principal_id with
  | Error e -> "Error: " ^ e
  | Ok surface -> (
      match Surf.inspect_accounts ~db ~surface () with
      | Error e -> "Error: " ^ e
      | Ok inspect -> render_inspect inspect)

let cmd_status ~db ~principal_id binding_id =
  match make_self_service ~db principal_id with
  | Error e -> "Error: " ^ e
  | Ok surface -> (
      match binding_id with
      | None -> cmd_list ~db ~principal_id ()
      | Some id -> (
          match
            (non_empty_trim "binding_id" id : (string, string) result) with
          | Error e -> "Error: " ^ e
          | Ok trimmed -> (
              match
                Surf.inspect_account ~db ~surface ~binding_id:trimmed ()
              with
              | Error e -> "Error: " ^ e
              | Ok (account, snaps) ->
                  render_single_account
                    ~principal_id:(P.principal_id_to_string principal_id)
                    account snaps)))

(* `use` selects the current Principal's Principal_default preference for the
   supplied binding. Resolves lineage id from the binding when missing so the
   stored value remains stable across display updates. *)
let cmd_use ~db ~principal_id binding_id rest =
  match non_empty_trim "binding_id" binding_id with
  | Error e -> "Error: " ^ e
  | Ok id -> begin
      match make_self_service ~db principal_id with
      | Error e -> "Error: " ^ e
      | Ok surface -> begin
          match B.get ~db ~id with
          | Error e -> "Error: " ^ e
          | Ok None -> "Error: binding not found: " ^ id
          | Ok Some binding ->
              if
                not
                  (String.equal
                     (P.principal_id_to_string binding.principal_id)
                     (P.principal_id_to_string principal_id))
              then
                Printf.sprintf
                  "Error: binding %s is not owned by the current Principal \
                   (use the Principal that owns it)"
                  id
              else begin
                match
                  Pref.make_preference_value
                    ~binding_id:binding.id ~lineage_id:binding.lineage_id ()
                with
                | Error e -> "Error: " ^ e
                | Ok value ->
                    (* --host is accepted for symmetry with the resolution
                       context but the stored preference key is the
                       Principal_default scope; resolve applies the host
                       filter at lookup time. *)
                    let _ =
                      match value_after "--host" rest with
                      | Some h -> String.trim h
                      | None -> binding.identity.host
                    in
                    let scope = Pref.Principal_default in
                    begin
                      match
                        Surf.set_preference ~db ~surface ~scope ~value ()
                      with
                      | Error e -> "Error: " ^ e
                      | Ok stored -> (
                          match stored.binding_id with
                          | Some bid ->
                              Printf.sprintf
                                "Selected GitHub account %s (login=%s) as \
                                 Principal_default.\n\n  scope=%s\n  \
                                 binding=%s\n  lineage=%s\n  revision=%d\n  \
                                 updated=%s\n"
                                binding.id
                                (Option.value ~default:"<unknown>"
                                   binding.display.login)
                                stored.scope bid
                                (Option.value ~default:"-" stored.lineage_id)
                                stored.revision stored.updated_at
                          | None ->
                              "Error: preference stored without binding id")
                    end
              end
        end
    end

(* `link` / `relink` are guidance-only on the full CLI: starting a private
   PKCE/device authorization requires a configured OAuth App, registered
   redirect URI, and master-key readiness — those prerequisites live outside
   the redacted surface and would otherwise leak App metadata into Room-bound
   output. Private continuation delivery is wired through
   {!Github_user_auth_delivery} once the flow is started by the configured
   App/Connector path.

   The guidance is intentionally secret-free and refuses in the minimal
   build via [Github_account_cli_min]. *)
let link_guidance subcommand =
  Printf.sprintf
    "GitHub account %s is delivered through the configured Connector's \
     private continuation. The authorization URL, device codes, and callback \
     results are routed only to authenticated private channels (Connector DM, \
     Principal browser continuation, or the initiating CLI); this CLI never \
     embeds them in redacted output.\n\n\
     Next steps:\n\
     1. Verify the Connector or Principal-bound web surface is configured.\n\
     2. Trigger the private authorization from a Room or from the configured \
     app — the deliverer records a continuation_handle and delivers the URL \
     privately.\n\
     3. Re-run `clawq github account status` to inspect redacted state once \
     the authorization completes.\n\n\
     No tokens, codes, or URLs are returned here. Use the configured app or \
     Connector to continue."
    subcommand

let cmd_link () = link_guidance "link"

let cmd_relink ~db ~principal_id binding_id =
  match non_empty_trim "binding_id" binding_id with
  | Error e -> "Error: " ^ e
  | Ok id -> (
      match make_self_service ~db principal_id with
      | Error e -> "Error: " ^ e
      | Ok surface -> (
          match
            Surf.plan_account_action ~db ~surface ~kind:Surf.Revoke ~binding_id:id
              ()
          with
          | Error e -> "Error: " ^ e
          | Ok plan ->
              let buf = Buffer.create 256 in
              Buffer.add_string buf
                (Printf.sprintf
                   "Relink plan (revoke then private re-authorize)\n\
                    kind:        %s\n\
                    binding:     %s\n\
                    lineage:     %s\n\
                    principal:   %s\n\
                    vault:       %s\n\
                    revision:    %d\n\
                    digest:      %s\n\
                    conflicts:   %d\n\n\
                    Review, then run:\n\
                    CLAWQ_PRINCIPAL_ID=%s clawq github account unlink %s %s\n\n\
                    After unlink completes, follow the link guidance to start \
                    a private continuation for the next account.\n"
                   (Surf.string_of_account_action_kind plan.kind)
                   plan.binding_id plan.lineage_id plan.principal_id
                   (if plan.vault_attached then "attached" else "none")
                   plan.expected_binding_revision plan.digest
                   (List.length plan.hard_conflicts)
                   (P.principal_id_to_string principal_id) plan.binding_id
                   plan.digest);
              if plan.hard_conflicts <> [] then begin
                Buffer.add_string buf
                  "Hard conflicts (must be resolved before apply):\n";
                List.iter
                  (fun (c : Surf.conflict) ->
                    Buffer.add_string buf
                      (Printf.sprintf "  - [%s] %s\n" c.code c.summary))
                  plan.hard_conflicts
              end;
              Buffer.contents buf))

(* `unlink BINDING_ID [DIGEST]` — with no digest, builds and shows a plan; with
   a matching digest, applies the plan and reports the redacted receipt. *)
let cmd_unlink ~db ~principal_id binding_id rest =
  match non_empty_trim "binding_id" binding_id with
  | Error e -> "Error: " ^ e
  | Ok id -> (
      match make_self_service ~db principal_id with
      | Error e -> "Error: " ^ e
      | Ok surface -> (
          match value_after "--digest" rest with
          | Some digest -> (
              match Surf.plan_account_action ~db ~surface
                      ~kind:Surf.Unlink_account ~binding_id:id () with
              | Error e -> "Error: " ^ e
              | Ok plan ->
                  if not (String.equal plan.digest digest) then
                    Printf.sprintf
                      "Error: presented digest %s does not match current plan \
                       digest %s. Re-run `clawq github account unlink %s` \
                       without --digest to see the current plan."
                      digest plan.digest id
                  else
                    match
                      Surf.apply_account_action ~db ~surface ~plan
                        ~presented_digest:digest ()
                    with
                    | Surf.Applied receipt ->
                        Printf.sprintf
                          "Unlinked binding %s (lineage %s).\n  \
                           previous_status: %s\n  new_status:     %s\n  \
                           revision:        %d\n  vault_ref:      %s\n  \
                           snapshot:        %s\n  applied_at:     %s"
                          receipt.binding_id receipt.lineage_id
                          receipt.previous_status receipt.new_status
                          receipt.binding_revision_after
                          (if receipt.vault_ref_cleared then "cleared"
                           else "retained")
                          (Option.value ~default:"-" receipt.snapshot_id)
                          receipt.applied_at
                    | Surf.Refused { reason; conflicts } ->
                        Printf.sprintf "Error: unlink refused: %s%s"
                          reason
                          (if conflicts = [] then ""
                           else
                             "\n  conflicts:\n"
                             ^ String.concat ""
                                 (List.map
                                    (fun (c : Surf.conflict) ->
                                      Printf.sprintf "  - [%s] %s\n" c.code
                                        c.summary)
                                    conflicts))
                    | Surf.Stale_revision msg ->
                        Printf.sprintf
                          "Error: binding revision conflict: %s. Re-run \
                           `clawq github account unlink %s` to refresh the \
                           plan."
                          msg id)
          | None -> (
              match Surf.plan_account_action ~db ~surface
                      ~kind:Surf.Unlink_account ~binding_id:id () with
              | Error e -> "Error: " ^ e
              | Ok plan ->
                  let buf = Buffer.create 256 in
                  Buffer.add_string buf
                    (Printf.sprintf
                       "Unlink plan\n\
                        kind:        %s\n\
                        binding:     %s\n\
                        lineage:     %s\n\
                        principal:   %s\n\
                        vault:       %s\n\
                        revision:    %d\n\
                        digest:      %s\n\
                        conflicts:   %d\n\
                        snapshot:    %s\n\n\
                        To apply, run:\n\
                        CLAWQ_PRINCIPAL_ID=%s clawq github account unlink %s \
                        --digest %s\n"
                       (Surf.string_of_account_action_kind plan.kind)
                       plan.binding_id plan.lineage_id plan.principal_id
                       (if plan.vault_attached then "attached" else "none")
                       plan.expected_binding_revision plan.digest
                       (List.length plan.hard_conflicts)
                       (if plan.will_snapshot then "yes" else "no")
                       (P.principal_id_to_string principal_id) plan.binding_id
                       plan.digest);
                  if plan.hard_conflicts <> [] then begin
                    Buffer.add_string buf
                      "Hard conflicts (must be resolved before apply):\n";
                    List.iter
                      (fun (c : Surf.conflict) ->
                        Buffer.add_string buf
                          (Printf.sprintf "  - [%s] %s\n" c.code c.summary))
                      plan.hard_conflicts
                  end;
                  Buffer.contents buf)))

(* -------------------------------------------------------------------------- *)
(* Entry points                                                                *)
(* -------------------------------------------------------------------------- *)

let usage () =
  Printf.sprintf
    "Usage: clawq github account <subcommand> [args]\n\n\
     Subcommands (all require CLAWQ_PRINCIPAL_ID=<principal-id>):\n\
       list                       Redacted accounts + preferences for the \
     current Principal.\n\
       status [BINDING_ID]        Status of every binding, or one binding \
     with historical snapshots.\n\
       use BINDING_ID [--host H]  Set the Principal_default preference to \
     BINDING_ID.\n\
       link                       Print private-continuation guidance for \
     starting a link.\n\
       relink BINDING_ID          Show a revoke plan + private-link guidance.\n\
       unlink BINDING_ID [--digest D]\n\
                                   Show an unlink plan; apply with --digest.\n\n\
     All output is redacted. Authorization URLs, device codes, callback \
     errors, and account-control payloads are delivered privately via \
     %s and never appear in this CLI output."
    "Github_user_auth_delivery"

let cmd_with_db ~db args =
  let rec resolve_principal = function
    | "list" :: _ ->
        (match cli_principal_id () with
         | Error e -> e
         | Ok pid -> cmd_list ~db ~principal_id:pid ())
    | "status" :: [] ->
        (match cli_principal_id () with
         | Error e -> e
         | Ok pid -> cmd_status ~db ~principal_id:pid None)
    | "status" :: id :: _ ->
        (match cli_principal_id () with
         | Error e -> e
         | Ok pid -> cmd_status ~db ~principal_id:pid (Some id))
    | "use" :: binding_id :: rest ->
        (match cli_principal_id () with
         | Error e -> e
         | Ok pid -> cmd_use ~db ~principal_id:pid binding_id rest)
    | "link" :: [] -> cmd_link ()
    | "link" :: _ ->
        "Error: `clawq github account link` takes no arguments"
    | "relink" :: binding_id :: [] ->
        (match cli_principal_id () with
         | Error e -> e
         | Ok pid -> cmd_relink ~db ~principal_id:pid binding_id)
    | "relink" :: _ ->
        "Error: `clawq github account relink BINDING_ID` expects exactly one \
         binding id"
    | "unlink" :: binding_id :: rest ->
        (match cli_principal_id () with
         | Error e -> e
         | Ok pid -> cmd_unlink ~db ~principal_id:pid binding_id rest)
    | "unlink" :: _ ->
        "Error: `clawq github account unlink BINDING_ID [--digest D]` expects \
         a binding id"
    | _ -> usage ()
  in
  match args with
  | "account" :: rest -> resolve_principal rest
  | _ -> usage ()

let cmd args =
  let db = Command_bridge_helpers.get_db () in
  Surf.ensure_schema db;
  cmd_with_db ~db args