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

(** Like [require_admin] but also records a denial event in the room activity
    ledger when the check fails. [room_id] identifies the affected room or
    profile scope; [action] describes the attempted operation. *)
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

let room_profile_deleted (p : Runtime_config.room_profile) =
  String.lowercase_ascii p.status = "deleted"
