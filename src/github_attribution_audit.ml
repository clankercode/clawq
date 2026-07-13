(* Attribution previews, receipts, repair states, and audit (P21.M3.E2.T005).
   See github_attribution_audit.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module A = Actor_snapshot
module Auth = Github_attribution_authorize
module Fallback = Github_attribution_fallback
module Policy = Github_attribution_policy
module Reconcile = Github_action_reconcile
module PI = Principal_identity

let schema_version = 1

(* -------------------------------------------------------------------------- *)
(* Kinds                                                                       *)
(* -------------------------------------------------------------------------- *)

type record_kind = Preview | Receipt | Repair_state | Audit

let record_kind_to_string = function
  | Preview -> "preview"
  | Receipt -> "receipt"
  | Repair_state -> "repair_state"
  | Audit -> "audit"

let record_kind_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "preview" -> Ok Preview
  | "receipt" -> Ok Receipt
  | "repair_state" | "repair" -> Ok Repair_state
  | "audit" -> Ok Audit
  | other -> Error (Printf.sprintf "unknown record_kind %S" other)

type result_kind =
  | Allowed
  | Denied
  | Fallback_app
  | Completed
  | Failed
  | Pending_repair
  | Reconfirm

let result_kind_to_string = function
  | Allowed -> "allowed"
  | Denied -> "denied"
  | Fallback_app -> "fallback_app"
  | Completed -> "completed"
  | Failed -> "failed"
  | Pending_repair -> "pending_repair"
  | Reconfirm -> "reconfirm"

let result_kind_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "allowed" | "allow" -> Ok Allowed
  | "denied" | "deny" -> Ok Denied
  | "fallback_app" | "app_fallback" -> Ok Fallback_app
  | "completed" | "complete" -> Ok Completed
  | "failed" | "fail" -> Ok Failed
  | "pending_repair" | "repair" -> Ok Pending_repair
  | "reconfirm" | "reconfirmation" -> Ok Reconfirm
  | other -> Error (Printf.sprintf "unknown result_kind %S" other)

type failure_class =
  | Sso
  | Permission
  | Refresh
  | Revocation
  | App_scope
  | Rollout_gate
  | Ambiguity
  | Identity
  | Policy
  | Confirmation
  | Live_state
  | Fallback
  | Other of string

let failure_class_to_string = function
  | Sso -> "sso"
  | Permission -> "permission"
  | Refresh -> "refresh"
  | Revocation -> "revocation"
  | App_scope -> "app_scope"
  | Rollout_gate -> "rollout_gate"
  | Ambiguity -> "ambiguity"
  | Identity -> "identity"
  | Policy -> "policy"
  | Confirmation -> "confirmation"
  | Live_state -> "live_state"
  | Fallback -> "fallback"
  | Other s -> "other:" ^ s

let failure_class_of_string s =
  let s = String.lowercase_ascii (String.trim s) in
  match s with
  | "sso" -> Ok Sso
  | "permission" | "permissions" -> Ok Permission
  | "refresh" -> Ok Refresh
  | "revocation" | "revoked" -> Ok Revocation
  | "app_scope" | "appscope" -> Ok App_scope
  | "rollout_gate" | "rollout" | "gate" -> Ok Rollout_gate
  | "ambiguity" | "ambiguous" -> Ok Ambiguity
  | "identity" -> Ok Identity
  | "policy" -> Ok Policy
  | "confirmation" -> Ok Confirmation
  | "live_state" | "live" -> Ok Live_state
  | "fallback" -> Ok Fallback
  | other when String.length other > 6 && String.sub other 0 6 = "other:" ->
      Ok (Other (String.sub other 6 (String.length other - 6)))
  | other -> Error (Printf.sprintf "unknown failure_class %S" other)

let contains_substring s ~sub =
  let n = String.length sub in
  let len = String.length s in
  if n = 0 then true
  else if n > len then false
  else
    let rec loop i =
      if i + n > len then false
      else if String.sub s i n = sub then true
      else loop (i + 1)
    in
    loop 0

let classify_failure ?failed_check ?code () =
  let code =
    Option.map (fun c -> String.lowercase_ascii (String.trim c)) code
    |> Option.value ~default:""
  in
  let check =
    Option.map (fun c -> String.lowercase_ascii (String.trim c)) failed_check
    |> Option.value ~default:""
  in
  match code with
  | "sso_required" -> Sso
  | "permissions_insufficient" | "org_policy_denied" -> Permission
  | "stale_vault_generation" | "generation_race" | "refresh_failed"
  | "token_refresh_required" ->
      Refresh
  | "vault_inactive" | "binding_not_authorized" | "user_authority_lost"
  | "installation_inactive" | "revoked" | "binding_revoked" ->
      Revocation
  | "installation_repo_denied" | "app_scope_denied" | "repo_not_in_selection" ->
      App_scope
  | "attribution_gate_disabled" | "rollout_gate_disabled"
  | "pilot_gate_disabled" | "user_required_disabled" ->
      Rollout_gate
  | "account_ambiguous" -> Ambiguity
  | "empty_principal" | "principal_not_current" | "lineage_mismatch"
  | "stale_binding_lineage" | "binding_required" | "no_eligible_account"
  | "stale_actor_snapshot" | "stale_principal_revision" | "empty_action" ->
      Identity
  | "tool_not_in_catalog" | "stale_tool_catalog_revision" | "empty_repo"
  | "repo_blocked" | "repo_not_granted" | "stale_access_revision" ->
      Policy
  | "confirmation_required" | "stale_confirmation" -> Confirmation
  | "live_state_failed" | "stale_live_state_revision" -> Live_state
  | "user_required_no_fallback" | "app_fallback_not_previewed"
  | "post_confirm_authority_lost" | "actor_mode_locked"
  | "pat_fallback_forbidden" ->
      Fallback
  | _ -> (
      match check with
      | "user_org_sso" ->
          if contains_substring code ~sub:"sso" || code = "sso_required" then
            Sso
          else if contains_substring code ~sub:"user" then Revocation
          else if code = "" then Sso
          else Permission
      | "installation" ->
          if contains_substring code ~sub:"permission" then Permission
          else if contains_substring code ~sub:"repo" then App_scope
          else Revocation
      | "binding" ->
          if contains_substring code ~sub:"ambiguous" then Ambiguity
          else if contains_substring code ~sub:"generation" then Refresh
          else Identity
      | "principal" | "actor_snapshot" -> Identity
      | "confirmation" -> Confirmation
      | "live_action" -> Live_state
      | "fallback" -> Fallback
      | "tool_catalog" | "repo_grant" | "policy" -> Policy
      | "" when code = "" -> Other "unspecified"
      | other when other <> "" -> Other other
      | _ -> Other (if code <> "" then code else "unspecified"))

(* -------------------------------------------------------------------------- *)
(* GitHub actor / lineage                                                      *)
(* -------------------------------------------------------------------------- *)

type github_actor =
  | Numeric_user of { host : string; app_id : int; github_user_id : int64 }
  | App of { installation_id : int option; app_id : int option }
  | Unspecified

let github_actor_to_string = function
  | Numeric_user { host; app_id; github_user_id } ->
      Printf.sprintf "user:host=%s:app=%d:uid=%Ld" host app_id github_user_id
  | App { installation_id; app_id } ->
      let inst =
        match installation_id with None -> "-" | Some i -> string_of_int i
      in
      let app = match app_id with None -> "-" | Some i -> string_of_int i in
      Printf.sprintf "app:installation=%s:app_id=%s" inst app
  | Unspecified -> "unspecified"

let github_actor_to_json = function
  | Numeric_user { host; app_id; github_user_id } ->
      `Assoc
        [
          ("kind", `String "numeric_user");
          ("host", `String host);
          ("app_id", `Int app_id);
          ("github_user_id", `String (Int64.to_string github_user_id));
        ]
  | App { installation_id; app_id } ->
      `Assoc
        [
          ("kind", `String "app");
          ( "installation_id",
            match installation_id with None -> `Null | Some i -> `Int i );
          ("app_id", match app_id with None -> `Null | Some i -> `Int i);
        ]
  | Unspecified -> `Assoc [ ("kind", `String "unspecified") ]

let github_actor_of_json = function
  | `Assoc fields as json -> (
      let member k =
        match List.assoc_opt k fields with Some v -> v | None -> `Null
      in
      match member "kind" with
      | `String "numeric_user" -> (
          match (member "host", member "app_id", member "github_user_id") with
          | `String host, `Int app_id, `String uid -> (
              match Int64.of_string_opt uid with
              | Some github_user_id ->
                  Ok (Numeric_user { host; app_id; github_user_id })
              | None -> Error "github_user_id not an int64 string")
          | `String host, `Int app_id, `Int uid ->
              Ok
                (Numeric_user
                   { host; app_id; github_user_id = Int64.of_int uid })
          | _ -> Error "numeric_user actor missing host/app_id/github_user_id")
      | `String "app" ->
          let installation_id =
            match member "installation_id" with
            | `Int i -> Some i
            | `Null | _ -> None
          in
          let app_id =
            match member "app_id" with `Int i -> Some i | `Null | _ -> None
          in
          Ok (App { installation_id; app_id })
      | `String "unspecified" | `Null -> Ok Unspecified
      | `String other ->
          Error (Printf.sprintf "unknown github_actor kind %S" other)
      | _ ->
          Error
            (Printf.sprintf "github_actor missing kind: %s"
               (Yojson.Safe.to_string json)))
  | _ -> Error "github_actor must be a JSON object"

type lineage_pin = {
  principal_id : string option;
  principal_revision : int option;
  actor_identity_key : string option;
  actor_revision : int option;
  identity_link_revision : int option;
  account_lineage_id : string option;
  binding_id : string option;
}

let empty_lineage_pin =
  {
    principal_id = None;
    principal_revision = None;
    actor_identity_key = None;
    actor_revision = None;
    identity_link_revision = None;
    account_lineage_id = None;
    binding_id = None;
  }

let opt_string_json = function None -> `Null | Some s -> `String s
let opt_int_json = function None -> `Null | Some n -> `Int n

let lineage_pin_to_json (l : lineage_pin) =
  `Assoc
    [
      ("principal_id", opt_string_json l.principal_id);
      ("principal_revision", opt_int_json l.principal_revision);
      ("actor_identity_key", opt_string_json l.actor_identity_key);
      ("actor_revision", opt_int_json l.actor_revision);
      ("identity_link_revision", opt_int_json l.identity_link_revision);
      ("account_lineage_id", opt_string_json l.account_lineage_id);
      ("binding_id", opt_string_json l.binding_id);
    ]

let lineage_pin_of_snapshot (s : A.t) : lineage_pin =
  {
    principal_id = Some (PI.principal_id_to_string s.lineage.principal_id);
    principal_revision = Some s.lineage.principal_revision;
    actor_identity_key = Some (PI.actor_identity_key s.lineage.actor_key);
    actor_revision = Some s.lineage.actor_revision;
    identity_link_revision = Some s.lineage.identity_link_revision;
    account_lineage_id = s.lineage.account_lineage_id;
    binding_id =
      (match s.account_binding with
      | None -> None
      | Some b -> Some b.binding_id);
  }

let lineage_of_checked_revisions (r : Auth.checked_revisions) : lineage_pin =
  {
    principal_id = r.principal_id;
    principal_revision = r.principal_revision;
    actor_identity_key = None;
    actor_revision = r.actor_revision;
    identity_link_revision = r.identity_link_revision;
    account_lineage_id = r.binding_lineage_id;
    binding_id = r.binding_id;
  }

let github_actor_of_revisions (r : Auth.checked_revisions)
    ?binding_github_user_id () =
  match binding_github_user_id with
  | Some uid ->
      Numeric_user
        {
          host = "github.com";
          app_id = Option.value r.installation_id ~default:0;
          github_user_id = uid;
        }
  | None -> (
      match r.installation_id with
      | Some _ as i -> App { installation_id = i; app_id = None }
      | None -> Unspecified)

(* -------------------------------------------------------------------------- *)
(* Redaction                                                                   *)
(* -------------------------------------------------------------------------- *)

let redact_secret_free s =
  let s = String.trim s in
  let s =
    Str.global_replace
      (Str.regexp "[Bb]earer [A-Za-z0-9._+/=-]+")
      "Bearer [REDACTED]" s
  in
  let s =
    Str.global_replace
      (Str.regexp "\\(ghp\\|gho\\|ghu\\|ghs\\|ghr\\)_[A-Za-z0-9_]+")
      "\\1_[REDACTED]" s
  in
  let s =
    Str.global_replace
      (Str.regexp "github_pat_[A-Za-z0-9_]+")
      "github_pat_[REDACTED]" s
  in
  let s =
    Str.global_replace (Str.regexp "xox[baprs]-[A-Za-z0-9-]+") "xox*-REDACTED" s
  in
  let s =
    Str.global_replace
      (Str.regexp_case_fold
         "\\(token\\|secret\\|password\\|api_key\\|private_key\\|bot_token\\)[ \
          \\t]*[=:][ \\t]*[^ \\t,;]+")
      "\\1=[REDACTED]" s
  in
  let max_len = 512 in
  let len = String.length s in
  if len <= max_len then s
  else
    String.sub s 0 max_len ^ Printf.sprintf "...<%d more bytes>" (len - max_len)

let redact_opt = function None -> None | Some s -> Some (redact_secret_free s)

let trim_nonempty = function
  | None -> None
  | Some s -> ( match String.trim s with "" -> None | t -> Some t)

let normalize_mode s =
  let s = String.lowercase_ascii (String.trim s) in
  match s with
  | "app" | "app_installation" | "installation" -> "app"
  | "user" | "user_required" | "user_preferred" -> "user"
  | "pat" | "pat_compat" -> "pat"
  | "pilot" -> "pilot"
  | other -> other

(* -------------------------------------------------------------------------- *)
(* Record type                                                                 *)
(* -------------------------------------------------------------------------- *)

type t = {
  id : string;
  kind : record_kind;
  schema_version : int;
  created_at : string;
  action : string;
  item_key : string option;
  room_id : string option;
  confirmation_id : string option;
  job_id : string option;
  plan_id : string option;
  receipt_id : string option;
  requested_mode : string option;
  resolved_mode : string option;
  used_app_fallback : bool;
  fallback_reason : string option;
  github_actor : github_actor;
  lineage : lineage_pin;
  actor_snapshot : A.t option;
  actor_snapshot_id : string option;
  result : result_kind;
  failure_class : failure_class option;
  failure_code : string option;
  reason : string;
  revisions_json : string option;
}

let generate_id ?(now = Unix.gettimeofday ()) ?(kind = Audit) () =
  let ts = int_of_float (now *. 1000.0) in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghattr_%s_%Ld_%06d"
    (record_kind_to_string kind)
    (Int64.of_int ts) rand

let sanitize_snapshot = function
  | None -> None
  | Some snap -> (
      if A.is_authority snap then None
      else
        match A.of_json (A.to_json snap) with
        | Ok s -> Some s
        | Error _ -> Some snap)

let make ?id ?(now = Unix.gettimeofday ()) ~kind ~action ~result ~reason
    ?item_key ?room_id ?confirmation_id ?job_id ?plan_id ?receipt_id
    ?requested_mode ?resolved_mode ?(used_app_fallback = false) ?fallback_reason
    ?(github_actor = Unspecified) ?(lineage = empty_lineage_pin) ?actor_snapshot
    ?actor_snapshot_id ?failure_class ?failure_code ?revisions_json () =
  let action = String.trim action in
  let reason = redact_secret_free reason in
  if action = "" then Error "action must be non-empty"
  else if String.trim reason = "" then Error "reason must be non-empty"
  else
    match actor_snapshot with
    | Some snap when A.is_authority snap ->
        Error "actor_snapshot must not be reusable authority"
    | _ ->
        let actor_snapshot = sanitize_snapshot actor_snapshot in
        let lineage =
          match (actor_snapshot, lineage = empty_lineage_pin) with
          | Some s, true -> lineage_pin_of_snapshot s
          | _ -> lineage
        in
        let actor_snapshot_id =
          match actor_snapshot_id with
          | Some _ as i -> trim_nonempty i
          | None -> (
              match actor_snapshot with None -> None | Some s -> Some s.id)
        in
        let id =
          match id with
          | Some i when String.trim i <> "" -> String.trim i
          | _ -> generate_id ~now ~kind ()
        in
        let requested_mode =
          match trim_nonempty requested_mode with
          | None -> None
          | Some m -> Some (normalize_mode (redact_secret_free m))
        in
        let resolved_mode =
          match trim_nonempty resolved_mode with
          | None -> None
          | Some m -> Some (normalize_mode (redact_secret_free m))
        in
        Ok
          {
            id;
            kind;
            schema_version;
            created_at = Time_util.iso8601_utc ~t:now ();
            action = redact_secret_free action;
            item_key = redact_opt item_key;
            room_id = redact_opt room_id;
            confirmation_id = redact_opt confirmation_id;
            job_id = redact_opt job_id;
            plan_id = redact_opt plan_id;
            receipt_id = redact_opt receipt_id;
            requested_mode;
            resolved_mode;
            used_app_fallback;
            fallback_reason = redact_opt fallback_reason;
            github_actor;
            lineage =
              {
                principal_id = redact_opt lineage.principal_id;
                principal_revision = lineage.principal_revision;
                actor_identity_key = redact_opt lineage.actor_identity_key;
                actor_revision = lineage.actor_revision;
                identity_link_revision = lineage.identity_link_revision;
                account_lineage_id = redact_opt lineage.account_lineage_id;
                binding_id = redact_opt lineage.binding_id;
              };
            actor_snapshot;
            actor_snapshot_id;
            result;
            failure_class;
            failure_code = redact_opt failure_code;
            reason;
            revisions_json = redact_opt revisions_json;
          }

let to_json (r : t) =
  let failure_class_json =
    match r.failure_class with
    | None -> `Null
    | Some c -> `String (failure_class_to_string c)
  in
  let snapshot_json =
    match r.actor_snapshot with None -> `Null | Some s -> A.to_redacted_json s
  in
  `Assoc
    [
      ("schema_version", `Int r.schema_version);
      ("id", `String r.id);
      ("kind", `String (record_kind_to_string r.kind));
      ("created_at", `String r.created_at);
      ("action", `String r.action);
      ("item_key", opt_string_json r.item_key);
      ("room_id", opt_string_json r.room_id);
      ("confirmation_id", opt_string_json r.confirmation_id);
      ("job_id", opt_string_json r.job_id);
      ("plan_id", opt_string_json r.plan_id);
      ("receipt_id", opt_string_json r.receipt_id);
      ("requested_mode", opt_string_json r.requested_mode);
      ("resolved_mode", opt_string_json r.resolved_mode);
      ("used_app_fallback", `Bool r.used_app_fallback);
      ("fallback_reason", opt_string_json r.fallback_reason);
      ("github_actor", github_actor_to_json r.github_actor);
      ("lineage", lineage_pin_to_json r.lineage);
      ("actor_snapshot", snapshot_json);
      ("actor_snapshot_id", opt_string_json r.actor_snapshot_id);
      ("result", `String (result_kind_to_string r.result));
      ("failure_class", failure_class_json);
      ("failure_code", opt_string_json r.failure_code);
      ("reason", `String r.reason);
      ( "revisions_json",
        match r.revisions_json with
        | None -> `Null
        | Some s -> ( try Yojson.Safe.from_string s with _ -> `String s) );
      ("authority", `Bool false);
      ("issues_token", `Bool false);
      ("issues_lease", `Bool false);
    ]

let member_opt key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let string_field key json =
  match member_opt key json with
  | Some (`String s) when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let int_field key json =
  match member_opt key json with Some (`Int n) -> Some n | _ -> None

let bool_field key json ~default =
  match member_opt key json with Some (`Bool b) -> b | _ -> default

let of_json json =
  match json with
  | `Assoc _ as j ->
      let ( let* ) = Result.bind in
      let* kind =
        match string_field "kind" j with
        | None -> Error "missing kind"
        | Some s -> record_kind_of_string s
      in
      let* result =
        match string_field "result" j with
        | None -> Error "missing result"
        | Some s -> result_kind_of_string s
      in
      let* action =
        match string_field "action" j with
        | None -> Error "missing action"
        | Some a -> Ok a
      in
      let* reason =
        match string_field "reason" j with
        | None -> Error "missing reason"
        | Some r -> Ok r
      in
      let* id =
        match string_field "id" j with
        | None -> Error "missing id"
        | Some i -> Ok i
      in
      let* github_actor =
        match member_opt "github_actor" j with
        | None | Some `Null -> Ok Unspecified
        | Some g -> github_actor_of_json g
      in
      let* failure_class =
        match string_field "failure_class" j with
        | None -> Ok None
        | Some s -> (
            match failure_class_of_string s with
            | Ok c -> Ok (Some c)
            | Error e -> Error e)
      in
      let* actor_snapshot =
        match member_opt "actor_snapshot" j with
        | None | Some `Null -> Ok None
        | Some s -> (
            match A.of_json s with
            | Ok snap -> Ok (Some snap)
            | Error e -> Error e)
      in
      let lineage =
        match member_opt "lineage" j with
        | Some (`Assoc fields) ->
            let get_s k =
              match List.assoc_opt k fields with
              | Some (`String s) when String.trim s <> "" ->
                  Some (String.trim s)
              | _ -> None
            in
            let get_i k =
              match List.assoc_opt k fields with
              | Some (`Int n) -> Some n
              | _ -> None
            in
            {
              principal_id = get_s "principal_id";
              principal_revision = get_i "principal_revision";
              actor_identity_key = get_s "actor_identity_key";
              actor_revision = get_i "actor_revision";
              identity_link_revision = get_i "identity_link_revision";
              account_lineage_id = get_s "account_lineage_id";
              binding_id = get_s "binding_id";
            }
        | _ -> empty_lineage_pin
      in
      let revisions_json =
        match member_opt "revisions_json" j with
        | None | Some `Null -> None
        | Some (`String s) -> Some s
        | Some v -> Some (Yojson.Safe.to_string v)
      in
      Ok
        {
          id;
          kind;
          schema_version =
            (match int_field "schema_version" j with
            | Some n -> n
            | None -> schema_version);
          created_at =
            (match string_field "created_at" j with
            | Some t -> t
            | None -> Time_util.iso8601_utc ());
          action;
          item_key = string_field "item_key" j;
          room_id = string_field "room_id" j;
          confirmation_id = string_field "confirmation_id" j;
          job_id = string_field "job_id" j;
          plan_id = string_field "plan_id" j;
          receipt_id = string_field "receipt_id" j;
          requested_mode =
            Option.map normalize_mode (string_field "requested_mode" j);
          resolved_mode =
            Option.map normalize_mode (string_field "resolved_mode" j);
          used_app_fallback = bool_field "used_app_fallback" j ~default:false;
          fallback_reason = string_field "fallback_reason" j;
          github_actor;
          lineage;
          actor_snapshot;
          actor_snapshot_id = string_field "actor_snapshot_id" j;
          result;
          failure_class;
          failure_code = string_field "failure_code" j;
          reason;
          revisions_json;
        }
  | _ -> Error "attribution audit record must be a JSON object"

let redacted_summary (r : t) =
  Printf.sprintf
    "%s action=%s result=%s req=%s res=%s fallback=%b class=%s code=%s"
    (record_kind_to_string r.kind)
    r.action
    (result_kind_to_string r.result)
    (Option.value r.requested_mode ~default:"-")
    (Option.value r.resolved_mode ~default:"-")
    r.used_app_fallback
    (match r.failure_class with
    | None -> "-"
    | Some c -> failure_class_to_string c)
    (Option.value r.failure_code ~default:"-")

let is_immutable_evidence (r : t) =
  match r.actor_snapshot with None -> false | Some s -> not (A.is_authority s)

let contains_token_material (r : t) =
  let check_json s =
    try A.contains_token_material (Yojson.Safe.from_string s) with _ -> false
  in
  (match r.actor_snapshot with
    | Some s -> A.contains_token_material (A.to_json s)
    | None -> false)
  || check_json r.reason
  ||
  match r.revisions_json with
  | Some s -> check_json s
  | None ->
      false
      ||
      let blob = Yojson.Safe.to_string (to_json r) in
      A.contains_token_material (`String blob)

let denial_exposes_token ~record ~plaintext =
  plaintext <> ""
  &&
  let blob = Yojson.Safe.to_string (to_json record) ^ redacted_summary record in
  contains_substring blob ~sub:plaintext

(* -------------------------------------------------------------------------- *)
(* Schema / persistence                                                        *)
(* -------------------------------------------------------------------------- *)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_attribution_audit schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let ensure_schema db =
  Sqlite3.busy_timeout db 5_000;
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_attribution_audit (
      id TEXT PRIMARY KEY NOT NULL,
      kind TEXT NOT NULL,
      schema_version INTEGER NOT NULL,
      created_at TEXT NOT NULL,
      action TEXT NOT NULL,
      item_key TEXT,
      room_id TEXT,
      confirmation_id TEXT,
      job_id TEXT,
      plan_id TEXT,
      receipt_id TEXT,
      requested_mode TEXT,
      resolved_mode TEXT,
      used_app_fallback INTEGER NOT NULL DEFAULT 0,
      fallback_reason TEXT,
      github_actor_json TEXT NOT NULL,
      principal_id TEXT,
      principal_revision INTEGER,
      actor_identity_key TEXT,
      actor_revision INTEGER,
      identity_link_revision INTEGER,
      account_lineage_id TEXT,
      binding_id TEXT,
      actor_snapshot_json TEXT,
      actor_snapshot_id TEXT,
      actor_snapshot_authority INTEGER NOT NULL DEFAULT 0,
      result TEXT NOT NULL,
      failure_class TEXT,
      failure_code TEXT,
      reason TEXT NOT NULL,
      revisions_json TEXT
    )|}
  in
  let indexes =
    [
      {|CREATE INDEX IF NOT EXISTS idx_ghattr_audit_kind
        ON github_attribution_audit(kind)|};
      {|CREATE INDEX IF NOT EXISTS idx_ghattr_audit_action
        ON github_attribution_audit(action)|};
      {|CREATE INDEX IF NOT EXISTS idx_ghattr_audit_snapshot
        ON github_attribution_audit(actor_snapshot_id)
        WHERE actor_snapshot_id IS NOT NULL|};
      {|CREATE INDEX IF NOT EXISTS idx_ghattr_audit_principal
        ON github_attribution_audit(principal_id)
        WHERE principal_id IS NOT NULL|};
      {|CREATE INDEX IF NOT EXISTS idx_ghattr_audit_failure
        ON github_attribution_audit(failure_class)
        WHERE failure_class IS NOT NULL|};
      {|CREATE INDEX IF NOT EXISTS idx_ghattr_audit_receipt
        ON github_attribution_audit(receipt_id)
        WHERE receipt_id IS NOT NULL|};
    ]
  in
  List.iter (exec_schema db) (table_sql :: indexes)

let data_opt_text = function
  | None -> Sqlite3.Data.NULL
  | Some s -> Sqlite3.Data.TEXT s

let data_opt_int = function
  | None -> Sqlite3.Data.NULL
  | Some n -> Sqlite3.Data.INT (Int64.of_int n)

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | Sqlite3.Data.INT n -> Int64.to_string n
  | _ -> ""

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let opt_int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Some (Int64.to_int n)
  | _ -> None

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | _ -> 0

let snapshot_of_json_col = function
  | None -> None
  | Some s -> (
      match A.of_json (Yojson.Safe.from_string s) with
      | Ok snap -> Some snap
      | Error _ -> None)

let row_of_stmt stmt : t =
  let kind =
    match record_kind_of_string (text_col stmt 1) with
    | Ok k -> k
    | Error _ -> Audit
  in
  let result =
    match result_kind_of_string (text_col stmt 26) with
    | Ok r -> r
    | Error _ -> Failed
  in
  let failure_class =
    match opt_text_col stmt 27 with
    | None -> None
    | Some s -> (
        match failure_class_of_string s with Ok c -> Some c | _ -> None)
  in
  let github_actor =
    match github_actor_of_json (Yojson.Safe.from_string (text_col stmt 15)) with
    | Ok a -> a
    | Error _ -> Unspecified
  in
  let actor_snapshot = snapshot_of_json_col (opt_text_col stmt 23) in
  {
    id = text_col stmt 0;
    kind;
    schema_version = int_col stmt 2;
    created_at = text_col stmt 3;
    action = text_col stmt 4;
    item_key = opt_text_col stmt 5;
    room_id = opt_text_col stmt 6;
    confirmation_id = opt_text_col stmt 7;
    job_id = opt_text_col stmt 8;
    plan_id = opt_text_col stmt 9;
    receipt_id = opt_text_col stmt 10;
    requested_mode = opt_text_col stmt 11;
    resolved_mode = opt_text_col stmt 12;
    used_app_fallback = int_col stmt 13 <> 0;
    fallback_reason = opt_text_col stmt 14;
    github_actor;
    lineage =
      {
        principal_id = opt_text_col stmt 16;
        principal_revision = opt_int_col stmt 17;
        actor_identity_key = opt_text_col stmt 18;
        actor_revision = opt_int_col stmt 19;
        identity_link_revision = opt_int_col stmt 20;
        account_lineage_id = opt_text_col stmt 21;
        binding_id = opt_text_col stmt 22;
      };
    actor_snapshot;
    actor_snapshot_id = opt_text_col stmt 24;
    result;
    failure_class;
    failure_code = opt_text_col stmt 28;
    reason = text_col stmt 29;
    revisions_json = opt_text_col stmt 30;
  }

let select_columns =
  {|id, kind, schema_version, created_at, action, item_key, room_id,
    confirmation_id, job_id, plan_id, receipt_id, requested_mode, resolved_mode,
    used_app_fallback, fallback_reason, github_actor_json, principal_id,
    principal_revision, actor_identity_key, actor_revision,
    identity_link_revision, account_lineage_id, binding_id, actor_snapshot_json,
    actor_snapshot_id, actor_snapshot_authority, result, failure_class,
    failure_code, reason, revisions_json|}

let insert ~db ~record ?(now = Unix.gettimeofday ()) () =
  ensure_schema db;
  if String.trim record.action = "" then Error "action must be non-empty"
  else if String.trim record.reason = "" then Error "reason must be non-empty"
  else if
    match record.actor_snapshot with
    | Some s -> A.is_authority s
    | None -> false
  then Error "actor_snapshot must not be reusable authority"
  else
    match
      make ~id:record.id ~now ~kind:record.kind ~action:record.action
        ~result:record.result ~reason:record.reason ?item_key:record.item_key
        ?room_id:record.room_id ?confirmation_id:record.confirmation_id
        ?job_id:record.job_id ?plan_id:record.plan_id
        ?receipt_id:record.receipt_id ?requested_mode:record.requested_mode
        ?resolved_mode:record.resolved_mode
        ~used_app_fallback:record.used_app_fallback
        ?fallback_reason:record.fallback_reason
        ~github_actor:record.github_actor ~lineage:record.lineage
        ?actor_snapshot:record.actor_snapshot
        ?actor_snapshot_id:record.actor_snapshot_id
        ?failure_class:record.failure_class ?failure_code:record.failure_code
        ?revisions_json:record.revisions_json ()
    with
    | Error e -> Error e
    | Ok sanitized -> (
        let record = { sanitized with created_at = record.created_at } in
        let snap_json, authority =
          match record.actor_snapshot with
          | None -> (None, 0)
          | Some s -> (Some (Yojson.Safe.to_string (A.to_json s)), 0)
        in
        let github_actor_json =
          Yojson.Safe.to_string (github_actor_to_json record.github_actor)
        in
        let sql =
          {|INSERT INTO github_attribution_audit
        (id, kind, schema_version, created_at, action, item_key, room_id,
         confirmation_id, job_id, plan_id, receipt_id, requested_mode,
         resolved_mode, used_app_fallback, fallback_reason, github_actor_json,
         principal_id, principal_revision, actor_identity_key, actor_revision,
         identity_link_revision, account_lineage_id, binding_id,
         actor_snapshot_json, actor_snapshot_id, actor_snapshot_authority,
         result, failure_class, failure_code, reason, revisions_json)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
                ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
        in
        try
          Sql_util.exec_with_params ~label:"github_attribution_audit insert" db
            sql
            [
              Sqlite3.Data.TEXT record.id;
              Sqlite3.Data.TEXT (record_kind_to_string record.kind);
              Sqlite3.Data.INT (Int64.of_int record.schema_version);
              Sqlite3.Data.TEXT record.created_at;
              Sqlite3.Data.TEXT record.action;
              data_opt_text record.item_key;
              data_opt_text record.room_id;
              data_opt_text record.confirmation_id;
              data_opt_text record.job_id;
              data_opt_text record.plan_id;
              data_opt_text record.receipt_id;
              data_opt_text record.requested_mode;
              data_opt_text record.resolved_mode;
              Sqlite3.Data.INT (if record.used_app_fallback then 1L else 0L);
              data_opt_text record.fallback_reason;
              Sqlite3.Data.TEXT github_actor_json;
              data_opt_text record.lineage.principal_id;
              data_opt_int record.lineage.principal_revision;
              data_opt_text record.lineage.actor_identity_key;
              data_opt_int record.lineage.actor_revision;
              data_opt_int record.lineage.identity_link_revision;
              data_opt_text record.lineage.account_lineage_id;
              data_opt_text record.lineage.binding_id;
              data_opt_text snap_json;
              data_opt_text record.actor_snapshot_id;
              Sqlite3.Data.INT (Int64.of_int authority);
              Sqlite3.Data.TEXT (result_kind_to_string record.result);
              data_opt_text
                (Option.map failure_class_to_string record.failure_class);
              data_opt_text record.failure_code;
              Sqlite3.Data.TEXT record.reason;
              data_opt_text record.revisions_json;
            ];
          Ok record
        with
        | Failure msg -> Error msg
        | exn -> Error (Printexc.to_string exn))

let query_one ~db sql params =
  ensure_schema db;
  let stmt = Sqlite3.prepare db sql in
  List.iteri (fun i p -> ignore (Sqlite3.bind stmt (i + 1) p)) params;
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let r = row_of_stmt stmt in
      ignore (Sqlite3.finalize stmt);
      Some r
  | _ ->
      ignore (Sqlite3.finalize stmt);
      None

let query_many ~db sql params ~limit =
  ensure_schema db;
  let stmt = Sqlite3.prepare db sql in
  List.iteri (fun i p -> ignore (Sqlite3.bind stmt (i + 1) p)) params;
  let rec loop acc n =
    if n >= limit then List.rev acc
    else
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> loop (row_of_stmt stmt :: acc) (n + 1)
      | _ -> List.rev acc
  in
  let rows = loop [] 0 in
  ignore (Sqlite3.finalize stmt);
  rows

let get_by_id ~db ~id =
  query_one ~db
    (Printf.sprintf "SELECT %s FROM github_attribution_audit WHERE id = ?"
       select_columns)
    [ Sqlite3.Data.TEXT id ]

let list_by_kind ~db ~kind ?(limit = 100) () =
  query_many ~db
    (Printf.sprintf
       "SELECT %s FROM github_attribution_audit WHERE kind = ? ORDER BY \
        created_at DESC, id DESC"
       select_columns)
    [ Sqlite3.Data.TEXT (record_kind_to_string kind) ]
    ~limit

let list_by_action ~db ~action ?(limit = 100) () =
  query_many ~db
    (Printf.sprintf
       "SELECT %s FROM github_attribution_audit WHERE action = ? ORDER BY \
        created_at DESC, id DESC"
       select_columns)
    [ Sqlite3.Data.TEXT action ]
    ~limit

let list_by_snapshot_id ~db ~actor_snapshot_id ?(limit = 100) () =
  query_many ~db
    (Printf.sprintf
       "SELECT %s FROM github_attribution_audit WHERE actor_snapshot_id = ? \
        ORDER BY created_at DESC, id DESC"
       select_columns)
    [ Sqlite3.Data.TEXT actor_snapshot_id ]
    ~limit

let list_by_principal ~db ~principal_id ?(limit = 100) () =
  query_many ~db
    (Printf.sprintf
       "SELECT %s FROM github_attribution_audit WHERE principal_id = ? ORDER \
        BY created_at DESC, id DESC"
       select_columns)
    [ Sqlite3.Data.TEXT principal_id ]
    ~limit

let list_by_failure_class ~db ~failure_class ?(limit = 100) () =
  query_many ~db
    (Printf.sprintf
       "SELECT %s FROM github_attribution_audit WHERE failure_class = ? ORDER \
        BY created_at DESC, id DESC"
       select_columns)
    [ Sqlite3.Data.TEXT (failure_class_to_string failure_class) ]
    ~limit

let count ~db ?kind () =
  ensure_schema db;
  let sql, params =
    match kind with
    | None -> ("SELECT COUNT(*) FROM github_attribution_audit", [])
    | Some k ->
        ( "SELECT COUNT(*) FROM github_attribution_audit WHERE kind = ?",
          [ Sqlite3.Data.TEXT (record_kind_to_string k) ] )
  in
  let stmt = Sqlite3.prepare db sql in
  List.iteri (fun i p -> ignore (Sqlite3.bind stmt (i + 1) p)) params;
  let n =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.INT i -> Int64.to_int i
        | _ -> 0)
    | _ -> 0
  in
  ignore (Sqlite3.finalize stmt);
  n

let rewrite_actor_evidence ~db:_ ~id:_ ~snapshot:_ =
  Error
    "historical actor evidence is immutable; merge/split must not rewrite \
     attribution audit snapshots"

(* -------------------------------------------------------------------------- *)
(* Convenience recorders                                                       *)
(* -------------------------------------------------------------------------- *)

let record_with ~db ~kind ~action ~result ~reason ?item_key ?room_id
    ?confirmation_id ?job_id ?plan_id ?receipt_id ?requested_mode ?resolved_mode
    ?used_app_fallback ?fallback_reason ?github_actor ?lineage ?actor_snapshot
    ?actor_snapshot_id ?failure_class ?failure_code ?revisions_json
    ?(now = Unix.gettimeofday ()) () =
  match
    make ~now ~kind ~action ~result ~reason ?item_key ?room_id ?confirmation_id
      ?job_id ?plan_id ?receipt_id ?requested_mode ?resolved_mode
      ?used_app_fallback ?fallback_reason ?github_actor ?lineage ?actor_snapshot
      ?actor_snapshot_id ?failure_class ?failure_code ?revisions_json ()
  with
  | Error e -> Error e
  | Ok record -> insert ~db ~record ~now ()

let record_preview ~db ~action ~reason ~result ?item_key ?room_id
    ?confirmation_id ?job_id ?plan_id ?requested_mode ?resolved_mode
    ?used_app_fallback ?fallback_reason ?github_actor ?lineage ?actor_snapshot
    ?actor_snapshot_id ?failure_class ?failure_code ?revisions_json ?now () =
  record_with ~db ~kind:Preview ~action ~reason ~result ?item_key ?room_id
    ?confirmation_id ?job_id ?plan_id ?requested_mode ?resolved_mode
    ?used_app_fallback ?fallback_reason ?github_actor ?lineage ?actor_snapshot
    ?actor_snapshot_id ?failure_class ?failure_code ?revisions_json ?now ()

let record_receipt ~db ~action ~reason ~result ?item_key ?room_id
    ?confirmation_id ?job_id ?plan_id ?receipt_id ?requested_mode ?resolved_mode
    ?used_app_fallback ?fallback_reason ?github_actor ?lineage ?actor_snapshot
    ?actor_snapshot_id ?failure_class ?failure_code ?revisions_json ?now () =
  record_with ~db ~kind:Receipt ~action ~reason ~result ?item_key ?room_id
    ?confirmation_id ?job_id ?plan_id ?receipt_id ?requested_mode ?resolved_mode
    ?used_app_fallback ?fallback_reason ?github_actor ?lineage ?actor_snapshot
    ?actor_snapshot_id ?failure_class ?failure_code ?revisions_json ?now ()

let record_repair ~db ~action ~reason ~failure_class ~failure_code ?result
    ?item_key ?room_id ?confirmation_id ?job_id ?plan_id ?requested_mode
    ?resolved_mode ?used_app_fallback ?fallback_reason ?github_actor ?lineage
    ?actor_snapshot ?actor_snapshot_id ?revisions_json ?now () =
  let result =
    match result with
    | Some r -> r
    | None -> (
        match failure_class with
        | Fallback when failure_code = "post_confirm_authority_lost" ->
            Reconfirm
        | Fallback when failure_code = "app_fallback_not_previewed" -> Reconfirm
        | _ -> Pending_repair)
  in
  record_with ~db ~kind:Repair_state ~action ~reason ~result ?item_key ?room_id
    ?confirmation_id ?job_id ?plan_id ?requested_mode ?resolved_mode
    ?used_app_fallback ?fallback_reason ?github_actor ?lineage ?actor_snapshot
    ?actor_snapshot_id ~failure_class ~failure_code ?revisions_json ?now ()

let record_audit ~db ~action ~reason ~result ?item_key ?room_id ?confirmation_id
    ?job_id ?plan_id ?receipt_id ?requested_mode ?resolved_mode
    ?used_app_fallback ?fallback_reason ?github_actor ?lineage ?actor_snapshot
    ?actor_snapshot_id ?failure_class ?failure_code ?revisions_json ?now () =
  record_with ~db ~kind:Audit ~action ~reason ~result ?item_key ?room_id
    ?confirmation_id ?job_id ?plan_id ?receipt_id ?requested_mode ?resolved_mode
    ?used_app_fallback ?fallback_reason ?github_actor ?lineage ?actor_snapshot
    ?actor_snapshot_id ?failure_class ?failure_code ?revisions_json ?now ()

(* -------------------------------------------------------------------------- *)
(* Project from authorize / fallback / reconcile                               *)
(* -------------------------------------------------------------------------- *)

let of_authorize_decision ~decision ?(kind = Audit) ?item_key ?room_id ?job_id
    ?plan_id ?receipt_id ?actor_snapshot ?github_user_id
    ?(now = Unix.gettimeofday ()) () =
  match decision with
  | Auth.Allow a ->
      let mode = Auth.resolved_mode_to_string a.mode in
      let req_mode =
        Policy.attribution_to_string a.requirement.attribution |> normalize_mode
      in
      let result = if a.used_app_fallback then Fallback_app else Allowed in
      let reason =
        if a.used_app_fallback then
          Printf.sprintf
            "Authorized via visible App fallback for action %s (requested %s)"
            a.requirement.action req_mode
        else
          Printf.sprintf "Authorized mode=%s action=%s" mode
            a.requirement.action
      in
      let lineage = lineage_of_checked_revisions a.revisions in
      let github_actor =
        github_actor_of_revisions a.revisions
          ?binding_github_user_id:github_user_id ()
      in
      let kind = if kind = Audit then Audit else kind in
      make ~now ~kind ~action:a.requirement.action ~result ~reason ?item_key
        ?room_id ?confirmation_id:a.revisions.confirmation_id ?job_id ?plan_id
        ?receipt_id ~requested_mode:req_mode ~resolved_mode:mode
        ~used_app_fallback:a.used_app_fallback
        ?fallback_reason:
          (if a.used_app_fallback then
             Some "policy-permitted visible App fallback"
           else None)
        ~github_actor ~lineage ?actor_snapshot
        ?actor_snapshot_id:a.revisions.actor_snapshot_id
        ~revisions_json:
          (Yojson.Safe.to_string (Auth.checked_revisions_to_json a.revisions))
        ()
  | Auth.Deny d ->
      let action = d.revisions.policy_action in
      let code = d.repair.code in
      let failed = d.failed_check in
      let failure_class = classify_failure ~failed_check:failed ~code () in
      let kind = match kind with Audit -> Repair_state | other -> other in
      let result =
        match failure_class with
        | Fallback
          when code = "post_confirm_authority_lost"
               || code = "app_fallback_not_previewed" ->
            Reconfirm
        | _ -> Pending_repair
      in
      let req_mode =
        match d.requirement with
        | None -> None
        | Some req ->
            Some (normalize_mode (Policy.attribution_to_string req.attribution))
      in
      let lineage = lineage_of_checked_revisions d.revisions in
      let github_actor =
        github_actor_of_revisions d.revisions
          ?binding_github_user_id:github_user_id ()
      in
      make ~now ~kind ~action ~result ~reason:d.repair.message ?item_key
        ?room_id ?confirmation_id:d.revisions.confirmation_id ?job_id ?plan_id
        ?receipt_id ?requested_mode:req_mode ~github_actor ~lineage
        ?actor_snapshot ?actor_snapshot_id:d.revisions.actor_snapshot_id
        ~failure_class ~failure_code:code
        ~revisions_json:
          (Yojson.Safe.to_string (Auth.checked_revisions_to_json d.revisions))
        ()

let of_fallback_decision ~decision ~action ?(kind = Audit) ?item_key ?room_id
    ?confirmation_id ?job_id ?requested_mode ?actor_snapshot
    ?(lineage = empty_lineage_pin) ?(now = Unix.gettimeofday ()) () =
  match decision with
  | Fallback.Allow a ->
      let mode = Fallback.actor_mode_to_string a.mode in
      let req =
        normalize_mode (Policy.attribution_to_string a.requirement.attribution)
      in
      let result = if a.used_app_fallback then Fallback_app else Allowed in
      make ~now ~kind ~action ~result ~reason:a.reason ?item_key ?room_id
        ?confirmation_id ?job_id
        ~requested_mode:(Option.value requested_mode ~default:req)
        ~resolved_mode:mode ~used_app_fallback:a.used_app_fallback
        ?fallback_reason:(if a.used_app_fallback then Some a.reason else None)
        ~lineage ?actor_snapshot ()
  | Fallback.Deny d ->
      let failure_class =
        classify_failure ~code:d.code ~failed_check:"fallback" ()
      in
      let result =
        match d.kind with
        | Fallback.Reconfirmation -> Reconfirm
        | Fallback.Repair -> Pending_repair
      in
      let kind = match kind with Audit -> Repair_state | other -> other in
      let attempted =
        match d.attempted_mode with
        | None -> None
        | Some m -> Some (Fallback.actor_mode_to_string m)
      in
      make ~now ~kind ~action ~result ~reason:d.message ?item_key ?room_id
        ?confirmation_id ?job_id ?requested_mode ?resolved_mode:attempted
        ~failure_class ~failure_code:d.code ~lineage ?actor_snapshot ()

let of_correlation ~correlation ?(kind = Receipt) ?(result = Completed)
    ?(reason = "GitHub action correlation recorded") ?job_id
    ?(used_app_fallback = false) ?fallback_reason ?(now = Unix.gettimeofday ())
    () =
  let c = correlation in
  let lineage =
    match c.Reconcile.actor_snapshot with
    | Some s -> lineage_pin_of_snapshot s
    | None -> empty_lineage_pin
  in
  let github_actor =
    match c.actor_snapshot with
    | Some s -> (
        match s.account_binding with
        | Some b ->
            Numeric_user
              {
                host = b.identity.host;
                app_id = b.identity.app_id;
                github_user_id = b.identity.github_user_id;
              }
        | None -> Unspecified)
    | None -> (
        match String.lowercase_ascii (Reconcile.resolved_attribution c) with
        | "app" | "pilot" -> App { installation_id = None; app_id = None }
        | _ -> Unspecified)
  in
  make ~now ~kind ~action:c.action ~result ~reason ?item_key:c.item_key
    ~room_id:c.room_id ?job_id ?plan_id:c.plan_id ?receipt_id:c.receipt_id
    ?requested_mode:c.requested_mode
    ~resolved_mode:(Reconcile.resolved_attribution c)
    ~used_app_fallback ?fallback_reason ~github_actor ~lineage
    ?actor_snapshot:c.actor_snapshot ()

let record_authorize_decision ~db ~decision ?kind ?item_key ?room_id ?job_id
    ?plan_id ?receipt_id ?actor_snapshot ?github_user_id
    ?(now = Unix.gettimeofday ()) () =
  match
    of_authorize_decision ~decision ?kind ?item_key ?room_id ?job_id ?plan_id
      ?receipt_id ?actor_snapshot ?github_user_id ~now ()
  with
  | Error e -> Error e
  | Ok record -> insert ~db ~record ~now ()

let record_from_correlation ~db ~correlation ?kind ?result ?reason ?job_id
    ?used_app_fallback ?fallback_reason ?(now = Unix.gettimeofday ()) () =
  match
    of_correlation ~correlation ?kind ?result ?reason ?job_id ?used_app_fallback
      ?fallback_reason ~now ()
  with
  | Error e -> Error e
  | Ok record -> insert ~db ~record ~now ()
