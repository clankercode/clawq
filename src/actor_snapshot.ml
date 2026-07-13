(* Immutable Actor snapshots for intents and delayed work (P21.M1.E3.T001).
   See actor_snapshot.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding

let schema_version = 1

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let opt_string_field key = function
  | None -> []
  | Some s -> [ (key, `String s) ]

let member_opt key json =
  match json with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key json with `Null -> None | v -> Some v)
  | _ -> None

let get_string key json =
  match member_opt key json with
  | Some (`String s) when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let get_string_required key json =
  match get_string key json with
  | Some s -> Ok s
  | None -> Error (Printf.sprintf "missing or empty field %S" key)

let get_int key json =
  match member_opt key json with
  | Some (`Int n) -> Some n
  | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
  | _ -> None

let get_int_required key json =
  match get_int key json with
  | Some n -> Ok n
  | None -> Error (Printf.sprintf "missing or non-int field %S" key)

let trim_nonempty s =
  let t = String.trim s in
  if t = "" then None else Some t

let contains_sub s sub =
  let n = String.length sub in
  let m = String.length s in
  let rec loop i =
    if i + n > m then false
    else if String.sub s i n = sub then true
    else loop (i + 1)
  in
  loop 0

(* -------------------------------------------------------------------------- *)
(* Source / work refs                                                         *)
(* -------------------------------------------------------------------------- *)

type source_context = {
  room_id : string option;
  session_id : string option;
  message_id : string option;
}

let empty_source_context =
  { room_id = None; session_id = None; message_id = None }

type work_refs = {
  intent_id : string option;
  confirmation_id : string option;
  delayed_job_id : string option;
}

let empty_work_refs =
  { intent_id = None; confirmation_id = None; delayed_job_id = None }

(* -------------------------------------------------------------------------- *)
(* Account binding evidence                                                   *)
(* -------------------------------------------------------------------------- *)

type account_binding_evidence = {
  binding_id : string;
  lineage_id : string;
  identity : B.account_identity;
  authorization_status : B.authorization_status;
}

let make_account_binding_evidence ~binding_id ~lineage_id ~identity
    ?(authorization_status = B.Authorized) () =
  match (trim_nonempty binding_id, trim_nonempty lineage_id) with
  | None, _ -> Error "binding_id must be non-empty"
  | _, None -> Error "lineage_id must be non-empty"
  | Some binding_id, Some lineage_id ->
      Ok { binding_id; lineage_id; identity; authorization_status }

let account_binding_evidence_of_binding (b : B.binding) =
  {
    binding_id = b.id;
    lineage_id = b.lineage_id;
    identity = b.identity;
    authorization_status = b.authorization_status;
  }

(* -------------------------------------------------------------------------- *)
(* Logical lineage + snapshot                                                 *)
(* -------------------------------------------------------------------------- *)

type logical_lineage = {
  principal_id : P.principal_id;
  principal_revision : int;
  actor_key : P.connector_actor_key;
  actor_revision : int;
  identity_link_id : string option;
  identity_link_revision : int;
  account_lineage_id : string option;
}

type t = {
  version : int;
  id : string;
  lineage : logical_lineage;
  display : P.display_metadata;
  source : source_context;
  account_binding : account_binding_evidence option;
  work_refs : work_refs;
  reason : string;
  captured_at : string;
}

let generate_id ?(now = Unix.gettimeofday ()) () =
  let ts_ms = Int64.of_float (now *. 1000.) in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "actorsnap_%Ld_%06d" ts_ms rand

let create ?id ?(now = Unix.gettimeofday ()) ?(reason = "intent_create")
    ~principal_id ?(principal_revision = 1) ~actor_key ?(actor_revision = 1)
    ?identity_link_id ?(identity_link_revision = 1) ?(display = P.empty_display)
    ?(source = empty_source_context) ?account_binding
    ?(work_refs = empty_work_refs) ?captured_at () =
  if principal_revision <= 0 then Error "principal_revision must be positive"
  else if actor_revision <= 0 then Error "actor_revision must be positive"
  else if identity_link_revision <= 0 then
    Error "identity_link_revision must be positive"
  else
    let reason =
      match trim_nonempty reason with None -> "intent_create" | Some r -> r
    in
    let id =
      match id with
      | Some s -> (
          match trim_nonempty s with None -> generate_id ~now () | Some s -> s)
      | None -> generate_id ~now ()
    in
    let captured_at =
      match captured_at with
      | Some s when String.trim s <> "" -> String.trim s
      | _ -> Time_util.iso8601_utc ~t:now ()
    in
    let identity_link_id =
      match identity_link_id with None -> None | Some s -> trim_nonempty s
    in
    let account_lineage_id =
      match account_binding with None -> None | Some ab -> Some ab.lineage_id
    in
    let lineage =
      {
        principal_id;
        principal_revision;
        actor_key;
        actor_revision;
        identity_link_id;
        identity_link_revision;
        account_lineage_id;
      }
    in
    Ok
      {
        version = schema_version;
        id;
        lineage;
        display;
        source;
        account_binding;
        work_refs;
        reason;
        captured_at;
      }

let is_authority (_ : t) = false

(* -------------------------------------------------------------------------- *)
(* Create from live store state                                               *)
(* -------------------------------------------------------------------------- *)

let create_from_live ~db ?id ?(now = Unix.gettimeofday ())
    ?(reason = "intent_create") ~actor_key ?account_binding_id
    ?(source = empty_source_context) ?(work_refs = empty_work_refs) ?display ()
    =
  match S.get_connector_actor ~db ~key:actor_key with
  | Error e -> Error e
  | Ok None ->
      Error
        (Printf.sprintf "connector actor not found: %s"
           (P.actor_identity_key actor_key))
  | Ok (Some actor) -> (
      match actor.lifecycle with
      | P.Disabled ->
          Error
            (Printf.sprintf "connector actor disabled: %s"
               (P.actor_identity_key actor_key))
      | P.Active | P.Unlinked -> (
          let link_res = S.get_active_identity_link ~db ~key:actor_key in
          match link_res with
          | Error e -> Error e
          | Ok link_opt -> (
              let principal_id, identity_link_id, identity_link_revision =
                match link_opt with
                | Some link -> (link.principal_id, Some link.id, link.revision)
                | None -> (actor.principal_id, None, 1)
              in
              match S.get_principal ~db ~id:principal_id with
              | Error e -> Error e
              | Ok None ->
                  Error
                    (Printf.sprintf "principal not found: %s"
                       (P.principal_id_to_string principal_id))
              | Ok (Some principal) -> (
                  let account_binding =
                    match account_binding_id with
                    | None -> Ok None
                    | Some bid -> (
                        match B.get ~db ~id:bid with
                        | Error e -> Error e
                        | Ok None ->
                            Error
                              (Printf.sprintf "account binding not found: %s"
                                 bid)
                        | Ok (Some b) ->
                            Ok (Some (account_binding_evidence_of_binding b)))
                  in
                  match account_binding with
                  | Error e -> Error e
                  | Ok account_binding ->
                      let display =
                        match display with Some d -> d | None -> actor.display
                      in
                      create ?id ~now ~reason ~principal_id
                        ~principal_revision:principal.revision ~actor_key
                        ~actor_revision:actor.revision ?identity_link_id
                        ~identity_link_revision ~display ~source
                        ?account_binding ~work_refs ()))))

(* -------------------------------------------------------------------------- *)
(* Token-material detection / JSON redaction                                  *)
(* -------------------------------------------------------------------------- *)

(* Exact keys or clear secret suffixes/fragments. Avoid matching non-secret
   fields like [authorization_status] (contains "authorization" as a word). *)
let secretish_exact_keys =
  [
    "token";
    "secret";
    "password";
    "passwd";
    "bearer";
    "authorization";
    "api_key";
    "apikey";
    "private_key";
    "client_secret";
    "access_token";
    "refresh_token";
    "id_token";
    "refresh";
    "vault_ref";
    "vault_cipher";
    "ciphertext";
    "sealed";
    "credential_blob";
    "raw_token";
    "user_token";
  ]

let secretish_key_suffixes =
  [
    "_token"; "_secret"; "_password"; "_passwd"; "_ciphertext"; "_key_material";
  ]

let key_looks_secret k =
  let lower = String.lowercase_ascii k in
  List.exists (String.equal lower) secretish_exact_keys
  || List.exists
       (fun suf ->
         let n = String.length suf in
         let m = String.length lower in
         m >= n && String.sub lower (m - n) n = suf)
       secretish_key_suffixes
  || contains_sub lower "access_token"
  || contains_sub lower "refresh_token"
  || contains_sub lower "client_secret"

let rec contains_token_material = function
  | `Assoc fields ->
      List.exists
        (fun (k, v) -> key_looks_secret k || contains_token_material v)
        fields
  | `List xs -> List.exists contains_token_material xs
  | _ -> false

let account_identity_to_json (i : B.account_identity) =
  `Assoc
    [
      ("host", `String i.host);
      ("app_id", `Int i.app_id);
      ("github_user_id", `String (Int64.to_string i.github_user_id));
    ]

let account_identity_of_json json : (B.account_identity, string) result =
  match json with
  | `Assoc _ -> (
      match
        ( get_string_required "host" json,
          get_int_required "app_id" json,
          get_string_required "github_user_id" json )
      with
      | Ok host, Ok app_id, Ok uid_s -> (
          match Int64.of_string_opt uid_s with
          | None -> Error "github_user_id must be an int64 string"
          | Some github_user_id ->
              B.make_account_identity ~host ~app_id ~github_user_id ())
      | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
  | _ -> Error "account identity must be a JSON object"

let account_binding_evidence_to_json (e : account_binding_evidence) =
  `Assoc
    [
      ("binding_id", `String e.binding_id);
      ("lineage_id", `String e.lineage_id);
      ("identity", account_identity_to_json e.identity);
      ( "authorization_status",
        `String (B.string_of_authorization_status e.authorization_status) );
    ]

let account_binding_evidence_of_json json :
    (account_binding_evidence, string) result =
  match json with
  | `Null -> Error "account_binding must be an object when present"
  | `Assoc _ -> (
      match
        ( get_string_required "binding_id" json,
          get_string_required "lineage_id" json,
          (match member_opt "identity" json with
          | None -> Error "missing field \"identity\""
          | Some v -> account_identity_of_json v),
          match get_string "authorization_status" json with
          | None -> Ok B.Authorized
          | Some s -> B.authorization_status_of_string s )
      with
      | Ok binding_id, Ok lineage_id, Ok identity, Ok authorization_status ->
          make_account_binding_evidence ~binding_id ~lineage_id ~identity
            ~authorization_status ()
      | Error e, _, _, _
      | _, Error e, _, _
      | _, _, Error e, _
      | _, _, _, Error e ->
          Error e)
  | _ -> Error "account_binding must be a JSON object"

let source_context_to_json (s : source_context) =
  `Assoc
    (opt_string_field "room_id" s.room_id
    @ opt_string_field "session_id" s.session_id
    @ opt_string_field "message_id" s.message_id
    @ [ ("identity", `Bool false) ])

let source_context_of_json json : (source_context, string) result =
  match json with
  | `Null -> Ok empty_source_context
  | `Assoc _ ->
      Ok
        {
          room_id = get_string "room_id" json;
          session_id = get_string "session_id" json;
          message_id = get_string "message_id" json;
        }
  | _ -> Error "source must be a JSON object"

let work_refs_to_json (w : work_refs) =
  `Assoc
    (opt_string_field "intent_id" w.intent_id
    @ opt_string_field "confirmation_id" w.confirmation_id
    @ opt_string_field "delayed_job_id" w.delayed_job_id)

let work_refs_of_json json : (work_refs, string) result =
  match json with
  | `Null -> Ok empty_work_refs
  | `Assoc _ ->
      Ok
        {
          intent_id = get_string "intent_id" json;
          confirmation_id = get_string "confirmation_id" json;
          delayed_job_id = get_string "delayed_job_id" json;
        }
  | _ -> Error "work_refs must be a JSON object"

let lineage_to_json (l : logical_lineage) =
  `Assoc
    ([
       ("principal_id", `String (P.principal_id_to_string l.principal_id));
       ("principal_revision", `Int l.principal_revision);
       ("actor_key", P.connector_actor_key_to_json l.actor_key);
       ("actor_identity_key", `String (P.actor_identity_key l.actor_key));
       ("actor_revision", `Int l.actor_revision);
       ("identity_link_revision", `Int l.identity_link_revision);
     ]
    @ opt_string_field "identity_link_id" l.identity_link_id
    @ opt_string_field "account_lineage_id" l.account_lineage_id)

let lineage_of_json json : (logical_lineage, string) result =
  match json with
  | `Assoc _ -> (
      match
        ( get_string_required "principal_id" json,
          get_int_required "principal_revision" json,
          get_int_required "actor_revision" json,
          get_int_required "identity_link_revision" json,
          match member_opt "actor_key" json with
          | None -> Error "missing field \"actor_key\""
          | Some v -> P.connector_actor_key_of_json v )
      with
      | ( Ok pid_s,
          Ok principal_revision,
          Ok actor_revision,
          Ok identity_link_revision,
          Ok actor_key ) -> (
          match P.principal_id_of_string pid_s with
          | Error e -> Error e
          | Ok principal_id ->
              Ok
                {
                  principal_id;
                  principal_revision;
                  actor_key;
                  actor_revision;
                  identity_link_id = get_string "identity_link_id" json;
                  identity_link_revision;
                  account_lineage_id = get_string "account_lineage_id" json;
                })
      | Error e, _, _, _, _
      | _, Error e, _, _, _
      | _, _, Error e, _, _
      | _, _, _, Error e, _
      | _, _, _, _, Error e ->
          Error e)
  | _ -> Error "lineage must be a JSON object"

let to_json (s : t) : Yojson.Safe.t =
  `Assoc
    [
      ("version", `Int s.version);
      ("id", `String s.id);
      ("lineage", lineage_to_json s.lineage);
      ("display", P.display_metadata_to_json s.display);
      ("source", source_context_to_json s.source);
      ( "account_binding",
        match s.account_binding with
        | None -> `Null
        | Some e -> account_binding_evidence_to_json e );
      ("work_refs", work_refs_to_json s.work_refs);
      ("reason", `String s.reason);
      ("captured_at", `String s.captured_at);
      ("authority", `Bool false);
      ("reusable_authority", `Bool false);
    ]

let of_json (json : Yojson.Safe.t) : (t, string) result =
  if contains_token_material json then
    Error "actor_snapshot JSON must not contain token or secret material"
  else
    match json with
    | `Assoc _ -> (
        match
          ( get_int_required "version" json,
            get_string_required "id" json,
            get_string_required "reason" json,
            match member_opt "lineage" json with
            | None -> Error "missing field \"lineage\""
            | Some v -> lineage_of_json v )
        with
        | Ok version, Ok id, Ok reason, Ok lineage -> (
            let display =
              match member_opt "display" json with
              | None -> Ok P.empty_display
              | Some v -> P.display_metadata_of_json v
            in
            let source =
              match member_opt "source" json with
              | None -> Ok empty_source_context
              | Some v -> source_context_of_json v
            in
            let work_refs =
              match member_opt "work_refs" json with
              | None -> Ok empty_work_refs
              | Some v -> work_refs_of_json v
            in
            let account_binding =
              match member_opt "account_binding" json with
              | None | Some `Null -> Ok None
              | Some v -> (
                  match account_binding_evidence_of_json v with
                  | Ok e -> Ok (Some e)
                  | Error e -> Error e)
            in
            match (display, source, work_refs, account_binding) with
            | Error e, _, _, _
            | _, Error e, _, _
            | _, _, Error e, _
            | _, _, _, Error e ->
                Error e
            | Ok display, Ok source, Ok work_refs, Ok account_binding ->
                Ok
                  {
                    version;
                    id;
                    lineage;
                    display;
                    source;
                    account_binding;
                    work_refs;
                    reason;
                    captured_at =
                      Option.value (get_string "captured_at" json) ~default:"";
                  })
        | Error e, _, _, _
        | _, Error e, _, _
        | _, _, Error e, _
        | _, _, _, Error e ->
            Error e)
    | _ -> Error "actor_snapshot must be a JSON object"

let to_redacted_json (s : t) : Yojson.Safe.t =
  (* Strip contact-adjacent display fields for logs/audit while keeping
     identity lineage intact. *)
  let display = { s.display with email = None; extra = [] } in
  match to_json { s with display } with
  | `Assoc fields -> `Assoc (fields @ [ ("redacted", `Bool true) ])
  | other -> other

let redacted_summary (s : t) =
  let ab =
    match s.account_binding with
    | None -> "account:none"
    | Some e -> Printf.sprintf "account:%s/lineage:%s" e.binding_id e.lineage_id
  in
  Printf.sprintf
    "actorsnap id=%s principal=%s actor=%s link_rev=%d %s reason=%s \
     authority=false"
    s.id
    (P.principal_id_to_string s.lineage.principal_id)
    (P.actor_identity_key s.lineage.actor_key)
    s.lineage.identity_link_revision ab s.reason

(* -------------------------------------------------------------------------- *)
(* Re-resolve current authority                                               *)
(* -------------------------------------------------------------------------- *)

type authority_break =
  | Actor_missing
  | Actor_disabled
  | Actor_unlinked
  | Identity_link_missing
  | Identity_link_inactive of { status : P.identity_link_status }
  | Identity_link_revision_changed of { expected : int; actual : int }
  | Principal_missing
  | Principal_disabled
  | Principal_changed of {
      snapshot_principal : P.principal_id;
      live_principal : P.principal_id;
    }
  | Account_binding_missing
  | Account_lineage_changed of { expected : string; actual : string }
  | Account_not_authorized of { status : B.authorization_status }
  | Account_owner_mismatch of { owner : P.principal_id }

type current_authority = {
  live_principal_id : P.principal_id option;
  live_principal_revision : int option;
  live_actor_revision : int option;
  live_identity_link_id : string option;
  live_identity_link_revision : int option;
  live_account_binding : B.binding option;
  followed_merge_alias : bool;
  breaks : authority_break list;
  usable : bool;
}

let string_of_authority_break = function
  | Actor_missing -> "actor_missing"
  | Actor_disabled -> "actor_disabled"
  | Actor_unlinked -> "actor_unlinked"
  | Identity_link_missing -> "identity_link_missing"
  | Identity_link_inactive { status } ->
      "identity_link_inactive:" ^ P.string_of_identity_link_status status
  | Identity_link_revision_changed { expected; actual } ->
      Printf.sprintf "identity_link_revision_changed:expected=%d:actual=%d"
        expected actual
  | Principal_missing -> "principal_missing"
  | Principal_disabled -> "principal_disabled"
  | Principal_changed { snapshot_principal; live_principal } ->
      Printf.sprintf "principal_changed:snapshot=%s:live=%s"
        (P.principal_id_to_string snapshot_principal)
        (P.principal_id_to_string live_principal)
  | Account_binding_missing -> "account_binding_missing"
  | Account_lineage_changed { expected; actual } ->
      Printf.sprintf "account_lineage_changed:expected=%s:actual=%s" expected
        actual
  | Account_not_authorized { status } ->
      "account_not_authorized:" ^ B.string_of_authorization_status status
  | Account_owner_mismatch { owner } ->
      "account_owner_mismatch:" ^ P.principal_id_to_string owner

(** Follow [Merged_into] aliases cycle-safely. Returns [(root, followed)]. *)
let rec follow_merge_alias ~db ~seen (id : P.principal_id) =
  let id_s = P.principal_id_to_string id in
  if List.exists (String.equal id_s) seen then
    Error (Printf.sprintf "principal merge alias cycle involving %s" id_s)
  else
    match S.get_principal ~db ~id with
    | Error e -> Error e
    | Ok None -> Ok (id, false)
    | Ok (Some p) -> (
        match p.lifecycle with
        | P.Merged_into target -> (
            match follow_merge_alias ~db ~seen:(id_s :: seen) target with
            | Error e -> Error e
            | Ok (root, _) -> Ok (root, true))
        | P.Active | P.Disabled -> Ok (id, false))

let empty_authority =
  {
    live_principal_id = None;
    live_principal_revision = None;
    live_actor_revision = None;
    live_identity_link_id = None;
    live_identity_link_revision = None;
    live_account_binding = None;
    followed_merge_alias = false;
    breaks = [];
    usable = false;
  }

let principal_revision_status ~db ~add live_pid =
  match S.get_principal ~db ~id:live_pid with
  | Error _ | Ok None ->
      add Principal_missing;
      None
  | Ok (Some p) -> (
      match p.lifecycle with
      | P.Active -> Some p.revision
      | P.Disabled ->
          add Principal_disabled;
          Some p.revision
      | P.Merged_into _ ->
          add Principal_missing;
          Some p.revision)

let check_account_binding ~db ~add ~live_pid snap =
  match snap.account_binding with
  | None -> None
  | Some ev -> (
      match B.get ~db ~id:ev.binding_id with
      | Error _ | Ok None ->
          add Account_binding_missing;
          None
      | Ok (Some b) ->
          if not (String.equal b.lineage_id ev.lineage_id) then
            add
              (Account_lineage_changed
                 { expected = ev.lineage_id; actual = b.lineage_id });
          (match b.authorization_status with
          | B.Authorized -> ()
          | st -> add (Account_not_authorized { status = st }));
          (match follow_merge_alias ~db ~seen:[] b.principal_id with
          | Error _ -> add Account_binding_missing
          | Ok (owner_root, _) ->
              if not (P.principal_id_equal owner_root live_pid) then
                add (Account_owner_mismatch { owner = b.principal_id }));
          Some b)

let finalize_authority ~(actor_lifecycle : P.actor_lifecycle)
    ~live_principal_revision ~breaks ~live_principal_id ~live_actor_revision
    ~live_identity_link_id ~live_identity_link_revision ~live_account_binding
    ~followed_merge_alias =
  (* Same-link revision bumps alone (e.g. CAS noise) are soft when no hard
     break remains; merge/split hard breaks stay. *)
  let has_hard =
    List.exists
      (function Identity_link_revision_changed _ -> false | _ -> true)
      breaks
  in
  let breaks =
    if has_hard then breaks
    else
      List.filter
        (function Identity_link_revision_changed _ -> false | _ -> true)
        breaks
  in
  let usable =
    breaks = []
    &&
    match (actor_lifecycle, live_principal_revision) with
    | P.Active, Some _ -> true
    | (P.Unlinked | P.Disabled), _ | _, None -> false
  in
  {
    live_principal_id;
    live_principal_revision;
    live_actor_revision;
    live_identity_link_id;
    live_identity_link_revision;
    live_account_binding;
    followed_merge_alias;
    breaks;
    usable;
  }

let re_resolve_current_authority ~db (snap : t) =
  let breaks = ref [] in
  let add b = breaks := b :: !breaks in
  let actor_key = snap.lineage.actor_key in
  match S.get_connector_actor ~db ~key:actor_key with
  | Error e -> Error e
  | Ok None ->
      Ok { empty_authority with breaks = [ Actor_missing ]; usable = false }
  | Ok (Some actor) -> (
      let live_actor_revision = Some actor.revision in
      (match actor.lifecycle with
      | P.Disabled -> add Actor_disabled
      | P.Unlinked -> add Actor_unlinked
      | P.Active -> ());
      match S.get_active_identity_link ~db ~key:actor_key with
      | Error e -> Error e
      | Ok None -> (
          (match snap.lineage.identity_link_id with
          | None -> add Identity_link_missing
          | Some lid -> (
              match S.get_identity_link ~db ~id:lid with
              | Error _ | Ok None -> add Identity_link_missing
              | Ok (Some link) ->
                  if link.status <> P.Active then
                    add (Identity_link_inactive { status = link.status })
                  else add Identity_link_missing));
          match follow_merge_alias ~db ~seen:[] actor.principal_id with
          | Error e -> Error e
          | Ok (live_pid, followed) -> (
              let live_principal_revision =
                principal_revision_status ~db ~add live_pid
              in
              match
                follow_merge_alias ~db ~seen:[] snap.lineage.principal_id
              with
              | Error e -> Error e
              | Ok (snap_root, _) ->
                  if not (P.principal_id_equal snap_root live_pid) then
                    add
                      (Principal_changed
                         {
                           snapshot_principal = snap.lineage.principal_id;
                           live_principal = live_pid;
                         });
                  let live_account_binding =
                    check_account_binding ~db ~add ~live_pid snap
                  in
                  Ok
                    (finalize_authority ~actor_lifecycle:actor.lifecycle
                       ~live_principal_revision ~breaks:(List.rev !breaks)
                       ~live_principal_id:(Some live_pid) ~live_actor_revision
                       ~live_identity_link_id:None
                       ~live_identity_link_revision:None ~live_account_binding
                       ~followed_merge_alias:followed)))
      | Ok (Some link) -> (
          let live_identity_link_id = Some link.id in
          let live_identity_link_revision = Some link.revision in
          if link.status <> P.Active then
            add (Identity_link_inactive { status = link.status });
          (match snap.lineage.identity_link_id with
          | Some sid when String.equal sid link.id ->
              if link.revision <> snap.lineage.identity_link_revision then
                add
                  (Identity_link_revision_changed
                     {
                       expected = snap.lineage.identity_link_revision;
                       actual = link.revision;
                     })
          | _ -> ());
          match follow_merge_alias ~db ~seen:[] link.principal_id with
          | Error e -> Error e
          | Ok (live_pid, followed_from_link) -> (
              match
                follow_merge_alias ~db ~seen:[] snap.lineage.principal_id
              with
              | Error e -> Error e
              | Ok (snap_root, followed_from_snap) ->
                  let followed_merge_alias =
                    followed_from_link || followed_from_snap
                    || (not
                          (P.principal_id_equal snap.lineage.principal_id
                             live_pid))
                       && P.principal_id_equal snap_root live_pid
                  in
                  if not (P.principal_id_equal snap_root live_pid) then
                    add
                      (Principal_changed
                         {
                           snapshot_principal = snap.lineage.principal_id;
                           live_principal = live_pid;
                         });
                  let live_principal_revision =
                    principal_revision_status ~db ~add live_pid
                  in
                  let live_account_binding =
                    check_account_binding ~db ~add ~live_pid snap
                  in
                  Ok
                    (finalize_authority ~actor_lifecycle:actor.lifecycle
                       ~live_principal_revision ~breaks:(List.rev !breaks)
                       ~live_principal_id:(Some live_pid) ~live_actor_revision
                       ~live_identity_link_id ~live_identity_link_revision
                       ~live_account_binding ~followed_merge_alias))))
