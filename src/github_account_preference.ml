(* Repository- and Room-aware GitHub account preferences (P21.M1.E2.T003).
   See github_account_preference.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity
module B = Github_account_binding
module M = Principal_merge

let schema_version = 1
let key_prefix = "github.account."

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let contains_sub s sub =
  let n = String.length sub in
  let m = String.length s in
  let rec loop i =
    if i + n > m then false
    else if String.sub s i n = sub then true
    else loop (i + 1)
  in
  loop 0

let normalize_host h =
  let t = String.trim h in
  if t = "" then B.default_host else String.lowercase_ascii t

let normalize_login s = String.lowercase_ascii (String.trim s)

let normalize_repo_full_name s =
  let t = String.trim s in
  (* Keep owner/name case as provided for storage key stability after first
     normalize to lowercase (GitHub is case-insensitive for owner/repo). *)
  String.lowercase_ascii t

let org_of_repo_full_name full =
  match String.split_on_char '/' (String.trim full) with
  | owner :: _name :: _ when String.trim owner <> "" ->
      Some (normalize_login owner)
  | _ -> None

let opt_nonempty s =
  match s with
  | None -> None
  | Some x ->
      let t = String.trim x in
      if t = "" then None else Some t

(* -------------------------------------------------------------------------- *)
(* Scopes                                                                     *)
(* -------------------------------------------------------------------------- *)

type org_ref = { host : string; org_login : string }
type repo_ref = { host : string; repo_full_name : string }
type room_ref = string

type preference_scope =
  | Principal_default
  | Org of org_ref
  | Repo of repo_ref
  | Room of { room_id : room_ref; repo : repo_ref option; org : org_ref option }

let make_org_ref ?(host = B.default_host) ~org_login () =
  let host = normalize_host host in
  let org_login = normalize_login org_login in
  if org_login = "" then Error "org_login must be non-empty"
  else if contains_sub org_login "/" then Error "org_login must not contain '/'"
  else Ok { host; org_login }

let make_repo_ref ?(host = B.default_host) ~repo_full_name () =
  let host = normalize_host host in
  let repo_full_name = normalize_repo_full_name repo_full_name in
  if repo_full_name = "" then Error "repo_full_name must be non-empty"
  else
    match String.split_on_char '/' repo_full_name with
    | [ owner; name ] when String.trim owner <> "" && String.trim name <> "" ->
        Ok { host; repo_full_name }
    | _ ->
        Error
          "repo_full_name must be owner/name (exactly one '/', non-empty \
           segments)"

let make_room_scope ~room_id ?repo ?org () =
  let room_id = String.trim room_id in
  if room_id = "" then Error "room_id must be non-empty"
  else
    (* Prefer repo over org when both provided for the stored key shape. *)
    let repo, org =
      match (repo, org) with Some r, _ -> (Some r, None) | None, o -> (None, o)
    in
    Ok (Room { room_id; repo; org })

let encode_seg s =
  (* Prefer URL-ish safe segments so keys stay single-token. *)
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '@' ->
          Buffer.add_char b c
      | '/' -> Buffer.add_string b "%2F"
      | ':' -> Buffer.add_string b "%3A"
      | '%' -> Buffer.add_string b "%25"
      | c -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let decode_seg s =
  let len = String.length s in
  let b = Buffer.create len in
  let rec loop i =
    if i >= len then Ok (Buffer.contents b)
    else if s.[i] = '%' && i + 2 < len then (
      let hex = String.sub s (i + 1) 2 in
      match int_of_string_opt ("0x" ^ hex) with
      | None -> Error (Printf.sprintf "invalid percent-encoding in key: %s" s)
      | Some code ->
          Buffer.add_char b (Char.chr code);
          loop (i + 3))
    else (
      Buffer.add_char b s.[i];
      loop (i + 1))
  in
  loop 0

let preference_scope_key = function
  | Principal_default -> key_prefix ^ "default"
  | Org { host; org_login } ->
      Printf.sprintf "%sorg:%s:%s" key_prefix (encode_seg host)
        (encode_seg org_login)
  | Repo { host; repo_full_name } ->
      Printf.sprintf "%srepo:%s:%s" key_prefix (encode_seg host)
        (encode_seg repo_full_name)
  | Room { room_id; repo = Some r; _ } ->
      Printf.sprintf "%sroom_repo:%s:%s:%s" key_prefix (encode_seg room_id)
        (encode_seg r.host)
        (encode_seg r.repo_full_name)
  | Room { room_id; repo = None; org = Some o } ->
      Printf.sprintf "%sroom_org:%s:%s:%s" key_prefix (encode_seg room_id)
        (encode_seg o.host) (encode_seg o.org_login)
  | Room { room_id; repo = None; org = None } ->
      Printf.sprintf "%sroom:%s" key_prefix (encode_seg room_id)

let split_colon_segs s =
  (* Split on unencoded ':'. Our encode never leaves raw ':'. *)
  String.split_on_char ':' s

let preference_scope_of_key key =
  let key = String.trim key in
  if not (String.length key >= String.length key_prefix) then
    Error "not a github account preference key"
  else if not (String.sub key 0 (String.length key_prefix) = key_prefix) then
    Error "not a github account preference key"
  else
    let rest =
      String.sub key (String.length key_prefix)
        (String.length key - String.length key_prefix)
    in
    match rest with
    | "default" -> Ok Principal_default
    | _ -> (
        match split_colon_segs rest with
        | [ "org"; host_e; org_e ] -> (
            match (decode_seg host_e, decode_seg org_e) with
            | Ok host, Ok org_login ->
                make_org_ref ~host ~org_login () |> Result.map (fun o -> Org o)
            | Error e, _ | _, Error e -> Error e)
        | [ "repo"; host_e; repo_e ] -> (
            match (decode_seg host_e, decode_seg repo_e) with
            | Ok host, Ok repo_full_name ->
                make_repo_ref ~host ~repo_full_name ()
                |> Result.map (fun r -> Repo r)
            | Error e, _ | _, Error e -> Error e)
        | [ "room"; room_e ] -> (
            match decode_seg room_e with
            | Error e -> Error e
            | Ok room_id -> make_room_scope ~room_id ())
        | [ "room_repo"; room_e; host_e; repo_e ] -> (
            match (decode_seg room_e, decode_seg host_e, decode_seg repo_e) with
            | Ok room_id, Ok host, Ok repo_full_name -> (
                match make_repo_ref ~host ~repo_full_name () with
                | Error e -> Error e
                | Ok repo -> make_room_scope ~room_id ~repo ())
            | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
        | [ "room_org"; room_e; host_e; org_e ] -> (
            match (decode_seg room_e, decode_seg host_e, decode_seg org_e) with
            | Ok room_id, Ok host, Ok org_login -> (
                match make_org_ref ~host ~org_login () with
                | Error e -> Error e
                | Ok org -> make_room_scope ~room_id ~org ())
            | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
        | _ ->
            Error
              (Printf.sprintf "unknown github account preference key shape: %s"
                 key))

let string_of_preference_scope = function
  | Principal_default -> "principal_default"
  | Org { host; org_login } -> Printf.sprintf "org:%s:%s" host org_login
  | Repo { host; repo_full_name } ->
      Printf.sprintf "repo:%s:%s" host repo_full_name
  | Room { room_id; repo = Some r; _ } ->
      Printf.sprintf "room_repo:%s:%s:%s" room_id r.host r.repo_full_name
  | Room { room_id; repo = None; org = Some o } ->
      Printf.sprintf "room_org:%s:%s:%s" room_id o.host o.org_login
  | Room { room_id; repo = None; org = None } ->
      Printf.sprintf "room:%s" room_id

let preference_scope_rank = function
  | Room { repo = Some _; _ } -> 50
  | Room { repo = None; org = Some _; _ } -> 40
  | Room { repo = None; org = None; _ } -> 35
  | Repo _ -> 30
  | Org _ -> 20
  | Principal_default -> 10

let is_github_account_preference_key key =
  let key = String.trim key in
  let n = String.length key_prefix in
  String.length key >= n && String.sub key 0 n = key_prefix

(* -------------------------------------------------------------------------- *)
(* Preference value                                                           *)
(* -------------------------------------------------------------------------- *)

type preference_value = {
  binding_id : string option;
  lineage_id : string option;
}

let make_preference_value ?binding_id ?lineage_id () =
  let binding_id = opt_nonempty binding_id in
  let lineage_id = opt_nonempty lineage_id in
  match (binding_id, lineage_id) with
  | None, None -> Error "preference_value requires binding_id and/or lineage_id"
  | binding_id, lineage_id -> Ok { binding_id; lineage_id }

let preference_value_to_storage (v : preference_value) =
  let fields =
    List.filter_map
      (fun x -> x)
      [
        (match v.binding_id with
        | None -> None
        | Some id -> Some ("binding_id", `String id));
        (match v.lineage_id with
        | None -> None
        | Some id -> Some ("lineage_id", `String id));
        Some ("version", `Int schema_version);
      ]
  in
  Yojson.Safe.to_string (`Assoc fields)

let preference_value_of_storage s =
  let s = String.trim s in
  if s = "" then Error "empty preference value"
  else if
    (* Accept bare binding id for forward simplicity, or JSON object. *)
    s.[0] <> '{'
  then make_preference_value ~binding_id:s ()
  else
    try
      match Yojson.Safe.from_string s with
      | `Assoc fields ->
          let binding_id =
            match List.assoc_opt "binding_id" fields with
            | Some (`String id) -> Some id
            | Some `Null | None -> None
            | Some _ -> None
          in
          let lineage_id =
            match List.assoc_opt "lineage_id" fields with
            | Some (`String id) -> Some id
            | Some `Null | None -> None
            | Some _ -> None
          in
          make_preference_value ?binding_id ?lineage_id ()
      | `String id -> make_preference_value ~binding_id:id ()
      | _ -> Error "preference value must be a JSON object or string"
    with Yojson.Json_error e -> Error ("preference value json: " ^ e)

type stored_preference = {
  principal_id : P.principal_id;
  scope : preference_scope;
  value : preference_value;
  revision : int;
  updated_at : string;
}

(* -------------------------------------------------------------------------- *)
(* Schema / CRUD                                                              *)
(* -------------------------------------------------------------------------- *)

let ensure_schema db =
  M.ensure_schema db;
  B.ensure_schema db

let set_preference ~db ?now ~principal_id ~scope ~value ?revision () =
  let key = preference_scope_key scope in
  let storage = preference_value_to_storage value in
  match
    M.put_preference ~db ?now ~principal_id ~key ~value:storage ?revision ()
  with
  | Error e -> Error e
  | Ok p ->
      Ok
        {
          principal_id = p.principal_id;
          scope;
          value;
          revision = p.revision;
          updated_at = p.updated_at;
        }

let get_preference ~db ~principal_id ~scope =
  let key = preference_scope_key scope in
  match M.list_preferences ~db ~principal_id with
  | Error e -> Error e
  | Ok prefs -> (
      match List.find_opt (fun (p : M.preference) -> p.key = key) prefs with
      | None -> Ok None
      | Some p -> (
          match preference_value_of_storage p.value with
          | Error e -> Error e
          | Ok value ->
              Ok
                (Some
                   {
                     principal_id = p.principal_id;
                     scope;
                     value;
                     revision = p.revision;
                     updated_at = p.updated_at;
                   })))

let clear_preference ~db ~principal_id ~scope =
  let key = preference_scope_key scope in
  Principal_merge_persist.delete_preference ~db ~principal_id ~key

let list_preferences ~db ~principal_id =
  match M.list_preferences ~db ~principal_id with
  | Error e -> Error e
  | Ok prefs -> (
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | (p : M.preference) :: rest -> (
            if not (is_github_account_preference_key p.key) then go acc rest
            else
              match preference_scope_of_key p.key with
              | Error _ -> go acc rest (* skip non-parseable legacy *)
              | Ok scope -> (
                  match preference_value_of_storage p.value with
                  | Error e -> Error e
                  | Ok value ->
                      go
                        ({
                           principal_id = p.principal_id;
                           scope;
                           value;
                           revision = p.revision;
                           updated_at = p.updated_at;
                         }
                        :: acc)
                        rest))
      in
      match go [] prefs with
      | Error e -> Error e
      | Ok items ->
          Ok
            (List.sort
               (fun a b ->
                 match
                   compare
                     (preference_scope_rank b.scope)
                     (preference_scope_rank a.scope)
                 with
                 | 0 ->
                     String.compare
                       (preference_scope_key a.scope)
                       (preference_scope_key b.scope)
                 | n -> n)
               items))

(* -------------------------------------------------------------------------- *)
(* Resolve context                                                            *)
(* -------------------------------------------------------------------------- *)

type resolve_context = {
  principal_id : P.principal_id;
  host : string;
  app_id : int option;
  room_id : string option;
  repo_full_name : string option;
  org_login : string option;
  explicit_binding_id : string option;
  explicit_lineage_id : string option;
}

let make_resolve_context ~principal_id ?(host = B.default_host) ?app_id ?room_id
    ?repo_full_name ?org_login ?explicit_binding_id ?explicit_lineage_id () =
  {
    principal_id;
    host = normalize_host host;
    app_id;
    room_id = opt_nonempty room_id;
    repo_full_name =
      (match opt_nonempty repo_full_name with
      | None -> None
      | Some r -> Some (normalize_repo_full_name r));
    org_login =
      (match opt_nonempty org_login with
      | None -> None
      | Some o -> Some (normalize_login o));
    explicit_binding_id = opt_nonempty explicit_binding_id;
    explicit_lineage_id = opt_nonempty explicit_lineage_id;
  }

(* -------------------------------------------------------------------------- *)
(* Eligibility                                                                *)
(* -------------------------------------------------------------------------- *)

let binding_is_eligible ~(host : string) ~(app_id : int option) (b : B.binding)
    =
  match b.authorization_status with
  | B.Authorized ->
      let host_ok =
        String.equal (normalize_host b.identity.host) (normalize_host host)
      in
      let app_ok =
        match app_id with None -> true | Some aid -> b.identity.app_id = aid
      in
      host_ok && app_ok
  | B.Pending | B.Disabled | B.Revoked | B.Unlinked -> false

let list_eligible_bindings ~db ~principal_id ?(host = B.default_host) ?app_id ()
    =
  match B.list_for_principal ~db ~principal_id with
  | Error e -> Error e
  | Ok all ->
      let host = normalize_host host in
      let eligible =
        List.filter (binding_is_eligible ~host ~app_id) all
        |> List.sort (fun (a : B.binding) (b : B.binding) ->
            String.compare a.id b.id)
      in
      Ok eligible

type resolution_source =
  | Explicit_choice
  | From_room_repo
  | From_room_org
  | From_room_only
  | From_principal_repo
  | From_principal_org
  | From_principal_default
  | Sole_eligible

let string_of_resolution_source = function
  | Explicit_choice -> "explicit_choice"
  | From_room_repo -> "room_repo"
  | From_room_org -> "room_org"
  | From_room_only -> "room_only"
  | From_principal_repo -> "principal_repo"
  | From_principal_org -> "principal_org"
  | From_principal_default -> "principal_default"
  | Sole_eligible -> "sole_eligible"

type redacted_candidate = {
  binding_id : string;
  lineage_id : string;
  host : string;
  app_id : int;
  github_user_id : int64;
  login : string option;
  authorization_status : string;
}

let redacted_candidate_of_binding (b : B.binding) =
  {
    binding_id = b.id;
    lineage_id = b.lineage_id;
    host = b.identity.host;
    app_id = b.identity.app_id;
    github_user_id = b.identity.github_user_id;
    login = b.display.login;
    authorization_status =
      B.string_of_authorization_status b.authorization_status;
  }

type private_prompt = {
  principal_id : string;
  reason : string;
  host : string;
  app_id : int option;
  room_id : string option;
  repo_full_name : string option;
  org_login : string option;
  candidates : redacted_candidate list;
  examined_sources : string list;
}

type resolve_result =
  | Resolved of {
      binding : B.binding;
      source : resolution_source;
      matched_scope : preference_scope option;
    }
  | Ambiguous of { prompt : private_prompt }
  | None_eligible of { prompt : private_prompt }

let make_prompt ~(context : resolve_context) ~reason
    ~(candidates : B.binding list) ~examined_sources : private_prompt =
  let pid = P.principal_id_to_string context.principal_id in
  {
    principal_id = pid;
    reason;
    host = context.host;
    app_id = context.app_id;
    room_id = context.room_id;
    repo_full_name = context.repo_full_name;
    org_login = context.org_login;
    candidates = List.map redacted_candidate_of_binding candidates;
    examined_sources;
  }

(* -------------------------------------------------------------------------- *)
(* Preference → binding lookup                                                *)
(* -------------------------------------------------------------------------- *)

let find_binding_by_value ~eligible (value : preference_value) =
  (* Prefer lineage match (survives identity-preserving mutations); then
     binding_id. Never match on login/display. *)
  let by_lineage =
    match value.lineage_id with
    | None -> None
    | Some lin ->
        List.find_opt
          (fun (b : B.binding) -> String.equal b.lineage_id lin)
          eligible
  in
  match by_lineage with
  | Some _ as b -> b
  | None -> (
      match value.binding_id with
      | None -> None
      | Some id ->
          List.find_opt (fun (b : B.binding) -> String.equal b.id id) eligible)

let try_scope ~db ~principal_id ~scope ~eligible =
  match get_preference ~db ~principal_id ~scope with
  | Error e -> Error e
  | Ok None -> Ok None
  | Ok (Some stored) -> (
      match find_binding_by_value ~eligible stored.value with
      | None -> Ok None (* stale / foreign / ineligible — fall through *)
      | Some b -> Ok (Some (b, scope)))

let effective_org (ctx : resolve_context) =
  match ctx.org_login with
  | Some o -> Some o
  | None -> (
      match ctx.repo_full_name with
      | None -> None
      | Some full -> org_of_repo_full_name full)

let find_explicit ~eligible (ctx : resolve_context) =
  (* Explicit lineage first, then binding id. Still must be eligible + owned
     (eligible list already principal-scoped). *)
  let by_lineage =
    match ctx.explicit_lineage_id with
    | None -> None
    | Some lin ->
        List.find_opt
          (fun (b : B.binding) -> String.equal b.lineage_id lin)
          eligible
  in
  match by_lineage with
  | Some b -> Some b
  | None -> (
      match ctx.explicit_binding_id with
      | None -> None
      | Some id ->
          List.find_opt (fun (b : B.binding) -> String.equal b.id id) eligible)

(* -------------------------------------------------------------------------- *)
(* Resolve                                                                    *)
(* -------------------------------------------------------------------------- *)

let resolve_with_eligible ~db ~(context : resolve_context)
    ~(eligible : B.binding list) () =
  (* Bind fields once to avoid record-field disambiguation against
     private_prompt.principal_id : string in this module. *)
  let principal_id = context.principal_id in
  let host = context.host in
  let examined = ref [] in
  let note src = examined := string_of_resolution_source src :: !examined in
  (* 1. Explicit choice *)
  match find_explicit ~eligible context with
  | Some b ->
      Ok
        (Resolved
           { binding = b; source = Explicit_choice; matched_scope = None })
  | None ->
      let walk_scopes : (preference_scope * resolution_source) list =
        let scopes = ref [] in
        let add scope source = scopes := (scope, source) :: !scopes in
        (match (context.room_id, context.repo_full_name) with
        | Some room_id, Some repo_full_name -> (
            match make_repo_ref ~host ~repo_full_name () with
            | Error _ -> ()
            | Ok repo -> (
                match make_room_scope ~room_id ~repo () with
                | Ok scope -> add scope From_room_repo
                | Error _ -> ()))
        | _ -> ());
        (match (context.room_id, effective_org context) with
        | Some room_id, Some org_login -> (
            match make_org_ref ~host ~org_login () with
            | Error _ -> ()
            | Ok org -> (
                match make_room_scope ~room_id ~org () with
                | Ok scope -> add scope From_room_org
                | Error _ -> ()))
        | _ -> ());
        (match context.room_id with
        | Some room_id -> (
            match make_room_scope ~room_id () with
            | Ok scope -> add scope From_room_only
            | Error _ -> ())
        | None -> ());
        (match context.repo_full_name with
        | Some repo_full_name -> (
            match make_repo_ref ~host ~repo_full_name () with
            | Ok repo -> add (Repo repo) From_principal_repo
            | Error _ -> ())
        | None -> ());
        (match effective_org context with
        | Some org_login -> (
            match make_org_ref ~host ~org_login () with
            | Ok org -> add (Org org) From_principal_org
            | Error _ -> ())
        | None -> ());
        add Principal_default From_principal_default;
        List.rev !scopes
      in
      let rec walk = function
        | [] -> (
            (* Sole eligible, else private prompt. *)
            match eligible with
            | [ b ] ->
                note Sole_eligible;
                Ok
                  (Resolved
                     {
                       binding = b;
                       source = Sole_eligible;
                       matched_scope = None;
                     })
            | [] ->
                Ok
                  (None_eligible
                     {
                       prompt =
                         make_prompt ~context ~reason:"no_eligible_accounts"
                           ~candidates:[] ~examined_sources:(List.rev !examined);
                     })
            | many ->
                Ok
                  (Ambiguous
                     {
                       prompt =
                         make_prompt ~context
                           ~reason:"multiple_eligible_no_preference"
                           ~candidates:many
                           ~examined_sources:(List.rev !examined);
                     }))
        | (scope, source) :: rest -> (
            note source;
            match try_scope ~db ~principal_id ~scope ~eligible with
            | Error e -> Error e
            | Ok None -> walk rest
            | Ok (Some (b, matched_scope)) ->
                Ok
                  (Resolved
                     { binding = b; source; matched_scope = Some matched_scope })
            )
      in
      walk walk_scopes

let resolve ~db ~(context : resolve_context) () =
  let principal_id = context.principal_id in
  let host = context.host in
  let app_id = context.app_id in
  match list_eligible_bindings ~db ~principal_id ~host ?app_id () with
  | Error e -> Error e
  | Ok eligible -> resolve_with_eligible ~db ~context ~eligible ()

(* -------------------------------------------------------------------------- *)
(* JSON                                                                       *)
(* -------------------------------------------------------------------------- *)

let candidate_to_json (c : redacted_candidate) =
  `Assoc
    [
      ("binding_id", `String c.binding_id);
      ("lineage_id", `String c.lineage_id);
      ("host", `String c.host);
      ("app_id", `Int c.app_id);
      ("github_user_id", `String (Int64.to_string c.github_user_id));
      ("login", match c.login with None -> `Null | Some l -> `String l);
      ("authorization_status", `String c.authorization_status);
    ]

let private_prompt_to_json (p : private_prompt) =
  `Assoc
    [
      ("principal_id", `String p.principal_id);
      ("reason", `String p.reason);
      ("host", `String p.host);
      ("app_id", match p.app_id with None -> `Null | Some i -> `Int i);
      ("room_id", match p.room_id with None -> `Null | Some r -> `String r);
      ( "repo_full_name",
        match p.repo_full_name with None -> `Null | Some r -> `String r );
      ("org_login", match p.org_login with None -> `Null | Some o -> `String o);
      ("candidates", `List (List.map candidate_to_json p.candidates));
      ( "examined_sources",
        `List (List.map (fun s -> `String s) p.examined_sources) );
    ]

let resolve_result_to_json = function
  | Resolved { binding; source; matched_scope } ->
      `Assoc
        [
          ("kind", `String "resolved");
          ("source", `String (string_of_resolution_source source));
          ("binding_id", `String binding.id);
          ("lineage_id", `String binding.lineage_id);
          ( "matched_scope",
            match matched_scope with
            | None -> `Null
            | Some s -> `String (string_of_preference_scope s) );
          ( "candidate",
            candidate_to_json (redacted_candidate_of_binding binding) );
        ]
  | Ambiguous { prompt } ->
      `Assoc
        [
          ("kind", `String "ambiguous");
          ("prompt", private_prompt_to_json prompt);
        ]
  | None_eligible { prompt } ->
      `Assoc
        [
          ("kind", `String "none_eligible");
          ("prompt", private_prompt_to_json prompt);
        ]
