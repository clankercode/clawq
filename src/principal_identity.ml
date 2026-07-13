(** Versioned Principal, Connector actor, and Identity Link domain model
    (P21.M1.E1.T001).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md. *)

let schema_version = 1

(* -------------------------------------------------------------------------- *)
(* Opaque Principal ID                                                        *)
(* -------------------------------------------------------------------------- *)

type principal_id = string

let principal_id_of_string s =
  let t = String.trim s in
  if t = "" then Error "principal_id must be non-empty" else Ok t

let principal_id_to_string (id : principal_id) = id
let principal_id_equal (a : principal_id) (b : principal_id) = String.equal a b

let principal_id_compare (a : principal_id) (b : principal_id) =
  String.compare a b

(* -------------------------------------------------------------------------- *)
(* Connector + scoped immutable user identity                                 *)
(* -------------------------------------------------------------------------- *)

type connector = Teams | Slack | Discord | Telegram | Web | Cli | Direct

let string_of_connector = function
  | Teams -> "teams"
  | Slack -> "slack"
  | Discord -> "discord"
  | Telegram -> "telegram"
  | Web -> "web"
  | Cli -> "cli"
  | Direct -> "direct"

let connector_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "teams" -> Ok Teams
  | "slack" -> Ok Slack
  | "discord" -> Ok Discord
  | "telegram" -> Ok Telegram
  | "web" -> Ok Web
  | "cli" -> Ok Cli
  | "direct" -> Ok Direct
  | other -> Error (Printf.sprintf "unknown connector: %s" other)

type connector_scope = {
  tenant_or_workspace : string;
  immutable_user_id : string;
}

type connector_actor_key = { connector : connector; scope : connector_scope }

let actor_identity_key (k : connector_actor_key) =
  Printf.sprintf "connector:%s:tenant:%s:user:%s"
    (string_of_connector k.connector)
    k.scope.tenant_or_workspace k.scope.immutable_user_id

let connector_actor_key_equal a b =
  String.equal (actor_identity_key a) (actor_identity_key b)

let make_connector_actor_key ~connector ~tenant_or_workspace ~immutable_user_id
    =
  let tenant = String.trim tenant_or_workspace in
  let user = String.trim immutable_user_id in
  if tenant = "" then Error "tenant_or_workspace must be non-empty"
  else if user = "" then Error "immutable_user_id must be non-empty"
  else
    Ok
      {
        connector;
        scope = { tenant_or_workspace = tenant; immutable_user_id = user };
      }

(* -------------------------------------------------------------------------- *)
(* Mutable display metadata (non-identity)                                    *)
(* -------------------------------------------------------------------------- *)

type display_metadata = {
  display_name : string option;
  avatar_url : string option;
  email : string option;
  extra : (string * string) list;
}

let empty_display =
  { display_name = None; avatar_url = None; email = None; extra = [] }

(* -------------------------------------------------------------------------- *)
(* Explicit non-identity execution context                                    *)
(* -------------------------------------------------------------------------- *)

type non_identity_context = {
  room_id : string option;
  session_id : string option;
  display_name : string option;
}

let empty_non_identity_context =
  { room_id = None; session_id = None; display_name = None }

(* -------------------------------------------------------------------------- *)
(* Lifecycle                                                                  *)
(* -------------------------------------------------------------------------- *)

type principal_lifecycle = Active | Disabled | Merged_into of principal_id
type actor_lifecycle = Active | Unlinked | Disabled
type identity_link_status = Active | Unlinked | Superseded

let string_of_principal_lifecycle (l : principal_lifecycle) =
  match l with
  | Active -> "active"
  | Disabled -> "disabled"
  | Merged_into id -> "merged_into:" ^ principal_id_to_string id

let string_of_actor_lifecycle (l : actor_lifecycle) =
  match l with
  | Active -> "active"
  | Unlinked -> "unlinked"
  | Disabled -> "disabled"

let string_of_identity_link_status (s : identity_link_status) =
  match s with
  | Active -> "active"
  | Unlinked -> "unlinked"
  | Superseded -> "superseded"

(* -------------------------------------------------------------------------- *)
(* Records                                                                    *)
(* -------------------------------------------------------------------------- *)

type principal = {
  version : int;
  id : principal_id;
  lifecycle : principal_lifecycle;
  revision : int;
  display : display_metadata;
  created_at : string;
  updated_at : string;
}

type connector_actor = {
  version : int;
  key : connector_actor_key;
  principal_id : principal_id;
  lifecycle : actor_lifecycle;
  revision : int;
  display : display_metadata;
  verified_at : string option;
  created_at : string;
  updated_at : string;
}

type identity_link = {
  version : int;
  id : string;
  principal_id : principal_id;
  actor_key : connector_actor_key;
  status : identity_link_status;
  revision : int;
  linked_at : string;
  unlinked_at : string option;
}

let make_principal ~id ?(lifecycle = (Active : principal_lifecycle))
    ?(revision = 1) ?(display = empty_display) ?(created_at = "")
    ?(updated_at = "") () =
  {
    version = schema_version;
    id;
    lifecycle;
    revision;
    display;
    created_at;
    updated_at;
  }

let principal_is_active (p : principal) =
  match p.lifecycle with Active -> true | Disabled | Merged_into _ -> false

let with_principal_display (p : principal) display = { p with display }

let make_connector_actor ~key ~principal_id
    ?(lifecycle = (Active : actor_lifecycle)) ?(revision = 1)
    ?(display = empty_display) ?verified_at ?(created_at = "")
    ?(updated_at = "") () =
  {
    version = schema_version;
    key;
    principal_id;
    lifecycle;
    revision;
    display;
    verified_at;
    created_at;
    updated_at;
  }

let with_actor_display (a : connector_actor) display = { a with display }

let make_identity_link ~id ~principal_id ~actor_key
    ?(status = (Active : identity_link_status)) ?(revision = 1)
    ?(linked_at = "") ?unlinked_at () =
  {
    version = schema_version;
    id;
    principal_id;
    actor_key;
    status;
    revision;
    linked_at;
    unlinked_at;
  }

(* -------------------------------------------------------------------------- *)
(* JSON helpers                                                               *)
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

let string_pair_list_to_json (xs : (string * string) list) =
  `Assoc (List.map (fun (k, v) -> (k, `String v)) xs)

let string_pair_list_of_json = function
  | `Assoc items ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | (k, `String v) :: rest -> loop ((k, v) :: acc) rest
        | (k, _) :: _ -> Error (Printf.sprintf "extra[%s] must be a string" k)
      in
      loop [] items
  | `Null -> Ok []
  | _ -> Error "extra must be a JSON object of string values"

(* -------------------------------------------------------------------------- *)
(* Display metadata                                                           *)
(* -------------------------------------------------------------------------- *)

let display_metadata_to_json (d : display_metadata) : Yojson.Safe.t =
  `Assoc
    (opt_string_field "display_name" d.display_name
    @ opt_string_field "avatar_url" d.avatar_url
    @ opt_string_field "email" d.email
    @
    match d.extra with
    | [] -> []
    | xs -> [ ("extra", string_pair_list_to_json xs) ])

let display_metadata_of_json (json : Yojson.Safe.t) :
    (display_metadata, string) result =
  match json with
  | `Null -> Ok empty_display
  | `Assoc _ -> (
      let extra =
        match member_opt "extra" json with
        | None -> Ok []
        | Some v -> string_pair_list_of_json v
      in
      match extra with
      | Error e -> Error e
      | Ok extra ->
          Ok
            {
              display_name = get_string "display_name" json;
              avatar_url = get_string "avatar_url" json;
              email = get_string "email" json;
              extra;
            })
  | _ -> Error "display_metadata must be a JSON object"

(* -------------------------------------------------------------------------- *)
(* Connector actor key                                                        *)
(* -------------------------------------------------------------------------- *)

let connector_actor_key_to_json (k : connector_actor_key) : Yojson.Safe.t =
  `Assoc
    [
      ("connector", `String (string_of_connector k.connector));
      ("tenant_or_workspace", `String k.scope.tenant_or_workspace);
      ("immutable_user_id", `String k.scope.immutable_user_id);
    ]

let connector_actor_key_of_json (json : Yojson.Safe.t) :
    (connector_actor_key, string) result =
  match json with
  | `Assoc _ -> (
      match
        ( get_string_required "connector" json,
          get_string_required "tenant_or_workspace" json,
          get_string_required "immutable_user_id" json )
      with
      | Ok conn_s, Ok tenant, Ok user -> (
          match connector_of_string conn_s with
          | Error e -> Error e
          | Ok connector ->
              make_connector_actor_key ~connector ~tenant_or_workspace:tenant
                ~immutable_user_id:user)
      | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
  | _ -> Error "connector_actor_key must be a JSON object"

(* -------------------------------------------------------------------------- *)
(* Principal lifecycle                                                        *)
(* -------------------------------------------------------------------------- *)

let principal_lifecycle_to_json (l : principal_lifecycle) =
  match l with
  | Active -> `Assoc [ ("kind", `String "active") ]
  | Disabled -> `Assoc [ ("kind", `String "disabled") ]
  | Merged_into id ->
      `Assoc
        [
          ("kind", `String "merged_into");
          ("survivor_id", `String (principal_id_to_string id));
        ]

let principal_lifecycle_of_json json : (principal_lifecycle, string) result =
  match json with
  | `String "active" -> Ok Active
  | `String "disabled" -> Ok Disabled
  | `Assoc _ as json -> (
      match get_string "kind" json with
      | Some "active" -> Ok Active
      | Some "disabled" -> Ok Disabled
      | Some "merged_into" -> (
          match get_string_required "survivor_id" json with
          | Error e -> Error e
          | Ok sid -> (
              match principal_id_of_string sid with
              | Ok id -> Ok (Merged_into id)
              | Error e -> Error e))
      | Some other ->
          Error (Printf.sprintf "unknown principal_lifecycle kind: %s" other)
      | None -> Error "principal_lifecycle missing kind")
  | _ -> Error "principal_lifecycle must be object or string"

let actor_lifecycle_of_string s : (actor_lifecycle, string) result =
  match s with
  | "active" -> Ok Active
  | "unlinked" -> Ok Unlinked
  | "disabled" -> Ok Disabled
  | s -> Error (Printf.sprintf "unknown actor_lifecycle: %s" s)

let identity_link_status_of_string s : (identity_link_status, string) result =
  match s with
  | "active" -> Ok Active
  | "unlinked" -> Ok Unlinked
  | "superseded" -> Ok Superseded
  | s -> Error (Printf.sprintf "unknown identity_link_status: %s" s)

(* -------------------------------------------------------------------------- *)
(* Principal                                                                  *)
(* -------------------------------------------------------------------------- *)

let principal_to_json (p : principal) : Yojson.Safe.t =
  `Assoc
    [
      ("version", `Int p.version);
      ("id", `String (principal_id_to_string p.id));
      ("lifecycle", principal_lifecycle_to_json p.lifecycle);
      ("revision", `Int p.revision);
      ("display", display_metadata_to_json p.display);
      ("created_at", `String p.created_at);
      ("updated_at", `String p.updated_at);
    ]

let principal_of_json (json : Yojson.Safe.t) : (principal, string) result =
  match json with
  | `Assoc _ -> (
      match
        ( get_int_required "version" json,
          get_string_required "id" json,
          get_int_required "revision" json )
      with
      | Ok version, Ok id_s, Ok revision -> (
          match principal_id_of_string id_s with
          | Error e -> Error e
          | Ok id -> (
              let lifecycle : (principal_lifecycle, string) result =
                match member_opt "lifecycle" json with
                | None -> Ok Active
                | Some v -> principal_lifecycle_of_json v
              in
              let display =
                match member_opt "display" json with
                | None -> Ok empty_display
                | Some v -> display_metadata_of_json v
              in
              match (lifecycle, display) with
              | Error e, _ | _, Error e -> Error e
              | Ok lifecycle, Ok display ->
                  Ok
                    {
                      version;
                      id;
                      lifecycle;
                      revision;
                      display;
                      created_at =
                        Option.value (get_string "created_at" json) ~default:"";
                      updated_at =
                        Option.value (get_string "updated_at" json) ~default:"";
                    }))
      | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
  | _ -> Error "principal must be a JSON object"

(* -------------------------------------------------------------------------- *)
(* Connector actor                                                            *)
(* -------------------------------------------------------------------------- *)

let connector_actor_to_json (a : connector_actor) : Yojson.Safe.t =
  `Assoc
    ([
       ("version", `Int a.version);
       ("key", connector_actor_key_to_json a.key);
       ("principal_id", `String (principal_id_to_string a.principal_id));
       ("lifecycle", `String (string_of_actor_lifecycle a.lifecycle));
       ("revision", `Int a.revision);
       ("display", display_metadata_to_json a.display);
       ("created_at", `String a.created_at);
       ("updated_at", `String a.updated_at);
     ]
    @ opt_string_field "verified_at" a.verified_at)

let connector_actor_of_json (json : Yojson.Safe.t) :
    (connector_actor, string) result =
  match json with
  | `Assoc _ -> (
      match
        ( get_int_required "version" json,
          get_string_required "principal_id" json,
          get_int_required "revision" json )
      with
      | Ok version, Ok pid_s, Ok revision -> (
          match principal_id_of_string pid_s with
          | Error e -> Error e
          | Ok principal_id -> (
              let key =
                match member_opt "key" json with
                | None -> Error "missing field \"key\""
                | Some v -> connector_actor_key_of_json v
              in
              let lifecycle : (actor_lifecycle, string) result =
                match get_string "lifecycle" json with
                | None -> Ok Active
                | Some s -> actor_lifecycle_of_string s
              in
              let display =
                match member_opt "display" json with
                | None -> Ok empty_display
                | Some v -> display_metadata_of_json v
              in
              match (key, lifecycle, display) with
              | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
              | Ok key, Ok lifecycle, Ok display ->
                  Ok
                    {
                      version;
                      key;
                      principal_id;
                      lifecycle;
                      revision;
                      display;
                      verified_at = get_string "verified_at" json;
                      created_at =
                        Option.value (get_string "created_at" json) ~default:"";
                      updated_at =
                        Option.value (get_string "updated_at" json) ~default:"";
                    }))
      | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
  | _ -> Error "connector_actor must be a JSON object"

(* -------------------------------------------------------------------------- *)
(* Identity link                                                              *)
(* -------------------------------------------------------------------------- *)

let identity_link_to_json (l : identity_link) : Yojson.Safe.t =
  `Assoc
    ([
       ("version", `Int l.version);
       ("id", `String l.id);
       ("principal_id", `String (principal_id_to_string l.principal_id));
       ("actor_key", connector_actor_key_to_json l.actor_key);
       ("status", `String (string_of_identity_link_status l.status));
       ("revision", `Int l.revision);
       ("linked_at", `String l.linked_at);
     ]
    @ opt_string_field "unlinked_at" l.unlinked_at)

let identity_link_of_json (json : Yojson.Safe.t) :
    (identity_link, string) result =
  match json with
  | `Assoc _ -> (
      match
        ( get_int_required "version" json,
          get_string_required "id" json,
          get_string_required "principal_id" json,
          get_int_required "revision" json )
      with
      | Ok version, Ok id, Ok pid_s, Ok revision -> (
          match principal_id_of_string pid_s with
          | Error e -> Error e
          | Ok principal_id -> (
              let actor_key =
                match member_opt "actor_key" json with
                | None -> Error "missing field \"actor_key\""
                | Some v -> connector_actor_key_of_json v
              in
              let status : (identity_link_status, string) result =
                match get_string "status" json with
                | None -> Ok Active
                | Some s -> identity_link_status_of_string s
              in
              match (actor_key, status) with
              | Error e, _ | _, Error e -> Error e
              | Ok actor_key, Ok status ->
                  Ok
                    {
                      version;
                      id;
                      principal_id;
                      actor_key;
                      status;
                      revision;
                      linked_at =
                        Option.value (get_string "linked_at" json) ~default:"";
                      unlinked_at = get_string "unlinked_at" json;
                    }))
      | Error e, _, _, _
      | _, Error e, _, _
      | _, _, Error e, _
      | _, _, _, Error e ->
          Error e)
  | _ -> Error "identity_link must be a JSON object"

(* -------------------------------------------------------------------------- *)
(* Non-identity context                                                       *)
(* -------------------------------------------------------------------------- *)

let non_identity_context_to_json (c : non_identity_context) : Yojson.Safe.t =
  `Assoc
    (opt_string_field "room_id" c.room_id
    @ opt_string_field "session_id" c.session_id
    @ opt_string_field "display_name" c.display_name
    @ [ ("identity", `Bool false) ])

let non_identity_context_of_json (json : Yojson.Safe.t) :
    (non_identity_context, string) result =
  match json with
  | `Null -> Ok empty_non_identity_context
  | `Assoc _ ->
      Ok
        {
          room_id = get_string "room_id" json;
          session_id = get_string "session_id" json;
          display_name = get_string "display_name" json;
        }
  | _ -> Error "non_identity_context must be a JSON object"
