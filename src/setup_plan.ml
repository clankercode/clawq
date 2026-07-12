(* Reusable typed admin setup plans.

   Planning produces values only — no config, database, Connector, or Session
   mutation. See setup_plan.mli and ADR 0003. *)

type plan_id = string
type principal_kind = Principal | Channel_actor | Cli | System
type principal = { id : string; kind : principal_kind; label : string option }

type context = {
  room_id : string option;
  session_key : string option;
  connector : string option;
  profile_id : string option;
  extra : (string * Yojson.Safe.t) list;
}

type readiness_status = Pass | Fail | Warn

type readiness_item = {
  name : string;
  status : readiness_status;
  message : string;
}

type warning = { code : string; message : string }

type diff_op =
  | Create of { path : string; value : Yojson.Safe.t }
  | Update of { path : string; from_ : Yojson.Safe.t; to_ : Yojson.Safe.t }
  | Delete of { path : string; old : Yojson.Safe.t }
  | Bind of { path : string; target : string; active : bool }
  | Note of { path : string; message : string }

type apply_kind =
  | Room_profile
  | Github_app_setup
  | Github_route
  | Access_bundle
  | Generic of string

type apply_payload = {
  kind : apply_kind;
  ops : Yojson.Safe.t;
  data : Yojson.Safe.t;
}

type t = {
  id : plan_id;
  principal : principal;
  source : context;
  destination : context;
  current_state : Yojson.Safe.t;
  planned_state : Yojson.Safe.t;
  diff : diff_op list;
  readiness : readiness_item list;
  warnings : warning list;
  base_revision : string;
  created_at : string;
  expires_at : string;
  digest : string;
  apply_payload : apply_payload;
}

let default_ttl_seconds = 900.0

let generate_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "plan_%d_%06d" ts rand

let base_revision_of_config (cfg : Runtime_config.t) =
  Access_snapshot.config_hash cfg

let digests_equal a b = Eqaf.equal a b

(* ── JSON helpers ───────────────────────────────────────────────── *)

let rec sort_json_keys (j : Yojson.Safe.t) : Yojson.Safe.t =
  match j with
  | `Assoc fields ->
      let sorted =
        List.sort (fun (a, _) (b, _) -> String.compare a b) fields
        |> List.map (fun (k, v) -> (k, sort_json_keys v))
      in
      `Assoc sorted
  | `List items -> `List (List.map sort_json_keys items)
  | other -> other

let digest_hex s = Digestif.SHA256.(digest_string s |> to_hex)

let opt_string_field name = function
  | None -> []
  | Some s -> [ (name, `String s) ]

let principal_kind_to_string = function
  | Principal -> "principal"
  | Channel_actor -> "channel_actor"
  | Cli -> "cli"
  | System -> "system"

let principal_kind_of_string = function
  | "principal" -> Ok Principal
  | "channel_actor" -> Ok Channel_actor
  | "cli" -> Ok Cli
  | "system" -> Ok System
  | s -> Error (Printf.sprintf "unknown principal kind: %s" s)

let readiness_status_to_string = function
  | Pass -> "pass"
  | Fail -> "fail"
  | Warn -> "warn"

let readiness_status_of_string = function
  | "pass" -> Ok Pass
  | "fail" -> Ok Fail
  | "warn" -> Ok Warn
  | s -> Error (Printf.sprintf "unknown readiness status: %s" s)

let apply_kind_to_json = function
  | Room_profile -> `String "room_profile"
  | Github_app_setup -> `String "github_app_setup"
  | Github_route -> `String "github_route"
  | Access_bundle -> `String "access_bundle"
  | Generic s -> `Assoc [ ("generic", `String s) ]

let apply_kind_of_json = function
  | `String "room_profile" -> Ok Room_profile
  | `String "github_app_setup" -> Ok Github_app_setup
  | `String "github_route" -> Ok Github_route
  | `String "access_bundle" -> Ok Access_bundle
  | `Assoc [ ("generic", `String s) ] -> Ok (Generic s)
  | _ -> Error "invalid apply_kind"

let principal_to_json (p : principal) : Yojson.Safe.t =
  `Assoc
    ([
       ("id", `String p.id); ("kind", `String (principal_kind_to_string p.kind));
     ]
    @ match p.label with None -> [] | Some l -> [ ("label", `String l) ])

let context_to_json (c : context) : Yojson.Safe.t =
  let extra =
    match c.extra with [] -> [] | fields -> [ ("extra", `Assoc fields) ]
  in
  `Assoc
    (opt_string_field "room_id" c.room_id
    @ opt_string_field "session_key" c.session_key
    @ opt_string_field "connector" c.connector
    @ opt_string_field "profile_id" c.profile_id
    @ extra)

let readiness_item_to_json (r : readiness_item) : Yojson.Safe.t =
  `Assoc
    [
      ("name", `String r.name);
      ("status", `String (readiness_status_to_string r.status));
      ("message", `String r.message);
    ]

let warning_to_json (w : warning) : Yojson.Safe.t =
  `Assoc [ ("code", `String w.code); ("message", `String w.message) ]

let diff_op_to_json = function
  | Create { path; value } ->
      `Assoc
        [ ("op", `String "create"); ("path", `String path); ("value", value) ]
  | Update { path; from_; to_ } ->
      `Assoc
        [
          ("op", `String "update");
          ("path", `String path);
          ("from", from_);
          ("to", to_);
        ]
  | Delete { path; old } ->
      `Assoc [ ("op", `String "delete"); ("path", `String path); ("old", old) ]
  | Bind { path; target; active } ->
      `Assoc
        [
          ("op", `String "bind");
          ("path", `String path);
          ("target", `String target);
          ("active", `Bool active);
        ]
  | Note { path; message } ->
      `Assoc
        [
          ("op", `String "note");
          ("path", `String path);
          ("message", `String message);
        ]

let apply_payload_to_json (p : apply_payload) : Yojson.Safe.t =
  `Assoc
    [ ("kind", apply_kind_to_json p.kind); ("ops", p.ops); ("data", p.data) ]

let body_fields ~(include_digest : bool) (plan : t) :
    (string * Yojson.Safe.t) list =
  let base =
    [
      ("id", `String plan.id);
      ("principal", principal_to_json plan.principal);
      ("source", context_to_json plan.source);
      ("destination", context_to_json plan.destination);
      ("current_state", plan.current_state);
      ("planned_state", plan.planned_state);
      ("diff", `List (List.map diff_op_to_json plan.diff));
      ("readiness", `List (List.map readiness_item_to_json plan.readiness));
      ("warnings", `List (List.map warning_to_json plan.warnings));
      ("base_revision", `String plan.base_revision);
      ("created_at", `String plan.created_at);
      ("expires_at", `String plan.expires_at);
      ("apply_payload", apply_payload_to_json plan.apply_payload);
    ]
  in
  if include_digest then base @ [ ("digest", `String plan.digest) ] else base

let to_canonical_json (plan : t) : Yojson.Safe.t =
  sort_json_keys (`Assoc (body_fields ~include_digest:false plan))

let compute_digest (plan : t) : string =
  let canonical = to_canonical_json plan in
  digest_hex (Yojson.Safe.to_string canonical)

(** Last segment of a dotted/slashed path (e.g. [channels.slack.bot_token]). *)
let path_leaf path =
  let parts =
    path |> String.split_on_char '.'
    |> List.concat_map (String.split_on_char '/')
    |> List.filter (fun s -> s <> "")
  in
  match List.rev parts with [] -> path | leaf :: _ -> leaf

(** Redact a scalar value when [path] ends in a sensitive key name. Nested
    objects still go through [Config_show.redact_json]. *)
let redact_value_for_path path (value : Yojson.Safe.t) : Yojson.Safe.t =
  let leaf = path_leaf path in
  match value with
  | `String s when String.length s > 0 && Config_show.is_secret_key leaf ->
      `String "***"
  | other -> Config_show.redact_json other

let redact_diff_op = function
  | Create { path; value } ->
      Create { path; value = redact_value_for_path path value }
  | Update { path; from_; to_ } ->
      Update
        {
          path;
          from_ = redact_value_for_path path from_;
          to_ = redact_value_for_path path to_;
        }
  | Delete { path; old } ->
      Delete { path; old = redact_value_for_path path old }
  | Bind b -> Bind b
  | Note n -> Note n

(** Walk free-form apply JSON and redact string [value]/[from]/[to]/[old] fields
    when a sibling [path]/[field]/[key] names a secret. *)
let rec redact_path_oriented_json (j : Yojson.Safe.t) : Yojson.Safe.t =
  match j with
  | `Assoc fields ->
      let path_hint =
        List.find_map
          (fun (k, v) ->
            match (k, v) with
            | ("path" | "field" | "key"), `String s -> Some s
            | _ -> None)
          fields
      in
      let fields =
        List.map
          (fun (k, v) ->
            match (k, path_hint, v) with
            | ("value" | "from" | "to" | "old"), Some path, `String _
              when Config_show.is_secret_key (path_leaf path) ->
                (k, `String "***")
            | _ -> (k, redact_path_oriented_json v))
          fields
      in
      (* Also apply key-based Config_show redaction. *)
      Config_show.redact_json (`Assoc fields)
  | `List items -> `List (List.map redact_path_oriented_json items)
  | other -> other

let redact (plan : t) : t =
  let redacted =
    {
      plan with
      current_state = Config_show.redact_json plan.current_state;
      planned_state = Config_show.redact_json plan.planned_state;
      diff = List.map redact_diff_op plan.diff;
      apply_payload =
        {
          plan.apply_payload with
          ops = redact_path_oriented_json plan.apply_payload.ops;
          data = redact_path_oriented_json plan.apply_payload.data;
        };
      source =
        {
          plan.source with
          extra =
            (match Config_show.redact_json (`Assoc plan.source.extra) with
            | `Assoc fields -> fields
            | _ -> plan.source.extra);
        };
      destination =
        {
          plan.destination with
          extra =
            (match Config_show.redact_json (`Assoc plan.destination.extra) with
            | `Assoc fields -> fields
            | _ -> plan.destination.extra);
        };
    }
  in
  { redacted with digest = compute_digest redacted }

let make ~principal ~source ~destination ~current_state ~planned_state ~diff
    ~readiness ~warnings ~base_revision ~apply_payload
    ?(ttl_seconds = default_ttl_seconds) ?(now = Unix.gettimeofday ()) ?id () =
  let id = match id with Some i -> i | None -> generate_id ~now () in
  let created_at = Time_util.iso8601_utc ~t:now () in
  let expires_at = Time_util.iso8601_utc ~t:(now +. ttl_seconds) () in
  let draft =
    {
      id;
      principal;
      source;
      destination;
      current_state;
      planned_state;
      diff;
      readiness;
      warnings;
      base_revision;
      created_at;
      expires_at;
      digest = "";
      apply_payload;
    }
  in
  (* Always freeze the redacted form so digest matches persist/render. *)
  redact draft

let is_expired ?(now = Unix.gettimeofday ()) (plan : t) =
  let expires = Time_util.iso8601_utc ~t:now () in
  (* Lexicographic ISO-8601 comparison is valid for same format. *)
  String.compare expires plan.expires_at > 0

let readiness_ok (plan : t) =
  not (List.exists (fun (r : readiness_item) -> r.status = Fail) plan.readiness)

let to_persist_json (plan : t) : Yojson.Safe.t =
  sort_json_keys (`Assoc (body_fields ~include_digest:true plan))

let to_render_json = to_persist_json

(* ── Decode ─────────────────────────────────────────────────────── *)

let json_assoc = function
  | `Assoc fields -> Ok fields
  | _ -> Error "expected object"

let require_string fields key =
  match List.assoc_opt key fields with
  | Some (`String s) -> Ok s
  | Some _ -> Error (Printf.sprintf "field %s must be a string" key)
  | None -> Error (Printf.sprintf "missing field %s" key)

let require_json fields key =
  match List.assoc_opt key fields with
  | Some j -> Ok j
  | None -> Error (Printf.sprintf "missing field %s" key)

let require_list fields key =
  match List.assoc_opt key fields with
  | Some (`List items) -> Ok items
  | Some _ -> Error (Printf.sprintf "field %s must be a list" key)
  | None -> Error (Printf.sprintf "missing field %s" key)

let opt_string fields key =
  match List.assoc_opt key fields with
  | None | Some `Null -> None
  | Some (`String s) -> Some s
  | Some _ -> None

let ( let* ) = Result.bind

let principal_of_json j =
  let* fields = json_assoc j in
  let* id = require_string fields "id" in
  let* kind_s = require_string fields "kind" in
  let* kind = principal_kind_of_string kind_s in
  Ok { id; kind; label = opt_string fields "label" }

let context_of_json j =
  let* fields = json_assoc j in
  let extra =
    match List.assoc_opt "extra" fields with Some (`Assoc e) -> e | _ -> []
  in
  Ok
    {
      room_id = opt_string fields "room_id";
      session_key = opt_string fields "session_key";
      connector = opt_string fields "connector";
      profile_id = opt_string fields "profile_id";
      extra;
    }

let readiness_item_of_json j =
  let* fields = json_assoc j in
  let* name = require_string fields "name" in
  let* status_s = require_string fields "status" in
  let* status = readiness_status_of_string status_s in
  let* message = require_string fields "message" in
  Ok { name; status; message }

let warning_of_json j =
  let* fields = json_assoc j in
  let* code = require_string fields "code" in
  let* message = require_string fields "message" in
  Ok { code; message }

let diff_op_of_json j =
  let* fields = json_assoc j in
  let* op = require_string fields "op" in
  let* path = require_string fields "path" in
  match op with
  | "create" ->
      let* value = require_json fields "value" in
      Ok (Create { path; value })
  | "update" ->
      let* from_ = require_json fields "from" in
      let* to_ = require_json fields "to" in
      Ok (Update { path; from_; to_ })
  | "delete" ->
      let* old = require_json fields "old" in
      Ok (Delete { path; old })
  | "bind" ->
      let* target = require_string fields "target" in
      let active =
        match List.assoc_opt "active" fields with
        | Some (`Bool b) -> b
        | _ -> true
      in
      Ok (Bind { path; target; active })
  | "note" ->
      let* message = require_string fields "message" in
      Ok (Note { path; message })
  | other -> Error (Printf.sprintf "unknown diff op: %s" other)

let apply_payload_of_json j =
  let* fields = json_assoc j in
  let* kind_j = require_json fields "kind" in
  let* kind = apply_kind_of_json kind_j in
  let* ops = require_json fields "ops" in
  let* data = require_json fields "data" in
  Ok { kind; ops; data }

let map_list f items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs -> (
        match f x with Ok v -> loop (v :: acc) xs | Error e -> Error e)
  in
  loop [] items

let of_persist_json j =
  let* fields = json_assoc j in
  let* id = require_string fields "id" in
  let* principal_j = require_json fields "principal" in
  let* principal = principal_of_json principal_j in
  let* source_j = require_json fields "source" in
  let* source = context_of_json source_j in
  let* dest_j = require_json fields "destination" in
  let* destination = context_of_json dest_j in
  let* current_state = require_json fields "current_state" in
  let* planned_state = require_json fields "planned_state" in
  let* diff_items = require_list fields "diff" in
  let* diff = map_list diff_op_of_json diff_items in
  let* readiness_items = require_list fields "readiness" in
  let* readiness = map_list readiness_item_of_json readiness_items in
  let* warning_items = require_list fields "warnings" in
  let* warnings = map_list warning_of_json warning_items in
  let* base_revision = require_string fields "base_revision" in
  let* created_at = require_string fields "created_at" in
  let* expires_at = require_string fields "expires_at" in
  let* digest = require_string fields "digest" in
  let* payload_j = require_json fields "apply_payload" in
  let* apply_payload = apply_payload_of_json payload_j in
  let plan =
    {
      id;
      principal;
      source;
      destination;
      current_state;
      planned_state;
      diff;
      readiness;
      warnings;
      base_revision;
      created_at;
      expires_at;
      digest;
      apply_payload;
    }
  in
  let expected = compute_digest plan in
  if not (digests_equal expected digest) then Error "digest mismatch on load"
  else Ok plan

let format_summary (plan : t) =
  let readiness_s =
    let fails =
      List.filter (fun (r : readiness_item) -> r.status = Fail) plan.readiness
    in
    let warns =
      List.filter (fun (r : readiness_item) -> r.status = Warn) plan.readiness
    in
    Printf.sprintf "%d checks (%d fail, %d warn)"
      (List.length plan.readiness)
      (List.length fails) (List.length warns)
  in
  let dest =
    match plan.destination.room_id with
    | Some r -> r
    | None -> (
        match plan.destination.profile_id with Some p -> p | None -> "(none)")
  in
  Printf.sprintf
    "Setup plan %s\n\
     Principal: %s (%s)\n\
     Destination: %s\n\
     Diff ops: %d\n\
     Readiness: %s\n\
     Warnings: %d\n\
     Base revision: %s\n\
     Expires: %s\n\
     Digest: %s"
    plan.id plan.principal.id
    (principal_kind_to_string plan.principal.kind)
    dest (List.length plan.diff) readiness_s
    (List.length plan.warnings)
    plan.base_revision plan.expires_at plan.digest
