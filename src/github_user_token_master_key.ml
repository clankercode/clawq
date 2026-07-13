(** Vault master-key source version and startup boundary (P21.M2.E4.T006).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

let schema_version = 1
let default_env_var = "CLAWQ_GITHUB_VAULT_MASTER_KEY"
let default_max_file_mode = 0o600

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type source_kind = Env of { var_name : string } | File of { path : string }
type key_role = Active | Staged | Backup_required | Retired
type key_id = string
type key_version = int

type key_metadata = {
  key_id : key_id;
  key_version : key_version;
  role : key_role;
  source_kind : source_kind;
}

type not_ready_reason =
  | Missing
  | Wrong
  | Duplicated
  | Unsupported
  | Inaccessible
  | Permissions
  | Empty
  | Invalid_metadata
  | No_active

type source_config = {
  kind : source_kind;
  key_id : key_id;
  key_version : key_version;
  role : key_role;
  min_length : int option;
  expected_length : int option;
  max_file_mode : int option;
}

type keyring_config = { schema_version : int; sources : source_config list }

type env_observation = {
  present : bool;
  empty : bool;
  byte_length : int option;
}

type file_observation = {
  exists : bool;
  readable : bool;
  is_regular : bool;
  mode : int option;
  size : int option;
  mode_ok : bool option;
}

type material_observation = {
  present : bool;
  empty : bool;
  byte_length : int option;
  valid : bool;
  failure : not_ready_reason option;
}

type source_observation = {
  config : source_config;
  env : env_observation option;
  file : file_observation option;
  material : material_observation;
  access_error : string option;
}

type file_stat = {
  exists : bool;
  readable : bool;
  is_regular : bool;
  mode : int;
  size : int;
}

type env_reader = var_name:string -> string option
type file_stat_fn = path:string -> (file_stat, string) result
type file_read_fn = path:string -> (string, string) result

type readiness =
  | Ready of { active : key_metadata; available : key_metadata list }
  | NotReady of {
      reasons : not_ready_reason list;
      observed : key_metadata list;
    }

type redacted_diagnostics = {
  schema_version : int;
  ready : bool;
  active_key_id : key_id option;
  active_key_version : key_version option;
  active_role : string option;
  active_source : string option;
  available_key_ids : key_id list;
  source_kinds : string list;
  reasons : string list;
  observed_key_ids : key_id list;
  allows_user_authorization : bool;
  note : string;
}

(* -------------------------------------------------------------------------- *)
(* String / metadata helpers                                                  *)
(* -------------------------------------------------------------------------- *)

let string_of_reason = function
  | Missing -> "missing"
  | Wrong -> "wrong"
  | Duplicated -> "duplicated"
  | Unsupported -> "unsupported"
  | Inaccessible -> "inaccessible"
  | Permissions -> "permissions"
  | Empty -> "empty"
  | Invalid_metadata -> "invalid_metadata"
  | No_active -> "no_active"

let string_of_role = function
  | Active -> "active"
  | Staged -> "staged"
  | Backup_required -> "backup_required"
  | Retired -> "retired"

let string_of_source_kind = function
  | Env { var_name } -> Printf.sprintf "env:%s" var_name
  | File { path } -> Printf.sprintf "file:%s" path

let metadata_of_config (c : source_config) : key_metadata =
  {
    key_id = c.key_id;
    key_version = c.key_version;
    role = c.role;
    source_kind = c.kind;
  }

let uniq_reasons (rs : not_ready_reason list) : not_ready_reason list =
  let rec go acc = function
    | [] -> List.rev acc
    | r :: rest ->
        if List.exists (fun x -> x = r) acc then go acc rest
        else go (r :: acc) rest
  in
  go [] rs

(* -------------------------------------------------------------------------- *)
(* Config construction / validation                                           *)
(* -------------------------------------------------------------------------- *)

let make_source ~kind ~key_id ~key_version ~role ?min_length ?expected_length
    ?max_file_mode () =
  let key_id = String.trim key_id in
  if key_id = "" then Error "key_id must be non-empty"
  else if key_version <= 0 then Error "key_version must be positive"
  else if match min_length with Some n when n < 0 -> true | _ -> false then
    Error "min_length must be non-negative"
  else if match expected_length with Some n when n <= 0 -> true | _ -> false
  then Error "expected_length must be positive"
  else
    let kind =
      match kind with
      | Env { var_name } ->
          let var_name = String.trim var_name in
          if var_name = "" then Error "env var_name must be non-empty"
          else Ok (Env { var_name })
      | File { path } ->
          let path = String.trim path in
          if path = "" then Error "file path must be non-empty"
          else Ok (File { path })
    in
    match kind with
    | Error e -> Error e
    | Ok kind ->
        Ok
          {
            kind;
            key_id;
            key_version;
            role;
            min_length;
            expected_length;
            max_file_mode;
          }

let make_keyring ?schema_version:sv ~sources () =
  let ver = match sv with None -> schema_version | Some v -> v in
  if sources = [] then Error "keyring sources must be non-empty"
  else if ver <= 0 then Error "schema_version must be positive"
  else if ver > 1 then
    Error (Printf.sprintf "unsupported master-key source schema_version %d" ver)
  else Ok { schema_version = ver; sources }

let source_metadata_valid (c : source_config) =
  let key_id = String.trim c.key_id in
  key_id <> "" && c.key_version > 0
  &&
  match c.kind with
  | Env { var_name } -> String.trim var_name <> ""
  | File { path } -> String.trim path <> ""

let validate_keyring_config (k : keyring_config) :
    (unit, not_ready_reason list) result =
  let reasons = ref [] in
  let add r = reasons := r :: !reasons in
  if k.schema_version <> schema_version then add Unsupported;
  if k.sources = [] then add No_active;
  List.iter
    (fun (c : source_config) ->
      if not (source_metadata_valid c) then add Invalid_metadata;
      (* Retired sources must not be used as live material providers. *)
      match c.role with
      | Retired -> add Unsupported
      | _ -> ())
    k.sources;
  let active =
    List.filter (fun (c : source_config) -> c.role = Active) k.sources
  in
  (match active with [] -> add No_active | [ _ ] -> () | _ -> add Duplicated);
  (* Duplicate key_id across any sources is fail-closed. *)
  let ids = List.map (fun (c : source_config) -> c.key_id) k.sources in
  let rec has_dup = function
    | [] | [ _ ] -> false
    | x :: rest -> List.mem x rest || has_dup rest
  in
  if has_dup ids then add Duplicated;
  match uniq_reasons (List.rev !reasons) with [] -> Ok () | rs -> Error rs

(* -------------------------------------------------------------------------- *)
(* Material validation (discard after check)                                  *)
(* -------------------------------------------------------------------------- *)

let empty_material =
  {
    present = false;
    empty = true;
    byte_length = None;
    valid = false;
    failure = Some Missing;
  }

let validate_material (c : source_config) (raw : string) : material_observation
    =
  let trimmed = String.trim raw in
  let len = String.length trimmed in
  if len = 0 then
    {
      present = true;
      empty = true;
      byte_length = Some 0;
      valid = false;
      failure = Some Empty;
    }
  else
    let min_ok = match c.min_length with None -> true | Some n -> len >= n in
    let exact_ok =
      match c.expected_length with None -> true | Some n -> len = n
    in
    if min_ok && exact_ok then
      {
        present = true;
        empty = false;
        byte_length = Some len;
        valid = true;
        failure = None;
      }
    else
      {
        present = true;
        empty = false;
        byte_length = Some len;
        valid = false;
        failure = Some Wrong;
      }

(* -------------------------------------------------------------------------- *)
(* Observations                                                               *)
(* -------------------------------------------------------------------------- *)

let default_env_reader ~var_name = Sys.getenv_opt var_name

let observe_env ~env_reader ~var_name : env_observation =
  match env_reader ~var_name with
  | None -> { present = false; empty = true; byte_length = None }
  | Some v ->
      let t = String.trim v in
      let len = String.length t in
      {
        present = true;
        empty = len = 0;
        byte_length = Some len (* length only; value discarded *);
      }

let mode_allowed ~max_mode mode =
  (* Refuse any bits outside owner r/w and refuse group/other access bits. *)
  let group_other = mode land 0o077 in
  group_other = 0 && mode land 0o777 land lnot max_mode = 0

let file_mode_cap (c : source_config) =
  match c.max_file_mode with Some m -> m | None -> default_max_file_mode

let observe_source ~env_reader ~file_stat ~file_read (c : source_config) :
    source_observation =
  match c.kind with
  | Env { var_name } ->
      let env = observe_env ~env_reader ~var_name in
      let material, access_error =
        match env_reader ~var_name with
        | None -> (empty_material, None)
        | Some raw ->
            (* Validate then drop raw. *)
            let m = validate_material c raw in
            (m, None)
      in
      { config = c; env = Some env; file = None; material; access_error }
  | File { path } -> (
      match file_stat ~path with
      | Error tag ->
          {
            config = c;
            env = None;
            file =
              Some
                {
                  exists = false;
                  readable = false;
                  is_regular = false;
                  mode = None;
                  size = None;
                  mode_ok = None;
                };
            material =
              {
                present = false;
                empty = true;
                byte_length = None;
                valid = false;
                failure = Some Inaccessible;
              };
            access_error = Some tag;
          }
      | Ok st ->
          let max_mode = file_mode_cap c in
          let mode_ok =
            if not st.exists then None
            else if not st.is_regular then Some false
            else Some (mode_allowed ~max_mode st.mode)
          in
          let file =
            {
              exists = st.exists;
              readable = st.readable;
              is_regular = st.is_regular;
              mode = (if st.exists then Some st.mode else None);
              size = (if st.exists then Some st.size else None);
              mode_ok;
            }
          in
          if not st.exists then
            {
              config = c;
              env = None;
              file = Some file;
              material = empty_material;
              access_error = Some "enoent";
            }
          else if not st.is_regular then
            {
              config = c;
              env = None;
              file = Some file;
              material =
                {
                  present = false;
                  empty = true;
                  byte_length = None;
                  valid = false;
                  failure = Some Permissions;
                };
              access_error = Some "not_regular";
            }
          else if mode_ok = Some false then
            {
              config = c;
              env = None;
              file = Some file;
              material =
                {
                  present = false;
                  empty = true;
                  byte_length = None;
                  valid = false;
                  failure = Some Permissions;
                };
              access_error = Some "mode";
            }
          else if not st.readable then
            {
              config = c;
              env = None;
              file = Some file;
              material =
                {
                  present = false;
                  empty = true;
                  byte_length = None;
                  valid = false;
                  failure = Some Inaccessible;
                };
              access_error = Some "eacces";
            }
          else
            let material, access_error =
              match file_read ~path with
              | Error tag ->
                  ( {
                      present = false;
                      empty = true;
                      byte_length = None;
                      valid = false;
                      failure = Some Inaccessible;
                    },
                    Some tag )
              | Ok raw ->
                  let m = validate_material c raw in
                  (m, None)
            in
            { config = c; env = None; file = Some file; material; access_error }
      )

let observe_keyring ~env_reader ~file_stat ~file_read (k : keyring_config) =
  List.map (observe_source ~env_reader ~file_stat ~file_read) k.sources

(* -------------------------------------------------------------------------- *)
(* Readiness evaluation                                                       *)
(* -------------------------------------------------------------------------- *)

let reason_of_observation (o : source_observation) : not_ready_reason option =
  match o.material.failure with
  | Some r -> Some r
  | None -> if o.material.valid then None else Some Wrong

let optional_hard_failure (o : source_observation) : not_ready_reason option =
  match o.config.role with
  | Active | Retired -> None
  | Staged | Backup_required -> (
      match o.material.failure with
      | Some Permissions -> Some Permissions
      | Some Wrong when o.material.present -> Some Wrong
      | Some Empty when o.material.present -> Some Empty
      | Some Inaccessible -> (
          (* Pure missing (enoent / unset) is soft for optional roles; an
             existing but unreadable file is hard. *)
          match o.file with
          | Some f when f.exists -> Some Inaccessible
          | _ -> None)
      | _ -> None)

let evaluate ~keyring ~observations : readiness =
  let observed = List.map (fun o -> metadata_of_config o.config) observations in
  match validate_keyring_config keyring with
  | Error rs -> NotReady { reasons = rs; observed }
  | Ok () -> (
      let by_id =
        List.map
          (fun (o : source_observation) -> (o.config.key_id, o))
          observations
      in
      let missing_obs =
        List.filter
          (fun (c : source_config) -> not (List.mem_assoc c.key_id by_id))
          keyring.sources
      in
      if missing_obs <> [] then NotReady { reasons = [ Missing ]; observed }
      else
        let active_cfgs =
          List.filter
            (fun (c : source_config) -> c.role = Active)
            keyring.sources
        in
        let active_obs =
          List.filter_map
            (fun (c : source_config) -> List.assoc_opt c.key_id by_id)
            active_cfgs
        in
        let valid_active =
          List.filter
            (fun (o : source_observation) -> o.material.valid)
            active_obs
        in
        let active_fail = List.filter_map reason_of_observation active_obs in
        let optional_hard =
          List.filter_map optional_hard_failure (List.map snd by_id)
        in
        let available_of observations_list =
          List.filter_map
            (fun (o : source_observation) ->
              match o.config.role with
              | (Staged | Backup_required) when o.material.valid ->
                  Some (metadata_of_config o.config)
              | _ -> None)
            observations_list
        in
        match (valid_active, optional_hard) with
        | [ one ], [] ->
            Ready
              {
                active = metadata_of_config one.config;
                available = available_of (List.map snd by_id);
              }
        | [], hard ->
            let rs = uniq_reasons (active_fail @ hard) in
            let rs = if rs = [] then [ No_active ] else rs in
            NotReady { reasons = rs; observed }
        | _ :: _ :: _, hard ->
            NotReady
              {
                reasons = uniq_reasons ((Duplicated :: active_fail) @ hard);
                observed;
              }
        | [ _ ], hard ->
            NotReady { reasons = uniq_reasons (active_fail @ hard); observed })

let default_file_stat ~path:_ = Error "no_file_probe"
let default_file_read ~path:_ = Error "no_file_probe"

let probe ?env_reader ?file_stat ?file_read keyring =
  let env_reader =
    match env_reader with Some f -> f | None -> default_env_reader
  in
  let file_stat =
    match file_stat with Some f -> f | None -> default_file_stat
  in
  let file_read =
    match file_read with Some f -> f | None -> default_file_read
  in
  let observations =
    observe_keyring ~env_reader ~file_stat ~file_read keyring
  in
  evaluate ~keyring ~observations

let is_ready = function Ready _ -> true | NotReady _ -> false

let active_metadata = function
  | Ready { active; _ } -> Some active
  | NotReady _ -> None

let reasons = function Ready _ -> [] | NotReady { reasons = rs; _ } -> rs
let allows_user_authorization = is_ready

(* -------------------------------------------------------------------------- *)
(* Redacted diagnostics                                                       *)
(* -------------------------------------------------------------------------- *)

let key_id_of (m : key_metadata) = m.key_id
let source_kind_of (m : key_metadata) = m.source_kind
let role_of (m : key_metadata) = m.role
let key_version_of (m : key_metadata) = m.key_version

let diagnostics ~schema_version (r : readiness) : redacted_diagnostics =
  match r with
  | Ready { active; available } ->
      {
        schema_version;
        ready = true;
        active_key_id = Some (key_id_of active);
        active_key_version = Some (key_version_of active);
        active_role = Some (string_of_role (role_of active));
        active_source = Some (string_of_source_kind (source_kind_of active));
        available_key_ids = List.map key_id_of available;
        source_kinds =
          string_of_source_kind (source_kind_of active)
          :: List.map
               (fun m -> string_of_source_kind (source_kind_of m))
               available;
        reasons = [];
        observed_key_ids = key_id_of active :: List.map key_id_of available;
        allows_user_authorization = true;
        note =
          "vault master key ready from external source; user authorization may \
           proceed only when other readiness checks also pass";
      }
  | NotReady { reasons = rs; observed } ->
      {
        schema_version;
        ready = false;
        active_key_id = None;
        active_key_version = None;
        active_role = None;
        active_source = None;
        available_key_ids = [];
        source_kinds =
          List.map (fun m -> string_of_source_kind (source_kind_of m)) observed;
        reasons = List.map string_of_reason rs;
        observed_key_ids = List.map key_id_of observed;
        allows_user_authorization = false;
        note =
          "vault master key not ready; refuse user authorization and vault \
           seal; no plaintext fallback storage";
      }

let diagnostics_to_json (d : redacted_diagnostics) : Yojson.Safe.t =
  let opt_string = function None -> `Null | Some s -> `String s in
  let opt_int = function None -> `Null | Some i -> `Int i in
  `Assoc
    [
      ("schema_version", `Int d.schema_version);
      ("ready", `Bool d.ready);
      ("active_key_id", opt_string d.active_key_id);
      ("active_key_version", opt_int d.active_key_version);
      ("active_role", opt_string d.active_role);
      ("active_source", opt_string d.active_source);
      ( "available_key_ids",
        `List (List.map (fun s -> `String s) d.available_key_ids) );
      ("source_kinds", `List (List.map (fun s -> `String s) d.source_kinds));
      ("reasons", `List (List.map (fun s -> `String s) d.reasons));
      ( "observed_key_ids",
        `List (List.map (fun s -> `String s) d.observed_key_ids) );
      ("allows_user_authorization", `Bool d.allows_user_authorization);
      ("note", `String d.note);
    ]

let format_diagnostics (d : redacted_diagnostics) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf
    (Printf.sprintf
       "GitHub vault master-key readiness: %s (allows_user_authorization=%b)\n"
       (if d.ready then "ready" else "not_ready")
       d.allows_user_authorization);
  Buffer.add_string buf
    (Printf.sprintf "  schema_version=%d\n" d.schema_version);
  (match (d.active_key_id, d.active_key_version, d.active_source) with
  | Some id, Some ver, Some src ->
      Buffer.add_string buf
        (Printf.sprintf "  active key_id=%s version=%d source=%s\n" id ver src)
  | _ -> Buffer.add_string buf "  active: none\n");
  if d.available_key_ids <> [] then
    Buffer.add_string buf
      (Printf.sprintf "  available_key_ids=%s\n"
         (String.concat "," d.available_key_ids));
  if d.reasons <> [] then
    Buffer.add_string buf
      (Printf.sprintf "  reasons=%s\n" (String.concat "," d.reasons));
  Buffer.add_string buf (Printf.sprintf "  note: %s\n" d.note);
  Buffer.contents buf

let diagnostics_contains_plaintext ~diagnostics:d ~plaintext =
  if plaintext = "" then false
  else
    let fields =
      [
        d.note;
        Option.value d.active_key_id ~default:"";
        Option.value d.active_role ~default:"";
        Option.value d.active_source ~default:"";
      ]
      @ d.available_key_ids @ d.source_kinds @ d.reasons @ d.observed_key_ids
    in
    List.exists
      (fun s -> s = plaintext || String_util.contains s plaintext)
      fields

let rec json_contains_plaintext ~json ~plaintext =
  if plaintext = "" then false
  else
    match json with
    | `String s -> String.equal s plaintext || String_util.contains s plaintext
    | `Assoc fields ->
        List.exists
          (fun (_k, v) -> json_contains_plaintext ~json:v ~plaintext)
          fields
    | `List items ->
        List.exists (fun v -> json_contains_plaintext ~json:v ~plaintext) items
    | _ -> false
