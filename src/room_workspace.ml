(** Safe persistent room workspace path generation and validation.

    Paths are deterministic: the same room identifier always produces the same
    directory under [~/.clawq/workspace/rooms/]. The path is a URL-safe slug
    plus a 12-hex-char SHA-256 suffix, guaranteeing uniqueness while keeping
    paths human-readable.

    Explicit [workspace_dir] overrides are admin-only and must pass containment
    validation: the resolved real path must live under the rooms root or the
    configured [extra_allowed_paths]. *)

let rooms_subdir = "rooms"

(** [rooms_root ()] returns the canonical rooms directory:
    [<dot_dir>/workspace/rooms/]. *)
let rooms_root () =
  Filename.concat (Filename.concat (Dot_dir.path ()) "workspace") rooms_subdir

(** [ensure_dir path] creates [path] and all missing parents, like [mkdir -p].
    Ignores errors (e.g. already exists). *)
let ensure_dir path =
  let rec loop p =
    if p = "" || p = "/" then ()
    else if Sys.file_exists p then ()
    else
      let parent = Filename.dirname p in
      if parent <> p then loop parent;
      try Unix.mkdir p 0o755 with _ -> ()
  in
  loop path

(* -- slug helpers ---------------------------------------------------------- *)

let is_alphanum_or_hyphen c =
  (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-'

let slugify ?(max_len = 48) raw =
  let len = String.length raw in
  let buf = Buffer.create len in
  let prev_was_hyphen = ref true in
  (* avoid leading hyphens *)
  String.iter
    (fun c ->
      let c = Char.lowercase_ascii c in
      if is_alphanum_or_hyphen c then begin
        Buffer.add_char buf c;
        prev_was_hyphen := c = '-'
      end
      else if not !prev_was_hyphen then begin
        Buffer.add_char buf '-';
        prev_was_hyphen := true
      end)
    raw;
  (* trim trailing hyphen *)
  let s = Buffer.contents buf in
  let s =
    if String.length s > 0 && s.[String.length s - 1] = '-' then
      String.sub s 0 (String.length s - 1)
    else s
  in
  (* truncate to max_len, trimming any trailing hyphen introduced by cut *)
  let s =
    if String.length s > max_len then
      let s' = String.sub s 0 max_len in
      if s'.[String.length s' - 1] = '-' then
        String.sub s' 0 (String.length s' - 1)
      else s'
    else s
  in
  (* fallback: if slug is empty, use "room" *)
  if s = "" then "room" else s

(* -- hash helper ----------------------------------------------------------- *)

let hash_hex raw =
  let digest = Digestif.SHA256.digest_string raw in
  let full = Digestif.SHA256.to_hex digest in
  (* take first 12 hex chars = 48 bits of entropy *)
  String.sub full 0 12

(* -- path generation ------------------------------------------------------- *)

(** [workspace_path room_id] computes the deterministic workspace directory for
    a given room identifier. The directory is
    [~/.clawq/workspace/rooms/<slug>-<hash>/]. The directory is created if it
    does not exist. *)
let workspace_path room_id =
  let slug = slugify room_id in
  let hash = hash_hex room_id in
  let dir_name = Printf.sprintf "%s-%s" slug hash in
  let path = Filename.concat (rooms_root ()) dir_name in
  ensure_dir path;
  path

(* -- validation for explicit overrides ------------------------------------- *)

type validation_error =
  | Path_is_empty
  | Contains_traversal
  | Contains_control_chars
  | Name_too_long
  | Contains_null
  | Symlink_escape
  | Containment_violation
  | Not_admin

let validation_error_to_string = function
  | Path_is_empty -> "workspace_dir path is empty"
  | Contains_traversal -> "workspace_dir contains path traversal (..)"
  | Contains_control_chars ->
      "workspace_dir contains control characters (0x00-0x1F or 0x7F)"
  | Name_too_long ->
      "workspace_dir name component exceeds maximum length (255 bytes)"
  | Contains_null -> "workspace_dir contains null byte"
  | Symlink_escape -> "workspace_dir resolves outside allowed directories"
  | Containment_violation ->
      "workspace_dir is not contained under the rooms root or allowed \
       directories"
  | Not_admin -> "workspace_dir override requires admin privileges"

(** [has_traversal_component path] returns [true] if any path component is
    [".."]. Does not do any filesystem resolution. *)
let has_traversal_component path =
  String.split_on_char '/' path |> List.exists (fun c -> c = "..")

(** [has_control_chars s] returns [true] if [s] contains any byte in 0x00-0x1F
    or 0x7F. *)
let has_control_chars s =
  let found = ref false in
  String.iter
    (fun c ->
      let code = Char.code c in
      if code < 0x20 || code = 0x7F then found := true)
    s;
  !found

(** [component_too_long path] returns [true] if any single path component
    exceeds 255 bytes (Linux NAME_MAX). *)
let component_too_long path =
  String.split_on_char '/' path |> List.exists (fun c -> String.length c > 255)

(** [contains_null path] returns [true] if [path] contains a null byte. *)
let contains_null path =
  try
    String.iter (fun c -> if c = '\000' then raise Exit) path;
    false
  with Exit -> true

(** [is_prefix_of ~prefix path] returns [true] if [path] starts with [prefix]
    followed by [/] or is exactly [prefix]. Requires both paths to be absolute.
*)
let is_prefix_of ~prefix path =
  let plen = String.length prefix in
  let pathlen = String.length path in
  if pathlen = plen then path = prefix
  else if pathlen > plen then
    String.sub path 0 plen = prefix && path.[plen] = '/'
  else false

(** [validate_override ?extra_allowed_paths workspace_dir] validates an explicit
    workspace directory override. Returns [Ok resolved_path] if the path passes
    all safety checks, or [Error validation_error] if it fails.

    The resolved real path must be contained under either:
    - the rooms root ([~/.clawq/workspace/rooms/]), or
    - one of the [extra_allowed_paths] (if provided).

    Checks (fail-closed):
    - non-empty
    - no traversal components (..)
    - no control characters
    - no null bytes
    - no component exceeding 255 bytes
    - realpath resolves without error
    - resolved path is not a symlink that escapes containment
    - resolved path is under rooms root or an allowed path *)
let validate_override ?(extra_allowed_paths = []) workspace_dir =
  if workspace_dir = "" then Error Path_is_empty
  else if contains_null workspace_dir then Error Contains_null
  else if has_traversal_component workspace_dir then Error Contains_traversal
  else if has_control_chars workspace_dir then Error Contains_control_chars
  else if component_too_long workspace_dir then Error Name_too_long
  else
    (* Resolve the real path — this follows symlinks and normalizes. *)
    let resolved =
      try Some (Unix.realpath workspace_dir) with Unix.Unix_error _ -> None
    in
    match resolved with
    | None -> Error Containment_violation
    | Some resolved_path ->
        let rooms = rooms_root () in
        let under_rooms = is_prefix_of ~prefix:rooms resolved_path in
        let under_extra =
          List.exists
            (fun extra ->
              let extra =
                if String.length extra > 0 && extra.[0] = '~' then
                  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
                  home ^ String.sub extra 1 (String.length extra - 1)
                else extra
              in
              is_prefix_of ~prefix:extra resolved_path)
            extra_allowed_paths
        in
        if under_rooms || under_extra then Ok resolved_path
        else Error Containment_violation

(** [resolve_workspace ~is_admin ?extra_allowed_paths ?workspace_dir room_id]
    returns the workspace directory for a room. If [workspace_dir] is provided,
    admin privileges are required and the path is validated against containment
    rules; otherwise the deterministic path is computed. *)
let resolve_workspace ~is_admin ?extra_allowed_paths ?workspace_dir room_id =
  match workspace_dir with
  | None -> Ok (workspace_path room_id)
  | Some _dir when not is_admin -> Error Not_admin
  | Some dir -> validate_override ?extra_allowed_paths dir
