(* MCP catalog: identities, pagination drain, list_changed quarantine. *)

type identity = { server : string; remote_name : string; revision : string }

type tool_def = {
  identity : identity;
  description : string;
  annotations : Yojson.Safe.t;
  schema : Yojson.Safe.t;
}

type page = { tools : tool_def list; next_cursor : string option }

type server_status =
  | Available of { revision : string; tools : tool_def list }
  | Quarantined of { revision : string; reason : string }
  | Unavailable of { reason : string }

type t = { servers : (string, server_status) Hashtbl.t }

let max_name_len = 128
let max_description_len = 4096
let max_schema_bytes = 64 * 1024
let max_schema_depth = 32
let max_annotations_bytes = 8 * 1024
let identity_key (id : identity) = id.server ^ "\x1f" ^ id.remote_name

let make_identity ~server ~remote_name ~revision =
  { server; remote_name; revision }

let empty () = { servers = Hashtbl.create 16 }

let has_control_chars s =
  let n = String.length s in
  let rec loop i =
    if i >= n then false
    else
      let c = Char.code s.[i] in
      if c < 32 && c <> 9 && c <> 10 && c <> 13 then true else loop (i + 1)
  in
  loop 0

let rec schema_depth = function
  | `Assoc fields ->
      1 + List.fold_left (fun acc (_, v) -> max acc (schema_depth v)) 0 fields
  | `List items ->
      1 + List.fold_left (fun acc v -> max acc (schema_depth v)) 0 items
  | _ -> 0

let validate_name s =
  if s = "" then Error "empty name"
  else if String.length s > max_name_len then Error "name too long"
  else if has_control_chars s then Error "name has control characters"
  else Ok ()

let ( let* ) = Result.bind

let validate_tool_def (td : tool_def) =
  let* () = validate_name td.identity.server in
  let* () = validate_name td.identity.remote_name in
  let* () =
    if td.identity.revision = "" then Error "empty revision" else Ok ()
  in
  let* () =
    if String.length td.description > max_description_len then
      Error "description too long"
    else if has_control_chars td.description then
      Error "description has control characters"
    else Ok ()
  in
  let ann_s = Yojson.Safe.to_string td.annotations in
  let* () =
    if String.length ann_s > max_annotations_bytes then
      Error "annotations too large"
    else Ok ()
  in
  let sch_s = Yojson.Safe.to_string td.schema in
  let* () =
    if String.length sch_s > max_schema_bytes then Error "schema too large"
    else Ok ()
  in
  let* () =
    if schema_depth td.schema > max_schema_depth then Error "schema too deep"
    else Ok ()
  in
  Ok ()

let apply_pages (cat : t) ~server ~revision ~pages =
  (* Pagination must be fully drained: last page next_cursor = None. *)
  let rec check_drain = function
    | [] -> Error "no pages drained"
    | [ p ] ->
        if p.next_cursor <> None then
          Error "pagination not fully drained (trailing cursor)"
        else Ok ()
    | p :: rest ->
        if p.next_cursor = None && rest <> [] then
          Error "pagination broken: cursor None before last page"
        else check_drain rest
  in
  match check_drain pages with
  | Error e -> Error e
  | Ok () -> (
      let tools = List.concat_map (fun p -> p.tools) pages in
      (* Validate all defs. *)
      let rec validate = function
        | [] -> Ok ()
        | td :: rest -> (
            match validate_tool_def td with
            | Error e -> Error e
            | Ok () ->
                if td.identity.server <> server then
                  Error "tool server mismatch"
                else if td.identity.revision <> revision then
                  Error "tool revision mismatch"
                else validate rest)
      in
      match validate tools with
      | Error e -> Error e
      | Ok () -> (
          (* Collision check within page set and against other available servers. *)
          let keys = Hashtbl.create 32 in
          let rec check_collisions = function
            | [] -> Ok ()
            | (td : tool_def) :: rest ->
                let k = identity_key td.identity in
                if Hashtbl.mem keys k then Error ("collision: " ^ k)
                else begin
                  Hashtbl.add keys k ();
                  (* Cross-server collision on remote_name under same server is
                     covered; global identity is server+remote. *)
                  check_collisions rest
                end
          in
          match check_collisions tools with
          | Error e -> Error e
          | Ok () ->
              Hashtbl.replace cat.servers server (Available { revision; tools });
              Ok cat))

let list_changed (cat : t) ~server ~revision =
  Hashtbl.replace cat.servers server
    (Quarantined
       {
         revision;
         reason =
           Printf.sprintf "list_changed for %s@%s; awaiting relist" server
             revision;
       });
  cat

let mark_relist_failed (cat : t) ~server ~reason =
  Hashtbl.replace cat.servers server (Unavailable { reason });
  cat

let repair_server (cat : t) ~server () =
  Hashtbl.remove cat.servers server;
  cat

let status (cat : t) ~server = Hashtbl.find_opt cat.servers server

let discoverable_tools (cat : t) =
  Hashtbl.fold
    (fun _server st acc ->
      match st with
      | Available { tools; _ } -> tools @ acc
      | Quarantined _ | Unavailable _ -> acc)
    cat.servers []

let is_discoverable (cat : t) ~identity =
  match Hashtbl.find_opt cat.servers identity.server with
  | Some (Available { revision; tools }) when revision = identity.revision ->
      List.exists
        (fun (td : tool_def) ->
          td.identity.remote_name = identity.remote_name
          && td.identity.revision = identity.revision)
        tools
  | _ -> false

let can_invoke (cat : t) ~identity =
  match Hashtbl.find_opt cat.servers identity.server with
  | None -> Error "server unknown"
  | Some (Quarantined { reason; _ }) -> Error ("server quarantined: " ^ reason)
  | Some (Unavailable { reason }) -> Error ("server unavailable: " ^ reason)
  | Some (Available { revision; tools }) ->
      if revision <> identity.revision then
        Error "revision mismatch (race with list_changed/relist)"
      else if
        List.exists
          (fun (td : tool_def) ->
            td.identity.remote_name = identity.remote_name)
          tools
      then Ok ()
      else Error "tool removed; not discoverable"
