(** Room-scoped GitHub read/search/status tools (P19.M4.E1.T002). *)

module P = Github_item_projection
module J = Github_room_event_journal
module Auth = Github_auth_selection
module E = Github_event_envelope

type tool_name = Get_item | Search_items | Get_status | List_room_items
type tool_request = { room_id : string; name : tool_name; args : Yojson.Safe.t }

type tool_result =
  | Ok_json of Yojson.Safe.t
  | Denied of string
  | Error of string

let tool_name_to_string = function
  | Get_item -> "github_room_get_item"
  | Search_items -> "github_room_search_items"
  | Get_status -> "github_room_get_status"
  | List_room_items -> "github_room_list_items"

(** {1 JSON helpers} *)

let opt_string = function None -> `Null | Some s -> `String s
let opt_bool = function None -> `Null | Some b -> `Bool b
let string_list_json xs = `List (List.map (fun s -> `String s) xs)

let projection_to_json (p : P.projection) : Yojson.Safe.t =
  let card_kind =
    match p.card_kind with P.Lifecycle -> "lifecycle" | P.Update -> "update"
  in
  let last_family =
    match p.last_family with
    | None -> `Null
    | Some f -> `String (E.string_of_family f)
  in
  `Assoc
    [
      ("room_id", `String p.room_id);
      ("item_key", `String p.item_key);
      ("title", opt_string p.title);
      ("state", opt_string p.state);
      ("draft", opt_bool p.draft);
      ("merged", opt_bool p.merged);
      ("labels", string_list_json p.labels);
      ("assignees", string_list_json p.assignees);
      ("head_sha", opt_string p.head_sha);
      ("html_url", opt_string p.html_url);
      ("last_event_at", opt_string p.last_event_at);
      ("last_family", last_family);
      ("comment_count", `Int p.comment_count);
      ("revision", `Int p.revision);
      ("card_kind", `String card_kind);
    ]

let status_to_json (p : P.projection) : Yojson.Safe.t =
  let last_family =
    match p.last_family with
    | None -> `Null
    | Some f -> `String (E.string_of_family f)
  in
  `Assoc
    [
      ("room_id", `String p.room_id);
      ("item_key", `String p.item_key);
      ("state", opt_string p.state);
      ("draft", opt_bool p.draft);
      ("merged", opt_bool p.merged);
      ("head_sha", opt_string p.head_sha);
      ("comment_count", `Int p.comment_count);
      ("revision", `Int p.revision);
      ("last_event_at", opt_string p.last_event_at);
      ("last_family", last_family);
      ("title", opt_string p.title);
      ("labels", string_list_json p.labels);
    ]

(** {1 Arg parsing} *)

let arg_string args key =
  match args with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key args with
      | `String s ->
          let t = String.trim s in
          if t = "" then None else Some t
      | _ -> None)
  | _ -> None

let arg_int_opt args key =
  match args with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key args with
      | `Int n -> Some n
      | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
      | _ -> None)
  | _ -> None

(** {1 Item key / auth} *)

let repo_of_item_key item_key =
  match String.split_on_char ':' item_key with
  | (("pr" | "issue") as _kind) :: repo :: _num :: _ when String.trim repo <> ""
    ->
      Some (String.trim repo)
  | "event" :: repo :: _ when String.trim repo <> "" -> Some (String.trim repo)
  | _ -> None

let authorize_repo ?auth ?installation ~repo () =
  match auth with
  | None -> Result.Ok ()
  | Some auth_snap -> (
      let sel =
        Auth.select_for_repo ~auth:auth_snap ?installation ~repo_full_name:repo
          ()
      in
      match sel.chosen with
      | `None ->
          Result.Error
            (Printf.sprintf "repository %s not authorized: %s" repo
               sel.explanation)
      | `Pat | `App _ -> Result.Ok ())

let authorize_item_key ?auth ?installation item_key =
  match repo_of_item_key item_key with
  | None -> (
      match auth with
      | None -> Result.Ok ()
      | Some _ ->
          Result.Error
            (Printf.sprintf
               "cannot authorize item_key %S: expected pr:owner/repo:N or \
                issue:owner/repo:N"
               item_key))
  | Some repo -> authorize_repo ?auth ?installation ~repo ()

let projection_authorized ?auth ?installation (p : P.projection) =
  match authorize_item_key ?auth ?installation p.item_key with
  | Result.Ok () -> true
  | Result.Error _ -> false

(** {1 Room access / emptiness} *)

let room_has_any_data ~db ~room_id =
  match P.list_for_room ~db ~room_id with
  | Result.Error e -> Result.Error e
  | Result.Ok (_ :: _) -> Result.Ok true
  | Result.Ok [] -> (
      match J.list_for_room ~db ~room_id ~limit:1 () with
      | Result.Error e -> Result.Error e
      | Result.Ok [] -> Result.Ok false
      | Result.Ok _ -> Result.Ok true)

let deny_if_room_empty ~db ~room_id : tool_result option =
  match room_has_any_data ~db ~room_id with
  | Result.Error e -> Some (Error e)
  | Result.Ok true -> None
  | Result.Ok false ->
      Some
        (Denied
           (Printf.sprintf
              "Room %S has no GitHub item access: empty journal and projections"
              room_id))

let item_in_room ~db ~room_id ~item_key =
  match P.get ~db ~room_id ~item_key with
  | Result.Error e -> Result.Error e
  | Result.Ok (Some _) -> Result.Ok true
  | Result.Ok None -> (
      match J.list_recent ~db ~room_id ~item_key ~limit:1 () with
      | Result.Error e -> Result.Error e
      | Result.Ok [] -> Result.Ok false
      | Result.Ok _ -> Result.Ok true)

(** {1 Search} *)

let contains_ci hay needle =
  let hay = String.lowercase_ascii hay in
  let needle = String.lowercase_ascii needle in
  let n = String.length needle in
  let h = String.length hay in
  if n = 0 then true
  else if n > h then false
  else
    let rec loop i =
      if i > h - n then false
      else if String.sub hay i n = needle then true
      else loop (i + 1)
    in
    loop 0

let projection_matches_query (p : P.projection) ~query =
  let q = String.trim query in
  if q = "" then true
  else
    let title = Option.value p.title ~default:"" in
    let state = Option.value p.state ~default:"" in
    let labels = String.concat " " p.labels in
    let assignees = String.concat " " p.assignees in
    let item_key = p.item_key in
    contains_ci title q || contains_ci state q || contains_ci labels q
    || contains_ci assignees q || contains_ci item_key q

(** {1 Tool schemas} *)

let schema_object ~properties ~required =
  `Assoc
    [
      ("type", `String "object");
      ("properties", `Assoc properties);
      ("required", `List (List.map (fun s -> `String s) required));
      ("additionalProperties", `Bool false);
    ]

let string_prop desc =
  `Assoc [ ("type", `String "string"); ("description", `String desc) ]

let int_prop desc =
  `Assoc [ ("type", `String "integer"); ("description", `String desc) ]

let openai_tool ~name ~description ~parameters : Yojson.Safe.t =
  `Assoc
    [
      ("type", `String "function");
      ( "function",
        `Assoc
          [
            ("name", `String name);
            ("description", `String description);
            ("parameters", parameters);
          ] );
    ]

let tool_definitions () : Yojson.Safe.t list =
  [
    openai_tool
      ~name:(tool_name_to_string Get_item)
      ~description:
        "Read a GitHub item projection for the current Room. Requires the \
         item_key to exist in this Room's projections or journal. Secret-free; \
         no cross-Room leakage."
      ~parameters:
        (schema_object
           ~properties:
             [
               ( "item_key",
                 string_prop
                   "Canonical item key (e.g. pr:owner/repo:42 or \
                    issue:owner/repo:7)" );
             ]
           ~required:[ "item_key" ]);
    openai_tool
      ~name:(tool_name_to_string Search_items)
      ~description:
        "Search GitHub item projections in the current Room by title, state, \
         labels, assignees, or item_key substring. Room-scoped only."
      ~parameters:
        (schema_object
           ~properties:
             [
               ( "query",
                 string_prop
                   "Substring to match against \
                    title/state/labels/assignees/item_key" );
               ("limit", int_prop "Max results to return (default 20)");
             ]
           ~required:[ "query" ]);
    openai_tool
      ~name:(tool_name_to_string Get_status)
      ~description:
        "Get status fields for a Room-scoped GitHub item projection (state, \
         draft, merged, head_sha, comment_count, revision)."
      ~parameters:
        (schema_object
           ~properties:
             [
               ( "item_key",
                 string_prop "Canonical item key (e.g. pr:owner/repo:42)" );
             ]
           ~required:[ "item_key" ]);
    openai_tool
      ~name:(tool_name_to_string List_room_items)
      ~description:
        "List all GitHub item projections visible in the current Room. Does \
         not include other Rooms."
      ~parameters:
        (schema_object
           ~properties:
             [ ("limit", int_prop "Max items to return (default: all)") ]
           ~required:[]);
  ]

(** {1 Dispatch} *)

let require_nonempty_room room_id =
  let room_id = String.trim room_id in
  if room_id = "" then Result.Error "room_id must be non-empty"
  else Result.Ok room_id

let take_limit ?limit xs =
  match limit with
  | None -> xs
  | Some n when n < 0 -> xs
  | Some n ->
      let rec loop i acc = function
        | [] -> List.rev acc
        | _ when i >= n -> List.rev acc
        | x :: xs -> loop (i + 1) (x :: acc) xs
      in
      loop 0 [] xs

let dispatch_get_item ~db ~room_id ~args ?auth ?installation () : tool_result =
  match arg_string args "item_key" with
  | None ->
      Error
        "missing required argument item_key (canonical form pr:owner/repo:N or \
         issue:owner/repo:N)"
  | Some item_key -> (
      match authorize_item_key ?auth ?installation item_key with
      | Result.Error msg -> Denied msg
      | Result.Ok () -> (
          match item_in_room ~db ~room_id ~item_key with
          | Result.Error e -> Error e
          | Result.Ok false ->
              Denied
                (Printf.sprintf
                   "item_key %S is not present in Room %S projections or \
                    journal"
                   item_key room_id)
          | Result.Ok true -> (
              match P.get ~db ~room_id ~item_key with
              | Result.Error e -> Error e
              | Result.Ok (Some p) -> Ok_json (projection_to_json p)
              | Result.Ok None ->
                  Ok_json
                    (`Assoc
                       [
                         ("room_id", `String room_id);
                         ("item_key", `String item_key);
                         ("source", `String "journal");
                         ("title", `Null);
                         ("state", `Null);
                         ("labels", `List []);
                         ("comment_count", `Int 0);
                         ("revision", `Int 0);
                       ]))))

let dispatch_get_status ~db ~room_id ~args ?auth ?installation () : tool_result
    =
  match arg_string args "item_key" with
  | None ->
      Error
        "missing required argument item_key (canonical form pr:owner/repo:N or \
         issue:owner/repo:N)"
  | Some item_key -> (
      match authorize_item_key ?auth ?installation item_key with
      | Result.Error msg -> Denied msg
      | Result.Ok () -> (
          match P.get ~db ~room_id ~item_key with
          | Result.Error e -> Error e
          | Result.Ok (Some p) -> Ok_json (status_to_json p)
          | Result.Ok None -> (
              match item_in_room ~db ~room_id ~item_key with
              | Result.Error e -> Error e
              | Result.Ok false ->
                  Denied
                    (Printf.sprintf
                       "item_key %S is not present in Room %S projections or \
                        journal"
                       item_key room_id)
              | Result.Ok true ->
                  Error
                    (Printf.sprintf
                       "item_key %S has journal history in Room %S but no \
                        projection status yet; re-reduce the room or wait for \
                        a lifecycle event"
                       item_key room_id))))

let dispatch_search ~db ~room_id ~args ?auth ?installation () : tool_result =
  match arg_string args "query" with
  | None ->
      Error
        "missing required argument query (substring over title/state/labels)"
  | Some query -> (
      match P.list_for_room ~db ~room_id with
      | Result.Error e -> Error e
      | Result.Ok all ->
          let limit = arg_int_opt args "limit" in
          let filtered =
            all
            |> List.filter (projection_authorized ?auth ?installation)
            |> List.filter (projection_matches_query ~query)
            |> take_limit ?limit
          in
          Ok_json
            (`Assoc
               [
                 ("room_id", `String room_id);
                 ("query", `String query);
                 ("count", `Int (List.length filtered));
                 ("items", `List (List.map projection_to_json filtered));
               ]))

let dispatch_list ~db ~room_id ~args ?auth ?installation () : tool_result =
  match P.list_for_room ~db ~room_id with
  | Result.Error e -> Error e
  | Result.Ok all ->
      let limit = arg_int_opt args "limit" in
      let filtered =
        all
        |> List.filter (projection_authorized ?auth ?installation)
        |> take_limit ?limit
      in
      Ok_json
        (`Assoc
           [
             ("room_id", `String room_id);
             ("count", `Int (List.length filtered));
             ("items", `List (List.map projection_to_json filtered));
           ])

let dispatch ~db ~request ?auth ?installation () : tool_result =
  match require_nonempty_room request.room_id with
  | Result.Error msg -> Error msg
  | Result.Ok room_id -> (
      match deny_if_room_empty ~db ~room_id with
      | Some r -> r
      | None -> (
          match request.name with
          | Get_item ->
              dispatch_get_item ~db ~room_id ~args:request.args ?auth
                ?installation ()
          | Get_status ->
              dispatch_get_status ~db ~room_id ~args:request.args ?auth
                ?installation ()
          | Search_items ->
              dispatch_search ~db ~room_id ~args:request.args ?auth
                ?installation ()
          | List_room_items ->
              dispatch_list ~db ~room_id ~args:request.args ?auth ?installation
                ()))
