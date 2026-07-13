(** Connector-neutral delivery intents for GitHub item cards (P19.M3.E2.T001).
*)

module P = Github_item_projection
module S = Github_route_store
module C = Github_comment_mode
module E = Github_event_envelope

type intent_kind =
  | Create_lifecycle_card
  | Update_card
  | Reply_in_thread
  | Plain_message

type intent = {
  id : string;
  room_id : string;
  item_key : string;
  kind : intent_kind;
  title : string option;
  summary : string;
  html_url : string option;
  state : string option;
  labels : string list;
  comment_mode : S.comment_mode option;
  projection_revision : int option;
  payload : Yojson.Safe.t;
  created_at : string;
}

let string_of_intent_kind = function
  | Create_lifecycle_card -> "create_lifecycle_card"
  | Update_card -> "update_card"
  | Reply_in_thread -> "reply_in_thread"
  | Plain_message -> "plain_message"

let intent_kind_of_string = function
  | "create_lifecycle_card" -> Ok Create_lifecycle_card
  | "update_card" -> Ok Update_card
  | "reply_in_thread" -> Ok Reply_in_thread
  | "plain_message" -> Ok Plain_message
  | s -> Error (Printf.sprintf "unknown intent_kind: %s" s)

let string_of_card_kind = function
  | P.Lifecycle -> "lifecycle"
  | P.Update -> "update"

let string_of_comment_mode = function
  | S.Off -> "off"
  | S.Summary -> "summary"
  | S.Threaded -> "threaded"

let comment_mode_of_string = function
  | "off" -> Ok S.Off
  | "summary" -> Ok S.Summary
  | "threaded" -> Ok S.Threaded
  | s -> Error (Printf.sprintf "unknown comment_mode: %s" s)

let generate_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghdi_%d_%06d" ts rand

let opt_string_field key = function
  | None -> []
  | Some s -> [ (key, `String s) ]

let opt_int_field key = function None -> [] | Some n -> [ (key, `Int n) ]
let opt_bool_field key = function None -> [] | Some b -> [ (key, `Bool b) ]
let string_list_to_json xs = `List (List.map (fun s -> `String s) xs)

let string_list_of_json = function
  | `List items ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | `String s :: rest -> loop (s :: acc) rest
        | _ -> Error "expected string list"
      in
      loop [] items
  | _ -> Error "expected JSON list"

let member_opt key json =
  match json with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key json with `Null -> None | v -> Some v)
  | _ -> None

let get_string key json =
  match member_opt key json with
  | Some (`String s) when String.trim s <> "" -> Some s
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

let family_to_json_string = function
  | None -> None
  | Some f -> Some (E.string_of_family f)

(** Secret-free projection snapshot for Connector renderers. *)
let payload_of_projection (p : P.projection) =
  `Assoc
    ([
       ("item_key", `String p.item_key);
       ("labels", string_list_to_json p.labels);
       ("assignees", string_list_to_json p.assignees);
       ("comment_count", `Int p.comment_count);
       ("revision", `Int p.revision);
       ("card_kind", `String (string_of_card_kind p.card_kind));
     ]
    @ opt_string_field "title" p.title
    @ opt_string_field "state" p.state
    @ opt_bool_field "draft" p.draft
    @ opt_bool_field "merged" p.merged
    @ opt_string_field "head_sha" p.head_sha
    @ opt_string_field "html_url" p.html_url
    @ opt_string_field "last_event_at" p.last_event_at
    @
    match family_to_json_string p.last_family with
    | None -> []
    | Some s -> [ ("last_family", `String s) ])

let display_title (p : P.projection) =
  match p.title with Some t when String.trim t <> "" -> t | _ -> p.item_key

let summary_for_projection (p : P.projection) (kind : intent_kind) =
  let title = display_title p in
  let state = Option.value p.state ~default:"unknown" in
  match kind with
  | Create_lifecycle_card -> Printf.sprintf "%s is %s" title state
  | Update_card ->
      Printf.sprintf "%s updated (%s, rev %d, comments %d)" title state
        p.revision p.comment_count
  | Reply_in_thread | Plain_message -> Printf.sprintf "%s · %s" title state

let kind_of_projection (p : P.projection) ~(prior : P.projection option option)
    =
  match prior with
  | None | Some None -> Create_lifecycle_card
  | Some (Some _) -> (
      match p.card_kind with
      | P.Lifecycle -> Create_lifecycle_card
      | P.Update -> Update_card)

let of_projection ~room_id ~(projection : P.projection) ?comment_mode ?prior
    ?(now = Unix.gettimeofday ()) () : intent =
  let kind = kind_of_projection projection ~prior in
  {
    id = generate_id ~now ();
    room_id;
    item_key = projection.item_key;
    kind;
    title = projection.title;
    summary = summary_for_projection projection kind;
    html_url = projection.html_url;
    state = projection.state;
    labels = projection.labels;
    comment_mode;
    projection_revision = Some projection.revision;
    payload = payload_of_projection projection;
    created_at = Time_util.iso8601_utc ~t:now ();
  }

let summary_for_comment_effect ~item_key ~(effect : C.comment_effect) =
  match effect with
  | C.Drop -> ""
  | C.Summary { comment_count_delta; latest_actor; latest_at = _ } ->
      let who =
        match latest_actor with
        | Some a when String.trim a <> "" -> a
        | _ -> "someone"
      in
      if comment_count_delta = 1 then
        Printf.sprintf "%s: new comment by %s" item_key who
      else
        Printf.sprintf "%s: %d new comments (latest by %s)" item_key
          comment_count_delta who
  | C.Threaded { comment_count_delta; latest_actor; latest_at = _; thread_ref }
    ->
      let who =
        match latest_actor with
        | Some a when String.trim a <> "" -> a
        | _ -> "someone"
      in
      let ref_s =
        match thread_ref with
        | Some r when String.trim r <> "" -> " (" ^ r ^ ")"
        | _ -> ""
      in
      if comment_count_delta = 1 then
        Printf.sprintf "%s: reply by %s%s" item_key who ref_s
      else
        Printf.sprintf "%s: %d replies (latest by %s)%s" item_key
          comment_count_delta who ref_s

let payload_of_comment_effect ~(effect : C.comment_effect) =
  match effect with
  | C.Drop -> `Assoc []
  | C.Summary { comment_count_delta; latest_actor; latest_at } ->
      `Assoc
        ([
           ("comment_count_delta", `Int comment_count_delta);
           ("mode", `String "summary");
         ]
        @ opt_string_field "latest_actor" latest_actor
        @ opt_string_field "latest_at" latest_at)
  | C.Threaded { comment_count_delta; latest_actor; latest_at; thread_ref } ->
      `Assoc
        ([
           ("comment_count_delta", `Int comment_count_delta);
           ("mode", `String "threaded");
         ]
        @ opt_string_field "latest_actor" latest_actor
        @ opt_string_field "latest_at" latest_at
        @ opt_string_field "thread_ref" thread_ref)

let of_comment_effect ~room_id ~item_key ~(effect : C.comment_effect)
    ?(now = Unix.gettimeofday ()) () : intent option =
  match effect with
  | C.Drop -> None
  | C.Summary _ as eff ->
      Some
        {
          id = generate_id ~now ();
          room_id;
          item_key;
          kind = Update_card;
          title = None;
          summary = summary_for_comment_effect ~item_key ~effect:eff;
          html_url = None;
          state = None;
          labels = [];
          comment_mode = Some S.Summary;
          projection_revision = None;
          payload = payload_of_comment_effect ~effect:eff;
          created_at = Time_util.iso8601_utc ~t:now ();
        }
  | C.Threaded _ as eff ->
      Some
        {
          id = generate_id ~now ();
          room_id;
          item_key;
          kind = Reply_in_thread;
          title = None;
          summary = summary_for_comment_effect ~item_key ~effect:eff;
          html_url = None;
          state = None;
          labels = [];
          comment_mode = Some S.Threaded;
          projection_revision = None;
          payload = payload_of_comment_effect ~effect:eff;
          created_at = Time_util.iso8601_utc ~t:now ();
        }

let to_json (i : intent) : Yojson.Safe.t =
  `Assoc
    ([
       ("id", `String i.id);
       ("room_id", `String i.room_id);
       ("item_key", `String i.item_key);
       ("kind", `String (string_of_intent_kind i.kind));
       ("summary", `String i.summary);
       ("labels", string_list_to_json i.labels);
       ("payload", i.payload);
       ("created_at", `String i.created_at);
     ]
    @ opt_string_field "title" i.title
    @ opt_string_field "html_url" i.html_url
    @ opt_string_field "state" i.state
    @ (match i.comment_mode with
      | None -> []
      | Some m -> [ ("comment_mode", `String (string_of_comment_mode m)) ])
    @ opt_int_field "projection_revision" i.projection_revision)

let of_json (json : Yojson.Safe.t) : (intent, string) result =
  match json with
  | `Assoc _ -> (
      match
        ( get_string_required "id" json,
          get_string_required "room_id" json,
          get_string_required "item_key" json,
          get_string_required "kind" json,
          get_string_required "summary" json,
          get_string_required "created_at" json )
      with
      | Ok id, Ok room_id, Ok item_key, Ok kind_s, Ok summary, Ok created_at
        -> (
          match intent_kind_of_string kind_s with
          | Error e -> Error e
          | Ok kind -> (
              let labels =
                match member_opt "labels" json with
                | None -> Ok []
                | Some v -> string_list_of_json v
              in
              let comment_mode =
                match get_string "comment_mode" json with
                | None -> Ok None
                | Some s -> (
                    match comment_mode_of_string s with
                    | Ok m -> Ok (Some m)
                    | Error e -> Error e)
              in
              let payload =
                match member_opt "payload" json with
                | Some p -> p
                | None -> `Assoc []
              in
              match (labels, comment_mode) with
              | Error e, _ | _, Error e -> Error e
              | Ok labels, Ok comment_mode ->
                  Ok
                    {
                      id;
                      room_id;
                      item_key;
                      kind;
                      title = get_string "title" json;
                      summary;
                      html_url = get_string "html_url" json;
                      state = get_string "state" json;
                      labels;
                      comment_mode;
                      projection_revision = get_int "projection_revision" json;
                      payload;
                      created_at;
                    }))
      | Error e, _, _, _, _, _
      | _, Error e, _, _, _, _
      | _, _, Error e, _, _, _
      | _, _, _, Error e, _, _
      | _, _, _, _, Error e, _
      | _, _, _, _, _, Error e ->
          Error e)
  | _ -> Error "intent JSON must be an object"
