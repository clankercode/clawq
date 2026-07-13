(** Indexed Room/item history for Session context (P19.M3.E1.T004). *)

module J = Github_room_event_journal
module P = Github_item_projection
module E = Github_event_envelope

type context_slice = {
  room_id : string;
  item_key : string option;
  entries : J.journal_entry list;
  projections : P.projection list;
  truncated : bool;
}

let default_context_limit = 50

let ensure_schema db =
  (* Journal owns the durable table + composite room/item/time indexes.
     Projections are loaded alongside journal entries for context slices. *)
  J.ensure_schema db;
  P.ensure_schema db

let history_for_room ~db ~room_id ?before ?limit () =
  ensure_schema db;
  J.list_recent ~db ~room_id ?before ?limit ()

let history_for_item ~db ~room_id ~item_key ?limit () =
  ensure_schema db;
  if String.trim item_key = "" then Error "item_key must be non-empty"
  else J.list_recent ~db ~room_id ~item_key ?limit ()

let projections_for_context ~db ~room_id ~item_key ~entries =
  match item_key with
  | Some key -> (
      match P.get ~db ~room_id ~item_key:key with
      | Error e -> Error e
      | Ok None -> Ok []
      | Ok (Some p) -> Ok [ p ])
  | None -> (
      match P.list_for_room ~db ~room_id with
      | Error e -> Error e
      | Ok (_ :: _ as ps) -> Ok ps
      | Ok [] ->
          (* No stored projections yet: try distinct keys from the journal
             window (still empty if reduce has not been run). *)
          let keys =
            entries
            |> List.map (fun (e : J.journal_entry) -> e.item_key)
            |> List.sort_uniq String.compare
          in
          let rec load acc = function
            | [] -> Ok (List.rev acc)
            | k :: rest -> (
                match P.get ~db ~room_id ~item_key:k with
                | Error e -> Error e
                | Ok None -> load acc rest
                | Ok (Some p) -> load (p :: acc) rest)
          in
          load [] keys)

let context_for_session ~db ~room_id ?item_key ?(limit = default_context_limit)
    () =
  ensure_schema db;
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    let item_key =
      match item_key with
      | Some k when String.trim k <> "" -> Some (String.trim k)
      | Some _ -> None
      | None -> None
    in
    let fetch_limit = if limit > 0 then limit + 1 else 0 in
    let history =
      match item_key with
      | Some key ->
          if fetch_limit > 0 then
            J.list_recent ~db ~room_id ~item_key:key ~limit:fetch_limit ()
          else J.list_recent ~db ~room_id ~item_key:key ()
      | None ->
          if fetch_limit > 0 then
            J.list_recent ~db ~room_id ~limit:fetch_limit ()
          else J.list_recent ~db ~room_id ()
    in
    match history with
    | Error e -> Error e
    | Ok rows -> (
        let truncated, entries =
          if limit > 0 && List.length rows > limit then
            (* rows are chronological ASC of the newest (limit+1); drop the
               oldest extra so the window stays the most recent [limit]. *)
            (true, List.tl rows)
          else (false, rows)
        in
        match projections_for_context ~db ~room_id ~item_key ~entries with
        | Error e -> Error e
        | Ok projections ->
            Ok { room_id; item_key; entries; projections; truncated })

let opt_s = function Some s -> s | None -> "-"

let opt_bool = function
  | Some true -> "true"
  | Some false -> "false"
  | None -> "-"

let string_of_card_kind = function
  | P.Lifecycle -> "lifecycle"
  | P.Update -> "update"

let format_projection_line (p : P.projection) =
  (* Structural projection fields only — omit free-text titles so a PR title
     that happens to contain token-like substrings cannot leak into the
     session preamble. Labels/assignees are GitHub handles, not secrets. *)
  let labels = match p.labels with [] -> "-" | xs -> String.concat "," xs in
  let assignees =
    match p.assignees with [] -> "-" | xs -> String.concat "," xs
  in
  Printf.sprintf
    "item=%s state=%s draft=%s merged=%s comments=%d rev=%d card=%s labels=%s \
     assignees=%s head=%s url=%s"
    p.item_key (opt_s p.state) (opt_bool p.draft) (opt_bool p.merged)
    p.comment_count p.revision
    (string_of_card_kind p.card_kind)
    labels assignees (opt_s p.head_sha) (opt_s p.html_url)

let format_entry_line (e : J.journal_entry) =
  (* Prefer short envelope summary; never dump raw JSON (could re-emit large
     fields). Envelope itself has no bodies/secrets by construction. *)
  match E.of_safe_json e.envelope_json with
  | Ok env ->
      let action = match env.action with Some a -> a | None -> "-" in
      let actor =
        match env.actor.login with
        | Some l when String.trim l <> "" -> l
        | _ -> "-"
      in
      let family = E.string_of_family env.family in
      Printf.sprintf "%s item=%s event=%s action=%s family=%s actor=%s"
        e.created_at e.item_key env.event action family actor
  | Error _ -> Printf.sprintf "%s item=%s id=%s" e.created_at e.item_key e.id

let format_context_block (slice : context_slice) =
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "[github_room_context]";
  add (Printf.sprintf "room_id=%s" slice.room_id);
  (match slice.item_key with
  | Some k -> add (Printf.sprintf "item_key=%s" k)
  | None -> add "item_key=*");
  add
    (Printf.sprintf "truncated=%s"
       (if slice.truncated then "true" else "false"));
  add (Printf.sprintf "entry_count=%d" (List.length slice.entries));
  add (Printf.sprintf "projection_count=%d" (List.length slice.projections));
  add "";
  add "projections:";
  (match slice.projections with
  | [] -> add "  (none)"
  | ps -> List.iter (fun p -> add ("  " ^ format_projection_line p)) ps);
  add "";
  add "recent_events:";
  (match slice.entries with
  | [] -> add "  (none)"
  | es -> List.iter (fun e -> add ("  " ^ format_entry_line e)) es);
  String.concat "\n" (List.rev !lines)
