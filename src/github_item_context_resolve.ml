(** Resolve card actions / thread replies / Room mentions to item context
    (P19.M3.E2.T004).

    Read-only: never wakes the agent. Room-scoped journal + projections only. *)

module J = Github_room_event_journal
module P = Github_item_projection
module H = Github_event_history_index

type source =
  | Card_action of { action : string; item_key : string; room_id : string }
  | Thread_reply of {
      room_id : string;
      thread_ref : string option;
      text : string;
    }
  | Room_mention of {
      room_id : string;
      text : string;
      item_key_hint : string option;
    }

type resolved = {
  room_id : string;
  item_key : string option;
  projection : P.projection option;
  history : J.journal_entry list;
  context_block : string;
  ambiguity : string list;
}

(** {1 Character class helpers for ref scanning} *)

let is_repo_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '_' || c = '.' || c = '-'

let is_digit c = c >= '0' && c <= '9'

let is_boundary = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '-' | '#' | '/' | ':' ->
      false
  | _ -> true

(** {1 parse_item_refs} *)

let push_unique_rev acc s =
  if List.exists (fun x -> String.equal x s) acc then acc else s :: acc

(** Scan [text] left-to-right for [owner/repo#N] and bare [#N]. Full refs are
    preferred; a bare [#N] that sits inside an already-captured full ref is
    omitted. *)
let parse_item_refs ~text =
  let s = text in
  let n = String.length s in
  let rec digits_end i =
    if i < n && is_digit s.[i] then digits_end (i + 1) else i
  in
  let rec repo_end i =
    if i < n && is_repo_char s.[i] then repo_end (i + 1) else i
  in
  (* Collect (start, end_exclusive, ref_string) hits. *)
  let hits = ref [] in
  let add ~start ~stop ref_s = hits := (start, stop, ref_s) :: !hits in
  (* Pass 1: owner/repo#N *)
  let rec pass_full i =
    if i >= n then ()
    else if is_repo_char s.[i] && (i = 0 || is_boundary s.[i - 1]) then
      let owner_end = repo_end i in
      if owner_end > i && owner_end < n && s.[owner_end] = '/' then
        let repo_start = owner_end + 1 in
        let repo_end_i = repo_end repo_start in
        if repo_end_i > repo_start && repo_end_i < n && s.[repo_end_i] = '#'
        then
          let num_end = digits_end (repo_end_i + 1) in
          if
            num_end > repo_end_i + 1 && (num_end >= n || is_boundary s.[num_end])
          then (
            let owner = String.sub s i (owner_end - i) in
            let repo = String.sub s repo_start (repo_end_i - repo_start) in
            let num =
              String.sub s (repo_end_i + 1) (num_end - repo_end_i - 1)
            in
            add ~start:i ~stop:num_end
              (Printf.sprintf "%s/%s#%s" owner repo num);
            pass_full num_end)
          else pass_full (i + 1)
        else pass_full (i + 1)
      else pass_full (i + 1)
    else pass_full (i + 1)
  in
  pass_full 0;
  let full_hits = List.rev !hits in
  hits := [];
  (* Pass 2: bare #N not overlapping a full hit. *)
  let overlaps i = List.exists (fun (a, b, _) -> i >= a && i < b) full_hits in
  let rec pass_bare i =
    if i >= n then ()
    else if s.[i] = '#' && not (overlaps i) then
      let num_end = digits_end (i + 1) in
      if
        num_end > i + 1
        && (i = 0 || is_boundary s.[i - 1] || s.[i - 1] = '/')
        && (num_end >= n || is_boundary s.[num_end])
      then (
        let num = String.sub s (i + 1) (num_end - i - 1) in
        add ~start:i ~stop:num_end (Printf.sprintf "#%s" num);
        pass_bare num_end)
      else pass_bare (i + 1)
    else pass_bare (i + 1)
  in
  pass_bare 0;
  let bare_hits = List.rev !hits in
  (* Merge by start position; de-dupe ref strings. *)
  let ordered =
    full_hits @ bare_hits |> List.sort (fun (a, _, _) (b, _, _) -> compare a b)
  in
  List.fold_left (fun acc (_, _, r) -> push_unique_rev acc r) [] ordered
  |> List.rev

(** {1 Room-scoped item inventory} *)

let sort_uniq_keys keys = keys |> List.sort_uniq String.compare

let room_item_keys ~db ~room_id =
  H.ensure_schema db;
  match P.list_for_room ~db ~room_id with
  | Error e -> Error e
  | Ok projections -> (
      let from_proj =
        List.map (fun (p : P.projection) -> p.item_key) projections
      in
      match J.list_recent ~db ~room_id () with
      | Error e -> Error e
      | Ok entries ->
          let from_j =
            List.map (fun (e : J.journal_entry) -> e.item_key) entries
          in
          Ok (sort_uniq_keys (from_proj @ from_j)))

let parse_stored_item_key key =
  match String.split_on_char ':' key with
  | [ "pr"; repo; num ] when String.trim repo <> "" && String.trim num <> "" ->
      Some (`Pr, String.lowercase_ascii repo, num)
  | [ "issue"; repo; num ] when String.trim repo <> "" && String.trim num <> ""
    ->
      Some (`Issue, String.lowercase_ascii repo, num)
  | _ -> None

let parse_ref_pattern ref_s =
  let t = String.trim ref_s in
  if t = "" then None
  else if t.[0] = '#' then
    let num = String.sub t 1 (String.length t - 1) in
    if num <> "" && String.for_all is_digit num then Some (`Bare num) else None
  else
    match String.split_on_char '#' t with
    | [ path; num ]
      when String.trim path <> ""
           && num <> ""
           && String.for_all is_digit num
           && String.contains path '/' ->
        Some (`Full (String.lowercase_ascii (String.trim path), num))
    | _ -> (
        (* Accept full stored keys as refs too. *)
        match parse_stored_item_key t with
        | Some (kind, repo, num) ->
            Some
              (`Stored
                 ( (match kind with `Pr -> "pr" | `Issue -> "issue"),
                   repo,
                   num,
                   t ))
        | None -> None)

let key_matches_ref key ref_pat =
  match (parse_stored_item_key key, ref_pat) with
  | None, _ -> false
  | Some (_, _, num), `Bare n -> String.equal num n
  | Some (_, repo, num), `Full (path, n) ->
      String.equal repo path && String.equal num n
  | Some (kind, repo, num), `Stored (k, r, n, _) ->
      let kind_s = match kind with `Pr -> "pr" | `Issue -> "issue" in
      String.equal kind_s k && String.equal repo r && String.equal num n

let match_refs_to_keys ~refs ~keys =
  let pats = refs |> List.filter_map parse_ref_pattern in
  (* Also treat raw exact key strings in refs. *)
  let exact =
    refs |> List.filter (fun r -> List.exists (String.equal r) keys)
  in
  let from_pats =
    keys
    |> List.filter (fun k -> List.exists (fun p -> key_matches_ref k p) pats)
  in
  sort_uniq_keys (exact @ from_pats)

(** {1 Thread ref → journal item_keys} *)

let item_keys_for_thread_ref ~db ~room_id ~thread_ref =
  let ref_s = String.trim thread_ref in
  if ref_s = "" then Ok []
  else
    match J.list_recent ~db ~room_id () with
    | Error e -> Error e
    | Ok entries ->
        let matched =
          List.filter
            (fun (e : J.journal_entry) ->
              String.equal e.item_key ref_s
              || String.equal e.id ref_s
              || (match e.delivery_id with
                | Some d -> String.equal d ref_s
                | None -> false)
              ||
              match e.session_message_id with
              | Some m -> String.equal m ref_s
              | None -> false)
            entries
        in
        Ok
          (matched
          |> List.map (fun (e : J.journal_entry) -> e.item_key)
          |> sort_uniq_keys)

(** {1 Build resolved slice} *)

let projection_for ~db ~room_id ~item_key =
  match item_key with
  | None -> Ok None
  | Some key -> (
      match P.get ~db ~room_id ~item_key:key with
      | Error e -> Error e
      | Ok p -> Ok p)

let build_resolved ~db ~room_id ~item_key ~ambiguity =
  let room_id = String.trim room_id in
  if room_id = "" then Error "room_id must be non-empty"
  else
    let item_key =
      match item_key with
      | Some k when String.trim k <> "" -> Some (String.trim k)
      | Some _ -> None
      | None -> None
    in
    match
      match item_key with
      | Some key -> H.context_for_session ~db ~room_id ~item_key:key ()
      | None -> H.context_for_session ~db ~room_id ()
    with
    | Error e -> Error e
    | Ok slice -> (
        match projection_for ~db ~room_id ~item_key with
        | Error e -> Error e
        | Ok projection ->
            (* Prefer the projection matching item_key; fall back to sole
               projection in the slice. *)
            let projection =
              match (item_key, projection, slice.projections) with
              | Some key, Some p, _ -> Some p
              | Some key, None, ps ->
                  List.find_opt
                    (fun (p : P.projection) -> String.equal p.item_key key)
                    ps
              | None, _, [ p ] when ambiguity = [] -> Some p
              | _ -> None
            in
            Ok
              {
                room_id;
                item_key;
                projection;
                history = slice.entries;
                context_block = H.format_context_block slice;
                ambiguity;
              })

let resolve_candidates ~db ~room_id ~candidates =
  match candidates with
  | [ one ] -> build_resolved ~db ~room_id ~item_key:(Some one) ~ambiguity:[]
  | [] -> build_resolved ~db ~room_id ~item_key:None ~ambiguity:[]
  | many -> build_resolved ~db ~room_id ~item_key:None ~ambiguity:many

(** {1 resolve} *)

let resolve ~db ~source () =
  H.ensure_schema db;
  match source with
  | Card_action { action = _; item_key; room_id } ->
      let room_id = String.trim room_id in
      let item_key = String.trim item_key in
      if room_id = "" then Error "room_id must be non-empty"
      else if item_key = "" then Error "item_key must be non-empty"
      else
        (* Card actions carry durable item identity; load room-scoped context
           only — never cross rooms. *)
        build_resolved ~db ~room_id ~item_key:(Some item_key) ~ambiguity:[]
  | Thread_reply { room_id; thread_ref; text } -> (
      let room_id = String.trim room_id in
      if room_id = "" then Error "room_id must be non-empty"
      else
        match
          match thread_ref with
          | Some r when String.trim r <> "" ->
              item_keys_for_thread_ref ~db ~room_id ~thread_ref:r
          | _ -> Ok []
        with
        | Error e -> Error e
        | Ok from_thread -> (
            match from_thread with
            | _ :: _ -> resolve_candidates ~db ~room_id ~candidates:from_thread
            | [] -> (
                (* Fall back to PR/issue refs in the reply text. *)
                match room_item_keys ~db ~room_id with
                | Error e -> Error e
                | Ok keys ->
                    let refs = parse_item_refs ~text in
                    let candidates = match_refs_to_keys ~refs ~keys in
                    resolve_candidates ~db ~room_id ~candidates)))
  | Room_mention { room_id; text; item_key_hint } -> (
      let room_id = String.trim room_id in
      if room_id = "" then Error "room_id must be non-empty"
      else
        match room_item_keys ~db ~room_id with
        | Error e -> Error e
        | Ok keys -> (
            (* Exact stored key in hint wins when present in this room. *)
            let exact_hint =
              match item_key_hint with
              | Some h ->
                  let h = String.trim h in
                  if h <> "" && List.exists (String.equal h) keys then Some h
                  else None
              | None -> None
            in
            match exact_hint with
            | Some key ->
                build_resolved ~db ~room_id ~item_key:(Some key) ~ambiguity:[]
            | None ->
                let refs =
                  let from_text = parse_item_refs ~text in
                  match item_key_hint with
                  | Some h when String.trim h <> "" ->
                      String.trim h :: from_text
                  | _ -> from_text
                in
                let candidates = match_refs_to_keys ~refs ~keys in
                resolve_candidates ~db ~room_id ~candidates))
