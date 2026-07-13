(** Plain-text and editless fallbacks for [Github_delivery_intent]
    (P19.M3.E2.T003).

    Pure string construction — no network, credentials, or comment bodies.
    Deterministic compact markdown for non-Teams connectors and Direct Sessions.
*)

module D = Github_delivery_intent

(** {1 Item key / display helpers} *)

(** Parse [pr:owner/repo:N] / [issue:owner/repo:N] for headings. *)
let parse_item_key item_key =
  match String.split_on_char ':' item_key with
  | [ "pr"; repo; num ] when String.trim repo <> "" && String.trim num <> "" ->
      Some (`Pull_request, repo, num)
  | [ "issue"; repo; num ] when String.trim repo <> "" && String.trim num <> ""
    ->
      Some (`Issue, repo, num)
  | _ -> None

let kind_label = function `Pull_request -> "PR" | `Issue -> "Issue"

let display_title (i : D.intent) =
  match i.title with
  | Some t when String.trim t <> "" -> String.trim t
  | _ -> i.item_key

let header_line (i : D.intent) =
  match parse_item_key i.item_key with
  | Some (kind, repo, num) ->
      Printf.sprintf "%s #%s · %s" (kind_label kind) num repo
  | None -> i.item_key

let payload_int key payload =
  match payload with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key payload with
      | `Int n -> Some n
      | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
      | _ -> None)
  | _ -> None

let payload_string_list key payload =
  match payload with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key payload with
      | `List items ->
          let rec loop acc = function
            | [] -> List.rev acc
            | `String s :: rest when String.trim s <> "" -> loop (s :: acc) rest
            | _ :: rest -> loop acc rest
          in
          loop [] items
      | _ -> [])
  | _ -> []

let format_csv = function [] -> None | xs -> Some (String.concat ", " xs)

(** {1 Body construction} *)

let kind_tag = function
  | D.Create_lifecycle_card -> "lifecycle"
  | D.Update_card -> "update"
  | D.Reply_in_thread -> "reply"
  | D.Plain_message -> "message"

let metadata_lines (i : D.intent) =
  let lines = ref [] in
  let add s = lines := s :: !lines in
  (match i.state with
  | Some s when String.trim s <> "" -> add (Printf.sprintf "State: %s" s)
  | _ -> ());
  (match format_csv i.labels with
  | Some s -> add (Printf.sprintf "Labels: %s" s)
  | None -> ());
  let assignees = payload_string_list "assignees" i.payload in
  (match format_csv assignees with
  | Some s -> add (Printf.sprintf "Assignees: %s" s)
  | None -> ());
  (match payload_int "comment_count" i.payload with
  | Some n when n > 0 -> add (Printf.sprintf "Comments: %d" n)
  | _ -> ());
  (match i.projection_revision with
  | Some r -> add (Printf.sprintf "Revision: %d" r)
  | None -> (
      match payload_int "revision" i.payload with
      | Some r -> add (Printf.sprintf "Revision: %d" r)
      | None -> ()));
  List.rev !lines

let full_message_body (i : D.intent) =
  let title = display_title i in
  let header = header_line i in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add (Printf.sprintf "**%s**" header);
  if title <> header then add (Printf.sprintf "**%s**" title);
  List.iter add (metadata_lines i);
  if String.trim i.summary <> "" then add i.summary;
  (match i.html_url with
  | Some url when String.trim url <> "" -> add url
  | _ -> ());
  add (Printf.sprintf "_%s_" (kind_tag i.kind));
  String.concat "\n" (List.rev !lines)

(** Compact reply / plain_message body — still includes item identity. *)
let compact_message_body (i : D.intent) =
  let header = header_line i in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add (Printf.sprintf "**%s**" header);
  (match i.title with
  | Some t when String.trim t <> "" ->
      add (Printf.sprintf "**%s**" (String.trim t))
  | _ -> ());
  (match i.state with
  | Some s when String.trim s <> "" -> add (Printf.sprintf "State: %s" s)
  | _ -> ());
  if String.trim i.summary <> "" then add i.summary else add "GitHub update";
  (match i.html_url with
  | Some url when String.trim url <> "" -> add url
  | _ -> ());
  add (Printf.sprintf "_%s_" (kind_tag i.kind));
  String.concat "\n" (List.rev !lines)

let body_for_intent (i : D.intent) =
  match i.kind with
  | D.Create_lifecycle_card | D.Update_card -> full_message_body i
  | D.Reply_in_thread | D.Plain_message -> compact_message_body i

(** Explicit degraded note for connectors/Sessions that cannot edit in place.
    Direct Sessions share the same intent path and report weaker continuity
    rather than silently implying card/thread continuity. *)
let editless_footer =
  "_Delivery: full replacement (no in-place edit; weaker continuity)_"

(** {1 Public API} *)

let render_plain (i : D.intent) = body_for_intent i

let render_editless (i : D.intent) =
  let body = body_for_intent i in
  body ^ "\n" ^ editless_footer

let select_renderer ~supports_adaptive_cards ~supports_edit (_intent : D.intent)
    =
  if supports_adaptive_cards then `Adaptive_card
  else if supports_edit then `Plain
  else `Editless_plain
