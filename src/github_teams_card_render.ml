(** Teams Adaptive Card renderer for [Github_delivery_intent] (P19.M3.E2.T002).

    Produces secret-free Bot Framework Adaptive Card envelopes (v1.4) for
    lifecycle, update, and compact reply intents. No network or credentials. *)

module D = Github_delivery_intent

let adaptive_card_schema = "http://adaptivecards.io/schemas/adaptive-card.json"
let adaptive_card_version = "1.4"

(** {1 Adaptive Card primitives} *)

let text_block ?(size = "Default") ?(weight = "Default") ?(is_subtle = false)
    ?(spacing = "Default") ?(color = "Default") ~text () =
  let fields =
    [
      ("type", `String "TextBlock"); ("text", `String text); ("wrap", `Bool true);
    ]
  in
  let fields =
    if size <> "Default" then ("size", `String size) :: fields else fields
  in
  let fields =
    if weight <> "Default" then ("weight", `String weight) :: fields else fields
  in
  let fields =
    if is_subtle then ("isSubtle", `Bool true) :: fields else fields
  in
  let fields =
    if spacing <> "Default" then ("spacing", `String spacing) :: fields
    else fields
  in
  let fields =
    if color <> "Default" then ("color", `String color) :: fields else fields
  in
  `Assoc fields

let fact_set (facts : (string * string) list) =
  let fact_items =
    List.map
      (fun (title, value) ->
        `Assoc [ ("title", `String title); ("value", `String value) ])
      facts
  in
  `Assoc
    [
      ("type", `String "FactSet");
      ("facts", `List fact_items);
      ("spacing", `String "Medium");
    ]

let open_url_action ~title ~url =
  `Assoc
    [
      ("type", `String "Action.OpenUrl");
      ("title", `String title);
      ("url", `String url);
    ]

(** Safe room actions (resolved later by action/context wiring). No secrets. *)
let submit_action ~title ~action ~item_key ~intent_id =
  `Assoc
    [
      ("type", `String "Action.Submit");
      ("title", `String title);
      ( "data",
        `Assoc
          [
            ("clawq_github_action", `String action);
            ("item_key", `String item_key);
            ("intent_id", `String intent_id);
          ] );
    ]

let wrap_adaptive_card ~body ~actions =
  let content_fields =
    [
      ("type", `String "AdaptiveCard");
      ("$schema", `String adaptive_card_schema);
      ("version", `String adaptive_card_version);
      ("body", `List body);
    ]
  in
  let content_fields =
    match actions with
    | [] -> content_fields
    | a -> content_fields @ [ ("actions", `List a) ]
  in
  `Assoc
    [
      ("type", `String "message");
      ( "attachments",
        `List
          [
            `Assoc
              [
                ( "contentType",
                  `String "application/vnd.microsoft.card.adaptive" );
                ("content", `Assoc content_fields);
              ];
          ] );
    ]

(** {1 Item key / display helpers} *)

(** Parse [pr:owner/repo:N] / [issue:owner/repo:N] for card headings. *)
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

let state_color = function
  | Some s -> (
      match String.lowercase_ascii (String.trim s) with
      | "open" | "reopened" -> "Good"
      | "closed" | "merged" -> "Attention"
      | "draft" -> "Warning"
      | _ -> "Default")
  | None -> "Default"

let payload_string key payload =
  match payload with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key payload with
      | `String s when String.trim s <> "" -> Some s
      | _ -> None)
  | _ -> None

let payload_int key payload =
  match payload with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key payload with
      | `Int n -> Some n
      | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
      | _ -> None)
  | _ -> None

let payload_bool key payload =
  match payload with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key payload with
      | `Bool b -> Some b
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

let format_labels labels =
  match labels with [] -> None | xs -> Some (String.concat ", " xs)

(** {1 Card body builders} *)

let header_text (i : D.intent) =
  match parse_item_key i.item_key with
  | Some (kind, repo, num) ->
      Printf.sprintf "%s #%s · %s" (kind_label kind) num repo
  | None -> i.item_key

let lifecycle_facts (i : D.intent) =
  let facts = ref [] in
  let add title value = facts := (title, value) :: !facts in
  (match parse_item_key i.item_key with
  | Some (kind, repo, num) ->
      add "Item" (Printf.sprintf "%s #%s" (kind_label kind) num);
      add "Repository" repo
  | None -> add "Item" i.item_key);
  (match i.state with
  | Some s when String.trim s <> "" -> add "State" s
  | _ -> ());
  (match payload_bool "draft" i.payload with
  | Some true -> add "Draft" "yes"
  | _ -> ());
  (match payload_bool "merged" i.payload with
  | Some true -> add "Merged" "yes"
  | _ -> ());
  (match format_labels i.labels with Some s -> add "Labels" s | None -> ());
  let assignees = payload_string_list "assignees" i.payload in
  (match format_labels assignees with
  | Some s -> add "Assignees" s
  | None -> ());
  (match payload_int "comment_count" i.payload with
  | Some n when n > 0 -> add "Comments" (string_of_int n)
  | _ -> ());
  (match (i.projection_revision, payload_int "revision" i.payload) with
  | Some r, _ | None, Some r -> add "Revision" (string_of_int r)
  | None, None -> ());
  (match payload_string "last_family" i.payload with
  | Some f -> add "Last event" f
  | None -> ());
  List.rev !facts

let full_card_body (i : D.intent) =
  let title = display_title i in
  let header =
    text_block ~text:(header_text i) ~size:"Medium" ~weight:"Bolder"
      ~color:(state_color i.state) ()
  in
  let title_block =
    text_block ~text:title ~size:"Large" ~weight:"Bolder" ~spacing:"Small" ()
  in
  let summary_block =
    if String.trim i.summary = "" then []
    else [ text_block ~text:i.summary ~is_subtle:true ~spacing:"Small" () ]
  in
  let facts = lifecycle_facts i in
  let fact_blocks = if facts = [] then [] else [ fact_set facts ] in
  (header :: title_block :: summary_block) @ fact_blocks

let full_card_actions (i : D.intent) =
  let open_actions =
    match i.html_url with
    | Some url when String.trim url <> "" ->
        [ open_url_action ~title:"Open on GitHub" ~url ]
    | _ -> []
  in
  (* Safe, non-gated room actions. Policy-gated Review is deferred to later
     action wiring so cards never offer unauthorized App-attributed work. *)
  let room_actions =
    [
      submit_action ~title:"Ask" ~action:"ask" ~item_key:i.item_key
        ~intent_id:i.id;
      submit_action ~title:"Summarize" ~action:"summarize" ~item_key:i.item_key
        ~intent_id:i.id;
    ]
  in
  open_actions @ room_actions

let compact_reply_body (i : D.intent) =
  let title =
    match parse_item_key i.item_key with
    | Some (kind, repo, num) ->
        Printf.sprintf "%s #%s · %s" (kind_label kind) num repo
    | None -> i.item_key
  in
  let summary =
    if String.trim i.summary <> "" then i.summary else "GitHub update"
  in
  [
    text_block ~text:title ~size:"Small" ~weight:"Bolder" ~is_subtle:true ();
    text_block ~text:summary ~spacing:"Small" ();
  ]

let compact_reply_actions (i : D.intent) =
  match i.html_url with
  | Some url when String.trim url <> "" ->
      [ open_url_action ~title:"Open on GitHub" ~url ]
  | _ -> []

(** {1 Public API} *)

let card_supports_edit (i : D.intent) =
  match i.kind with
  | D.Create_lifecycle_card | D.Update_card -> true
  | D.Reply_in_thread | D.Plain_message -> false

let render_adaptive_card (i : D.intent) =
  match i.kind with
  | D.Create_lifecycle_card | D.Update_card ->
      wrap_adaptive_card ~body:(full_card_body i) ~actions:(full_card_actions i)
  | D.Reply_in_thread | D.Plain_message ->
      wrap_adaptive_card ~body:(compact_reply_body i)
        ~actions:(compact_reply_actions i)

let render_update_card (i : D.intent) = render_adaptive_card i
