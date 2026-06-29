(** Teams "what can Clawq do here" capability card.

    Introspects the current room/session state and renders an Adaptive Card
    showing: connector capabilities, profile binding, history capture, delivery
    mode, memory/GitHub readiness, and degraded-behavior explanations. *)

(** {1 Capability introspection} *)

type capability_status = {
  edit : bool;
  delete : bool;
  react : bool;
  typing_indicator : bool;
  status_messages : bool;
  file_sending : bool;
  adaptive_cards : bool;
  buttons : bool;
  history_capture : bool;
  profile_bound : bool;
  memory_available : bool;
  github_configured : bool;
  connector_history_enabled : bool;
  connector_history_persist : bool;
  delivery_mode : string;
  max_message_length : int;
}
(** Snapshot of all capabilities relevant to the current room. *)

(** Derive a human-readable delivery mode string from capabilities. *)
let delivery_mode_of_caps (caps : Connector_capabilities.t) =
  match Connector_capabilities.progress_delivery caps with
  | Edit_progress_in_place -> "edit in place"
  | Delete_and_resend_progress -> "delete and resend"
  | Buffered_progress -> "buffered"

(** Introspect current room capabilities. [~caps] defaults to Teams; pass a
    connector-specific {!Connector_capabilities.t} for non-Teams connectors. *)
let snapshot ?(caps = Connector_capabilities.teams)
    ~(session_manager : Session.t) ~conversation_id () : capability_status =
  let cfg = Session.get_config session_manager in
  let db = Session.get_db session_manager in
  let profile_bound =
    match db with
    | Some db ->
        Option.is_some
          (Memory.get_room_profile_binding ~db ~room_id:conversation_id)
    | None -> false
  in
  let memory_available = Option.is_some db in
  let github_configured =
    match cfg.channels.github with
    | Some g -> Runtime_config.github_has_valid_credentials g
    | None -> false
  in
  {
    edit = caps.can_edit <> No_edit;
    delete = caps.can_delete;
    react = caps.can_react;
    typing_indicator = caps.can_type;
    status_messages = caps.can_show_status;
    file_sending = caps.can_send_files;
    adaptive_cards = caps.can_send_cards;
    buttons = caps.can_send_buttons;
    history_capture =
      profile_bound
      && Connector_capabilities.should_capture_history
           ~enabled:cfg.connector_history.enabled caps;
    profile_bound;
    memory_available;
    github_configured;
    connector_history_enabled = cfg.connector_history.enabled;
    connector_history_persist = cfg.connector_history.persist_to_db;
    delivery_mode = delivery_mode_of_caps caps;
    max_message_length = caps.max_message_length;
  }

(** {1 Degraded behavior explanations} *)

type degraded_item = { feature : string; reason : string }

let degraded_behaviors (snap : capability_status) : degraded_item list =
  let items = ref [] in
  let add feature reason = items := { feature; reason } :: !items in
  if not snap.memory_available then begin
    add "Memory"
      "Database not available — memories and room bindings require a database.";
    add "Room memory"
      "Database not available — room-scoped memory requires a database.";
    add "History (DB)"
      "Database not available — history will only persist in-memory."
  end;
  if not snap.profile_bound then begin
    add "Scoped access"
      "This room is not bound to a room profile — using global access scope.";
    add "Room-scoped memory"
      "No profile binding — room memory commands are inactive."
  end;
  if not snap.history_capture then begin
    let reason =
      if not snap.connector_history_enabled then
        "Connector history is disabled in config (connector_history.enabled = \
         false)."
      else "History capture requires a room profile binding for this connector."
    in
    add "History capture" reason
  end;
  if (not snap.connector_history_persist) && snap.connector_history_enabled then
    add "History persistence"
      "History is only kept in-memory (connector_history.persist_to_db = \
       false). Lost on restart.";
  if not snap.github_configured then
    add "GitHub"
      "GitHub integration not configured — PR/issue features unavailable.";
  List.rev !items

(** {1 Adaptive Card rendering} *)

(** Build a TextBlock element. *)
let text_block ~text ?(size = "Default") ?(weight = "Default")
    ?(spacing = "Default") ?(wrap = true) ?(is_subtle = false) () =
  let fields =
    [
      ("type", `String "TextBlock");
      ("text", `String text);
      ("wrap", `Bool wrap);
      ("spacing", `String spacing);
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
  `Assoc fields

(** Build a FactSet element from a list of (title, value) pairs. *)
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

(** Build an icon indicator. *)
let icon yes no = if yes then "\xE2\x9C\x85" (* check *) else no

(** Build the complete Adaptive Card JSON for the "what can do" view. *)
let build_card ~(snap : capability_status) () : Yojson.Safe.t =
  let degraded = degraded_behaviors snap in
  let header =
    text_block ~text:"\xF0\x9F\x94\x8D What can Clawq do here?" ~size:"Large"
      ~weight:"Bolder" ()
  in
  (* Capabilities fact set *)
  let cap_facts =
    [
      ( "Edit messages",
        icon snap.edit "\xE2\x9D\x8C"
        ^ " "
        ^ if snap.edit then "in-place edit" else "no edit support" );
      ( "Delete messages",
        icon snap.delete "\xE2\x9D\x8C"
        ^ " "
        ^ if snap.delete then "yes" else "no" );
      ( "Reactions",
        icon snap.react "\xE2\x9D\x8C"
        ^ " "
        ^ if snap.react then "yes" else "no" );
      ( "Typing indicator",
        icon snap.typing_indicator "\xE2\x9D\x8C"
        ^ " "
        ^ if snap.typing_indicator then "yes" else "no" );
      ( "Status updates",
        icon snap.status_messages "\xE2\x9D\x8C"
        ^ " "
        ^ if snap.status_messages then "yes" else "no" );
      ( "File sending",
        icon snap.file_sending "\xE2\x9D\x8C"
        ^ " "
        ^ if snap.file_sending then "yes" else "no" );
      ( "Adaptive Cards",
        icon snap.adaptive_cards "\xE2\x9D\x8C"
        ^ " "
        ^ if snap.adaptive_cards then "yes" else "no" );
      ( "Buttons",
        icon snap.buttons "\xE2\x9D\x8C"
        ^ " "
        ^ if snap.buttons then "yes" else "no" );
    ]
  in
  let cap_section =
    [
      text_block ~text:"**Connector Capabilities**" ~spacing:"Large" ();
      fact_set cap_facts;
    ]
  in
  (* Room state section *)
  let room_facts =
    [
      ( "Profile binding",
        icon snap.profile_bound "\xE2\x9D\x97"
        ^ " "
        ^ if snap.profile_bound then "bound" else "not bound" );
      ( "History capture",
        icon snap.history_capture "\xE2\x9D\x97"
        ^ " "
        ^ if snap.history_capture then "active" else "inactive" );
      ( "History persist to DB",
        icon snap.connector_history_persist "\xE2\x9D\x97"
        ^ " "
        ^ if snap.connector_history_persist then "yes" else "in-memory only" );
      ("Delivery mode", snap.delivery_mode);
      ("Max message length", string_of_int snap.max_message_length ^ " chars");
    ]
  in
  let room_section =
    [
      text_block ~text:"**Room & Session State**" ~spacing:"Large" ();
      fact_set room_facts;
    ]
  in
  (* Readiness section *)
  let readiness_facts =
    [
      ( "Memory (database)",
        icon snap.memory_available "\xE2\x9D\x97"
        ^ " "
        ^ if snap.memory_available then "ready" else "not available" );
      ( "GitHub",
        icon snap.github_configured "\xE2\x9D\x97"
        ^ " "
        ^ if snap.github_configured then "configured" else "not configured" );
    ]
  in
  let readiness_section =
    [
      text_block ~text:"**Readiness**" ~spacing:"Large" ();
      fact_set readiness_facts;
    ]
  in
  (* Degraded behaviors section *)
  let degraded_section =
    if degraded = [] then
      [
        text_block
          ~text:
            "\xE2\x9C\x85 **No degraded behaviors** \xe2\x80\x94 all features \
             are available for this room."
          ~spacing:"Large" ();
      ]
    else
      let lines =
        List.map
          (fun (d : degraded_item) ->
            Printf.sprintf "\xE2\x9A\xA0\xEF\xB8\x8F **%s**: %s" d.feature
              d.reason)
          degraded
      in
      [
        text_block ~text:"**Degraded Behaviors**" ~spacing:"Large" ();
        text_block ~text:(String.concat "\n\n" lines) ~spacing:"Small" ();
      ]
  in
  let body =
    (header :: cap_section) @ room_section @ readiness_section
    @ degraded_section
  in
  (* Wrap as Bot Framework attachment *)
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
                ( "content",
                  `Assoc
                    [
                      ("type", `String "AdaptiveCard");
                      ( "$schema",
                        `String
                          "http://adaptivecards.io/schemas/adaptive-card.json"
                      );
                      ("version", `String "1.3");
                      ("body", `List body);
                    ] );
              ];
          ] );
    ]

(** {1 Plain text fallback} *)

(** Build a markdown fallback for non-card connectors or degraded display. *)
let build_text ~(snap : capability_status) () : string =
  let buf = Buffer.create 1024 in
  let add s =
    Buffer.add_string buf s;
    Buffer.add_char buf '\n'
  in
  add "\xF0\x9F\x94\x8D **What can Clawq do here?**";
  add "";
  add "**Connector Capabilities**";
  add
    (Printf.sprintf "- Edit messages: %s"
       (if snap.edit then "\xE2\x9C\x85 in-place edit"
        else "\xE2\x9D\x8C no edit support"));
  add
    (Printf.sprintf "- Delete messages: %s"
       (if snap.delete then "\xE2\x9C\x85 yes" else "\xE2\x9D\x8C no"));
  add
    (Printf.sprintf "- Typing indicator: %s"
       (if snap.typing_indicator then "\xE2\x9C\x85 yes" else "\xE2\x9D\x8C no"));
  add
    (Printf.sprintf "- Status updates: %s"
       (if snap.status_messages then "\xE2\x9C\x85 yes" else "\xE2\x9D\x8C no"));
  add
    (Printf.sprintf "- File sending: %s"
       (if snap.file_sending then "\xE2\x9C\x85 yes" else "\xE2\x9D\x8C no"));
  add
    (Printf.sprintf "- Adaptive Cards: %s"
       (if snap.adaptive_cards then "\xE2\x9C\x85 yes" else "\xE2\x9D\x8C no"));
  add "";
  add "**Room & Session State**";
  add
    (Printf.sprintf "- Profile binding: %s"
       (if snap.profile_bound then "\xE2\x9C\x85 bound"
        else "\xE2\x9D\x97 not bound"));
  add
    (Printf.sprintf "- History capture: %s"
       (if snap.history_capture then "\xE2\x9C\x85 active"
        else "\xE2\x9D\x97 inactive"));
  add
    (Printf.sprintf "- History persist: %s"
       (if snap.connector_history_persist then "\xE2\x9C\x85 database"
        else "\xE2\x9D\x97 in-memory only"));
  add (Printf.sprintf "- Delivery mode: %s" snap.delivery_mode);
  add (Printf.sprintf "- Max message length: %d chars" snap.max_message_length);
  add "";
  add "**Readiness**";
  add
    (Printf.sprintf "- Memory (database): %s"
       (if snap.memory_available then "\xE2\x9C\x85 ready"
        else "\xE2\x9D\x97 not available"));
  add
    (Printf.sprintf "- GitHub: %s"
       (if snap.github_configured then "\xE2\x9C\x85 configured"
        else "\xE2\x9D\x97 not configured"));
  add "";
  let degraded = degraded_behaviors snap in
  if degraded = [] then
    add
      "\xE2\x9C\x85 **No degraded behaviors** \xe2\x80\x94 all features \
       available."
  else begin
    add "**Degraded Behaviors**";
    List.iter
      (fun (d : degraded_item) ->
        add
          (Printf.sprintf "- \xE2\x9A\xA0\xEF\xB8\x8F **%s**: %s" d.feature
             d.reason))
      degraded
  end;
  Buffer.contents buf
