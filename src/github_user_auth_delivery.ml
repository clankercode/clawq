(* Deliver GitHub user-authorization continuations privately.
   See github_user_auth_delivery.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity
module Tx = Github_user_auth_tx

let protocol_version = 1

(* -------------------------------------------------------------------------- *)
(* Content classification                                                     *)
(* -------------------------------------------------------------------------- *)

type content_class = Shared_room_progress | Private_auth_material

let string_of_content_class = function
  | Shared_room_progress -> "shared_room_progress"
  | Private_auth_material -> "private_auth_material"

let content_class_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "shared_room_progress" -> Ok Shared_room_progress
  | "private_auth_material" -> Ok Private_auth_material
  | other -> Error (Printf.sprintf "unknown content_class: %s" other)

type private_material_kind =
  | Authorization_url
  | Device_code
  | Callback_error
  | Account_control

let string_of_private_material_kind = function
  | Authorization_url -> "authorization_url"
  | Device_code -> "device_code"
  | Callback_error -> "callback_error"
  | Account_control -> "account_control"

let private_material_kind_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "authorization_url" -> Ok Authorization_url
  | "device_code" -> Ok Device_code
  | "callback_error" -> Ok Callback_error
  | "account_control" -> Ok Account_control
  | other -> Error (Printf.sprintf "unknown private_material_kind: %s" other)

let classify_material_kind (_ : private_material_kind) = Private_auth_material

let classify_content = function
  | `Progress -> Shared_room_progress
  | `Material _ -> Private_auth_material

(* -------------------------------------------------------------------------- *)
(* Delivery channels                                                          *)
(* -------------------------------------------------------------------------- *)

type delivery_channel =
  | Private_connector_dm of { connector : P.connector; handle_id : string }
  | Principal_browser_continuation of { handle_id : string }
  | Initiating_cli of { handle_id : string }
  | Absent

let string_of_delivery_channel = function
  | Private_connector_dm { connector; handle_id } ->
      Printf.sprintf "private_connector_dm:%s:%s"
        (P.string_of_connector connector)
        handle_id
  | Principal_browser_continuation { handle_id } ->
      "principal_browser_continuation:" ^ handle_id
  | Initiating_cli { handle_id } -> "initiating_cli:" ^ handle_id
  | Absent -> "absent"

let delivery_channel_is_private = function Absent -> false | _ -> true

let validate_delivery_channel = function
  | Absent -> Ok ()
  | Private_connector_dm { handle_id; _ }
  | Principal_browser_continuation { handle_id }
  | Initiating_cli { handle_id } ->
      if String.trim handle_id = "" then
        Error
          "private delivery handle_id must be a non-empty opaque alias (never \
           a token, code, or URL secret)"
      else Ok ()

let make_private_connector_dm ~connector ~handle_id =
  let handle_id = String.trim handle_id in
  let ch = Private_connector_dm { connector; handle_id } in
  match validate_delivery_channel ch with Ok () -> Ok ch | Error e -> Error e

let make_principal_browser_continuation ~handle_id =
  let handle_id = String.trim handle_id in
  let ch = Principal_browser_continuation { handle_id } in
  match validate_delivery_channel ch with Ok () -> Ok ch | Error e -> Error e

let make_initiating_cli ~handle_id =
  let handle_id = String.trim handle_id in
  let ch = Initiating_cli { handle_id } in
  match validate_delivery_channel ch with Ok () -> Ok ch | Error e -> Error e

(* -------------------------------------------------------------------------- *)
(* Content payloads                                                           *)
(* -------------------------------------------------------------------------- *)

type progress_content = { phase : string; detail : string option }

type private_material = {
  kind : private_material_kind;
  authorization_url : string option;
  user_code : string option;
  verification_uri : string option;
  verification_uri_complete : string option;
  device_code : string option;
  error_code : string option;
  error_message : string option;
  account_prompt : string option;
  account_options : string list;
}

type content = Progress of progress_content | Material of private_material

let content_class_of = function
  | Progress _ -> Shared_room_progress
  | Material _ -> Private_auth_material

let empty_material kind : private_material =
  {
    kind;
    authorization_url = None;
    user_code = None;
    verification_uri = None;
    verification_uri_complete = None;
    device_code = None;
    error_code = None;
    error_message = None;
    account_prompt = None;
    account_options = [];
  }

(* Heuristic: progress detail must not smuggle private auth material. *)
let contains_sub hay needle = String_util.contains hay needle

let contains_oauth_markers lower =
  let needles =
    [
      "login/oauth";
      "oauth/authorize";
      "device/code";
      "client_id=";
      "access_token";
      "code_verifier";
      "device_code";
    ]
  in
  List.exists (fun n -> contains_sub lower n) needles

let is_device_user_code_token t =
  (* GitHub device user codes are typically XXXX-XXXX (A-Z0-9), presented in
     uppercase. Match whole uppercase tokens only so prose like "room-safe" is
     not flagged. *)
  let t = String.trim t in
  if String.length t <> 9 then false
  else if String.exists (fun c -> c >= 'a' && c <= 'z') t then false
  else
    let upper = String.uppercase_ascii t in
    let ok i =
      let c = upper.[i] in
      (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
    in
    ok 0 && ok 1 && ok 2 && ok 3
    && upper.[4] = '-'
    && ok 5 && ok 6 && ok 7 && ok 8

let looks_like_device_user_code s =
  let t = String.trim s in
  if is_device_user_code_token t then true
  else
    (* Split on whitespace and common separators; flag standalone code tokens. *)
    let buf = Buffer.create (String.length t) in
    let tokens = ref [] in
    let flush () =
      if Buffer.length buf > 0 then (
        tokens := Buffer.contents buf :: !tokens;
        Buffer.clear buf)
    in
    String.iter
      (fun c ->
        match c with
        | ' ' | '\t' | '\n' | '\r' | ',' | ';' | ':' | '(' | ')' | '[' | ']'
        | '"' | '\'' ->
            flush ()
        | _ -> Buffer.add_char buf c)
      t;
    flush ();
    List.exists is_device_user_code_token !tokens

let looks_like_secret_progress detail =
  let t = String.trim detail in
  if t = "" then false
  else
    let lower = String.lowercase_ascii t in
    let urlish =
      String.starts_with ~prefix:"https://" lower
      || String.starts_with ~prefix:"http://" lower
    in
    urlish || contains_oauth_markers lower || looks_like_device_user_code t

let make_progress ~phase ?detail () =
  let phase = String.trim phase in
  if phase = "" then Error "progress phase must be non-empty"
  else
    match detail with
    | None -> Ok { phase; detail = None }
    | Some d ->
        let d = String.trim d in
        if d = "" then Ok { phase; detail = None }
        else if looks_like_secret_progress d then
          Error
            "progress detail must be secret-free (no authorization URLs, \
             device codes, or OAuth query material); deliver those privately"
        else Ok { phase; detail = Some d }

let make_authorization_url ~url =
  let url = String.trim url in
  if url = "" then Error "authorization_url must be non-empty"
  else if
    not
      (String.starts_with ~prefix:"https://" (String.lowercase_ascii url)
      || String.starts_with ~prefix:"http://" (String.lowercase_ascii url))
  then Error "authorization_url must be an http(s) URL"
  else
    Ok { (empty_material Authorization_url) with authorization_url = Some url }

let make_device_codes ~user_code ~verification_uri ?verification_uri_complete
    ?device_code () =
  let user_code = String.trim user_code in
  let verification_uri = String.trim verification_uri in
  if user_code = "" then Error "user_code must be non-empty"
  else if verification_uri = "" then Error "verification_uri must be non-empty"
  else
    let verification_uri_complete =
      match verification_uri_complete with
      | None -> None
      | Some s ->
          let t = String.trim s in
          if t = "" then None else Some t
    in
    let device_code =
      match device_code with
      | None -> None
      | Some s ->
          let t = String.trim s in
          if t = "" then None else Some t
    in
    Ok
      {
        (empty_material Device_code) with
        user_code = Some user_code;
        verification_uri = Some verification_uri;
        verification_uri_complete;
        device_code;
      }

let make_callback_error ?code ~message () =
  let message = String.trim message in
  if message = "" then Error "callback error message must be non-empty"
  else
    let error_code =
      match code with
      | None -> None
      | Some c ->
          let t = String.trim c in
          if t = "" then None else Some t
    in
    Ok
      {
        (empty_material Callback_error) with
        error_code;
        error_message = Some message;
      }

let make_account_control ~prompt ?(options = []) () =
  let prompt = String.trim prompt in
  if prompt = "" then Error "account_control prompt must be non-empty"
  else
    let account_options =
      List.filter_map
        (fun s ->
          let t = String.trim s in
          if t = "" then None else Some t)
        options
    in
    Ok
      {
        (empty_material Account_control) with
        account_prompt = Some prompt;
        account_options;
      }

(* -------------------------------------------------------------------------- *)
(* Bound delivery context                                                     *)
(* -------------------------------------------------------------------------- *)

type delivery_context = {
  principal_id : string;
  continuation_handle : string;
  tx_id : string option;
  source : Tx.source option;
  flow_kind : Tx.flow_kind option;
}

let make_delivery_context ~principal_id ~continuation_handle ?tx_id ?source
    ?flow_kind () =
  let principal_id = String.trim principal_id in
  let continuation_handle = String.trim continuation_handle in
  if principal_id = "" then Error "principal_id must be non-empty"
  else if continuation_handle = "" then
    Error "continuation_handle must be non-empty"
  else
    let tx_id =
      match tx_id with
      | None -> None
      | Some id ->
          let t = String.trim id in
          if t = "" then None else Some t
    in
    Ok { principal_id; continuation_handle; tx_id; source; flow_kind }

let context_of_tx (tx : Tx.t) : delivery_context =
  {
    principal_id = tx.principal_id;
    continuation_handle = tx.continuation_handle;
    tx_id = Some tx.id;
    source = Some tx.source;
    flow_kind = Some tx.flow_kind;
  }

(* -------------------------------------------------------------------------- *)
(* Refuse / plan types                                                        *)
(* -------------------------------------------------------------------------- *)

type refuse_reason =
  | No_private_channel
  | Shared_room_blocked_private
  | Invalid_channel of string
  | Invalid_content of string
  | Principal_required

let string_of_refuse_reason = function
  | No_private_channel -> "no_private_channel"
  | Shared_room_blocked_private -> "shared_room_blocked_private"
  | Invalid_channel s -> "invalid_channel:" ^ s
  | Invalid_content s -> "invalid_content:" ^ s
  | Principal_required -> "principal_required"

type refuse_error = {
  reason : refuse_reason;
  message : string;
  room_safe_progress : progress_content option;
}

type private_body = {
  channel : delivery_channel;
  material : private_material;
  rendered : string;
  redacted_summary : string;
}

type room_body = {
  room_id : string;
  progress : progress_content;
  rendered : string;
}

type delivery_plan =
  | Private of {
      private_delivery : private_body;
      companion_room : room_body option;
    }
  | Room_progress of room_body
  | Refused of refuse_error

(* -------------------------------------------------------------------------- *)
(* Rendering                                                                  *)
(* -------------------------------------------------------------------------- *)

let render_room_progress (p : progress_content) =
  match p.detail with
  | None | Some "" -> Printf.sprintf "GitHub authorization: %s" p.phase
  | Some d -> Printf.sprintf "GitHub authorization: %s — %s" p.phase d

let render_private_material (m : private_material) =
  let lines = ref [ "GitHub authorization (private)" ] in
  let add s = lines := !lines @ [ s ] in
  add ("  kind: " ^ string_of_private_material_kind m.kind);
  (match m.authorization_url with
  | Some u -> add ("  authorization_url: " ^ u)
  | None -> ());
  (match m.verification_uri with
  | Some u -> add ("  verification_uri: " ^ u)
  | None -> ());
  (match m.verification_uri_complete with
  | Some u -> add ("  verification_uri_complete: " ^ u)
  | None -> ());
  (match m.user_code with Some c -> add ("  user_code: " ^ c) | None -> ());
  (match m.device_code with
  | Some c -> add ("  device_code: " ^ c)
  | None -> ());
  (match m.error_code with Some c -> add ("  error_code: " ^ c) | None -> ());
  (match m.error_message with
  | Some msg -> add ("  error: " ^ msg)
  | None -> ());
  (match m.account_prompt with
  | Some p -> add ("  account_prompt: " ^ p)
  | None -> ());
  if m.account_options <> [] then
    add ("  account_options: " ^ String.concat ", " m.account_options);
  String.concat "\n" !lines

let url_host_only url =
  let lower = String.lowercase_ascii (String.trim url) in
  let strip_scheme s =
    if String.starts_with ~prefix:"https://" s then
      String.sub s 8 (String.length s - 8)
    else if String.starts_with ~prefix:"http://" s then
      String.sub s 7 (String.length s - 7)
    else s
  in
  let rest = strip_scheme lower in
  match String.split_on_char '/' rest with
  | host :: _ -> (
      match String.split_on_char '?' host with h :: _ -> h | [] -> host)
  | [] -> "(url)"

let redacted_room_summary ~context ~content =
  let phase =
    match content with
    | Progress p -> p.phase
    | Material m ->
        "private:"
        ^ string_of_private_material_kind m.kind
        ^ " (not shown in room)"
  in
  let src =
    match context.source with
    | None -> "(none)"
    | Some s -> Tx.string_of_source s
  in
  String.concat "\n"
    [
      "GitHub auth delivery (shared room summary)";
      Printf.sprintf "  principal: %s" context.principal_id;
      Printf.sprintf "  continuation_handle: %s" context.continuation_handle;
      (match context.tx_id with
      | None -> "  tx_id: (none)"
      | Some id -> "  tx_id: " ^ id);
      Printf.sprintf "  source: %s" src;
      Printf.sprintf "  content: %s" phase;
      "  (authorization URLs, device codes, callback secrets, and account \
       controls are never included)";
    ]

let channel_kind_label = function
  | Private_connector_dm { connector; _ } ->
      "private_connector_dm:" ^ P.string_of_connector connector
  | Principal_browser_continuation _ -> "principal_browser_continuation"
  | Initiating_cli _ -> "initiating_cli"
  | Absent -> "absent"

let channel_handle = function
  | Private_connector_dm { handle_id; _ }
  | Principal_browser_continuation { handle_id }
  | Initiating_cli { handle_id } ->
      handle_id
  | Absent -> ""

let redacted_private_summary ~context ~channel ~content =
  let material_bits =
    match content with
    | Progress p -> [ "progress:" ^ p.phase ]
    | Material m ->
        let flag name = function
          | None -> name ^ "=absent"
          | Some _ -> name ^ "=present"
        in
        [
          "kind=" ^ string_of_private_material_kind m.kind;
          flag "authorization_url" m.authorization_url;
          (match m.authorization_url with
          | Some u -> "authorization_url_host=" ^ url_host_only u
          | None -> "authorization_url_host=n/a");
          flag "user_code" m.user_code;
          flag "verification_uri" m.verification_uri;
          flag "device_code" m.device_code;
          flag "error_message" m.error_message;
          flag "account_prompt" m.account_prompt;
          Printf.sprintf "account_options=%d" (List.length m.account_options);
        ]
  in
  String.concat "\n"
    ([
       "GitHub auth delivery (private redacted)";
       Printf.sprintf "  principal: %s" context.principal_id;
       Printf.sprintf "  continuation_handle: %s" context.continuation_handle;
       (match context.tx_id with
       | None -> "  tx_id: (none)"
       | Some id -> "  tx_id: " ^ id);
       Printf.sprintf "  channel: %s" (channel_kind_label channel);
       (match channel with
       | Absent -> "  handle: (none)"
       | _ -> "  handle: " ^ channel_handle channel);
     ]
    @ List.map (fun s -> "  " ^ s) material_bits)

let room_message_is_safe text = not (looks_like_secret_progress text)

let contains_private_secrets (m : private_material) text =
  let check = function
    | None | Some "" -> false
    | Some secret -> contains_sub text secret
  in
  check m.authorization_url || check m.user_code || check m.device_code
  || check m.verification_uri_complete
(* verification_uri alone may be the public github.com/login/device page. *)

(* -------------------------------------------------------------------------- *)
(* Routing                                                                    *)
(* -------------------------------------------------------------------------- *)

let resolve_room_id ~context ~shared_room_id =
  match shared_room_id with
  | Some id ->
      let t = String.trim id in
      if t = "" then None else Some t
  | None -> (
      match context.source with
      | Some (Tx.Room id) -> Some id
      | Some (Tx.Session _) | None -> None)

let refused ~reason ~message ?room_progress () : delivery_plan =
  Refused { reason; message; room_safe_progress = room_progress }

let companion_progress_for_private (m : private_material) : progress_content =
  let phase =
    match m.kind with
    | Authorization_url -> "awaiting_authorization"
    | Device_code -> "awaiting_device_authorization"
    | Callback_error -> "authorization_error"
    | Account_control -> "awaiting_account_selection"
  in
  {
    phase;
    detail =
      Some
        "Continuation delivered privately to the authorizing Principal. Shared \
         Rooms do not receive authorization URLs, device codes, or account \
         controls.";
  }

let refuse_progress_no_channel () : progress_content =
  {
    phase = "refused_no_private_channel";
    detail =
      Some
        "GitHub user authorization requires a private delivery channel \
         (Connector DM, Principal browser continuation, or initiating CLI). No \
         authorization URL, device code, or account control was posted to this \
         Room.";
  }

let assert_private_channel (channel : delivery_channel) :
    (delivery_channel, refuse_error) result =
  match validate_delivery_channel channel with
  | Error e ->
      Error
        {
          reason = Invalid_channel e;
          message = e;
          room_safe_progress = Some (refuse_progress_no_channel ());
        }
  | Ok () -> (
      match channel with
      | Absent ->
          Error
            {
              reason = No_private_channel;
              message =
                "No private delivery channel is available. Authorization URLs, \
                 device codes, callback errors, and account controls cannot be \
                 sent to shared Rooms. Provide an authenticated private \
                 Connector DM, Principal-bound browser continuation, or \
                 initiating CLI, then retry.";
              room_safe_progress = Some (refuse_progress_no_channel ());
            }
      | _ -> Ok channel)

let require_private_for_material content channel =
  match content with
  | Progress _ -> Ok ()
  | Material _ -> (
      match assert_private_channel channel with
      | Ok _ -> Ok ()
      | Error e -> Error e)

let make_room_body ~room_id (progress : progress_content) : room_body =
  let rendered = render_room_progress progress in
  { room_id; progress; rendered }

let route_delivery ~context ~channel ~content ?shared_room_id () =
  if String.trim context.principal_id = "" then
    refused ~reason:Principal_required
      ~message:"delivery requires a non-empty principal_id" ()
  else
    let room_id = resolve_room_id ~context ~shared_room_id in
    match content with
    | Progress p ->
        let room_id = Option.value room_id ~default:"" in
        Room_progress (make_room_body ~room_id p)
    | Material m -> (
        match assert_private_channel channel with
        | Error e ->
            let room_safe =
              match room_id with
              | None -> e.room_safe_progress
              | Some _ ->
                  Some
                    (Option.value e.room_safe_progress
                       ~default:(refuse_progress_no_channel ()))
            in
            Refused { e with room_safe_progress = room_safe }
        | Ok private_ch -> (
            let rendered = render_private_material m in
            let redacted_summary =
              redacted_private_summary ~context ~channel:private_ch
                ~content:(Material m)
            in
            let private_delivery =
              { channel = private_ch; material = m; rendered; redacted_summary }
            in
            let companion_room =
              match room_id with
              | None -> None
              | Some rid ->
                  let progress = companion_progress_for_private m in
                  Some (make_room_body ~room_id:rid progress)
            in
            (* Defense in depth: companion room text must never embed secrets. *)
            match companion_room with
            | Some rb when contains_private_secrets m rb.rendered ->
                refused ~reason:Shared_room_blocked_private
                  ~message:
                    "Refused to post private authorization material to a \
                     shared Room; private delivery was not completed."
                  ~room_progress:(refuse_progress_no_channel ())
                  ()
            | _ -> Private { private_delivery; companion_room }))

let deliver ~context ~channel ~content ?shared_room_id () =
  match route_delivery ~context ~channel ~content ?shared_room_id () with
  | Refused e -> Error e
  | plan -> Ok plan

let plan_redacted_summary = function
  | Room_progress rb ->
      Printf.sprintf "room_progress room=%s phase=%s" rb.room_id
        rb.progress.phase
  | Private { private_delivery; companion_room } ->
      let companion =
        match companion_room with
        | None -> "companion=none"
        | Some rb ->
            "companion_room=" ^ rb.room_id ^ " phase=" ^ rb.progress.phase
      in
      Printf.sprintf "private channel=%s kind=%s %s"
        (channel_kind_label private_delivery.channel)
        (string_of_private_material_kind private_delivery.material.kind)
        companion
  | Refused e ->
      Printf.sprintf "refused reason=%s msg=%s"
        (string_of_refuse_reason e.reason)
        e.message

(* -------------------------------------------------------------------------- *)
(* JSON                                                                       *)
(* -------------------------------------------------------------------------- *)

let delivery_channel_to_json = function
  | Private_connector_dm { connector; handle_id } ->
      `Assoc
        [
          ("kind", `String "private_connector_dm");
          ("connector", `String (P.string_of_connector connector));
          ("handle_id", `String handle_id);
        ]
  | Principal_browser_continuation { handle_id } ->
      `Assoc
        [
          ("kind", `String "principal_browser_continuation");
          ("handle_id", `String handle_id);
        ]
  | Initiating_cli { handle_id } ->
      `Assoc
        [ ("kind", `String "initiating_cli"); ("handle_id", `String handle_id) ]
  | Absent -> `Assoc [ ("kind", `String "absent") ]

let progress_to_json (p : progress_content) =
  `Assoc
    [
      ("phase", `String p.phase);
      ("detail", match p.detail with None -> `Null | Some d -> `String d);
    ]

let private_material_to_json_redacted (m : private_material) =
  let present = function None -> `Bool false | Some _ -> `Bool true in
  `Assoc
    [
      ("kind", `String (string_of_private_material_kind m.kind));
      ("authorization_url_present", present m.authorization_url);
      ( "authorization_url_host",
        match m.authorization_url with
        | None -> `Null
        | Some u -> `String (url_host_only u) );
      ("user_code_present", present m.user_code);
      ("verification_uri_present", present m.verification_uri);
      ("verification_uri_complete_present", present m.verification_uri_complete);
      ("device_code_present", present m.device_code);
      ( "error_code",
        match m.error_code with None -> `Null | Some c -> `String c );
      ("error_message_present", present m.error_message);
      ("account_prompt_present", present m.account_prompt);
      ("account_options_count", `Int (List.length m.account_options));
    ]

let delivery_plan_to_json_redacted = function
  | Room_progress rb ->
      `Assoc
        [
          ("outcome", `String "room_progress");
          ("room_id", `String rb.room_id);
          ("progress", progress_to_json rb.progress);
        ]
  | Private { private_delivery; companion_room } ->
      let companion =
        match companion_room with
        | None -> `Null
        | Some rb ->
            `Assoc
              [
                ("room_id", `String rb.room_id);
                ("progress", progress_to_json rb.progress);
              ]
      in
      `Assoc
        [
          ("outcome", `String "private");
          ("channel", delivery_channel_to_json private_delivery.channel);
          ( "material",
            private_material_to_json_redacted private_delivery.material );
          ("companion_room", companion);
        ]
  | Refused e ->
      `Assoc
        [
          ("outcome", `String "refused");
          ("reason", `String (string_of_refuse_reason e.reason));
          ("message", `String e.message);
          ( "room_safe_progress",
            match e.room_safe_progress with
            | None -> `Null
            | Some p -> progress_to_json p );
        ]
