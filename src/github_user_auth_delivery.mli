(** Deliver GitHub user-authorization continuations privately (P21.M2.E1.T003).

    Shared Rooms receive only neutral progress. Authorization URLs, device
    codes, callback errors, and account-selection controls are delivered only
    through an authenticated private Connector response, a Principal-bound
    browser continuation, or the initiating CLI. When no private channel is
    available, delivery refuses safely with a typed error and never leaks
    secrets into Room-visible messages.

    This module is pure routing + redaction. It does not perform network I/O;
    callers (web/device flows, Connector adapters, CLI) execute the planned
    deliveries.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

val protocol_version : int
(** Delivery protocol schema version; starts at 1. *)

(** {1 Content classification} *)

(** Whether a piece of authorization content may appear in a shared Room. *)
type content_class =
  | Shared_room_progress
      (** Neutral status only — safe for multi-participant Rooms. *)
  | Private_auth_material
      (** Authorization URL, device/user code, callback error detail, or account
          controls — never Room-visible. *)

val string_of_content_class : content_class -> string
val content_class_of_string : string -> (content_class, string) result

(** Kinds of private authorization material. *)
type private_material_kind =
  | Authorization_url  (** Browser OAuth authorization URL. *)
  | Device_code
      (** Device-flow user_code / verification URI (and related private codes).
      *)
  | Callback_error  (** OAuth callback or exchange error detail. *)
  | Account_control  (** Account selection / relink / unlink controls. *)

val string_of_private_material_kind : private_material_kind -> string

val private_material_kind_of_string :
  string -> (private_material_kind, string) result

val classify_material_kind : private_material_kind -> content_class
(** Always [Private_auth_material]. *)

val classify_content :
  [ `Progress | `Material of private_material_kind ] -> content_class
(** Map a content tag to its class. *)

(** {1 Delivery channels} *)

(** Where private material may be sent. [Absent] means no private path. *)
type delivery_channel =
  | Private_connector_dm of {
      connector : Principal_identity.connector;
      handle_id : string;
          (** Opaque private-delivery handle (DM alias / conversation id). Never
              a token, code, or URL secret. *)
    }  (** Authenticated private Connector response (e.g. Teams/Slack DM). *)
  | Principal_browser_continuation of { handle_id : string }
      (** Principal-bound browser continuation (private web surface). *)
  | Initiating_cli of { handle_id : string }
      (** Deliver to the CLI that started the flow. *)
  | Absent  (** No private channel; private material must refuse safely. *)

val string_of_delivery_channel : delivery_channel -> string

val delivery_channel_is_private : delivery_channel -> bool
(** [false] only for [Absent]. *)

val validate_delivery_channel : delivery_channel -> (unit, string) result
(** Reject empty handles on concrete channels. [Absent] is always valid. *)

val make_private_connector_dm :
  connector:Principal_identity.connector ->
  handle_id:string ->
  (delivery_channel, string) result

val make_principal_browser_continuation :
  handle_id:string -> (delivery_channel, string) result

val make_initiating_cli : handle_id:string -> (delivery_channel, string) result

(** {1 Content payloads} *)

type progress_content = {
  phase : string;
      (** Short phase id: e.g. [awaiting_authorization], [completed], [refused].
      *)
  detail : string option;
      (** Optional human detail. Must already be secret-free; [make_progress]
          does not accept URLs/codes. *)
}
(** Neutral progress safe for shared Rooms. *)

type private_material = {
  kind : private_material_kind;
  authorization_url : string option;
  user_code : string option;
  verification_uri : string option;
  verification_uri_complete : string option;
  device_code : string option;
      (** Server device_code when needed for private tooling; never Room-bound.
      *)
  error_code : string option;
  error_message : string option;
  account_prompt : string option;
  account_options : string list;
}
(** Private authorization material. Fields are optional per [kind]; unused
    fields should be [None] / [[]]. *)

type content = Progress of progress_content | Material of private_material

val content_class_of : content -> content_class

val make_progress :
  phase:string -> ?detail:string -> unit -> (progress_content, string) result
(** Require non-empty [phase]. Reject [detail] that looks like auth material
    (URLs with query secrets, device-code-shaped strings). *)

val make_authorization_url : url:string -> (private_material, string) result

val make_device_codes :
  user_code:string ->
  verification_uri:string ->
  ?verification_uri_complete:string ->
  ?device_code:string ->
  unit ->
  (private_material, string) result

val make_callback_error :
  ?code:string -> message:string -> unit -> (private_material, string) result

val make_account_control :
  prompt:string ->
  ?options:string list ->
  unit ->
  (private_material, string) result

(** {1 Bound delivery context} *)

type delivery_context = {
  principal_id : string;
  continuation_handle : string;
      (** Opaque handle from [Github_user_auth_tx]; never embeds secrets. *)
  tx_id : string option;
  source : Github_user_auth_tx.source option;
      (** Originating Room/Session for optional companion progress. *)
  flow_kind : Github_user_auth_tx.flow_kind option;
}
(** Principal-bound correlation for routing. Does not carry credentials. *)

val make_delivery_context :
  principal_id:string ->
  continuation_handle:string ->
  ?tx_id:string ->
  ?source:Github_user_auth_tx.source ->
  ?flow_kind:Github_user_auth_tx.flow_kind ->
  unit ->
  (delivery_context, string) result

val context_of_tx : Github_user_auth_tx.t -> delivery_context
(** Project a persisted authorization transaction into a delivery context. *)

(** {1 Routing and delivery plans} *)

type refuse_reason =
  | No_private_channel
      (** Private material requested but channel is [Absent] or invalid. *)
  | Shared_room_blocked_private
      (** Attempted to place private material on a shared Room path. *)
  | Invalid_channel of string
  | Invalid_content of string
  | Principal_required

val string_of_refuse_reason : refuse_reason -> string

type refuse_error = {
  reason : refuse_reason;
  message : string;  (** Actionable operator/user message (secret-free). *)
  room_safe_progress : progress_content option;
      (** Optional companion Room progress when a source Room exists. Never
          contains secrets. *)
}
(** Safe refusal: typed reason, no secret leakage. *)

type private_body = {
  channel : delivery_channel;  (** Concrete private channel (not [Absent]). *)
  material : private_material;
  rendered : string;  (** Full private body for the channel. *)
  redacted_summary : string;  (** Safe for logs / audit. *)
}

type room_body = {
  room_id : string;
  progress : progress_content;
  rendered : string;  (** Neutral Room-visible text. *)
}

type delivery_plan =
  | Private of {
      private_delivery : private_body;
      companion_room : room_body option;
          (** Optional neutral progress for the source Room. *)
    }
  | Room_progress of room_body
      (** Neutral progress only — no private material. *)
  | Refused of refuse_error

val route_delivery :
  context:delivery_context ->
  channel:delivery_channel ->
  content:content ->
  ?shared_room_id:string ->
  unit ->
  delivery_plan
(** Pure routing.

    - [Progress] → [Room_progress] when a room is known (source Room or
      [shared_room_id]); otherwise still [Room_progress] with empty room id only
      if no room — prefer providing a room. Progress never requires a private
      channel.
    - [Material] with a private [channel] → [Private], plus optional neutral
      companion Room progress ("authorization continuation delivered
      privately").
    - [Material] with [Absent] or invalid channel → [Refused] with
      [No_private_channel] (or [Invalid_channel]) and a secret-free Room
      progress when a Room is known.

    Never returns private fields on [Room_progress] or [refuse_error]
    [room_safe_progress]. *)

val deliver :
  context:delivery_context ->
  channel:delivery_channel ->
  content:content ->
  ?shared_room_id:string ->
  unit ->
  (delivery_plan, refuse_error) result
(** Like [route_delivery], but maps [Refused] to [Error] for callers that want
    result-style handling. Successful plans are [Ok]. *)

val assert_private_channel :
  delivery_channel -> (delivery_channel, refuse_error) result
(** Fail closed when channel is [Absent] or invalid. *)

val require_private_for_material :
  content -> delivery_channel -> (unit, refuse_error) result
(** [Ok ()] for progress always; for material, require a valid private channel.
*)

(** {1 Redaction and rendering} *)

val render_room_progress : progress_content -> string
(** Human text for shared Rooms — phase + optional detail. *)

val render_private_material : private_material -> string
(** Full private body including URL/codes/errors/controls. *)

val redacted_room_summary :
  context:delivery_context -> content:content -> string
(** Room-safe summary: never includes URL, codes, or raw error detail that may
    carry tokens. *)

val redacted_private_summary :
  context:delivery_context ->
  channel:delivery_channel ->
  content:content ->
  string
(** Private-path summary for logs/audit: channel kind + material kind + handles;
    secrets redacted (URL host only, codes as present/absent). *)

val plan_redacted_summary : delivery_plan -> string
(** Export-safe summary of a plan. *)

val room_message_is_safe : string -> bool
(** Heuristic: reject messages that embed authorization URLs with query
    material, device codes, or user codes. *)

val contains_private_secrets : private_material -> string -> bool
(** [true] when [text] embeds any non-empty secret field from [material]. *)

(** {1 JSON (secret-aware: export forms redact)} *)

val delivery_channel_to_json : delivery_channel -> Yojson.Safe.t
(** Handle ids only; never tokens. *)

val progress_to_json : progress_content -> Yojson.Safe.t

val private_material_to_json_redacted : private_material -> Yojson.Safe.t
(** Redacted: kinds and presence flags only (no URL/code/error bodies). *)

val delivery_plan_to_json_redacted : delivery_plan -> Yojson.Safe.t
(** Fully redacted export for audit/persistence. *)
