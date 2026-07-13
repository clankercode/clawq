(** Telegram Bot API ingress: authenticity boundary and canonical human identity
    derivation (P21.M1.E1.T008).

    Production trust boundary for long-poll is successful Bot API [getUpdates]
    over HTTPS with the configured bot token. The bot token / webhook
    [secret_token] are used only to establish update authenticity — they never
    form human identity.

    Canonical human identity is the configured bot/account namespace plus the
    immutable Telegram user id ([from.id]). Display names, usernames, chat
    titles, and room/session ids are non-identity context only.

    Rejects bots ([is_bot]), missing/malformed sender identity, stale/replayed
    [update_id]s (monotonic offset per bot namespace), and display-only
    evidence. Webhook ingress separately requires constant-time [secret_token]
    verification before the same identity derivation applies.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md *)

(** {1 Canonical human identity} *)

type human_identity = {
  bot_namespace : string;
      (** Configured bot/account namespace (typically numeric bot id). Equal
          user ids under different bots remain distinct actors. *)
  user_id : string;  (** Immutable Telegram [from.id] as decimal string. *)
}

val human_identity_key : human_identity -> string
(** Canonical key: [bot:<namespace>:user:<user_id>]. Display fields omitted. *)

(** {1 Non-identity chat context} *)

type chat_kind = Private | Group | Supergroup | Channel | Unknown of string

type chat_context = { chat_id : string; kind : chat_kind }
(** Routing/display context only — never part of [human_identity]. *)

(** {1 Derivation outcome} *)

type outcome =
  | Human of {
      identity : human_identity;
      display_name : string option;
      username : string option;
      chat : chat_context option;
      update_id : int;
    }
  | Bot_rejected of string
  | Invalid of string
  | Stale_or_replay of { update_id : int; last_offset : int; message : string }

(** {1 Monotonic update_id offset store (per bot namespace)} *)

type offset_store
(** Tracks the highest processed [update_id] per bot namespace for monotonic
    advance and restart-safe replay rejection. *)

val create_offset_store : ?initial:(string * int) list -> unit -> offset_store
(** Create an offset store. Optional [initial] pairs are
    [bot_namespace, last_update_id] snapshots (e.g. loaded after restart). *)

val clear_offset_store : offset_store -> unit
val last_offset : offset_store -> bot_namespace:string -> int option

val advance_offset :
  offset_store -> bot_namespace:string -> update_id:int -> unit
(** Advance only when [update_id] is strictly greater than the stored value. *)

val offset_store_to_list : offset_store -> (string * int) list
(** Snapshot for durable persistence across restart. *)

val mark_seen :
  offset_store -> bot_namespace:string -> update_id:int -> [ `New | `Replay ]
(** Record [update_id] for dedupe. Returns [`Replay] when the id is not above
    the current watermark for that bot namespace. *)

(** {1 Bot token / secret helpers (authenticity only)} *)

val bot_namespace_of_token : string -> string option
(** Extract the numeric bot id prefix from a Telegram bot token
    ([<bot_id>:<secret>]). Used only as the account namespace for identity
    scoping — the secret material is never part of the identity key. *)

val verify_webhook_secret_token :
  expected:string -> provided:string option -> bool
(** Constant-time check of Telegram webhook header
    [X-Telegram-Bot-Api-Secret-Token] against the configured [secret_token].
    Empty expected secrets fail closed. *)

(** {1 Long-poll derivation} *)

val verify_and_derive_long_poll :
  ?offset_store:offset_store ->
  ?advance:bool ->
  bot_namespace:string ->
  update_json:Yojson.Safe.t ->
  unit ->
  outcome
(** Derive identity from a Bot API update that was already obtained via
    bot-token-authenticated [getUpdates] (HTTPS long-poll trust boundary).

    When [offset_store] is supplied, rejects [update_id] not strictly above the
    stored watermark ([Stale_or_replay]). When [advance] is true (default when
    an offset store is given), advances the watermark for [Human] and
    [Bot_rejected] outcomes so poison/bot traffic still drains. *)

(** {1 Webhook derivation} *)

val verify_and_derive_webhook :
  ?offset_store:offset_store ->
  ?advance:bool ->
  bot_namespace:string ->
  expected_secret_token:string ->
  provided_secret_token:string option ->
  update_json:Yojson.Safe.t ->
  unit ->
  outcome
(** Verify webhook [secret_token] first (authenticity only), then apply the same
    identity derivation as long-poll. Fail closed on missing/mismatched secret.
    Does not use the bot token secret as identity. *)
