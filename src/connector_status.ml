(** Per-channel phase-to-emoji mapping *)

module type S = sig
  val phase_emoji : Status_phase.t -> string
  val interrupt_ack_emoji : string
  val status_parse_mode : string
end

let is_interrupt_ack_message message =
  String.length message > 0 && message.[0] = '!'

module Telegram : S = struct
  let phase_emoji = function
    | Status_phase.Received -> "\xF0\x9F\x91\x80" (* 👀 *)
    | Processing -> "\xE2\x9A\xA1" (* ⚡ *)
    | Completed -> "\xF0\x9F\x91\x8D" (* 👍 *)
    | Failed -> "\xF0\x9F\x92\x94" (* 💔 *)

  let interrupt_ack_emoji = "\xF0\x9F\xAB\xA1" (* 🫡 *)
  let status_parse_mode = "HTML"
end

module Discord : S = struct
  let phase_emoji = function
    | Status_phase.Received -> "\xe2\x8f\xb3" (* ⏳ *)
    | Processing -> "\xe2\x9a\x99\xef\xb8\x8f" (* ⚙️ *)
    | Completed -> "\xE2\x9C\x85" (* ✅ *)
    | Failed -> "\xE2\x9A\xA0\xEF\xB8\x8F" (* ⚠️ *)

  let interrupt_ack_emoji = "\xF0\x9F\xAB\xA1" (* 🫡 *)
  let status_parse_mode = "Markdown"
end

module Slack : S = struct
  let phase_emoji = function
    | Status_phase.Received -> "hourglass_flowing_sand"
    | Processing -> "gear"
    | Completed -> "white_check_mark"
    | Failed -> "warning"

  let interrupt_ack_emoji = "saluting_face"
  let status_parse_mode = "mrkdwn"
end
