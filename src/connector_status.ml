(** Per-channel phase-to-emoji mapping *)

module type S = sig
  val phase_emoji : Status_phase.t -> string
  val status_parse_mode : string
end

module Telegram : S = struct
  let phase_emoji = function
    | Status_phase.Received -> "\xF0\x9F\x91\x80" (* 👀 *)
    | Processing -> "\xE2\x9A\xA1" (* ⚡ *)
    | Completed -> "\xF0\x9F\x91\x8D" (* 👍 *)
    | Failed -> "\xF0\x9F\x92\x94" (* 💔 *)

  let status_parse_mode = "HTML"
end

module Discord : S = struct
  let phase_emoji = function
    | Status_phase.Received -> "\xe2\x8f\xb3" (* ⏳ *)
    | Processing -> "\xe2\x9a\x99\xef\xb8\x8f" (* ⚙️ *)
    | Completed -> "\xE2\x9C\x85" (* ✅ *)
    | Failed -> "\xE2\x9A\xA0\xEF\xB8\x8F" (* ⚠️ *)

  let status_parse_mode = "Markdown"
end

module Slack : S = struct
  let phase_emoji = function
    | Status_phase.Received -> "hourglass_flowing_sand"
    | Processing -> "gear"
    | Completed -> "white_check_mark"
    | Failed -> "warning"

  let status_parse_mode = "mrkdwn"
end
