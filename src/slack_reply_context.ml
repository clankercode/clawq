(** Presentation metadata only; this carries no destination or tool authority.
    Keep this module below Prompt_builder and independent of config/Session. *)
type conversation = Channel | Direct | Group_direct | Unknown

type t = { connector : string; conversation : conversation }

let key : t Lwt.key = Lwt.new_key ()
let with_context context f = Lwt.with_value key (Some context) f
let current_context () = Lwt.get key
