module type S = sig
  val name : string
  val start : config:Runtime_config.t -> session_manager:Session.t -> unit Lwt.t
end
