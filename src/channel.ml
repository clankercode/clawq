(* Channel module type for connectors.
   Command registration is connector-specific:
   - Telegram: setMyCommands API call at startup (sorted by priority)
   - Teams: static manifest (clawq manifest teams) + runtime /menu Adaptive Card
   - Discord/Slack: future (application commands / app manifest) *)
module type S = sig
  val name : string
  val start : config:Runtime_config.t -> session_manager:Session.t -> unit Lwt.t
end
