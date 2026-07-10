(* A startable messaging channel, as the daemon sees it.

   Previously this module was a phantom seam: a `module type S` that no channel
   implemented (the real start functions all took extra per-channel arguments
   like ~db / ~message_limiter / ~event_limiter, so none could conform). It is
   now a concrete channel-spec value: a display name, an enabled predicate over
   config, and a supervised start thunk that closes over whatever that channel
   needs. daemon_channels builds a [t list] and folds over it, so the
   async + catch + log supervision policy lives in exactly one place. *)
type t = {
  name : string;
  enabled : Runtime_config.t -> bool;
  start : unit -> unit Lwt.t;
}
