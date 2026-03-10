type session_stats = {
  session_key : string;
  turn_count : int;
  total_tool_calls : int;
  error_count : int;
  session_age_s : float;
}

type verdict =
  | Ok
  | Stuck of { reason : string; confidence : [ `High | `Medium ] }
  | Error of string  (** LLM call failed *)

val observer_config_for : config:Runtime_config.t -> Runtime_config.t
(** Build a config override that routes requests to the observer model. Clears
    default_provider and sets primary_model to config.observer.model. *)

val check_stuck :
  config:Runtime_config.t ->
  history:Provider.message list ->
  stats:session_stats ->
  unit ->
  verdict Lwt.t
(** Two-round protocol: Round 1 with last round1_window messages (newest-first).
    If NEED_MORE, Round 2 with last round2_window messages + tool histogram. *)

val build_tool_histogram : Provider.message list -> string
(** Build a human-readable tool call histogram from message history.
    Args/filepaths shown verbatim. Format: file_write(path="/foo/bar.txt") × 4
    errors (3): "Error: No such file or directory" success (1) *)

val check_thinking_excerpt :
  config:Runtime_config.t ->
  excerpt:string ->
  unit ->
  [ `Sane | `Looping of string ] Lwt.t
(** Check if a thinking token excerpt appears to be looping/incoherent. Used
    from streaming path when thinking token count exceeds threshold. *)
