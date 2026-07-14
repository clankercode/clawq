type signal =
  | ConsecutiveErrors of { count : int; tool : string; last_error : string }
  | RepeatedToolCall of { tool : string; args : string; count : int }
  | SameErrorString of { msg : string; count : int }
  | NearMaxIters of { current : int; max_iters : int }

type result = Clear | Suspicious of signal list | Definite of signal list

val check :
  history:Provider.message list -> iteration:int -> max_iters:int -> result
(** [check ~history ~iteration ~max_iters] runs heuristic stuck detection on the
    agent history. [history] is newest-first. Returns a stuck assessment based
    on error patterns, repeated tool calls, and iteration proximity to the
    limit. No I/O or LLM calls are made. *)

val signals_to_string : signal list -> string
(** [signals_to_string signals] produces a human-readable summary of the signals
    suitable for injection into LLM context. *)

val is_configuration_error : string -> bool
(** [true] when a tool error string indicates a non-retryable room/setup
    misconfiguration (missing profile binding, empty GitHub room access, missing
    CLAWQ_PRINCIPAL_ID, etc.). *)

val has_configuration_error : signal list -> bool
(** [true] when any stuck signal is a configuration-class failure. *)

val configuration_abort_message : signal list -> string
(** User-facing abort text for configuration-error loops (B778). *)
