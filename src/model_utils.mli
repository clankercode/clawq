(* model_utils.mli — Shared model-name utilities *)

val strip_date_suffix : string -> string
(** [strip_date_suffix s] removes a trailing [-YYYYMMDD] date suffix from a
    model name string when the suffix is exactly 8 digits. Returns [s] unchanged
    otherwise. *)
