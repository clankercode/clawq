(* model_utils.ml — Shared model-name utilities *)

(** [strip_date_suffix s] removes a trailing [-YYYYMMDD] date suffix from a
    model name string when the suffix is exactly 8 digits. Returns [s] unchanged
    otherwise. *)
let strip_date_suffix s =
  let len = String.length s in
  if len >= 9 && s.[len - 9] = '-' then
    let suffix = String.sub s (len - 8) 8 in
    let all_digits =
      try
        String.iter (fun c -> if c < '0' || c > '9' then raise Exit) suffix;
        true
      with Exit -> false
    in
    if all_digits then String.sub s 0 (len - 9) else s
  else s
