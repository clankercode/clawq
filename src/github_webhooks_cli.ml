(** [clawq github webhooks clear-queue] trims unsent GitHub notification texts.

    Persisted webhook payloads are left alone. Omitting the count leaves the
    [default_keep] newest queued messages. [0] drops the whole unsent queue. *)

let default_keep = 55

let usage =
  "Usage: clawq github webhooks clear-queue [N]\n\n\
   Drop GitHub notification messages that have been queued but not sent yet. \
   Persisted webhook payloads are not touched. N is how many of the newest \
   queued messages to leave.\n\
   Default: 55. Use 0 to drop the whole unsent queue.\n"

let parse_keep = function
  | [] -> Ok default_keep
  | [ "--keep"; n ] | [ n ] -> (
      try
        let keep = int_of_string n in
        if keep < 0 then Error "keep must be >= 0" else Ok keep
      with _ -> Error ("invalid keep count: " ^ n))
  | "--keep" :: n :: rest when String.length n > 0 && n.[0] <> '-' -> (
      try
        let keep = int_of_string n in
        if keep < 0 then Error "keep must be >= 0"
        else if rest = [] then Ok keep
        else Error "unexpected arguments"
      with _ -> Error ("invalid keep count: " ^ n))
  | _ -> Error "unexpected arguments"

type clear_report = { keep : int; deleted : int; remaining : int }

let clear ~db ~keep () =
  match Github_notify_batch.trim ~db ~keep with
  | Error e -> Error e
  | Ok notify ->
      Ok { keep; deleted = notify.deleted; remaining = notify.remaining }

let format_report (r : clear_report) =
  Printf.sprintf
    "Unsent notify queue: kept %d, deleted %d, remaining %d.\n\
     Persisted webhook payloads were not changed.\n"
    r.keep r.deleted r.remaining

let cmd ~db args =
  match args with
  | "webhooks" :: "clear-queue" :: rest -> (
      match parse_keep rest with
      | Error e -> "Error: " ^ e ^ "\n" ^ usage
      | Ok keep -> (
          match clear ~db ~keep () with
          | Error e -> "Error: " ^ e
          | Ok report -> format_report report))
  | "webhooks" :: "clear" :: _ ->
      "Error: use `clawq github webhooks clear-queue [N]`.\n\
       That trims unsent notification messages only. Persisted webhook \
       payloads are not deleted.\n"
  | "webhooks" :: _ -> usage
  | _ -> usage
