(* Repeated-identical-line log suppression.

   Under a SQLite BUSY storm the same underlying error fires once per delivery,
   which flooded daemon.log with hundreds of near-identical ERROR lines. The
   dedup key is the error text (not the whole log line), so per-delivery
   prefixes do not defeat the window; the first line in a window is logged and
   the next one reports how many were swallowed. *)

type entry = { mutable suppressed : int; mutable last_emit : float }

type t = {
  entries : (string, entry) Hashtbl.t;
  window_s : float;
  max_keys : int;
}

let default_window_s = 60.0
let default_max_keys = 1024

let create ?(window_s = default_window_s) ?(max_keys = default_max_keys) () =
  { entries = Hashtbl.create 64; window_s; max_keys }

(* Process-wide table so callers can log without threading state around. *)
let global = create ()
let now () = Unix.gettimeofday ()

(** [emit t ~now ~key message] returns [Some line] when the line should be
    logged: the first occurrence of [key] in a window, or the first one after
    the window elapsed (annotated with the suppressed count). Returns [None]
    while [key] is still inside its window. *)
let emit t ~now ~key message =
  match Hashtbl.find_opt t.entries key with
  | Some entry when now -. entry.last_emit < t.window_s ->
      entry.suppressed <- entry.suppressed + 1;
      None
  | Some entry ->
      let suppressed = entry.suppressed in
      entry.suppressed <- 0;
      entry.last_emit <- now;
      Some
        (if suppressed = 0 then message
         else
           Printf.sprintf "%s (suppressed %d in %.0fs)" message suppressed
             t.window_s)
  | None ->
      (* Bound the table so a stream of distinct errors cannot grow it
         forever; dropping it only costs one extra emitted line. *)
      if Hashtbl.length t.entries >= t.max_keys then Hashtbl.reset t.entries;
      Hashtbl.replace t.entries key { suppressed = 0; last_emit = now };
      Some message

(** [err ~key message] logs [message] at error level, deduplicated on [key]. *)
let err ~key message =
  match emit global ~now:(now ()) ~key message with
  | Some line -> Logs.err (fun m -> m "%s" line)
  | None -> ()

(** [warn ~key message] logs [message] at warn level, deduplicated on [key]. *)
let warn ~key message =
  match emit global ~now:(now ()) ~key message with
  | Some line -> Logs.warn (fun m -> m "%s" line)
  | None -> ()
