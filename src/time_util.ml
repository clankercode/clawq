(* Shared timestamp formatting. Consolidates the hand-rolled
   [Printf.sprintf "%04d-%02d-%02dT..."] variants that were duplicated (with
   subtly divergent shapes) across ~15 modules. Depends only on [Unix].

   Each function preserves the exact output shape and timezone of the call
   sites it replaces:
   - [iso8601_utc]         "YYYY-MM-DDTHH:MM:SSZ"          (gmtime)
   - [iso8601_utc_micros]  "YYYY-MM-DDTHH:MM:SS.ffffffZ"   (gmtime)
   - [iso8601_utc_millis]  "YYYY-MM-DDTHH:MM:SS.fffZ"      (gmtime)
   - [sql_datetime_utc]    "YYYY-MM-DD HH:MM:SS"           (gmtime, space sep)
   - [date_utc]            "YYYY-MM-DD"                    (gmtime)
   - [iso8601_local]       "YYYY-MM-DDTHH:MM:SS"           (localtime, no Z)

   The optional [~t] is epoch seconds; it defaults to [Unix.gettimeofday ()]
   so a bare call yields "now". *)

let iso8601_utc_of_tm (tm : Unix.tm) =
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let iso8601_utc ?(t = Unix.gettimeofday ()) () =
  iso8601_utc_of_tm (Unix.gmtime t)

let iso8601_utc_micros ?(t = Unix.gettimeofday ()) () =
  let tm = Unix.gmtime t in
  let micros = int_of_float ((t -. floor t) *. 1_000_000.0) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%06dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec micros

let iso8601_utc_millis ?(t = Unix.gettimeofday ()) () =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec
    (int_of_float (Float.rem (t *. 1000.0) 1000.0))

let sql_datetime_utc ?(t = Unix.gettimeofday ()) () =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let date_utc ?(t = Unix.gettimeofday ()) () =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday

let iso8601_local ?(t = Unix.gettimeofday ()) () =
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec
