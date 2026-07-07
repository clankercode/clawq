let escape_newlines s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if c = '\n' then Buffer.add_string buf "\\n" else Buffer.add_char buf c)
    s;
  Buffer.contents buf

let contains haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let found = ref false in
    let i = ref 0 in
    while !i <= hlen - nlen && not !found do
      if String.sub haystack !i nlen = needle then found := true else incr i
    done;
    !found

(* Alias matching the name used across provider, ui_server, and
   command_bridge_helpers — prefer this over local redefinitions. *)
let string_contains = contains

(* Case-insensitive substring test. Empty needle matches. Prefer this over
   local case-insensitive redefinitions. *)
let contains_ci haystack needle =
  contains (String.lowercase_ascii haystack) (String.lowercase_ascii needle)

let unescape_newlines s =
  let len = String.length s in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    if !i + 1 < len && s.[!i] = '\\' && s.[!i + 1] = 'n' then begin
      Buffer.add_char buf '\n';
      i := !i + 2
    end
    else begin
      Buffer.add_char buf s.[!i];
      i := !i + 1
    end
  done;
  Buffer.contents buf

let redact_token s =
  let len = String.length s in
  if len <= 8 then String.make len '*'
  else String.sub s 0 4 ^ "..." ^ String.sub s (len - 4) 4

(* True for hostnames that resolve to the local loopback interface. Shared by
   command_bridge_helpers (gateway pairing auto-fetch guard) and
   tools_builtin_io (http_get localhost-only guard). Trims surrounding
   whitespace defensively. *)
let is_loopback_host host =
  match String.lowercase_ascii (String.trim host) with
  | "localhost" | "127.0.0.1" | "::1" -> true
  | _ -> false
