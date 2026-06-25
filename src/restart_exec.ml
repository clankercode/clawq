let reexec_path_env = "CLAWQ_REEXEC_PATH"

let executable () =
  match Sys.getenv_opt reexec_path_env with
  | Some path when String.trim path <> "" -> String.trim path
  | _ -> Sys.executable_name

let deleted_suffix = " (deleted)"

let path_mentions_deleted path =
  try
    let target = Unix.readlink path in
    let lower = String.lowercase_ascii target in
    let suffix = String.lowercase_ascii deleted_suffix in
    let tlen = String.length lower and slen = String.length suffix in
    tlen >= slen && String.sub lower (tlen - slen) slen = suffix
  with _ -> false

let path_is_deleted path =
  let lower = String.lowercase_ascii path in
  let suffix = String.lowercase_ascii deleted_suffix in
  let plen = String.length lower and slen = String.length suffix in
  plen >= slen && String.sub lower (plen - slen) slen = suffix

let validate_and_fix path =
  if path_is_deleted path || path_mentions_deleted path then
    Error
      (Printf.sprintf
         "execve target %s points to a deleted binary; rebuild or set %s to a \
          fresh executable"
         path reexec_path_env)
  else
    try
      Unix.access path [ Unix.X_OK ];
      Ok path
    with
    | Unix.Unix_error (Unix.EACCES, _, _) -> (
        try
          Unix.chmod path 0o755;
          Unix.access path [ Unix.X_OK ];
          Ok path
        with Unix.Unix_error (err, func, arg) ->
          Error
            (Printf.sprintf
               "execve target %s not executable after chmod attempt: %s(%s)%s"
               path (Unix.error_message err) func
               (if arg <> "" then ": " ^ arg else "")))
    | Unix.Unix_error _ ->
        (* Non-EACCES errors (e.g. ENOENT) — let execve report the real error *)
        Ok path
