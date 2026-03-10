let reexec_path_env = "CLAWQ_REEXEC_PATH"

let executable () =
  match Sys.getenv_opt reexec_path_env with
  | Some path when String.trim path <> "" -> String.trim path
  | _ -> Sys.executable_name

let validate_and_fix path =
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
