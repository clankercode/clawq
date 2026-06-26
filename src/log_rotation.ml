let rotated_path log_path n =
  if n = 0 then log_path else Printf.sprintf "%s.%d" log_path n

let file_size path =
  try
    let st = Unix.stat path in
    st.Unix.st_size
  with Unix.Unix_error _ -> 0

let maybe_rotate ~log_path ~(config : Runtime_config.log_config) =
  let max_bytes = config.max_size_mb * 1024 * 1024 in
  if max_bytes <= 0 then false
  else
    let size = file_size log_path in
    if size < max_bytes then false
    else begin
      let max_files = max 1 config.max_files in
      let oldest = rotated_path log_path max_files in
      (try Sys.remove oldest with Sys_error _ -> ());
      for i = max_files - 1 downto 1 do
        let src = rotated_path log_path i in
        let dst = rotated_path log_path (i + 1) in
        if Sys.file_exists src then
          try Sys.rename src dst with Sys_error _ -> ()
      done;
      (try Sys.rename log_path (rotated_path log_path 1)
       with Sys_error _ -> ());
      let new_fd =
        Unix.openfile log_path
          [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
          0o644
      in
      flush Stdlib.stdout;
      flush Stdlib.stderr;
      Format.pp_print_flush Format.std_formatter ();
      Format.pp_print_flush Format.err_formatter ();
      Unix.dup2 new_fd Unix.stdout;
      Unix.dup2 new_fd Unix.stderr;
      Unix.close new_fd;
      true
    end
