open Cmdliner

let run name args =
  let result =
    String_util.unescape_newlines (Command_bridge.handle (name :: args))
  in
  if Cli_exit.should_error ~name ~args ~result then `Error (false, result)
  else begin
    print_string result;
    `Ok ()
  end

let rest_args docv = Arg.(value & pos_all string [] & info [] ~docv)

let required_rest_args docv =
  Arg.(non_empty & pos_all string [] & info [] ~docv)

let required_trailing_args start docv =
  Arg.(non_empty & pos_right start string [] & info [] ~docv)

let simple name doc =
  Cmd.v (Cmd.info name ~doc) Term.(ret (const (run name) $ const []))

let with_args name doc man =
  let args = rest_args "ARGS" in
  Cmd.v (Cmd.info name ~doc ~man) Term.(ret (const (run name) $ args))
