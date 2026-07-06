open Cmdliner
open Main_cmd_common

let models_list_cmd =
  let provider =
    Arg.(
      value
      & opt (some string) None
      & info [ "provider" ] ~docv:"P" ~doc:"Filter by provider name.")
  in
  let json = Arg.(value & flag & info [ "json" ] ~doc:"Output as JSON.") in
  let availability_values =
    [
      ("available", Models_catalog.Available);
      ("unavailable", Models_catalog.Unavailable);
      ("all", Models_catalog.All);
    ]
  in
  let availability =
    Arg.(
      value
      & opt (some (enum availability_values)) None
      & info [ "availability" ] ~docv:"MODE"
          ~doc:"Filter by availability: available, unavailable, or all.")
  in
  let available =
    Arg.(value & flag & info [ "available" ] ~doc:"Show available models.")
  in
  let unavailable =
    Arg.(value & flag & info [ "unavailable" ] ~doc:"Show unavailable models.")
  in
  let all = Arg.(value & flag & info [ "all" ] ~doc:"Show all models.") in
  Cmd.v
    (Cmd.info "list"
       ~doc:
         "List known models from the catalog (optionally filter by provider).")
    Term.(
      ret
        (const (fun provider json availability available unavailable all ->
             let args = [ "list" ] in
             let args =
               match provider with
               | Some p -> args @ [ "--provider"; p ]
               | None -> args
             in
             let args = if json then args @ [ "--json" ] else args in
             let args =
               match availability with
               | Some mode ->
                   args
                   @ [
                       "--availability";
                       Models_catalog.availability_filter_to_string mode;
                     ]
               | None -> args
             in
             let args = if available then args @ [ "--available" ] else args in
             let args =
               if unavailable then args @ [ "--unavailable" ] else args
             in
             let args = if all then args @ [ "--all" ] else args in
             run "models" args)
        $ provider $ json $ availability $ available $ unavailable $ all))

let models_set_default_cmd =
  let model =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"MODEL")
  in
  let skip_validation =
    Arg.(
      value & flag
      & info
          [ "skip-validation"; "no-test" ]
          ~doc:
            "Skip the live test completion that normally runs before \
             committing the switch. Use only when you know the model works.")
  in
  Cmd.v
    (Cmd.info "set-default"
       ~doc:"Set default model (e.g. anthropic:claude-sonnet-4-6).")
    Term.(
      ret
        (const (fun model skip ->
             let args = [ "set-default"; model ] in
             let args = if skip then args @ [ "--skip-validation" ] else args in
             run "models" args)
        $ model $ skip_validation))

let models_refresh_cmd =
  let force =
    Arg.(
      value & flag
      & info [ "force" ] ~doc:"Force refresh, ignoring the cache TTL.")
  in
  let provider =
    Arg.(
      value
      & opt (some string) None
      & info [ "provider" ] ~docv:"PNAME"
          ~doc:"Refresh models for a specific provider only.")
  in
  Cmd.v
    (Cmd.info "refresh" ~doc:"Refresh model list from provider APIs.")
    Term.(
      ret
        (const (fun force provider ->
             let args = [ "refresh" ] in
             let args =
               match provider with
               | Some p -> args @ [ "--provider"; p ]
               | None -> args
             in
             let args = if force then args @ [ "--force" ] else args in
             run "models" args)
        $ force $ provider))

let models_cmd =
  Cmd.group
    ~default:Term.(ret (const (run "models") $ const []))
    (Cmd.info "models" ~doc:"List known models and set default model.")
    [ models_list_cmd; models_set_default_cmd; models_refresh_cmd ]
