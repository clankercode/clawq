open Cmdliner
open Main_cmd_common

let audit_list_cmd =
  let limit =
    Arg.(
      value & opt int 20
      & info [ "limit" ] ~docv:"N" ~doc:"Show at most $(docv) entries.")
  in
  let info = Cmd.info "list" ~doc:"Show recent audit entries." in
  Cmd.v info
    Term.(
      ret
        (const (fun limit ->
             run "audit" [ "list"; "--limit"; string_of_int limit ])
        $ limit))

let audit_verify_cmd =
  let info = Cmd.info "verify" ~doc:"Verify the signed audit chain." in
  Cmd.v info Term.(ret (const (run "audit") $ const [ "verify" ]))

let audit_export_cmd =
  let path = Arg.(value & pos 0 (some string) None & info [] ~docv:"PATH") in
  let info =
    Cmd.info "export"
      ~doc:
        "Export all entries as JSONL. With no PATH, uses the configured audit \
         export location."
  in
  Cmd.v info
    Term.(
      ret
        (const (fun path -> run "audit" ([ "export" ] @ Option.to_list path))
        $ path))

let audit_import_cmd =
  let path = Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH") in
  let anchor =
    Arg.(value & opt (some string) None & info [ "anchor" ] ~docv:"PATH")
  in
  let info =
    Cmd.info "import"
      ~doc:
        "Restore an exported JSONL file into an empty audit log, loading the \
         default or explicit anchor sidecar when present."
  in
  Cmd.v info
    Term.(
      ret
        (const (fun path anchor ->
             let args = [ "import"; path ] in
             let args =
               match anchor with
               | Some anchor_path -> args @ [ "--anchor"; anchor_path ]
               | None -> args
             in
             run "audit" args)
        $ path $ anchor))

let audit_purge_cmd =
  let info =
    Cmd.info "purge"
      ~doc:
        "Retain the newest contiguous suffix allowed by the retention policy."
  in
  Cmd.v info Term.(ret (const (run "audit") $ const [ "purge" ]))

let audit_cmd =
  Cmd.group
    ~default:Term.(ret (const (run "audit") $ const [ "list" ]))
    (Cmd.info "audit" ~doc:"View and manage the security audit log.")
    [
      audit_list_cmd;
      audit_verify_cmd;
      audit_export_cmd;
      audit_import_cmd;
      audit_purge_cmd;
    ]
