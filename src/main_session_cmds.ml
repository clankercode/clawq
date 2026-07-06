open Cmdliner
open Main_cmd_common

let session_list_cmd =
  let channel =
    Arg.(
      value
      & opt (some string) None
      & info [ "channel" ] ~docv:"NAME" ~doc:"Filter by channel name.")
  in
  let prefix =
    Arg.(
      value
      & opt (some string) None
      & info [ "prefix" ] ~docv:"PREFIX" ~doc:"Filter by session key prefix.")
  in
  let active =
    Arg.(value & flag & info [ "active" ] ~doc:"Show only active sessions.")
  in
  let inactive =
    Arg.(value & flag & info [ "inactive" ] ~doc:"Show only inactive sessions.")
  in
  let main_only =
    Arg.(value & flag & info [ "main" ] ~doc:"Show only main sessions.")
  in
  let non_main =
    Arg.(value & flag & info [ "non-main" ] ~doc:"Show only non-main sessions.")
  in
  Cmd.v
    (Cmd.info "list" ~doc:"List persisted sessions with optional filters.")
    Term.(
      ret
        (const (fun channel prefix active inactive main_only non_main ->
             let args = [ "list" ] in
             let args =
               match channel with
               | Some v -> args @ [ "--channel"; v ]
               | None -> args
             in
             let args =
               match prefix with
               | Some v -> args @ [ "--prefix"; v ]
               | None -> args
             in
             let args =
               if active then args @ [ "--active" ]
               else if inactive then args @ [ "--inactive" ]
               else args
             in
             let args =
               if main_only then args @ [ "--main" ]
               else if non_main then args @ [ "--non-main" ]
               else args
             in
             run "session" args)
        $ channel $ prefix $ active $ inactive $ main_only $ non_main))

let session_epochs_cmd =
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  Cmd.v
    (Cmd.info "epochs" ~doc:"List the current and archived chat-log epochs.")
    Term.(ret (const (fun sk -> run "session" [ "epochs"; sk ]) $ session_key))

let session_show_cmd =
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  let epoch =
    Arg.(
      value
      & opt (some string) None
      & info [ "epoch" ] ~docv:"current|ID"
          ~doc:"Select epoch: 'current' or a numeric archive ID.")
  in
  let offset =
    Arg.(
      value
      & opt (some int) None
      & info [ "offset" ] ~docv:"N" ~doc:"Skip the first N messages.")
  in
  let limit =
    Arg.(
      value
      & opt (some int) None
      & info [ "limit" ] ~docv:"N" ~doc:"Show at most N messages.")
  in
  Cmd.v
    (Cmd.info "show"
       ~doc:
         "Print the raw chat log for the current or a specific archived epoch.")
    Term.(
      ret
        (const (fun sk epoch offset limit ->
             let args = [ "show"; sk ] in
             let args =
               match epoch with
               | Some v -> args @ [ "--epoch"; v ]
               | None -> args
             in
             let args =
               match offset with
               | Some n -> args @ [ "--offset"; string_of_int n ]
               | None -> args
             in
             let args =
               match limit with
               | Some n -> args @ [ "--limit"; string_of_int n ]
               | None -> args
             in
             run "session" args)
        $ session_key $ epoch $ offset $ limit))

let session_inject_cmd =
  let args = required_trailing_args 0 "MESSAGE" in
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  let cwd =
    Arg.(
      value
      & opt (some string) None
      & info [ "cwd" ]
          ~doc:"Set the agent's working directory for this session."
          ~docv:"PATH")
  in
  Cmd.v
    (Cmd.info "inject"
       ~doc:"Inject a live inbound message through the daemon session manager.")
    Term.(
      ret
        (const (fun cwd sk msg_parts ->
             run "session"
               ([ "inject" ]
               @ (match cwd with Some c -> [ "--cwd"; c ] | None -> [])
               @ [ sk ] @ msg_parts))
        $ cwd $ session_key $ args))

let session_send_cmd =
  let args = required_trailing_args 0 "MESSAGE" in
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  let cwd =
    Arg.(
      value
      & opt (some string) None
      & info [ "cwd" ]
          ~doc:"Set the agent's working directory for this session."
          ~docv:"PATH")
  in
  Cmd.v
    (Cmd.info "send"
       ~doc:"Send an inbound message to another live or queued session.")
    Term.(
      ret
        (const (fun cwd sk msg_parts ->
             run "session"
               ([ "send" ]
               @ (match cwd with Some c -> [ "--cwd"; c ] | None -> [])
               @ [ sk ] @ msg_parts))
        $ cwd $ session_key $ args))

let session_events_cmd =
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  let epoch =
    Arg.(
      value
      & opt (some string) None
      & info [ "epoch" ] ~docv:"current|ID"
          ~doc:"Select epoch: 'current' or a numeric archive ID.")
  in
  let event_type =
    Arg.(
      value
      & opt (some string) None
      & info [ "type" ] ~docv:"TYPE"
          ~doc:
            "Filter to a specific event type: workspace_refresh, \
             unknown_event, memory_context, attachment, compaction.")
  in
  Cmd.v
    (Cmd.info "events"
       ~doc:"Show event, system, and compaction messages for a session.")
    Term.(
      ret
        (const (fun sk epoch event_type ->
             let args = [ "events"; sk ] in
             let args =
               match epoch with
               | Some v -> args @ [ "--epoch"; v ]
               | None -> args
             in
             let args =
               match event_type with
               | Some v -> args @ [ "--type"; v ]
               | None -> args
             in
             run "session" args)
        $ session_key $ epoch $ event_type))

let session_pending_cmd =
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  Cmd.v
    (Cmd.info "pending" ~doc:"Show pending inbound queue rows for a session.")
    Term.(ret (const (fun sk -> run "session" [ "pending"; sk ]) $ session_key))

let session_compact_cmd =
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  Cmd.v
    (Cmd.info "compact"
       ~doc:
         "Compact session history by summarizing older messages to free up \
          context space.")
    Term.(ret (const (fun sk -> run "session" [ "compact"; sk ]) $ session_key))

let session_model_cmd =
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  let args = Arg.(value & pos_right 0 string [] & info [] ~docv:"ARGS") in
  Cmd.v
    (Cmd.info "model"
       ~doc:
         "Get, set, or clear the per-session model override (e.g. model \
          SESSION set anthropic:claude-sonnet-4-6).")
    Term.(
      ret
        (const (fun sk rest -> run "session" ([ "model"; sk ] @ rest))
        $ session_key $ args))

let session_cmd =
  Cmd.group
    ~default:Term.(ret (const (run "session") $ const []))
    (Cmd.info "session"
       ~doc:"Inspect persisted sessions and raw chat log epochs.")
    [
      session_list_cmd;
      session_epochs_cmd;
      session_show_cmd;
      session_inject_cmd;
      session_send_cmd;
      session_events_cmd;
      session_pending_cmd;
      session_compact_cmd;
      session_model_cmd;
    ]
