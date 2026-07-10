(* B774: subscriber-worker client.

   Connects OUTBOUND to a Clawq control plane, advertises capabilities,
   claims one work item at a time, executes it locally through the shared
   runner/session-host/isolation stack, heartbeats the lease while running,
   and completes idempotently. Provider subscription credentials never
   leave this machine: only the result summary is sent back. *)

type config = {
  server : string;  (** control-plane base URL, e.g. http://cachy:8321 *)
  token : string;  (** gateway auth token *)
  capabilities : Work_item_lease.capabilities;
  poll_seconds : float;
  lease_seconds : int;
  isolation : Runner_isolation.policy;
}

let auth_headers (cfg : config) = [ ("Authorization", "Bearer " ^ cfg.token) ]

let post ~cfg ~path ~json =
  let open Lwt.Syntax in
  let* status, body =
    Http_client.post_json ~uri:(cfg.server ^ path) ~headers:(auth_headers cfg)
      ~body:(Yojson.Safe.to_string json)
  in
  Lwt.return (status, body)

type claimed = {
  item_id : int;
  lease_token : string;
  runner_pref : string option;
  host_pref : string option;
  prompt : string;
  preamble : string;
  repo_full_name : string;
}

let parse_claim body =
  match Yojson.Safe.from_string body with
  | exception _ -> Error "claim response was not JSON"
  | json -> (
      let open Yojson.Safe.Util in
      let item = member "work_item" json in
      let str key =
        match member key item with `String s -> Some s | _ -> None
      in
      match (member "id" item, member "lease_token" json) with
      | `Int item_id, `String lease_token ->
          Ok
            {
              item_id;
              lease_token;
              runner_pref = str "runner_pref";
              host_pref = str "host_pref";
              prompt = Option.value (str "prompt") ~default:"";
              preamble = Option.value (str "preamble") ~default:"";
              repo_full_name = Option.value (str "repo_full_name") ~default:"";
            }
      | _ -> Error "claim response lacked work_item.id or lease_token")

let claim ~cfg () =
  let open Lwt.Syntax in
  let* status, body =
    post ~cfg ~path:"/worker/claim"
      ~json:
        (`Assoc
           [
             ( "capabilities",
               Work_item_lease.capabilities_to_json cfg.capabilities );
             ("lease_seconds", `Int cfg.lease_seconds);
           ])
  in
  match status with
  | 204 -> Lwt.return (Ok None)
  | 200 -> Lwt.return (Result.map Option.some (parse_claim body))
  | s -> Lwt.return (Error (Printf.sprintf "claim failed: HTTP %d %s" s body))

let heartbeat ~cfg ~claimed () =
  post ~cfg ~path:"/worker/heartbeat"
    ~json:
      (`Assoc
         [
           ("item_id", `Int claimed.item_id);
           ("lease_token", `String claimed.lease_token);
         ])

let complete ~cfg ~claimed ~status ~result_kind ~result_summary =
  post ~cfg ~path:"/worker/complete"
    ~json:
      (`Assoc
         [
           ("item_id", `Int claimed.item_id);
           ("lease_token", `String claimed.lease_token);
           ("status", `String status);
           ("result_kind", `String result_kind);
           ("result_summary", `String result_summary);
         ])

let release ~cfg ~claimed =
  post ~cfg ~path:"/worker/release"
    ~json:
      (`Assoc
         [
           ("item_id", `Int claimed.item_id);
           ("lease_token", `String claimed.lease_token);
         ])

(* Execute a claimed item through the shared stack: runner argv from
   Runner_framework, host from the session-host registry, minimal env +
   sandbox from the isolation policy. Reply-only (worktree-free) execution:
   remote draft-PR publication is a control-plane concern. *)
let execute ~cfg ~(claimed : claimed) () =
  let open Lwt.Syntax in
  let runner =
    match claimed.runner_pref with
    | Some r when String.lowercase_ascii r <> "auto" ->
        Background_task_0_format.runner_of_string r
    | _ -> (
        match
          Background_task_0_format.resolve_runner ~check_available:true
            ~allow_claude:true ()
        with
        | Ok (r, _) -> Some r
        | Error _ -> None)
  in
  match runner with
  | None | Some Background_task_0_format.Local ->
      Lwt.return
        (Error
           "no usable runner available on this worker for the requested \
            preference")
  | Some runner -> (
      let framework_runner =
        match runner with
        | Background_task_0_format.Codex -> Runner_framework.Codex
        | Background_task_0_format.Claude -> Runner_framework.Claude
        | Background_task_0_format.Kimi -> Runner_framework.Kimi
        | Background_task_0_format.Gemini -> Runner_framework.Gemini
        | Background_task_0_format.Opencode -> Runner_framework.Opencode
        | Background_task_0_format.Cursor -> Runner_framework.Cursor
        | Background_task_0_format.Local -> assert false
      in
      let def = Runner_framework.runner_def_of_runner framework_runner in
      let prompt =
        String.concat "\n\n"
          [
            claimed.preamble;
            "## Request\n" ^ claimed.prompt;
            "## Execution contract\n\
             - Answer or plan only: do NOT create commits, branches, or pull \
             requests.\n\
             - Your final message is posted to the GitHub thread.";
          ]
      in
      let command =
        Runner_framework.build_command_for ~model:None ~prompt
          ~runner_session_id:None def Runner_framework.Fresh
      in
      let host_kind =
        Option.value claimed.host_pref ~default:Session_host_direct.kind
      in
      match Session_host_registry.find host_kind with
      | None ->
          Lwt.return
            (Error (Session_host_registry.unknown_kind_error host_kind))
      | Some host -> (
          match host.Session_host.ready () with
          | Error msg -> Lwt.return (Error msg)
          | Ok () -> (
              match Runner_isolation.preflight cfg.isolation with
              | Error msg -> Lwt.return (Error msg)
              | Ok () -> (
                  let workspace =
                    Filename.concat
                      (Filename.get_temp_dir_name ())
                      (Printf.sprintf "clawq-worker-item-%d" claimed.item_id)
                  in
                  (try Unix.mkdir workspace 0o755 with _ -> ());
                  let log_path = Filename.concat workspace "run.log" in
                  let base_env =
                    Runtime_config.augment_env_path (Unix.environment ())
                  in
                  let env =
                    if
                      cfg.isolation.Runner_isolation.mode
                      <> Runner_isolation.Off
                    then Runner_isolation.minimal_env base_env
                    else base_env
                  in
                  let argv, _sandboxed =
                    Runner_isolation.wrap_argv cfg.isolation ~worktree:workspace
                      ~log_path command.Runner_framework.argv
                  in
                  let* started =
                    host.Session_host.start
                      {
                        Session_host.command = Process_group.Exec argv;
                        cwd = workspace;
                        env;
                        log_path;
                      }
                  in
                  match started with
                  | Error msg -> Lwt.return (Error msg)
                  | Ok session -> (
                      let* waited = host.Session_host.wait session in
                      let log_text =
                        Background_task_0_format.read_log_tail log_path
                          (64 * 1024)
                      in
                      let summary =
                        let extracted =
                          Github.extract_final_agent_message log_text
                        in
                        if String.trim extracted <> "" then extracted
                        else log_text
                      in
                      match waited with
                      | Ok 0 -> Lwt.return (Ok summary)
                      | Ok code ->
                          Lwt.return
                            (Error
                               (Printf.sprintf "runner exited %d: %s" code
                                  (Background_task_0_format.preview_text_n 500
                                     summary)))
                      | Error msg -> Lwt.return (Error msg))))))

(* Claim once; if work was found, execute it with a heartbeat loop and
   report completion. Returns a short human-readable outcome. *)
let run_once ~cfg () =
  let open Lwt.Syntax in
  let* claimed = claim ~cfg () in
  match claimed with
  | Error msg -> Lwt.return (Error msg)
  | Ok None -> Lwt.return (Ok "no matching work")
  | Ok (Some claimed) ->
      let stop_heartbeat = ref false in
      Lwt.async (fun () ->
          let interval = max 10.0 (float_of_int cfg.lease_seconds /. 3.0) in
          let rec beat () =
            if !stop_heartbeat then Lwt.return_unit
            else
              let* () = Lwt_unix.sleep interval in
              if !stop_heartbeat then Lwt.return_unit
              else
                let* status, _ = heartbeat ~cfg ~claimed () in
                if status = 200 then beat ()
                else begin
                  (* lost the lease: abandon quietly; the control plane has
                     re-queued or completed the item *)
                  stop_heartbeat := true;
                  Lwt.return_unit
                end
          in
          beat ());
      let* result =
        Lwt.finalize
          (fun () -> execute ~cfg ~claimed ())
          (fun () ->
            stop_heartbeat := true;
            Lwt.return_unit)
      in
      let* status_code, body =
        match result with
        | Ok summary ->
            complete ~cfg ~claimed ~status:"succeeded" ~result_kind:"reply"
              ~result_summary:summary
        | Error msg ->
            complete ~cfg ~claimed ~status:"failed" ~result_kind:"failed"
              ~result_summary:msg
      in
      if status_code = 200 then
        Lwt.return
          (Ok
             (Printf.sprintf "work item %d %s" claimed.item_id
                (match result with Ok _ -> "completed" | Error _ -> "failed")))
      else
        Lwt.return
          (Error
             (Printf.sprintf
                "completion delivery for item %d failed: HTTP %d %s"
                claimed.item_id status_code body))

let rec run_loop ~cfg () : unit Lwt.t =
  let open Lwt.Syntax in
  let* outcome = run_once ~cfg () in
  (match outcome with
  | Ok msg when msg <> "no matching work" ->
      Logs.info (fun m -> m "worker: %s" msg)
  | Ok _ -> ()
  | Error msg -> Logs.warn (fun m -> m "worker: %s" msg));
  let* () = Lwt_unix.sleep cfg.poll_seconds in
  run_loop ~cfg ()
