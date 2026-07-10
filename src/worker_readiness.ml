(* B776: worker-fleet readiness diagnostics.

   One provider-neutral surface that classifies each boundary a hosted-work
   deployment depends on — queue, worker identity, queue auth, local runner
   auth, session host, repository grants, sandbox, publisher, and version —
   as pass / warn / fail with an actionable repair step. Secrets are never
   printed: presence is reported, values are redacted. *)

type level = Pass | Warn | Fail

let string_of_level = function
  | Pass -> "pass"
  | Warn -> "warn"
  | Fail -> "fail"

type check = { name : string; level : level; detail : string; repair : string }
type role = Control_plane | Worker

(* Redact anything token-shaped before it can reach a detail string. *)
let safe s = String_util.redact_token s
let runner_present name = Background_task_0_format.command_exists name

let check_queue ~db_present =
  if db_present then
    {
      name = "queue";
      level = Pass;
      detail = "control-plane database reachable";
      repair = "";
    }
  else
    {
      name = "queue";
      level = Fail;
      detail = "control-plane database not initialized";
      repair =
        "Start the Clawq daemon on the control-plane host so the work-item \
         queue is created.";
    }

let check_worker_identity ~worker_id =
  match worker_id with
  | Some id when String.trim id <> "" ->
      {
        name = "worker";
        level = Pass;
        detail = Printf.sprintf "worker id %s" (safe id);
        repair = "";
      }
  | _ ->
      {
        name = "worker";
        level = Fail;
        detail = "no stable worker identity";
        repair = "Pass --id <stable-worker-id> when running the worker.";
      }

let check_queue_auth ~token =
  match token with
  | Some t when String.trim t <> "" ->
      {
        name = "queue-auth";
        level = Pass;
        detail = "gateway auth token present (redacted)";
        repair = "";
      }
  | _ ->
      {
        name = "queue-auth";
        level = Warn;
        detail = "no gateway auth token configured";
        repair =
          "Set gateway.auth_token on the control plane and pass --token to the \
           worker so the queue is not open to unauthenticated callers.";
      }

(* Runner auth is checked WITHOUT surfacing credentials: only presence of
   the official CLI and a non-crashing version probe. A missing CLI is a
   fail; a present CLI is a pass with a reminder that subscription login is
   verified by the CLI itself. *)
let check_runners ~runners =
  List.map
    (fun runner ->
      if runner_present runner then
        {
          name = "runner:" ^ runner;
          level = Pass;
          detail =
            Printf.sprintf
              "%s CLI present; subscription login is managed by the CLI" runner;
          repair = "";
        }
      else
        {
          name = "runner:" ^ runner;
          level = Fail;
          detail = Printf.sprintf "%s CLI not found in PATH" runner;
          repair =
            Printf.sprintf
              "Install the %s CLI and sign in locally; provider credentials \
               stay on this worker."
              runner;
        })
    runners

let check_hosts ~hosts =
  List.map
    (fun host ->
      match Session_host_registry.find host with
      | None ->
          {
            name = "host:" ^ host;
            level = Fail;
            detail = Printf.sprintf "unknown session host %S" host;
            repair =
              Printf.sprintf "Use one of: %s."
                (String.concat ", " (Session_host_registry.known_kinds ()));
          }
      | Some h -> (
          match h.Session_host.ready () with
          | Ok () ->
              {
                name = "host:" ^ host;
                level = Pass;
                detail = Printf.sprintf "%s session host ready" host;
                repair = "";
              }
          | Error msg ->
              {
                name = "host:" ^ host;
                level = Fail;
                detail = safe msg;
                repair =
                  "Install the session host binary, or advertise a different \
                   host (direct is always available).";
              }))
    hosts

let check_repos ~repos =
  if repos = [] then
    {
      name = "repos";
      level = Fail;
      detail = "no repositories granted";
      repair =
        "Pass --repos owner/repo[,owner2/repo2]; workers only serve repos they \
         are explicitly allowed to.";
    }
  else
    {
      name = "repos";
      level = Pass;
      detail = Printf.sprintf "%d repository grant(s)" (List.length repos);
      repair = "";
    }

let check_sandbox ~(isolation : Runner_isolation.policy) =
  match Runner_isolation.preflight isolation with
  | Ok () -> (
      match isolation.Runner_isolation.mode with
      | Runner_isolation.Off ->
          {
            name = "sandbox";
            level = Warn;
            detail =
              "hosted_runner_isolation=off — no OS sandbox around hosted \
               runners";
            repair =
              "Set security.hosted_runner_isolation to \"require\" for remote \
               workers so credential-bearing environments cannot leak.";
          }
      | _ ->
          {
            name = "sandbox";
            level = Pass;
            detail =
              Printf.sprintf "isolation=%s with a working backend"
                (Runner_isolation.string_of_mode isolation.Runner_isolation.mode);
            repair = "";
          })
  | Error msg ->
      {
        name = "sandbox";
        level = Fail;
        detail = safe msg;
        repair =
          "Install bubblewrap or firejail, or lower \
           security.hosted_runner_isolation.";
      }

(* Publisher check reports whether GitHub publication is configured WITHOUT
   resolving or printing any credential. *)
let check_publisher ~github_configured =
  if github_configured then
    {
      name = "publisher";
      level = Pass;
      detail =
        "GitHub publisher configured on the control plane (credentials stay \
         with the daemon, never sent to workers)";
      repair = "";
    }
  else
    {
      name = "publisher";
      level = Warn;
      detail = "no GitHub channel configured";
      repair =
        "Configure channels.github on the control plane to publish work-item \
         results.";
    }

let check_version () =
  {
    name = "version";
    level = Pass;
    detail = Printf.sprintf "clawq %s" Build_info.version;
    repair = "";
  }

type inputs = {
  role : role;
  db_present : bool;
  worker_id : string option;
  token : string option;
  runners : string list;
  hosts : string list;
  repos : string list;
  isolation : Runner_isolation.policy;
  github_configured : bool;
}

let run (inputs : inputs) : check list =
  let common =
    [
      check_queue ~db_present:inputs.db_present;
      check_queue_auth ~token:inputs.token;
      check_version ();
    ]
  in
  let worker_specific =
    match inputs.role with
    | Control_plane ->
        [ check_publisher ~github_configured:inputs.github_configured ]
    | Worker ->
        List.concat
          [
            [ check_worker_identity ~worker_id:inputs.worker_id ];
            check_runners ~runners:inputs.runners;
            check_hosts ~hosts:inputs.hosts;
            [ check_repos ~repos:inputs.repos ];
            [ check_sandbox ~isolation:inputs.isolation ];
          ]
  in
  common @ worker_specific

let overall (checks : check list) : level =
  if List.exists (fun c -> c.level = Fail) checks then Fail
  else if List.exists (fun c -> c.level = Warn) checks then Warn
  else Pass

let format (checks : check list) : string =
  let buf = Buffer.create 512 in
  Buffer.add_string buf
    (Printf.sprintf "Readiness: %s\n" (string_of_level (overall checks)));
  List.iter
    (fun c ->
      let marker =
        match c.level with
        | Pass -> "[pass]"
        | Warn -> "[warn]"
        | Fail -> "[fail]"
      in
      Buffer.add_string buf
        (Printf.sprintf "  %-6s %-16s %s\n" marker c.name c.detail);
      if c.repair <> "" && c.level <> Pass then
        Buffer.add_string buf (Printf.sprintf "         -> %s\n" c.repair))
    checks;
  Buffer.contents buf
