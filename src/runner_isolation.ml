(* B775: OS isolation and credential separation for hosted external runners.

   Background Codex/Claude tasks run with provider permission-bypass flags,
   so the outer boundary must come from the host system, not the model: a
   minimal allowlisted environment (publisher and cloud credentials never
   reach the agent) plus an argv-level filesystem sandbox around the runner
   command. The wrapper composes with any session host (direct, Herdr, tmux)
   because it rewrites argv before the host sees it; untrusted prompt text
   stays a single argv element throughout.

   Provider subscription auth stays local: the provider CLI reads its own
   config directory (e.g. ~/.codex, ~/.claude), which is bind-mounted
   read-write into the sandbox. API-key environment variables are stripped
   so a subscription login can never silently fall back to pay-as-you-go
   credentials. *)

type mode = Off | Prefer | Require

let mode_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "" | "off" | "none" | "disabled" -> Off
  | "prefer" | "auto" -> Prefer
  | "require" | "required" | "strict" -> Require
  | other ->
      Logs.warn (fun m ->
          m
            "Unknown security.hosted_runner_isolation %S; treating as \
             \"require\" (fail closed). Valid values: off, prefer, require."
            other);
      Require

let string_of_mode = function
  | Off -> "off"
  | Prefer -> "prefer"
  | Require -> "require"

(** {1 Minimal environment}

    Allowlist construction: only variables the runner legitimately needs.
    Everything else — GITHUB_TOKEN/GH_TOKEN, SSH_AUTH_SOCK, AWS_*/GOOGLE_* cloud
    credentials, provider API keys, GitHub App key paths — is absent by
    construction rather than by enumeration. *)

let allowed_env_keys =
  [
    "PATH";
    "HOME";
    "USER";
    "LOGNAME";
    "SHELL";
    "TERM";
    "COLORTERM";
    "LANG";
    "TZ";
    "TMPDIR";
    "XDG_RUNTIME_DIR";
    "XDG_DATA_HOME";
    "XDG_CONFIG_HOME";
    "XDG_CACHE_HOME";
  ]

let allowed_env_prefixes = [ "LC_"; "CLAWQ_" ]

let env_entry_key entry =
  match String.index_opt entry '=' with
  | Some i -> String.sub entry 0 i
  | None -> entry

let key_allowed key =
  List.mem key allowed_env_keys
  || List.exists
       (fun prefix ->
         String.length key >= String.length prefix
         && String.sub key 0 (String.length prefix) = prefix)
       allowed_env_prefixes

let minimal_env (base : string array) : string array =
  Array.of_list
    (List.filter
       (fun entry -> key_allowed (env_entry_key entry))
       (Array.to_list base))

(** {1 Filesystem sandbox} *)

(* Provider CLI state the hosted runner legitimately needs read-write so
   the worker identity's subscription login keeps working without copying
   tokens anywhere. *)
let default_provider_paths () =
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  if home = "" then []
  else
    List.map (Filename.concat home)
      [ ".codex"; ".claude"; ".claude.json"; ".config/claude"; ".cache" ]

type policy = {
  mode : mode;
  backend : Sandbox.backend;
  extra_paths : string list;  (** additional rw binds beyond the worktree *)
}

let policy_of_config (security : Runtime_config_types.security_config) =
  {
    mode = mode_of_string security.hosted_runner_isolation;
    backend = Sandbox.backend_of_policy security.sandbox_backend;
    extra_paths =
      default_provider_paths ()
      @ List.map Runtime_config.expand_home security.extra_allowed_paths;
  }

(* Fail-closed preflight: Require + no usable backend refuses the start. *)
let preflight (policy : policy) : (unit, string) result =
  match policy.mode with
  | Off -> Ok ()
  | Prefer | Require -> (
      match policy.backend with
      | Sandbox.Bubblewrap when Sandbox.is_available Sandbox.Bubblewrap -> Ok ()
      | Sandbox.Firejail when Sandbox.is_available Sandbox.Firejail -> Ok ()
      | _ when policy.mode = Prefer ->
          Logs.warn (fun m ->
              m
                "hosted_runner_isolation=prefer but no sandbox backend \
                 (bubblewrap/firejail) is available; launching WITHOUT OS \
                 isolation");
          Ok ()
      | _ ->
          Error
            "hosted_runner_isolation=require but no sandbox backend is \
             available. Install bubblewrap (`bwrap`) or firejail, set \
             security.sandbox_backend accordingly, or lower \
             security.hosted_runner_isolation to \"prefer\"/\"off\" if this \
             machine is a dedicated, trusted worker.")

let backend_usable (policy : policy) =
  match policy.backend with
  | Sandbox.Bubblewrap -> Sandbox.is_available Sandbox.Bubblewrap
  | Sandbox.Firejail -> Sandbox.is_available Sandbox.Firejail
  | Sandbox.None -> false

(* Argv-level bubblewrap wrapper: read-only system, read-write worktree +
   granted paths, private /tmp, network shared (provider APIs), everything
   else unshared. The wrapped argv is appended verbatim after "--" so no
   element is ever re-interpreted by a shell. *)
let bwrap_argv ~worktree ~log_path ~extra_paths argv =
  let ro path =
    if Sys.file_exists path then [ "--ro-bind"; path; path ] else []
  in
  let rw path = if Sys.file_exists path then [ "--bind"; path; path ] else [] in
  let prefix =
    List.concat
      [
        [ "bwrap" ];
        ro "/usr";
        ro "/bin";
        ro "/lib";
        ro "/lib64";
        ro "/etc";
        List.concat_map ro (Runtime_config.common_user_bin_dirs ());
        rw worktree;
        rw (Filename.dirname log_path);
        List.concat_map rw extra_paths;
        [ "--dev"; "/dev"; "--proc"; "/proc"; "--tmpfs"; "/tmp" ];
        [ "--unshare-pid"; "--unshare-ipc"; "--unshare-uts" ];
        [ "--die-with-parent"; "--" ];
      ]
  in
  Array.append (Array.of_list prefix) argv

let firejail_argv ~worktree ~extra_paths argv =
  let whitelists =
    (worktree :: extra_paths) @ Runtime_config.common_user_bin_dirs ()
    |> List.filter Sys.file_exists
    |> List.map (fun p -> "--whitelist=" ^ p)
  in
  let prefix =
    [ "firejail"; "--quiet"; "--noprofile" ] @ whitelists @ [ "--" ]
  in
  Array.append (Array.of_list prefix) argv

(** Wrap a hosted-runner command according to policy. Returns the (possibly
    unchanged) argv plus whether isolation was actually applied. Callers must
    run {!preflight} first; this function assumes a usable backend when the mode
    is not [Off]. *)
let wrap_argv (policy : policy) ~worktree ~log_path (argv : string array) :
    string array * bool =
  match policy.mode with
  | Off -> (argv, false)
  | Prefer when not (backend_usable policy) -> (argv, false)
  | Require when not (backend_usable policy) ->
      (* preflight refuses this earlier; never silently pass through *)
      invalid_arg "Runner_isolation.wrap_argv: require mode without backend"
  | Prefer | Require -> (
      match policy.backend with
      | Sandbox.Bubblewrap ->
          ( bwrap_argv ~worktree ~log_path ~extra_paths:policy.extra_paths argv,
            true )
      | Sandbox.Firejail ->
          (firejail_argv ~worktree ~extra_paths:policy.extra_paths argv, true)
      | Sandbox.None -> (argv, false))
