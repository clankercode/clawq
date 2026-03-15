type shell = Bash | Zsh | Fish

let shell_name = function Bash -> "bash" | Zsh -> "zsh" | Fish -> "fish"

let shell_of_string = function
  | "bash" -> Ok Bash
  | "zsh" -> Ok Zsh
  | "fish" -> Ok Fish
  | s ->
      Error
        (Printf.sprintf "Unknown shell %S; supported shells: bash, zsh, fish" s)

let detect_shell () =
  match Sys.getenv_opt "SHELL" with
  | Some s when String.length s > 0 -> (
      match Filename.basename s with
      | "bash" -> Some Bash
      | "zsh" -> Some Zsh
      | "fish" -> Some Fish
      | _ -> None)
  | _ -> None

let bash_script =
  {|_clawq_completions() {
    local cur prev words cword
    _init_completion 2>/dev/null || {
        COMPREPLY=()
        cur="${COMP_WORDS[COMP_CWORD]}"
        prev="${COMP_WORDS[COMP_CWORD-1]}"
    }
    local commands="active agent audit auth background benchmark capabilities channel completions config costs cron debug delegate doctor hardware memory mcp migrate models onboard otp-show phase2 plan provider reset-agent reset-workspace runtime service session skills status transcribe tunnel update usage version workspace"
    if [[ ${COMP_CWORD} -eq 1 ]]; then
        COMPREPLY=( $(compgen -W "${commands}" -- "${cur}") )
        return
    fi
    case "${COMP_WORDS[1]}" in
        audit)       COMPREPLY=( $(compgen -W "list verify export import purge" -- "${cur}") );;
        auth)        COMPREPLY=( $(compgen -W "set-key providers encrypt codex-login codex-status codex-logout" -- "${cur}") );;
        background)  COMPREPLY=( $(compgen -W "list show add wait logs cancel" -- "${cur}") );;
        completions) COMPREPLY=( $(compgen -W "print install" -- "${cur}") );;
        config)      COMPREPLY=( $(compgen -W "wizard set get show" -- "${cur}") );;
        cron)        COMPREPLY=( $(compgen -W "list add remove history runs" -- "${cur}") );;
        debug)       COMPREPLY=( $(compgen -W "html-preview prompt context" -- "${cur}") );;
        models)      COMPREPLY=( $(compgen -W "list set-default refresh" -- "${cur}") );;
        plan)        COMPREPLY=( $(compgen -W "start list show logs cancel" -- "${cur}") );;
        provider)    COMPREPLY=( $(compgen -W "quota list" -- "${cur}") );;
        runtime)     COMPREPLY=( $(compgen -W "status native docker" -- "${cur}") );;
        service)     COMPREPLY=( $(compgen -W "start stop restart signal-restart status" -- "${cur}") );;
        session)     COMPREPLY=( $(compgen -W "list epochs show inject events pending compact" -- "${cur}") );;
        skills)      COMPREPLY=( $(compgen -W "list path init" -- "${cur}") );;
        tunnel)      COMPREPLY=( $(compgen -W "start stop status apply restart daemon-status" -- "${cur}") );;
    esac
}
complete -F _clawq_completions clawq
|}

let zsh_script =
  {|#compdef clawq

_clawq() {
    local state line
    typeset -A opt_args
    _arguments -C \
        '1: :->command' \
        '*: :->args'
    case $state in
        command)
            local -a commands
            commands=(
                'agent:Start the clawq daemon'
                'audit:View and manage the security audit log'
                'auth:Manage provider authentication'
                'background:Manage background coding tasks'
                'benchmark:Measure tool invocation latency'
                'capabilities:List active runtime capabilities'
                'channel:List configured channels'
                'completions:Generate shell completion scripts'
                'config:Manage configuration'
                'cron:Manage scheduled agent messages'
                'debug:Debug utilities'
                'delegate:High-level background-task workflow'
                'doctor:Check configuration for common issues'
                'hardware:Hardware integration'
                'memory:Show memory backend configuration'
                'mcp:Start the MCP server'
                'migrate:Run database migrations'
                'models:List known models and set default model'
                'onboard:Create starter config file'
                'otp-show:Show pairing codes'
                'phase2:Show Phase 2 feature status'
                'plan:Manage planning pipelines'
                'provider:Show provider quota and list'
                'reset-agent:Wipe and redeploy agent'
                'reset-workspace:Wipe chat history and redeploy'
                'runtime:Manage native and Docker runtimes'
                'service:Manage the clawq system service'
                'session:Manage agent sessions'
                'skills:Manage agent skills'
                'status:Show runtime configuration summary'
                'transcribe:Transcribe an audio file'
                'tunnel:Manage public tunnel'
                'update:Trigger a daemon update'
                'active:Show active 5-hour window usage'
                'costs:Show cost breakdowns'
                'usage:Show provider quota/usage status'
                'version:Print version and build info'
                'workspace:Print the current workspace directory'
            )
            _describe 'command' commands
            ;;
        args)
            case $line[1] in
                audit)       _values 'subcommand' 'list' 'verify' 'export' 'import' 'purge';;
                auth)        _values 'subcommand' 'set-key' 'providers' 'encrypt' 'codex-login' 'codex-status' 'codex-logout';;
                background)  _values 'subcommand' 'list' 'show' 'add' 'wait' 'logs' 'cancel';;
                completions) _values 'subcommand' 'print' 'install';;
                config)      _values 'subcommand' 'wizard' 'set' 'get' 'show';;
                cron)        _values 'subcommand' 'list' 'add' 'remove' 'history' 'runs';;
                debug)       _values 'subcommand' 'html-preview' 'prompt' 'context';;
                models)      _values 'subcommand' 'list' 'set-default' 'refresh';;
                plan)        _values 'subcommand' 'start' 'list' 'show' 'logs' 'cancel';;
                provider)    _values 'subcommand' 'quota' 'list';;
                runtime)     _values 'subcommand' 'status' 'native' 'docker';;
                service)     _values 'subcommand' 'start' 'stop' 'restart' 'signal-restart' 'status';;
                session)     _values 'subcommand' 'list' 'epochs' 'show' 'inject' 'events' 'pending' 'compact';;
                skills)      _values 'subcommand' 'list' 'path' 'init';;
                tunnel)      _values 'subcommand' 'start' 'stop' 'status' 'apply' 'restart' 'daemon-status';;
            esac
            ;;
    esac
}

_clawq
|}

let fish_script =
  {|# clawq fish completions
set -l clawq_commands active agent audit auth background benchmark capabilities channel completions config costs cron debug delegate doctor hardware memory mcp migrate models onboard otp-show phase2 plan provider reset-agent reset-workspace runtime service session skills status transcribe tunnel update usage version workspace

complete -c clawq -f
for cmd in $clawq_commands
    complete -c clawq -n "not __fish_seen_subcommand_from $clawq_commands" -a $cmd
end

complete -c clawq -n "__fish_seen_subcommand_from audit"       -a "list verify export import purge"
complete -c clawq -n "__fish_seen_subcommand_from auth"        -a "set-key providers encrypt codex-login codex-status codex-logout"
complete -c clawq -n "__fish_seen_subcommand_from background"  -a "list show add wait logs cancel"
complete -c clawq -n "__fish_seen_subcommand_from completions" -a "print install"
complete -c clawq -n "__fish_seen_subcommand_from config"      -a "wizard set get show"
complete -c clawq -n "__fish_seen_subcommand_from cron"        -a "list add remove history runs"
complete -c clawq -n "__fish_seen_subcommand_from debug"       -a "html-preview prompt context"
complete -c clawq -n "__fish_seen_subcommand_from models"      -a "list set-default refresh"
complete -c clawq -n "__fish_seen_subcommand_from plan"        -a "start list show logs cancel"
complete -c clawq -n "__fish_seen_subcommand_from provider"    -a "quota list"
complete -c clawq -n "__fish_seen_subcommand_from runtime"     -a "status native docker"
complete -c clawq -n "__fish_seen_subcommand_from service"     -a "start stop restart signal-restart status"
complete -c clawq -n "__fish_seen_subcommand_from session"     -a "list epochs show inject events pending compact"
complete -c clawq -n "__fish_seen_subcommand_from skills"      -a "list path init"
complete -c clawq -n "__fish_seen_subcommand_from tunnel"      -a "start stop status apply restart daemon-status"
|}

let script_for_shell = function
  | Bash -> bash_script
  | Zsh -> zsh_script
  | Fish -> fish_script

let install_path_for_shell shell =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "~" in
  match shell with
  | Bash -> home ^ "/.local/share/bash-completion/completions/clawq"
  | Zsh -> home ^ "/.zfunc/_clawq"
  | Fish -> home ^ "/.config/fish/completions/clawq.fish"

let activate_instructions shell path =
  match shell with
  | Bash ->
      Printf.sprintf
        "To load automatically, add to ~/.bashrc:\n\
        \  source %s\n\
         Or, if bash-completion is installed, it will be loaded automatically\n\
         from ~/.local/share/bash-completion/completions/ on next shell start."
        path
  | Zsh ->
      "To load automatically, add to ~/.zshrc before compinit:\n\
      \  fpath=(~/.zfunc $fpath)\n\
      \  autoload -Uz compinit && compinit\n\
       Then restart your shell or run: exec zsh"
  | Fish ->
      "Fish completions are loaded automatically from\n\
       ~/.config/fish/completions/. Start a new fish session or run: exec fish"

let rec mkdirp dir =
  if not (Sys.file_exists dir) then begin
    mkdirp (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_file path content =
  mkdirp (Filename.dirname path);
  let oc = open_out path in
  output_string oc content;
  close_out oc

let install_shell shell =
  let path = install_path_for_shell shell in
  try
    write_file path (script_for_shell shell);
    Printf.sprintf "Installed %s completions to:\n  %s\n\n%s\n"
      (shell_name shell) path
      (activate_instructions shell path)
  with e ->
    Printf.sprintf "Error installing %s completions: %s" (shell_name shell)
      (Printexc.to_string e)

let help_message () =
  let shell_str =
    match detect_shell () with
    | Some s -> shell_name s
    | None -> "unknown (use --shell to specify)"
  in
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "~" in
  Printf.sprintf
    {|Shell completions for clawq

  Auto-detected shell: %s

  SUBCOMMANDS
    completions print [--shell bash|zsh|fish]
        Print the completion script for the current or specified shell.
    completions install [--shell bash|zsh|fish]
        Install completions to the default location.

  INSTALL PATHS
    bash: %s/.local/share/bash-completion/completions/clawq
    zsh:  %s/.zfunc/_clawq
    fish: %s/.config/fish/completions/clawq.fish

  QUICK SETUP
    bash:  eval "$(clawq completions print --shell bash)"
    zsh:   eval "$(clawq completions print --shell zsh)"
    fish:  clawq completions install --shell fish

  Or auto-install for detected shell:
    clawq completions install
|}
    shell_str home home home

let cmd_completions args =
  match args with
  | [] -> help_message ()
  | [ "print" ] -> (
      match detect_shell () with
      | Some s -> script_for_shell s
      | None ->
          "Error: could not detect shell from $SHELL.\n\
           Use: clawq completions print --shell bash|zsh|fish\n")
  | [ "print"; "--shell"; s ] -> (
      match shell_of_string s with
      | Ok shell -> script_for_shell shell
      | Error e -> "Error: " ^ e ^ "\n")
  | [ "install" ] -> (
      match detect_shell () with
      | Some shell -> install_shell shell
      | None ->
          "Error: could not detect shell from $SHELL.\n\
           Use: clawq completions install --shell bash|zsh|fish\n")
  | [ "install"; "--shell"; s ] -> (
      match shell_of_string s with
      | Ok shell -> install_shell shell
      | Error e -> "Error: " ^ e ^ "\n")
  | _ ->
      "Usage: clawq completions [print|install] [--shell bash|zsh|fish]\n\n\
       Run 'clawq completions' for full help.\n"
