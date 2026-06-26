(** Detection of how the running clawq binary was installed, so that a
    self-update can be routed through the originating package manager
    (npm/pnpm/yarn/bun) or Homebrew rather than the generic git/binary paths.

    The detection is path-based: a global package-manager install places the
    executable inside (or symlinks it into) a recognizable directory tree. We
    resolve the real path of the running binary and classify it.

    All functions here are pure and platform-aware (Linux/macOS live today;
    Windows branches are written for forward compatibility but not yet a
    supported runtime target). *)

type manager = Npm | Pnpm | Yarn | Bun | Homebrew

val package_name : string
(** The npm package name clawq is published under, e.g. ["@clawq/clawq"]. *)

val name : manager -> string
(** Short human label for a manager, e.g. ["npm"], ["pnpm"], ["Homebrew"]. *)

val detect :
  ?executable:string ->
  ?os:string ->
  ?realpath:(string -> string) ->
  ?command_exists:(string -> bool) ->
  unit ->
  manager option
(** Detect which package manager owns the running binary.

    @param executable
      Path to the running binary (default [Sys.executable_name]).
    @param os
      Platform tag, one of ["Unix"] / ["Win32"] / ["Cygwin"] (default
      [Sys.os_type]). Controls path normalization and which managers are
      eligible (Homebrew is excluded on Win32).
    @param realpath
      Resolver used to follow symlinks to the real install location (default
      [Unix.realpath], falling back to the input on error).
    @param command_exists
      Predicate testing whether a CLI is on [PATH] (default scans [$PATH]). A
      detected manager is only returned when its CLI is actually invokable, so
      callers can safely fall through to other update modes otherwise.

    Returns [None] when the path matches no known install layout or the
    manager's CLI is unavailable. *)

val stable_bin_path : ?os:string -> string -> string option
(** Locate the stable on-[PATH] executable named [name] (the bin symlink a
    package manager maintains), returning its full path. Used to pick a re-exec
    target after an upgrade: managers with versioned stores (pnpm, Homebrew)
    replace the resolved binary path, but keep the PATH symlink pointing at the
    new version. *)

val update_argv : ?os:string -> manager -> string array
(** The argv (program + args) that upgrades clawq globally via [manager].
    Platform-aware: yields [npm.cmd]/[bun.exe] style names on Win32. *)

val describe_command : ?os:string -> manager -> string
(** Human-readable form of {!update_argv} for progress display, e.g.
    ["npm install -g @clawq/clawq@latest"]. *)
