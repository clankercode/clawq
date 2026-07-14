(** Minimal-build disabled guidance for `clawq github account` (P21.M4.E1.T001).

    The full surface requires redacted introspection of Principal-owned GitHub
    bindings, live PKCE/device authorization, and the canonical
    invalidate-lifecycle CAS path — all integration-only. The minimal build
    refuses safely with an actionable message instead of leaking partial
    behavior. *)

let disabled_message =
  Printf.sprintf
    "`clawq github account` (list/status/use/link/relink/unlink) is not \
     available in the minimal build. Use the full `clawq` binary for \
     Principal-owned GitHub account lifecycle. The narrow `github_account` \
     agent tool is also disabled in minimal builds; agents can only observe \
     redacted GitHub status via the full build's %s surface."
    "Github_account_admin_surface"

let cmd args =
  match args with
  | "account" :: _ -> disabled_message
  | _ ->
      Printf.sprintf "Usage: clawq-min github account <subcommand>\n\n%s"
        disabled_message
