(** Minimal-build disabled guidance for `clawq github user-auth`
    (P21.M4.E1.T002).

    Admin enablement readiness, plan-confirm-apply production gates, and repair
    diagnostics require the full integrations surface (vault, OAuth readiness,
    attribution rollout, account admin). The minimal build refuses safely. *)

let disabled_message =
  "`clawq github user-auth` (status/readiness/repair/enable/disable/apply) is \
   not available in the minimal build. Use the full `clawq` binary for admin \
   enablement of Principal-owned GitHub user attribution. Production user \
   attribution remains disabled until an audited full-build enablement."

let cmd args =
  match args with
  | "user-auth" :: _ | _ ->
      Printf.sprintf "Usage: clawq-min github user-auth <subcommand>\n\n%s"
        disabled_message
