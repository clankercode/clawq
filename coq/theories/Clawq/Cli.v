From Coq Require Import String List.
Import ListNotations.
Open Scope string_scope.

Inductive Command :=
| CmdAgent
| CmdOnboard
| CmdStatus
| CmdDoctor
| CmdCron
| CmdChannel
| CmdSkills
| CmdHardware
| CmdMigrate
| CmdService
| CmdModels
| CmdMemory
| CmdWorkspace
| CmdCapabilities
| CmdAuth
| CmdVersion
| CmdHelp
| CmdUnknown.

Definition parse_command (s : string) : Command :=
  if String.eqb s "agent" then CmdAgent else
  if String.eqb s "onboard" then CmdOnboard else
  if String.eqb s "status" then CmdStatus else
  if String.eqb s "doctor" then CmdDoctor else
  if String.eqb s "cron" then CmdCron else
  if String.eqb s "channel" then CmdChannel else
  if String.eqb s "skills" then CmdSkills else
  if String.eqb s "hardware" then CmdHardware else
  if String.eqb s "migrate" then CmdMigrate else
  if String.eqb s "service" then CmdService else
  if String.eqb s "models" then CmdModels else
  if String.eqb s "memory" then CmdMemory else
  if String.eqb s "workspace" then CmdWorkspace else
  if String.eqb s "capabilities" then CmdCapabilities else
  if String.eqb s "auth" then CmdAuth else
  if String.eqb s "version" then CmdVersion else
  if String.eqb s "help" then CmdHelp else
  CmdUnknown.

Definition usage : string :=
  "Usage: clawq <command>\n"
  ++ "Commands: onboard, agent, status, doctor, cron, channel, skills, hardware,\n"
  ++ "          migrate, service, models, memory, workspace, capabilities, auth\n".

Definition dispatch (args : list string) : string :=
  match args with
  | [] => usage
  | cmd :: _ =>
      match parse_command cmd with
      | CmdAgent => "agent: TODO (MVP command skeleton wired)"
      | CmdOnboard => "onboard: TODO (MVP command skeleton wired)"
      | CmdStatus => "status: TODO (MVP command skeleton wired)"
      | CmdDoctor => "doctor: TODO (MVP command skeleton wired)"
      | CmdCron => "cron: scheduler-backed command available in full runtime; use CLI bridge for list/add/remove/history/runs"
      | CmdChannel => "channel: TODO (MVP command skeleton wired)"
      | CmdSkills => "skills: TODO (MVP command skeleton wired)"
      | CmdHardware => "hardware: deferred in part (phase 2 peripherals)"
      | CmdMigrate => "migrate: TODO (MVP command skeleton wired)"
      | CmdService => "service: TODO (MVP command skeleton wired)"
      | CmdModels => "models: TODO (MVP command skeleton wired)"
      | CmdMemory => "memory: TODO (MVP command skeleton wired)"
      | CmdWorkspace => "workspace: TODO (MVP command skeleton wired)"
      | CmdCapabilities => "capabilities: TODO (MVP command skeleton wired)"
      | CmdAuth => "auth: TODO (MVP command skeleton wired)"
      | CmdVersion => "clawq 0.4.0-dev"
      | CmdHelp => usage
      | CmdUnknown => "unknown command\n" ++ usage
      end
  end.
