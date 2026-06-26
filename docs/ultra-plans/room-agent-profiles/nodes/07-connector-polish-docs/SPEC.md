# 07 Connector Polish Docs SPEC

## Responsibilities

- Extend connector capability matrix for room-agent delivery/history behavior.
- Harden Teams room-agent UX paths after the P11 audit.
- Document room profiles, scoped memory, persistent CWDs, routines, admin controls, and troubleshooting.

## Non-Responsibilities

- Slack-first MVP implementation.
- Core room profile schema.
- Scheduler execution.

## Backlog Mapping

- P13.M3.E1.T001: Extend connector capability matrix for room agents.
- P13.M3.E2.T001: Harden Teams room-agent slash commands.
- P13.M3.E2.T002: Harden Teams consent card actions for room agents.
- P13.M3.E2.T003: Harden Teams background completion and tool visibility.
- P13.M3.E3.T001: Document room agents and update llms-full.

## Granularity Note

Teams hardening has been split by known surface:

- Teams slash commands.
- Teams consent card actions.
- Teams background completion and tool visibility.

Further bug-specific splits can happen after the P11 Teams audit identifies exact gaps.

`P13.M3.E1` is a prerequisite for P13 ambient history capture despite living in this polish milestone.
