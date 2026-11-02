# Autonomous session check-ins

Automatic continuation and session keepalive give Clawq another turn to advance
work already assigned in the session. When actionable work remains, both prompts
instruct the agent to take the next concrete action immediately and use tools
during that turn. Active review, harvest, commit, and research lanes should make
progress before the agent reports what it did. A status update, plan, or promise
to continue later does not satisfy that instruction.

Check-ins stay within the user's existing authorization. If one step is blocked,
the agent should attempt another available action. When nothing actionable remains,
or all remaining work requires user input or authorization, the prompt permits a
brief completion report or a specific blocker. A check-in does not authorize new
work or bypass tool permissions.

Automatic continuation uses `agent_defaults.autonomous_continuation_enabled`
(default `true`) and `agent_defaults.autonomous_continuation_delay` (default
`90.0` seconds). The existing hidden idle response still disarms continuation;
the prompts do not advertise it. With `send_continuation_checkin` at its default
`false`, channel output is suppressed while the turn still executes tools and
records conversation history.

Session keepalive is enabled separately with `clawq session keepalive SESSION on`.
Its nudge uses the same action instructions and runs with channel output
suppressed. The action instructions guide the model; they do not reject every
text-only response at runtime. Tools may be unavailable or work may be blocked.

Regression tests use an in-process mock provider, a scratch workspace, and an
in-memory database to drive the real session turn and tool loop. They verify
that `doc_write` completes before progress is returned, tool results persist,
channel output stays suppressed, and continuation can still disarm after work.
These checks cover prompt delivery and tool execution, without measuring live
model compliance.
