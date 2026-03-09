# Test Guidelines

## Process and Lwt Patterns

Process I/O:
- Always read stdout and stderr in **parallel** using `Lwt.both`. Sequential reads (`let* stdout = read ... in let* stderr = read ...`) can deadlock if the process fills the stderr pipe buffer while stdout hasn't closed yet.
- Use `Process_group.start` (fork+setsid+execve) for spawning child processes. This ensures signals reach the entire process group.
- Always close channels via `Lwt.finalize` wrapping the read+wait body.

Timeout/interrupt kill races:
- When a timeout or interrupt kills a process, the runner branch (reading stdout/stderr + waitpid) may resolve **before** the timeout branch because SIGTERM causes immediate pipe EOF and process exit. Use the `forced_result` ref pattern to handle this:
  1. Timeout/interrupt sets `forced_result := Some msg` **before** calling terminate.
  2. The runner branch in `Lwt.pick` checks `!forced_result` — if set, returns the forced message instead of the raw exit result.
- This pattern is established in `tools_builtin.ml` (`run_process_with_timeout`, `git_operations`) and `skills.ml`. Follow it for any new process-spawning code with timeout/interrupt support.

Signal numbers:
- OCaml's `Sys.sigterm` is `-11` (not POSIX 15). `WSIGNALED n -> 128 + n` produces `117` for SIGTERM-killed processes. Do not hardcode POSIX signal numbers; use `Sys.sig*` constants.

## Test Resilience Patterns

Lwt test isolation:
- `Lwt.async` threads from one `Lwt_main.run` persist in the global Lwt scheduler. If a test launches background Lwt work (timers, continuations), it can cause the **next** test or the test binary exit to hang. Ensure all Lwt work completes or is cancelled before `Lwt_main.run` returns.
- Never call real scheduling machinery (e.g., `schedule_autonomous_continuation`, `Agent.turn`) in tests unless the test is specifically testing that path with a mock provider. These involve timers (90s default), retries with exponential backoff, and real HTTP calls that will hang or take minutes to fail.
- When testing that state is armed/configured (e.g., continuation checks), set the state directly in the test callback rather than running the full production path.

Production throttles and sleeps:
- Production code often has `Lwt_unix.sleep` throttles (e.g., rate-limit guards between API edits). When tests use mock/instant callbacks, these sleeps still fire and silently make tests slow.
- When adding or calling production functions that contain sleeps, expose an optional `?throttle` (or similar) parameter so tests can set it to `0.0`. Example: `Update_tool.make_progress_sender ~throttle:0.0`.
- If you notice a test taking >0.5s without spawning processes or doing I/O, check for hidden `Lwt_unix.sleep` calls in the code under test.

Process-spawning tests:
- Tests that spawn real processes (e.g., `sleep 10`) via `Process_group.start` must ensure cleanup. Use `Process_group.terminate` or `terminate_blocking` in test teardown.
- Prefer short-lived commands in tests. If a long-running command is needed to test timeout/interrupt, keep the timeout short (1-2s) and verify the forced_result pattern is in place.
