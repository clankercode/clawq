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
- Prefer short-lived commands in tests. If a long-running command is needed to test timeout/interrupt, keep the timeout short (0.2s is enough for descendant-kill tests) and verify the forced_result pattern is in place.
- **Timeout message format**: `"Error: command timed out after %.0f seconds"` — a timeout of 0.2 produces `"...after 0 seconds"`, 1.0 produces `"...after 1 seconds"`. Use 0.2s for tests that just need to verify the timeout fires.

## `terminate_blocking` Zombie Issue (critical performance pitfall)

`Process_group.terminate_blocking` has a known behavior: after sending SIGKILL, it polls `group_alive(pid)` which uses `kill(-pid, 0)`. On Linux, a zombie process (dead but not yet `waitpid`'d) still makes `kill(-pgid, 0)` succeed. This causes `terminate_blocking` to loop for its full `wait_seconds=1.0` timeout after sending SIGKILL, even though the process is already dead.

**Result**: Any test that calls `terminate_blocking` on a process it later intends to `waitpid` (via `Process_group.wait`) will always take ~1.2s (0.2s grace + 1.0s zombie loop) instead of the expected <50ms.

**Fix pattern for tests**: Use `Process_group.terminate` (the Lwt version) combined with `Process_group.wait` in a single `Lwt_main.run` call. This reaps the zombie inside the same Lwt scheduler run, so the watchdog/waitpid resolves before the zombie loop would start:
```ocaml
(* Instead of:                                        *)
(*   Process_group.terminate_blocking proc.pid;       *)
(*   ignore (Lwt_main.run (Process_group.wait ...));  *)
(* Do:                                                *)
Lwt_main.run
  (let open Lwt.Syntax in
   let* () = Process_group.terminate proc.pid in
   let* _ = Process_group.wait proc.pid in
   Process_group.close proc)
```

**Fake PIDs**: If a test uses a fake/invalid PID (e.g., `pid:4321`) and `cancel_with_signal` is called, pass `~terminate_group:(fun ?grace_seconds:_ ?wait_seconds:_ _pid -> ())` to skip the 0.2s grace sleep entirely:
```ocaml
Background_task.cancel_with_signal
  ~send_signal:(fun pid signal -> ...)
  ~terminate_group:(fun ?grace_seconds:_ ?wait_seconds:_ _pid -> ())
  ~db ~id ()
```

## Timing Sleeps in Tests

Keep Lwt sleeps as short as functionally required:
- **Process liveness**: A process only needs to be alive for a few ms when `readopt_running_tasks` (or similar) checks it. Use `sleep 0.1` not `sleep 1` for commands that just need to be "alive during a DB query". Example: `"echo output; sleep 0.1"` instead of `"echo output; sleep 1"`.
- **Log-follow tests**: Reduce intermediate sleep delays (e.g., `sleep 0.05` between appending a line and finishing the task) and use a faster `poll_seconds` (e.g., `0.02`) to reduce total follow time from ~300ms to ~100ms.
- **Benchmark tests**: The `test_run_default` benchmark test should use `["-n"; "1"]` (1 iteration) not `[]` (10 iterations default) — the test only checks output format, not performance accuracy.
- **Interrupt tests**: A 30ms `Lwt_unix.sleep` is enough to ensure the tested process has started before setting the interrupt flag. 100ms is wasteful.
