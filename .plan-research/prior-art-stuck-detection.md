# Prior Art: Stuck Detection & Correction Loops in AI Agent Systems

Research for I016 — designing a lightweight observer mechanism in clawq.

---

## 1. Structural / Heuristic Approaches (No LLM Required)

### 1.1 Hard Iteration & Time Caps

- **What it is:** A fixed max on turns/steps (e.g. `max_turns=25`) or wall-clock time before the loop is forcibly stopped.
- **Signals:** Turn counter, elapsed time.
- **Weight:** Negligible — integer comparison per turn.
- **Separate model:** No.
- **Correction trigger:** Hard abort, surface error to user.
- **Cost:** Zero.
- **Used by:** LangChain (`max_iterations`, `max_execution_time`), Vercel AI SDK (`stopWhen`), Claude Code (`--max-turns`).
- **Limitation:** Blunt instrument; stops even healthy long-running tasks; doesn't detect loops within few turns. The most commonly deployed pattern but the least discriminating.

### 1.2 Sliding-Window Repeated-Call Detection

- **What it is:** Track the last N tool calls (name + normalized/hashed arguments). If all N are identical, the agent is stuck.
- **Signals:** Tool name + argument hash fingerprint, sliding window of recent calls.
- **Weight:** Very low — O(N) comparison with small N (3-10).
- **Separate model:** No.
- **Correction trigger:** Inject a system-level warning message into context: "You have called {tool} {N} times with identical arguments. Try a different approach."
- **Cost:** Negligible.
- **Used by:** Claude Code feature requests #4277 and #5962; sketch.dev community; browser-use issue #191; SuperAGI issue #542.
- **False positive risk:** Legitimate polling loops (e.g., waiting for a file to appear, or a legitimate analysis task requiring many calls to the same tool with different args) can trigger false positives if thresholds are too low. Cursor/Qwen3 community reported false positives with N=3 for legitimate multi-step code analysis.
- **Implementation sketch:** A `ResultTracker` struct or record that maintains a deque of the last N (tool_name, hash(args)) pairs. On each tool invocation, append and check if all N entries are equal.

### 1.3 Same-Tool + Same-Error Pattern

- **What it is:** Tracks not just repeated calls but repeated _failures_. If the same tool is called N times and fails with the same error, that is a stronger stuck signal than repetition alone.
- **Signals:** Tool name + error type/message hash.
- **Weight:** Very low.
- **Separate model:** No.
- **Correction trigger:** Inject corrective prompt or abort.
- **Cost:** Negligible.
- **Used by:** `maxConsecutiveToolErrors` pattern proposed for Claude Code; sketch.dev; openclaw issue #806.
- **Note:** This is more discriminating than pure repetition detection — an agent retrying with the _same_ error is clearly stuck on something it cannot fix by retrying. An agent retrying with _different_ errors may be making progress. The discriminator is the error hash, not the call count alone.
- **Proposed config shape:** `max_consecutive_tool_errors: 3` with `tool_error_action: "warn" | "inject" | "abort"`.

### 1.4 Semantic Completion Validation (Task-Specific)

- **What it is:** After each LLM turn, run a programmatic check that expected outputs are present (e.g., "does the file exist?", "does the API return 200?", "does the test suite pass?"). Only allow termination if validation passes.
- **Signals:** External observable state, not just the model's self-assessment.
- **Weight:** Low to medium, depends on validation logic.
- **Separate model:** No (can be optionally delegated to a lightweight model for complex cases).
- **Correction trigger:** Continue forcing the loop if completion criteria are not met.
- **Cost:** Low — the cost of the validation check, not an LLM call.
- **Used by:** Described on fixbrokenaiapps.com as `is_task_semantically_complete()`. Also used in test-driven agent workflows where the agent loop runs until the test suite passes.
- **Limitation:** Requires task-specific validation logic; not generic. Most applicable to coding/scripting tasks.

### 1.5 Stop-Token / Explicit Termination Signal

- **What it is:** In the ReAct style, the LLM itself generates a special FINISH/TERMINATE token or calls a "done" tool. The outer loop checks for this signal. The loop does not exit unless this signal is present.
- **Signals:** Presence of a known stop token or tool call in model output.
- **Weight:** Negligible.
- **Separate model:** No.
- **Correction trigger:** N/A — this is about clean exit, not correction.
- **Cost:** Negligible.
- **Used by:** ReAct architecture (Yao et al., 2022), Vercel AI SDK (`done` tool pattern), MemGPT heartbeat pattern (inverse: `request_heartbeat=false` signals the model is done).
- **Note:** Complementary to stuck detection, not a replacement. Without stuck detection, an agent that never generates FINISH will run until a hard cap.

### 1.6 Entropy / Token Distribution Monitoring

- **What it is:** Unusually low or oscillating entropy in LLM token distributions can indicate repetitive/stuck outputs. Monitor log-probability entropy of generated tokens across turns.
- **Signals:** Token probability entropy from LLM response metadata (logprobs).
- **Weight:** Low — arithmetic on logprob values returned by the API.
- **Separate model:** No.
- **Correction trigger:** Alert if entropy drops below threshold for N consecutive turns.
- **Cost:** Near-zero if logprobs are already returned; requires API support.
- **Used by:** Research literature on LLM monitoring; not widely deployed in production.
- **Limitation:** Not all APIs return logprobs (e.g., Anthropic's API does not expose them for Claude models). More useful for text generation stuck detection than tool-calling loop detection. Not recommended as a primary signal for clawq.

---

## 2. Self-Reflection / In-Context Correction (Same Model)

### 2.1 Reflexion (Shinn et al., NeurIPS 2023)

- **Paper:** arXiv:2303.11366 — "Reflexion: Language Agents with Verbal Reinforcement Learning"
- **What it is:** After each episode/attempt, the _same_ LLM generates a natural-language "reflection" on what went wrong and what to do differently. This reflection is stored in a memory buffer and prepended to the next attempt's context. The key innovation is that reinforcement is delivered as text ("verbal reinforcement") rather than gradient updates or scalar rewards.
- **Architecture:** Three roles — Actor (generates actions), Evaluator (scores trajectories), Self-Reflection model (generates verbal critique). All three roles can be fulfilled by the same model instance with different prompts.
- **Signals:** Task outcome (scalar reward or free-form evaluator feedback), full trajectory history.
- **Weight:** Medium — requires an extra LLM call per failed attempt (same model, different prompt).
- **Separate model:** Optional. Evaluator and Self-Reflection can be the same model as Actor, or a separate cheaper model.
- **Correction trigger:** After each failed episode. Reflection text is appended as context for next attempt.
- **Cost:** One extra LLM call per correction cycle. Same model = same per-token cost. Bounded by max_episodes.
- **Key results:** 91% pass@1 on HumanEval coding benchmark (vs GPT-4's 80% one-shot). Significant gains on sequential decision-making (AlfWorld, WebArena) and language reasoning tasks.
- **Limitation:** Relies on the agent accurately self-evaluating its own failures. Breaks down for complex tasks where the model cannot identify the root cause. Sliding window context limit on memory buffer means very long sequences of reflections may lose early ones. Weight increases with episodes.
- **Relevance to clawq:** Strong pattern for multi-attempt agent loops. The "verbal reinforcement" approach fits naturally into clawq's message history model — reflections can be stored in SQLite (session_state or a dedicated reflections table) and injected at session resume. Does not require a separate model.

### 2.2 Self-Refine (Madaan et al., NeurIPS 2023)

- **Paper:** arXiv:2303.17651 — "Self-Refine: Iterative Refinement with Self-Feedback"
- **What it is:** A single LLM generates output, then generates feedback on that output (identifying what is wrong and why), then refines the output using the feedback. Repeats in a GENERATE -> FEEDBACK -> REFINE loop until a stopping condition is met (quality threshold or max iterations).
- **Signals:** LLM's own self-critique — identifies the specific location of the problem and provides an improvement instruction.
- **Weight:** Medium — 2 extra LLM calls per refinement cycle (feedback call + refine call).
- **Separate model:** No — same model does all three roles with different prompts.
- **Correction trigger:** After each output, the model generates feedback; if feedback says "needs improvement," execute a refine step.
- **Cost:** 2x to 3x the base cost per refinement cycle. Bounded by max iterations.
- **Key results:** ~20% absolute improvement across 7 diverse tasks (code optimization, math reasoning, dialogue, sentiment reversal, acronym generation, constrained generation, story generation). No training required, works with any frozen LLM.
- **Limitation:** The same model may be blind to its own systematic errors — if the model is consistently wrong in a particular way, self-feedback may not catch it. Each refinement step adds to context window usage. Stopping criterion (when to stop refining) must be defined.
- **Relevance to clawq:** Simpler to implement than Reflexion (no memory buffer, no episode boundary). Could be triggered selectively when the primary agent turn produces output flagged by a lightweight heuristic (e.g., the response contains "I'm not sure" or an error prefix, or the tool returned an error).

### 2.3 CRITIC (Gou et al., 2023)

- **Paper:** arXiv:2305.11738 — "CRITIC: Large Language Models Can Self-Correct with Tool-Interactive Critiquing"
- **What it is:** LLM generates output, then uses external tools (web search, code executor, calculator, fact-checker, etc.) to verify and critique the output, then revises based on tool-grounded feedback. The key insight is that tool-grounded feedback is more reliable than pure self-critique because it anchors correction to ground truth.
- **Signals:** Tool-executed verification results (factual check, test execution result, calculation output).
- **Weight:** Medium — same model, plus tool execution for verification.
- **Separate model:** No.
- **Correction trigger:** Tool returns a counterexample, failed test, or factual contradiction.
- **Cost:** Tool execution cost + 1-2 extra LLM calls per verification cycle.
- **Key results:** Improvements on free-form QA (TriviaQA, HotpotQA), math (GSM8K, SVAMP), and toxicity reduction.
- **Relevance to clawq:** Most applicable when clawq's tools can validate correctness directly (e.g., shell_exec running a test suite, file_read confirming a file was written). Less directly applicable to "stuck loop" detection per se; more about output quality correction. The tool-grounded approach is more reliable than pure self-critique.

### 2.4 ReAct (Yao et al., 2022)

- **Paper:** arXiv:2210.03629 — "ReAct: Synergizing Reasoning and Acting in Language Models"
- **What it is:** Interleaves Thought, Action, Observation steps within a single model call chain. The "Thought" step encourages explicit chain-of-thought reasoning about the next action before taking it. Loop terminates when the LLM generates a FINISH action with an answer. The Thought step functions as a lightweight in-context observer.
- **Signals:** Presence of FINISH/TERMINATE in model output; quality of Thought reasoning (observable externally but not acted on by default).
- **Weight:** Light — no extra model calls; same model generates Thoughts inline, slightly more tokens per turn.
- **Separate model:** No.
- **Correction trigger:** Implicit — Thought steps allow the model to self-correct between actions. External observer can monitor Thought text for signs of confusion.
- **Cost:** Slightly more tokens per turn (Thought text, typically 20-100 tokens).
- **Relevance to clawq:** Already the dominant pattern in how modern agents work. The Thought step is the "lightweight in-context observer" — monitoring Thought text externally is a near-zero-cost observation channel. Clawq could log Thought text separately and flag patterns like "I've tried this before" appearing repeatedly as a stuck signal.

### 2.5 Chain-of-Thought Self-Consistency (Wang et al., 2022)

- **Paper:** arXiv:2203.11171
- **What it is:** Sample multiple independent reasoning chains (via temperature > 0) and take the majority vote answer. Inconsistency between chains signals uncertainty or potential error.
- **Signals:** Variance in answers across sampled chains.
- **Weight:** Heavy for correction (N × model cost), but lightweight for detection (just checking if N chains agree).
- **Separate model:** No.
- **Correction trigger:** High disagreement among sampled chains triggers a re-try with more careful prompting.
- **Cost:** N × base cost. N=5-10 typical.
- **Relevance to clawq:** Too expensive for per-turn stuck detection. More useful as an offline quality check. Noted here because the disagreement signal is useful — if the model is genuinely stuck, repeated sampling will produce wildly inconsistent attempts.

---

## 3. Separate Observer / Evaluator Model

### 3.1 Reflexion with Separate Evaluator

- **What it is:** In Reflexion's three-role architecture, the Evaluator (trajectory scorer) is explicitly separable from the Actor. The Evaluator can be a smaller, cheaper model, a trained classifier, or a rule-based heuristic.
- **Weight:** Depends on evaluator model size. Can range from negligible (rule-based) to medium (small LLM).
- **Separate model:** Yes — explicitly designed for this separation.
- **Correction trigger:** Evaluator returns low reward score → Self-Reflection model generates verbal critique → Actor uses critique as context next episode.
- **Cost:** Evaluator cost + reflection call cost. Evaluator cost can be dramatically reduced by using a smaller model or heuristic.
- **Design choice for clawq:** The Evaluator could be a pure heuristic (did the last tool call succeed? did the session end with an error?) rather than an LLM, making the "separate model" effectively free.

### 3.2 AWS Observer & Monitoring Agent Pattern

- **Reference:** AWS Prescriptive Guidance — "Observer and Monitoring Agents" (2025)
- **What it is:** A dedicated passive observer agent watches system telemetry (logs, metrics, events, traces), reasons about anomalies, and triggers downstream agents or alerts. The observer does not directly participate in the primary agent's execution; it watches from outside.
- **Signals:** Structured telemetry — traces, spans, tool call logs, timing, error rates, output entropy.
- **Weight:** The observer runs asynchronously and passively; it does not add latency to the primary agent's critical path.
- **Separate model:** Yes — typically a separate LLM instance, but can be a lightweight rule engine.
- **Correction trigger:** Anomaly detected → alert emitted → downstream remediation agent or human notified.
- **Cost:** Adds one model call per observation cycle. Often batched or sampled (e.g., observe every 5th turn, or only on error events).
- **Relevance to clawq:** Good architectural pattern for a daemon-side observer. The daemon already has a supervisory role over sessions; an observer component fits naturally. The observer doesn't need to be as capable as the primary agent — a small model or rule engine suffices for most stuck signals.

### 3.3 Multi-LLM Evaluator / AIME Framework

- **Reference:** Patel et al., 2024 — "AIME: AI System Meta-Evaluation"
- **What it is:** Multiple specialized LLM evaluators each focus on one quality dimension (syntax, logic, correctness, readability, efficiency, etc.) and vote or ensemble their assessments.
- **Weight:** Heavy — N evaluator calls per output evaluated.
- **Separate model:** Yes — one model per quality dimension, or one model called N times with different prompts.
- **Correction trigger:** Ensemble consensus below threshold.
- **Cost:** N x model cost per evaluation cycle.
- **Key results:** Up to 62% higher error detection rate and 28.87% greater risk identification accuracy vs. single evaluator.
- **Relevance to clawq:** Too heavy for a lightweight stuck detection mechanism. More appropriate for periodic quality audits of session transcripts, not per-turn monitoring. Noted as upper-bound reference for what's achievable with separate evaluator models.

### 3.4 Constitutional AI — Self-Critique Pattern (Anthropic, 2022)

- **Paper:** arXiv:2212.08073 — "Constitutional AI: Harmlessness from AI Feedback"
- **What it is:** Model generates a response, then a "critic" prompt asks it to critique the response against a set of constitutional principles (rules about what is good/bad behavior), then a "revision" prompt asks it to revise the response to comply. All three steps use the same model with different prompts.
- **Signals:** Constitutional principle violations identified by self-critique.
- **Weight:** Medium — 2 extra model calls per cycle.
- **Separate model:** No (same model, different prompts). The "constitution" is a list of rules in the prompt, not a separate model.
- **Correction trigger:** Critique identifies principle violations → revision step.
- **Cost:** 2-3x token cost. Bounded by length of constitution and number of revision cycles.
- **Relevance to clawq:** The critique-revision structure maps directly onto an observation-correction design for agent behavior. The "constitution" could be a small set of rules about what good agent behavior looks like (e.g., "did not repeat the same failing action more than twice", "produced a concrete observable result"). Less directly applicable to raw "stuck loop" detection; more applicable to behavioral quality correction.

---

## 4. Postmortem / Failure Attribution (Post-Hoc Analysis)

### 4.1 Automated Failure Attribution (Zhang et al., ICML 2025)

- **Paper:** arXiv:2505.00212 — "Which Agent Causes Task Failures and When?"
- **What it is:** After a multi-agent task fails, an LLM analyzes the full failure log to identify which agent step caused the failure and at what point in the execution the fault occurred.
- **Three methods compared:**
  - All-at-Once: Feed entire log to LLM, ask it to identify the failure point. Cheapest, least precise.
  - Step-by-Step: Evaluate each step in sequence. Most expensive, most precise.
  - Binary Search: Bisect the log to find the failure boundary. Compromise between cost and precision.
- **Weight:** Post-hoc — does not impact the live agent loop at all.
- **Separate model:** Yes — a separate analysis model reviews the completed log after failure.
- **Correction trigger:** None in-loop; feeds back to agent design, prompt engineering, or system configuration.
- **Cost:** One to several extra model calls per failed session.
- **Key finding (critical):** The best method achieves only **53.5% accuracy** in identifying the failure-responsible agent step, and only **14.2% accuracy** for the exact failure step. Even o1 and DeepSeek R1 fail to achieve practical accuracy. Human agreement on failure attribution is also low (~62%), suggesting the task is inherently ambiguous.
- **Relevance to clawq:** This finding is sobering and important: automated postmortem via LLM is unreliable even with frontier models. This strongly argues for preferring simple, deterministic heuristic detection over LLM-based failure analysis. A postmortem command in clawq should be offered as a best-effort diagnostic tool, not a reliable root-cause oracle.

### 4.2 AgentDebug (Gao et al., 2025)

- **Paper:** arXiv:2509.25370
- **What it is:** A framework that learns from agent failure cases to iteratively build more robust agent versions. Focuses on identifying root-cause errors rather than surface-level symptoms.
- **Weight:** Offline — not in the live loop. Runs between deployments or sessions.
- **Key finding:** Focusing on root-cause errors outperforms fixing surface-level symptoms. Agents that correct the proximate cause (the immediate failing action) perform worse than agents that identify and correct the underlying cause (the wrong assumption or wrong plan that led to the failing action sequence).
- **Relevance to clawq:** Important design principle for correction prompt construction. When clawq's observer detects a stuck loop, the correction prompt should target the _reason_ for the loop (wrong assumption, unavailable resource, logical error), not merely tell the model to retry differently.

### 4.3 ReflAgent and Post-Episode Reflection Patterns

- **What it is:** Several systems (ReflAgent, ExpeL, Voyager) implement episodic reflection after each attempt, storing lessons in a persistent experience buffer that is retrieved at the start of subsequent attempts.
- **Pattern:** At the end of each episode (regardless of success/failure), generate a brief reflection and store it tagged with the task type. At the start of future similar tasks, retrieve relevant past reflections as few-shot context.
- **Weight:** Post-episode (between sessions), not in-loop.
- **Relevance to clawq:** The experience buffer maps naturally to clawq's existing memory system (memory_store/memory_recall tools, or the memory.db SQLite store). A "lessons learned" namespace in memory would let clawq accumulate correction experience across sessions without increasing per-turn cost.

---

## 5. Declarative / Rule-Based Guardrails

### 5.1 Invariant Guardrails (Multi-Turn Pattern Matching)

- **Reference:** Invariant Labs — https://explorer.invariantlabs.ai/docs/guardrails/loops/
- **What it is:** A DSL for writing multi-turn guardrailing rules that match patterns across the conversation history. Rules can detect sequences like "tool X called 3+ times without different output" and trigger configured actions (alert, block, log, abort).
- **Signals:** Tool call history, output comparison, tool result diffing.
- **Weight:** Very low — rule evaluation is pattern matching over structured data, not an LLM call.
- **Separate model:** No.
- **Correction trigger:** Rule match → configured action (alert, block, log, inject message, abort).
- **Cost:** Negligible — O(history_length) pattern matching per turn.
- **Example rule (paraphrased):** `if last_N_tool_calls(tool="bash", N=3).all_same_args() and last_N_tool_calls(tool="bash", N=3).all_same_result() then inject("You have called bash 3 times with the same command and got the same result. Try a different approach.")`
- **Relevance to clawq:** This is the most directly implementable pattern in OCaml. A small rule engine that evaluates a list of stuck-detection predicates on the turn history would be zero-cost and highly configurable. Rules can be expressed as a list of OCaml functions `(history -> stuck_signal option)`.

### 5.2 MemGPT / Letta Tool Rules Engine

- **Reference:** Letta documentation; arXiv:2310.08560 (MemGPT paper)
- **What it is:** Structured constraints on which tools can be called in which sequences, and what must happen after specific tool outcomes. A "tool rules" engine enforces a state machine over the tool call sequence.
- **Signals:** Tool call sequence, tool outcomes.
- **Weight:** Very low — state machine transition evaluation.
- **Separate model:** No.
- **Correction trigger:** Rule violation → inject error message, force a different tool, or abort.
- **Cost:** Negligible.
- **Example rule:** `after_tool_failure("bash", n=3) -> require_tool("summarize_failure")` — forces the agent to call a summarization tool after 3 bash failures, breaking the loop structurally.
- **Relevance to clawq:** Tool rules could be implemented as a small extension to clawq's tool_registry.ml. Rules that constrain post-failure behavior (e.g., "after N failures of tool X, next tool call must be something other than X") are structurally loop-breaking.

### 5.3 LangChain / LangGraph Guard Rails

- **What it is:** Max iterations and max execution time parameters on agent executor. In LangGraph, explicit state machine edges can express "if error_count > N, transition to error_handler node."
- **Signals:** Turn counter, wall clock, node state.
- **Weight:** Negligible to very low.
- **Separate model:** No.
- **Correction trigger:** Threshold exceeded → transition to recovery state or abort.
- **Cost:** Negligible.
- **Relevance to clawq:** The LangGraph node-based approach — where the agent loop is an explicit state machine with error states — is worth considering for clawq's agent.ml. The current turn loop could have explicit states: Normal, WarnedOnce, WarnedTwice, Aborting.

---

## 6. Production Engineering Lessons

### 6.1 Prompt Injection on Loop Detection Is the Most Deployed Pattern

When a loop is detected (by any heuristic), the most widely deployed correction mechanism is to inject a corrective system message into the next turn's context. Example wording that has been found effective:

> "WARNING: You have called {tool_name} {N} times with identical arguments and it keeps failing with: {error_message}. You MUST try a completely different approach. Do NOT call {tool_name} with the same arguments again."

If the agent continues past 2x the threshold (e.g., 10 identical calls when the threshold was 5), force-kill the session and notify the user. This two-stage approach (warn then kill) avoids false-positive hard kills while still guaranteeing eventual termination.

The key property of effective injected correction prompts is **specificity**: name the tool, name the argument pattern, name the error, and explicitly forbid the exact action. Generic "try something different" prompts are much less effective than specific prohibitions.

### 6.2 Context Compaction Makes Loops Worse

A key insight from Claude Code's issue tracker (issue #4277): context compaction (summarizing or truncating history to stay within context limits) can **erase evidence of prior failed tool calls** from the model's view. After compaction, the model has no memory of having already tried the failing approach and may retry it again, potentially triggering another round of context compaction.

The stuck detector must operate **outside the context window** — in the outer loop logic, in structured data structures in the process, not relying on the model's context. The outer loop retains the full uncompacted history of tool calls and errors as structured records. This is why a heuristic implemented in the agent's turn loop (agent.ml) is more reliable than asking the model to "remember" it already tried something.

### 6.3 Root-Cause vs. Surface-Level Correction

AgentDebug finding (section 4.2): targeting root-cause errors outperforms fixing surface symptoms by a significant margin. For clawq's observer, this implies:

- Don't just tell the model "retry with different arguments."
- Identify the _type_ of failure (wrong arguments, tool unavailable, permission error, logical error in the plan, missing prerequisite) and inject a targeted correction prompt that addresses the root cause.
- A permission error should trigger "check if you have permission before calling this tool again."
- A tool-not-found error should trigger "this tool is not available; use an alternative."
- A logical error (tool succeeds but produces wrong result) requires a different approach entirely.

### 6.4 False Positives Are a Real Deployment Problem

Cursor/Qwen3 community bug report (March 2026): loop detection fired on a custom model performing legitimate iterative code analysis — many tool calls with similar (not identical) arguments, all producing meaningful progress. The false positive caused premature termination of a productive session.

Tuning implications:
- Threshold of N=3 is too aggressive for general use; N=5-7 is safer.
- The strongest signal is **same tool + same args + same error**, not just same tool + same args.
- Exact argument matching is safer than fuzzy/semantic matching (fewer false positives, may miss some true positives).
- A "learning curve" approach: warn at N=5, escalate at N=8, abort at N=12 provides graduated response.

### 6.5 The Outer Loop Must Track Structured State

Every production stuck-detection implementation independently converges on the same architecture: a structured record maintained in the outer loop (not in the model's context) that tracks:
- Per-tool call counts in the current session.
- Per-(tool, args_hash) counts in the current session.
- Per-(tool, args_hash, error_hash) counts in the current session.
- Timestamps of last N tool calls (to detect time-based anomalies like polling loops).

This structured state is cheap to maintain (a few hash maps and counters), is unaffected by context compaction, and provides a reliable detection surface regardless of model behavior.

### 6.6 Distinguishing "Stuck" from "Slow"

A common false-positive category is mistaking a slow-progressing agent for a stuck agent. Signals that distinguish them:
- **Stuck:** Same tool, same args, same result (or same error). No observable state change between calls.
- **Slow:** Same tool, same args _prefix_, but different specific args each call (iterating through a list). Or: same tool, different args, different results (searching through options). Or: same tool, same args, but results are changing over time (polling a changing external resource).

The discriminator is whether the agent's observable environment state is changing between calls. If yes, the agent is progressing (possibly slowly). If no, the agent is stuck.

---

## 7. Summary Comparison Table

| Approach | Weight | Separate Model | Primary Signals | Correction Trigger | Per-Turn Cost |
|---|---|---|---|---|---|
| Hard turn/time cap | Negligible | No | Counter, clock | Abort | Zero |
| Sliding window repeat detection | Very low | No | Tool+args hash | Inject warning prompt | Zero |
| Same tool+error detection | Very low | No | Tool+error hash | Inject correction or abort | Zero |
| Semantic completion validation | Low-Medium | No | External state | Force loop continuation | Validation cost |
| Explicit stop token (ReAct) | Negligible | No | FINISH in output | Loop exit | Zero extra |
| Entropy monitoring | Low | No | Logprob entropy | Alert if below threshold | Near-zero (if logprobs available) |
| ReAct Thought monitoring | Negligible | No | Thought text patterns | External alert | Zero extra |
| Self-Refine (same model) | Medium | No | LLM self-critique | Refine step | 2-3x token cost |
| Reflexion (same or diff model) | Medium | Optional | Episode outcome + trajectory | Verbal reflection as next context | 1 extra call per episode |
| CRITIC (tool-grounded) | Medium | No | Tool verification result | Revision step | Tool cost + 1-2 LLM calls |
| Constitutional AI critique | Medium | No | Principle violations | Revision step | 2-3x token cost |
| Observer agent (AWS pattern) | Low-Medium | Yes | Telemetry, traces, logs | Alert or downstream agent | 1 extra call per observation cycle |
| Multi-LLM evaluator (AIME) | Heavy | Yes | Multi-dimensional quality | Ensemble consensus below threshold | N x model cost |
| Invariant declarative guardrails | Negligible | No | Tool call history, result diff | Rule-defined action | Zero |
| MemGPT tool rules engine | Negligible | No | Tool call sequence | State machine transition | Zero |
| Postmortem attribution (LLM) | High | Yes | Full failure log | None in-loop; feeds back to design | 1+ extra calls post-hoc |
| Experience buffer (ReflAgent) | Low (post-episode) | No | Episode outcome | Retrieved context at next attempt | Post-episode write cost only |

---

## 8. Layered Design Recommendation for clawq

Based on all findings, a layered approach is recommended. Each layer is independent; layers can be enabled or disabled by config without affecting the others.

### Layer 1 — Deterministic Heuristics (Zero Cost, Always On)

Implemented in `agent.ml`'s turn loop as a persistent `stuck_state` record maintained outside the context window. Evaluated on every tool call result.

Three independent detectors:

**1a. Turn cap:**
- Count total turns in session.
- At `max_turns` (configurable, default 50), abort with a clear user-facing message.
- Already partially implemented; formalize as a named threshold.

**1b. Sliding-window repeat detector:**
- Maintain a deque of last N (tool_name, hash(serialized_args)) pairs. N=7 default.
- If all N entries are identical: increment warn_count, inject Layer 2 warning.
- If warn_count exceeds 2 (14 identical calls total): abort session.

**1c. Same-tool-same-error detector:**
- Maintain a map from (tool_name, error_hash) to consecutive failure count.
- Reset count when a different tool or different error occurs.
- At count=3: inject Layer 2 correction. At count=6: abort.
- This is the strongest signal and should trigger at lower thresholds than 1b.

All three detectors are O(1) per turn, zero LLM calls, unaffected by context compaction.

### Layer 2 — In-Context Correction Injection (Zero Extra LLM Cost)

Triggered by Layer 1 detection. Constructs a targeted correction message injected as an additional user-visible assistant-addressed message into the next turn's context.

Message template (1b trigger):
> "SYSTEM OBSERVATION: You have called `{tool_name}` {N} times with identical arguments `{args_summary}`. No progress has been detected. You must change your approach. Do not call `{tool_name}` with these arguments again. Consider: (a) whether you have the right tool for this task, (b) whether a prerequisite step is missing, or (c) whether the task is currently impossible and should be reported as failed."

Message template (1c trigger — error loop):
> "SYSTEM OBSERVATION: You have called `{tool_name}` {N} times and received the same error `{error_summary}` each time. This error will not resolve by retrying. You must try a different approach. Specifically: {error_specific_guidance}."

Error-specific guidance is a small lookup table mapping common error patterns (ENOENT, EACCES, connection refused, tool not found, etc.) to actionable instructions. This lookup table is cheap to maintain and dramatically improves correction quality over a generic message.

### Layer 3 — Reflexion-Style Post-Attempt Reflection (Optional, Medium Cost)

Gated behind config flag `enable_session_reflection: false` (off by default). Activated when a session ends in a user-visible failure state.

After session failure:
1. Construct a Reflexion reflection prompt from the session transcript (tool calls, errors, final failure message).
2. Call the primary provider (no separate model needed) with the reflection prompt.
3. Store the reflection in the memory system under a key like `session_reflection:{session_id}`.
4. On the next session for the same task (detected by user re-submitting a similar request), retrieve and prepend the reflection as context.

This uses clawq's existing provider.ml and memory system. No new dependencies.

### Layer 4 — Async Postmortem Command (Future)

A `session postmortem SESSION_ID` CLI command that:
1. Retrieves the full tool call log for the session from SQLite.
2. Constructs a failure attribution prompt (Step-by-Step or Binary Search method from Zhang et al.).
3. Calls the provider and returns a human-readable failure analysis.
4. Optionally stores the analysis as a session_reflection for future retrieval.

Important caveat to surface in UX: automated postmortem attribution has only ~53% accuracy (Zhang et al., ICML 2025). Present results as diagnostic hints, not authoritative root-cause analysis.

### What NOT to Build (at least initially)

- A separate observer LLM model — adds operational complexity and cost without proportionate benefit over Layer 1 heuristics.
- Multi-LLM evaluator ensembles — too expensive per turn for a general-purpose assistant runtime.
- Entropy monitoring — requires logprob API access that Anthropic and most providers do not expose.
- Full Constitutional AI pipeline — appropriate for safety/alignment work, not for stuck-loop detection specifically.
- Semantic similarity matching for "near-identical" argument detection — introduces false positives and is much more complex to implement than exact hash matching, for marginal gain.

### Key Architectural Principle

The stuck detector must live **outside the context window** in the outer loop. Context compaction erases failure evidence from the model's view. Only the outer loop's structured state (the `stuck_state` record in the Lwt agent fiber) retains the complete uncompacted history. This is the single most important architectural constraint identified across all reviewed systems.

---

## Sources

- arXiv:2303.11366 — Reflexion (Shinn et al., NeurIPS 2023): https://arxiv.org/abs/2303.11366
- arXiv:2303.17651 — Self-Refine (Madaan et al., NeurIPS 2023): https://arxiv.org/abs/2303.17651
- arXiv:2305.11738 — CRITIC (Gou et al., 2023): https://arxiv.org/abs/2305.11738
- arXiv:2210.03629 — ReAct (Yao et al., 2022): https://arxiv.org/abs/2210.03629
- arXiv:2203.11171 — Chain-of-Thought Self-Consistency (Wang et al., 2022): https://arxiv.org/abs/2203.11171
- arXiv:2212.08073 — Constitutional AI (Anthropic, 2022): https://arxiv.org/abs/2212.08073
- arXiv:2505.00212 — Failure Attribution (Zhang et al., ICML 2025): https://arxiv.org/abs/2505.00212
- arXiv:2509.25370 — AgentDebug: https://arxiv.org/pdf/2509.25370
- arXiv:2310.08560 — MemGPT (Packer et al., 2023): https://arxiv.org/abs/2310.08560
- AWS Prescriptive Guidance — Observer and Monitoring Agents: https://docs.aws.amazon.com/prescriptive-guidance/latest/agentic-ai-patterns/observer-and-monitoring-agents.html
- Invariant Guardrails loop detection docs: https://explorer.invariantlabs.ai/docs/guardrails/loops/
- Letta V1 Agent Loop blog: https://www.letta.com/blog/letta-v1-agent
- Claude Code feature request #4277 (Loop Detection Service): https://github.com/anthropics/claude-code/issues/4277
- Claude Code issue #5962 (agent resilience, stuck tool-call loops): https://github.com/openclaw/openclaw/issues/5962
- fixbrokenaiapps.com — Why AI Agents Get Stuck in Loops: https://www.fixbrokenaiapps.com/blog/ai-agents-infinite-loops
- Sketch.dev agent loop blog: https://sketch.dev/blog/agent-loop
- Vercel AI SDK Loop Control docs: https://ai-sdk.dev/docs/agents/loop-control
- Promptingguide.ai Reflexion overview: https://www.promptingguide.ai/techniques/reflexion
- Cursor forum — false positive loop detection report (March 2026): https://forum.cursor.com/t/false-positive-loop-detection-when-using-custom-model-qwen3-coder-plus-with-repetitive-reasoning-text-before-different-tool-calls/145252
