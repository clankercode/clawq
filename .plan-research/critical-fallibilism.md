# Critical Fallibilism: Research Report for AI Agent Design

**Research date:** 2026-03-10
**Primary source:** criticalfallibilism.com (Elliot Temple)
**Purpose:** Design guidance for observation-correction loops, stuck detection, and iterative improvement in AI agents (Issue I016)

---

## Table of Contents

1. Executive Summary
2. What is Critical Fallibilism?
3. Core Epistemological Principles
4. Overreach: The Error Budget Model
5. Paths Forward: Systematic Error Correction Architecture
6. Theory of Constraints Integration
7. Error Correction Mechanisms and Taxonomy
8. Getting Stuck: Detection and Recovery
9. Software Design Patterns Derived from CF
10. AI Agent Correction Loop Design
11. Key Quotes
12. Sources

---

## 1. Executive Summary

Critical Fallibilism (CF) is a philosophy of reason developed by Elliot Temple that synthesizes Karl Popper's Critical Rationalism, Eli Goldratt's Theory of Constraints, and Ayn Rand's Objectivism. It provides a rigorous, actionable framework for how any learning system - human, institutional, or artificial - should handle the inevitability of error.

The central insight is this: **the primary metric of a healthy reasoning system is whether its rate of error correction can keep pace with its rate of error creation.** When correction lags behind creation, backlogs accumulate, the system becomes overwhelmed, and correction mechanisms get bypassed. This failure mode - called "overreach" - is the root cause of most chronic failure in learning systems.

For AI agent design, CF yields six concrete, non-obvious principles:

1. **Binary evaluation over credence scores.** Errors are either known (refuted) or not (non-refuted). Confidence distributions are a category error at decision boundaries.
2. **Error correction is a budgeted resource.** Agents must not generate more errors than they can process. This caps acceptable task complexity and step size.
3. **Every reasoning path must have a Path Forward.** The question "if I am wrong, how will I find out?" must have a concrete answer at every decision point.
4. **Digital (discrete) representations are required for error detection.** Analog/continuous reasoning resists clean error identification and correction.
5. **Bottleneck first.** Optimizing non-bottleneck components wastes resources. Find the constraint and buffer it.
6. **Stuck detection is quantitative.** A success rate below 75% means learning is stalled; below 50% means severe difficulty. Exponential difficulty reduction (not linear) is the correct recovery response.

---

## 2. What is Critical Fallibilism?

### Origins and Intellectual Lineage

Critical Fallibilism was developed by Elliot Temple as an original philosophy that builds upon and extends three prior frameworks:

**Karl Popper's Critical Rationalism (CR):** The foundational epistemology. Humans cannot guarantee the truth of ideas. Knowledge grows through cycles of conjecture and refutation - generating ideas, then finding and eliminating errors. Induction is rejected as logically flawed. Falsifiability is the criterion of scientific claims. Learning is an evolutionary process: brainstorming replicates and varies ideas; criticism does selection.

**Eli Goldratt's Theory of Constraints (TOC):** A management and systems theory focused on bottlenecks. In any system, one constraint limits throughput. Most components have excess capacity. Optimizing non-constraints is wasted effort. Buffers should be maintained at constraints, not distributed uniformly. Variance in any subsystem degrades downstream throughput unless buffered.

**Ayn Rand's Objectivism:** CF adopts the Objectivist account of concept formation - learning requires integration (combining simpler ideas into higher-level concepts) and automatization (practicing until behaviors become intuitive). Mastery requires deliberate practice to make correct behavior automatic.

### What CF Adds That Is Original

Temple's contributions beyond these sources include:

- **Yes/No Philosophy:** All ideas are binary - refuted or non-refuted. Not stronger or weaker, not more or less probable.
- **IGC Analysis:** Idea-Goal-Context triples as the unit of evaluation. An idea is not good or bad in isolation; it is non-refuted or refuted relative to a specific goal in a specific context.
- **Overreach:** The formal concept of error rate exceeding error correction rate as the primary failure mode of learning systems.
- **Paths Forward:** A systematic architecture ensuring that any error a person holds that someone else can identify has a route to being corrected.
- **Breakpoints:** The concept that analog spectrums have discrete qualitative thresholds. Most positions on a spectrum are equivalent; only positions near breakpoints matter.
- **Rejection of analog epistemology:** The explicit argument that analog/continuous representations of argument quality are incompatible with genuine error correction.

---

## 3. Core Epistemological Principles

### 3.1 Fallibilism: Errors Are Normal, Not Exceptional

The foundational commitment: humans are unavoidably capable of making mistakes. This is not a theoretical caveat but a practical reality - mistakes are common even when people feel confident they are correct. CF insists on accepting this genuinely rather than as a formality.

The practical consequence: a system that treats errors as exceptional events to be minimized will fail differently than a system designed around the expectation that errors are routine and must be continuously found and corrected. The former tries to prevent errors; the latter tries to process them.

### 3.2 Error Correction Over Justification

CF rejects "justificationism" - the belief that ideas can be positively supported by evidence or argument. The logical asymmetry is decisive: a single counterexample refutes a universal claim, but any number of confirming instances cannot prove it. One orange raven refutes "all ravens are black." A thousand black ravens prove nothing about the next raven.

The practical consequence for reasoning systems: **do not accumulate positive evidence for a hypothesis. Instead, actively seek refutations.** The correct response to "this has worked ten times" is not increased confidence - it is identifying the conditions under which it would fail.

### 3.3 Binary Evaluation (Yes/No Philosophy)

An idea evaluated for a specific goal in a specific context is either:
- **Non-refuted:** No known error has been identified
- **Refuted:** At least one known error has been identified

There is no spectrum. There is no "mostly correct." There is no "I give this 0.73 credence."

This matters because analog evaluation is incompatible with error correction. If you score an idea 0.73, what does finding a new error do? Lower it to 0.68? At what point is it "wrong enough" to stop using? The binary framework makes this clean: a single decisive refutation ends the idea's use for that goal in that context. A non-refuted idea is used regardless of how many near-misses or warning signs exist (because those are themselves refutations if they are real).

**Decisive arguments:** A refutation is not "this seems questionable" or "I have some doubts." It is a specific statement that contradicts the idea and explains why it fails at the goal. Vague concerns are not refutations - they are prompts to formulate refutations.

### 3.4 IGC Framework: Idea-Goal-Context

Ideas are not evaluated in the abstract. The evaluation unit is a triple: (idea, goal, context).

- "Use a hammer" is non-refuted for (driving nails, wooden construction).
- "Use a hammer" is refuted for (serving soup, any context).

The same idea can be simultaneously non-refuted for some goals and refuted for others. This prevents the false dichotomy of ideas being globally "good" or "bad."

For AI agents: actions, plans, and sub-goals should be evaluated against specific success criteria in specific contexts, not rated globally. A plan that succeeds in 80% of cases is not "mostly good" - it is refuted for the 20% case and the agent needs to know what distinguishes those cases.

### 3.5 Digital Error Correction Requires Discrete Representations

CF makes a formal claim: **error correction requires digital (discrete) issues.** Analog representations - continuous spectra of quality, confidence distributions, strength-of-argument measures - are fundamentally incompatible with error correction.

The argument: to correct an error, you must be able to identify it as an error. Identification requires a boundary - a point where something crosses from non-error to error. Analog representations have no such boundaries unless you impose them artificially (which is introducing a digital distinction). Without a clean boundary, there is no fact of the matter about whether a correction succeeded.

The practical consequence: design decision points as binary thresholds, not as continuous optimization targets. A plan either succeeds or fails at the goal. A test either passes or does not. A claim is either refuted by evidence or it is not.

### 3.6 Evolutionary Epistemology

Knowledge creation is an evolutionary process:
- Brainstorming generates variation (new ideas, approaches, hypotheses)
- Criticism performs selection (eliminating ideas with identified errors)
- Surviving ideas are the current best knowledge, held tentatively

This is not a metaphor - it is the same logical structure as biological evolution: replication with variation and selection. Intelligence "literally evolves ideas in the subconscious mind on the same principle."

The practical consequence for iterative agents: do not treat each attempt as independent. Treat it as one generation in an evolutionary sequence. What was learned about why the last attempt failed should directly inform variation in the next attempt. Random restart after failure is the wrong model; directed variation based on error analysis is the right one.

---

## 4. Overreach: The Error Budget Model

### 4.1 Core Definition

"Overreach is about considering and managing your rate of making errors compared with your rate of correcting errors. If your error rate exceeds your error correction rate, then you're doing stuff that's too hard for you."

This is the central failure mode analysis in CF. It is not about making mistakes - mistakes are inevitable. It is about the ratio of creation to correction.

### 4.2 The Backlog Mechanism

When a system creates errors faster than it corrects them:

1. An unsolved problems backlog begins accumulating
2. As the backlog grows, each new error is less likely to be addressed
3. Encountering more errors while already overwhelmed makes criticism feel hostile rather than helpful
4. The system begins ignoring errors rather than correcting them
5. This further increases the effective error creation rate (errors are no longer even registered)
6. The system enters a positive feedback loop of increasing backlog and decreasing correction

This is not a theoretical concern - it is the normal endpoint of overreach. People who overreach "may fail a bunch, get stuck, and give up without ever trying easy enough activities."

### 4.3 The Budget Metaphor

Error correction ability is a resource budget that constrains what can be done successfully. Properties of this budget:

- **It is finite at any given time.** You cannot correct unlimited errors simultaneously.
- **It can grow.** Improving rationality skills, mastery, and automatization increases the budget.
- **Exceeding it produces less, not more.** Attempting tasks beyond budget means higher failure rates and lower throughput, not just "somewhat lower success."
- **Budget growth requires investment.** The path to harder tasks is through increasing correction capacity first, not through attempting harder tasks directly.

### 4.4 The Technical Debt Analogy

CF explicitly maps overreach to software technical debt:

"Going into error correction debt is like having software that's disorganized and full of bugs (called 'technical debt'), and then writing more buggy code instead of fixing stuff. The more technical debt there is, the more effort it takes to make changes like adding new features or fixing bugs. Companies go out of business due to technical debt."

This analogy is structurally precise, not just illustrative. Technical debt IS accumulated overreach in a software system. The mechanism is identical: errors (bugs, design problems) accumulate faster than they are corrected, making the system increasingly brittle and costly to modify, until forward progress becomes impossible.

### 4.5 The 20% Rule

The recommended step size for sustainable progress: **the next task should be at most 20% harder than a previously demonstrated success.**

This is not a conservative heuristic - it is derived from the error budget constraint. If you can succeed at level X, you have demonstrated error correction capacity sufficient for X. Attempting X + 20% means adding a bounded amount of new error surface while maintaining the core correction capacity. Attempting X * 2 introduces error surface that likely exceeds correction capacity.

The rule requires maintaining a track record of objective successes against which new tasks can be measured. Vague self-assessment is insufficient; the track record must be specific and externally verifiable.

### 4.6 Error Correction Debt as Worse Than Financial Debt

CF argues that error correction debt is structurally worse than credit card debt because:

- Financial debt has explicit interest rates and balances that are visible and measurable
- Error correction debt compounds invisibly - each uncorrected error increases the cost of future corrections
- Financial debt does not directly degrade your ability to earn income; error correction debt directly degrades your ability to correct further errors
- Financial bankruptcy is recoverable; systems that completely lose error correction capacity may not be recoverable without external intervention

---

## 5. Paths Forward: Systematic Error Correction Architecture

### 5.1 The Core Question

The Paths Forward concept is organized around one question: **"If I am wrong about this, and someone else knows it, how will they be able to tell me?"**

Most intelligent people try to seek truth but lack systematic mechanisms for error correction. Even when someone outside your social circle understands your mistake and wants to share that understanding, the connection often never happens. CF calls this "avoidable failure" - staying wrong about issues that others have already solved and are willing to explain.

### 5.2 The Operational Definition

A Path Forward is: "a way to make progress - a way to find out about and correct a mistake."

For any belief, claim, or plan, a Path Forward answers: given that this might be wrong, what is the concrete mechanism by which that wrongness would be detected and corrected? If no such mechanism exists, the belief is held in an error-correction-free zone. That is the vulnerability.

### 5.3 The Three-Step Implementation

**Step 1: Document positions in writing.**
"Don't just learn things. Get them in writing." Written positions create stable reference points that can be examined, criticized, and revised. Unwritten positions can shift post-hoc, resist examination, and cannot be compared against criticisms.

**Step 2: Address objections explicitly.**
Less than half the written material should state positive positions. The majority should answer "potential questions and criticisms, and criticisms of contradictory rival positions." The focus is on objections, not affirmations.

**Step 3: Enable efficient engagement.**
Pre-written responses to common objections mean critics can be answered quickly unless they raise something new. This scales: the same written response serves many critics, making comprehensive engagement tractable. The guideline: "address specifics three times then cover the more general issue," preventing endless repetition cycles.

### 5.4 Transparency Requirements

CF specifies that error correction policies must be written, public, and followed consistently. The argument against unstated methodology is structural: "If they don't have a methodology written down, that allows them to act on their biases; it allows them to ignore critics and criticism based on social status; and it makes it difficult for anyone to critique their methodology."

Unstated methodology creates a system that appears open to correction but is actually closed to it, because the criteria for dismissing criticism are themselves hidden from criticism.

### 5.5 The Self-Trust Problem

Without Paths Forward, the default failure mode is relying solely on personal judgment to assess criticisms. This is structurally broken because:

- Personal judgment has biases that are invisible to the judge
- Social status influences which criticisms are taken seriously (expert criticism vs. outsider criticism)
- Unconventional correct ideas are systematically suppressed by popularity-based filtering
- "Good ideas do not float to the top" - current systems are not designed to make that work

CF's response: all criticism should be addressed. You cannot know a criticism is incorrect until after you have refuted it. Treating unrefuted criticism as obviously wrong is a rationality failure, not a time-saving measure.

---

## 6. Theory of Constraints Integration

### 6.1 Core TOC Principles

Goldratt's Theory of Constraints, as integrated into CF:

- Every system has exactly one current constraint (bottleneck) limiting throughput
- The constraint limits the whole system regardless of performance elsewhere
- Most components have excess capacity relative to the constraint
- Optimizing non-constraint components does not increase throughput
- "Optimization away from the constraint is wasted"

### 6.2 Excess Capacity as Buffer

In any healthy system, most components perform above the minimum required. This excess capacity serves as a buffer against variance. The failure mode of "balanced" systems (equal capacity everywhere) is that statistical fluctuations cause downstream starvation - one component running slightly slow cascades into blocked queues throughout.

CF translates this directly: **most factors in a decision or system do not matter to the outcome because they have excess capacity.** They are well above whatever threshold would change the outcome. Only factors at or near a breakpoint matter.

This gives the practical rule: identify which factor is the actual constraint, and direct all optimization effort there. Improvements to non-constraint factors do not improve outcomes regardless of how much effort is invested.

### 6.3 Breakpoints: Where Analog Becomes Digital

A breakpoint is a position on a continuous spectrum where crossing it produces a qualitative change, not merely a quantitative one. Examples:
- A rope with excess load capacity: adding 1kg changes nothing until the breakpoint where it snaps
- A vote count: 50.01% and 49.99% are not "similar" - they have categorically different outcomes
- Code compilation: a program almost compiles is categorically different from one that does

The CF insight: **most positions on any continuous spectrum are not near a breakpoint.** This means most changes to a factor do not matter. The value curve is flat almost everywhere and steep only near breakpoints. Linear improvement assumptions are systematically wrong.

### 6.4 Applying TOC to Error Correction

In an error correction system, the bottleneck is wherever errors accumulate waiting to be addressed. The correct intervention:

1. Identify the constraint: where does the error backlog form?
2. Exploit the constraint: increase throughput at that specific point
3. Subordinate everything else: ensure non-constraint components support the constraint
4. Elevate the constraint: if still limiting, increase capacity there specifically
5. Repeat: the next constraint becomes the new bottleneck

For AI agents, this means: before optimizing planning, tool use, response quality, or any other dimension, identify whether there is an error correction bottleneck. If the agent cannot process its own errors faster than it creates them, all other optimization is secondary.

---

## 7. Error Correction Mechanisms and Taxonomy

### 7.1 Types of Error Correction

**Explanatory error correction:** Understanding *why* an error occurred, then developing an alternative solution that avoids that specific flaw. This is the primary mode in CF - it produces insight that generalizes beyond the specific error.

**Quantitative error correction:** Addressing measurable deviations through repeated measurement and averaging, statistical analysis, or calibration. This is appropriate for measurement errors but does not generalize - it does not explain the source of variation.

CF prioritizes explanatory correction because it creates knowledge (understanding of the error mechanism) rather than just data (a better estimate).

### 7.2 Error Taxonomy

**By source:**
- Categorical confusion (treating different kinds of things as the same kind)
- Non sequiturs (conclusions not following from premises)
- Overlooked factors (missing relevant considerations)
- Wrong premises (starting from false assumptions)
- Incorrect logical connections (valid-seeming but invalid inferences)
- Vagueness (claims not precise enough to be refuted)
- Intentional falsehoods

**By pattern:**
- Systemic vs. isolated (recurring vs. one-off)
- Habitual vs. occasional (deeply automatized vs. situational)
- High-reach vs. bounded (affecting many downstream beliefs vs. contained)

**By action type:**
- Errors of omission (failing to do something necessary)
- Errors of commission (doing something harmful or unnecessary)

**By autonomy:**
- Context-dependent (only appear in specific situations)
- Self-perpetuating (create conditions that cause further instances of the same error)

### 7.3 Constraining Solution Space for Error Detection

A critical operational principle: **constrain the solution space to enable error detection.** If any output is acceptable, there is no error to detect. Error detection requires knowing what the correct output would look like, which requires a defined success criterion.

For AI agents: tasks that do not have clear success criteria cannot have their errors detected. Before attempting a task, define what constitutes success and what constitutes failure. "Do a good job" is not a testable criterion. "The compiled output produces the same observable behavior as the original" is.

### 7.4 Automatization and Practice

Errors that require conscious correction at every occurrence cannot be corrected reliably under load. The goal is to automatize correct behavior through practice until it runs without conscious attention. Only then is the correction robust.

Corollary: **do not attempt tasks that require simultaneous conscious management of too many correction loops.** Two new things to manage at once is twice as hard; three things is more than three times harder. The difficulty scaling is superlinear because conscious error correction is a shared resource.

---

## 8. Getting Stuck: Detection and Recovery

### 8.1 The Stuck Mechanism

Getting stuck is the end state of unrecovered overreach. The path:

1. System attempts tasks harder than its error correction budget allows
2. Failures accumulate and are not resolved
3. Each failure makes the next attempt harder (discouragement, compounding errors)
4. Eventually the system cannot make progress on any front related to the original goal
5. "Stuck" is not the absence of effort - it is effort that produces no forward progress

A critical failure mode: people often do not recognize when they are stuck. "Many people think they won't get stuck as others did, so they think it's OK for them to do hard, ambitious, complicated stuff without working up to it. Then when they do get stuck, they don't change their attitude effectively, so they stay stuck."

### 8.2 Quantitative Detection Thresholds

CF provides specific, numerical success rate thresholds for detecting stuck states:

| Success Rate | Status |
|---|---|
| > 90% | Healthy - appropriate difficulty level |
| 75-90% | Acceptable - some overreach, monitor |
| < 75% | Learning is stalling |
| ~50% | Severe difficulty - significant overreach |
| < 50% | Likely in stuck state |

These are not vague guidelines - they are operationally meaningful thresholds. If your success rate on mini-projects or sub-tasks is below 75%, the current approach is not working and should be changed before continuing.

### 8.3 Project Granularity

"People often get stuck because they fail to break problems down into more manageable parts and instead try to do the whole thing at once."

Key guidelines for project structure:
- Each step should produce approximately 1-2 errors (not zero, which means no new learning; not twenty, which means the step is too large)
- Success criteria should be clear and binary at each step
- Each step should be short enough to evaluate within approximately one week
- Steps should build on each other - isolated successes that don't accumulate are not progress

The one-to-two errors guideline is counterintuitive: **zero errors per step is a warning sign, not a success signal.** It means the step was too easy to produce learning. The goal is calibrated difficulty, not maximum difficulty or minimum difficulty.

### 8.4 Exponential Backoff on Failure

When failures occur, the correct response is **exponential reduction in difficulty, not linear reduction:**

- One failure: reduce to one-third the previous difficulty
- Two consecutive failures: reduce to one-ninth the previous difficulty
- Pattern: difficulty ~ (1/3)^(number_of_consecutive_failures)

The intuition: if you failed at level X, you don't know how far below X your actual capacity is. Linear reduction (try X - 1) assumes the gap is small. Exponential reduction finds your actual capacity quickly and then builds back up from a solid foundation.

### 8.5 Recovery Protocol

After getting stuck:
1. Recognize the stuck state explicitly (use the quantitative thresholds)
2. Stop the current approach - do not push through
3. Reduce to a task you can succeed at with high probability (> 90% success rate)
4. Build a track record of successes at that level
5. Increase difficulty by at most 20% per step, with objective success criteria
6. Do not resume the original approach until you have demonstrated the prerequisite skills

The key error in recovery: "Many people accomplish more than they realize but dismiss easy wins as insignificant." Reframing quick successes as legitimate progress is not lowering the bar - it is accurately calibrating position.

---

## 9. Software Design Patterns Derived from CF

### 9.1 Binary Exit Conditions Over Confidence Thresholds

**Pattern:** Replace "confidence > 0.8 → proceed" with explicit pass/fail criteria derived from the goal.

**Rationale:** Confidence scores are analog representations. The question is not "how confident am I?" but "has this been refuted?" Define what would constitute a refutation of the current plan, and check for it. If no refutation has been found, proceed. If one has, the plan is refuted and must be replaced - not adjusted by 20%.

**Implementation:** For each decision point, specify: "This branch is refuted if [specific condition]. This branch is non-refuted if [specific condition]." These conditions must be checkable, not estimated.

### 9.2 Explicit Error Budget Accounting

**Pattern:** Track the ratio of errors generated to errors resolved. When this ratio exceeds 1.0 (more errors generated than resolved per time unit), halt new work and process the backlog.

**Rationale:** This is the operational implementation of the overreach model. The specific threshold (1.0) is where the backlog begins growing. Maintaining the ratio at or below 1.0 prevents the positive feedback loop of increasing backlog and decreasing correction capacity.

**Implementation:** Maintain explicit counts of open errors (failures, unexpected behaviors, unresolved questions) and resolved errors per iteration. If open count is increasing, the system is in overreach.

### 9.3 Mandatory Paths Forward at Decision Points

**Pattern:** At any decision fork, document the answer to: "If this path is wrong, how will we detect it and what will we do?"

**Rationale:** Decisions made without a Path Forward cannot be corrected if wrong. The correction mechanism must be defined before the decision is made, not retrospectively when problems appear.

**Implementation:** Decision records include: the decision, the criteria that would indicate it was wrong, the monitoring mechanism for those criteria, and the fallback if those criteria are met. Decisions without this structure are flagged as incomplete.

### 9.4 Constraint-First Optimization

**Pattern:** Before optimizing any system component, identify the current bottleneck. Only optimize the bottleneck. Log all other optimization attempts as deferred until the bottleneck changes.

**Rationale:** TOC applied directly. Optimizing non-bottleneck components does not improve throughput. It wastes resources and may mask the actual bottleneck.

**Implementation:** Profile the system's throughput and identify the single slowest stage. Fix it. Re-profile. Repeat. Resist the temptation to fix the second-slowest stage while the slowest is still the constraint.

### 9.5 Discrete State Representation at Decision Boundaries

**Pattern:** At any point where a decision is made, represent the relevant state as a discrete category, not a continuous value. Define the breakpoints explicitly.

**Rationale:** Error correction requires identifying when the system has crossed from an acceptable state to an unacceptable one. This requires a defined boundary. Continuous representations lack clean boundaries unless they are discretized.

**Implementation:** Define state categories with explicit thresholds. "Response quality: [acceptable | needs_revision | rejected]" is a discrete representation. "Response quality: 0.73" is not - it requires a further decision about what threshold matters, which defers the binary evaluation.

### 9.6 Granular Task Decomposition with Observable Success Criteria

**Pattern:** Decompose tasks until each sub-task has: a clear binary success criterion, an expected completion time short enough to detect failure before compounding, and a dependency structure that makes error propagation visible.

**Rationale:** The 1-2 errors per step guideline requires steps small enough to isolate individual error sources. Large steps mix multiple error sources, making it impossible to identify which error caused the failure.

**Implementation:** If a task is failing, decompose it further. If it is passing, the decomposition may be fine-grained enough. The test: can you articulate exactly what went wrong in a failure? If not, the task is too coarse.

### 9.7 Exponential Backoff in Retry Logic

**Pattern:** When a task or step fails consecutively, reduce its scope/complexity exponentially before retrying. Do not retry at the same difficulty level.

**Rationale:** Consecutive failures at the same difficulty level are evidence that the difficulty exceeds current capacity. Linear reduction (reduce by X) does not adequately escape the capacity zone. Exponential reduction (divide by 3 per failure) reaches the actual capacity floor quickly.

**Implementation:** Failure count drives a difficulty multiplier: 1 failure → 0.33x, 2 failures → 0.11x, 3 failures → 0.04x. Rebuild difficulty from the recoverable level using the 20% rule.

---

## 10. AI Agent Correction Loop Design

### 10.1 The Fundamental Architecture Requirement

An AI agent that cannot correct its own errors is not a learning system - it is a lookup system. CF defines what genuine error correction requires:

1. **Error detection:** The agent must be able to identify that an error occurred. This requires binary success criteria at each step.
2. **Error attribution:** The agent must be able to identify *why* the error occurred (explanatory correction, not just quantitative correction).
3. **Error response:** The agent must modify its approach in a way that specifically avoids the identified error source.
4. **Path Forward:** The agent must have a mechanism by which external sources of error knowledge (users, tests, environment feedback) can reach and modify its behavior.

### 10.2 Observation-Correction Loop Structure

The canonical CF-aligned agent loop:

```
1. STATE: Non-refuted current plan (held tentatively)
2. EXECUTE: Take one discrete step
3. OBSERVE: Collect result against binary success criterion
4. EVALUATE (binary):
   a. Success: plan is non-refuted for this step, continue
   b. Failure: plan is refuted for this context, branch to correction
5. CORRECTION BRANCH:
   a. Attribute: why did this fail? (explanatory correction)
   b. Check error budget: am I generating more errors than I can process?
   c. If budget exceeded: reduce task scope by factor of 3, rebuild
   d. If budget OK: generate variant that avoids the identified flaw
   e. Update plan (tentatively non-refuted again)
6. PATH FORWARD CHECK: Is there still a mechanism by which an undetected error could be reported?
7. GOTO 2
```

The key non-obvious elements:
- Step 4 is binary. Not "how successful was this?" but "did it meet the criterion or not?"
- Step 5c (exponential backoff) triggers on *budget* overreach, not individual failures
- Step 6 is not optional - it is the systemic health check

### 10.3 Stuck Detection in Agents

An agent is in a stuck state when:
- Success rate over a sliding window drops below 75%
- Error backlog (unresolved failures) is increasing over time
- The agent is modifying the same plan component repeatedly without improvement
- The agent is generating new variants without incorporating previous failure analysis

Stuck detection should be a first-class mechanism, not inferred post-hoc. The agent should maintain:
- A success/failure rate over recent attempts (sliding window of ~10 steps)
- An open error backlog count
- A flag for whether recent failures have received explanatory attribution

### 10.4 The "Paths Forward" Requirement for Agents

An AI agent must always maintain a Path Forward - a route by which errors it holds can be corrected. This has specific implications:

**Explicit reasoning:** If the agent's reasoning is opaque, no external observer can identify where an error exists. "If you're mistaken, it's very hard for anyone to tell you unless you share your reasoning." Agent outputs should include the reasoning chain, not just conclusions.

**Written-down criteria:** The criteria by which the agent accepts or rejects information must be explicit and consistent. If these criteria are hidden or variable, they cannot be examined or corrected.

**Comprehensive engagement with corrections:** When the agent receives a correction, it must engage with it. Discarding corrections without refuting them is rationality failure. The agent should either: (a) incorporate the correction, or (b) produce a specific refutation of why the correction does not apply.

**No permanent dismissal without refutation:** Filtering corrections based on their source (low-confidence signal, user with no track record, etc.) is permissible for prioritization but not for permanent dismissal. A correction remains open until it is refuted.

### 10.5 Overreach Detection in Agent Task Selection

Before accepting a task, an agent should evaluate whether the task is within its error correction budget:

1. **Establish baseline:** What is the most recently demonstrated successful task of this type?
2. **Estimate difficulty differential:** Is the new task more than 20% harder than that baseline?
3. **Check current error budget:** Is the current backlog of unresolved errors within normal range?
4. **If within budget:** Proceed with explicit Path Forward and binary success criteria
5. **If outside budget:** Decompose the task before proceeding, or explicitly flag that the task may exceed current capacity

This is not risk-aversion - it is maintaining the error correction capacity required for genuine progress. An agent that systematically attempts tasks beyond its correction budget will accumulate a growing error backlog and eventually stall.

### 10.6 The EmberCF Project

A GitHub organization (github.com/EmberCF) has begun building CF-based toolkits for AI agents. The project description: "A Critical Fallibilism toolkit for AI agents offering frameworks for clearer thinking, error correction, and productive engagement with ideas."

The project appears oriented toward:
- Structured decision frameworks based on CF's binary evaluation
- Error correction mechanisms formalized for agent use
- Rationality skills packaged for agent consumption

This confirms the tractability of applying CF to agent design and suggests the community has already begun formalizing these patterns.

### 10.7 CF's Critique of Current AI Reasoning

From the "Error Correction and AI Alignment" article, Temple's critique of AI alignment discourse has structural implications for agent design:

- AI systems that do not document their reasoning cannot be externally corrected
- Systems that dismiss criticisms without refuting them are closed to correction
- Systems built on unstated methodologies cannot have their methodology criticized or improved
- The absence of visible debate records means there is no way to verify that corrections have occurred

These are not problems unique to AI alignment - they are architectural properties. Any AI agent that hides its reasoning, dismisses corrections without explicit refutation, or uses unstated criteria for evaluation is structurally resistant to error correction regardless of its task domain.

---

## 11. Key Quotes

"Overreach is about considering and managing your rate of making errors compared with your rate of correcting errors. If your error rate exceeds your error correction rate, then you're doing stuff that's too hard for you."
— *Overreach Summary*, criticalfallibilism.com

"Going into error correction debt is like having software that's disorganized and full of bugs (called 'technical debt'), and then writing more buggy code instead of fixing stuff. The more technical debt there is, the more effort it takes to make changes."
— *Overreach Summary*, criticalfallibilism.com

"Error correction requires digital issues."
— *Critical Fallibilism, Evolution and Digital Error Correction*, criticalfallibilism.com

"A central epistemological idea is fallibility: people are unavoidably capable of making mistakes. And this isn't just a theoretical issue; mistakes are common. So CF emphasizes finding and correcting mistakes. That's the key to learning and thinking - reason is about being good at finding and correcting mistakes."
— *Critical Fallibilism and Critical Rationalism Bullet Points*, criticalfallibilism.com

"Optimization away from the constraint is wasted."
— *Critical Fallibilism and Theory of Constraints in One Analyzed Paragraph*, criticalfallibilism.com

"If I'm wrong, and you know it, how can I find out?"
— *Paths Forward Summary*, criticalfallibilism.com

"Public intellectuals should have written policies for how they deal with critics and have transparency so people can see the policies are followed."
— *Paths Forward Summary*, criticalfallibilism.com

"If you're mistaken, it's very hard for anyone to tell you unless you share your reasoning."
— *Error Correction and AI Alignment*, criticalfallibilism.com

"They shouldn't ignore some critics or criticisms for no clear, predictable, understandable, written-down-in-advance reasons."
— *Error Correction and AI Alignment*, criticalfallibilism.com

"Having two things to worry about at once is twice as hard as dealing with one. Having three things is more than three times harder than one."
— *Learning Many Small Skills Instead of Getting Stuck*, criticalfallibilism.com

"People often get stuck because they fail to break problems down into more manageable parts and instead try to do the whole thing at once."
— *Critical Fallibilism and Critical Rationalism Bullet Points*, criticalfallibilism.com

"Brainstorming replicates and varies ideas while criticism does selection."
— *Introduction to Critical Rationalism*, criticalfallibilism.com

"A single counter-example can refute an idea, while many compatible pieces of evidence cannot prove an idea true."
— *Introduction to Critical Rationalism*, criticalfallibilism.com

"We can at least correct errors that other people already understand and are willing to explain to us. That's a relatively easy, accessible opportunity. There's no need to stay wrong about issues that people have already figured out and are willing to share information about."
— *Paths Forward Summary*, criticalfallibilism.com

"The desire to always update the credence on any good or bad evidence is wrong! They don't know you can have excess!"
— *Critical Fallibilism and Theory of Constraints in One Analyzed Paragraph*, criticalfallibilism.com

---

## 12. Sources

### Primary: criticalfallibilism.com articles (fetched directly)

- https://criticalfallibilism.com/introduction-to-critical-fallibilism/
- https://criticalfallibilism.com/introduction-to-critical-rationalism/
- https://criticalfallibilism.com/overreach-summary/
- https://criticalfallibilism.com/error-correction-math-and-types/
- https://criticalfallibilism.com/error-correction-and-ai-alignment/
- https://criticalfallibilism.com/critical-fallibilism-evolution-and-digital-error-correction/
- https://criticalfallibilism.com/paths-forward-to-correct-errors/
- https://criticalfallibilism.com/paths-forward-summary/
- https://criticalfallibilism.com/error-correction-policies-are-hard/
- https://criticalfallibilism.com/critical-fallibilism-and-theory-of-constraints-in-one-analyzed-paragraph/
- https://criticalfallibilism.com/learning-many-small-skills-instead-of-getting-stuck/
- https://criticalfallibilism.com/critical-fallibilism-and-critical-rationalism-bullet-points/

### Web search results synthesized

- "critical fallibilism AI agents error correction" - found EmberCF (github.com/EmberCF), CF principles
- "Karl Popper fallibilism software design error correction" - Popper/software mapping, piecemeal engineering
- "critical fallibilism correction loops overreach stuck detection" - overreach mechanics, stuck patterns
- "EmberCF critical fallibilism AI agent toolkit" - GitHub toolkit project
- "Popper conjecture refutation AI agent loop design piecemeal software" - agent loop mappings
- "critical fallibilism paths forward error correction systematic" - paths forward deep dive

### Related external sources encountered

- https://iep.utm.edu/pop-sci/ (Internet Encyclopedia of Philosophy: Karl Popper)
- https://plato.stanford.edu/entries/popper/ (Stanford Encyclopedia of Philosophy: Karl Popper)
- https://en.wikipedia.org/wiki/Fallibilism (Wikipedia: Fallibilism)
- https://github.com/EmberCF (EmberCF GitHub organization)
