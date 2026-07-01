---
name: pipeline-designer
description: Design and create structured output pipelines. Gathers requirements, designs step sequences with output schemas, generates valid YAML pipeline definitions, and validates them.
allowed-tools: shell_exec, file_write, file_read, ask_user_question, send_message
argument-hint: [description of what the pipeline should do]
---

# /pipeline-designer — Structured Output Pipeline Designer

Design and create multi-step structured output pipelines for clawq. Each pipeline is a YAML file defining a sequence of LLM prompt steps with validated JSON Schema outputs. Steps can reference previous step outputs and compose other pipelines.

**Note:** This skill provides in-conversation pipeline authoring guided by the agent. For an interactive CLI setup wizard, use `clawq pipeline wizard` instead. Both approaches produce the same pipeline YAML files — use whichever fits your workflow.

## Progress Reporting

At the start of each phase, call send_message to report progress:
- send_message(text="Pipeline Designer, step 1/5: Gathering requirements...")
- send_message(text="Pipeline Designer, step 2/5: Designing step sequence...")
- send_message(text="Pipeline Designer, step 3/5: Defining output schemas...")
- send_message(text="Pipeline Designer, step 4/5: Generating YAML definition...")
- send_message(text="Pipeline Designer, step 5/5: Validating pipeline...")

Always send the progress message before starting each phase.

## Phase 1: Gather Requirements

> First, call send_message(text="Pipeline Designer, step 1/5: Gathering requirements...").

If `$ARGUMENTS` provides a description, parse it for context and pre-fill answers. Confirm with the user.

Collect via `ask_user_question` (or conversationally if unavailable):

1. **Pipeline name** (text) — short identifier using alphanumeric chars and hyphens (e.g. "research-report")
2. **Description** (text) — what the pipeline should accomplish
3. **Inputs** — what parameters the pipeline needs (name, type, description, required?, default value?)
4. **Desired outputs** — what the final output should look like
5. **Number of steps** (optional) — rough estimate of how many steps

## Phase 2: Design Step Sequence

> First, call send_message(text="Pipeline Designer, step 2/5: Designing step sequence...").

Based on requirements, design the step sequence:

1. Identify logical stages (e.g. outline -> draft -> review)
2. Determine data flow between steps (which step outputs feed into which step prompts)
3. Decide if any steps should reference existing pipelines (composability)
4. Choose appropriate models for each step if different from default (optional)
5. Set retry counts for steps that may need multiple attempts (default: 2)

Present the proposed step sequence to the user for review before proceeding.

## Phase 3: Define Output Schemas

> First, call send_message(text="Pipeline Designer, step 3/5: Defining output schemas...").

For each prompt step, define a JSON Schema for the expected output. Follow these rules:

### Supported JSON Schema Keywords

The pipeline validator supports this subset of JSON Schema:

**Type keywords:** `type` (one of: `object`, `array`, `string`, `integer`, `number`, `boolean`, `null`)

**Object keywords:**
- `properties` — map of property name to sub-schema
- `required` — array of required property names
- `additionalProperties` — boolean (default true; set false to reject extra keys)

**Array keywords:**
- `items` — schema for array elements
- `minItems`, `maxItems` — integer bounds on array length

**String keywords:**
- `minLength`, `maxLength` — integer bounds on string length
- `enum` — array of allowed values

**Numeric keywords:**
- `minimum`, `maximum` — numeric bounds

### Schema Design Guidelines

- Keep schemas focused — only require what subsequent steps actually need
- Use `required` for fields that downstream steps depend on
- Prefer `string` for free-text content, `integer` for counts, `array` for lists
- Use `enum` to constrain categorical values (e.g. `["low", "medium", "high"]`)
- Nest objects for structured sub-components
- Set `additionalProperties: false` only when strict shape control matters

## Phase 4: Generate YAML Definition

> First, call send_message(text="Pipeline Designer, step 4/5: Generating YAML definition...").

Generate the complete pipeline YAML file. Use `file_write` to save it to `~/.clawq/pipelines/<name>.yaml`.

### Pipeline YAML Format Reference

```yaml
name: pipeline-name
version: "1.0"
description: What this pipeline does

inputs:
  input_name:
    type: string
    description: What this input is for
    required: true
    default: optional-default-value

steps:
  # Prompt step — calls an LLM and validates the output
  - name: step-name
    prompt: |
      Your prompt text here.
      Use {{input.input_name}} for input variables.
      Use {{previous_step_name}} for full JSON output of a previous step.
      Use {{previous_step_name.field}} for a specific field from a previous step.
    system_prompt: Optional system prompt override
    model: Optional model override (e.g. "openai:gpt-5.4")
    output_schema:
      type: object
      properties:
        field_name:
          type: string
        another_field:
          type: integer
      required: [field_name]
    max_retries: 2

  # Pipeline step — runs another pipeline as a sub-step
  - name: sub-step-name
    pipeline: other-pipeline-name
    input_map:
      other_input: "{{input.my_input}}"
      derived_input: "{{previous_step_name.field}}"
```

### Template Variable Syntax

- `{{input.X}}` — replaced with the value of input parameter `X`
- `{{step_name}}` — replaced with the full JSON output of the named step
- `{{step_name.field}}` — replaced with a specific field extracted from a step's JSON output (top-level string/number fields only; objects/arrays use the full JSON representation)

### Key Constraints

- Step names must be unique within a pipeline
- Step names must be valid identifiers (alphanumeric + hyphens)
- Pipeline steps can reference other pipelines by name (max nesting depth: 3)
- `max_retries` defaults to 1 if omitted
- `version` should be a string (e.g. "1.0")

## Phase 5: Validate Pipeline

> First, call send_message(text="Pipeline Designer, step 5/5: Validating pipeline...").

After writing the YAML file, validate it:

```
shell_exec("clawq pipeline validate <name>")
```

If validation fails, fix the issues and re-validate. Common issues:
- Duplicate step names
- Invalid JSON Schema (unknown type, malformed properties)
- Missing required fields in pipeline definition
- YAML syntax errors

Optionally, show the user how to run the pipeline:

```
clawq pipeline run <name> --input key1=value1 --input key2=value2
```

## Example Pipelines

### Research Report Pipeline
A 3-step pipeline: outline the report structure, write the draft, then review it.
- Inputs: `topic` (required), `depth` (default: "medium")
- Steps: outline -> draft -> review
- Each step's schema validates the expected structure

### Code Review Pipeline
A 2-step pipeline: analyze code, then generate review feedback.
- Inputs: `code` (required), `language` (default: "ocaml")
- Steps: analyze -> review
- Analysis step outputs: issues array, complexity score
- Review step outputs: summary, recommendations array, rating

### Data Extraction Pipeline
A 2-step pipeline: extract entities, then classify them.
- Inputs: `text` (required), `categories` (required)
- Steps: extract -> classify
- Extract outputs array of entities with name and context
- Classify maps each entity to a category from the input list
