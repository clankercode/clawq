# Model Prompt Cache TTLs

Provider prompt cache TTL data feeds cache-aware blocking, session-management
heuristics, and cached-token cost estimation. Store defaults per provider and
use model-specific overrides only when provider docs say a model differs.

| Provider | Default TTL seconds | Source | Notes |
| --- | ---: | --- | --- |
| `anthropic` | 300 | https://platform.claude.com/docs/en/build-with-claude/prompt-caching | Default ephemeral prompt cache lifetime is 5 minutes. Anthropic also documents a 1-hour cache option, but it is opt-in. |
| `deepseek` |  | https://api-docs.deepseek.com/guides/kv_cache | DeepSeek documents automatic context caching and says caches are usually cleared after a few hours to a few days, so no fixed TTL is encoded. |
| `gemini` |  | https://ai.google.dev/gemini-api/docs/generate-content/caching | Gemini implicit caching is enabled for newer models but does not document a fixed TTL. Explicit cached-content resources default to 1 hour, but Clawq's current Gemini path does not create those resources. |
| `groq` |  | https://console.groq.com/docs/prompt-caching | Groq prompt caching is limited to documented supported models, so no provider-wide default is encoded. |
| `openai` |  | https://developers.openai.com/api/docs/guides/prompt-caching | For most models, OpenAI prompt cache retention defaults depend on organization data-retention policy: non-ZDR defaults to 24h, while ZDR defaults to in-memory. No single provider default is encoded. |
| `openai-codex` |  | https://developers.openai.com/api/docs/guides/prompt-caching | Codex catalog entries use OpenAI prompt caching, so the same policy-dependent default applies unless a model-specific override is documented. |
| `opencodex` |  |  | OpenCodex routes heterogeneous upstream providers, so no universal proxy-wide prompt cache TTL is asserted. |

## Model Overrides

| Provider | Model | Effective TTL seconds | Source | Notes |
| --- | --- | ---: | --- | --- |
| `openai` | `gpt-5.5` | 86400 | https://developers.openai.com/api/docs/guides/prompt-caching | OpenAI documents `gpt-5.5` as supporting only 24-hour prompt cache retention. |
| `openai-codex` | `gpt-5.5` | 86400 | https://developers.openai.com/api/docs/guides/prompt-caching | The Codex catalog exposes the same model id under the Codex provider. |
| `groq` | `openai/gpt-oss-20b` | 7200 | https://console.groq.com/docs/prompt-caching | Groq prompt caching docs list this model as supported and say cached data expires after 2 hours without use. |
| `groq` | `openai/gpt-oss-120b` | 7200 | https://console.groq.com/docs/prompt-caching | Groq prompt caching docs list this model as supported and say cached data expires after 2 hours without use. |

Providers not listed here either have no documented prompt cache TTL, only
document cached-token pricing without a retention window, or are local-only.
