import type { ChatLog, NormalizedMessage, NormalizedToolCall } from "./types";

/** Detect format and normalize to ChatLog */
export function normalize(input: unknown): ChatLog {
  if (Array.isArray(input)) {
    return normalizeArray(input);
  }
  if (input && typeof input === "object" && "messages" in input) {
    return normalizeClawqSession(input as Record<string, unknown>);
  }
  throw new Error("Unrecognized format: expected an array of messages or an object with a 'messages' key.");
}

function normalizeArray(items: unknown[]): ChatLog {
  if (items.length === 0) {
    return { messages: [] };
  }

  // Detect ACP (Agent Client Protocol) JSONL: entries are JSON-RPC messages
  // with "jsonrpc": "2.0" and methods like "session/update", "session/prompt"
  if (isAcpJsonl(items)) {
    return { messages: normalizeAcpMessages(items) };
  }

  // Detect Claude Code session JSONL: entries have a top-level "type" field
  // like "user", "assistant", "file-history-snapshot", "progress", etc.
  // Actual messages are nested under .message with .message.role
  if (isClaudeCodeSessionJsonl(items)) {
    const messages = extractClaudeCodeSessionMessages(items);
    return { messages: normalizeClaudeCodeMessages(messages) };
  }

  const first = items[0] as Record<string, unknown>;
  if (!first || typeof first !== "object" || !("role" in first)) {
    throw new Error("Array items must have a 'role' field.");
  }

  // Detect Claude Code format: content is array of typed blocks
  if (isClaudeCodeFormat(items)) {
    return { messages: normalizeClaudeCodeMessages(items) };
  }

  // Plain OpenAI-style messages (content is string)
  return { messages: items.map((item, i) => normalizeSimpleMessage(item as Record<string, unknown>, i)) };
}

/** Claude Code session JSONL: each line has a top-level "type" field ("user", "assistant",
 *  "file-history-snapshot", "progress", "system", "queue-operation", "last-prompt", etc.)
 *  and message entries nest the actual message under .message */
function isClaudeCodeSessionJsonl(items: unknown[]): boolean {
  const first = items[0] as Record<string, unknown>;
  if (!first || typeof first !== "object" || !("type" in first)) return false;
  const t = first.type;
  // Check if the top-level type matches known Claude Code session entry types
  const sessionTypes = new Set([
    "user", "assistant", "file-history-snapshot", "progress",
    "system", "queue-operation", "last-prompt",
  ]);
  return typeof t === "string" && sessionTypes.has(t);
}

/** Extract the inner .message objects from Claude Code session JSONL entries */
function extractClaudeCodeSessionMessages(items: unknown[]): unknown[] {
  const messages: unknown[] = [];
  for (const item of items) {
    const entry = item as Record<string, unknown>;
    if ((entry.type === "user" || entry.type === "assistant") && entry.message && typeof entry.message === "object") {
      messages.push(entry.message);
    }
  }
  return messages;
}

function isClaudeCodeFormat(items: unknown[]): boolean {
  return items.some((item) => {
    const msg = item as Record<string, unknown>;
    return Array.isArray(msg.content) && (msg.content as unknown[]).some(
      (block) => block && typeof block === "object" && "type" in (block as Record<string, unknown>)
    );
  });
}

function normalizeClaudeCodeMessages(items: unknown[]): NormalizedMessage[] {
  const messages: NormalizedMessage[] = [];
  let index = 0;

  for (const item of items) {
    const msg = item as Record<string, unknown>;
    const role = msg.role as string;

    if (!Array.isArray(msg.content)) {
      // Simple string content
      messages.push({
        index: index++,
        role: mapRole(role),
        content: typeof msg.content === "string" ? msg.content : "",
      });
      continue;
    }

    const blocks = msg.content as Record<string, unknown>[];
    const textParts: string[] = [];
    let thinking = "";
    const toolCalls: NormalizedToolCall[] = [];
    const toolResults: { toolUseId: string; content: string }[] = [];

    for (const block of blocks) {
      switch (block.type) {
        case "text":
          textParts.push(block.text as string);
          break;
        case "thinking":
          thinking += (thinking ? "\n\n" : "") + (block.thinking as string);
          break;
        case "tool_use":
          toolCalls.push({
            id: block.id as string,
            name: block.name as string,
            arguments: typeof block.input === "string" ? block.input : JSON.stringify(block.input, null, 2),
          });
          break;
        case "tool_result":
          toolResults.push({
            toolUseId: block.tool_use_id as string,
            content: extractToolResultContent(block.content),
          });
          break;
      }
    }

    // Emit the main message (user/assistant)
    if (textParts.length > 0 || thinking || toolCalls.length > 0 || toolResults.length === 0) {
      messages.push({
        index: index++,
        role: mapRole(role),
        content: textParts.join("\n\n"),
        ...(toolCalls.length > 0 ? { toolCalls } : {}),
        ...(thinking ? { thinking } : {}),
      });
    }

    // Emit separate tool result messages
    for (const result of toolResults) {
      messages.push({
        index: index++,
        role: "tool",
        content: result.content,
        toolCallId: result.toolUseId,
      });
    }
  }

  return messages;
}

function extractToolResultContent(content: unknown): string {
  if (typeof content === "string") return content;
  if (Array.isArray(content)) {
    return content
      .map((block) => {
        if (typeof block === "string") return block;
        const b = block as Record<string, unknown>;
        if (b.type === "text") return b.text as string;
        return JSON.stringify(block);
      })
      .join("\n");
  }
  return JSON.stringify(content);
}

function normalizeSimpleMessage(msg: Record<string, unknown>, index: number): NormalizedMessage {
  const role = mapRole(msg.role as string);
  const content = typeof msg.content === "string" ? msg.content : "";

  // Extract OpenAI-style tool_calls from assistant messages
  const toolCalls: NormalizedToolCall[] = [];
  if (Array.isArray(msg.tool_calls)) {
    for (const tc of msg.tool_calls as Record<string, unknown>[]) {
      const fn = tc.function as Record<string, unknown> | undefined;
      toolCalls.push({
        id: (tc.id as string) || "",
        name: fn ? (fn.name as string) || "" : (tc.name as string) || "",
        arguments: fn
          ? typeof fn.arguments === "string" ? fn.arguments : JSON.stringify(fn.arguments, null, 2)
          : typeof tc.arguments === "string" ? tc.arguments : JSON.stringify(tc.arguments, null, 2),
      });
    }
  }

  return {
    index,
    role,
    content,
    ...(toolCalls.length > 0 ? { toolCalls } : {}),
    ...(msg.tool_call_id ? { toolCallId: msg.tool_call_id as string } : {}),
    ...(msg.name && role === "tool" ? { toolName: msg.name as string } : {}),
  };
}

function normalizeClawqSession(input: Record<string, unknown>): ChatLog {
  const rawMessages = input.messages as Record<string, unknown>[];
  if (!Array.isArray(rawMessages)) {
    throw new Error("'messages' field must be an array.");
  }

  const messages: NormalizedMessage[] = rawMessages.map((msg, i) => {
    const toolCalls = parseToolCallsJson(msg.tool_calls_json as string | null);
    const thinking = extractThinkingFromProviderItems(msg.provider_response_items_json as string | null);

    return {
      index: typeof msg.index === "number" ? msg.index : i,
      role: mapRole(msg.role as string),
      content: typeof msg.content === "string" ? msg.content : "",
      ...(toolCalls.length > 0 ? { toolCalls } : {}),
      ...(msg.tool_call_id ? { toolCallId: msg.tool_call_id as string } : {}),
      ...(msg.tool_name ? { toolName: msg.tool_name as string } : {}),
      ...(thinking ? { thinking } : {}),
      ...(msg.created_at ? { createdAt: msg.created_at as string } : {}),
    };
  });

  const epoch = input.epoch;
  let epochStr: string | undefined;
  if (typeof epoch === "string") epochStr = epoch;
  else if (epoch && typeof epoch === "object" && "index" in (epoch as Record<string, unknown>)) {
    epochStr = `epoch ${(epoch as Record<string, unknown>).index}`;
  }

  return {
    sessionKey: input.session_key as string | undefined,
    epoch: epochStr,
    systemPrompt: input.system_prompt as string | undefined,
    messages,
  };
}

function parseToolCallsJson(raw: string | null | undefined): NormalizedToolCall[] {
  if (!raw) return [];
  try {
    const parsed = JSON.parse(raw) as Record<string, unknown>[];
    return parsed.map((tc) => ({
      id: (tc.id as string) || "",
      name: (tc.function_name as string) || (tc.name as string) || "",
      arguments: typeof tc.arguments === "string" ? tc.arguments : JSON.stringify(tc.arguments, null, 2),
    }));
  } catch {
    return [];
  }
}

function extractThinkingFromProviderItems(raw: string | null | undefined): string {
  if (!raw) return "";
  try {
    const items = JSON.parse(raw) as Record<string, unknown>[];
    const thinkingParts: string[] = [];
    for (const item of items) {
      if (item.type === "reasoning" && typeof item.summary === "object" && item.summary) {
        const summaryItems = (item.summary as Record<string, unknown>[]);
        if (Array.isArray(summaryItems)) {
          for (const s of summaryItems) {
            if (s.type === "summary_text" && typeof s.text === "string") {
              thinkingParts.push(s.text);
            }
          }
        }
      }
    }
    return thinkingParts.join("\n\n");
  } catch {
    return "";
  }
}

/** ACP JSONL: lines are JSON-RPC 2.0 messages with "jsonrpc": "2.0" */
function isAcpJsonl(items: unknown[]): boolean {
  return items.some((item) => {
    const msg = item as Record<string, unknown>;
    return msg.jsonrpc === "2.0" && (
      typeof msg.method === "string" ||
      msg.result !== undefined ||
      msg.error !== undefined
    );
  });
}

/** Normalize ACP JSON-RPC messages into ChatLog messages */
function normalizeAcpMessages(items: unknown[]): NormalizedMessage[] {
  const messages: NormalizedMessage[] = [];
  let index = 0;
  let currentAssistantText = "";

  for (const item of items) {
    const msg = item as Record<string, unknown>;
    const method = msg.method as string | undefined;
    const params = msg.params as Record<string, unknown> | undefined;

    if (method === "session/prompt" && params) {
      // User prompt
      const promptBlocks = params.prompt as Record<string, unknown>[] | undefined;
      let text = "";
      if (Array.isArray(promptBlocks)) {
        text = promptBlocks
          .filter((b) => b.type === "text")
          .map((b) => b.text as string)
          .join("\n\n");
      }
      if (text) {
        // Flush any accumulated assistant text
        if (currentAssistantText) {
          messages.push({ index: index++, role: "assistant", content: currentAssistantText });
          currentAssistantText = "";
        }
        messages.push({ index: index++, role: "user", content: text });
      }
    } else if (method === "session/update" && params) {
      const update = params.update as Record<string, unknown> | undefined;
      if (!update) continue;

      const sessionUpdate = update.sessionUpdate as string;
      switch (sessionUpdate) {
        case "agent_message_chunk": {
          const content = update.content as Record<string, unknown> | undefined;
          if (content?.type === "text") {
            currentAssistantText += content.text as string;
          }
          break;
        }
        case "thought_message_chunk": {
          // Emit accumulated text first
          if (currentAssistantText) {
            messages.push({ index: index++, role: "assistant", content: currentAssistantText });
            currentAssistantText = "";
          }
          const content = update.content as Record<string, unknown> | undefined;
          if (content?.type === "text") {
            messages.push({
              index: index++,
              role: "assistant",
              content: "",
              thinking: content.text as string,
            });
          }
          break;
        }
        case "tool_call": {
          // Flush assistant text
          if (currentAssistantText) {
            messages.push({ index: index++, role: "assistant", content: currentAssistantText });
            currentAssistantText = "";
          }
          const toolCalls: NormalizedToolCall[] = [{
            id: update.toolCallId as string || "",
            name: update.title as string || "",
            arguments: update.rawInput ? JSON.stringify(update.rawInput, null, 2) : "{}",
          }];
          messages.push({ index: index++, role: "assistant", content: "", toolCalls });
          break;
        }
        case "tool_call_update": {
          const status = update.status as string | undefined;
          const toolCallId = update.toolCallId as string || "";
          const contentBlocks = update.content as Record<string, unknown>[] | undefined;
          let resultText = "";
          if (Array.isArray(contentBlocks)) {
            resultText = contentBlocks
              .map((c) => {
                if (c.type === "content") {
                  const inner = c.content as Record<string, unknown>;
                  return inner?.type === "text" ? inner.text as string : JSON.stringify(inner);
                }
                return JSON.stringify(c);
              })
              .join("\n");
          }
          if (status === "completed" || status === "failed") {
            messages.push({
              index: index++,
              role: "tool",
              content: resultText || `[${status}]`,
              toolCallId,
            });
          }
          break;
        }
      }
    }
  }

  // Flush remaining assistant text
  if (currentAssistantText) {
    messages.push({ index: index++, role: "assistant", content: currentAssistantText });
  }

  return messages;
}

function mapRole(role: string): NormalizedMessage["role"] {
  switch (role) {
    case "user": return "user";
    case "assistant": return "assistant";
    case "system": return "system";
    case "tool": return "tool";
    case "event": return "event";
    default: return "user";
  }
}
