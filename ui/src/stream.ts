export type ServerEvent =
  | { type: "delta"; content: string }
  | { type: "thinking_delta"; content: string }
  | { type: "tool_call_delta"; index: number; id?: string; function_name?: string; arguments?: string }
  | { type: "tool_start"; id: string; name: string; arguments: string }
  | { type: "tool_output_delta"; id: string; chunk: string }
  | { type: "tool_result"; id: string; name: string; result: string; is_error: boolean }
  | { type: "error"; message: string }
  | { type: "done" };

function parseEventChunk(chunk: string): ServerEvent[] {
  const dataLines = chunk
    .split("\n")
    .filter((line) => line.startsWith("data:"))
    .map((line) => line.slice(5).trimStart());

  if (dataLines.length === 0) {
    return [];
  }

  const data = dataLines.join("\n").trim();
  if (!data || data === "[DONE]") {
    return [{ type: "done" }];
  }

  try {
    return [JSON.parse(data) as ServerEvent];
  } catch {
    return [{ type: "error", message: `Malformed stream event: ${data}` }];
  }
}

export async function* readSSE(response: Response): AsyncGenerator<ServerEvent> {
  if (!response.body) {
    yield { type: "error", message: "Response body missing" };
    return;
  }

  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  let buffer = "";

  while (true) {
    const { done, value } = await reader.read();
    if (done) {
      break;
    }
    buffer += decoder.decode(value, { stream: true });
    const parts = buffer.split("\n\n");
    buffer = parts.pop() ?? "";
    for (const part of parts) {
      for (const event of parseEventChunk(part)) {
        yield event;
      }
    }
  }

  const tail = buffer.trim();
  if (tail) {
    for (const event of parseEventChunk(tail)) {
      yield event;
    }
  }
}
