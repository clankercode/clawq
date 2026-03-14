export interface NormalizedToolCall {
  id: string;
  name: string;
  arguments: string;
}

export interface NormalizedMessage {
  index: number;
  role: "user" | "assistant" | "system" | "tool" | "event";
  content: string;
  toolCalls?: NormalizedToolCall[];
  toolCallId?: string;
  toolName?: string;
  thinking?: string;
  createdAt?: string;
}

export interface ChatLog {
  sessionKey?: string;
  epoch?: string;
  systemPrompt?: string;
  messages: NormalizedMessage[];
}
