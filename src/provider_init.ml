(* Provider_init registers all native provider backends with Provider's
   dispatch hooks. This module must be linked into any binary that wants
   native provider dispatch (i.e., not just OpenAI-compatible fallback).
   It is included in clawq_runtime_integrations so registration happens at
   startup when the full binary is built. *)

let () =
  Provider.register_native_complete Provider.OpenAICodex
    Provider_openai_codex.complete;
  Provider.register_native_complete Provider.Anthropic
    Provider_anthropic.complete;
  Provider.register_native_complete Provider.Ollama Provider_ollama.complete;
  Provider.register_native_complete Provider.Gemini Provider_gemini.complete;
  Provider.register_native_complete Provider.Vertex Provider_vertex.complete;
  Provider.register_native_complete Provider.Cohere Provider_cohere.complete;
  Provider.register_native_complete Provider.MiniMax Provider_minimax.complete;
  Provider.register_native_stream Provider.Anthropic
    Provider_anthropic.complete_streaming;
  Provider.register_native_stream Provider.OpenAICodex
    Provider_openai_codex.complete_streaming;
  Provider.register_native_stream Provider.Ollama
    Provider_ollama.complete_streaming;
  Provider.register_native_stream Provider.Gemini
    Provider_gemini.complete_streaming;
  Provider.register_native_stream Provider.Vertex
    Provider_vertex.complete_streaming;
  Provider.register_native_stream Provider.Cohere
    Provider_cohere.complete_streaming;
  Provider.register_native_stream Provider.MiniMax
    Provider_minimax.complete_streaming

(* Sentinel referenced by command_bridge.ml to force-link this module.
   Without an exported symbol the native linker drops the registration code. *)
let registered = true
