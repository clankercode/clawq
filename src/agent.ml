include Agent_turn_core

(* Buffered turn: obtain each response via Provider.complete, no live chunk
   emission. See [run_turn] for the shared loop. *)
let turn agent ~user_message ?db ?session_key ?interrupt_check ?inject_messages
    ?on_inject_messages ?on_tool_round_complete ?runtime_context
    ?(history_prepared = false) ?on_history_update ?on_stuck ?on_llm_call_debug
    () =
  run_turn agent
    ~mk_io:(fun ~quota_states_opt ~tools ->
      buffered_io agent ?db ?session_key ~quota_states_opt ~tools ())
    ~user_message ?db ?session_key ?interrupt_check ?inject_messages
    ?on_inject_messages ?on_tool_round_complete ?runtime_context
    ~history_prepared ?on_history_update ?on_stuck ?on_llm_call_debug ()

(* Streaming turn: obtain each response via Provider.complete_stream, surfacing
   deltas and tool events through [on_chunk]. *)
let turn_stream agent ~user_message ?db ?session_key ?interrupt_check
    ?inject_messages ?on_inject_messages ?on_tool_round_complete
    ?runtime_context ?(history_prepared = false) ?on_history_update ?on_stuck
    ?on_llm_call_debug ~on_chunk () =
  run_turn agent
    ~mk_io:(fun ~quota_states_opt ~tools ->
      streaming_io agent ?db ?session_key ~quota_states_opt ~tools ~on_chunk
        ~interrupt_check ())
    ~user_message ?db ?session_key ?interrupt_check ?inject_messages
    ?on_inject_messages ?on_tool_round_complete ?runtime_context
    ~history_prepared ?on_history_update ?on_stuck ?on_llm_call_debug ()
