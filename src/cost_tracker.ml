let pricing_table =
  [
    (* Anthropic - current *)
    ("claude-opus-4-6", (5.0, 25.0));
    ("claude-opus-4-5", (5.0, 25.0));
    ("claude-opus-4-1", (15.0, 75.0));
    ("claude-opus-4-0", (15.0, 75.0));
    ("claude-sonnet-4-6", (3.0, 15.0));
    ("claude-sonnet-4-5", (3.0, 15.0));
    ("claude-sonnet-4-0", (3.0, 15.0));
    ("claude-haiku-4-5", (1.0, 5.0));
    (* Anthropic - legacy/deprecated *)
    ("claude-3-7-sonnet", (3.0, 15.0));
    ("claude-3-5-sonnet", (3.0, 15.0));
    ("claude-3-5-haiku", (0.80, 4.0));
    ("claude-3-opus", (15.0, 75.0));
    ("claude-3-sonnet", (3.0, 15.0));
    ("claude-3-haiku", (0.25, 1.25));
    (* OpenAI - GPT-5 family *)
    ("gpt-5.4-pro", (30.0, 180.0));
    ("gpt-5.4", (2.50, 15.0));
    ("gpt-5.2-pro", (21.0, 168.0));
    ("gpt-5.2", (1.75, 14.0));
    ("gpt-5-pro", (15.0, 120.0));
    ("gpt-5-mini", (0.25, 2.0));
    ("gpt-5-nano", (0.05, 0.40));
    ("gpt-5", (1.25, 10.0));
    (* OpenAI - GPT-4.1 family *)
    ("gpt-4.1-nano", (0.10, 0.40));
    ("gpt-4.1-mini", (0.40, 1.60));
    ("gpt-4.1", (2.0, 8.0));
    (* OpenAI - GPT-4o family *)
    ("gpt-4o-mini", (0.15, 0.60));
    ("gpt-4o", (2.50, 10.0));
    (* OpenAI - legacy *)
    ("gpt-4-turbo", (10.0, 30.0));
    ("gpt-4", (30.0, 60.0));
    ("gpt-3.5-turbo", (0.50, 1.50));
    (* OpenAI - reasoning *)
    ("o4-mini", (1.10, 4.40));
    ("o3-pro", (20.0, 80.0));
    ("o3-mini", (1.10, 4.40));
    ("o3", (2.0, 8.0));
    ("o1-pro", (150.0, 600.0));
    ("o1-mini", (3.0, 12.0));
    ("o1", (15.0, 60.0));
    (* Google Gemini *)
    ("gemini-2.5-pro", (1.25, 10.0));
    ("gemini-2.5-flash", (0.30, 2.50));
    ("gemini-2.0-flash", (0.10, 0.40));
    ("gemini-1.5-pro", (1.25, 5.0));
    ("gemini-1.5-flash", (0.075, 0.30));
    (* DeepSeek *)
    ("deepseek-reasoner", (0.28, 0.42));
    ("deepseek-chat", (0.28, 0.42));
    ("deepseek-v3", (0.28, 0.42));
    ("deepseek-r1", (0.28, 0.42));
    (* Mistral *)
    ("mistral-large", (0.50, 1.50));
    ("mistral-medium", (0.40, 2.0));
    ("mistral-small", (0.06, 0.18));
    ("codestral", (0.30, 0.90));
    ("mixtral-8x7b", (0.24, 0.24));
    (* Cohere *)
    ("command-a", (2.50, 10.0));
    ("command-r-plus", (2.50, 10.0));
    ("command-r", (0.15, 0.60));
    (* Meta Llama via Groq *)
    ("llama-3.3-70b", (0.59, 0.79));
    ("llama-3.1-405b", (3.0, 3.0));
    ("llama-3.1-70b", (0.59, 0.79));
    ("llama-3.1-8b", (0.05, 0.08));
    (* Moonshot/Kimi *)
    ("kimi-k2.5", (0.60, 3.0));
    ("kimi-k2", (0.60, 2.50));
    ("kimi-k2-thinking", (0.60, 2.50));
    ("kimi-for-coding", (0.60, 2.50));
    ("moonshot-v1-128k", (2.0, 5.0));
    ("moonshot-v1-32k", (1.0, 3.0));
    ("moonshot-v1-8k", (0.20, 2.0));
    (* MiniMax *)
    ("minimax-m2.5", (0.30, 1.20));
    ("minimax-m1", (0.40, 1.76));
    ("minimax-text-01", (0.20, 1.10));
    (* Z.ai - Source: https://docs.z.ai/guides/overview/pricing *)
    ("glm-5", (1.0, 3.2));
    ("glm-4.7", (0.60, 2.20));
    ("glm-4.6", (0.60, 2.20));
    (* Z.ai Coding endpoint - Source: https://docs.z.ai/guides/overview/pricing *)
    ("zai_coding/glm-5", (1.20, 5.0));
    ("zai_coding/glm-4.7", (0.60, 2.20));
    ("zai_coding/glm-4.6", (0.60, 2.20));
    (* Xiaomi MiMo *)
    ("mimo-v2-flash", (0.10, 0.30));
  ]

let normalize_model s =
  let s = String.lowercase_ascii (String.trim s) in
  let s = Runtime_config.strip_model_provider_prefix s in
  let strip_date s =
    let len = String.length s in
    if len >= 9 && s.[len - 9] = '-' then
      let suffix = String.sub s (len - 8) 8 in
      let all_digits =
        try
          String.iter (fun c -> if c < '0' || c > '9' then raise Exit) suffix;
          true
        with Exit -> false
      in
      if all_digits then String.sub s 0 (len - 9) else s
    else s
  in
  strip_date s

let lookup_pricing model =
  let raw = String.lowercase_ascii (String.trim model) in
  let norm = normalize_model model in
  let find_prefix hay needle =
    String.length hay >= String.length needle
    && String.sub hay 0 (String.length needle) = needle
  in
  (* Try exact match on raw first to preserve provider-qualified names like zai_coding/glm-5 *)
  match List.find_opt (fun (k, _) -> raw = k) pricing_table with
  | Some (_, v) -> Some v
  | None -> (
      match List.find_opt (fun (k, _) -> norm = k) pricing_table with
      | Some (_, v) -> Some v
      | None -> (
          match
            List.find_opt (fun (k, _) -> find_prefix norm k) pricing_table
          with
          | Some (_, v) -> Some v
          | None -> None))

let calculate_cost ~model ~prompt_tokens ~completion_tokens =
  match lookup_pricing model with
  | None -> 0.0
  | Some (input_per_m, output_per_m) ->
      let input_cost =
        float_of_int prompt_tokens *. input_per_m /. 1_000_000.0
      in
      let output_cost =
        float_of_int completion_tokens *. output_per_m /. 1_000_000.0
      in
      input_cost +. output_cost

type session_stats = {
  mutable total_prompt_tokens : int;
  mutable total_completion_tokens : int;
  mutable total_cost : float;
  mutable turn_count : int;
}

let sessions : (string, session_stats) Hashtbl.t = Hashtbl.create 16

let record_turn ~model ~prompt_tokens ~completion_tokens ~session_id =
  let cost = calculate_cost ~model ~prompt_tokens ~completion_tokens in
  let stats =
    match Hashtbl.find_opt sessions session_id with
    | Some s -> s
    | None ->
        let s =
          {
            total_prompt_tokens = 0;
            total_completion_tokens = 0;
            total_cost = 0.0;
            turn_count = 0;
          }
        in
        Hashtbl.replace sessions session_id s;
        s
  in
  stats.total_prompt_tokens <- stats.total_prompt_tokens + prompt_tokens;
  stats.total_completion_tokens <-
    stats.total_completion_tokens + completion_tokens;
  stats.total_cost <- stats.total_cost +. cost;
  stats.turn_count <- stats.turn_count + 1;
  let sk_tag = if session_id <> "" then "[" ^ session_id ^ "] " else "" in
  Logs.info (fun m ->
      m
        "%sCost: model=%s prompt=%d completion=%d cost=$%.6f \
         session_total=$%.4f"
        sk_tag model prompt_tokens completion_tokens cost stats.total_cost)

let get_session_cost ~session_id =
  match Hashtbl.find_opt sessions session_id with
  | Some s -> s.total_cost
  | None -> 0.0

let get_session_stats ~session_id = Hashtbl.find_opt sessions session_id
