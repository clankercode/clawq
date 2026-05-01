type model_pricing = {
  input_per_m : float;
  output_per_m : float;
  cache_read_per_m : float option;
}

let pricing_table =
  [
    (* Anthropic - current (cache_read = 10% of input) *)
    ( "claude-opus-4-6",
      { input_per_m = 5.0; output_per_m = 25.0; cache_read_per_m = Some 0.50 }
    );
    ( "claude-opus-4-5",
      { input_per_m = 5.0; output_per_m = 25.0; cache_read_per_m = Some 0.50 }
    );
    ( "claude-opus-4-1",
      { input_per_m = 15.0; output_per_m = 75.0; cache_read_per_m = Some 1.50 }
    );
    ( "claude-opus-4-0",
      { input_per_m = 15.0; output_per_m = 75.0; cache_read_per_m = Some 1.50 }
    );
    ( "claude-sonnet-4-6",
      { input_per_m = 3.0; output_per_m = 15.0; cache_read_per_m = Some 0.30 }
    );
    ( "claude-sonnet-4-5",
      { input_per_m = 3.0; output_per_m = 15.0; cache_read_per_m = Some 0.30 }
    );
    ( "claude-sonnet-4-0",
      { input_per_m = 3.0; output_per_m = 15.0; cache_read_per_m = Some 0.30 }
    );
    ( "claude-haiku-4-5",
      { input_per_m = 1.0; output_per_m = 5.0; cache_read_per_m = Some 0.10 } );
    (* Anthropic - legacy/deprecated *)
    ( "claude-3-7-sonnet",
      { input_per_m = 3.0; output_per_m = 15.0; cache_read_per_m = Some 0.30 }
    );
    ( "claude-3-5-sonnet",
      { input_per_m = 3.0; output_per_m = 15.0; cache_read_per_m = Some 0.30 }
    );
    ( "claude-3-5-haiku",
      { input_per_m = 0.80; output_per_m = 4.0; cache_read_per_m = Some 0.08 }
    );
    ( "claude-3-opus",
      { input_per_m = 15.0; output_per_m = 75.0; cache_read_per_m = Some 1.50 }
    );
    ( "claude-3-sonnet",
      { input_per_m = 3.0; output_per_m = 15.0; cache_read_per_m = Some 0.30 }
    );
    ( "claude-3-haiku",
      { input_per_m = 0.25; output_per_m = 1.25; cache_read_per_m = Some 0.025 }
    );
    (* OpenAI - GPT-5 family (cache_read = 50% of input) *)
    ( "gpt-5.4-pro",
      { input_per_m = 30.0; output_per_m = 180.0; cache_read_per_m = Some 15.0 }
    );
    ( "gpt-5.4",
      { input_per_m = 2.50; output_per_m = 15.0; cache_read_per_m = Some 1.25 }
    );
    ( "gpt-5.2-pro",
      {
        input_per_m = 21.0;
        output_per_m = 168.0;
        cache_read_per_m = Some 10.50;
      } );
    ( "gpt-5.2",
      { input_per_m = 1.75; output_per_m = 14.0; cache_read_per_m = Some 0.875 }
    );
    ( "gpt-5-pro",
      { input_per_m = 15.0; output_per_m = 120.0; cache_read_per_m = Some 7.50 }
    );
    ( "gpt-5-mini",
      { input_per_m = 0.25; output_per_m = 2.0; cache_read_per_m = Some 0.125 }
    );
    ( "gpt-5-nano",
      { input_per_m = 0.05; output_per_m = 0.40; cache_read_per_m = Some 0.025 }
    );
    ( "gpt-5",
      { input_per_m = 1.25; output_per_m = 10.0; cache_read_per_m = Some 0.625 }
    );
    (* OpenAI - GPT-4.1 family *)
    ( "gpt-4.1-nano",
      { input_per_m = 0.10; output_per_m = 0.40; cache_read_per_m = Some 0.05 }
    );
    ( "gpt-4.1-mini",
      { input_per_m = 0.40; output_per_m = 1.60; cache_read_per_m = Some 0.20 }
    );
    ( "gpt-4.1",
      { input_per_m = 2.0; output_per_m = 8.0; cache_read_per_m = Some 1.0 } );
    (* OpenAI - GPT-4o family *)
    ( "gpt-4o-mini",
      { input_per_m = 0.15; output_per_m = 0.60; cache_read_per_m = Some 0.075 }
    );
    ( "gpt-4o",
      { input_per_m = 2.50; output_per_m = 10.0; cache_read_per_m = Some 1.25 }
    );
    (* OpenAI - legacy *)
    ( "gpt-4-turbo",
      { input_per_m = 10.0; output_per_m = 30.0; cache_read_per_m = None } );
    ( "gpt-4",
      { input_per_m = 30.0; output_per_m = 60.0; cache_read_per_m = None } );
    ( "gpt-3.5-turbo",
      { input_per_m = 0.50; output_per_m = 1.50; cache_read_per_m = None } );
    (* OpenAI - reasoning *)
    ( "o4-mini",
      { input_per_m = 1.10; output_per_m = 4.40; cache_read_per_m = Some 0.55 }
    );
    ( "o3-pro",
      { input_per_m = 20.0; output_per_m = 80.0; cache_read_per_m = None } );
    ( "o3-mini",
      { input_per_m = 1.10; output_per_m = 4.40; cache_read_per_m = Some 0.55 }
    );
    ( "o3",
      { input_per_m = 2.0; output_per_m = 8.0; cache_read_per_m = Some 1.0 } );
    ( "o1-pro",
      { input_per_m = 150.0; output_per_m = 600.0; cache_read_per_m = None } );
    ( "o1-mini",
      { input_per_m = 3.0; output_per_m = 12.0; cache_read_per_m = None } );
    ("o1", { input_per_m = 15.0; output_per_m = 60.0; cache_read_per_m = None });
    (* Google Gemini (cache_read = 25% of input) *)
    ( "gemini-2.5-pro",
      {
        input_per_m = 1.25;
        output_per_m = 10.0;
        cache_read_per_m = Some 0.3125;
      } );
    ( "gemini-2.5-flash",
      { input_per_m = 0.30; output_per_m = 2.50; cache_read_per_m = Some 0.075 }
    );
    ( "gemini-2.0-flash",
      { input_per_m = 0.10; output_per_m = 0.40; cache_read_per_m = Some 0.025 }
    );
    ( "gemini-1.5-pro",
      { input_per_m = 1.25; output_per_m = 5.0; cache_read_per_m = Some 0.3125 }
    );
    ( "gemini-1.5-flash",
      {
        input_per_m = 0.075;
        output_per_m = 0.30;
        cache_read_per_m = Some 0.01875;
      } );
    (* DeepSeek *)
    ( "deepseek-reasoner",
      { input_per_m = 0.28; output_per_m = 0.42; cache_read_per_m = None } );
    ( "deepseek-chat",
      { input_per_m = 0.28; output_per_m = 0.42; cache_read_per_m = None } );
    ( "deepseek-v3",
      { input_per_m = 0.28; output_per_m = 0.42; cache_read_per_m = None } );
    ( "deepseek-r1",
      { input_per_m = 0.28; output_per_m = 0.42; cache_read_per_m = None } );
    (* Mistral *)
    ( "mistral-large",
      { input_per_m = 0.50; output_per_m = 1.50; cache_read_per_m = None } );
    ( "mistral-medium",
      { input_per_m = 0.40; output_per_m = 2.0; cache_read_per_m = None } );
    ( "mistral-small",
      { input_per_m = 0.06; output_per_m = 0.18; cache_read_per_m = None } );
    ( "codestral",
      { input_per_m = 0.30; output_per_m = 0.90; cache_read_per_m = None } );
    ( "mixtral-8x7b",
      { input_per_m = 0.24; output_per_m = 0.24; cache_read_per_m = None } );
    (* Cohere *)
    ( "command-a",
      { input_per_m = 2.50; output_per_m = 10.0; cache_read_per_m = None } );
    ( "command-r-plus",
      { input_per_m = 2.50; output_per_m = 10.0; cache_read_per_m = None } );
    ( "command-r",
      { input_per_m = 0.15; output_per_m = 0.60; cache_read_per_m = None } );
    (* Meta Llama via Groq *)
    ( "llama-3.3-70b",
      { input_per_m = 0.59; output_per_m = 0.79; cache_read_per_m = None } );
    ( "llama-3.1-405b",
      { input_per_m = 3.0; output_per_m = 3.0; cache_read_per_m = None } );
    ( "llama-3.1-70b",
      { input_per_m = 0.59; output_per_m = 0.79; cache_read_per_m = None } );
    ( "llama-3.1-8b",
      { input_per_m = 0.05; output_per_m = 0.08; cache_read_per_m = None } );
    (* Moonshot/Kimi *)
    ( "kimi-k2.5",
      { input_per_m = 0.60; output_per_m = 3.0; cache_read_per_m = None } );
    ( "kimi-k2",
      { input_per_m = 0.60; output_per_m = 2.50; cache_read_per_m = None } );
    ( "kimi-k2-thinking",
      { input_per_m = 0.60; output_per_m = 2.50; cache_read_per_m = None } );
    ( "kimi-for-coding",
      { input_per_m = 0.60; output_per_m = 2.50; cache_read_per_m = None } );
    ( "moonshot-v1-128k",
      { input_per_m = 2.0; output_per_m = 5.0; cache_read_per_m = None } );
    ( "moonshot-v1-32k",
      { input_per_m = 1.0; output_per_m = 3.0; cache_read_per_m = None } );
    ( "moonshot-v1-8k",
      { input_per_m = 0.20; output_per_m = 2.0; cache_read_per_m = None } );
    (* MiniMax *)
    ( "minimax-m2.7",
      { input_per_m = 0.30; output_per_m = 1.20; cache_read_per_m = Some 0.06 }
    );
    ( "minimax-m2.7-highspeed",
      { input_per_m = 0.60; output_per_m = 2.40; cache_read_per_m = Some 0.06 }
    );
    ( "minimax-m2.5",
      { input_per_m = 0.30; output_per_m = 1.20; cache_read_per_m = Some 0.06 }
    );
    ( "minimax-m1",
      { input_per_m = 0.40; output_per_m = 1.76; cache_read_per_m = None } );
    ( "minimax-text-01",
      { input_per_m = 0.20; output_per_m = 1.10; cache_read_per_m = None } );
    (* Z.ai - Source: https://docs.z.ai/guides/overview/pricing *)
    ( "glm-5.1",
      { input_per_m = 1.0; output_per_m = 3.2; cache_read_per_m = None } );
    ( "glm-5-turbo",
      { input_per_m = 1.2; output_per_m = 4.0; cache_read_per_m = Some 0.24 } );
    ( "glm-5",
      { input_per_m = 1.0; output_per_m = 3.2; cache_read_per_m = Some 0.2 } );
    ( "glm-4.7",
      { input_per_m = 0.60; output_per_m = 2.20; cache_read_per_m = None } );
    ( "glm-4.6",
      { input_per_m = 0.60; output_per_m = 2.20; cache_read_per_m = None } );
    (* Z.ai Coding endpoint - Source: https://docs.z.ai/guides/overview/pricing *)
    ( "zai_coding/glm-5.1",
      { input_per_m = 1.20; output_per_m = 5.0; cache_read_per_m = None } );
    ( "zai_coding/glm-5-turbo",
      { input_per_m = 1.20; output_per_m = 4.0; cache_read_per_m = None } );
    ( "zai_coding/glm-5",
      { input_per_m = 1.20; output_per_m = 5.0; cache_read_per_m = None } );
    ( "zai_coding/glm-4.7",
      { input_per_m = 0.60; output_per_m = 2.20; cache_read_per_m = None } );
    ( "zai_coding/glm-4.6",
      { input_per_m = 0.60; output_per_m = 2.20; cache_read_per_m = None } );
    (* Xiaomi MiMo *)
    ( "mimo-v2-flash",
      { input_per_m = 0.10; output_per_m = 0.30; cache_read_per_m = None } );
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
  | Some p ->
      let input_cost =
        float_of_int prompt_tokens *. p.input_per_m /. 1_000_000.0
      in
      let output_cost =
        float_of_int completion_tokens *. p.output_per_m /. 1_000_000.0
      in
      input_cost +. output_cost

let calculate_cost_with_cache ~model ~prompt_tokens ~completion_tokens
    ~added_prompt_tokens ~cache_hit ?(api_cached_tokens = 0) () =
  match lookup_pricing model with
  | None -> 0.0
  | Some p -> (
      match (cache_hit, p.cache_read_per_m) with
      | true, Some cache_per_m ->
          let cached_tokens =
            if api_cached_tokens > 0 then api_cached_tokens
            else max 0 (prompt_tokens - added_prompt_tokens)
          in
          let fresh_tokens = max 0 (prompt_tokens - cached_tokens) in
          let fresh_cost =
            float_of_int fresh_tokens *. p.input_per_m /. 1_000_000.0
          in
          let cached_cost =
            float_of_int cached_tokens *. cache_per_m /. 1_000_000.0
          in
          let output_cost =
            float_of_int completion_tokens *. p.output_per_m /. 1_000_000.0
          in
          fresh_cost +. cached_cost +. output_cost
      | _ ->
          let input_cost =
            float_of_int prompt_tokens *. p.input_per_m /. 1_000_000.0
          in
          let output_cost =
            float_of_int completion_tokens *. p.output_per_m /. 1_000_000.0
          in
          input_cost +. output_cost)

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
