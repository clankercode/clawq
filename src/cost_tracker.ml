let pricing_table =
  [
    ("claude-opus-4-6", (15.0, 75.0));
    ("claude-sonnet-4-6", (3.0, 15.0));
    ("claude-haiku-4-5", (0.80, 4.0));
    ("claude-3.5-sonnet", (3.0, 15.0));
    ("claude-3.5-haiku", (0.80, 4.0));
    ("claude-3-opus", (15.0, 75.0));
    ("claude-3-sonnet", (3.0, 15.0));
    ("claude-3-haiku", (0.25, 1.25));
    ("gpt-4o", (2.50, 10.0));
    ("gpt-4o-mini", (0.15, 0.60));
    ("gpt-4-turbo", (10.0, 30.0));
    ("gpt-4", (30.0, 60.0));
    ("gpt-3.5-turbo", (0.50, 1.50));
    ("o1", (15.0, 60.0));
    ("o1-mini", (3.0, 12.0));
    ("o3", (10.0, 40.0));
    ("o3-mini", (1.10, 4.40));
    ("llama-3.3-70b", (0.59, 0.79));
    ("llama-3.1-405b", (3.0, 3.0));
    ("llama-3.1-70b", (0.59, 0.79));
    ("llama-3.1-8b", (0.05, 0.08));
    ("mistral-large", (2.0, 6.0));
    ("mixtral-8x7b", (0.24, 0.24));
    ("gemini-2.0-flash", (0.10, 0.40));
    ("gemini-1.5-pro", (1.25, 5.0));
    ("gemini-1.5-flash", (0.075, 0.30));
    ("deepseek-v3", (0.27, 1.10));
    ("deepseek-r1", (0.55, 2.19));
    ("command-r-plus", (2.50, 10.0));
    ("command-r", (0.15, 0.60));
  ]

let normalize_model s =
  let s = String.lowercase_ascii (String.trim s) in
  let strip_provider s =
    match String.index_opt s '/' with
    | Some i when i + 1 < String.length s ->
        String.sub s (i + 1) (String.length s - i - 1)
    | _ -> s
  in
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
  strip_date (strip_provider s)

let lookup_pricing model =
  let norm = normalize_model model in
  let find_prefix hay needle =
    String.length hay >= String.length needle
    && String.sub hay 0 (String.length needle) = needle
  in
  match List.find_opt (fun (k, _) -> norm = k) pricing_table with
  | Some (_, v) -> Some v
  | None -> (
      match List.find_opt (fun (k, _) -> find_prefix norm k) pricing_table with
      | Some (_, v) -> Some v
      | None -> None)

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
  Logs.info (fun m ->
      m "Cost: model=%s prompt=%d completion=%d cost=$%.6f session_total=$%.4f"
        model prompt_tokens completion_tokens cost stats.total_cost)

let get_session_cost ~session_id =
  match Hashtbl.find_opt sessions session_id with
  | Some s -> s.total_cost
  | None -> 0.0

let get_session_stats ~session_id = Hashtbl.find_opt sessions session_id
