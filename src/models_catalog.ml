type model_info = {
  provider : string;
  id : string;
  display_name : string option;
  context_window : int option;
  supports_vision : bool;
  supports_tools : bool;
  supports_thinking : bool;
  deprecated : bool;
  unavailable : bool;
}

type availability_filter = Available | Unavailable | All

let is_available (m : model_info) = (not m.deprecated) && not m.unavailable

let matches_availability availability m =
  match availability with
  | Available -> is_available m
  | Unavailable -> not (is_available m)
  | All -> true

let availability_filter_of_string value =
  match String.lowercase_ascii (String.trim value) with
  | "" | "available" -> Some Available
  | "unavailable" -> Some Unavailable
  | "all" -> Some All
  | _ -> None

let availability_filter_to_string = function
  | Available -> "available"
  | Unavailable -> "unavailable"
  | All -> "all"

(* B697: Xiaomi MiMo catalog entries, derived from Xiaomi.catalog_specs so the
   data lives in xiaomi.ml (one source of truth across catalog/pricing/routing).
   All MiMo models are reasoning + tool-capable. *)
let xiaomi_catalog_models : model_info list =
  List.map
    (fun (provider, (s : Xiaomi.model_spec)) ->
      {
        provider;
        id = s.Xiaomi.id;
        display_name = Some s.Xiaomi.display;
        context_window = Some s.Xiaomi.context_window;
        supports_vision = s.Xiaomi.supports_vision;
        supports_tools = true;
        supports_thinking = true;
        deprecated = false;
        unavailable = false;
      })
    Xiaomi.catalog_specs

let model ?display_name ?context_window ?(supports_vision = false)
    ?(supports_tools = true) ?(supports_thinking = false) ?(deprecated = false)
    ?(unavailable = false) provider id =
  {
    provider;
    id;
    display_name;
    context_window;
    supports_vision;
    supports_tools;
    supports_thinking;
    deprecated;
    unavailable;
  }

let known_models : model_info list =
  [
    (* Anthropic Claude *)
    {
      provider = "anthropic";
      id = "claude-opus-4-6";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-opus-4-5";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-opus-4-1";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-opus-4-0";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-sonnet-4-6";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-sonnet-4-5";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-sonnet-4-0";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-haiku-4-5";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-3-7-sonnet";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-3-5-sonnet";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-3-5-haiku";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-3-opus";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-3-sonnet";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = false;
      deprecated = true;
      unavailable = false;
    };
    {
      provider = "anthropic";
      id = "claude-3-haiku";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
      unavailable = false;
    };
    (* OpenAI *)
    model "openai" "gpt-5.5" ~context_window:272000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "gpt-5.4" ~context_window:1050000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "gpt-5.4-pro" ~context_window:1050000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "gpt-5.2" ~context_window:400000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "gpt-5.2-pro" ~context_window:400000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "gpt-5-pro" ~context_window:400000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "gpt-5-mini" ~context_window:400000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "gpt-5" ~context_window:400000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "gpt-5.1" ~context_window:200000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "gpt-5-nano" ~context_window:400000 ~supports_vision:true;
    model "openai" "gpt-4.1" ~context_window:1000000 ~supports_vision:true;
    model "openai" "gpt-4.1-mini" ~context_window:1000000 ~supports_vision:true;
    model "openai" "gpt-4.1-nano" ~context_window:1000000 ~supports_vision:true;
    model "openai" "gpt-4o" ~context_window:128000 ~supports_vision:true;
    model "openai" "gpt-4o-mini" ~context_window:128000 ~supports_vision:true;
    model "openai" "gpt-4-turbo" ~context_window:128000 ~supports_vision:true;
    model "openai" "gpt-4" ~context_window:8192 ~deprecated:true;
    model "openai" "gpt-3.5-turbo" ~context_window:16385 ~deprecated:true;
    model "openai" "o3" ~context_window:200000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "o3-pro" ~context_window:200000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "o3-mini" ~context_window:200000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "o4-mini" ~context_window:200000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai" "o1" ~context_window:200000 ~supports_tools:false
      ~supports_thinking:true;
    model "openai" "o1-pro" ~context_window:200000 ~supports_tools:false
      ~supports_thinking:true;
    model "openai" "o1-mini" ~context_window:128000 ~supports_tools:false
      ~supports_thinking:true;
    model "openai" "o1-preview" ~context_window:128000 ~supports_tools:false
      ~supports_thinking:true ~deprecated:true;
    (* OpenAI Codex *)
    model "openai-codex" "gpt-5-codex" ~context_window:272000
      ~supports_vision:true ~supports_thinking:true;
    model "openai-codex" "gpt-5.3-codex-spark" ~context_window:128000
      ~supports_thinking:true;
    model "openai-codex" "gpt-5.3-codex" ~context_window:272000
      ~supports_vision:true ~supports_thinking:true;
    model "openai-codex" "gpt-5.4" ~context_window:1050000 ~supports_vision:true
      ~supports_thinking:true;
    model "openai-codex" "gpt-5.4-mini" ~context_window:400000
      ~supports_vision:true ~supports_thinking:true;
    model "openai-codex" "gpt-5.5" ~context_window:272000 ~supports_vision:true
      ~supports_thinking:true;
    (* Google Gemini *)
    {
      provider = "gemini";
      id = "gemini-3-pro";
      display_name = None;
      context_window = Some 2097152;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "gemini";
      id = "gemini-3-flash";
      display_name = None;
      context_window = Some 1048576;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "gemini";
      id = "gemini-3.1-pro";
      display_name = None;
      context_window = Some 2097152;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "gemini";
      id = "gemini-2.5-pro";
      display_name = None;
      context_window = Some 2097152;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "gemini";
      id = "gemini-2.5-flash";
      display_name = None;
      context_window = Some 1048576;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "gemini";
      id = "gemini-2.0-flash";
      display_name = None;
      context_window = Some 1048576;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "gemini";
      id = "gemini-1.5-pro";
      display_name = None;
      context_window = Some 2097152;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "gemini";
      id = "gemini-1.5-flash";
      display_name = None;
      context_window = Some 1048576;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
      unavailable = false;
    };
    (* DeepSeek *)
    model "deepseek" "deepseek-chat" ~context_window:131072;
    model "deepseek" "deepseek-reasoner" ~context_window:131072
      ~supports_thinking:true;
    (* Meta Llama *)
    {
      provider = "ollama";
      id = "llama-3.3-70b";
      display_name = None;
      context_window = Some 128000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "ollama";
      id = "llama-3.1-405b";
      display_name = None;
      context_window = Some 128000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "ollama";
      id = "llama-3.1-70b";
      display_name = None;
      context_window = Some 128000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
      unavailable = false;
    };
    {
      provider = "ollama";
      id = "llama-3.1-8b";
      display_name = None;
      context_window = Some 128000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
      unavailable = false;
    };
    (* Mistral *)
    model "mistral" "mistral-large-latest" ~context_window:262144
      ~supports_vision:true;
    model "mistral" "mistral-medium-latest" ~context_window:131072
      ~supports_vision:true;
    model "mistral" "mistral-small-latest" ~context_window:131072
      ~supports_vision:true;
    model "mistral" "codestral-latest" ~context_window:262144;
    (* Cohere *)
    model "cohere" "command-a-03-2025" ~context_window:262144;
    model "cohere" "command-r-plus-08-2024" ~context_window:131072;
    model "cohere" "command-r-08-2024" ~context_window:131072;
    (* Groq *)
    model "groq" "llama-3.3-70b-versatile" ~context_window:131072;
    model "groq" "llama-3.1-8b-instant" ~context_window:131072;
    model "groq" "meta-llama/llama-4-scout-17b-16e-instruct"
      ~context_window:131072 ~supports_vision:true;
    model "groq" "meta-llama/llama-4-maverick-17b-128e-instruct"
      ~context_window:131072 ~supports_vision:true;
    model "groq" "openai/gpt-oss-20b" ~context_window:131072
      ~supports_thinking:true;
    model "groq" "openai/gpt-oss-120b" ~context_window:131072
      ~supports_thinking:true;
    model "groq" "qwen/qwen3-32b" ~context_window:131072 ~supports_thinking:true;
    model "groq" "moonshotai/kimi-k2-instruct-0905" ~context_window:262144;
    (* Kimi via Moonshot platform — K2 preview series EOL 2026-05-25 *)
    model "kimi" "kimi-k2.6" ~context_window:262144 ~supports_vision:true
      ~supports_thinking:true;
    model "kimi" "kimi-k2.5" ~context_window:262144 ~supports_vision:true
      ~supports_thinking:true;
    model "kimi" "kimi-k2-thinking" ~context_window:262144
      ~supports_thinking:true;
    model "kimi" "kimi-k2-thinking-turbo" ~context_window:262144
      ~supports_thinking:true;
    model "kimi" "kimi-k2-turbo-preview" ~context_window:262144 ~deprecated:true;
    model "kimi" "kimi-k2-0905-preview" ~context_window:262144 ~deprecated:true;
    (* Kimi Coding subscription — kimi-for-coding is a stable backend-routed
       alias that automatically points at the current coding backend.
       Always use kimi-for-coding as the default on this endpoint per docs. *)
    model "kimi_coding" "kimi-for-coding" ~display_name:"Kimi for Coding"
      ~context_window:262144 ~supports_vision:true ~supports_thinking:true;
    model "kimi_coding" "kimi-k2.7-code" ~context_window:262144
      ~supports_vision:true ~supports_thinking:true;
    model "kimi_coding" "kimi-k2.7-code-highspeed" ~context_window:262144
      ~supports_vision:true ~supports_thinking:true;
    model "kimi_coding" "kimi-k2.6" ~context_window:262144 ~supports_vision:true
      ~supports_thinking:true;
    model "kimi_coding" "kimi-k2.5" ~context_window:262144 ~supports_vision:true
      ~supports_thinking:true;
    (* Z.ai *)
    (* Runtime compaction budget caps glm-5.2 to 272k
       (see default_model_context_caps) because the API slows past ~500k. *)
    model "zai" "glm-5.2" ~context_window:1000000 ~supports_thinking:true;
    model "zai" "glm-5.1" ~context_window:204800 ~supports_thinking:true;
    model "zai" "glm-5-turbo" ~context_window:200000 ~supports_thinking:true;
    model "zai" "glm-5" ~context_window:200000 ~supports_thinking:true;
    model "zai" "glm-4.7" ~context_window:200000 ~supports_thinking:true;
    model "zai" "glm-4.7-flashx" ~context_window:200000;
    model "zai" "glm-4.7-flash" ~context_window:200000;
    model "zai" "glm-4.6" ~context_window:200000 ~supports_thinking:true;
    model "zai" "glm-4.6v" ~context_window:200000 ~supports_vision:true;
    model "zai" "glm-4.5" ~context_window:128000;
    model "zai" "glm-4.5-x" ~context_window:128000;
    model "zai" "glm-4.5-air" ~context_window:128000;
    model "zai" "glm-4.5-airx" ~context_window:128000;
    model "zai" "glm-4.5-flash" ~context_window:128000;
    model "zai_coding" "glm-5" ~context_window:200000 ~supports_thinking:true;
    model "zai_coding" "glm-5.2" ~context_window:1000000 ~supports_thinking:true;
    model "zai_coding" "glm-5.1" ~context_window:204800 ~supports_thinking:true;
    model "zai_coding" "glm-5-turbo" ~context_window:200000
      ~supports_thinking:true;
    model "zai_coding" "glm-4.7" ~context_window:200000 ~supports_thinking:true;
    model "zai_coding" "glm-4.6" ~context_window:200000 ~supports_thinking:true;
    model "zai_coding" "glm-4.5" ~context_window:128000;
    (* Minimax *)
    model "minimax" "MiniMax-M3" ~display_name:"MiniMax-M3"
      ~context_window:512000 ~supports_vision:true ~supports_thinking:true;
    model "minimax" "MiniMax-M2.7" ~display_name:"MiniMax-M2.7"
      ~context_window:204800 ~supports_thinking:true;
    model "minimax" "MiniMax-M2.7-highspeed"
      ~display_name:"MiniMax-M2.7-highspeed" ~context_window:204800
      ~supports_thinking:true;
    model "minimax" "MiniMax-M2.5" ~display_name:"MiniMax-M2.5"
      ~context_window:204800 ~supports_thinking:true ~deprecated:true;
    model "minimax" "MiniMax-M2.5-highspeed"
      ~display_name:"MiniMax-M2.5-highspeed" ~context_window:204800
      ~supports_thinking:true ~deprecated:true;
    model "minimax" "MiniMax-M2.1" ~display_name:"MiniMax-M2.1"
      ~context_window:204800 ~supports_thinking:true ~deprecated:true;
    model "minimax" "MiniMax-M2.1-highspeed"
      ~display_name:"MiniMax-M2.1-highspeed" ~context_window:204800
      ~supports_thinking:true ~deprecated:true;
    model "minimax" "MiniMax-M2" ~display_name:"MiniMax-M2"
      ~context_window:204800 ~supports_thinking:true ~deprecated:true;
  ]
  @ xiaomi_catalog_models

let providers =
  let tbl = Hashtbl.create 16 in
  List.iter (fun m -> Hashtbl.replace tbl m.provider ()) known_models;
  Hashtbl.fold (fun k () acc -> k :: acc) tbl [] |> List.sort String.compare

let by_provider provider =
  List.filter (fun m -> m.provider = provider) known_models

let model_id_matches (m : model_info) id =
  m.id = id || String.lowercase_ascii m.id = String.lowercase_ascii id

let find_by_id id = List.find_opt (fun m -> model_id_matches m id) known_models
let full_name m = m.provider ^ ":" ^ m.id

let find_all_by_id id =
  List.filter (fun m -> model_id_matches m id) known_models

let contains_case_insensitive ~needle haystack =
  let needle = String.lowercase_ascii (String.trim needle) in
  let haystack = String.lowercase_ascii haystack in
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop i =
    i + needle_len <= haystack_len
    && (String.sub haystack i needle_len = needle || loop (i + 1))
  in
  needle <> "" && loop 0

let fuzzy_plain_matches name =
  let exact = find_all_by_id name in
  if exact <> [] then exact
  else
    List.filter
      (fun m ->
        contains_case_insensitive ~needle:name m.id
        || contains_case_insensitive ~needle:name (full_name m)
        ||
        match m.display_name with
        | Some display -> contains_case_insensitive ~needle:name display
        | None -> false)
      known_models

(* Returns the catalog's canonical id when a case-insensitive match exists
   for (provider, id). Returns None when the model isn't in the catalog at all,
   or when the casing is already canonical. *)
let canonical_id ~provider id =
  match
    List.find_opt
      (fun m -> m.provider = provider && model_id_matches m id)
      known_models
  with
  | Some m when m.id <> id -> Some m.id
  | _ -> None

(* Returns the canonical form of a "provider:model" / "provider/model" string
   when case differs from the catalog. Returns None if already canonical or
   unknown. Preserves the input delimiter style (':' or '/'). *)
let canonical_full_name name =
  match
    match String.index_opt name ':' with
    | Some i when i > 0 && i + 1 < String.length name -> Some (':', i)
    | _ -> (
        match String.index_opt name '/' with
        | Some i when i > 0 && i + 1 < String.length name -> Some ('/', i)
        | _ -> None)
  with
  | None -> None
  | Some (delim, i) -> (
      let provider = String.sub name 0 i in
      let model = String.sub name (i + 1) (String.length name - i - 1) in
      match canonical_id ~provider model with
      | Some canonical ->
          Some (Printf.sprintf "%s%c%s" provider delim canonical)
      | None -> None)

(* Bare-name aliases for common workflows. These map a short user-typed
   alias ("kimi", "glm") to the canonical "provider:model" form.

   Resolution is case-insensitive and applies to bare names only — if the
   user typed something with a colon or slash, the alias table is NOT
   consulted (they meant a specific provider:model pair).

   Add new entries here as new short aliases are introduced. Keep this list
   short; the broader runner-aliases system (B487) handles user-defined
   aliases. *)
let aliases : (string * string) list =
  [ ("kimi", "kimi_coding:kimi-for-coding") ]

let resolve_alias name =
  let lower = String.lowercase_ascii (String.trim name) in
  match String.index_opt lower ':' with
  | Some _ -> None
  | None -> (
      match String.index_opt lower '/' with
      | Some _ -> None
      | None -> List.assoc_opt lower aliases)

(* If [name] is a known alias, return its canonical resolution; otherwise
   return [name] unchanged. Caller can compare result to input to detect
   that an alias was applied. *)
let resolve_alias_or_name name =
  match resolve_alias name with Some canonical -> canonical | None -> name

type name_format = Canonical | Legacy | Plain

(* Returns (provider, model_id, format).
   Tries ':' (canonical) first, then '/' (legacy). *)
let split_name name =
  let try_split delim fmt =
    match String.index_opt name delim with
    | Some i when i > 0 && i + 1 < String.length name ->
        let provider = String.sub name 0 i in
        let model = String.sub name (i + 1) (String.length name - i - 1) in
        Some (provider, model, fmt)
    | _ -> None
  in
  match try_split ':' Canonical with
  | Some r -> r
  | None -> (
      match try_split '/' Legacy with Some r -> r | None -> ("", name, Plain))

let find_by_full_name name =
  match split_name name with
  | provider, model, (Canonical | Legacy) ->
      List.find_opt
        (fun m -> m.provider = provider && model_id_matches m model)
        known_models
  | _, model, Plain -> find_by_id model

let validate_model_name ~configured_providers name =
  let provider, _model_id, fmt = split_name name in
  match fmt with
  | Canonical | Legacy ->
      if List.mem provider configured_providers then None
      else
        Some
          (Printf.sprintf
             "Unknown provider '%s'. Use /model set-force %s to set anyway, or \
              add '%s' to your config.json providers."
             provider name provider)
  | Plain -> (
      match fuzzy_plain_matches name with
      | [ _ ] -> None
      | _ :: _ as matches ->
          let candidates =
            matches |> List.map full_name
            |> List.sort_uniq String.compare
            |> String.concat ", "
          in
          Some
            (Printf.sprintf
               "Ambiguous model '%s'. Use provider:model format. Candidates: %s"
               name candidates)
      | [] ->
          Some
            (Printf.sprintf
               "Unknown model '%s'. Use /model set-force %s to set anyway, or \
                use provider:model format (e.g., openai:%s)."
               name name name))

type resolved_model_name = {
  canonical_value : string;
  canonical_provider : string;
  canonical_model_id : string;
  fmt : name_format;
  display_provider : string;
  display_model : string;
  hint : string;
  catalog_match : model_info option;
}

let ambiguous_plain_error name matches =
  let candidates =
    matches |> List.map full_name
    |> List.sort_uniq String.compare
    |> String.concat ", "
  in
  Printf.sprintf
    "Ambiguous model '%s'. Use provider:model format. Candidates: %s" name
    candidates

let unknown_plain_error name =
  Printf.sprintf
    "Unknown model '%s'. Use /model set-force %s to set anyway, or use \
     provider:model format (e.g., openai:%s)."
    name name name

let resolve_model_name_for_set ?(force = false)
    ?(require_configured_provider = true) ~configured_providers raw_name =
  let name = resolve_alias_or_name raw_name in
  let provider, model_id, fmt = split_name name in
  let resolve_provider_model () =
    let canonical_id =
      Option.value ~default:model_id (canonical_id ~provider model_id)
    in
    let canonical_value = provider ^ ":" ^ canonical_id in
    let hint =
      match fmt with
      | Legacy ->
          Printf.sprintf "\nHint: use %s:%s format instead." provider
            canonical_id
      | Canonical when canonical_id <> model_id ->
          Printf.sprintf "\nNote: corrected model casing \"%s\" -> \"%s\"." name
            canonical_value
      | Canonical | Plain -> ""
    in
    Ok
      {
        canonical_value;
        canonical_provider = provider;
        canonical_model_id = canonical_id;
        fmt;
        display_provider = provider;
        display_model = canonical_id;
        hint;
        catalog_match = find_by_full_name canonical_value;
      }
  in
  match fmt with
  | Canonical | Legacy ->
      if
        (not force) && require_configured_provider
        && not (List.mem provider configured_providers)
      then
        Error
          (Printf.sprintf
             "Unknown provider '%s'. Use /model set-force %s to set anyway, or \
              add '%s' to your config.json providers."
             provider name provider)
      else resolve_provider_model ()
  | Plain -> (
      let matches = fuzzy_plain_matches name in
      match matches with
      | [ m ] when m.provider <> "" ->
          let canonical_value = full_name m in
          let hint =
            if canonical_value <> raw_name then
              Printf.sprintf "\nNote: resolved bare model name to \"%s\"."
                canonical_value
            else ""
          in
          Ok
            {
              canonical_value;
              canonical_provider = m.provider;
              canonical_model_id = m.id;
              fmt = Plain;
              display_provider = m.provider;
              display_model = m.id;
              hint;
              catalog_match = Some m;
            }
      | [] when force ->
          Ok
            {
              canonical_value = name;
              canonical_provider = "";
              canonical_model_id = name;
              fmt = Plain;
              display_provider = "";
              display_model = name;
              hint = "";
              catalog_match = None;
            }
      | [] -> Error (unknown_plain_error name)
      | _ :: _ -> Error (ambiguous_plain_error name matches))

let format_context_window = function
  | None -> ""
  | Some n ->
      if n >= 1_000_000 then
        Printf.sprintf "%.1fM" (float_of_int n /. 1_000_000.0)
      else Printf.sprintf "%dK" (n / 1000)

let to_plain_list ?(provider_filter = None) ?(availability = Available)
    ?(db_extras = []) () =
  let filtered =
    match provider_filter with None -> known_models | Some p -> by_provider p
  in
  let visible = List.filter (matches_availability availability) filtered in
  let format_badges m =
    let badges = Buffer.create 16 in
    if m.supports_vision then Buffer.add_string badges " vision";
    if m.supports_thinking then Buffer.add_string badges " thinking";
    if m.deprecated then Buffer.add_string badges " deprecated";
    if m.unavailable then Buffer.add_string badges " unavailable";
    Buffer.contents badges
  in
  let catalog_lines =
    List.map
      (fun m ->
        let full = Printf.sprintf "%s:%s" m.provider m.id in
        let ctx = format_context_window m.context_window in
        let badge_str = format_badges m in
        if ctx = "" then full
        else if badge_str = "" then Printf.sprintf "%s (%s)" full ctx
        else Printf.sprintf "%s (%s%s)" full ctx badge_str)
      visible
  in
  let extra_lines =
    List.map
      (fun m ->
        let tags =
          String.concat ""
            (List.filter_map
               (fun (enabled, label) ->
                 if enabled then Some (" [" ^ label ^ "]") else None)
               [
                 (true, "db");
                 (m.deprecated, "deprecated");
                 (m.unavailable, "unavailable");
               ])
        in
        Printf.sprintf "%s:%s%s" m.provider m.id tags)
      (List.filter (matches_availability availability) db_extras)
  in
  String.concat "\n" (catalog_lines @ extra_lines)

let to_json ?(provider_filter = None) ?(availability = Available)
    ?(db_extras = []) () : Yojson.Safe.t =
  let filtered =
    match provider_filter with None -> known_models | Some p -> by_provider p
  in
  let filtered =
    List.filter (matches_availability availability) (filtered @ db_extras)
  in
  `List
    (List.map
       (fun m ->
         let fields =
           [ ("provider", `String m.provider); ("id", `String m.id) ]
         in
         let fields =
           match m.display_name with
           | None -> fields
           | Some n -> ("display_name", `String n) :: fields
         in
         let fields =
           match m.context_window with
           | None -> fields
           | Some n -> ("context_window", `Int n) :: fields
         in
         let fields =
           ("supports_vision", `Bool m.supports_vision)
           :: ("supports_tools", `Bool m.supports_tools)
           :: ("supports_thinking", `Bool m.supports_thinking)
           :: ("deprecated", `Bool m.deprecated)
           :: ("unavailable", `Bool m.unavailable)
           :: fields
         in
         `Assoc (List.rev fields))
       filtered)
