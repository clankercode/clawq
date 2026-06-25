type model_info = {
  provider : string;
  id : string;
  display_name : string option;
  context_window : int option;
  supports_vision : bool;
  supports_tools : bool;
  supports_thinking : bool;
  deprecated : bool;
}

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
      })
    Xiaomi.catalog_specs

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
    };
    (* OpenAI *)
    {
      provider = "openai";
      id = "gpt-5.4-pro";
      display_name = None;
      context_window = Some 272000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "openai";
      id = "gpt-5.4";
      display_name = None;
      context_window = Some 272000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    (* OpenAI Codex *)
    {
      provider = "openai-codex";
      id = "gpt-5.3-codex-spark";
      display_name = None;
      context_window = Some 128000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "openai-codex";
      id = "gpt-5.3-codex";
      display_name = None;
      context_window = Some 272000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "openai-codex";
      id = "gpt-5.5";
      display_name = None;
      context_window = Some 272000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "openai";
      id = "gpt-5.1";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "openai";
      id = "gpt-5-nano";
      display_name = None;
      context_window = Some 128000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
    };
    {
      provider = "openai";
      id = "gpt-5";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "openai";
      id = "gpt-4o";
      display_name = None;
      context_window = Some 128000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
    };
    {
      provider = "openai";
      id = "gpt-4o-mini";
      display_name = None;
      context_window = Some 128000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
    };
    {
      provider = "openai";
      id = "gpt-4-turbo";
      display_name = None;
      context_window = Some 128000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
    };
    {
      provider = "openai";
      id = "gpt-4";
      display_name = None;
      context_window = Some 8192;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = true;
    };
    {
      provider = "openai";
      id = "gpt-3.5-turbo";
      display_name = None;
      context_window = Some 16385;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = true;
    };
    {
      provider = "openai";
      id = "o3";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "openai";
      id = "o3-mini";
      display_name = None;
      context_window = Some 200000;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "openai";
      id = "o1";
      display_name = None;
      context_window = Some 200000;
      supports_vision = false;
      supports_tools = false;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "openai";
      id = "o1-mini";
      display_name = None;
      context_window = Some 128000;
      supports_vision = false;
      supports_tools = false;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "openai";
      id = "o1-preview";
      display_name = None;
      context_window = Some 128000;
      supports_vision = false;
      supports_tools = false;
      supports_thinking = true;
      deprecated = true;
    };
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
    };
    (* DeepSeek *)
    {
      provider = "deepseek";
      id = "deepseek-v3";
      display_name = None;
      context_window = Some 128000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
    };
    {
      provider = "deepseek";
      id = "deepseek-r1";
      display_name = None;
      context_window = Some 128000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
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
    };
    (* Mistral *)
    {
      provider = "mistral";
      id = "mistral-large";
      display_name = None;
      context_window = Some 128000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
    };
    {
      provider = "mistral";
      id = "mixtral-8x7b";
      display_name = None;
      context_window = Some 32768;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
    };
    (* Cohere *)
    {
      provider = "cohere";
      id = "command-r-plus";
      display_name = None;
      context_window = Some 128000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
    };
    {
      provider = "cohere";
      id = "command-r";
      display_name = None;
      context_window = Some 128000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
    };
    (* Kimi via Moonshot platform — K2 preview series EOL 2026-05-25 *)
    {
      provider = "kimi";
      id = "kimi-k2.6";
      display_name = None;
      context_window = Some 262144;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "kimi";
      id = "kimi-k2.5";
      display_name = None;
      context_window = Some 262144;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "kimi";
      id = "kimi-k2-thinking";
      display_name = None;
      context_window = Some 262144;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "kimi";
      id = "kimi-k2-thinking-turbo";
      display_name = None;
      context_window = Some 262144;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "kimi";
      id = "kimi-k2-turbo-preview";
      display_name = None;
      context_window = Some 262144;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      (* K2 preview series scheduled EOL 2026-05-25 *)
      deprecated = true;
    };
    {
      provider = "kimi";
      id = "kimi-k2-0905-preview";
      display_name = None;
      context_window = Some 262144;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = true;
    };
    (* Kimi Coding subscription — kimi-for-coding is a stable backend-routed
       alias that automatically points at the current model (currently K2.6).
       Always use kimi-for-coding as the default on this endpoint per docs. *)
    {
      provider = "kimi_coding";
      id = "kimi-for-coding";
      display_name = Some "Kimi for Coding";
      context_window = Some 262144;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "kimi_coding";
      id = "kimi-k2.6";
      display_name = None;
      context_window = Some 262144;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "kimi_coding";
      id = "kimi-k2.5";
      display_name = None;
      context_window = Some 262144;
      supports_vision = true;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    (* Z.ai *)
    {
      provider = "zai";
      id = "glm-5.2";
      display_name = None;
      (* Advertised window; the runtime compaction budget caps glm-5.2 to 272k
         (see default_model_context_caps) because the API slows past ~500k. *)
      context_window = Some 1048576;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "zai";
      id = "glm-5.1";
      display_name = None;
      context_window = Some 200000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "zai";
      id = "glm-5-turbo";
      display_name = None;
      context_window = Some 200000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "zai";
      id = "glm-5";
      display_name = None;
      context_window = Some 200000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "zai";
      id = "glm-4.7";
      display_name = None;
      context_window = Some 200000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "zai";
      id = "glm-4.6";
      display_name = None;
      context_window = Some 200000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
    };
    (* Minimax *)
    {
      provider = "minimax";
      id = "MiniMax-M3";
      display_name = Some "MiniMax-M3";
      context_window = Some 512000;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "minimax";
      id = "MiniMax-M2.7";
      display_name = Some "MiniMax-M2.7";
      context_window = Some 204800;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "minimax";
      id = "MiniMax-M2.7-highspeed";
      display_name = Some "MiniMax-M2.7-highspeed";
      context_window = Some 204800;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "minimax";
      id = "MiniMax-M2.5";
      display_name = Some "MiniMax-M2.5";
      context_window = Some 204800;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "minimax";
      id = "MiniMax-M2.5-highspeed";
      display_name = Some "MiniMax-M2.5-highspeed";
      context_window = Some 204800;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "minimax";
      id = "MiniMax-M2.1";
      display_name = Some "MiniMax-M2.1";
      context_window = Some 204800;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "minimax";
      id = "MiniMax-M2.1-highspeed";
      display_name = Some "MiniMax-M2.1-highspeed";
      context_window = Some 204800;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
    {
      provider = "minimax";
      id = "MiniMax-M2";
      display_name = Some "MiniMax-M2";
      context_window = Some 204800;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = true;
      deprecated = false;
    };
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
      match find_by_full_name name with
      | Some _ -> None
      | None ->
          Some
            (Printf.sprintf
               "Unknown model '%s'. Use /model set-force %s to set anyway, or \
                use provider:model format (e.g., openai:%s)."
               name name name))

let format_context_window = function
  | None -> ""
  | Some n ->
      if n >= 1_000_000 then
        Printf.sprintf "%.1fM" (float_of_int n /. 1_000_000.0)
      else Printf.sprintf "%dK" (n / 1000)

let to_plain_list ?(provider_filter = None) ?(db_extras = []) () =
  let filtered =
    match provider_filter with None -> known_models | Some p -> by_provider p
  in
  let non_deprecated = List.filter (fun m -> not m.deprecated) filtered in
  let catalog_lines =
    List.map
      (fun m ->
        let full = Printf.sprintf "%s:%s" m.provider m.id in
        let ctx = format_context_window m.context_window in
        let badges = Buffer.create 16 in
        if m.supports_vision then Buffer.add_string badges " vision";
        if m.supports_thinking then Buffer.add_string badges " thinking";
        let badge_str = Buffer.contents badges in
        if ctx = "" then full
        else if badge_str = "" then Printf.sprintf "%s (%s)" full ctx
        else Printf.sprintf "%s (%s%s)" full ctx badge_str)
      non_deprecated
  in
  let extra_lines =
    List.map (fun (p, m) -> Printf.sprintf "%s:%s [db]" p m) db_extras
  in
  String.concat "\n" (catalog_lines @ extra_lines)

let to_json ?(provider_filter = None) () : Yojson.Safe.t =
  let filtered =
    match provider_filter with None -> known_models | Some p -> by_provider p
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
           :: fields
         in
         `Assoc (List.rev fields))
       filtered)
