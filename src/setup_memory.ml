(* setup_memory.ml — Interactive setup wizard for memory configuration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_weight s =
  match int_of_string_opt s with
  | Some v when v >= 0 && v <= 100 -> Ok s
  | Some _ -> Error "Weight must be between 0 and 100."
  | None -> Error "Weight must be a valid integer."

let validate_compaction_threshold s =
  match int_of_string_opt s with
  | Some v when v >= 1 && v <= 100 -> Ok s
  | Some _ -> Error "Compaction threshold must be between 1 and 100."
  | None -> Error "Compaction threshold must be a valid integer."

let validate_positive_int s =
  match int_of_string_opt s with
  | Some v when v > 0 -> Ok s
  | Some _ -> Error "Value must be a positive integer."
  | None -> Error "Value must be a valid integer."

let build_memory_json ~backend ~search_enabled ~vector_weight ~keyword_weight
    ~embedding_model ~embedding_provider ~compaction_threshold_percent
    ~max_messages_per_session ~max_message_age_days ~pre_compaction_flush
    ~task_tree_purge_after_days =
  let opt_str_or_null s = if s = "" then `Null else `String s in
  `Assoc
    [
      ( "memory",
        `Assoc
          [
            ("backend", `String backend);
            ("search_enabled", `Bool search_enabled);
            ("vector_weight", `Int vector_weight);
            ("keyword_weight", `Int keyword_weight);
            ("embedding_model", opt_str_or_null embedding_model);
            ("embedding_provider", opt_str_or_null embedding_provider);
            ("compaction_threshold_percent", `Int compaction_threshold_percent);
            ("max_messages_per_session", `Int max_messages_per_session);
            ("max_message_age_days", `Int max_message_age_days);
            ("pre_compaction_flush", `Bool pre_compaction_flush);
            ("task_tree_purge_after_days", `Int task_tree_purge_after_days);
          ] );
    ]

let post_setup_instructions =
  {|
  Memory configuration setup:

    1. backend: Storage backend for memory. Currently only "sqlite" is supported.
    2. search_enabled: Enable semantic search over memory (requires embedding
       model/provider to be configured).
    3. vector_weight / keyword_weight: Weights for hybrid search (must sum to 100).
    4. embedding_model / embedding_provider: Required when search_enabled=true.
       e.g. embedding_model="text-embedding-3-small", embedding_provider="openai".
    5. compaction_threshold_percent: When session history exceeds this percentage
       of max_messages_per_session, compaction is triggered (1-100).
    6. max_messages_per_session: Maximum messages kept per session before compaction.
    7. max_message_age_days: Messages older than this are eligible for cleanup.
    8. pre_compaction_flush: Flush pending writes before compaction begins.
    9. task_tree_purge_after_days: Hard-purge soft-deleted task tree rows after
       this many days. Set to -1 to disable (default).

  After saving:

    - Restart the daemon: clawq daemon restart
    - Verify: clawq status

  Full documentation: https://clawq.org/memory/
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    Some cfg.memory
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let default = Runtime_config.default.memory in
  let get_mem f = match existing with Some c -> f c | None -> f default in
  let backend =
    Setup_tui.make_choice_field ~key:"b" ~label:"Backend"
      ~menu_label:"Set storage backend" ~choices:[ "sqlite" ]
      ~description:"Storage backend for memory. Only sqlite is supported."
      ~default:(get_mem (fun c -> c.Runtime_config.backend))
      ()
  in
  let search_enabled =
    Setup_tui.make_bool_field ~key:"e" ~label:"Search enabled"
      ~menu_label:"Toggle semantic search"
      ~description:"Enable semantic/hybrid search over memory."
      ~default:(get_mem (fun c -> c.Runtime_config.search_enabled))
      ()
  in
  let vector_weight =
    Setup_tui.make_int_field ~key:"v" ~label:"Vector weight"
      ~menu_label:"Set vector search weight (0-100)"
      ~description:
        "Weight for vector search in hybrid search (must sum to 100 with \
         keyword weight)."
      ~validate:validate_weight
      ~default:(get_mem (fun c -> c.Runtime_config.vector_weight))
      ()
  in
  let keyword_weight =
    Setup_tui.make_int_field ~key:"k" ~label:"Keyword weight"
      ~menu_label:"Set keyword search weight (0-100)"
      ~description:
        "Weight for keyword search in hybrid search (must sum to 100 with \
         vector weight)."
      ~validate:validate_weight
      ~default:(get_mem (fun c -> c.Runtime_config.keyword_weight))
      ()
  in
  let embedding_model =
    Setup_tui.make_field ~key:"em" ~label:"Embedding model"
      ~menu_label:"Set embedding model (optional)"
      ~description:
        "Embedding model for semantic search, e.g. text-embedding-3-small. \
         Leave blank to disable."
      ~default:
        (match get_mem (fun c -> c.Runtime_config.embedding_model) with
        | Some s -> s
        | None -> "")
      ()
  in
  let embedding_provider =
    Setup_tui.make_field ~key:"ep" ~label:"Embedding provider"
      ~menu_label:"Set embedding provider (optional)"
      ~description:
        "Provider for embedding model, e.g. openai. Leave blank to disable."
      ~default:
        (match get_mem (fun c -> c.Runtime_config.embedding_provider) with
        | Some s -> s
        | None -> "")
      ()
  in
  let compaction_threshold_percent =
    Setup_tui.make_int_field ~key:"c" ~label:"Compaction threshold %"
      ~menu_label:"Set compaction threshold (1-100)"
      ~description:
        "Trigger compaction when session history fills this percent of max \
         messages."
      ~validate:validate_compaction_threshold
      ~default:
        (get_mem (fun c -> c.Runtime_config.compaction_threshold_percent))
      ()
  in
  let max_messages_per_session =
    Setup_tui.make_int_field ~key:"m" ~label:"Max messages/session"
      ~menu_label:"Set max messages per session"
      ~description:
        "Maximum messages kept per session before compaction triggers."
      ~validate:validate_positive_int
      ~default:(get_mem (fun c -> c.Runtime_config.max_messages_per_session))
      ()
  in
  let max_message_age_days =
    Setup_tui.make_int_field ~key:"a" ~label:"Max message age (days)"
      ~menu_label:"Set max message age in days"
      ~description:
        "Messages older than this many days are eligible for cleanup."
      ~validate:validate_positive_int
      ~default:(get_mem (fun c -> c.Runtime_config.max_message_age_days))
      ()
  in
  let pre_compaction_flush =
    Setup_tui.make_bool_field ~key:"f" ~label:"Pre-compaction flush"
      ~menu_label:"Toggle pre-compaction flush"
      ~description:"Flush pending writes to DB before compaction begins."
      ~default:(get_mem (fun c -> c.Runtime_config.pre_compaction_flush))
      ()
  in
  let task_tree_purge_after_days =
    Setup_tui.make_int_field ~key:"t" ~label:"Task tree purge (days)"
      ~menu_label:"Set task tree purge interval (-1 to disable)"
      ~description:
        "Hard-purge soft-deleted task tree rows after this many days. -1 \
         disables."
      ~default:(get_mem (fun c -> c.Runtime_config.task_tree_purge_after_days))
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Memory Configuration ";
      docs_url = "https://clawq.org/memory/";
      fields =
        [
          backend;
          search_enabled;
          vector_weight;
          keyword_weight;
          embedding_model;
          embedding_provider;
          compaction_threshold_percent;
          max_messages_per_session;
          max_message_age_days;
          pre_compaction_flush;
          task_tree_purge_after_days;
        ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_memory_json
            ~backend:(Setup_tui.get_str backend)
            ~search_enabled:(Setup_tui.get_bool search_enabled)
            ~vector_weight:(Setup_tui.get_int vector_weight)
            ~keyword_weight:(Setup_tui.get_int keyword_weight)
            ~embedding_model:(Setup_tui.get_str embedding_model)
            ~embedding_provider:(Setup_tui.get_str embedding_provider)
            ~compaction_threshold_percent:
              (Setup_tui.get_int compaction_threshold_percent)
            ~max_messages_per_session:
              (Setup_tui.get_int max_messages_per_session)
            ~max_message_age_days:(Setup_tui.get_int max_message_age_days)
            ~pre_compaction_flush:(Setup_tui.get_bool pre_compaction_flush)
            ~task_tree_purge_after_days:
              (Setup_tui.get_int task_tree_purge_after_days));
      pre_save_check =
        (fun () ->
          let vw = Setup_tui.get_int vector_weight in
          let kw = Setup_tui.get_int keyword_weight in
          if vw + kw <> 100 then
            Error
              (Printf.sprintf
                 "vector_weight (%d) + keyword_weight (%d) must equal 100." vw
                 kw)
          else Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
