(** Versioned GitHub PR/Issue event envelopes (P19.M2.E2.T001). *)

let envelope_version = 1

type item_kind = Pull_request | Issue

type family =
  | Lifecycle
  | Review
  | Comment
  | Commit
  | Ci
  | State_update
  | Other of string

type actor = { login : string option; id : int option; type_ : string option }

type safe_state = {
  title : string option;
  state : string option;
  draft : bool option;
  merged : bool option;
  labels : string list;
  assignees : string list;
  milestone : string option;
  head_sha : string option;
  base_ref : string option;
  head_ref : string option;
}

type transfer_info = { from_repo : string option; to_repo : string option }

type t = {
  version : int;
  delivery_id : string option;
  installation_id : int option;
  event : string;
  action : string option;
  repo_full_name : string;
  org : string option;
  item_kind : item_kind option;
  item_number : int option;
  item_node_id : string option;
  item_url : string option;
  html_url : string option;
  family : family;
  actor : actor;
  item_author : string option;
  before : safe_state option;
  after : safe_state option;
  transfer : transfer_info option;
  received_at : string option;
  event_at : string option;
  head_sha : string option;
  unsupported : bool;
  skip_reason : string option;
}

type normalize_result =
  | Ok_envelope of t
  | Unsupported of { event : string; action : string option; reason : string }
  | Error of string

let empty_actor = { login = None; id = None; type_ = None }

let empty_safe_state =
  {
    title = None;
    state = None;
    draft = None;
    merged = None;
    labels = [];
    assignees = [];
    milestone = None;
    head_sha = None;
    base_ref = None;
    head_ref = None;
  }

let string_of_item_kind = function
  | Pull_request -> "pull_request"
  | Issue -> "issue"

let string_of_family = function
  | Lifecycle -> "lifecycle"
  | Review -> "review"
  | Comment -> "comment"
  | Commit -> "commit"
  | Ci -> "ci"
  | State_update -> "state_update"
  | Other s -> "other:" ^ s

let opt_string_field key = function
  | None -> []
  | Some s -> [ (key, `String s) ]

let opt_int_field key = function None -> [] | Some n -> [ (key, `Int n) ]
let opt_bool_field key = function None -> [] | Some b -> [ (key, `Bool b) ]

let safe_state_to_json (s : safe_state) =
  `Assoc
    (opt_string_field "title" s.title
    @ opt_string_field "state" s.state
    @ opt_bool_field "draft" s.draft
    @ opt_bool_field "merged" s.merged
    @ [ ("labels", `List (List.map (fun l -> `String l) s.labels)) ]
    @ [ ("assignees", `List (List.map (fun a -> `String a) s.assignees)) ]
    @ opt_string_field "milestone" s.milestone
    @ opt_string_field "head_sha" s.head_sha
    @ opt_string_field "base_ref" s.base_ref
    @ opt_string_field "head_ref" s.head_ref)

let opt_safe_state_field key = function
  | None -> []
  | Some s -> [ (key, safe_state_to_json s) ]

let actor_to_json (a : actor) =
  `Assoc
    (opt_string_field "login" a.login
    @ opt_int_field "id" a.id
    @ opt_string_field "type" a.type_)

let transfer_to_json (t : transfer_info) =
  `Assoc
    (opt_string_field "from_repo" t.from_repo
    @ opt_string_field "to_repo" t.to_repo)

let to_safe_json (env : t) =
  `Assoc
    ([
       ("version", `Int env.version);
       ("event", `String env.event);
       ("repo_full_name", `String env.repo_full_name);
       ("family", `String (string_of_family env.family));
       ("actor", actor_to_json env.actor);
       ("unsupported", `Bool env.unsupported);
     ]
    @ opt_string_field "delivery_id" env.delivery_id
    @ opt_int_field "installation_id" env.installation_id
    @ opt_string_field "action" env.action
    @ opt_string_field "org" env.org
    @ (match env.item_kind with
      | None -> []
      | Some k -> [ ("item_kind", `String (string_of_item_kind k)) ])
    @ opt_int_field "item_number" env.item_number
    @ opt_string_field "item_node_id" env.item_node_id
    @ opt_string_field "item_url" env.item_url
    @ opt_string_field "html_url" env.html_url
    @ opt_string_field "item_author" env.item_author
    @ opt_safe_state_field "before" env.before
    @ opt_safe_state_field "after" env.after
    @ (match env.transfer with
      | None -> []
      | Some t -> [ ("transfer", transfer_to_json t) ])
    @ opt_string_field "received_at" env.received_at
    @ opt_string_field "event_at" env.event_at
    @ opt_string_field "head_sha" env.head_sha
    @ opt_string_field "skip_reason" env.skip_reason)

(* --- JSON helpers (never raise) --- *)

let json_int = function
  | `Int n -> Some n
  | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
  | `Float f when Float.is_integer f -> Some (int_of_float f)
  | _ -> None

let json_string = function
  | `String s when String.trim s <> "" -> Some s
  | _ -> None

let json_bool = function `Bool b -> Some b | _ -> None

let member_opt key json =
  match json with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key json with `Null -> None | v -> Some v)
  | _ -> None

let get_string key json =
  match member_opt key json with Some v -> json_string v | None -> None

let get_int key json =
  match member_opt key json with Some v -> json_int v | None -> None

let get_bool key json =
  match member_opt key json with Some v -> json_bool v | None -> None

let get_assoc key json =
  match member_opt key json with Some (`Assoc _ as a) -> Some a | _ -> None

let get_list key json =
  match member_opt key json with Some (`List xs) -> xs | _ -> []

let non_empty_opt s =
  match s with Some s when String.trim s <> "" -> Some s | _ -> None

(* --- Field extractors --- *)

let extract_repo_full_name payload =
  match get_assoc "repository" payload with
  | None -> None
  | Some repo -> (
      match get_string "full_name" repo with
      | Some name -> Some name
      | None -> (
          match (get_string "name" repo, get_assoc "owner" repo) with
          | Some name, Some owner -> (
              match get_string "login" owner with
              | Some login -> Some (login ^ "/" ^ name)
              | None -> None)
          | _ -> None))

let extract_org payload repo_full_name =
  match get_assoc "organization" payload with
  | Some org -> get_string "login" org
  | None -> (
      match String.split_on_char '/' repo_full_name with
      | owner :: _ :: _ when owner <> "" -> Some owner
      | _ -> None)

let extract_installation_id ?installation_id payload =
  match installation_id with
  | Some id -> Some id
  | None -> (
      match get_assoc "installation" payload with
      | Some inst -> get_int "id" inst
      | None -> None)

let extract_actor payload =
  match get_assoc "sender" payload with
  | None -> empty_actor
  | Some sender ->
      {
        login = get_string "login" sender;
        id = get_int "id" sender;
        type_ = get_string "type" sender;
      }

let label_names node =
  get_list "labels" node
  |> List.filter_map (function
    | `String s when String.trim s <> "" -> Some s
    | `Assoc _ as j -> get_string "name" j
    | _ -> None)

let assignee_logins node =
  get_list "assignees" node
  |> List.filter_map (function
    | `Assoc _ as j -> get_string "login" j
    | _ -> None)

let milestone_title node =
  match get_assoc "milestone" node with
  | Some m -> get_string "title" m
  | None -> None

let head_sha_of node =
  match get_assoc "head" node with
  | Some head -> get_string "sha" head
  | None -> get_string "head_sha" node

let base_ref_of node =
  match get_assoc "base" node with
  | Some base -> get_string "ref" base
  | None -> None

let head_ref_of node =
  match get_assoc "head" node with
  | Some head -> get_string "ref" head
  | None -> None

let item_author_of node =
  match get_assoc "user" node with
  | Some user -> get_string "login" user
  | None -> None

let safe_state_of_item ?(is_pr = false) node =
  let draft =
    if is_pr then get_bool "draft" node
    else match get_bool "draft" node with Some _ as d -> d | None -> None
  in
  let merged = if is_pr then get_bool "merged" node else None in
  {
    title = get_string "title" node;
    state = get_string "state" node;
    draft;
    merged;
    labels = label_names node;
    assignees = assignee_logins node;
    milestone = milestone_title node;
    head_sha = head_sha_of node;
    base_ref = base_ref_of node;
    head_ref = head_ref_of node;
  }

let first_pr_number_from_list node key =
  match get_list key node with [] -> None | hd :: _ -> get_int "number" hd

let event_at_of_item node =
  match get_string "updated_at" node with
  | Some _ as t -> t
  | None -> get_string "created_at" node

let event_at_from_payload ~item payload =
  match item with
  | Some node -> (
      match event_at_of_item node with
      | Some _ as t -> t
      | None -> get_string "created_at" payload)
  | None -> get_string "created_at" payload

let apply_title_change before changes =
  match get_assoc "title" changes with
  | None -> before
  | Some title_change -> (
      match get_string "from" title_change with
      | None -> before
      | Some from_title -> { before with title = Some from_title })

let before_from_changes ~after changes =
  (* Start from after-state and reverse known metadata changes for journal
     before/after projection. Bodies/secrets are never stored. *)
  apply_title_change after changes

let issue_is_pr issue =
  match member_opt "pull_request" issue with
  | Some (`Null | `Bool false) | None -> false
  | Some _ -> true

let make_envelope ~delivery_id ~installation_id ~received_at ~event ~action
    ~repo_full_name ~org ~item_kind ~item_number ~item_node_id ~item_url
    ~html_url ~family ~actor ~before ~after ~transfer ~event_at ~head_sha
    ?item_author ?(unsupported = false) ?skip_reason () =
  {
    version = envelope_version;
    delivery_id = non_empty_opt delivery_id;
    installation_id;
    event;
    action;
    repo_full_name;
    org;
    item_kind;
    item_number;
    item_node_id;
    item_url;
    html_url;
    family;
    actor;
    item_author = non_empty_opt item_author;
    before;
    after;
    transfer;
    received_at = non_empty_opt received_at;
    event_at;
    head_sha;
    unsupported;
    skip_reason;
  }

let unsupported ~event ~action reason = Unsupported { event; action; reason }

(* --- Event classifiers --- *)

let pr_lifecycle_actions =
  [
    "opened";
    "reopened";
    "closed";
    "converted_to_draft";
    "ready_for_review";
    "enqueued";
    "dequeued";
  ]

let pr_state_update_actions =
  [
    "edited";
    "labeled";
    "unlabeled";
    "assigned";
    "unassigned";
    "review_requested";
    "review_request_removed";
    "auto_merge_enabled";
    "auto_merge_disabled";
    "locked";
    "unlocked";
    "milestoned";
    "demilestoned";
  ]

let issue_lifecycle_actions = [ "opened"; "reopened"; "closed"; "transferred" ]

let issue_state_update_actions =
  [
    "edited";
    "labeled";
    "unlabeled";
    "assigned";
    "unassigned";
    "milestoned";
    "demilestoned";
    "locked";
    "unlocked";
    "pinned";
    "unpinned";
    "typed";
    "untyped";
  ]

let comment_actions = [ "created"; "edited"; "deleted" ]
let review_actions = [ "submitted"; "edited"; "dismissed" ]
let review_comment_actions = [ "created"; "edited"; "deleted" ]

let check_run_actions =
  [ "created"; "completed"; "rerequested"; "requested_action" ]

let check_suite_actions = [ "completed"; "requested"; "rerequested" ]
let workflow_run_actions = [ "requested"; "completed"; "in_progress" ]

let list_mem_action actions action =
  List.exists (fun a -> String.equal a action) actions

(* --- Per-event normalizers --- *)

let normalize_pull_request ~delivery_id ~installation_id ~received_at ~event
    ~action ~payload ~repo_full_name ~org ~actor =
  let action_s = Option.value action ~default:"" in
  let pr = get_assoc "pull_request" payload in
  let item_author = Option.bind pr item_author_of in
  let after =
    match pr with
    | Some node -> Some (safe_state_of_item ~is_pr:true node)
    | None -> None
  in
  let before =
    match (get_assoc "changes" payload, after) with
    | Some changes, Some after_st ->
        Some (before_from_changes ~after:after_st changes)
    | _ -> None
  in
  let item_number =
    match pr with Some n -> get_int "number" n | None -> None
  in
  let item_node_id =
    match pr with Some n -> get_string "node_id" n | None -> None
  in
  let item_url =
    match pr with
    | Some n -> (
        match get_string "url" n with
        | Some _ as u -> u
        | None -> get_string "html_url" n)
    | None -> None
  in
  let html_url =
    match pr with Some n -> get_string "html_url" n | None -> None
  in
  let head_sha = match after with Some s -> s.head_sha | None -> None in
  let event_at = event_at_from_payload ~item:pr payload in
  let family_opt =
    if action_s = "synchronize" then Some Commit
    else if list_mem_action pr_lifecycle_actions action_s then Some Lifecycle
    else if list_mem_action pr_state_update_actions action_s then
      Some State_update
    else None
  in
  match family_opt with
  | None ->
      unsupported ~event ~action
        (Printf.sprintf "unsupported pull_request action %S" action_s)
  | Some family ->
      Ok_envelope
        (make_envelope ~delivery_id ~installation_id ~received_at ~event ~action
           ~repo_full_name ~org ~item_kind:(Some Pull_request) ~item_number
           ~item_node_id ~item_url ~html_url ~family ~actor ~before ~after
           ~transfer:None ~event_at ~head_sha ?item_author ())

let normalize_issues ~delivery_id ~installation_id ~received_at ~event ~action
    ~payload ~repo_full_name ~org ~actor =
  let action_s = Option.value action ~default:"" in
  let issue = get_assoc "issue" payload in
  let item_author = Option.bind issue item_author_of in
  let kind =
    match issue with
    | Some node when issue_is_pr node -> Pull_request
    | _ -> Issue
  in
  let after =
    match issue with
    | Some node -> Some (safe_state_of_item ~is_pr:(kind = Pull_request) node)
    | None -> None
  in
  let before =
    match (get_assoc "changes" payload, after) with
    | Some changes, Some after_st ->
        Some (before_from_changes ~after:after_st changes)
    | _ -> None
  in
  let item_number =
    match issue with Some n -> get_int "number" n | None -> None
  in
  let item_node_id =
    match issue with Some n -> get_string "node_id" n | None -> None
  in
  let item_url =
    match issue with
    | Some n -> (
        match get_string "url" n with
        | Some _ as u -> u
        | None -> get_string "html_url" n)
    | None -> None
  in
  let html_url =
    match issue with Some n -> get_string "html_url" n | None -> None
  in
  let event_at = event_at_from_payload ~item:issue payload in
  (* Transfer: repository is the source; changes.new_repository is the
     destination (per plan: from repository / changes new_repository). *)
  let transfer =
    if action_s <> "transferred" then None
    else
      let changes = get_assoc "changes" payload in
      let to_repo =
        match changes with
        | Some c -> (
            match get_assoc "new_repository" c with
            | Some nr -> get_string "full_name" nr
            | None -> None)
        | None -> None
      in
      let from_repo =
        match changes with
        | Some c -> (
            match get_assoc "old_repository" c with
            | Some old_r -> get_string "full_name" old_r
            | None -> Some repo_full_name)
        | None -> Some repo_full_name
      in
      Some { from_repo; to_repo }
  in
  let family_opt =
    if list_mem_action issue_lifecycle_actions action_s then Some Lifecycle
    else if list_mem_action issue_state_update_actions action_s then
      Some State_update
    else None
  in
  match family_opt with
  | None ->
      unsupported ~event ~action
        (Printf.sprintf "unsupported issues action %S" action_s)
  | Some family ->
      Ok_envelope
        (make_envelope ~delivery_id ~installation_id ~received_at ~event ~action
           ~repo_full_name ~org ~item_kind:(Some kind) ~item_number
           ~item_node_id ~item_url ~html_url ~family ~actor ~before ~after
           ~transfer ~event_at ~head_sha:None ?item_author ())

let normalize_issue_comment ~delivery_id ~installation_id ~received_at ~event
    ~action ~payload ~repo_full_name ~org ~actor =
  let action_s = Option.value action ~default:"" in
  if not (list_mem_action comment_actions action_s) then
    unsupported ~event ~action
      (Printf.sprintf "unsupported issue_comment action %S" action_s)
  else
    let issue = get_assoc "issue" payload in
    let item_author = Option.bind issue item_author_of in
    let comment = get_assoc "comment" payload in
    let kind =
      match issue with
      | Some node when issue_is_pr node -> Pull_request
      | _ -> Issue
    in
    let after =
      match issue with
      | Some node -> Some (safe_state_of_item ~is_pr:(kind = Pull_request) node)
      | None -> None
    in
    let item_number =
      match issue with Some n -> get_int "number" n | None -> None
    in
    let item_node_id =
      match issue with Some n -> get_string "node_id" n | None -> None
    in
    let item_url =
      match issue with Some n -> get_string "html_url" n | None -> None
    in
    let html_url =
      match comment with Some c -> get_string "html_url" c | None -> item_url
    in
    (* Never copy comment body. *)
    let event_at =
      match comment with
      | Some c -> (
          match get_string "updated_at" c with
          | Some _ as t -> t
          | None -> get_string "created_at" c)
      | None -> event_at_from_payload ~item:issue payload
    in
    let head_sha =
      match (kind, issue) with
      | Pull_request, Some node -> (
          match get_assoc "pull_request" node with
          | Some pr_obj -> head_sha_of pr_obj
          | None -> None)
      | _ -> None
    in
    Ok_envelope
      (make_envelope ~delivery_id ~installation_id ~received_at ~event ~action
         ~repo_full_name ~org ~item_kind:(Some kind) ~item_number ~item_node_id
         ~item_url ~html_url ~family:Comment ~actor ~before:None ~after
         ~transfer:None ~event_at ~head_sha ?item_author ())

let normalize_pull_request_review ~delivery_id ~installation_id ~received_at
    ~event ~action ~payload ~repo_full_name ~org ~actor =
  let action_s = Option.value action ~default:"" in
  if not (list_mem_action review_actions action_s) then
    unsupported ~event ~action
      (Printf.sprintf "unsupported pull_request_review action %S" action_s)
  else
    let pr = get_assoc "pull_request" payload in
    let item_author = Option.bind pr item_author_of in
    let review = get_assoc "review" payload in
    let after =
      match pr with
      | Some node -> Some (safe_state_of_item ~is_pr:true node)
      | None -> None
    in
    let item_number =
      match pr with Some n -> get_int "number" n | None -> None
    in
    let item_node_id =
      match pr with Some n -> get_string "node_id" n | None -> None
    in
    let item_url =
      match pr with Some n -> get_string "html_url" n | None -> None
    in
    let html_url =
      match review with
      | Some r -> (
          match get_string "html_url" r with
          | Some _ as u -> u
          | None -> item_url)
      | None -> item_url
    in
    (* Never copy review body. *)
    let head_sha =
      match after with
      | Some s when s.head_sha <> None -> s.head_sha
      | _ -> (
          match review with Some r -> get_string "commit_id" r | None -> None)
    in
    let event_at =
      match review with
      | Some r -> (
          match get_string "submitted_at" r with
          | Some _ as t -> t
          | None -> event_at_from_payload ~item:pr payload)
      | None -> event_at_from_payload ~item:pr payload
    in
    Ok_envelope
      (make_envelope ~delivery_id ~installation_id ~received_at ~event ~action
         ~repo_full_name ~org ~item_kind:(Some Pull_request) ~item_number
         ~item_node_id ~item_url ~html_url ~family:Review ~actor ~before:None
         ~after ~transfer:None ~event_at ~head_sha ?item_author ())

let normalize_pull_request_review_comment ~delivery_id ~installation_id
    ~received_at ~event ~action ~payload ~repo_full_name ~org ~actor =
  let action_s = Option.value action ~default:"" in
  if not (list_mem_action review_comment_actions action_s) then
    unsupported ~event ~action
      (Printf.sprintf "unsupported pull_request_review_comment action %S"
         action_s)
  else
    let pr = get_assoc "pull_request" payload in
    let item_author = Option.bind pr item_author_of in
    let comment = get_assoc "comment" payload in
    let after =
      match pr with
      | Some node -> Some (safe_state_of_item ~is_pr:true node)
      | None -> None
    in
    let item_number =
      match pr with Some n -> get_int "number" n | None -> None
    in
    let item_node_id =
      match pr with Some n -> get_string "node_id" n | None -> None
    in
    let item_url =
      match pr with Some n -> get_string "html_url" n | None -> None
    in
    let html_url =
      match comment with Some c -> get_string "html_url" c | None -> item_url
    in
    let head_sha =
      match after with
      | Some s when s.head_sha <> None -> s.head_sha
      | _ -> (
          match comment with Some c -> get_string "commit_id" c | None -> None)
    in
    let event_at =
      match comment with
      | Some c -> (
          match get_string "updated_at" c with
          | Some _ as t -> t
          | None -> get_string "created_at" c)
      | None -> event_at_from_payload ~item:pr payload
    in
    Ok_envelope
      (make_envelope ~delivery_id ~installation_id ~received_at ~event ~action
         ~repo_full_name ~org ~item_kind:(Some Pull_request) ~item_number
         ~item_node_id ~item_url ~html_url ~family:Comment ~actor ~before:None
         ~after ~transfer:None ~event_at ~head_sha ?item_author ())

let normalize_ci ~delivery_id ~installation_id ~received_at ~event ~action
    ~payload ~repo_full_name ~org ~actor ~object_key ~allowed_actions =
  let action_s = Option.value action ~default:"" in
  if not (list_mem_action allowed_actions action_s) then
    unsupported ~event ~action
      (Printf.sprintf "unsupported %s action %S" event action_s)
  else
    let obj = get_assoc object_key payload in
    let head_sha =
      match obj with
      | Some o -> (
          match get_string "head_sha" o with
          | Some _ as s -> s
          | None -> (
              match get_assoc "head_commit" o with
              | Some hc -> get_string "id" hc
              | None -> None))
      | None -> None
    in
    let item_number =
      match obj with
      | Some o -> first_pr_number_from_list o "pull_requests"
      | None -> None
    in
    let html_url =
      match obj with Some o -> get_string "html_url" o | None -> None
    in
    let item_url = html_url in
    let event_at =
      match obj with
      | Some o -> (
          match get_string "updated_at" o with
          | Some _ as t -> t
          | None -> (
              match get_string "completed_at" o with
              | Some _ as t -> t
              | None -> get_string "created_at" o))
      | None -> None
    in
    let item_kind = if item_number = None then None else Some Pull_request in
    Ok_envelope
      (make_envelope ~delivery_id ~installation_id ~received_at ~event ~action
         ~repo_full_name ~org ~item_kind ~item_number ~item_node_id:None
         ~item_url ~html_url ~family:Ci ~actor ~before:None ~after:None
         ~transfer:None ~event_at ~head_sha ())

let installation_events =
  [ "installation"; "installation_repositories"; "github_app_authorization" ]

let is_installation_event event =
  List.exists (fun e -> String.equal e event) installation_events

let normalize ?delivery_id ?installation_id ?received_at ~event ~payload () =
  let event = String.trim event in
  if event = "" then Error "missing event name"
  else if is_installation_event event then
    unsupported ~event
      ~action:(get_string "action" payload)
      (Printf.sprintf
         "event %S is installation-scoped, not an item envelope subject" event)
  else if event = "ping" then
    unsupported ~event ~action:None "ping is not an item event"
  else
    match extract_repo_full_name payload with
    | None ->
        Error
          (Printf.sprintf "missing repository.full_name for repo-bound event %S"
             event)
    | Some repo_full_name -> (
        let org = extract_org payload repo_full_name in
        let installation_id =
          extract_installation_id ?installation_id payload
        in
        let actor = extract_actor payload in
        let action = get_string "action" payload in
        let delivery_id = delivery_id in
        let received_at = received_at in
        match event with
        | "pull_request" ->
            normalize_pull_request ~delivery_id ~installation_id ~received_at
              ~event ~action ~payload ~repo_full_name ~org ~actor
        | "issues" ->
            normalize_issues ~delivery_id ~installation_id ~received_at ~event
              ~action ~payload ~repo_full_name ~org ~actor
        | "issue_comment" ->
            normalize_issue_comment ~delivery_id ~installation_id ~received_at
              ~event ~action ~payload ~repo_full_name ~org ~actor
        | "pull_request_review" ->
            normalize_pull_request_review ~delivery_id ~installation_id
              ~received_at ~event ~action ~payload ~repo_full_name ~org ~actor
        | "pull_request_review_comment" ->
            normalize_pull_request_review_comment ~delivery_id ~installation_id
              ~received_at ~event ~action ~payload ~repo_full_name ~org ~actor
        | "check_run" ->
            normalize_ci ~delivery_id ~installation_id ~received_at ~event
              ~action ~payload ~repo_full_name ~org ~actor
              ~object_key:"check_run" ~allowed_actions:check_run_actions
        | "check_suite" ->
            normalize_ci ~delivery_id ~installation_id ~received_at ~event
              ~action ~payload ~repo_full_name ~org ~actor
              ~object_key:"check_suite" ~allowed_actions:check_suite_actions
        | "workflow_run" ->
            normalize_ci ~delivery_id ~installation_id ~received_at ~event
              ~action ~payload ~repo_full_name ~org ~actor
              ~object_key:"workflow_run" ~allowed_actions:workflow_run_actions
        | other ->
            unsupported ~event ~action
              (Printf.sprintf "unsupported event %S for item envelopes" other))

(* --- Safe JSON decode (inverse of to_safe_json) --- *)

let item_kind_of_string = function
  | "pull_request" -> Some Pull_request
  | "issue" -> Some Issue
  | _ -> None

let family_of_string s =
  match s with
  | "lifecycle" -> Lifecycle
  | "review" -> Review
  | "comment" -> Comment
  | "commit" -> Commit
  | "ci" -> Ci
  | "state_update" -> State_update
  | s ->
      let prefix = "other:" in
      let plen = String.length prefix in
      if String.length s >= plen && String.sub s 0 plen = prefix then
        Other (String.sub s plen (String.length s - plen))
      else Other s

let string_list_of_json_field = function
  | `List items ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

let safe_state_of_json = function
  | `Assoc _ as j ->
      {
        title = get_string "title" j;
        state = get_string "state" j;
        draft = get_bool "draft" j;
        merged = get_bool "merged" j;
        labels = string_list_of_json_field (Yojson.Safe.Util.member "labels" j);
        assignees =
          string_list_of_json_field (Yojson.Safe.Util.member "assignees" j);
        milestone = get_string "milestone" j;
        head_sha = get_string "head_sha" j;
        base_ref = get_string "base_ref" j;
        head_ref = get_string "head_ref" j;
      }
  | _ -> empty_safe_state

let actor_of_json = function
  | `Assoc _ as j ->
      {
        login = get_string "login" j;
        id = get_int "id" j;
        type_ = get_string "type" j;
      }
  | _ -> empty_actor

let transfer_of_json = function
  | `Assoc _ as j ->
      Some
        {
          from_repo = get_string "from_repo" j;
          to_repo = get_string "to_repo" j;
        }
  | _ -> None

let of_safe_json json : (t, string) result =
  match json with
  | `Assoc _ as j -> (
      match (get_string "event" j, get_string "repo_full_name" j) with
      | None, _ -> Result.Error "of_safe_json: missing event"
      | _, None -> Result.Error "of_safe_json: missing repo_full_name"
      | Some event, Some repo_full_name ->
          let family =
            match get_string "family" j with
            | Some s -> family_of_string s
            | None -> Other "unknown"
          in
          let item_kind =
            match get_string "item_kind" j with
            | Some s -> item_kind_of_string s
            | None -> None
          in
          let version =
            match get_int "version" j with
            | Some n -> n
            | None -> envelope_version
          in
          let actor =
            match member_opt "actor" j with
            | Some a -> actor_of_json a
            | None -> empty_actor
          in
          let before =
            match member_opt "before" j with
            | Some b -> Some (safe_state_of_json b)
            | None -> None
          in
          let after =
            match member_opt "after" j with
            | Some a -> Some (safe_state_of_json a)
            | None -> None
          in
          let transfer =
            match member_opt "transfer" j with
            | Some t -> transfer_of_json t
            | None -> None
          in
          let unsupported =
            match get_bool "unsupported" j with Some b -> b | None -> false
          in
          Result.Ok
            {
              version;
              delivery_id = get_string "delivery_id" j;
              installation_id = get_int "installation_id" j;
              event;
              action = get_string "action" j;
              repo_full_name;
              org = get_string "org" j;
              item_kind;
              item_number = get_int "item_number" j;
              item_node_id = get_string "item_node_id" j;
              item_url = get_string "item_url" j;
              html_url = get_string "html_url" j;
              family;
              actor;
              item_author = get_string "item_author" j;
              before;
              after;
              transfer;
              received_at = get_string "received_at" j;
              event_at = get_string "event_at" j;
              head_sha = get_string "head_sha" j;
              unsupported;
              skip_reason = get_string "skip_reason" j;
            })
  | _ -> Result.Error "of_safe_json: expected JSON object"

let envelope_of_json = of_safe_json
