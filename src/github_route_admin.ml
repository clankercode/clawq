(* Agent/CLI GitHub route plan/inspect/change/disable/remove admin API.
   See github_route_admin.mli and docs/plans/2026-07-12-github-item-room-routing.md. *)

type inspect_view = {
  route : Github_route_store.t;
  summary : string;
  explain : string list;
}

(* ── JSON helpers (secret-free; mirror store encoding) ─────────── *)

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let string_list_to_json xs = `List (List.map (fun s -> `String s) xs)

let string_list_of_json = function
  | `List items ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | `String s :: rest -> loop (s :: acc) rest
        | _ -> Error "expected string list"
      in
      loop [] items
  | `Null -> Ok []
  | _ -> Error "expected string list or null"

let comment_mode_to_string = function
  | Github_route_store.Off -> "off"
  | Summary -> "summary"
  | Threaded -> "threaded"

let comment_mode_of_string = function
  | "off" -> Ok Github_route_store.Off
  | "summary" -> Ok Summary
  | "threaded" -> Ok Threaded
  | s -> Error (Printf.sprintf "unknown comment_mode: %s" s)

let item_kind_to_string = function `Pull_request -> "pr" | `Issue -> "issue"

let item_kind_of_string = function
  | "pr" | "pull_request" | "Pull_request" -> Ok `Pull_request
  | "issue" | "Issue" -> Ok `Issue
  | s -> Error (Printf.sprintf "unknown item kind: %s" s)

let destination_to_json (d : Github_route_store.destination) : Yojson.Safe.t =
  match d with
  | Room id -> `Assoc [ ("type", `String "room"); ("id", `String id) ]
  | Session key -> `Assoc [ ("type", `String "session"); ("id", `String key) ]

let destination_of_json (j : Yojson.Safe.t) :
    (Github_route_store.destination, string) result =
  let open Yojson.Safe.Util in
  match j with
  | `String s ->
      let s = String.trim s in
      if String.length s > 5 && String.sub s 0 5 = "room:" then
        Ok (Github_route_store.Room (String.sub s 5 (String.length s - 5)))
      else if String.length s > 8 && String.sub s 0 8 = "session:" then
        Ok (Github_route_store.Session (String.sub s 8 (String.length s - 8)))
      else Error (Printf.sprintf "invalid destination key: %s" s)
  | `Assoc _ -> (
      match (member "type" j, member "id" j) with
      | `String "room", `String id when String.trim id <> "" ->
          Ok (Github_route_store.Room id)
      | `String "session", `String id when String.trim id <> "" ->
          Ok (Github_route_store.Session id)
      | `String "room", _ -> Error "room destination missing id"
      | `String "session", _ -> Error "session destination missing id"
      | `String t, _ -> Error (Printf.sprintf "unknown destination type: %s" t)
      | _ -> Error "destination.type missing")
  | _ -> Error "destination must be string or object"

let selector_to_json (s : Github_route_store.selector) : Yojson.Safe.t =
  match s with
  | Item { repo_full_name; kind; number } ->
      `Assoc
        [
          ("type", `String "item");
          ("repo_full_name", `String repo_full_name);
          ("kind", `String (item_kind_to_string kind));
          ("number", `Int number);
        ]
  | Repo repo -> `Assoc [ ("type", `String "repo"); ("repo", `String repo) ]
  | Org org -> `Assoc [ ("type", `String "org"); ("org", `String org) ]

let selector_of_json (j : Yojson.Safe.t) :
    (Github_route_store.selector, string) result =
  let open Yojson.Safe.Util in
  match j with
  | `String s ->
      (* Compact key forms: repo:owner/repo | org:name | item:owner/repo:pr:N *)
      let s = String.trim s in
      if String.length s > 5 && String.sub s 0 5 = "repo:" then
        Ok (Github_route_store.Repo (String.sub s 5 (String.length s - 5)))
      else if String.length s > 4 && String.sub s 0 4 = "org:" then
        Ok (Github_route_store.Org (String.sub s 4 (String.length s - 4)))
      else if String.length s > 5 && String.sub s 0 5 = "item:" then
        let rest = String.sub s 5 (String.length s - 5) in
        match String.split_on_char ':' rest with
        | [ repo; kind; num ] -> (
            match (item_kind_of_string kind, int_of_string_opt num) with
            | Ok kind, Some number when number > 0 ->
                Ok
                  (Github_route_store.Item
                     { repo_full_name = repo; kind; number })
            | Error e, _ -> Error e
            | _, _ -> Error "item selector number must be positive")
        | _ -> Error (Printf.sprintf "invalid item selector key: %s" s)
      else Error (Printf.sprintf "invalid selector key: %s" s)
  | `Assoc _ -> (
      match member "type" j with
      | `String "item" -> (
          let repo =
            match member "repo_full_name" j with `String s -> s | _ -> ""
          in
          let number =
            match member "number" j with
            | `Int n -> n
            | `Intlit s -> ( try int_of_string s with _ -> 0)
            | _ -> 0
          in
          let kind_s = match member "kind" j with `String s -> s | _ -> "" in
          match item_kind_of_string kind_s with
          | Error e -> Error e
          | Ok kind ->
              if String.trim repo = "" then Error "item selector missing repo"
              else if number <= 0 then
                Error "item selector number must be positive"
              else
                Ok
                  (Github_route_store.Item
                     { repo_full_name = repo; kind; number }))
      | `String "repo" -> (
          match member "repo" j with
          | `String s when String.trim s <> "" -> Ok (Github_route_store.Repo s)
          | _ -> Error "repo selector missing repo")
      | `String "org" -> (
          match member "org" j with
          | `String s when String.trim s <> "" -> Ok (Github_route_store.Org s)
          | _ -> Error "org selector missing org")
      | `String t -> Error (Printf.sprintf "unknown selector type: %s" t)
      | _ -> Error "selector.type missing")
  | _ -> Error "selector must be string or object"

let filter_to_json (f : Github_route_store.event_filter) : Yojson.Safe.t =
  Github_route_filter.to_json f

let filter_of_json (j : Yojson.Safe.t) :
    (Github_route_store.event_filter, string) result =
  Github_route_filter.of_json j

let capability_to_json (c : Github_route_store.capability_policy) :
    Yojson.Safe.t =
  let extra =
    `Assoc (sort_assoc (List.map (fun (k, v) -> (k, `Bool v)) c.extra))
  in
  `Assoc
    [
      ("allow_reply", `Bool c.allow_reply);
      ("allow_label", `Bool c.allow_label);
      ("allow_assign", `Bool c.allow_assign);
      ("allow_review", `Bool c.allow_review);
      ("allow_merge", `Bool c.allow_merge);
      ("allow_close", `Bool c.allow_close);
      ("extra", extra);
    ]

let capability_of_json (j : Yojson.Safe.t) :
    (Github_route_store.capability_policy, string) result =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> Ok Github_route_store.default_capability_policy
  | `Assoc _ ->
      let get_bool key = match member key j with `Bool b -> b | _ -> false in
      let extra =
        match member "extra" j with
        | `Assoc fields ->
            List.filter_map
              (fun (k, v) -> match v with `Bool b -> Some (k, b) | _ -> None)
              fields
        | _ -> []
      in
      Ok
        {
          Github_route_store.allow_reply = get_bool "allow_reply";
          allow_label = get_bool "allow_label";
          allow_assign = get_bool "allow_assign";
          allow_review = get_bool "allow_review";
          allow_merge = get_bool "allow_merge";
          allow_close = get_bool "allow_close";
          extra;
        }
  | _ -> Error "capability_policy must be object or null"

let route_public_json (r : Github_route_store.t) : Yojson.Safe.t =
  let opt_str k = function None -> [] | Some v -> [ (k, `String v) ] in
  `Assoc
    (sort_assoc
       ([
          ("id", `String r.id);
          ("destination", destination_to_json r.destination);
          ( "destination_key",
            `String (Github_route_store.destination_key r.destination) );
          ("selector", selector_to_json r.selector);
          ( "selector_key",
            `String (Github_route_store.canonical_selector_key r.selector) );
          ("filter", filter_to_json r.filter);
          ("comment_mode", `String (comment_mode_to_string r.comment_mode));
          ("capability_policy", capability_to_json r.capability_policy);
          ("enabled", `Bool r.enabled);
          ("revision", `String r.revision);
        ]
       @ opt_str "managed_bundle_id" r.managed_bundle_id
       @ opt_str "managed_feature_id" r.managed_feature_id
       @ [
           ("created_at", `String r.created_at);
           ("updated_at", `String r.updated_at);
         ]))

let planned_route_json ~destination ~selector ~filter ~comment_mode
    ~capability_policy ~enabled ?route_id ?managed_bundle_id ?managed_feature_id
    () : Yojson.Safe.t =
  let opt_str k = function None -> [] | Some v -> [ (k, `String v) ] in
  `Assoc
    (sort_assoc
       (opt_str "id" route_id
       @ [
           ("destination", destination_to_json destination);
           ( "destination_key",
             `String (Github_route_store.destination_key destination) );
           ("selector", selector_to_json selector);
           ( "selector_key",
             `String (Github_route_store.canonical_selector_key selector) );
           ("filter", filter_to_json filter);
           ("comment_mode", `String (comment_mode_to_string comment_mode));
           ("capability_policy", capability_to_json capability_policy);
           ("enabled", `Bool enabled);
         ]
       @ opt_str "managed_bundle_id" managed_bundle_id
       @ opt_str "managed_feature_id" managed_feature_id))

let context_of_destination (d : Github_route_store.destination) :
    Setup_plan.context =
  match d with
  | Room room_id ->
      {
        room_id = Some room_id;
        session_key = None;
        connector = None;
        profile_id = None;
        extra = [];
      }
  | Session session_key ->
      {
        room_id = None;
        session_key = Some session_key;
        connector = None;
        profile_id = None;
        extra = [];
      }

let destination_matches_context (d : Github_route_store.destination)
    (ctx : Setup_plan.context) =
  match d with
  | Room id -> (
      match ctx.room_id with Some rid -> String.equal rid id | None -> false)
  | Session key -> (
      match ctx.session_key with
      | Some sk -> String.equal sk key
      | None -> false)

let specificity_label = function
  | Github_route_store.Item _ -> "item"
  | Repo _ -> "repo"
  | Org _ -> "org"

let caps_summary (c : Github_route_store.capability_policy) =
  let flags =
    [
      ("reply", c.allow_reply);
      ("label", c.allow_label);
      ("assign", c.allow_assign);
      ("review", c.allow_review);
      ("merge", c.allow_merge);
      ("close", c.allow_close);
    ]
  in
  let on = List.filter_map (fun (n, b) -> if b then Some n else None) flags in
  match on with [] -> "none (read/forward only)" | xs -> String.concat "," xs

let filter_summary (f : Github_route_store.event_filter) =
  let parts = ref [] in
  if f.include_events <> [] then
    parts :=
      ("include_events=[" ^ String.concat "," f.include_events ^ "]") :: !parts;
  if f.exclude_events <> [] then
    parts :=
      ("exclude_events=[" ^ String.concat "," f.exclude_events ^ "]") :: !parts;
  if f.include_repos <> [] then
    parts :=
      ("include_repos=[" ^ String.concat "," f.include_repos ^ "]") :: !parts;
  if f.exclude_repos <> [] then
    parts :=
      ("exclude_repos=[" ^ String.concat "," f.exclude_repos ^ "]") :: !parts;
  if Github_route_filter.has_advanced f then
    parts :=
      ("schema_version=" ^ string_of_int f.schema_version ^ " advanced")
      :: !parts;
  match List.rev !parts with
  | [] -> "baseline (all non-excluded events)"
  | xs -> String.concat "; " xs

let format_route_summary (r : Github_route_store.t) =
  Printf.sprintf
    "route %s %s → %s selector=%s comment=%s caps=%s enabled=%b rev=%s" r.id
    (if r.enabled then "active" else "disabled")
    (Github_route_store.destination_key r.destination)
    (Github_route_store.canonical_selector_key r.selector)
    (comment_mode_to_string r.comment_mode)
    (caps_summary r.capability_policy)
    r.enabled r.revision

let explain_route (r : Github_route_store.t) : string list =
  let spec = specificity_label r.selector in
  let fallthrough =
    Printf.sprintf
      "No fallthrough: most-specific configured selector class wins \
       (item>repo>org) before enabled/filter; a disabled or filtered %s route \
       mutes broader routes for matching events."
      spec
  in
  let filter_line =
    Printf.sprintf "Forwarding filter: %s" (filter_summary r.filter)
  in
  let comment_line =
    Printf.sprintf "Comment mode: %s" (comment_mode_to_string r.comment_mode)
  in
  let caps_line =
    Printf.sprintf "Capabilities: %s (mutation still requires authz gate)"
      (caps_summary r.capability_policy)
  in
  let managed =
    match (r.managed_bundle_id, r.managed_feature_id) with
    | None, None -> "Managed access: none"
    | bundle, feature ->
        Printf.sprintf "Managed access: bundle=%s feature=%s"
          (Option.value bundle ~default:"-")
          (Option.value feature ~default:"-")
  in
  let state =
    if r.enabled then "Lifecycle: enabled (active slot holder)"
    else "Lifecycle: disabled (slot free for destination+selector)"
  in
  [ fallthrough; filter_line; comment_line; caps_line; managed; state ]

let build_inspect (r : Github_route_store.t) : inspect_view =
  { route = r; summary = format_route_summary r; explain = explain_route r }

let inspect ~db ~id =
  match Github_route_store.get ~db ~id with
  | Error e -> Error e
  | Ok None -> Error (Printf.sprintf "route not found: %s" id)
  | Ok (Some r) -> Ok (build_inspect r)

let list_inspect_for_destination ~db ~destination =
  match Github_route_store.list_for_destination ~db ~destination with
  | Error e -> Error e
  | Ok routes -> Ok (List.map build_inspect routes)

let preview_filter ~db ~destination ~envelope ?enrichment () =
  Github_route_filter_preview.preview ~db ~destination ~envelope ?enrichment ()

(* ── Plan builders ─────────────────────────────────────────────── *)

let ensure_plan_schema db = Setup_plan_apply.init_schema db

let store_pending ~db (plan : Setup_plan.t) =
  ensure_plan_schema db;
  match Setup_plan_apply.store_plan ~db plan with
  | Ok () -> Ok plan
  | Error e -> Error e

let collision_on_collision = function
  | `Reject -> "reject"
  | `Replace -> "replace"

let on_collision_of_string = function
  | "replace" -> Ok `Replace
  | "reject" | "" -> Ok `Reject
  | s -> Error (Printf.sprintf "unknown on_collision: %s" s)

let readiness_item ~name ~status ~message : Setup_plan.readiness_item =
  { name; status; message }

let create_readiness ~destination ~selector ~enabled ~collision :
    Setup_plan.readiness_item list =
  let dest_ok =
    match destination with
    | Github_route_store.Room id when String.trim id <> "" -> true
    | Session key when String.trim key <> "" -> true
    | _ -> false
  in
  let dest_item =
    readiness_item ~name:"destination"
      ~status:(if dest_ok then Setup_plan.Pass else Fail)
      ~message:
        (if dest_ok then Github_route_store.destination_key destination
         else "destination id empty")
  in
  let sel_item =
    readiness_item ~name:"selector" ~status:Setup_plan.Pass
      ~message:(Github_route_store.canonical_selector_key selector)
  in
  let collision_item =
    match collision with
    | None ->
        readiness_item ~name:"active_slot" ~status:Setup_plan.Pass
          ~message:"no active route for destination+selector"
    | Some existing_id when enabled ->
        readiness_item ~name:"active_slot" ~status:Setup_plan.Warn
          ~message:
            (Printf.sprintf
               "active route %s already holds slot; create may fail unless \
                on_collision=replace"
               existing_id)
    | Some existing_id ->
        readiness_item ~name:"active_slot" ~status:Setup_plan.Pass
          ~message:
            (Printf.sprintf
               "active route %s exists; planned route disabled so no collision"
               existing_id)
  in
  let no_fallthrough =
    readiness_item ~name:"no_fallthrough" ~status:Setup_plan.Pass
      ~message:
        "item>repo>org specificity; disabled/filtered narrow routes mute \
         broader"
  in
  [ dest_item; sel_item; collision_item; no_fallthrough ]

let plan_create ~db ~principal ~destination ~selector
    ?(filter = Github_route_store.default_filter)
    ?(comment_mode = Github_route_store.default_comment_mode)
    ?(capability_policy = Github_route_store.default_capability_policy)
    ?(enabled = true) ?route_id ?managed_bundle_id ?managed_feature_id
    ?(on_collision = `Reject) ~base_revision ?(now = Unix.gettimeofday ()) () =
  Github_route_store.ensure_schema db;
  let collision =
    match Github_route_store.find_active ~db ~destination ~selector with
    | Error e -> Error e
    | Ok None -> Ok None
    | Ok (Some r) -> Ok (Some r.id)
  in
  match collision with
  | Error e -> Error e
  | Ok collision_id ->
      let current_state =
        match Github_route_store.find_active ~db ~destination ~selector with
        | Ok (Some r) -> route_public_json r
        | _ -> `Null
      in
      let planned_state =
        planned_route_json ~destination ~selector ~filter ~comment_mode
          ~capability_policy ~enabled ?route_id ?managed_bundle_id
          ?managed_feature_id ()
      in
      let path =
        Printf.sprintf "github_routes/%s/%s"
          (Github_route_store.destination_key destination)
          (Github_route_store.canonical_selector_key selector)
      in
      let diff =
        [
          Setup_plan.Create { path; value = planned_state };
          Setup_plan.Note
            {
              path;
              message =
                "No fallthrough: more-specific disabled/filtered routes mute \
                 broader Org/Repo feeds.";
            };
        ]
      in
      let readiness =
        create_readiness ~destination ~selector ~enabled ~collision:collision_id
      in
      let warnings =
        match collision_id with
        | Some id when enabled && on_collision = `Reject ->
            [
              {
                Setup_plan.code = "route_slot_collision";
                message =
                  Printf.sprintf
                    "active route %s already occupies destination+selector" id;
              };
            ]
        | _ -> []
      in
      let op_fields =
        [
          ("op", `String "create");
          ("destination", destination_to_json destination);
          ("selector", selector_to_json selector);
          ("filter", filter_to_json filter);
          ("comment_mode", `String (comment_mode_to_string comment_mode));
          ("capability_policy", capability_to_json capability_policy);
          ("enabled", `Bool enabled);
          ("on_collision", `String (collision_on_collision on_collision));
        ]
        @ (match route_id with None -> [] | Some id -> [ ("id", `String id) ])
        @ (match managed_bundle_id with
          | None -> []
          | Some id -> [ ("managed_bundle_id", `String id) ])
        @
        match managed_feature_id with
        | None -> []
        | Some id -> [ ("managed_feature_id", `String id) ]
      in
      let ops = `List [ `Assoc (sort_assoc op_fields) ] in
      let data =
        `Assoc
          (sort_assoc
             [
               ("base_revision", `String base_revision);
               ( "destination_key",
                 `String (Github_route_store.destination_key destination) );
               ( "selector_key",
                 `String (Github_route_store.canonical_selector_key selector) );
             ])
      in
      let ctx = context_of_destination destination in
      let plan =
        Setup_plan.make ~principal ~source:ctx ~destination:ctx ~current_state
          ~planned_state ~diff ~readiness ~warnings ~base_revision
          ~apply_payload:{ kind = Setup_plan.Github_route; ops; data }
          ~now ()
      in
      store_pending ~db plan

let load_route ~db ~id =
  match Github_route_store.get ~db ~id with
  | Error e -> Error e
  | Ok None -> Error (Printf.sprintf "route not found: %s" id)
  | Ok (Some r) -> Ok r

let plan_update ~db ~principal ~id ?filter ?comment_mode ?capability_policy
    ?enabled ?expected_revision ~base_revision ?(now = Unix.gettimeofday ()) ()
    =
  Github_route_store.ensure_schema db;
  match load_route ~db ~id with
  | Error e -> Error e
  | Ok cur ->
      let next_filter = match filter with Some f -> f | None -> cur.filter in
      let next_mode =
        match comment_mode with Some m -> m | None -> cur.comment_mode
      in
      let next_caps =
        match capability_policy with
        | Some c -> c
        | None -> cur.capability_policy
      in
      let next_enabled =
        match enabled with Some e -> e | None -> cur.enabled
      in
      let expected =
        match expected_revision with Some r -> r | None -> cur.revision
      in
      let planned =
        planned_route_json ~destination:cur.destination ~selector:cur.selector
          ~filter:next_filter ~comment_mode:next_mode
          ~capability_policy:next_caps ~enabled:next_enabled ~route_id:cur.id
          ?managed_bundle_id:cur.managed_bundle_id
          ?managed_feature_id:cur.managed_feature_id ()
      in
      let path = Printf.sprintf "github_routes/id/%s" cur.id in
      let diff =
        [
          Setup_plan.Update
            { path; from_ = route_public_json cur; to_ = planned };
        ]
      in
      let readiness =
        [
          readiness_item ~name:"route_exists" ~status:Setup_plan.Pass
            ~message:cur.id;
          readiness_item ~name:"expected_revision" ~status:Setup_plan.Pass
            ~message:expected;
          readiness_item ~name:"no_fallthrough" ~status:Setup_plan.Pass
            ~message:
              "disabled/filtered most-specific routes continue to mute broader";
        ]
      in
      let op_fields =
        [
          ("op", `String "update");
          ("id", `String cur.id);
          ("expected_revision", `String expected);
        ]
        @ (match filter with
          | None -> []
          | Some f -> [ ("filter", filter_to_json f) ])
        @ (match comment_mode with
          | None -> []
          | Some m -> [ ("comment_mode", `String (comment_mode_to_string m)) ])
        @ (match capability_policy with
          | None -> []
          | Some c -> [ ("capability_policy", capability_to_json c) ])
        @ match enabled with None -> [] | Some e -> [ ("enabled", `Bool e) ]
      in
      let ops = `List [ `Assoc (sort_assoc op_fields) ] in
      let data =
        `Assoc
          [
            ("base_revision", `String base_revision);
            ("route_id", `String cur.id);
          ]
      in
      let ctx = context_of_destination cur.destination in
      let plan =
        Setup_plan.make ~principal ~source:ctx ~destination:ctx
          ~current_state:(route_public_json cur) ~planned_state:planned ~diff
          ~readiness ~warnings:[] ~base_revision
          ~apply_payload:{ kind = Setup_plan.Github_route; ops; data }
          ~now ()
      in
      store_pending ~db plan

let plan_remove ~db ~principal ~id ?expected_revision ~base_revision
    ?(now = Unix.gettimeofday ()) () =
  Github_route_store.ensure_schema db;
  match load_route ~db ~id with
  | Error e -> Error e
  | Ok cur ->
      let expected =
        match expected_revision with Some r -> r | None -> cur.revision
      in
      let planned =
        planned_route_json ~destination:cur.destination ~selector:cur.selector
          ~filter:cur.filter ~comment_mode:cur.comment_mode
          ~capability_policy:cur.capability_policy ~enabled:false
          ~route_id:cur.id ?managed_bundle_id:cur.managed_bundle_id
          ?managed_feature_id:cur.managed_feature_id ()
      in
      let path = Printf.sprintf "github_routes/id/%s" cur.id in
      let diff =
        [
          Setup_plan.Delete { path; old = route_public_json cur };
          Setup_plan.Note
            {
              path;
              message =
                "Soft remove: enabled=false frees active destination+selector \
                 slot; row retained for audit.";
            };
        ]
      in
      let readiness =
        [
          readiness_item ~name:"route_exists" ~status:Setup_plan.Pass
            ~message:cur.id;
          readiness_item ~name:"soft_remove" ~status:Setup_plan.Pass
            ~message:"enabled=false (hard delete not used)";
        ]
      in
      let ops =
        `List
          [
            `Assoc
              (sort_assoc
                 [
                   ("op", `String "remove");
                   ("id", `String cur.id);
                   ("expected_revision", `String expected);
                 ]);
          ]
      in
      let data =
        `Assoc
          [
            ("base_revision", `String base_revision);
            ("route_id", `String cur.id);
            ("mode", `String "soft");
          ]
      in
      let ctx = context_of_destination cur.destination in
      let plan =
        Setup_plan.make ~principal ~source:ctx ~destination:ctx
          ~current_state:(route_public_json cur) ~planned_state:planned ~diff
          ~readiness ~warnings:[] ~base_revision
          ~apply_payload:{ kind = Setup_plan.Github_route; ops; data }
          ~now ()
      in
      store_pending ~db plan

(* plan_disable should use op "disable" for clarity in apply payloads. *)
let plan_disable ~db ~principal ~id ?expected_revision ~base_revision
    ?(now = Unix.gettimeofday ()) () =
  Github_route_store.ensure_schema db;
  match load_route ~db ~id with
  | Error e -> Error e
  | Ok cur ->
      let expected =
        match expected_revision with Some r -> r | None -> cur.revision
      in
      let planned =
        planned_route_json ~destination:cur.destination ~selector:cur.selector
          ~filter:cur.filter ~comment_mode:cur.comment_mode
          ~capability_policy:cur.capability_policy ~enabled:false
          ~route_id:cur.id ?managed_bundle_id:cur.managed_bundle_id
          ?managed_feature_id:cur.managed_feature_id ()
      in
      let path = Printf.sprintf "github_routes/id/%s" cur.id in
      let diff =
        [
          Setup_plan.Update
            { path; from_ = route_public_json cur; to_ = planned };
          Setup_plan.Note
            {
              path;
              message =
                "Disable frees active slot; no-fallthrough mute behavior \
                 retained for the disabled row.";
            };
        ]
      in
      let readiness =
        [
          readiness_item ~name:"route_exists" ~status:Setup_plan.Pass
            ~message:cur.id;
          readiness_item ~name:"disable" ~status:Setup_plan.Pass
            ~message:"enabled=false";
        ]
      in
      let ops =
        `List
          [
            `Assoc
              (sort_assoc
                 [
                   ("op", `String "disable");
                   ("id", `String cur.id);
                   ("expected_revision", `String expected);
                 ]);
          ]
      in
      let data =
        `Assoc
          [
            ("base_revision", `String base_revision);
            ("route_id", `String cur.id);
          ]
      in
      let ctx = context_of_destination cur.destination in
      let plan =
        Setup_plan.make ~principal ~source:ctx ~destination:ctx
          ~current_state:(route_public_json cur) ~planned_state:planned ~diff
          ~readiness ~warnings:[] ~base_revision
          ~apply_payload:{ kind = Setup_plan.Github_route; ops; data }
          ~now ()
      in
      store_pending ~db plan

let list_plans_for_destination ~db ~destination ?(status = "pending") () =
  ensure_plan_schema db;
  let sql =
    {|SELECT plan_json FROM setup_plans WHERE status = ? ORDER BY created_at ASC|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT status));
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.TEXT json -> (
                match
                  Yojson.Safe.from_string json |> Setup_plan.of_persist_json
                with
                | Ok plan
                  when plan.apply_payload.kind = Setup_plan.Github_route
                       && destination_matches_context destination
                            plan.destination ->
                    loop (plan :: acc)
                | _ -> loop acc)
            | _ -> loop acc)
        | _ -> List.rev acc
      in
      loop [])

(* ── Apply adapter ─────────────────────────────────────────────── *)

let member_opt key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let string_member key j =
  match member_opt key j with Some (`String s) -> Some s | _ -> None

let bool_member key j =
  match member_opt key j with Some (`Bool b) -> Some b | _ -> None

let apply_create ~db ~op ~plan_id ~receipt_id =
  let open Yojson.Safe.Util in
  match
    ( destination_of_json (member "destination" op),
      selector_of_json (member "selector" op) )
  with
  | Error e, _ | _, Error e -> Error e
  | Ok destination, Ok selector -> (
      let filter =
        match member_opt "filter" op with
        | None | Some `Null -> Ok Github_route_store.default_filter
        | Some j -> filter_of_json j
      in
      let comment_mode =
        match string_member "comment_mode" op with
        | None -> Ok Github_route_store.default_comment_mode
        | Some s -> comment_mode_of_string s
      in
      let capability_policy =
        match member_opt "capability_policy" op with
        | None | Some `Null -> Ok Github_route_store.default_capability_policy
        | Some j -> capability_of_json j
      in
      let enabled =
        match bool_member "enabled" op with Some b -> b | None -> true
      in
      let on_collision =
        match string_member "on_collision" op with
        | None -> Ok `Reject
        | Some s -> on_collision_of_string s
      in
      let route_id = string_member "id" op in
      let managed_bundle_id = string_member "managed_bundle_id" op in
      let managed_feature_id = string_member "managed_feature_id" op in
      match (filter, comment_mode, capability_policy, on_collision) with
      | Error e, _, _, _
      | _, Error e, _, _
      | _, _, Error e, _
      | _, _, _, Error e ->
          Error e
      | Ok filter, Ok comment_mode, Ok capability_policy, Ok on_collision -> (
          (* Idempotent: if id already exists, treat as success. *)
          let already =
            match route_id with
            | None -> Ok None
            | Some id -> Github_route_store.get ~db ~id
          in
          match already with
          | Error e -> Error e
          | Ok (Some _) -> Ok ()
          | Ok None -> (
              let provenance : Github_route_store.provenance =
                {
                  created_by = None;
                  created_via = Some "setup_plan";
                  setup_plan_id = Some plan_id;
                  notes = Some (Printf.sprintf "receipt=%s" receipt_id);
                }
              in
              match
                Github_route_store.create ~db ?id:route_id ~destination
                  ~selector ~filter ~comment_mode ~capability_policy ~enabled
                  ?managed_bundle_id ?managed_feature_id ~provenance
                  ~on_collision ()
              with
              | Ok _ -> Ok ()
              | Error e -> Error e)))

let apply_update_like ~db ~op ~enabled_override =
  match string_member "id" op with
  | None -> Error "update/disable/remove op missing id"
  | Some id -> (
      match Github_route_store.get ~db ~id with
      | Error e -> Error e
      | Ok None -> Error (Printf.sprintf "route not found: %s" id)
      | Ok (Some cur) -> (
          let expected = string_member "expected_revision" op in
          (* Soft-remove/disable already applied → idempotent success. *)
          match (enabled_override, cur.enabled) with
          | Some false, false -> Ok ()
          | _ -> (
              let parse_filter =
                match member_opt "filter" op with
                | None | Some `Null -> Ok None
                | Some j -> (
                    match filter_of_json j with
                    | Ok f -> Ok (Some f)
                    | Error e -> Error e)
              in
              let parse_mode =
                match string_member "comment_mode" op with
                | None -> Ok None
                | Some s -> (
                    match comment_mode_of_string s with
                    | Ok m -> Ok (Some m)
                    | Error e -> Error e)
              in
              let parse_caps =
                match member_opt "capability_policy" op with
                | None | Some `Null -> Ok None
                | Some j -> (
                    match capability_of_json j with
                    | Ok c -> Ok (Some c)
                    | Error e -> Error e)
              in
              match (parse_filter, parse_mode, parse_caps) with
              | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
              | Ok filter, Ok comment_mode, Ok capability_policy -> (
                  let enabled =
                    match enabled_override with
                    | Some e -> Some e
                    | None -> bool_member "enabled" op
                  in
                  match
                    Github_route_store.update ~db ~id
                      ?expected_revision:expected ?filter ?comment_mode
                      ?capability_policy ?enabled ()
                  with
                  | Ok _ -> Ok ()
                  | Error e -> Error e))))

let apply_one_op ~db ~plan_id ~receipt_id (op : Yojson.Safe.t) =
  match string_member "op" op with
  | Some "create" -> apply_create ~db ~op ~plan_id ~receipt_id
  | Some "update" -> apply_update_like ~db ~op ~enabled_override:None
  | Some "disable" -> apply_update_like ~db ~op ~enabled_override:(Some false)
  | Some "remove" -> apply_update_like ~db ~op ~enabled_override:(Some false)
  | Some other ->
      Error
        (Printf.sprintf
           "unknown github_route op %S; expected create|update|disable|remove"
           other)
  | None -> Error "github_route op missing \"op\" field"

let ops_list (ops : Yojson.Safe.t) : (Yojson.Safe.t list, string) result =
  match ops with
  | `List items -> Ok items
  | `Assoc _ as single -> Ok [ single ]
  | `Null -> Ok []
  | _ -> Error "apply_payload.ops must be a list or object"

let apply_route_ops ~db ~(plan : Setup_plan.t) ~receipt_id =
  match plan.apply_payload.kind with
  | Setup_plan.Github_route -> (
      Github_route_store.ensure_schema db;
      match ops_list plan.apply_payload.ops with
      | Error e -> Error e
      | Ok [] -> Error "github_route apply_payload.ops is empty"
      | Ok ops ->
          let rec loop = function
            | [] -> Ok ()
            | op :: rest -> (
                match apply_one_op ~db ~plan_id:plan.id ~receipt_id op with
                | Error e -> Error e
                | Ok () -> loop rest)
          in
          loop ops)
  | other ->
      let name =
        match other with
        | Setup_plan.Room_profile -> "room_profile"
        | Github_app_setup -> "github_app_setup"
        | Access_bundle -> "access_bundle"
        | Generic s -> s
        | Github_route -> "github_route"
      in
      Error
        (Printf.sprintf
           "apply_route_ops requires apply_payload.kind=github_route, got %s"
           name)
