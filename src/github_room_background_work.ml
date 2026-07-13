(* Room-anchored Claude-tag-equivalent background work (P19.M4.E2.T002).
   See github_room_background_work.mli and
   docs/plans/2026-07-12-github-item-room-routing.md. *)

open Github_route_store

type request = {
  room_id : string;
  item_key : string option;
  prompt : string;
  runner_pref : string option;
  thread_ref : string option;
  dedup_key : string;
}

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
}

let capability_key = "background_work"

let default_pilot_gate : pilot_gate =
  {
    enabled = false;
    pilot_name = "p19-room-background-work-pilot";
    expires_at = None;
  }

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let has_background_work_capability (c : capability_policy) =
  match List.assoc_opt capability_key c.extra with
  | Some true -> true
  | _ -> false

let pilot_expired ~(now : float) (pilot : pilot_gate) =
  match pilot.expires_at with
  | None -> false
  | Some exp when String.trim exp = "" -> false
  | Some exp ->
      let now_iso = Time_util.iso8601_utc ~t:now () in
      String.compare now_iso exp > 0

let pilot_unavailable_reason ~(pilot : pilot_gate) ~now =
  if not pilot.enabled then
    Printf.sprintf
      "room-anchored background work is not available outside the named \
       time-bounded pilot %S (P19 pilot gate off by default; not \
       production-ready). Production availability waits for P21 User_required \
       attribution. No App/PAT production fallback."
      pilot.pilot_name
  else if pilot_expired ~now pilot then
    let exp = match pilot.expires_at with Some e -> e | None -> "unknown" in
    Printf.sprintf
      "room-background-work pilot %S expired at %s; not available outside \
       pilot (not production-ready). Production waits for P21 User_required. \
       No App/PAT production fallback."
      pilot.pilot_name exp
  else
    "room-anchored background work is not available outside the named \
     time-bounded pilot (not production-ready)."

let authorize ~route ~pilot ?(now = Unix.gettimeofday ()) () =
  if (not pilot.enabled) || pilot_expired ~now pilot then
    Error (pilot_unavailable_reason ~pilot ~now)
  else
    match route with
    | None ->
        Error
          (Printf.sprintf
             "no route available to authorize background_work (capability \
              extra %S required; pilot path only)"
             capability_key)
    | Some (r : t) ->
        if not (has_background_work_capability r.capability_policy) then
          Error
            (Printf.sprintf
               "capability extra %S not granted by route %s policy for \
                background_work (defaults off; separate from code_change)"
               capability_key r.id)
        else Ok ()

let is_digit c = c >= '0' && c <= '9'

let parse_positive_int s =
  let s = String.trim s in
  if s = "" || not (String.for_all is_digit s) then None
  else
    try
      let n = int_of_string s in
      if n > 0 then Some n else None
    with _ -> None

(** Parse stored item keys used across journal/projection and looser item: forms
    used by other P19 action modules. *)
let parse_item_key key =
  let key = String.trim key in
  if key = "" then None
  else
    match String.split_on_char ':' key with
    | [ "pr"; repo; num ] when String.trim repo <> "" -> (
        match parse_positive_int num with
        | Some n -> Some (String.trim repo, n, true)
        | None -> None)
    | [ "issue"; repo; num ] when String.trim repo <> "" -> (
        match parse_positive_int num with
        | Some n -> Some (String.trim repo, n, false)
        | None -> None)
    | [ "item"; repo; "pr"; num ] when String.trim repo <> "" -> (
        match parse_positive_int num with
        | Some n -> Some (String.trim repo, n, true)
        | None -> None)
    | [ "item"; repo; "issue"; num ] when String.trim repo <> "" -> (
        match parse_positive_int num with
        | Some n -> Some (String.trim repo, n, false)
        | None -> None)
    | _ -> None

let validate_request (req : request) =
  let room_id = String.trim req.room_id in
  let prompt = String.trim req.prompt in
  let dedup = String.trim req.dedup_key in
  if room_id = "" then Error "background_work room_id must be non-empty"
  else if prompt = "" then Error "background_work prompt must be non-empty"
  else if dedup = "" then Error "background_work dedup_key must be non-empty"
  else
    match req.runner_pref with
    | Some r when String.trim r = "" ->
        Error "background_work runner_pref must be non-empty when provided"
    | _ -> Ok ()

let room_context ~room_id : Setup_plan.context =
  {
    room_id = Some room_id;
    session_key = None;
    connector = None;
    profile_id = None;
    extra = [];
  }

let store_pending ~db (plan : Setup_plan.t) =
  Setup_plan_apply.init_schema db;
  match Setup_plan_apply.store_plan ~db plan with
  | Ok () -> Ok plan
  | Error e -> Error e

let request_to_json ~(pilot : pilot_gate) ~(req : request) =
  `Assoc
    (sort_assoc
       ([
          ("kind", `String "room_background_work");
          ("room_id", `String (String.trim req.room_id));
          ("prompt", `String (String.trim req.prompt));
          ("dedup_key", `String (String.trim req.dedup_key));
          ("pilot_name", `String pilot.pilot_name);
          ("attribution", `String "App");
          ("pilot_only", `Bool true);
          ("production_ready", `Bool false);
          ("capability", `String capability_key);
          (* Code-changing / PR creation reuse T007; this capability is
             independent. *)
          ("code_change_family", `String "github_code_change_action");
          ("webhook_correlation", `String "room_background_work");
        ]
       @ (match req.item_key with
         | None -> []
         | Some k when String.trim k = "" -> []
         | Some k -> [ ("item_key", `String (String.trim k)) ])
       @ (match req.runner_pref with
         | None -> []
         | Some r when String.trim r = "" -> []
         | Some r -> [ ("runner_pref", `String (String.trim r)) ])
       @
       match req.thread_ref with
       | None -> []
       | Some t when String.trim t = "" -> []
       | Some t -> [ ("thread_ref", `String (String.trim t)) ]))

let maybe_attach_attribution ~db ~plan ~room_id ?session_id ?actor_key
    ?actor_snapshot ?attribution_allow ?expected_github_actor ?confirmation_id
    ?account_binding_id ?now () =
  let capture_snapshot () =
    match (actor_snapshot, actor_key) with
    | Some snap, _ -> Ok (Some snap)
    | None, Some key -> (
        match
          Github_durable_job_actor_attribution.capture_for_delayed_job ~db
            ~actor_key:key ~delayed_job_id:plan.Setup_plan.id
            ?account_binding_id ~room_id ?session_id ?now ~intent_id:plan.id ()
        with
        | Error e -> Error e
        | Ok snap -> Ok (Some snap))
    | None, None -> Ok None
  in
  match capture_snapshot () with
  | Error e -> Error e
  | Ok None -> (
      match attribution_allow with
      | Some _ ->
          Error
            "attribution_allow requires actor_snapshot or actor_key for \
             delayed pin"
      | None -> Ok plan)
  | Ok (Some snap) -> (
      match attribution_allow with
      | None ->
          Github_action_actor_attribution.attach_and_restamp ~db ~plan
            ~snapshot:snap ()
      | Some allow ->
          let ( let* ) = Result.bind in
          let* pin =
            Github_delayed_attribution.make_pin ~job_id:plan.id ~snapshot:snap
              ~allow ?expected_github_actor ?confirmation_id ()
          in
          Github_delayed_attribution.attach_and_restamp ~db ~plan ~pin ())

let plan_background ~db ~principal ~(req : request) ~base_revision ?route
    ?(pilot = default_pilot_gate) ?actor_key ?actor_snapshot ?attribution_allow
    ?expected_github_actor ?confirmation_id ?account_binding_id ?session_id
    ?(now = Unix.gettimeofday ()) () =
  match validate_request req with
  | Error e -> Error e
  | Ok () -> (
      match authorize ~route ~pilot ~now () with
      | Error e -> Error e
      | Ok () -> (
          let room_id = String.trim req.room_id in
          let prompt = String.trim req.prompt in
          let dedup = String.trim req.dedup_key in
          let action_json = request_to_json ~pilot ~req in
          let path =
            Printf.sprintf "github_room_background_work/%s/%s" room_id dedup
          in
          let current_state =
            `Assoc
              (sort_assoc
                 [
                   ("room_id", `String room_id);
                   ("dedup_key", `String dedup);
                   ("status", `String "pending_enqueue");
                   ("pilot_name", `String pilot.pilot_name);
                 ])
          in
          let planned_state =
            `Assoc
              (sort_assoc
                 ([
                    ("action", action_json);
                    ("capability", `String capability_key);
                    ("room_id", `String room_id);
                    ("dedup_key", `String dedup);
                    ("prompt", `String prompt);
                    ("pilot_name", `String pilot.pilot_name);
                    ("status", `String "planned");
                    ("attribution", `String "App");
                    ("production_ready", `Bool false);
                    ("pilot_only", `Bool true);
                    ("webhook_correlation", `String "room_background_work");
                    (* Receipts + work-item publication reconcile without
                       loops via work-item dedup / published_comment_id. *)
                    ("reconcile_without_loops", `Bool true);
                    ("work_item_semantics", `String "github_work_item");
                  ]
                 @ (match req.item_key with
                   | None -> []
                   | Some k when String.trim k = "" -> []
                   | Some k -> [ ("item_key", `String (String.trim k)) ])
                 @ (match req.runner_pref with
                   | None -> []
                   | Some r when String.trim r = "" -> []
                   | Some r -> [ ("runner_pref", `String (String.trim r)) ])
                 @ (match req.thread_ref with
                   | None -> []
                   | Some t when String.trim t = "" -> []
                   | Some t -> [ ("thread_ref", `String (String.trim t)) ])
                 @
                 match route with
                 | None -> []
                 | Some (r : t) ->
                     [
                       ("route_id", `String r.id);
                       ("route_revision", `String r.revision);
                     ]))
          in
          let runner_note =
            match req.runner_pref with
            | None -> "runner_pref=auto"
            | Some r -> Printf.sprintf "runner_pref=%s" (String.trim r)
          in
          let thread_note =
            match req.thread_ref with
            | None -> "no thread_ref (room-level)"
            | Some t -> Printf.sprintf "anchored thread_ref=%s" (String.trim t)
          in
          let diff =
            [
              Setup_plan.Create { path; value = action_json };
              Setup_plan.Note
                {
                  path;
                  message =
                    Printf.sprintf
                      "High-risk App-attributed room background work in room \
                       %s under pilot %S (dedup_key=%s; %s; %s). Reuses \
                       Github_work_item \
                       isolation/ack/cancel/retry/progress/blocked/completion. \
                       Code-changing and constrained PR creation use \
                       github_code_change_action (T007) separately. Confirm \
                       before enqueue/apply. Not production-ready (P21 \
                       User_required pending). No live runner dispatch at plan \
                       time. Receipts and webhooks reconcile without loops via \
                       dedup publication. Initiating Actor_snapshot (when \
                       pinned) is preserved through \
                       enqueue/retry/cancel/restart and re-resolved at \
                       execution."
                      room_id pilot.pilot_name dedup runner_note thread_note;
                };
            ]
          in
          let readiness =
            [
              {
                Setup_plan.name = "capability";
                status = Setup_plan.Pass;
                message = capability_key;
              };
              {
                name = "pilot";
                status = Setup_plan.Pass;
                message = pilot.pilot_name;
              };
              { name = "room_id"; status = Setup_plan.Pass; message = room_id };
              { name = "dedup_key"; status = Setup_plan.Pass; message = dedup };
              {
                name = "work_item_semantics";
                status = Setup_plan.Pass;
                message = "github_work_item";
              };
              {
                name = "code_change_separate";
                status = Setup_plan.Pass;
                message =
                  "code_change/PR creation reuse T007; capability independent";
              };
              {
                name = "not_production_ready";
                status = Setup_plan.Pass;
                message =
                  "P19 pilot only; production waits for P21 User_required";
              };
              {
                name = "no_live_mutation";
                status = Setup_plan.Pass;
                message =
                  "plan only; live runner enqueue requires confirm/apply";
              };
            ]
          in
          let op_fields =
            sort_assoc
              ([
                 ("op", `String "room_background_work");
                 ("room_id", `String room_id);
                 ("dedup_key", `String dedup);
                 ("prompt", `String prompt);
                 ("pilot_name", `String pilot.pilot_name);
                 ("capability", `String capability_key);
                 ("action", action_json);
               ]
              @ (match req.item_key with
                | None -> []
                | Some k when String.trim k = "" -> []
                | Some k -> [ ("item_key", `String (String.trim k)) ])
              @ (match req.runner_pref with
                | None -> []
                | Some r when String.trim r = "" -> []
                | Some r -> [ ("runner_pref", `String (String.trim r)) ])
              @ (match req.thread_ref with
                | None -> []
                | Some t when String.trim t = "" -> []
                | Some t -> [ ("thread_ref", `String (String.trim t)) ])
              @
              match route with
              | None -> []
              | Some (r : t) ->
                  [
                    ("route_id", `String r.id);
                    ("route_revision", `String r.revision);
                  ])
          in
          let ops = `List [ `Assoc op_fields ] in
          let data =
            `Assoc
              (sort_assoc
                 [
                   ("base_revision", `String base_revision);
                   ("room_id", `String room_id);
                   ("dedup_key", `String dedup);
                   ("pilot_name", `String pilot.pilot_name);
                   ("capability", `String capability_key);
                   ("production_ready", `Bool false);
                   ("webhook_correlation", `String "room_background_work");
                   ("work_item_semantics", `String "github_work_item");
                 ])
          in
          let ctx = room_context ~room_id in
          let plan =
            Setup_plan.make ~principal ~source:ctx ~destination:ctx
              ~current_state ~planned_state ~diff ~readiness ~warnings:[]
              ~base_revision
              ~apply_payload:
                {
                  kind = Setup_plan.Generic "github_room_background_work";
                  ops;
                  data;
                }
              ~now ()
          in
          match store_pending ~db plan with
          | Error e -> Error e
          | Ok plan ->
              maybe_attach_attribution ~db ~plan ~room_id ?session_id ?actor_key
                ?actor_snapshot ?attribution_allow ?expected_github_actor
                ?confirmation_id ?account_binding_id ~now ()))

let build_preamble ~(req : request) =
  let parts =
    [
      "## Room background work context";
      Printf.sprintf "room_id: %s" (String.trim req.room_id);
    ]
    @ (match req.item_key with
      | None -> []
      | Some k when String.trim k = "" -> []
      | Some k -> [ Printf.sprintf "item_key: %s" (String.trim k) ])
    @ (match req.thread_ref with
      | None -> []
      | Some t when String.trim t = "" -> []
      | Some t ->
          [
            Printf.sprintf "thread_ref: %s" (String.trim t);
            "progress_and_results: anchored_thread";
          ])
    @ [ "source: room_background_work"; "attribution: App (P19 pilot)" ]
  in
  String.concat "\n" parts

let enqueue_work_item ~db ~(req : request) ?actor_snapshot ?attribution_allow
    ?now:_ () =
  match validate_request req with
  | Error e -> Error e
  | Ok () -> (
      Github_work_item.init_schema db;
      let room_id = String.trim req.room_id in
      let prompt = String.trim req.prompt in
      let dedup = String.trim req.dedup_key in
      let repo_full_name, issue_number, is_pr =
        match req.item_key with
        | Some k -> (
            match parse_item_key k with
            | Some (repo, n, is_pr) -> (repo, n, is_pr)
            | None ->
                (* Unparseable key: room-scoped synthetic identity so enqueue
                   still records durable work without inventing a GitHub
                   issue number from free text. *)
                (Printf.sprintf "room/%s" room_id, 1, false))
        | None -> (Printf.sprintf "room/%s" room_id, 1, false)
      in
      let requester = Printf.sprintf "room:%s" room_id in
      let runner_pref =
        match req.runner_pref with
        | None -> None
        | Some r when String.trim r = "" -> None
        | Some r -> Some (String.trim r)
      in
      let host_pref =
        match req.thread_ref with
        | None -> None
        | Some t when String.trim t = "" -> None
        | Some t -> Some (Printf.sprintf "thread:%s" (String.trim t))
      in
      let preamble = build_preamble ~req in
      let policy_ref =
        match req.thread_ref with
        | Some t when String.trim t <> "" ->
            Some (Printf.sprintf "thread_ref:%s" (String.trim t))
        | _ -> Some (Printf.sprintf "room:%s" room_id)
      in
      match
        Github_work_item.create_if_new ~db ~dedup_key:dedup ~repo_full_name
          ~is_pr ~issue_number ~requester ~trigger:"room_background"
          ?runner_pref ?host_pref ~prompt ~preamble ?policy_ref ?actor_snapshot
          ?attribution_allow ()
      with
      | Ok (Github_work_item.Created item) -> Ok item
      | Ok (Github_work_item.Duplicate item) -> Ok item
      | Error e -> Error e)

let get_or_err ~db ~id =
  match Github_work_item.get ~db ~id with
  | Some item -> Ok item
  | None -> Error (Printf.sprintf "work item %d not found" id)

let cancel_work_item ~db ~id () =
  match get_or_err ~db ~id with
  | Error e -> Error e
  | Ok item ->
      if Github_work_item.is_terminal_status item.status then
        (* Already terminal: treat cancel as idempotent when already cancelled. *)
        if item.status = Github_work_item.Cancelled then Ok item
        else
          Error
            (Printf.sprintf "cannot cancel work item %d in terminal status %s"
               id
               (Github_work_item.string_of_status item.status))
      else (
        Github_work_item.record_result ~db ~id
          ~status:Github_work_item.Cancelled
          ~result_kind:Github_work_item.Result_failed
          ~result_summary:"cancelled from room background work";
        get_or_err ~db ~id)

let request_retry ~db ~id () =
  match get_or_err ~db ~id with
  | Error e -> Error e
  | Ok item -> (
      match item.status with
      | Github_work_item.Running ->
          Error
            (Printf.sprintf
               "cannot retry work item %d while running; cancel first or wait"
               id)
      | Github_work_item.Queued -> Ok item
      | Github_work_item.Succeeded | Github_work_item.Failed
      | Github_work_item.Cancelled | Github_work_item.Blocked ->
          (* Re-queue; bump attempt_count when a prior background task was
             attached by re-using attach_task with the same id when present. *)
          (match item.background_task_id with
          | Some tid ->
              Github_work_item.attach_task ~db ~id ~background_task_id:tid
          | None -> ());
          Github_work_item.set_status ~db ~id ~status:Github_work_item.Queued;
          get_or_err ~db ~id)

let mark_progress ~db ~id () =
  match get_or_err ~db ~id with
  | Error e -> Error e
  | Ok item ->
      if Github_work_item.is_terminal_status item.status then
        Error
          (Printf.sprintf "cannot mark progress on terminal work item %d (%s)"
             id
             (Github_work_item.string_of_status item.status))
      else (
        Github_work_item.set_status ~db ~id ~status:Github_work_item.Running;
        get_or_err ~db ~id)

let mark_blocked ~db ~id ~summary () =
  match get_or_err ~db ~id with
  | Error e -> Error e
  | Ok item ->
      if Github_work_item.is_terminal_status item.status then
        Error
          (Printf.sprintf "cannot mark blocked on terminal work item %d (%s)" id
             (Github_work_item.string_of_status item.status))
      else (
        Github_work_item.record_result ~db ~id ~status:Github_work_item.Blocked
          ~result_kind:Github_work_item.Result_blocked
          ~result_summary:(String.trim summary);
        get_or_err ~db ~id)

let mark_completed ~db ~id ~summary () =
  match get_or_err ~db ~id with
  | Error e -> Error e
  | Ok item ->
      if Github_work_item.is_terminal_status item.status then
        Error
          (Printf.sprintf "cannot mark completed on terminal work item %d (%s)"
             id
             (Github_work_item.string_of_status item.status))
      else (
        Github_work_item.record_result ~db ~id
          ~status:Github_work_item.Succeeded ~result_kind:Github_work_item.Reply
          ~result_summary:(String.trim summary);
        get_or_err ~db ~id)

let is_background_plan (plan : Setup_plan.t) =
  match plan.apply_payload.kind with
  | Setup_plan.Generic "github_room_background_work" -> true
  | _ -> false

let receipt_safe_error error =
  let s = String.trim error in
  let s =
    Str.global_replace
      (Str.regexp "[Bb]earer [A-Za-z0-9._+/=-]+")
      "Bearer [REDACTED]" s
  in
  let s =
    Str.global_replace
      (Str.regexp "\\(ghp\\|gho\\|ghu\\|ghs\\|ghr\\)_[A-Za-z0-9_]+")
      "\\1_[REDACTED]" s
  in
  let s =
    Str.global_replace
      (Str.regexp "github_pat_[A-Za-z0-9_]+")
      "github_pat_[REDACTED]" s
  in
  let s =
    Str.global_replace (Str.regexp "xox[baprs]-[A-Za-z0-9-]+") "xox*-REDACTED" s
  in
  let s =
    Str.global_replace
      (Str.regexp_case_fold
         "\\(token\\|secret\\|password\\|api_key\\|private_key\\|bot_token\\)[ \
          \\t]*[=:][ \\t]*[^ \\t,;]+")
      "\\1=[REDACTED]" s
  in
  let max_len = 512 in
  let len = String.length s in
  if len <= max_len then s
  else
    String.sub s 0 max_len ^ Printf.sprintf "...<%d more bytes>" (len - max_len)
