open Lwt.Syntax
(** Bounded pause-first discovery. No upload, relay or recovery admission is
    performed here. The enclosing reconciler owns lifecycle and notice holds. *)

module T = Slack_standing_order_types
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Fetch = Slack_standing_order_fetch
module R = Slack_standing_order_recovery_store
module S = Slack_standing_order_scan_store
module Q = Slack_standing_order_schedule
module P = Slack_standing_order_provenance
module Pages = Slack_standing_order_pages
module Pause = Slack_standing_order_pause
module Journal = Slack_standing_order_pause_store
module Ctx = Slack_standing_order_ctx
module Store = Slack_standing_order_store
module Sql = Slack_standing_order_sql
module L = Slack_huddle_ledger
module C = Slack_huddle_classify

type incomplete_code = Pages.incomplete_code

type scan_failure =
  | Grant_failure of Grant.failure
  | Root_failure of Fetch.resolution_failure
  | Page_incomplete of { method_ : string; code : incomplete_code }
  | Lease_lost

let lower_bound ~active_since ~now = max active_since (now -. 172800.)
let timestamp f = Printf.sprintf "%.6f" f
let reconcile_id owner = owner.R.owner_id ^ ":" ^ string_of_int owner.generation

let ( let*! ) p f =
  let* r = p in
  match r with Ok v -> f v | Error _ as e -> Lwt.return e

let ok = Lwt.return (Ok ())

let failure_fields = function
  | Grant_failure f | Root_failure (Fetch.Authority f) ->
      let method_ =
        match f.reason with
        | Grant.Missing_scope m | Egress_denied m | Indeterminate m -> m
        | _ -> "grant"
      in
      let cls =
        match f.cls with
        | Grant.Hard -> "hard"
        | Transient -> "transient"
        | Inactive -> "inactive"
        | Capped -> "capped"
      in
      (method_, cls ^ ":" ^ Grant.reason_code f.reason)
  | Root_failure (Fetch.Incomplete { method_; code })
  | Page_incomplete { method_; code } ->
      (method_, Pages.code_name code)
  | Root_failure Fetch.Lost_fence | Lease_lost -> ("fence", "lost")

let record_failure db (order : T.order) ~owner resume_unit failure =
  let method_, code = failure_fields failure in
  if R.replay_fenced db ~order ~owner ~now:(Store.now ()) then
    S.status db ~order_id:order.T.id ~method_ ~code ~resume_unit

let active_fence ~ctx ~db ~manager ~config ~owner (order : T.order) =
  let gap = Sql.guard (fun () -> Ok (R.pass_token db order.id)) in
  fun () ->
    if
      R.attempt_bool (fun () ->
          ctx.Ctx.accepting ()
          && Session.get_config manager == config
          && Result.fold
               ~ok:(fun gap -> R.current_gap db gap ~connected:true)
               ~error:(fun _ -> false)
               gap
          && R.fenced db ~order_id:order.id ~owner ~now:(ctx.now ()))
    then Ok ()
    else Error SO.Dispatch_refused

let pause_fence ~ctx ~db ~manager ~config ~owner ~(order : T.order) =
  let gap = Sql.guard (fun () -> Ok (R.pass_token db order.id)) in
  fun () ->
    if
      ctx.Ctx.accepting ()
      && Session.get_config manager == config
      && Result.fold
           ~ok:(fun gap -> R.current_gap db gap ~connected:true)
           ~error:(fun _ -> false)
           gap
      && R.replay_fenced db ~order ~owner ~now:(ctx.now ())
    then Ok ()
    else Error SO.Dispatch_refused

let scoped ~check ~priority call method_ params =
  match check () with
  | Error e -> Lwt.return (Error e)
  | Ok () -> SO.with_policy ~check ~priority (fun () -> call method_ params)

let page ~check ~call ~method_ ~params ~seen ~remaining =
  let* response = call method_ params in
  if check () <> Ok () then Lwt.return (Error Lease_lost)
  else
    match response with
    | Error e -> Lwt.return (Error (Grant_failure (P.grant_failure ~method_ e)))
    | Ok json ->
        Lwt.return
          (match (Pages.messages json, Pages.next ~seen ~remaining json) with
          | Ok messages, Ok next -> Ok (messages, next)
          | Error code, _ | _, Error code ->
              Error (Page_incomplete { method_; code }))

let cursor_params cursor =
  if cursor = "" then [] else [ ("cursor", `String cursor) ]

let protect ~check ~db ~(order : T.order) ~owner ~unit work =
  Lwt.catch
    (fun () ->
      let* result = work () in
      (* A committed pause can finish with its barrier closed even when a
         notification's config is revoked. Only errors can mutate status here. *)
      if Result.is_error result && check () <> Ok () then
        Lwt.return (Error Lease_lost)
      else (
        (match result with
        | Error f -> (
            (match f with
            | Root_failure _ | Lease_lost -> ()
            | _ -> record_failure db order ~owner unit f);
            match f with
            | Grant_failure failure
              when failure.cls = Grant.Hard
                   && R.fenced db ~order_id:order.id ~owner ~now:(Store.now ())
              -> (
                match
                  Sql.in_tx db ~on_error:Fun.id (fun () ->
                      if
                        R.fenced db ~order_id:order.id ~owner
                          ~now:(Store.now ())
                      then
                        Store.suspend_if_epoch_in_tx db ~order_id:order.id
                          ~epoch:order.epoch
                          ~reason:(Grant.reason_code failure.reason)
                          ~now:(Store.now ())
                      else Ok false)
                with
                | Ok true -> ()
                | _ ->
                    Logs.warn (fun m ->
                        m "standing-order reconcile suspension fence lost"))
            | _ -> ())
        | Ok () -> ());
        Lwt.return result))
    (function
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn when Sql.is_db_error exn ->
          if check () <> Ok () then Lwt.return (Error Lease_lost)
          else
            let failure =
              Page_incomplete
                { method_ = "conversations.history"; code = Pages.Store_failed }
            in
            (* A failed status write is still a typed failure, never completion. *)
            (try record_failure db order ~owner unit failure
             with exn when Sql.is_db_error exn ->
               Logs.warn (fun m -> m "standing-order scan status write failed"));
            Lwt.return (Error failure)
      | exn -> Lwt.fail exn)

let collect ~db ~(order : T.order) ~oldest ~latest ~parent messages =
  let within ts = P.compare_ts ts oldest >= 0 && P.compare_ts ts latest <= 0 in
  List.iter
    (fun message ->
      let user = P.field "user" message and ts = P.field "ts" message in
      if
        within ts
        && Slack_config_types.valid_id "UW" user
        && user <> "USLACKBOT"
        && P.get "bot_id" message = `Null
        && P.get "subtype" message = `Null
        && P.get "edited" message = `Null
        && P.same_channel order.channel_id message
      then
        match Pause.parse_command (P.field "text" message) with
        | Some n when n = order.display_no -> (
            let thread_ts = Option.value parent ~default:ts in
            let applied =
              Sql.int_query db
                "SELECT count(*) FROM slack_order_ingress WHERE installation=? \
                 AND team=? AND app=? AND channel=? AND identity=? AND pause=1 \
                 AND applied=1"
                (Slack_standing_order_admission.scope db ~team:order.team_id
                   ~app:order.api_app_id ~channel:order.channel_id
                @ [ Sql.text ("message:" ^ ts) ])
              = 1
            in
            if not applied then
              let status =
                Journal.remember db order { ts; thread_ts; user_id = user }
              in
              if status = "pending" && S.maintenance db order.id then
                match
                  R.close_gap db ~team_id:order.team_id ~app_id:order.api_app_id
                    ~channel_id:order.channel_id ~cause:"missed_pause"
                    ~now:(Store.now ())
                with
                | Ok token -> ignore (R.mark_connected db ~token)
                | Error e -> failwith e)
        | _ -> ())
    messages

let sweep_internal ~page_budget ~wrap ~db ~manager ~ctx ~owner
    ~(order : T.order) ~snapshot ~call =
  let config = Session.get_config manager in
  let replay_check = pause_fence ~ctx ~db ~manager ~config ~owner ~order in
  protect ~check:replay_check ~db ~order ~owner ~unit:"pause" (fun () ->
      if
        not
          (ctx.Ctx.accepting ()
          && R.fenced db ~order_id:order.id ~owner ~now:(ctx.now ()))
      then Lwt.return (Error Lease_lost)
      else (
        S.ensure_schema db;
        Journal.ensure_schema db;
        S.status db ~order_id:order.id ~method_:"conversations.history"
          ~code:"pause_incomplete" ~resume_unit:"pause";
        let check = active_fence ~ctx ~db ~manager ~config ~owner order in
        let call =
          scoped ~check ~priority:Slack_read_limiter.Maintenance (wrap call)
        in
        match R.load_coverage db ~order_id:order.T.id with
        | None -> Lwt.return (Error Lease_lost)
        | Some coverage -> (
            let now = Option.get (float_of_string_opt coverage.latest) in
            let oldest =
              if S.maintenance db order.id then coverage.oldest
              else
                timestamp
                  (max (now -. 172800.)
                     (Option.value coverage.last_success
                        ~default:(now -. 172800.)))
            in
            let history_done =
              S.done_unit db order coverage "pause" "history"
            in
            let save c =
              R.save_page db ~order_id:order.id ~owner ~coverage:c
                ~candidates:[] ~now:(ctx.now ())
            in
            let add_thread root =
              S.complete_unit db ~order ~owner ~coverage ~kind:"pause_parent"
                ~unit:root ~outcome:"known" ~now:(ctx.now ())
              && (List.exists
                    (fun (r, _, _) -> r = root)
                    (S.thread_work db ~order_id:order.id ~owner)
                 || R.save_thread db ~order_id:order.id ~owner ~root_ts:root
                      ~cursor:"" ~completed:false ~now:(ctx.now ()))
            in
            let add_threads roots = List.for_all add_thread roots in
            let rec history remaining seen cursor =
              let*! messages, next =
                page ~check ~call ~method_:"conversations.history" ~seen
                  ~remaining
                  ~params:
                    ([
                       ("channel", `String order.channel_id);
                       ("oldest", `String oldest);
                       ("latest", `String coverage.latest);
                       ("inclusive", `Bool true);
                       ("limit", `Int 200);
                     ]
                    @ cursor_params cursor)
              in
              if check () <> Ok () then Lwt.return (Error Lease_lost)
              else if
                not (List.for_all (P.same_channel order.channel_id) messages)
              then
                Lwt.return
                  (Error
                     (Page_incomplete
                        {
                          method_ = "conversations.history";
                          code = Pages.Malformed;
                        }))
              else (
                collect ~db ~order ~oldest ~latest:coverage.latest ~parent:None
                  messages;
                let parents =
                  List.filter_map
                    (fun m ->
                      let root, needed =
                        S.parent_metadata db order coverage m
                      in
                      if needed then Some root else None)
                    messages
                in
                let known =
                  S.threads db order
                  |> List.filter (S.known_thread_needed db order coverage)
                in
                if not (add_threads (parents @ known)) then
                  Lwt.return (Error Lease_lost)
                else if not (save { coverage with R.pause_cursor = next }) then
                  Lwt.return (Error Lease_lost)
                else if next <> "" then
                  history (remaining - 1) (next :: seen) next
                else if
                  S.complete_unit db ~order ~owner ~coverage ~kind:"pause"
                    ~unit:"history" ~outcome:"complete" ~now:(ctx.now ())
                then ok
                else Lwt.return (Error Lease_lost))
            in
            let*! () =
              if history_done then ok
              else
                history page_budget
                  (if coverage.pause_cursor = "" then []
                   else [ coverage.pause_cursor ])
                  coverage.pause_cursor
            in
            let*! () =
              if
                add_threads
                  (S.parents db order coverage
                  @ (S.threads db order
                    |> List.filter (S.known_thread_needed db order coverage)))
              then ok
              else Lwt.return (Error Lease_lost)
            in
            let rec thread root remaining seen cursor =
              let*! messages, next =
                page ~check ~call ~method_:"conversations.replies" ~seen
                  ~remaining
                  ~params:
                    ([
                       ("channel", `String order.channel_id);
                       ("ts", `String root);
                       ("limit", `Int 200);
                     ]
                    @ cursor_params cursor)
              in
              if check () <> Ok () then Lwt.return (Error Lease_lost)
              else if
                not
                  (List.for_all
                     (fun m ->
                       P.same_channel order.channel_id m
                       && (P.field "ts" m = root || P.field "thread_ts" m = root))
                     messages)
              then
                Lwt.return
                  (Error
                     (Page_incomplete
                        {
                          method_ = "conversations.replies";
                          code = Pages.Malformed;
                        }))
              else (
                collect ~db ~order ~oldest ~latest:coverage.latest
                  ~parent:(Some root) messages;
                if
                  not
                    (R.save_thread db ~order_id:order.id ~owner ~root_ts:root
                       ~cursor:next ~completed:(next = "") ~now:(ctx.now ()))
                then Lwt.return (Error Lease_lost)
                else if next = "" then (
                  S.finish_thread db order coverage root;
                  ok)
                else thread root (remaining - 1) (next :: seen) next)
            in
            let rec threads = function
              | [] -> ok
              | (_, _, true) :: rest -> threads rest
              | (root, cursor, false) :: rest ->
                  let*! () =
                    thread root page_budget
                      (if cursor = "" then [] else [ cursor ])
                      cursor
                  in
                  threads rest
            in
            let*! () = threads (S.thread_work db ~order_id:order.id ~owner) in
            (* Collect all pages before replay so pagination order never reorders commands. *)
            let replay_failure = ref None in
            let replay_call method_ params =
              let+ result =
                scoped ~check:replay_check ~priority:Slack_read_limiter.Pause
                  (* Use the original call, not the active-only wrapper. *)
                  snapshot.Grant.call method_ params
              in
              (match result with
              | Error error ->
                  replay_failure := Some (P.grant_failure ~method_ error)
              | Ok _ -> ());
              result
            in
            let limiter =
              match SO.describe_call snapshot.Grant.call with
              | Some { limiter = Some l; _ } -> l
              | _ -> (
                  match ctx.limiter with
                  | Some l -> l
                  | None -> SO.ensure_limiter ())
            in
            let replay =
              {
                Pause.call = replay_call;
                reconcile_id = reconcile_id owner;
                snapshot_id = snapshot.snapshot.id;
                limiter;
                check = (fun () -> replay_check () = Ok ());
              }
            in
            let messages =
              Journal.messages db order
              |> List.sort (fun (a : Journal.message) b ->
                  P.compare_ts a.ts b.ts)
            in
            let rec replay_all = function
              | [] -> ok
              | message :: rest -> (
                  replay_failure := None;
                  let* outcome =
                    Pause.run ~manager ~now:(ctx.now ()) ~replay ~message
                      ~team_id:order.team_id ~app_id:order.api_app_id
                      ~channel_id:order.channel_id ~user_id:message.user_id
                      ~target:(`Order order.id) ~source:Pause.Text ()
                  in
                  match outcome with
                  | Pause.Refused
                      ( Grant.Member_indeterminate method_
                      | Grant.Member_egress method_ ) ->
                      Lwt.return
                        (Error
                           (Grant_failure
                              (Option.value !replay_failure
                                 ~default:
                                   (Grant.failure (Grant.Indeterminate method_)))))
                  | Pause.Unreadable _ ->
                      Lwt.return
                        (Error
                           (Page_incomplete
                              {
                                method_ = "conversations.history";
                                code = Pages.Store_failed;
                              }))
                  | _ ->
                      let* () =
                        Pause.post_receipt ~manager ~order ~replay ~message
                          ~channel_id:order.channel_id
                          ~thread_ts:message.thread_ts
                          ("Late pause receipt. "
                          ^ Pause.receipt ~display_no:order.display_no
                              ~by:message.user_id outcome)
                      in
                      replay_all rest)
            in
            let*! () = replay_all messages in
            if
              not
                (ctx.accepting ()
                && R.replay_fenced db ~order ~owner ~now:(ctx.now ()))
            then Lwt.return (Error Lease_lost)
            else
              match Store.find db order.id with
              | Some o when o.state <> T.Active ->
                  S.status db ~order_id:order.id
                    ~method_:"conversations.history" ~code:"paused"
                    ~resume_unit:"pause";
                  ok (* Committed pause keeps barrier closed. *)
              | _ ->
                  if
                    R.complete_pause db ~order_id:order.id ~owner
                      ~now:(ctx.now ())
                  then (
                    S.status db ~order_id:order.id
                      ~method_:"conversations.history" ~code:"pause_complete"
                      ~resume_unit:"history";
                    ok)
                  else Lwt.return (Error Lease_lost))))

let make_validated ~db ~manager ~owner ~order ~call =
  Lwt.return
    (Grant.reconcile_validation ~db
       ~current_config:(fun () -> Session.get_config manager)
       ~now:(Store.now ()) ~reconcile_id:(reconcile_id owner) ~call
       (Grant.subject_of_order order))

let sweep ~db ~manager ~ctx ~owner ~(order : T.order) ~call =
  (* Sweep's membership authority is independent of full grantor/chat policy.
     Its transport must already be persisted and capability-fenced. *)
  match SO.describe_call call with
  | None ->
      Lwt.return
        (Error
           (Grant_failure
              (Grant.failure (Grant.Indeterminate "access_snapshot"))))
  | Some scope
    when scope.channel_id = order.T.channel_id
         && scope.snapshot.standing_order
            = Some { Access_snapshot.order_id = order.id; fire_id = None }
         && scope.audit
            = SO.audit_label ~order_id:(Some order.id)
                ~unit_label:("reconcile:" ^ reconcile_id owner) ->
      let validated =
        {
          Grant.snapshot = scope.snapshot;
          config = Session.get_config manager;
          order = Some order;
          replaced = None;
          enterprise_id = order.enterprise_id;
          bot_user_id = "";
          bot_id = "";
          team_url = "";
          call;
        }
      in
      sweep_internal ~page_budget:20 ~wrap:Fun.id ~db ~manager ~ctx ~owner
        ~order ~snapshot:validated ~call
  | Some _ ->
      Lwt.return (Error (Grant_failure (Grant.failure Grant.Identity_changed)))

let sweep_with_grant_wrapped ~page_budget ~wrap ~db ~manager ~ctx ~owner
    ~(validated : Grant.validated) ~order =
  sweep_internal ~page_budget ~wrap ~db ~manager ~ctx ~owner ~order
    ~snapshot:validated ~call:validated.call

let sweep_with_grant ~db ~manager ~ctx ~owner ~validated ~order =
  sweep_with_grant_wrapped ~page_budget:20 ~wrap:Fun.id ~db ~manager ~ctx ~owner
    ~validated ~order

let discover_wrapped ~page_budget ~wrap ~db ~manager ~ctx ~owner
    ~(order : T.order) ~call =
  let config = Session.get_config manager in
  let check = active_fence ~ctx ~db ~manager ~config ~owner order in
  let result_check = pause_fence ~ctx ~db ~manager ~config ~owner ~order in
  (* The resolver's entire inspection, including post-response local writes,
     belongs to this captured pass/gap/config, not just its HTTP dispatch. *)
  let resolver_ctx =
    { ctx with Ctx.accepting = (fun () -> check () = Ok ()) }
  in
  protect ~check:result_check ~db ~order ~owner ~unit:"history" (fun () ->
      let*! () =
        if check () = Ok () then ok else Lwt.return (Error Lease_lost)
      in
      S.ensure_schema db;
      let validated_call = call in
      let call =
        scoped ~check ~priority:Slack_read_limiter.Maintenance (wrap call)
      in
      match R.load_coverage db ~order_id:order.T.id with
      | None -> Lwt.return (Error Lease_lost)
      | Some _ when R.phase db order.id <> Some "history" ->
          Lwt.return (Error Lease_lost)
      | Some coverage ->
          let*! validated =
            let+ v =
              make_validated ~db ~manager ~owner ~order ~call:validated_call
            in
            Result.map_error (fun f -> Grant_failure f) v
          in
          let validated = { validated with Grant.call } in
          let inspect root =
            if check () <> Ok () then Lwt.return (Error Lease_lost)
            else if S.done_unit db order coverage "root" root then ok
            else
              match S.previous_outcome db order root with
              | Some (("no_notes" | "ineligible") as outcome) ->
                  if
                    S.complete_unit db ~order ~owner ~coverage ~kind:"root"
                      ~unit:root ~outcome ~now:(ctx.now ())
                  then ok
                  else Lwt.return (Error Lease_lost)
              | _ -> (
                  Q.ensure_schema db;
                  Q.sync ~db order;
                  let key = Q.key order root in
                  if
                    Sql.int_query db
                      ("SELECT count(*) FROM slack_order_root_schedule WHERE "
                     ^ Q.where)
                      (Q.params key)
                    = 0
                  then Q.register ~db ~key ~metadata:"pinned_recovery_root";
                  Q.reopen_for_inspection ~db ~key:(Q.key order root)
                    ~now:(ctx.now ());
                  if not (Q.due ~db ~key:(Q.key order root) ~now:(ctx.now ()))
                  then
                    if
                      Sql.int_query db
                        "SELECT count(*) FROM slack_order_root_schedule WHERE \
                         order_id=? AND root=? AND retry_at>?"
                        Sql.
                          [
                            text order.id;
                            text root;
                            Sqlite3.Data.FLOAT (ctx.now ());
                          ]
                      > 0
                    then
                      Lwt.return
                        (Error
                           (Root_failure
                              (Fetch.Incomplete
                                 {
                                   method_ = "conversations.history";
                                   code = Pages.Store_failed;
                                 })))
                    else ok
                  else
                    let* result =
                      Fetch.resolve_with_grant ~ctx:resolver_ctx ~db ~manager
                        ~validated ~root order
                    in
                    match result with
                    | Error Fetch.Lost_fence ->
                        Lwt.return (Error (Root_failure Fetch.Lost_fence))
                    | Error failure ->
                        if result_check () = Ok () then
                          record_failure db order ~owner root
                            (Root_failure failure);
                        Lwt.return (Error (Root_failure failure))
                    | Ok _ when check () <> Ok () ->
                        Lwt.return (Error Lease_lost)
                    | Ok outcome ->
                        Sql.exec db
                          "UPDATE slack_order_root_schedule SET \
                           next_probe_at=MIN(next_probe_at,CAST(? AS \
                           REAL)+300) WHERE order_id=? AND epoch=? AND root=? \
                           AND retry_at=0"
                          Sql.
                            [
                              text coverage.latest;
                              text order.id;
                              int order.epoch;
                              text root;
                            ];
                        let name =
                          match outcome with
                          | Fetch.Live _ -> "live"
                          | Not_ready _ -> "not_ready"
                          | Candidate _ -> "candidate"
                          | Ineligible _ -> "ineligible"
                          | Missing_exact_root -> "missing_exact_root"
                          | Unavailable _ -> "unavailable"
                          | No_notes _ -> "no_notes"
                        in
                        if
                          S.complete_unit db ~order ~owner ~coverage
                            ~kind:"root" ~unit:root ~outcome:name
                            ~now:(ctx.now ())
                        then ok
                        else Lwt.return (Error Lease_lost))
          in
          let rec inspect_all = function
            | [] -> ok
            | root :: rest ->
                let*! () = inspect root in
                inspect_all rest
          in
          let rec history remaining seen cursor =
            let*! messages, next =
              page ~check ~call ~method_:"conversations.history" ~seen
                ~remaining
                ~params:
                  ([
                     ("channel", `String order.channel_id);
                     ("oldest", `String coverage.oldest);
                     ("latest", `String coverage.latest);
                     ("inclusive", `Bool true);
                     ("limit", `Int 200);
                   ]
                  @ cursor_params cursor)
            in
            if check () <> Ok () then Lwt.return (Error Lease_lost)
            else if
              not (List.for_all (P.same_channel order.channel_id) messages)
            then
              Lwt.return
                (Error
                   (Page_incomplete
                      {
                        method_ = "conversations.history";
                        code = Pages.Malformed;
                      }))
            else
              let roots =
                messages
                |> List.filter_map (fun json ->
                    match C.room_of_json (P.get "room" json) with
                    | Some room when room.date_start <> None ->
                        let root = P.field "ts" json in
                        ignore
                          (Slack_standing_order_resolution.record ~db ~order
                             ~root room ~now:(ctx.now ()));
                        L.record_seen ~db ~team:order.team_id
                          ~channel:order.channel_id ~source:"reconcile"
                          ~now:(ctx.now ()) ~root_ts:root
                          ~file_ids:(P.file_ids json) room;
                        Some root
                    | _
                      when P.field "call_family" (P.get "room" json) = "huddle"
                      ->
                        Some (P.field "ts" json)
                    | _ -> None)
                |> List.sort_uniq P.compare_ts
              in
              if
                (not
                   (List.for_all
                      (fun root ->
                        S.complete_unit db ~order ~owner ~coverage
                          ~kind:"discovery_root" ~unit:root ~outcome:"pending"
                          ~now:(ctx.now ()))
                      roots))
                || check () <> Ok ()
              then Lwt.return (Error Lease_lost)
              else if
                not
                  (R.save_page db ~order_id:order.id ~owner
                     ~coverage:
                       {
                         coverage with
                         R.history_cursor = next;
                         pause_cursor = "";
                       }
                     ~candidates:[] ~now:(ctx.now ()))
              then Lwt.return (Error Lease_lost)
              else if next = "" then ok
              else history (remaining - 1) (next :: seen) next
          in
          let*! () =
            if S.done_unit db order coverage "history" "enumerated" then ok
            else
              let*! () =
                history page_budget
                  (if coverage.history_cursor = "" then []
                   else [ coverage.history_cursor ])
                  coverage.history_cursor
              in
              if
                S.complete_unit db ~order ~owner ~coverage ~kind:"history"
                  ~unit:"enumerated" ~outcome:"complete" ~now:(ctx.now ())
              then ok
              else Lwt.return (Error Lease_lost)
          in
          let*! () =
            inspect_all
              (List.sort_uniq P.compare_ts
                 (S.discovery_roots db order coverage @ S.pending_roots db order))
          in
          if check () <> Ok () then Lwt.return (Error Lease_lost)
          else if
            R.complete_history db ~order_id:order.id ~owner ~now:(ctx.now ())
          then (
            S.status db ~order_id:order.id ~method_:"conversations.history"
              ~code:"discovery_complete" ~resume_unit:"";
            ok)
          else Lwt.return (Error Lease_lost))

let discover ~db ~manager ~ctx ~owner ~order ~call =
  discover_wrapped ~page_budget:20 ~wrap:Fun.id ~db ~manager ~ctx ~owner ~order
    ~call
