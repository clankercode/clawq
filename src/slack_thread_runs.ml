open Lwt.Syntax
(** Run ownership is independent of Slack's aggregate session status. Native
    stops must name an owned stream; an empty list cannot select a generation.
*)

open Slack_huddles

type clock = { now : unit -> float; sleep : float -> unit Lwt.t }

let clock_key : clock Lwt.key = Lwt.new_key ()
let with_clock ~now ~sleep f = Lwt.with_value clock_key (Some { now; sleep }) f

type group = {
  ui : Lwt_mutex.t;
  mutable owner : string option;
  mutable references : int;
  mutable terminal : unit Lwt.t;
}

let groups : (Session.t * string * group) list ref = ref []

let group manager key =
  match List.find_opt (fun (m, k, _) -> m == manager && k = key) !groups with
  | Some (_, _, g) ->
      g.references <- g.references + 1;
      g
  | None ->
      let g =
        {
          ui = Lwt_mutex.create ();
          owner = None;
          references = 1;
          terminal = Lwt.return_unit;
        }
      in
      groups := (manager, key, g) :: !groups;
      g

type run = {
  manager : Session.t;
  route : Slack_thread_route.t;
  key : string;
  message : string;
  owner : string;
  reference : string;
  group : group;
  done_ : unit Lwt.t;
  ended : unit Lwt.u;
  mutable control : string option;
  mutable stream : string option;
  mutable active : bool;
  mutable finished : bool;
  mutable stopped : bool;
  clock : clock;
  mutable stream_deadline : float option;
  mutable maintenance : unit Lwt.t option;
  mutable maintenance_stop : (unit Lwt.t * unit Lwt.u) option;
}

let runs : (string, run) Hashtbl.t = Hashtbl.create 32

let find manager key message =
  Hashtbl.to_seq_values runs |> List.of_seq
  |> List.find_opt (fun r ->
      r.manager == manager && r.key = key && r.message = message
      && not r.finished)

let token manager =
  match (Session.get_config manager).channels.slack with
  | Some sc when Runtime_config.slack_has_valid_credentials sc ->
      Some sc.bot_token
  | _ -> None

let safely f =
  Lwt.catch f (function
    | Lwt.Canceled -> Lwt.fail Lwt.Canceled
    | exn ->
        Logs.warn (fun m -> m "Slack task UI: %s" (Printexc.to_string exn));
        Lwt.return_unit)

let call r ~token method_ params =
  Slack_thread_api.with_route r.manager r.route (fun () ->
      Slack_thread_api.call ~token method_ params)

let status r state =
  match token r.manager with
  | None -> Lwt.return_unit
  | Some token ->
      let* result =
        call r ~token "agents.sessions.setStatus"
          [
            ("channel_id", str r.route.channel);
            ("thread_ts", str r.route.root);
            ("status", str state);
          ]
      in
      (match result with
      | Ok _ -> ()
      | Error e ->
          Logs.warn (fun m ->
              m "Slack native status unavailable: %s; using thread progress" e));
      Lwt.return_unit

let create ~manager ~route ~message ~owner =
  let key = Slack_thread_route.key route in
  let done_, ended = Lwt.wait () in
  let r =
    {
      manager;
      route;
      key;
      message;
      owner;
      group = group manager key;
      done_;
      ended;
      reference = Temp_downloads.generate_token ();
      control = None;
      stream = None;
      active = false;
      finished = false;
      stopped = false;
      clock =
        Option.value (Lwt.get clock_key)
          ~default:{ now = Unix.gettimeofday; sleep = Lwt_unix.sleep };
      stream_deadline = None;
      maintenance = None;
      maintenance_stop = None;
    }
  in
  Hashtbl.add runs r.reference r;
  r

let control r =
  match token r.manager with
  | None -> Lwt.return_unit
  | Some token ->
      let* result =
        call r ~token "chat.postMessage"
          [
            ("channel", str r.route.channel);
            ("thread_ts", str r.route.root);
            ("text", str "Task queued. Stop applies only to this request.");
            ( "blocks",
              `List
                [
                  obj
                    [
                      ("type", str "section");
                      ( "text",
                        obj
                          [
                            ("type", str "plain_text");
                            ("text", str "Task queued / working");
                          ] );
                    ];
                  obj
                    [
                      ("type", str "actions");
                      ( "elements",
                        `List
                          [
                            obj
                              [
                                ("type", str "button");
                                ("action_id", str "clawq_stop_run");
                                ( "text",
                                  obj
                                    [
                                      ("type", str "plain_text");
                                      ("text", str "Stop this task");
                                    ] );
                                ("value", str r.reference);
                              ];
                          ] );
                    ];
                ] );
          ]
      in
      (match result with
      | Ok j ->
          let ts = field "ts" j in
          if valid_ts ts then r.control <- Some ts
      | Error e -> Logs.warn (fun m -> m "%s" e));
      Lwt.return_unit

let owns r =
  r.active && (not r.finished) && (not r.stopped)
  && r.group.owner = Some r.reference

let open_stream r =
  match token r.manager with
  | None -> Lwt.return_unit
  | Some token ->
      let requested_at = r.clock.now () in
      let* result =
        call r ~token "chat.startStream"
          [
            ("channel", str r.route.channel);
            ("thread_ts", str r.route.root);
            ("recipient_user_id", str r.owner);
            ("recipient_team_id", str r.route.team);
            ("markdown_text", str "Working…");
          ]
      in
      (match result with
      | Ok json ->
          let ts = field "ts" json in
          if valid_ts ts then begin
            r.stream <- Some ts;
            r.stream_deadline <- Some (requested_at +. 600.)
          end
      | Error reason ->
          Logs.warn (fun m -> m "Slack stream unavailable: %s" reason));
      Lwt.return_unit

let close_stream r state =
  let previous = r.stream in
  r.stream <- None;
  r.stream_deadline <- None;
  match (previous, token r.manager) with
  | Some ts, Some token ->
      let* result =
        call r ~token "chat.stopStream"
          [
            ("channel", str r.route.channel);
            ("ts", str ts);
            ("session_status", str state);
          ]
      in
      (match result with
      | Ok _ -> ()
      | Error reason -> Logs.warn (fun m -> m "Slack stream close: %s" reason));
      Lwt.return_unit
  | _ -> Lwt.return_unit

let start_maintenance r =
  let stop, wake = Lwt.wait () in
  r.maintenance_stop <- Some (stop, wake);
  let rec loop () =
    if not (owns r) then Lwt.return_unit
    else
      let delay =
        match r.stream_deadline with
        | Some deadline -> max 0. (min 540. (deadline -. r.clock.now () -. 60.))
        | None -> 540.
      in
      let* tick =
        Lwt.pick
          [
            (let* () = r.clock.sleep delay in
             Lwt.return_true);
            (let* () = Lwt.protected stop in
             Lwt.return_false);
          ]
      in
      if not tick then Lwt.return_unit
      else
        let* () =
          Lwt_mutex.with_lock r.group.ui (fun () ->
              if not (owns r) then Lwt.return_unit
              else
                let* () = status r "processing" in
                if not (owns r) then Lwt.return_unit
                else
                  let* () = close_stream r "processing" in
                  if owns r then open_stream r else Lwt.return_unit)
        in
        loop ()
  in
  let task =
    Lwt.catch loop (function
      | Lwt.Canceled -> Lwt.return_unit
      | exn ->
          Logs.warn (fun m ->
              m "Slack task maintenance: %s" (Printexc.to_string exn));
          Lwt.return_unit)
  in
  r.maintenance <- Some task

let stop_maintenance r =
  (match r.maintenance_stop with
  | Some (stop, wake) when Lwt.is_sleeping stop -> Lwt.wakeup_later wake ()
  | _ -> ());
  r.maintenance_stop <- None;
  match r.maintenance with
  | None -> Lwt.return_unit
  | Some task ->
      (* Join an in-flight UI operation instead of abandoning its reply. The
         ownership check prevents reopening a stream once finish has begun. *)
      let* () = Lwt.protected task in
      r.maintenance <- None;
      Lwt.return_unit

let start r =
  let authorized_config = Session.get_config r.manager in
  let* () = Lwt.protected r.group.terminal in
  Lwt_mutex.with_lock r.group.ui (fun () ->
      if r.finished || r.stopped then
        Lwt.fail_with "Slack task cancelled before execution"
      else begin
        r.active <- true;
        r.group.owner <- Some r.reference;
        r.group.terminal <- r.done_;
        let* authorized =
          Slack_thread_api.authorize ~manager:r.manager ~route:r.route
            ~user:r.owner
        in
        let* () =
          match authorized with
          | Ok _ -> Lwt.return_unit
          | Error reason -> Lwt.fail_with reason
        in
        if Session.get_config r.manager != authorized_config then
          failwith
            "Slack configuration changed while awaiting task ownership; retry \
             the request";
        Slack_thread_route.save_owner r.manager r.route r.owner;
        let* () = status r "processing" in
        match token r.manager with
        | None -> Lwt.fail_with "Slack disabled before model execution"
        | Some token ->
            let* () = open_stream r in
            let* renamed =
              call r ~token "agents.sessions.rename"
                [
                  ("channel_id", str r.route.channel);
                  ("thread_ts", str r.route.root);
                  ("title", str "Clawq task");
                ]
            in
            (match renamed with
            | Ok _ -> ()
            | Error e -> Logs.warn (fun m -> m "%s" e));
            if Session.get_config r.manager != authorized_config then
              Lwt.fail_with
                "Slack configuration changed during task preparation; request \
                 retained"
            else if r.stopped then
              Lwt.fail_with "Task stopped before model execution"
            else (
              start_maintenance r;
              Lwt.return_unit)
      end)

let finish r =
  if r.finished then Lwt.protected r.done_
  else begin
    r.finished <- true;
    Hashtbl.remove runs r.reference;
    Lwt.finalize
      (fun () ->
        let* () = stop_maintenance r in
        Lwt_mutex.with_lock r.group.ui (fun () ->
            let owns_status = r.active && r.group.owner = Some r.reference in
            r.active <- false;
            let* () =
              match token r.manager with
              | None -> Lwt.return_unit
              | Some token -> (
                  let* () =
                    match r.stream with
                    | None -> Lwt.return_unit
                    | Some _ when not owns_status ->
                        r.stream <- None;
                        Lwt.return_unit
                    | Some ts ->
                        r.stream <- None;
                        (* stopStream changes native session status too. If ownership
                   moved, only finalize this old message without an active write. *)
                        let* _ =
                          call r ~token "chat.stopStream"
                            [
                              ("channel", str r.route.channel);
                              ("ts", str ts);
                              ("session_status", str "active");
                            ]
                        in
                        Lwt.return_unit
                  in
                  match r.control with
                  | None -> Lwt.return_unit
                  | Some ts ->
                      let* _ =
                        call r ~token "chat.update"
                          [
                            ("channel", str r.route.channel);
                            ("ts", str ts);
                            ( "text",
                              str
                                (if r.stopped then "Task stopped."
                                 else "Task finished.") );
                            ("blocks", `List []);
                          ]
                      in
                      Lwt.return_unit)
            in
            if owns_status then (
              r.group.owner <- None;
              status r "active")
            else Lwt.return_unit))
      (fun () ->
        if Lwt.is_sleeping r.done_ then Lwt.wakeup_later r.ended ();
        r.group.references <- r.group.references - 1;
        if r.group.references = 0 then
          groups := List.filter (fun (_, _, g) -> g != r.group) !groups;
        Lwt.return_unit)
  end

let finish_message manager key id =
  match Option.bind id (find manager key) with
  | Some r -> finish r
  | None -> Lwt.return_unit

let start_queued manager key id =
  match Option.bind id (find manager key) with
  | Some r ->
      Lwt.catch
        (fun () ->
          let* () = start r in
          Lwt.return_true)
        (fun exn ->
          if r.stopped then
            let* () = finish r in
            Lwt.return_false
          else Lwt.fail exn)
  | None -> Lwt.return_true

let cancel r user =
  let* authorization =
    Slack_thread_api.authorize ~manager:r.manager ~route:r.route ~user
  in
  match authorization with
  | Error _ -> Lwt.return_unit
  | Ok _ ->
      (* Recheck after every async authorization operation, then mutate without a
       yield. Never interrupt the current thread merely because a control exists. *)
      if r.finished || (not (Hashtbl.mem runs r.reference)) || user <> r.owner
      then Lwt.return_unit
      else
        let removed = ref false in
        let pending =
          Option.value
            (Hashtbl.find_opt r.manager.queued_messages r.key)
            ~default:[]
        in
        let remaining =
          List.filter
            (fun (q : Session_types.queued_message) ->
              if q.message_id = Some r.message then (
                removed := true;
                Session_queue.delete_queued_message_row r.manager q;
                false)
              else true)
            pending
        in
        if !removed then
          Hashtbl.replace r.manager.queued_messages r.key remaining;
        if (not !removed) && r.active && r.group.owner = Some r.reference then (
          Session.cancel_pending_question r.manager ~key:r.key;
          match Hashtbl.find_opt r.manager.sessions r.key with
          | Some (_, _, interrupt) ->
              interrupt := Some Agent.stop_interrupt_token
          | None -> ());
        r.stopped <- true;
        if !removed then finish r else Lwt.return_unit

let handle_text ~manager ~route ~user text =
  let key = Slack_thread_route.key route in
  let owned () =
    Hashtbl.to_seq_values runs
    |> Seq.find_map (fun r ->
        if
          r.manager == manager && r.key = key && r.owner = user && r.active
          && (not r.finished)
          && r.group.owner = Some r.reference
        then Some r
        else None)
  in
  match owned () with
  | None -> Lwt.return_false
  | Some r ->
      let admin =
        match Session.get_db manager with
        | Some db -> Admin.is_admin ~db ~channel:"slack" ~sender_id:user
        | None -> false
      in
      let bang = String.starts_with ~prefix:"!" text in
      let stop = admin && String.lowercase_ascii (String.trim text) = "stop" in
      if bang || stop then
        let* () = cancel r user in
        let* () =
          if bang && (not r.finished) && r.group.owner = Some r.reference then
            let raw = String.sub text 1 (String.length text - 1) in
            Session.set_interrupt_if_present manager ~key
              (if String.trim raw = "" then "[interrupted]" else raw)
          else Lwt.return_unit
        in
        Lwt.return_true
      else
        Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
          ~label:"sessions_lock" manager.sessions_lock (fun () ->
            match
              (owned (), Hashtbl.find_opt manager.pending_questions key)
            with
            | Some current, Some resolver when current == r ->
                Hashtbl.remove manager.pending_questions key;
                Lwt.wakeup_later resolver text;
                Lwt.return_true
            | _ -> Lwt.return_false)

let handle ~manager json =
  let team = field "team_id" json and app = field "api_app_id" json in
  let event = get "event" json in
  if field "type" event = "agent_session_stopped" then
    let streams = list (get "streaming_message_ts" event) |> List.map text in
    let matches =
      Hashtbl.to_seq_values runs |> List.of_seq
      |> List.filter (fun r ->
          r.manager == manager && r.route.team = team && r.route.app = app
          && r.route.channel = field "channel" event
          && r.route.root = field "thread_ts" event
          && r.active && (not r.finished)
          &&
          match r.stream with
          | Some ts -> List.mem ts streams
          | None -> false)
    in
    Lwt_list.iter_s (fun r -> cancel r (field "user" event)) matches
  else if field "type" json = "block_actions" then
    let team = field "id" (get "team" json) in
    let container = get "container" json in
    Lwt_list.iter_s
      (fun action ->
        match Hashtbl.find_opt runs (field "value" action) with
        | Some r
          when field "action_id" action = "clawq_stop_run"
               && r.manager == manager && r.route.team = team
               && r.route.app = field "api_app_id" json
               && r.route.channel = field "channel_id" container
               && r.control = Some (field "message_ts" container)
               && field "ts" (get "message" json) = field "message_ts" container
          ->
            cancel r (field "id" (get "user" json))
        | _ -> Lwt.return_unit)
      (list (get "actions" json))
  else Lwt.return_unit

(** Restart loses transport closures and live generations. Recover only the
    persisted address/last executing human, then authorize a fresh run. Old
    native streams and public controls cannot target this new generation. *)
let with_resumed ~manager ~key f =
  match Slack_thread_route.of_key key with
  | None -> f ()
  | Some route -> (
      match Slack_thread_route.load_owner manager key with
      | None ->
          Lwt.fail_with
            "Slack resume has no verified persisted owner; await a fresh \
             thread message"
      | Some owner -> (
          let* authorized =
            Slack_thread_api.authorize ~manager ~route ~user:owner
          in
          match authorized with
          | Error reason -> Lwt.fail_with ("Slack resume refused: " ^ reason)
          | Ok _ ->
              let r =
                create ~manager ~route
                  ~message:("resume:" ^ Temp_downloads.generate_token ())
                  ~owner
              in
              Lwt.finalize
                (fun () ->
                  let* () = control r in
                  let* () = start r in
                  f ())
                (fun () -> finish r)))

let shutdown manager =
  let finishing =
    List.filter_map
      (fun (m, _, g) -> if m == manager then Some g.terminal else None)
      !groups
  in
  let owned =
    Hashtbl.to_seq_values runs |> List.of_seq
    |> List.filter (fun r -> r.manager == manager)
  in
  let* () =
    Lwt_list.iter_s
      (fun r ->
        (if r.active then
           match Hashtbl.find_opt manager.Session.sessions r.key with
           | Some (_, _, interrupt)
             when !interrupt <> Some Agent.restart_interrupt_token ->
               interrupt := Some Agent.stop_interrupt_token
           | _ -> ());
        finish r)
      owned
  in
  Lwt_list.iter_s (fun task -> Lwt.protected task) finishing
