module T = Slack_standing_order_types
module R = Slack_standing_order_recovery_store
module A = Slack_standing_order_admission
module Store = Slack_standing_order_store
module Sql = Slack_standing_order_sql
module Ctx = Slack_standing_order_ctx
module H = Slack_huddles

let enqueue : (unit -> unit) ref = ref (fun () -> ())

type configured = {
  db : Sqlite3.db;
  manager : Session.t;
  ctx : Ctx.t;
  mutable connected : bool;
  mutable socket_mode : bool;
  mutable scopes : (string * string * string) list;
  mutable stopped : bool;
  restore : (unit -> unit) list ref;
}

let configured : configured option ref = ref None

let scopes c =
  let rows =
    Sql.query c.db
      "SELECT DISTINCT team_id,api_app_id,channel_id FROM slack_standing_orders"
      [] (fun s ->
        ( Sql_util.text_column s 0,
          Sql_util.text_column s 1,
          Sql_util.text_column s 2 ))
  in
  List.iter
    (fun (team_id, app_id, channel_id) ->
      if not (List.mem (team_id, app_id, channel_id) c.scopes) then
        match
          R.close_gap c.db ~team_id ~app_id ~channel_id ~cause:"new_order"
            ~now:(c.ctx.now ())
        with
        | Error e -> failwith e
        | Ok token ->
            if c.connected && not (R.mark_connected c.db ~token) then
              failwith "new channel ingress health failed")
    rows;
  c.scopes <- List.sort_uniq compare (c.scopes @ rows);
  c.scopes

let token c (team, app, channel) =
  match
    Sql.query c.db
      "SELECT generation FROM slack_standing_order_recovery_gaps WHERE \
       installation=? AND team=? AND app=? AND channel=?"
      (A.scope c.db ~team ~app ~channel) (fun s -> Sql_util.int_column s 0)
  with
  | [ gap_generation ] ->
      Some
        { R.team_id = team; app_id = app; channel_id = channel; gap_generation }
  | _ -> None

let close c ~cause ~disconnect =
  if not c.stopped then (
    if disconnect then c.connected <- false;
    (* Latch every previously known scope before the first fallible store read. *)
    List.iter
      (fun (team_id, app_id, channel_id) ->
        R.latch c.db { R.team_id; app_id; channel_id; gap_generation = 0 })
      c.scopes;
    List.iter
      (fun (team_id, app_id, channel_id) ->
        match
          R.close_gap c.db ~team_id ~app_id ~channel_id ~cause
            ~now:(c.ctx.now ())
        with
        | Error e -> failwith e
        | Ok token ->
            if c.connected && not (R.mark_connected c.db ~token) then
              failwith "cannot persist ingress connectivity")
      (scopes c);
    !enqueue ())

let installed c body f =
  let json = try Yojson.Safe.from_string body with _ -> `Null in
  let team =
    match H.field "team_id" json with
    | "" -> H.field "id" (H.get "team" json)
    | s -> s
  in
  let app = H.field "api_app_id" json in
  let event = H.get "event" json in
  let channel =
    match H.field "channel" event with
    | "" -> H.field "id" (H.get "channel" json)
    | s -> s
  in
  if List.mem (team, app, channel) (scopes c) then f json event team app channel

let delivery_identity json event =
  let ts = H.field "ts" event in
  if Slack_standing_order_types.valid_slack_ts ts then Some ("message:" ^ ts, ts)
  else
    let ts = H.field "event_ts" event in
    let id = H.field "event_id" json in
    if id <> "" && Slack_standing_order_types.valid_slack_ts ts then
      Some ("event:" ^ id, ts)
    else None

let admit c ~body =
  installed c body (fun json event team app channel ->
      Option.iter
        (fun (identity, ts) ->
          let pause =
            List.mem (H.field "type" event) [ "message"; "app_mention" ]
            && H.field "user" event <> "USLACKBOT"
            && H.field "user" event <> ""
            && H.get "bot_id" event = `Null
            && H.get "edited" event = `Null
            && List.mem (H.field "subtype" event) [ ""; "thread_broadcast" ]
            && Slack_standing_order_pause.parse_command (H.field "text" event)
               <> None
          in
          A.reserve c.db ~team ~app ~channel ~identity ~ts ~pause)
        (delivery_identity json event))

let applied c ~body =
  A.settle_pauses c.db;
  installed c body (fun json event team app channel ->
      Option.iter
        (fun (identity, _) ->
          let pending =
            Sql.int_query c.db
              "SELECT count(*) FROM slack_order_ingress WHERE installation=? \
               AND team=? AND app=? AND channel=? AND identity=? AND pause=1 \
               AND applied=0"
              (A.scope c.db ~team ~app ~channel @ [ Sql.text identity ])
            > 0
          in
          if pending then
            let exists =
              match
                Slack_standing_order_pause.parse_command (H.field "text" event)
              with
              | None -> true
              | Some n ->
                  Sql.int_query c.db
                    "SELECT count(*) FROM slack_standing_orders WHERE \
                     team_id=? AND api_app_id=? AND channel_id=? AND \
                     display_no=?"
                    Sql.[ text team; text app; text channel; int n ]
                  > 0
            in
            if exists then close c ~cause:"pause_unresolved" ~disconnect:false
            else A.applied c.db ~team ~app ~channel ~identity
          else ())
        (delivery_identity json event));
  if
    List.exists
      (fun (o : Slack_standing_order_types.order) ->
        o.state = Slack_standing_order_types.Active
        && not (R.covered c.db o.id o.epoch))
      (Slack_standing_order_store.list c.db ())
  then !enqueue ()

let begin_handler c ~body =
  let ticket = ref None in
  installed c body (fun json event team app channel ->
      Option.iter
        (fun (identity, _) ->
          ticket := A.start_handler c.db ~team ~app ~channel ~identity)
        (delivery_identity json event));
  fun ~success -> A.finish_handler c.db !ticket ~success

let own c cell fn =
  let previous = !cell and active = ref (Some (c, fn)) in
  let callback x =
    Option.iter (fun (c, fn) -> fn c x) !active;
    previous x
  in
  cell := callback;
  c.restore :=
    (fun () ->
      active := None;
      if !cell == callback then cell := previous)
    :: !(c.restore)

let configure ~db ~manager ~ctx =
  match !configured with
  | Some c when c.db == db && c.manager == manager && not c.stopped -> ()
  | Some _ -> invalid_arg "standing-order reconciler already configured"
  | None ->
      R.ensure_schema db;
      A.ensure_schema db;
      let limiter =
        match ctx.Ctx.limiter with
        | Some l -> l
        | None -> Slack_standing_order_slack.ensure_limiter ()
      in
      Slack_standing_order_slack.configure_limiter (Some limiter);
      let connected =
        match (Session.get_config manager).channels.slack with
        | Some sc -> not sc.socket_mode
        | None -> false
      in
      let c =
        {
          db;
          manager;
          ctx = { ctx with Ctx.limiter = Some limiter };
          connected;
          socket_mode = not connected;
          scopes = [];
          stopped = false;
          restore = ref [];
        }
      in
      ignore (scopes c);
      R.recover_startup db ~now:(ctx.now ());
      if connected then
        List.iter
          (fun scope ->
            Option.iter
              (fun token ->
                if not (R.mark_connected db ~token) then
                  failwith "HTTP ingress initialization failed")
              (token c scope))
          c.scopes;
      let active = ref (Some c) in
      let with_active fn = Option.iter fn !active in
      c.restore := [ (fun () -> active := None) ];
      own c Slack_socket.on_connected (fun c () ->
          c.connected <- true;
          List.iter
            (fun scope ->
              Option.iter
                (fun token ->
                  if not (R.mark_connected c.db ~token) then
                    failwith "hello persistence failed")
                (token c scope))
            (scopes c);
          !enqueue ());
      let gap ~cause =
        with_active (fun c ->
            close c ~cause
              ~disconnect:
                (List.mem cause
                   [
                     "hello";
                     "disconnect";
                     "transport_closed";
                     "transport_failure";
                     "connection_failure";
                     "socket_stopped";
                   ]))
      in
      let previous_gap = !Slack_socket.on_gap in
      let gap_callback ~cause =
        gap ~cause;
        previous_gap ~cause
      in
      Slack_socket.on_gap := gap_callback;
      c.restore :=
        (fun () ->
          if !Slack_socket.on_gap == gap_callback then
            Slack_socket.on_gap := previous_gap)
        :: !(c.restore);
      let old_begin = !Slack_thread_ingress.on_ingress_begin in
      let begin_ ~body =
        let finish =
          match !active with
          | None -> fun ~success:_ -> ()
          | Some c -> begin_handler c ~body
        in
        let previous =
          try old_begin ~body
          with exn ->
            finish ~success:false;
            raise exn
        in
        fun ~success ->
          finish ~success;
          previous ~success
      in
      Slack_thread_ingress.on_ingress_begin := begin_;
      c.restore :=
        (fun () ->
          if !Slack_thread_ingress.on_ingress_begin == begin_ then
            Slack_thread_ingress.on_ingress_begin := old_begin)
        :: !(c.restore);
      let old_admit = !Slack_thread_ingress.on_ingress_admit
      and old_applied = !Slack_thread_ingress.on_ingress_applied
      and old_failure = !Slack_thread_ingress.on_ingress_failure in
      let reserve ~body =
        try
          with_active (fun c -> admit c ~body);
          old_admit ~body
        with exn ->
          with_active (fun c ->
              close c ~cause:"ingress_admission_failure" ~disconnect:false);
          raise exn
      in
      let apply ~body =
        try
          with_active (fun c -> applied c ~body);
          old_applied ~body
        with exn ->
          with_active (fun c ->
              close c ~cause:"ingress_apply_failure" ~disconnect:false);
          raise exn
      in
      let failure ~cause =
        with_active (fun c -> close c ~cause ~disconnect:false);
        old_failure ~cause
      in
      Slack_thread_ingress.on_ingress_admit := reserve;
      Slack_thread_ingress.on_ingress_applied := apply;
      Slack_thread_ingress.on_ingress_failure := failure;
      c.restore :=
        (fun () ->
          if !Slack_thread_ingress.on_ingress_admit == reserve then
            Slack_thread_ingress.on_ingress_admit := old_admit;
          if !Slack_thread_ingress.on_ingress_applied == apply then
            Slack_thread_ingress.on_ingress_applied := old_applied;
          if !Slack_thread_ingress.on_ingress_failure == failure then
            Slack_thread_ingress.on_ingress_failure := old_failure)
        :: !(c.restore);
      own c Session_management.on_config_published (fun c () ->
          let socket_mode =
            match (Session.get_config c.manager).channels.slack with
            | Some sc -> sc.socket_mode
            | None -> true
          in
          if socket_mode <> c.socket_mode then (
            c.socket_mode <- socket_mode;
            c.connected <- not socket_mode);
          close c ~cause:"config_publication" ~disconnect:false;
          Slack_standing_order_rollout_store.invalidate_identity c.db;
          List.iter
            (fun (_, _, channel) ->
              Slack_standing_order_schedule.dirty_channel ~db:c.db ~channel)
            (scopes c));
      let old_exec = !Slack_standing_order_exec.on_gap in
      let exec_gap ~cause =
        with_active (fun c -> close c ~cause ~disconnect:false);
        old_exec ~cause
      in
      Slack_standing_order_exec.on_gap := exec_gap;
      Slack_standing_order_exec.recovery_managed := true;
      c.restore :=
        (fun () ->
          if !Slack_standing_order_exec.on_gap == exec_gap then (
            Slack_standing_order_exec.on_gap := old_exec;
            Slack_standing_order_exec.recovery_managed := false))
        :: !(c.restore);
      configured := Some c

let stop () =
  match !configured with
  | None -> ()
  | Some c ->
      Fun.protect
        ~finally:(fun () ->
          c.stopped <- true;
          List.iter (fun restore -> restore ()) !(c.restore);
          c.restore := [];
          configured := None)
        (fun () -> close c ~cause:"reconciler_stopped" ~disconnect:true)
