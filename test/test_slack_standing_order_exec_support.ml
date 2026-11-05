(* Executor fixtures: in-memory state, injected HTTP and the shared store clock. *)
module S = Test_slack_standing_order_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module L = Slack_huddle_ledger
module F = Test_slack_standing_order_provenance

let set = F.set
let str = F.str
let obj = F.obj
let get = F.get
let start = 1700000001
let root_ts = "1700000001.000100"
let ready_ts = "1700000060.000100"
let ended = 1700000030.

type env = {
  db : Sqlite3.db;
  manager : Session.t;
  order : T.order;
  mock : S.mock;
  accepting : bool ref;
  limits : int ref;
  root : Yojson.Safe.t ref;
  replies : Yojson.Safe.t ref;
  canvas : Yojson.Safe.t ref;
  transcript : Yojson.Safe.t ref;
}

let with_env f =
  let db = S.fresh_db () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      L.ensure_schema db;
      let order = S.seed db () in
      let c =
        Store.insert_card db ~kind:T.Pause_button ~order ~created_by:"U123"
          ~now:S.now ()
      in
      Store.set_card_message_ts db ~card_id:c.id "1700000000.000200";
      let mock = S.mock () in
      let root =
        ref
          (set "ts" (str root_ts)
             (set "room"
                (set "date_start" (`Int start)
                   (set "canvas_thread_ts" (str root_ts)
                      (set "thread_root_ts" (str root_ts) F.room)))
                F.root))
      in
      let ready = set "thread_ts" (str root_ts) F.ready in
      let replies = ref (F.page [ !root; ready ]) in
      let canvas =
        ref
          (set "shares"
             (obj
                [
                  ( "private",
                    obj
                      [ ("C123", `List [ F.share root_ts; F.share ready_ts ]) ]
                  );
                ])
             F.canvas)
      in
      let transcript =
        ref
          (set "huddle_transcription"
             (set "date_start" (`Int start)
                (get "huddle_transcription" F.transcript))
             F.transcript)
      in
      S.override mock "conversations.history" (fun _ ->
          (200, set "ok" (`Bool true) (F.page [ !root ])));
      S.override mock "conversations.replies" (fun _ ->
          (200, set "ok" (`Bool true) !replies));
      S.override mock "files.info" (fun p ->
          ( 200,
            S.ok
              [
                ( "file",
                  if get "file" p = str "FCANVAS" then !canvas else !transcript
                );
              ] ));
      S.override mock "canvases.getContent" (fun _ ->
          (200, S.ok [ ("content", str "private meeting notes") ]));
      let accepting = ref true and limits = ref 0 in
      let manager = Session.create ~config:(S.config ()) () in
      S.at (ended +. 10.);
      f
        {
          db;
          manager;
          order;
          mock;
          accepting;
          limits;
          root;
          replies;
          canvas;
          transcript;
        })

let ctx e =
  Slack_standing_order_ctx.make
    ~accepting:(fun () -> !(e.accepting))
    ~limit:(fun ~channel_id ->
      Alcotest.(check string) "paced channel" "C123" channel_id;
      incr e.limits;
      Lwt.return_unit)
    ()

let seen e =
  let room =
    Option.get (Slack_huddle_classify.room_of_json (get "room" !(e.root)))
  in
  L.record_seen ~db:e.db ~team:"T123" ~channel:"C123" ~source:"event"
    ~now:(Store.now ()) ~root_ts ~file_ids:[ "FCANVAS" ] room

let ready e ?(ts = ready_ts) () =
  ignore
    (L.record_notes_ready ~db:e.db ~team:"T123" ~channel:"C123" ~source:"event"
       ~now:(Store.now ()) ~thread_ts:root_ts ~ts ~file_ids:[ "FCANVAS" ]
       ~canvas_file_id:"FCANVAS" ~variant:Slack_huddle_classify.Notes_only)

let enable_transcript e =
  e.canvas := set "huddle_transcript_file_id" (str "FTRANS") !(e.canvas)

let fires e = Store.list_fires e.db ~order_id:e.order.id ~limit:100

let one_fire e =
  match fires e with [ f ] -> f | _ -> Alcotest.fail "expected one fire"

let runtime e id = Option.get (Store.runtime e.db ~fire_id:id)

let count e name =
  List.length (List.filter (( = ) name) (S.methods_called e.mock))

let no_upload e =
  List.iter
    (fun name -> Alcotest.(check int) name 0 (count e name))
    [
      "files.getUploadURLExternal";
      "files.completeUploadExternal";
      "chat.postMessage";
    ]

let resolve e =
  Lwt_main.run
    (Slack_standing_order_exec.resolve_pending ~ctx:(ctx e) ~db:e.db
       ~manager:e.manager ~http:e.mock.http e.order)

let with_claim e id f =
  let lease =
    match
      Store.claim e.db ~fire_id:id ~owner:"executor-test" ~now:(Store.now ())
    with
    | Ok (Some l) -> l
    | _ -> Alcotest.fail "claim failed"
  in
  Fun.protect
    ~finally:(fun () ->
      Store.release e.db ~fire_id:id ~lease ~now:(Store.now ()))
    (fun () -> f lease (runtime e id))

let fetch e id =
  with_claim e id (fun lease r ->
      match
        Lwt_main.run
          (Slack_standing_order_exec.fetch ~ctx:(ctx e) ~db:e.db
             ~manager:e.manager ~http:e.mock.http ~lease r)
      with
      | Ok v -> v
      | Error code -> Alcotest.fail ("fetch: " ^ code))

let check_state e id expected =
  Alcotest.check S.fire_state_t "fire state" expected (runtime e id).fire.state
