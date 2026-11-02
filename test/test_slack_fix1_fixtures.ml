open Lwt.Syntax
include Test_slack_huddle_fixtures
include Test_slack_threads

let key = "slack:C123:thread:T123:A123:100.000001"
let route = Option.get (Slack_thread_route.of_key key)
let ignore_error f = Lwt.catch f (fun _ -> Lwt.return_unit)

let text model content =
  Provider.Text
    {
      model;
      content;
      usage = None;
      thinking = None;
      provider_response_items_json = None;
    }

let with_recording ?delay f =
  with_ingress ?delay (fun base calls handler seen provider run ->
      let inherited = Option.get (Lwt.get Slack_transport.key) in
      let credentials = ref [] in
      let record uri headers =
        credentials :=
          !credentials
          @ [
              ( Filename.basename (Uri.path (Uri.of_string uri)),
                List.assoc_opt "Authorization" headers );
            ]
      in
      Slack_transport.with_http
        {
          get =
            (fun ~uri ~headers ->
              record uri headers;
              inherited.get ~uri ~headers);
          post =
            (fun ~uri ~headers ~body ->
              record uri headers;
              inherited.post ~uri ~headers ~body);
        }
        (fun () -> f base calls credentials handler seen provider run))

let reload_slack base change =
  let cfg = base.manager.config in
  let cfg =
    {
      cfg with
      channels =
        { cfg.channels with slack = Option.map change cfg.channels.slack };
    }
  in
  base.cfg := cfg;
  Session.update_config base.manager cfg

let deny_egress base room =
  let parsed =
    Config_loader.parse_config ~resolve_secrets:false
      (obj
         [
           ( "access_bundles",
             arr
               [
                 obj
                   [
                     ("id", str "deny-slack");
                     ( "egress_rules",
                       arr
                         [
                           obj
                             [
                               ("host", str "slack.com");
                               ("path", str "/api/*");
                               ("action", str "deny");
                             ];
                         ] );
                   ];
               ] );
           ( "access_scopes",
             arr
               [
                 obj
                   [
                     ("id", str "deny-thread");
                     ("level", str "room");
                     ("room", str room);
                     ("access_bundle_ids", arr [ str "deny-slack" ]);
                   ];
               ] );
         ])
  in
  let cfg =
    {
      base.manager.config with
      access_bundles = parsed.access_bundles;
      access_scopes = parsed.access_scopes;
    }
  in
  base.cfg := cfg;
  Session.update_config base.manager cfg

let writes calls =
  List.filter
    (fun (m, _) ->
      List.mem m
        [
          "chat.postMessage";
          "chat.update";
          "chat.delete";
          "chat.startStream";
          "chat.stopStream";
          "agents.sessions.setStatus";
          "agents.sessions.rename";
        ])
    !calls

let origin_task () =
  let origin =
    Room_origin.make ~connector:"slack" ~workspace_id:"T123" ~room_id:"C123"
      ~requester_id:"U123" ~source_message_id:"100.000001"
      ~thread_id:"100.000001" ()
  in
  {
    (Test_daemon.make_test_task ()) with
    session_key = Some key;
    channel = Some "slack";
    channel_id = Some "C123";
    thread_id = Some "100.000001";
    origin_json = Some (Room_origin.to_json_string origin);
    agent_name = Some "fixture";
    use_worktree = false;
  }

let queued_count manager =
  Hashtbl.fold
    (fun _ qs n -> n + List.length qs)
    manager.Session.queued_messages 0

let live_runs manager =
  Hashtbl.to_seq_values Slack_thread_runs.runs
  |> List.of_seq
  |> List.filter (fun (r : Slack_thread_runs.run) -> r.manager == manager)

let groups manager =
  List.filter (fun (m, _, _) -> m == manager) !Slack_thread_runs.groups
