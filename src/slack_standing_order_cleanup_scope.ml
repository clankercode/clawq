module SO = Slack_standing_order_slack
module C = Slack_standing_order_cleanup_store
module Pages = Slack_standing_order_pages

let note_text =
  "Clawq removed its relay files because the source was deleted. Clawq cannot \
   recall Anthropic's copy."

let marker ~job ~thread =
  "clawq-cleanup/1:" ^ Digest.to_hex (Digest.string (job ^ ":" ^ thread))

let note_params ~(channel : string) ~job ~thread =
  [
    ("channel", `String channel);
    ("thread_ts", `String thread);
    ("text", `String note_text);
    ("unfurl_links", `Bool false);
    ("unfurl_media", `Bool false);
    ( "blocks",
      `List
        [
          `Assoc
            [
              ("type", `String "section");
              ("block_id", `String (marker ~job ~thread));
              ( "text",
                `Assoc
                  [
                    ("type", `String "plain_text"); ("text", `String note_text);
                  ] );
            ];
        ] );
  ]

let matches ~(job : C.job) ~channel ~thread message =
  Pages.field "user" message = job.own_user
  && Pages.field "bot_id" message = job.own_bot
  && Pages.field "thread_ts" message = thread
  && (Pages.get "channel" message = `Null
     || Pages.get "channel" message = `String channel)
  && Pages.field "text" message = note_text
  &&
  match Pages.get "blocks" message with
  | `List [ block ] -> (
      let text = Pages.get "text" block in
      Pages.field "type" block = "section"
      && Pages.field "block_id" block = marker ~job:job.id ~thread
      && Pages.field "type" text = "plain_text"
      && Pages.field "text" text = note_text
      &&
      match Pages.get "emoji" text with
      | `Null | `Bool _ -> true
      | _ -> false)
  | _ -> false

let unique params =
  List.length params
  = List.length (List.sort_uniq String.compare (List.map fst params))

let scoped ~db ~(job : C.job) ~channel (call : SO.call) : SO.call =
 fun method_ params ->
  let value key = Option.value (List.assoc_opt key params) ~default:`Null in
  let exact_file allowed =
    match params with
    | [ ("file", `String id) ] -> List.mem id allowed
    | _ -> false
  in
  let missing =
    Option.fold ~none:false
      ~some:(fun (j : C.job) -> j.missing)
      (C.find db job.id)
  in
  let permitted =
    unique params
    &&
    match method_ with
    | "files.info" -> exact_file (C.sources job)
    | "files.delete" -> missing && exact_file (List.map fst (C.files db job.id))
    | "conversations.replies" ->
        value "channel" = `String channel
        && List.mem
             (Slack_huddles.text (value "ts"))
             (List.map fst (C.threads db job.id))
        && List.for_all
             (fun (k, _) ->
               List.mem k
                 [ "channel"; "ts"; "cursor"; "limit"; "inclusive"; "latest" ])
             params
    | "chat.postMessage" ->
        let thread = Slack_huddles.text (value "thread_ts") in
        missing
        && List.for_all
             (fun (_, status) -> status = "removed")
             (C.files db job.id)
        && List.mem thread (List.map fst (C.threads db job.id))
        && List.sort compare params
           = List.sort compare (note_params ~channel ~job:job.id ~thread)
    | _ -> false
  in
  if permitted then call method_ params
  else Lwt.return (Error (SO.Method_not_permitted method_))
