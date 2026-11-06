module Sql = Slack_standing_order_sql
(** Strict retained automatic-delivery inventory, shared by import and durable
    readiness. Never use order/fire decoders which may omit malformed rows. *)

type delivery = {
  order_id : string;
  root_ts : string;
  room_id : string;
  date_start : string;
  channel_id : string;
  identity : string list * string option;
  relay_ts : string;
  files : string list;
}

let sorted = List.sort_uniq String.compare

let of_json j =
  let open Yojson.Safe.Util in
  let s key = member key j |> to_string in
  let identity =
    match member "identity" j with
    | `List [ team; app; enterprise; user; bot ] ->
        ( [ to_string team; to_string app; to_string user; to_string bot ],
          to_string_option enterprise )
    | _ -> failwith "invalid delivery identity"
  in
  {
    order_id = s "order_id";
    root_ts = s "root_ts";
    room_id = s "room_id";
    date_start = s "date_start";
    channel_id = s "channel_id";
    identity;
    relay_ts = s "relay_ts";
    files = member "own_file_ids" j |> to_list |> List.map to_string;
  }

let automatic_match db (r : delivery) =
  let rows =
    Sql.query db
      "WITH retained AS (SELECT fire_id,attempt_no,relay_ts,uploaded_file_ids \
       FROM slack_standing_order_dispatches WHERE dispatched_at IS NOT NULL OR \
       relay_ts IS NOT NULL OR uploaded_file_ids<>'[]' UNION ALL SELECT \
       f.id,0,f.relay_ts,f.uploaded_file_ids FROM slack_standing_order_fires f \
       WHERE (f.dispatched_at IS NOT NULL OR f.relay_ts IS NOT NULL OR \
       f.uploaded_file_ids<>'[]') AND NOT EXISTS (SELECT 1 FROM \
       slack_standing_order_dispatches d WHERE d.fire_id=f.id)) SELECT \
       d.fire_id,d.attempt_no,d.relay_ts,d.uploaded_file_ids, \
       f.order_id,f.root_ts,f.room_id,f.date_start,o.channel_id, \
       o.team_id,o.api_app_id,o.bot_user_id,o.bot_id,o.enterprise_id FROM \
       retained d LEFT JOIN slack_standing_order_fires f ON f.id=d.fire_id \
       LEFT JOIN slack_standing_orders o ON o.id=f.order_id"
      [] (fun s ->
        ( Sql_util.text_column s 0,
          Sql_util.int_column s 1,
          List.init 12 (fun i -> Sql_util.opt_text_column s (i + 2)) ))
  in
  let conflicting_instance =
    Sql.query db
      "SELECT root_ts,room_id,date_start FROM slack_standing_order_fires WHERE \
       order_id=? AND (root_ts=? OR (room_id=? AND date_start=?))"
      Sql.[ text r.order_id; text r.root_ts; text r.room_id; text r.date_start ]
      (fun s ->
        ( Sql_util.opt_text_column s 0,
          Sql_util.text_column s 1,
          Sql_util.text_column s 2 ))
    |> List.exists (fun (root, room, date) ->
        root <> Some r.root_ts || room <> r.room_id || date <> r.date_start)
  in
  let conflict = ref conflicting_instance
  and matches = ref []
  and same_instance_seen = ref false in
  List.iter
    (fun (id, n, fields) ->
      match fields with
      | [
       ts;
       Some raw;
       order;
       root;
       room;
       date;
       channel;
       team;
       app;
       user;
       bot;
       enterprise;
      ] -> (
          let files =
            try
              Some
                (Yojson.Safe.from_string raw
                |> Yojson.Safe.Util.to_list
                |> List.map Yojson.Safe.Util.to_string)
            with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> None
          in
          match files with
          | None -> conflict := true
          | Some files ->
              (* Orphan evidence cannot establish absence or installation scope. *)
              if order = None || channel = None then conflict := true
              else if
                order = Some r.order_id
                && (root = Some r.root_ts
                   || (room = Some r.room_id && date = Some r.date_start))
                || (channel = Some r.channel_id && ts = Some r.relay_ts)
                || List.exists (fun f -> List.mem f r.files) files
              then
                let identity, ent = r.identity in
                if
                  order = Some r.order_id && root = Some r.root_ts
                  && room = Some r.room_id && date = Some r.date_start
                  && channel = Some r.channel_id
                  && [ team; app; user; bot ] = List.map Option.some identity
                  && enterprise = ent
                then (
                  same_instance_seen := true;
                  if
                    ts = Some r.relay_ts
                    && sorted files = sorted r.files
                    && List.length files = List.length r.files
                  then matches := (id, n) :: !matches
                  else if
                    (channel = Some r.channel_id && ts = Some r.relay_ts)
                    || List.exists (fun f -> List.mem f r.files) files
                  then conflict := true)
                else conflict := true)
      | _ -> conflict := true)
    rows;
  if !conflict then Error ()
  else
    match !matches with
    | [] when not !same_instance_seen -> Ok None
    | [] -> Error ()
    | [ link ] -> Ok (Some link)
    | _ -> Error ()

let link_valid db json stored =
  match automatic_match db (of_json json) with
  | Ok link -> link = stored
  | Error () -> false
