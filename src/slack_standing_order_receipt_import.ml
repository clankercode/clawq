module Sql = Slack_standing_order_sql
(** Offline reviewed metadata import. No credentials, Slack calls, executor
    debits, fabricated nonce or pickup. Preview never initializes schema. *)

module R = Slack_standing_order_rollout_store
module T = Slack_standing_order_types

type import_error =
  | Invalid_format
  | Invalid_installation
  | Invalid_identity
  | Invalid_scope
  | Invalid_evidence
  | Conflicting_delivery
  | Incomplete_inventory
  | Invalid_proof
  | Storage_error

type external_receipt = {
  schema_version : int;
  installation : string;
  identity : R.identity;
  order_id : string;
  channel_id : string;
  root_ts : string;
  room_id : string;
  date_start : string;
  relay_ts : string;
  own_file_ids : string list;
  source_canvas_id : string option;
  source_transcript_id : string option;
  evidence_digest : string;
  reviewer : string;
}

type activation_scope = {
  order_id : string;
  epoch : int;
  active_since : string option;
  activation_evidence : string;
}

type disposition =
  | Delivered of string
  | Out_of_scope of { entry_digest : string; reason : string }

type inventory = {
  schema_version : int;
  installation : string;
  identity : R.identity;
  bootstrap_scope_digest : string;
  reviewer : string;
  scope_review : string;
  orders : activation_scope list;
  entries : string list;
  dispositions : disposition list;
}

type validated_receipt =
  | Delivery of external_receipt
  | Complete_inventory of inventory

type change =
  | Suppress of string
  | Link_automatic of string * string * int
  | Already_imported of string
  | Complete_scope of string list

type audit_receipt = {
  inventory_digest : string;
  item_dispositions : disposition list;
  installation : string;
  schema_version : int;
  order_ids : string list;
}

let error_name = function
  | Invalid_format -> "invalid_format"
  | Invalid_installation -> "invalid_installation"
  | Invalid_identity -> "invalid_identity"
  | Invalid_scope -> "invalid_scope"
  | Invalid_evidence -> "invalid_evidence"
  | Conflicting_delivery -> "conflicting_delivery"
  | Incomplete_inventory -> "incomplete_inventory"
  | Invalid_proof -> "invalid_proof"
  | Storage_error -> "storage_error"

exception Refused of import_error

let refuse e = raise (Refused e)

let protect fn =
  try Ok (fn ()) with
  | Refused e -> Error e
  | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ | Invalid_argument _ ->
      Error Invalid_format
  | exn when Sql.is_db_error exn -> Error Storage_error

let str = R.json_string
let strings xs = `List (List.map str xs)
let sorted = List.sort_uniq String.compare

let valid_digest s =
  String.length s = 64
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) s

let valid_text s =
  String.length (String.trim s) > 0
  && String.length s <= 500
  && not (String.contains s '\000')

let receipt_payload (r : external_receipt) =
  `Assoc
    [
      ("schema_version", `Int r.schema_version);
      ("installation", str r.installation);
      ("identity", R.identity_json r.identity);
      ("order_id", str r.order_id);
      ("channel_id", str r.channel_id);
      ("root_ts", str r.root_ts);
      ("room_id", str r.room_id);
      ("date_start", str r.date_start);
      ("relay_ts", str r.relay_ts);
      ("own_file_ids", strings (sorted r.own_file_ids));
      ("source_canvas_id", R.opt_json r.source_canvas_id);
      ("source_transcript_id", R.opt_json r.source_transcript_id);
    ]

let receipt_json (r : external_receipt) =
  match receipt_payload r with
  | `Assoc fields ->
      `Assoc
        (fields
        @ [
            ("evidence_digest", str r.evidence_digest);
            ("reviewer", str r.reviewer);
          ])
  | _ -> assert false

let receipt_digest r = R.digest (Yojson.Safe.to_string (receipt_payload r))

let disposition_json = function
  | Delivered d -> `Assoc [ ("entry", str d); ("disposition", str "delivered") ]
  | Out_of_scope { entry_digest; reason } ->
      `Assoc
        [
          ("entry", str entry_digest);
          ("disposition", str "out_of_scope");
          ("reason", str reason);
        ]

let entry = function
  | Delivered d -> d
  | Out_of_scope { entry_digest; _ } -> entry_digest

let scope_json (s : activation_scope) =
  `Assoc
    [
      ("order_id", str s.order_id);
      ("epoch", `Int s.epoch);
      ("active_since", R.opt_json s.active_since);
      ("activation_evidence", str s.activation_evidence);
    ]

let inventory_json (m : inventory) =
  `Assoc
    [
      ("schema_version", `Int m.schema_version);
      ("installation", str m.installation);
      ("identity", R.identity_json m.identity);
      ("bootstrap_scope_digest", str m.bootstrap_scope_digest);
      ("reviewer", str m.reviewer);
      ("scope_review", str m.scope_review);
      ("orders", `List (List.map scope_json m.orders));
      ("entries", strings m.entries);
      ("dispositions", `List (List.map disposition_json m.dispositions));
    ]

let inventory_digest m = R.digest (Yojson.Safe.to_string (inventory_json m))

let audit_json (a : audit_receipt) =
  `Assoc
    [
      ("inventory_digest", str a.inventory_digest);
      ("installation", str a.installation);
      ("schema_version", `Int a.schema_version);
      ("order_ids", strings a.order_ids);
      ( "item_dispositions",
        `List (List.map disposition_json a.item_dispositions) );
    ]

let installation db uuid =
  let actual, _, _, _, _, _ = R.validate_proof db in
  if uuid <> actual then refuse Invalid_installation

let valid_date s =
  s <> ""
  && String.for_all (function '0' .. '9' -> true | _ -> false) s
  && s.[0] <> '0'

let order_metadata db order_id =
  match
    Sql.query db
      "SELECT \
       team_id,api_app_id,enterprise_id,channel_id,epoch,active_since,bot_user_id,bot_id \
       FROM slack_standing_orders WHERE id=?"
      [ Sql.text order_id ]
      (fun s ->
        ( Sql_util.text_column s 0,
          Sql_util.text_column s 1,
          Sql_util.opt_text_column s 2,
          Sql_util.text_column s 3,
          Sql_util.int_column s 4,
          Sql_util.opt_text_column s 5,
          Sql_util.opt_text_column s 6,
          Sql_util.opt_text_column s 7 ))
  with
  | [ o ] -> o
  | _ -> refuse Invalid_scope

let check_identity db order_id channel (i : R.identity) =
  if not (R.valid_identity i) then refuse Invalid_identity;
  let team, app, enterprise, ch, _, _, user, bot = order_metadata db order_id in
  if
    team <> i.team_id || app <> i.app_id
    || enterprise <> i.enterprise_id
    || ch <> channel
    || (user <> None && user <> Some i.own_user_id)
    || (bot <> None && bot <> Some i.own_bot_id)
  then refuse Invalid_identity;
  let _, _, _, bound, _, _ = R.validate_proof db in
  if bound <> None && bound <> Some (R.identity_text i) then
    refuse Invalid_identity

let automatic_match db (r : external_receipt) =
  let fires =
    Sql.query db
      "SELECT id,root_ts,room_id,date_start FROM slack_standing_order_fires \
       WHERE order_id=? AND (root_ts=? OR (room_id=? AND date_start=?))"
      Sql.[ text r.order_id; text r.root_ts; text r.room_id; text r.date_start ]
      (fun s ->
        ( Sql_util.text_column s 0,
          Sql_util.opt_text_column s 1,
          Sql_util.text_column s 2,
          Sql_util.text_column s 3 ))
  in
  List.iter
    (fun (_, root, room, date) ->
      if root <> Some r.root_ts || room <> r.room_id || date <> r.date_start
      then refuse Conflicting_delivery)
    fires;
  let attempts =
    List.concat_map
      (fun (id, _, _, _) ->
        Sql.query db
          "SELECT attempt_no,relay_ts,uploaded_file_ids FROM \
           slack_standing_order_dispatches WHERE fire_id=? AND dispatched_at \
           IS NOT NULL"
          [ Sql.text id ]
          (fun s ->
            ( id,
              Sql_util.int_column s 0,
              Sql_util.opt_text_column s 1,
              Sql_util.text_column s 2 )))
      fires
  in
  let matches =
    List.filter
      (fun (_, _, ts, files) ->
        let files =
          Yojson.Safe.Util.to_list (Yojson.Safe.from_string files)
          |> List.map Yojson.Safe.Util.to_string
        in
        ts = Some r.relay_ts && sorted files = sorted r.own_file_ids)
      attempts
  in
  match (matches, attempts) with
  | [ (id, n, _, _) ], _ -> Some (id, n)
  | [], [] -> None
  | _ -> refuse Conflicting_delivery

let validate db ~(receipt : external_receipt) =
  protect (fun () ->
      R.require_initialized db;
      let r = receipt in
      installation db r.installation;
      let valid = Slack_config_types.valid_id in
      if
        r.schema_version <> 1
        || (not (valid "CG" r.channel_id && valid "R" r.room_id))
        || (not
              (T.valid_slack_ts r.root_ts
              && T.valid_slack_ts r.relay_ts
              && valid_date r.date_start))
        || r.own_file_ids = []
        || List.length r.own_file_ids <> List.length (sorted r.own_file_ids)
        || (not (List.for_all (valid "F") r.own_file_ids))
        || (not
              (List.for_all
                 (Option.fold ~none:true ~some:(valid "F"))
                 [ r.source_canvas_id; r.source_transcript_id ]))
        || not (valid_text r.reviewer)
      then refuse Invalid_format;
      if
        (not (valid_digest r.evidence_digest))
        || r.evidence_digest <> receipt_digest r
      then refuse Invalid_evidence;
      check_identity db r.order_id r.channel_id r.identity;
      let existing =
        Sql.query db
          "SELECT receipt_digest FROM slack_standing_order_suppression WHERE \
           order_id=? AND (root_ts=? OR (room_id=? AND date_start=?))"
          Sql.
            [
              text r.order_id; text r.root_ts; text r.room_id; text r.date_start;
            ]
          (fun s -> Sql_util.text_column s 0)
      in
      if List.exists (( <> ) r.evidence_digest) existing then
        refuse Conflicting_delivery;
      let file_conflict =
        Sql.query db
          "SELECT metadata FROM slack_standing_order_external_deliveries WHERE \
           receipt_digest<>?"
          [ Sql.text r.evidence_digest ]
          (fun s -> Yojson.Safe.from_string (Sql_util.text_column s 0))
      in
      List.iter
        (fun json ->
          let ids =
            Yojson.Safe.Util.(
              json |> member "own_file_ids" |> to_list |> List.map to_string)
          in
          if List.exists (fun id -> List.mem id ids) r.own_file_ids then
            refuse Conflicting_delivery)
        file_conflict;
      ignore (automatic_match db r);
      Delivery r)

let validate_inventory db (m : inventory) =
  protect (fun () ->
      R.require_initialized db;
      installation db m.installation;
      if
        m.schema_version <> 1
        || (not (R.valid_identity m.identity))
        || not (valid_text m.reviewer && valid_text m.scope_review)
      then refuse Invalid_format;
      if
        m.bootstrap_scope_digest
        <> R.digest (Yojson.Safe.to_string (R.bootstrap_inventory db))
      then refuse Invalid_scope;
      if
        sorted m.entries <> List.sort String.compare m.entries
        || (not (List.for_all valid_digest m.entries))
        || sorted (List.map entry m.dispositions) <> sorted m.entries
        || List.length m.dispositions <> List.length m.entries
      then refuse Incomplete_inventory;
      List.iter
        (function
          | Out_of_scope { reason; _ } when not (valid_text reason) ->
              refuse Invalid_evidence
          | _ -> ())
        m.dispositions;
      let actual =
        Sql.query db "SELECT id FROM slack_standing_orders ORDER BY id" []
          (fun s -> Sql_util.text_column s 0)
      in
      let initial =
        Yojson.Safe.Util.(
          R.bootstrap_inventory db |> member "orders" |> to_list
          |> List.map to_string)
      in
      let listed =
        List.map (fun (s : activation_scope) -> s.order_id) m.orders
      in
      if
        sorted listed <> actual
        || List.length listed <> List.length actual
        || not (List.for_all (fun id -> List.mem id listed) initial)
      then refuse Incomplete_inventory;
      List.iter
        (fun (s : activation_scope) ->
          let _, _, _, channel, epoch, active, _, _ =
            order_metadata db s.order_id
          in
          check_identity db s.order_id channel m.identity;
          let state =
            Sql.query db "SELECT state FROM slack_standing_orders WHERE id=?"
              [ Sql.text s.order_id ]
              (fun row -> Sql_util.text_column row 0)
          in
          if
            s.epoch > epoch || s.epoch < 0
            || (state = [ "active" ] && s.epoch <> epoch)
            || active <> s.active_since
            || (not
                  (Option.fold ~none:(s.epoch = 0) ~some:T.valid_slack_ts
                     s.active_since))
            || not (valid_digest s.activation_evidence)
          then refuse Invalid_scope)
        m.orders;
      Complete_inventory m)

let preview db receipts =
  protect (fun () ->
      R.require_initialized db;
      List.concat_map
        (function
          | Delivery r -> (
              (match validate db ~receipt:r with
              | Error e -> refuse e
              | Ok _ -> ());
              let existing =
                Sql.int_query db
                  "SELECT count(*) FROM \
                   slack_standing_order_external_deliveries WHERE \
                   receipt_digest=?"
                  [ Sql.text r.evidence_digest ]
              in
              if existing = 1 then [ Already_imported r.evidence_digest ]
              else
                match automatic_match db r with
                | Some (id, n) -> [ Link_automatic (r.evidence_digest, id, n) ]
                | None -> [ Suppress r.evidence_digest ])
          | Complete_inventory m ->
              (match validate_inventory db m with
              | Error e -> refuse e
              | Ok _ -> ());
              [
                Complete_scope
                  (List.map (fun (s : activation_scope) -> s.order_id) m.orders);
              ])
        receipts)

let verify_rollout db ~manifest_digest =
  protect (fun () ->
      R.require_initialized db;
      let uuid, state, _, identity, _, manifest = R.validate_proof db in
      if state <> "inventory_verified" || manifest <> Some manifest_digest then
        refuse Incomplete_inventory;
      if not (R.inventory_complete db uuid manifest_digest) then
        refuse Invalid_proof;
      match
        Sql.query db
          "SELECT manifest,audit FROM slack_standing_order_inventory_manifests \
           WHERE digest=? AND installation=?"
          Sql.[ text manifest_digest; text uuid ]
          (fun s -> (Sql_util.text_column s 0, Sql_util.text_column s 1))
      with
      | [ (raw, audit) ] when R.digest raw = manifest_digest ->
          let j = Yojson.Safe.from_string raw in
          let open Yojson.Safe.Util in
          if identity <> Some (Yojson.Safe.to_string (member "identity" j)) then
            refuse Invalid_identity;
          let dispositions =
            member "dispositions" j |> to_list
            |> List.map (fun d ->
                let id = member "entry" d |> to_string in
                if member "disposition" d = `String "delivered" then (
                  if
                    Sql.int_query db
                      "SELECT count(*) FROM slack_standing_order_suppression s \
                       JOIN slack_standing_order_external_deliveries e ON \
                       e.receipt_digest=s.receipt_digest WHERE \
                       s.receipt_digest=? AND s.installation=?"
                      Sql.[ text id; text uuid ]
                    <> 1
                  then refuse Incomplete_inventory;
                  Delivered id)
                else
                  Out_of_scope
                    {
                      entry_digest = id;
                      reason = member "reason" d |> to_string;
                    })
          in
          let order_ids =
            member "orders" j |> to_list
            |> List.map (fun s -> member "order_id" s |> to_string)
          in
          let receipt =
            {
              inventory_digest = manifest_digest;
              item_dispositions = dispositions;
              installation = uuid;
              schema_version = 1;
              order_ids;
            }
          in
          if Yojson.Safe.to_string (audit_json receipt) <> audit then
            refuse Invalid_proof;
          receipt
      | _ -> refuse Invalid_proof)

let apply db receipts ~manifest_digest =
  let run () =
    let inventories =
      List.filter_map
        (function Complete_inventory m -> Some m | _ -> None)
        receipts
    in
    let m =
      match inventories with [ m ] -> m | _ -> refuse Incomplete_inventory
    in
    if inventory_digest m <> manifest_digest then refuse Invalid_evidence;
    let _, state, _, _, _, manifest = R.validate_proof db in
    if state = "inventory_verified" && manifest = Some manifest_digest then
      match verify_rollout db ~manifest_digest with
      | Ok a -> a
      | Error e -> refuse e
    else (
      if state <> "legacy_inventory_pending" then refuse Invalid_scope;
      (match preview db receipts with Error e -> refuse e | Ok _ -> ());
      let deliveries =
        List.filter_map (function Delivery r -> Some r | _ -> None) receipts
      in
      let files =
        List.concat_map
          (fun (r : external_receipt) -> r.own_file_ids)
          deliveries
      in
      let relays =
        List.map
          (fun (r : external_receipt) -> r.channel_id ^ ":" ^ r.relay_ts)
          deliveries
      in
      if
        List.length files <> List.length (sorted files)
        || List.length relays <> List.length (sorted relays)
      then refuse Conflicting_delivery;
      let expected =
        List.filter_map
          (function Delivered d -> Some d | _ -> None)
          m.dispositions
        |> sorted
      in
      if
        sorted
          (List.map
             (fun (r : external_receipt) -> r.evidence_digest)
             deliveries)
        <> expected
        || List.length deliveries <> List.length expected
      then refuse Incomplete_inventory;
      List.iter
        (fun (r : external_receipt) ->
          if r.identity <> m.identity || r.installation <> m.installation then
            refuse Invalid_identity;
          let automatic_fire, automatic_attempt =
            match automatic_match db r with
            | None -> (None, None)
            | Some (id, n) -> (Some id, Some n)
          in
          Sql.exec db
            "INSERT INTO slack_standing_order_external_deliveries \
             VALUES(?,?,?,?) ON CONFLICT(receipt_digest) DO NOTHING"
            Sql.
              [
                text r.evidence_digest;
                text (Yojson.Safe.to_string (receipt_json r));
                Sqlite3.Data.opt_text automatic_fire;
                Sqlite3.Data.opt_int automatic_attempt;
              ];
          Sql.exec db
            "INSERT INTO slack_standing_order_suppression VALUES(?,?,?,?,?,?) \
             ON CONFLICT(installation,order_id,root_ts,room_id,date_start) DO \
             NOTHING"
            Sql.
              [
                text r.installation;
                text r.order_id;
                text r.root_ts;
                text r.room_id;
                text r.date_start;
                text r.evidence_digest;
              ])
        deliveries;
      let identity = R.identity_text m.identity in
      let seq =
        R.append db ~kind:"inventory_completion" ~payload:(inventory_json m)
          ~state:"inventory_verified" ~identity:(Some identity)
          ~identity_valid:true ~manifest:(Some manifest_digest)
      in
      List.iter
        (fun (s : activation_scope) ->
          Sql.exec db
            "INSERT INTO slack_standing_order_order_bindings VALUES(?,?,?,?) \
             ON CONFLICT(order_id) DO UPDATE SET \
             identity=excluded.identity,seq=excluded.seq WHERE \
             installation=excluded.installation"
            Sql.[ text s.order_id; text m.installation; text identity; int seq ];
          Option.iter
            (fun active_since ->
              Sql.exec db
                "INSERT INTO slack_standing_order_activation_bindings \
                 VALUES(?,?,?,?,?) ON CONFLICT(order_id,epoch) DO UPDATE SET \
                 identity=excluded.identity,seq=excluded.seq WHERE \
                 active_since=excluded.active_since"
                Sql.
                  [
                    text s.order_id;
                    int s.epoch;
                    text active_since;
                    text identity;
                    int seq;
                  ])
            s.active_since)
        m.orders;
      let audit =
        {
          inventory_digest = manifest_digest;
          item_dispositions = m.dispositions;
          installation = m.installation;
          schema_version = 1;
          order_ids =
            List.map (fun (s : activation_scope) -> s.order_id) m.orders;
        }
      in
      Sql.exec db
        "INSERT INTO slack_standing_order_inventory_manifests VALUES(?,?,?,?)"
        Sql.
          [
            text manifest_digest;
            text m.installation;
            text (Yojson.Safe.to_string (inventory_json m));
            text (Yojson.Safe.to_string (audit_json audit));
          ];
      match verify_rollout db ~manifest_digest with
      | Ok a -> a
      | Error e -> refuse e)
  in
  (* Refusals are converted INSIDE the transaction so every partial write rolls
     back, including a failure immediately before manifest completion. *)
  match
    Sql.in_tx db ~on_error:(fun _ -> Storage_error) (fun () -> protect run)
  with
  | Ok a -> Ok a
  | Error e -> Error e

let identity_of_json j : R.identity =
  let open Yojson.Safe.Util in
  match j with
  | `List [ team; app; enterprise; user; bot ] ->
      {
        team_id = to_string team;
        app_id = to_string app;
        enterprise_id = to_string_option enterprise;
        own_user_id = to_string user;
        own_bot_id = to_string bot;
      }
  | _ -> refuse Invalid_format

let receipt_of_json j =
  let open Yojson.Safe.Util in
  let s k = member k j |> to_string
  and opt k = member k j |> to_string_option in
  {
    schema_version = member "schema_version" j |> to_int;
    installation = s "installation";
    identity = identity_of_json (member "identity" j);
    order_id = s "order_id";
    channel_id = s "channel_id";
    root_ts = s "root_ts";
    room_id = s "room_id";
    date_start = s "date_start";
    relay_ts = s "relay_ts";
    own_file_ids = member "own_file_ids" j |> to_list |> List.map to_string;
    source_canvas_id = opt "source_canvas_id";
    source_transcript_id = opt "source_transcript_id";
    evidence_digest = s "evidence_digest";
    reviewer = s "reviewer";
  }

let inventory_of_json j =
  let open Yojson.Safe.Util in
  let s k = member k j |> to_string in
  {
    schema_version = member "schema_version" j |> to_int;
    installation = s "installation";
    identity = identity_of_json (member "identity" j);
    bootstrap_scope_digest = s "bootstrap_scope_digest";
    reviewer = s "reviewer";
    scope_review = s "scope_review";
    orders =
      member "orders" j |> to_list
      |> List.map (fun o ->
          {
            order_id = member "order_id" o |> to_string;
            epoch = member "epoch" o |> to_int;
            active_since = member "active_since" o |> to_string_option;
            activation_evidence = member "activation_evidence" o |> to_string;
          });
    entries = member "entries" j |> to_list |> List.map to_string;
    dispositions =
      member "dispositions" j |> to_list
      |> List.map (fun d ->
          let entry_digest = member "entry" d |> to_string in
          match member "disposition" d with
          | `String "delivered" -> Delivered entry_digest
          | `String "out_of_scope" ->
              Out_of_scope
                { entry_digest; reason = member "reason" d |> to_string }
          | _ -> refuse Invalid_format);
  }

let validate_document db json =
  protect (fun () ->
      let open Yojson.Safe.Util in
      let manifest = inventory_of_json (member "inventory" json) in
      let validate_result = function Ok v -> v | Error e -> refuse e in
      let inventory = validate_result (validate_inventory db manifest) in
      let deliveries =
        member "receipts" json |> to_list
        |> List.map (fun j ->
            validate_result (validate db ~receipt:(receipt_of_json j)))
      in
      (inventory_digest manifest, inventory :: deliveries))
