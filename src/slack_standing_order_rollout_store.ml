include Slack_standing_order_rollout_schema
(** Installation evidence is independent of grant/network modules. Bootstrap is
    called with the existing schema installer inside ONE immediate transaction.
    Readiness never creates, migrates or repairs anything. *)

module T = Slack_standing_order_types

type rollout_state =
  | Unused_surface_unbound
  | Fresh_no_legacy_work
  | Legacy_inventory_pending
  | Inventory_verified

type rollout_denial =
  | Unbound_installation
  | Inventory_pending
  | Missing_proof
  | Invalid_proof
  | Identity_mismatch
  | Activation_unbound
  | Storage_error

type rollout_readiness = Ready of rollout_state | Denied of rollout_denial

type identity = {
  team_id : string;
  app_id : string;
  enterprise_id : string option;
  own_user_id : string;
  own_bot_id : string;
}

let state_name = function
  | Unused_surface_unbound -> "unused_surface_unbound"
  | Fresh_no_legacy_work -> "fresh_no_legacy_work"
  | Legacy_inventory_pending -> "legacy_inventory_pending"
  | Inventory_verified -> "inventory_verified"

let denial_name = function
  | Unbound_installation -> "unbound_installation"
  | Inventory_pending -> "inventory_pending"
  | Missing_proof -> "missing_proof"
  | Invalid_proof -> "invalid_proof"
  | Identity_mismatch -> "identity_mismatch"
  | Activation_unbound -> "activation_unbound"
  | Storage_error -> "storage_error"

let readiness_name = function
  | Ready s -> state_name s
  | Denied d -> denial_name d

let digest s = Digestif.SHA256.(digest_string s |> to_hex)
let json_string s = `String s
let opt_json = function None -> `Null | Some s -> `String s

let identity_json (i : identity) =
  `List
    [
      json_string i.team_id;
      json_string i.app_id;
      opt_json i.enterprise_id;
      json_string i.own_user_id;
      json_string i.own_bot_id;
    ]

let identity_text i = Yojson.Safe.to_string (identity_json i)

let valid_identity (i : identity) =
  let valid = Slack_config_types.valid_id in
  valid "T" i.team_id && valid "A" i.app_id && valid "UW" i.own_user_id
  && valid "B" i.own_bot_id
  && Option.fold ~none:true ~some:(valid "E") i.enterprise_id

(* Only feature-owned structural objects enter the fingerprint. Auxiliary P3
   scheduling/checkpoint tables remain lazily created and are inventoried, but
   are not installation proof components. *)
let fingerprint db =
  let required = base_names @ List.map fst tables in
  let objects =
    query db
      "SELECT name,tbl_name,type,sql FROM sqlite_master WHERE sql IS NOT NULL \
       ORDER BY name"
      [] (fun s ->
        ( Sql_util.text_column s 0,
          Sql_util.text_column s 1,
          Sql_util.text_column s 2,
          Sql_util.text_column s 3 ))
  in
  if not (List.for_all (table_exists db) required) then
    failwith "incomplete feature schema";
  objects
  |> List.filter (fun (n, t, k, _) ->
      List.mem t required
      && (k <> "trigger"
         || String.ends_with ~suffix:"_no_update" n
         || String.ends_with ~suffix:"_no_delete" n))
  |> List.map (fun (n, t, k, s) -> `List (List.map json_string [ n; t; k; s ]))
  |> fun xs -> digest (Yojson.Safe.to_string (`List xs))

let immutable db table =
  List.iter
    (fun op ->
      exec db
        ("CREATE TRIGGER " ^ table ^ "_no_" ^ String.lowercase_ascii op
       ^ " BEFORE " ^ op ^ " ON " ^ table
       ^ " BEGIN SELECT RAISE(ABORT,'immutable standing-order evidence'); END")
        [])
    [ "UPDATE"; "DELETE" ]

let inventory db =
  let ns = names db in
  let counts =
    List.filter work_table ns
    |> List.map (fun n ->
        (n, `Int (int_query db ("SELECT count(*) FROM \"" ^ n ^ "\"") [])))
  in
  let ambiguity = ref false in
  let plan_ids = ref [] in
  let plans =
    if not (List.mem "setup_plans" ns) then 0
    else
      query db "SELECT id,plan_json FROM setup_plans" [] (fun s ->
          (Sql_util.text_column s 0, Sql_util.text_column s 1))
      |> List.fold_left
           (fun count (id, raw) ->
             let applicable =
               try
                 let j = Yojson.Safe.from_string raw in
                 (match Setup_plan.of_persist_json j with
                 | Ok plan when plan.id = id -> ()
                 | _ -> failwith "ambiguous setup plan");
                 let open Yojson.Safe.Util in
                 match j |> member "apply_payload" |> member "kind" with
                 | `Assoc [ ("generic", `String k) ]
                   when starts "slack_standing_order" k ->
                     true
                 | `String k
                   when List.mem k
                          [
                            "room_profile";
                            "github_app_setup";
                            "github_route";
                            "access_bundle";
                          ] ->
                     false
                 | `Assoc [ ("generic", `String k) ]
                   when List.mem k
                          [
                            "github_merge";
                            "github_workflow_dispatch";
                            "github_request_reviewers";
                            "github_submit_review";
                            "github_code_work";
                            "github_pr_create";
                            "github_room_background_work";
                            "github_issue_create";
                            "github_issue_open";
                            "github_issue_close";
                            "github_issue_reopen";
                            "github_collab_action";
                          ] ->
                     false
                 | _ ->
                     ambiguity := true;
                     true
               with _ ->
                 ambiguity := true;
                 true
             in
             if applicable then (
               plan_ids := id :: !plan_ids;
               count + 1)
             else count)
           0
  in
  let audit =
    if not (List.mem "setup_plan_audit" ns) then 0
    else
      query db "SELECT plan_id FROM setup_plan_audit" [] (fun s ->
          Sql_util.text_column s 0)
      |> List.fold_left
           (fun n id ->
             if List.mem id !plan_ids then n + 1
             else if
               table_exists db "setup_plans"
               && int_query db "SELECT count(*) FROM setup_plans WHERE id=?"
                    [ text id ]
                  = 1
             then n
             else (
               ambiguity := true;
               n + 1))
           0
  in
  let snapshots =
    if not (List.mem "access_snapshots" ns) then 0
    else
      let refs =
        List.filter
          (fun c -> columns_present db "access_snapshots" [ c ])
          [ "standing_order_id"; "standing_order_fire_id" ]
      in
      int_query db
        ("SELECT count(*) FROM access_snapshots WHERE work_type IS NULL OR \
          work_type NOT IN \
          ('room_turn','background_task','ambient_work','github_trigger','routine')"
        ^ String.concat ""
            (List.map (fun c -> " OR " ^ c ^ " IS NOT NULL") refs))
        []
  in
  let present = List.filter (fun n -> List.mem_assoc n required_columns) ns in
  let recognized =
    (present = [] && not (List.exists work_table ns)) || base_shape db
  in
  let count_json =
    `Assoc
      (counts
      @ [
          ("setup_plans", `Int plans);
          ("setup_plan_audit", `Int audit);
          ("access_snapshots", `Int snapshots);
        ])
  in
  let empty =
    List.for_all
      (fun (_, v) -> v = `Int 0)
      (match count_json with `Assoc xs -> xs | _ -> [])
  in
  let order_scope =
    if
      table_exists db "slack_standing_orders"
      && columns_present db "slack_standing_orders" [ "id" ]
    then
      query db "SELECT id FROM slack_standing_orders ORDER BY id" [] (fun s ->
          json_string (Sql_util.text_column s 0))
    else []
  in
  let known_optional =
    [
      "slack_standing_order_reports";
      "slack_standing_order_observations";
      "slack_standing_order_fetch";
      "slack_order_root_schedule";
      "slack_order_ready_hints";
      "slack_order_ready_taken";
    ]
  in
  List.iter
    (fun (name, cols) ->
      if List.mem name ns && not (columns_present db name cols) then
        ambiguity := true)
    [
      ( "slack_standing_order_reports",
        columns "order_id cause_key period_key fire_id status message_ts" );
      ( "slack_standing_order_observations",
        columns "fire_id attempt_no team_id api_app_id payload observed_at" );
      ( "slack_standing_order_fetch",
        columns "fire_id notes_deadline notes_retries transient_retries" );
      ( "slack_order_root_schedule",
        columns
          "team app channel order_id epoch root metadata dirty_revision \
           inspected_revision next_probe_at retry_at retry_count retry_class \
           settled owner lease_until lease_epoch" );
      ( "slack_order_ready_hints",
        columns "id team app channel order_id epoch thread canvas ready" );
      ("slack_order_ready_taken", columns "fire_id hint_id");
    ];
  if
    List.exists
      (fun n -> work_table n && not (List.mem n (base_names @ known_optional)))
      ns
  then ambiguity := true;
  let fresh = recognized && empty && not !ambiguity in
  ( `Assoc
      [
        ("counts", count_json);
        ("orders", `List order_scope);
        ( "origin",
          json_string (if fresh then "fresh_no_legacy" else "legacy_inventory")
        );
        ( "reason",
          json_string
            (if not recognized then "partial_or_unknown_schema"
             else if !ambiguity then "ambiguous_setup_work"
             else if not empty then "existing_order_work"
             else "proven_unused") );
      ],
    fresh )

let evidence_digest ~installation ~seq ~at ~kind ~payload ~previous =
  digest
    (Yojson.Safe.to_string
       (`List
          [
            json_string installation;
            `Int 1;
            `Int seq;
            `Float at;
            json_string kind;
            json_string payload;
            json_string previous;
          ]))

let read_anchor db =
  match
    query db
      "SELECT version,installation,bootstrap_digest,fingerprint FROM \
       slack_standing_order_feature_version WHERE singleton=1"
      [] (fun s ->
        ( Sql_util.int_column s 0,
          Sql_util.text_column s 1,
          Sql_util.text_column s 2,
          Sql_util.text_column s 3 ))
  with
  | [ ((1, _, _, _) as a) ] -> a
  | _ -> failwith "invalid feature anchor"

let state_row db =
  match
    query db
      "SELECT state,seq,identity,identity_valid,manifest FROM \
       slack_standing_order_rollout_state WHERE singleton=1"
      [] (fun s ->
        ( Sql_util.text_column s 0,
          Sql_util.int_column s 1,
          Sql_util.opt_text_column s 2,
          Sql_util.sql_bool s 3,
          Sql_util.opt_text_column s 4 ))
  with
  | [ s ] -> s
  | _ -> failwith "missing rollout state"

let validate_proof db =
  let _, installation, bootstrap, fp = read_anchor db in
  if String.length installation <> 36 || fingerprint db <> fp then
    failwith "invalid installation schema";
  let state, seq, identity, valid, manifest = state_row db in
  let rows =
    query db
      "SELECT seq,installation,version,at,kind,payload,previous,digest FROM \
       slack_standing_order_rollout_evidence ORDER BY seq"
      [] (fun s ->
        ( Sql_util.int_column s 0,
          Sql_util.text_column s 1,
          Sql_util.int_column s 2,
          Sqlite3.column_double s 3,
          Sql_util.text_column s 4,
          Sql_util.text_column s 5,
          Sql_util.text_column s 6,
          Sql_util.text_column s 7 ))
  in
  let previous = ref ""
  and next = ref 1
  and final_payload = ref `Null
  and bootstrap_payload = ref `Null in
  List.iter
    (fun (n, uuid, v, at, kind, payload, prev, d) ->
      if
        n <> !next || uuid <> installation || v <> 1 || prev <> !previous
        || evidence_digest ~installation ~seq:n ~at ~kind ~payload
             ~previous:prev
           <> d
        || (n = 1 && (d <> bootstrap || kind <> "bootstrap"))
      then failwith "invalid evidence chain";
      let decoded = Yojson.Safe.from_string payload in
      if n = 1 then bootstrap_payload := decoded;
      final_payload := decoded;
      previous := d;
      incr next)
    rows;
  if seq <> !next - 1 || seq < 1 then failwith "invalid state sequence";
  let member = Yojson.Safe.Util.member in
  if
    member "state" !final_payload <> json_string state
    || member "identity" !final_payload
       <> Option.fold ~none:`Null ~some:json_string identity
    || member "manifest" !final_payload
       <> Option.fold ~none:`Null ~some:json_string manifest
    || member "fingerprint" !bootstrap_payload <> json_string fp
  then failwith "state disagrees with evidence";
  let inventory = member "inventory" !bootstrap_payload in
  if List.mem state [ "unused_surface_unbound"; "fresh_no_legacy_work" ] then (
    if member "origin" inventory <> json_string "fresh_no_legacy" then
      failwith "freshness origin mismatch";
    match member "counts" inventory with
    | `Assoc counts when List.for_all (fun (_, v) -> v = `Int 0) counts -> ()
    | _ -> failwith "invalid unused-surface attestation");
  (installation, state, seq, identity, valid, manifest)

let require_initialized db = ignore (validate_proof db)

let bootstrap db ~install =
  if table_exists db "slack_standing_order_feature_version" then
    require_initialized db
  else if List.exists feature_artifact (names db) then
    failwith "orphan feature proof; audited repair required"
  else
    match
      in_tx db ~on_error:Fun.id (fun () ->
          (* The second handle checks again after acquiring the writer lock. *)
          if table_exists db "slack_standing_order_feature_version" then (
            require_initialized db;
            Ok ())
          else (
            if List.exists feature_artifact (names db) then
              failwith "orphan feature artifacts";
            let inv, fresh = inventory db in
            install ();
            List.iter
              (fun (n, cols) ->
                exec db ("CREATE TABLE " ^ n ^ " (" ^ cols ^ ")") [])
              tables;
            List.iter (immutable db)
              [
                "slack_standing_order_feature_version";
                "slack_standing_order_rollout_evidence";
                "slack_standing_order_suppression";
                "slack_standing_order_external_deliveries";
                "slack_standing_order_inventory_manifests";
              ];
            let raw = Temp_downloads.generate_token () in
            let hex = digest raw in
            let installation =
              Printf.sprintf "%s-%s-4%s-a%s-%s" (String.sub hex 0 8)
                (String.sub hex 8 4) (String.sub hex 12 3) (String.sub hex 15 3)
                (String.sub hex 18 12)
            in
            let fp = fingerprint db in
            let initial_state =
              if fresh then "unused_surface_unbound"
              else "legacy_inventory_pending"
            in
            let payload =
              Yojson.Safe.to_string
                (`Assoc
                   [
                     ("inventory", inv);
                     ("fingerprint", json_string fp);
                     ("state", json_string initial_state);
                     ("identity", `Null);
                     ("manifest", `Null);
                   ])
            in
            let at = Unix.gettimeofday () in
            let d =
              evidence_digest ~installation ~seq:1 ~at ~kind:"bootstrap"
                ~payload ~previous:""
            in
            exec db
              "INSERT INTO slack_standing_order_rollout_evidence \
               VALUES(1,?,1,?,'bootstrap',?,'',?)"
              [ text installation; Sqlite3.Data.FLOAT at; text payload; text d ];
            exec db
              "INSERT INTO \
               slack_standing_order_rollout_state(singleton,state,seq) \
               VALUES(1,?,1)"
              [
                text
                  (if fresh then "unused_surface_unbound"
                   else "legacy_inventory_pending");
              ];
            exec db
              "INSERT INTO slack_standing_order_feature_version \
               VALUES(1,1,?,?,?)"
              [ text installation; text d; text fp ];
            Ok ()))
    with
    | Ok () -> ()
    | Error e -> failwith e

let append db ~kind ~payload ~state ~identity ~identity_valid ~manifest =
  let installation, _, seq, _, _, _ = validate_proof db in
  let previous =
    match
      query db
        "SELECT digest FROM slack_standing_order_rollout_evidence WHERE seq=?"
        [ int seq ]
        (fun s -> Sql_util.text_column s 0)
    with
    | [ d ] -> d
    | _ -> failwith "missing previous evidence"
  in
  let payload =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("body", payload);
           ("state", json_string state);
           ("identity", Option.fold ~none:`Null ~some:json_string identity);
           ("manifest", Option.fold ~none:`Null ~some:json_string manifest);
         ])
  and at = Unix.gettimeofday () in
  let next = seq + 1 in
  let d =
    evidence_digest ~installation ~seq:next ~at ~kind ~payload ~previous
  in
  exec db
    "INSERT INTO slack_standing_order_rollout_evidence VALUES(?,?,1,?,?,?,?,?)"
    [
      int next;
      text installation;
      Sqlite3.Data.FLOAT at;
      text kind;
      text payload;
      text previous;
      text d;
    ];
  exec db
    "UPDATE slack_standing_order_rollout_state SET \
     state=?,seq=?,identity=?,identity_valid=?,manifest=? WHERE singleton=1 \
     AND seq=?"
    [
      text state;
      int next;
      Sqlite3.Data.opt_text identity;
      bool identity_valid;
      Sqlite3.Data.opt_text manifest;
      int seq;
    ];
  if Sqlite3.changes db <> 1 then failwith "stale rollout evidence";
  next

let bind_pending_in_tx db ~(order : T.order) ?verified_identity () =
  let uuid, state, _, bound, valid, manifest = validate_proof db in
  let identity = Option.map identity_text verified_identity in
  Option.iter
    (fun (i : identity) ->
      if
        (not (valid_identity i))
        || i.team_id <> order.team_id
        || i.app_id <> order.api_app_id
        || i.enterprise_id <> order.enterprise_id
      then failwith "verified installation mismatch")
    verified_identity;
  let state, bound, valid =
    match (state, identity) with
    | "unused_surface_unbound", Some i ->
        let intervening =
          List.fold_left
            (fun n t -> n + int_query db ("SELECT count(*) FROM " ^ t) [])
            0
            (base_names
            @ [
                "slack_standing_order_recovery_candidates";
                "slack_standing_order_external_deliveries";
                "slack_standing_order_suppression";
              ])
        in
        if intervening <> 0 then
          failwith "intervening order work invalidates first binding";
        ("fresh_no_legacy_work", Some i, true)
    | "unused_surface_unbound", None -> ("legacy_inventory_pending", None, false)
    | _, Some i when bound <> None && bound <> Some i ->
        failwith "installation identity changed"
    | _, Some _ when (not valid) && bound <> None ->
        failwith "installation requires revalidation"
    | _ -> (state, bound, valid)
  in
  let seq =
    append db ~kind:"pending_binding"
      ~payload:
        (`Assoc
           [
             ("order", json_string order.id);
             ("identity", Option.fold ~none:`Null ~some:json_string bound);
           ])
      ~state ~identity:bound ~identity_valid:valid ~manifest
  in
  exec db "INSERT INTO slack_standing_order_order_bindings VALUES(?,?,?,?)"
    [ text order.id; text uuid; Sqlite3.Data.opt_text bound; int seq ]

let bind_activation_in_tx db (o : T.order) =
  let _, state, _, identity, valid, manifest = validate_proof db in
  let binding =
    int_query db
      "SELECT count(*) FROM slack_standing_order_order_bindings WHERE \
       order_id=?"
      [ text o.id ]
  in
  (* Unbound legacy rows stay manageable; they cannot pass readiness. *)
  if binding = 1 then
    let identity = match identity with Some i -> i | None -> "unbound" in
    let at = Option.get o.active_since in
    let seq =
      append db ~kind:"activation"
        ~payload:
          (`List
             [
               json_string o.id;
               `Int o.epoch;
               json_string at;
               json_string identity;
             ])
        ~state
        ~identity:(if identity = "unbound" then None else Some identity)
        ~identity_valid:valid ~manifest
    in
    exec db
      "INSERT INTO slack_standing_order_activation_bindings VALUES(?,?,?,?,?)"
      [ text o.id; int o.epoch; text at; text identity; int seq ]

let invalidate_identity db =
  exec db
    "UPDATE slack_standing_order_rollout_state SET identity_valid=0 WHERE \
     singleton=1"
    []

let revalidate_identity db (i : identity) =
  in_tx db ~on_error:Fun.id (fun () ->
      let _, _, _, bound, _, _ = validate_proof db in
      if bound = None then Ok true
      else if bound = Some (identity_text i) then (
        exec db
          "UPDATE slack_standing_order_rollout_state SET identity_valid=1 \
           WHERE singleton=1"
          [];
        Ok true)
      else (
        invalidate_identity db;
        Ok false))

let external_payload = function
  | `Assoc fields ->
      `Assoc
        (List.filter
           (fun (k, _) -> k <> "reviewer" && k <> "evidence_digest")
           fields)
  | _ -> failwith "invalid external metadata"

let inventory_complete db uuid manifest =
  let open Yojson.Safe.Util in
  match
    query db
      "SELECT manifest,audit FROM slack_standing_order_inventory_manifests \
       WHERE digest=? AND installation=?"
      [ text manifest; text uuid ]
      (fun s -> (Sql_util.text_column s 0, Sql_util.text_column s 1))
  with
  | [ (raw, audit) ] when digest raw = manifest ->
      let json = Yojson.Safe.from_string raw
      and audit = Yojson.Safe.from_string audit in
      let entries = member "entries" json |> to_list |> List.map to_string in
      let dispositions = member "dispositions" json |> to_list in
      let disposed =
        List.map (fun d -> member "entry" d |> to_string) dispositions
      in
      List.sort_uniq String.compare entries = List.sort String.compare disposed
      && List.length entries = List.length dispositions
      && member "inventory_digest" audit = `String manifest
      && member "installation" audit = `String uuid
      && member "item_dispositions" audit = `List dispositions
      && List.for_all
           (fun scope ->
             let order_id = member "order_id" scope |> to_string in
             let identity = Yojson.Safe.to_string (member "identity" json) in
             int_query db
               "SELECT count(*) FROM slack_standing_order_order_bindings WHERE \
                order_id=? AND installation=? AND identity=?"
               [ text order_id; text uuid; text identity ]
             = 1
             &&
             match member "active_since" scope with
             | `Null -> true
             | `String at ->
                 int_query db
                   "SELECT count(*) FROM \
                    slack_standing_order_activation_bindings WHERE order_id=? \
                    AND epoch=? AND active_since=? AND identity=?"
                   [
                     text order_id;
                     int (member "epoch" scope |> to_int);
                     text at;
                     text identity;
                   ]
                 = 1
             | _ -> false)
           (member "orders" json |> to_list)
      && List.for_all
           (fun d ->
             let id = member "entry" d |> to_string in
             match member "disposition" d with
             | `String "delivered" -> (
                 match
                   query db
                     "SELECT e.metadata FROM \
                      slack_standing_order_external_deliveries e JOIN \
                      slack_standing_order_suppression s ON \
                      s.receipt_digest=e.receipt_digest WHERE s.installation=? \
                      AND s.receipt_digest=?"
                     [ text uuid; text id ]
                     (fun s -> Sql_util.text_column s 0)
                 with
                 | [ metadata ] ->
                     let j = Yojson.Safe.from_string metadata in
                     member "evidence_digest" j = `String id
                     && member "installation" j = `String uuid
                     && digest (Yojson.Safe.to_string (external_payload j)) = id
                 | _ -> false)
             | `String "out_of_scope" -> (
                 match member "reason" d with
                 | `String reason -> String.trim reason <> ""
                 | _ -> false)
             | _ -> false)
           dispositions
  | _ -> false

let binding_evidence db ~order_id ~epoch ~active_since ~identity ~binding_seq
    ~activation_seq =
  let evidence seq =
    match
      query db
        "SELECT kind,payload FROM slack_standing_order_rollout_evidence WHERE \
         seq=?"
        [ int seq ]
        (fun s ->
          ( Sql_util.text_column s 0,
            Yojson.Safe.from_string (Sql_util.text_column s 1) ))
    with
    | [ (kind, payload) ] -> (kind, Yojson.Safe.Util.member "body" payload)
    | _ -> ("", `Null)
  in
  let has_scope body =
    let open Yojson.Safe.Util in
    member "orders" body |> to_list
    |> List.exists (fun s ->
        member "order_id" s = `String order_id
        && member "epoch" s = `Int epoch
        && member "active_since" s = `String active_since)
  in
  let pending =
    match evidence binding_seq with
    | "pending_binding", body ->
        Yojson.Safe.Util.member "order" body = `String order_id
        && Yojson.Safe.Util.member "identity" body = `String identity
    | "inventory_completion", body ->
        Yojson.Safe.Util.(
          member "orders" body |> to_list
          |> List.exists (fun s -> member "order_id" s = `String order_id))
    | _ -> false
  in
  let activated =
    match evidence activation_seq with
    | "activation", body ->
        body
        = `List
            [
              `String order_id;
              `Int epoch;
              `String active_since;
              `String identity;
            ]
    | "inventory_completion", body -> has_scope body
    | _ -> false
  in
  pending && activated

let rollout_readiness db ~order_id =
  try
    if not (table_exists db "slack_standing_order_feature_version") then
      Denied
        (if List.exists feature_artifact (names db) then Invalid_proof
         else Missing_proof)
    else
      match
        try Ok (validate_proof db)
        with Failure _ | Yojson.Json_error _ -> Error Invalid_proof
      with
      | Error d -> Denied d
      | Ok (_, "unused_surface_unbound", _, _, _, _) ->
          Denied Unbound_installation
      | Ok (_, "legacy_inventory_pending", _, _, _, _) ->
          Denied Inventory_pending
      | Ok (uuid, state, _, Some identity, true, manifest)
        when List.mem state [ "fresh_no_legacy_work"; "inventory_verified" ]
        -> (
          if not (base_shape db) then failwith "malformed order schema";
          let rows =
            query db
              "SELECT \
               o.team_id,o.api_app_id,o.enterprise_id,o.bot_user_id,o.bot_id,o.active_since,o.epoch,b.installation,b.identity,a.active_since,a.identity,b.seq,a.seq \
               FROM slack_standing_orders o LEFT JOIN \
               slack_standing_order_order_bindings b ON b.order_id=o.id LEFT \
               JOIN slack_standing_order_activation_bindings a ON \
               a.order_id=o.id AND a.epoch=o.epoch WHERE o.id=?"
              [ text order_id ]
              (fun s ->
                ( List.init 6 (Sql_util.opt_text_column s),
                  Sql_util.int_column s 6,
                  List.init 4 (fun n -> Sql_util.opt_text_column s (n + 7)),
                  Sql_util.int_column s 11,
                  Sql_util.int_column s 12 ))
          in
          match rows with
          | [
           ( [
               Some team; Some app; enterprise; Some user; Some bot; Some active;
             ],
             epoch,
             [ Some installation; Some bi; Some ai; Some ii ],
             binding_seq,
             activation_seq );
          ]
            when installation = uuid && bi = identity && ii = identity
                 && ai = active && T.valid_slack_ts active -> (
              if
                not
                  (binding_evidence db ~order_id ~epoch ~active_since:active
                     ~identity ~binding_seq ~activation_seq)
              then Denied Invalid_proof
              else if
                identity_text
                  {
                    team_id = team;
                    app_id = app;
                    enterprise_id = enterprise;
                    own_user_id = user;
                    own_bot_id = bot;
                  }
                <> identity
              then Denied Identity_mismatch
              else if state = "fresh_no_legacy_work" then
                Ready Fresh_no_legacy_work
              else
                match manifest with
                | Some d when inventory_complete db uuid d ->
                    Ready Inventory_verified
                | _ -> Denied Invalid_proof)
          | _ -> Denied Activation_unbound)
      | Ok (_, _, _, _, false, _) -> Denied Identity_mismatch
      | _ -> Denied Invalid_proof
  with
  | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> Denied Invalid_proof
  | exn when is_db_error exn -> Denied Storage_error

let suppressed db ~order_id ~root_ts ~room_id ~date_start =
  int_query db
    "SELECT count(*) FROM slack_standing_order_suppression WHERE order_id=? \
     AND (root_ts=? OR (room_id=? AND date_start=?))"
    [ text order_id; text root_ts; text room_id; text date_start ]
  > 0

let bootstrap_inventory db =
  match
    query db
      "SELECT payload FROM slack_standing_order_rollout_evidence WHERE seq=1" []
      (fun s -> Sql_util.text_column s 0)
  with
  | [ s ] -> Yojson.Safe.Util.member "inventory" (Yojson.Safe.from_string s)
  | _ -> failwith "bootstrap inventory missing"
