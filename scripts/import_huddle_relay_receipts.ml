module Import = Slack_standing_order_receipt_import
(** Explicit offline DB and reviewed metadata only; preview is the default. *)

let change_json = function
  | Import.Suppress digest -> `Assoc [ ("suppress", `String digest) ]
  | Import.Link_automatic (digest, fire, attempt) ->
      `Assoc
        [
          ("receipt", `String digest);
          ("automatic_fire", `String fire);
          ("automatic_attempt", `Int attempt);
        ]
  | Import.Already_imported digest ->
      `Assoc [ ("already_imported", `String digest) ]
  | Import.Complete_scope orders ->
      `Assoc
        [ ("complete_scope", `List (List.map (fun id -> `String id) orders)) ]

let () =
  let db_path = ref "" and manifest_path = ref "" and apply_mode = ref false in
  Arg.parse
    [
      ("--db", Arg.Set_string db_path, "Explicit offline DB path");
      ( "--manifest",
        Arg.Set_string manifest_path,
        "Reviewed metadata manifest JSON" );
      ( "--apply",
        Arg.Set apply_mode,
        "Apply complete inventory atomically (default: preview)" );
    ]
    (fun _ -> raise (Arg.Bad "unexpected positional argument"))
    "import_huddle_relay_receipts --db PATH --manifest PATH [--apply]";
  if !db_path = "" || !manifest_path = "" then (
    prerr_endline "--db and --manifest are required";
    exit 2);
  let db =
    if !apply_mode then Sqlite3.db_open ~mode:`NO_CREATE !db_path
    else Sqlite3.db_open ~mode:`READONLY !db_path
  in
  let failed e =
    prerr_endline (Import.error_name e);
    exit 1
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let document = Yojson.Safe.from_file !manifest_path in
      match Import.validate_document db document with
      | Error e -> failed e
      | Ok (manifest_digest, receipts) -> (
          if !apply_mode then
            match Import.apply db receipts ~manifest_digest with
            | Error e -> failed e
            | Ok audit ->
                print_endline
                  (Yojson.Safe.pretty_to_string (Import.audit_json audit))
          else
            match Import.preview db receipts with
            | Error e -> failed e
            | Ok changes ->
                print_endline
                  (Yojson.Safe.pretty_to_string
                     (`Assoc
                        [
                          ("mode", `String "preview");
                          ("inventory_digest", `String manifest_digest);
                          ("changes", `List (List.map change_json changes));
                          ( "inventory",
                            Yojson.Safe.Util.member "inventory" document );
                        ]))))
