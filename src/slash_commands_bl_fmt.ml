type bl_action = BlList | BlShow of string | BlBugs | BlIdeas

type held_items_action =
  | HeldItemsList of bool
  | HeldItemsShow of int
  | HeldItemsApprove of int
  | HeldItemsReject of int * string option

(* ── Existing format: bl ───────────────────────────────────────────────── *)

let run_bl_command args =
  try
    let cmd =
      "bl " ^ String.concat " " args ^ " --json --no-color 2>/dev/null"
    in
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 4096 in
    (try
       while true do
         Buffer.add_char buf (input_char ic)
       done
     with End_of_file -> ());
    let _status = Unix.close_process_in ic in
    let output = Buffer.contents buf in
    if String.trim output = "" then Error "No backlog data found."
    else Ok (Yojson.Safe.from_string output)
  with exn ->
    Error (Printf.sprintf "Failed to run bl: %s" (Printexc.to_string exn))

let format_bl_list ~connector json =
  let open Yojson.Safe.Util in
  let buf = Buffer.create 2048 in
  let phases = json |> member "phases" |> to_list in
  let critical_path =
    try json |> member "critical_path" |> to_list |> List.map to_string
    with _ -> []
  in
  if critical_path <> [] then begin
    Buffer.add_string buf
      (Format_adapter.bold connector "Critical Path"
      ^ " "
      ^ String.concat " \xE2\x86\x92 " critical_path
      ^ "\n\n")
  end;
  let phase_columns =
    Table_format.
      [
        { header = "ID"; align = Left; min_width = 4; flex = false };
        { header = "PHASE"; align = Left; min_width = 10; flex = true };
        { header = "DONE"; align = Right; min_width = 4; flex = false };
        { header = "TOTAL"; align = Right; min_width = 5; flex = false };
      ]
  in
  let phase_rows =
    List.map
      (fun phase ->
        let id = phase |> member "id" |> to_string in
        let name = phase |> member "name" |> to_string in
        let stats = phase |> member "stats" in
        let done_count = try stats |> member "done" |> to_int with _ -> 0 in
        let total = try stats |> member "total" |> to_int with _ -> 0 in
        [ id; name; string_of_int done_count; string_of_int total ])
      phases
  in
  if phase_rows <> [] then begin
    Buffer.add_string buf (Format_adapter.bold connector "Phases");
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.render_table connector ~max_width:70 phase_columns
         phase_rows)
  end;
  let bugs = try json |> member "bugs" |> to_list with _ -> [] in
  let open_bugs =
    List.filter
      (fun b ->
        let s = try b |> member "status" |> to_string with _ -> "" in
        s <> "done")
      bugs
  in
  if open_bugs <> [] then begin
    if phase_rows <> [] then Buffer.add_string buf "\n\n";
    let bug_columns =
      Table_format.
        [
          { header = "ID"; align = Left; min_width = 4; flex = false };
          { header = "STATUS"; align = Left; min_width = 6; flex = false };
          { header = "PRI"; align = Left; min_width = 3; flex = false };
          { header = "TITLE"; align = Left; min_width = 10; flex = true };
        ]
    in
    let bug_rows =
      List.map
        (fun b ->
          let id = try b |> member "id" |> to_string with _ -> "?" in
          let status = try b |> member "status" |> to_string with _ -> "?" in
          let priority =
            try b |> member "priority" |> to_string with _ -> ""
          in
          let title = try b |> member "title" |> to_string with _ -> "" in
          let title =
            if String.length title > 60 then String.sub title 0 57 ^ "..."
            else title
          in
          [ id; status; priority; title ])
        open_bugs
    in
    Buffer.add_string buf
      (Format_adapter.bold connector
         (Printf.sprintf "Open Bugs (%d)" (List.length open_bugs)));
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.render_table connector ~max_width:80 bug_columns bug_rows)
  end;
  let ideas = try json |> member "ideas" |> to_list with _ -> [] in
  let open_ideas =
    List.filter
      (fun i ->
        let s = try i |> member "status" |> to_string with _ -> "" in
        s <> "done")
      ideas
  in
  if open_ideas <> [] then begin
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.bold connector
         (Printf.sprintf "Open Ideas (%d)" (List.length open_ideas)));
    Buffer.add_string buf "\n\n";
    let idea_rows =
      List.map
        (fun i ->
          let id = try i |> member "id" |> to_string with _ -> "?" in
          let status = try i |> member "status" |> to_string with _ -> "?" in
          let title = try i |> member "title" |> to_string with _ -> "" in
          let title =
            if String.length title > 60 then String.sub title 0 57 ^ "..."
            else title
          in
          [ id; status; title ])
        open_ideas
    in
    let idea_columns =
      Table_format.
        [
          { header = "ID"; align = Left; min_width = 4; flex = false };
          { header = "STATUS"; align = Left; min_width = 6; flex = false };
          { header = "TITLE"; align = Left; min_width = 10; flex = true };
        ]
    in
    Buffer.add_string buf
      (Format_adapter.render_table connector ~max_width:80 idea_columns
         idea_rows)
  end;
  Buffer.contents buf

let format_bl_show ~connector id json =
  let open Yojson.Safe.Util in
  let find_task () =
    let search_in items =
      List.find_opt
        (fun item ->
          try item |> member "id" |> to_string = id with _ -> false)
        items
    in
    let bugs = try json |> member "bugs" |> to_list with _ -> [] in
    match search_in bugs with
    | Some t -> Some t
    | None ->
        let ideas = try json |> member "ideas" |> to_list with _ -> [] in
        search_in ideas
  in
  match find_task () with
  | None -> Printf.sprintf "Task '%s' not found in backlog." id
  | Some task ->
      let id = try task |> member "id" |> to_string with _ -> "?" in
      let title = try task |> member "title" |> to_string with _ -> "" in
      let status = try task |> member "status" |> to_string with _ -> "?" in
      let priority =
        try task |> member "priority" |> to_string with _ -> ""
      in
      let complexity =
        try task |> member "complexity" |> to_string with _ -> ""
      in
      let estimate =
        try
          let h = task |> member "estimate_hours" |> to_int in
          Printf.sprintf "%dh" h
        with _ -> ""
      in
      let rows =
        [ [ "ID"; id ]; [ "Title"; title ]; [ "Status"; status ] ]
        @ (if priority <> "" then [ [ "Priority"; priority ] ] else [])
        @ (if complexity <> "" then [ [ "Complexity"; complexity ] ] else [])
        @ if estimate <> "" then [ [ "Estimate"; estimate ] ] else []
      in
      let columns =
        Table_format.
          [
            { header = "FIELD"; align = Left; min_width = 10; flex = false };
            { header = "VALUE"; align = Left; min_width = 20; flex = true };
          ]
      in
      Format_adapter.bold connector (Printf.sprintf "Task %s" id)
      ^ "\n\n"
      ^ Format_adapter.render_table connector ~max_width:60 columns rows

let format_bl_filtered ~connector ~filter_type json =
  let open Yojson.Safe.Util in
  let items = try json |> member filter_type |> to_list with _ -> [] in
  if items = [] then Printf.sprintf "No %s found in backlog." filter_type
  else
    let columns =
      Table_format.
        [
          { header = "ID"; align = Left; min_width = 4; flex = false };
          { header = "STATUS"; align = Left; min_width = 6; flex = false };
          { header = "PRI"; align = Left; min_width = 3; flex = false };
          { header = "TITLE"; align = Left; min_width = 10; flex = true };
        ]
    in
    let rows =
      List.map
        (fun item ->
          let id = try item |> member "id" |> to_string with _ -> "?" in
          let status =
            try item |> member "status" |> to_string with _ -> "?"
          in
          let priority =
            try item |> member "priority" |> to_string with _ -> ""
          in
          let title = try item |> member "title" |> to_string with _ -> "" in
          let title =
            if String.length title > 60 then String.sub title 0 57 ^ "..."
            else title
          in
          [ id; status; priority; title ])
        items
    in
    let label = String.capitalize_ascii filter_type in
    Format_adapter.bold connector
      (Printf.sprintf "%s (%d)" label (List.length items))
    ^ "\n\n"
    ^ Format_adapter.render_table connector ~max_width:80 columns rows

let format_bl ~connector action =
  let json_args =
    match action with
    | BlList -> [ "list" ]
    | BlBugs -> [ "list"; "--bugs" ]
    | BlIdeas -> [ "list"; "--ideas" ]
    | BlShow _ -> [ "list" ]
  in
  match run_bl_command json_args with
  | Error msg -> msg
  | Ok json -> (
      match action with
      | BlList -> format_bl_list ~connector json
      | BlBugs -> format_bl_filtered ~connector ~filter_type:"bugs" json
      | BlIdeas -> format_bl_filtered ~connector ~filter_type:"ideas" json
      | BlShow id -> format_bl_show ~connector id json)

(* ── Held items format ────────────────────────────────────────────────── *)

let format_held_items ~connector ~(db : Sqlite3.db) action =
  Held_items.init_db db;
  match action with
  | HeldItemsList show_all ->
      let status = if show_all then "all" else "pending" in
      let items = Held_items.list_items ~db ~status () in
      if items = [] then
        Format_adapter.bold connector "No"
        ^ if show_all then " held items." else " pending held items."
      else
        let columns =
          Table_format.
            [
              { header = "ID"; align = Right; min_width = 2; flex = false };
              { header = "NAME"; align = Left; min_width = 8; flex = false };
              { header = "L"; align = Right; min_width = 1; flex = false };
              { header = "STATUS"; align = Left; min_width = 6; flex = false };
              { header = "DESC"; align = Left; min_width = 10; flex = true };
            ]
        in
        let rows =
          List.map
            (fun (item : Held_items.held_item) ->
              let desc_short =
                if String.length item.description > 40 then
                  String.sub item.description 0 40 ^ "..."
                else item.description
              in
              [
                string_of_int item.id;
                item.feature_name;
                string_of_int item.layer;
                item.status;
                desc_short;
              ])
            items
        in
        let label =
          if show_all then "Held Items (all)" else "Held Items (pending)"
        in
        Format_adapter.bold connector label
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:80 columns rows
  | HeldItemsShow id -> (
      match Held_items.get ~db ~id with
      | None -> Printf.sprintf "No held item found with ID %d." id
      | Some item ->
          let open Content_dsl in
          let doc =
            [
              Paragraph
                [ Bold "Held Item"; Text " #"; Text (string_of_int item.id) ];
              Paragraph [ Text "Name: "; Code item.feature_name ];
              Paragraph [ Text "Layer: "; Code (string_of_int item.layer) ];
              Paragraph [ Text "Status: "; Code item.status ];
              Paragraph [ Text "Description: "; Text item.description ];
              Paragraph
                [
                  Text "Requestor: ";
                  Text (Option.value ~default:"-" item.requestor_id);
                ];
              Paragraph
                [
                  Text "Channel: ";
                  Text (Option.value ~default:"-" item.channel);
                ];
              Paragraph [ Text "Created: "; Text item.created_at ];
            ]
          in
          let doc =
            match item.reviewed_by with
            | Some by ->
                doc
                @ [
                    Paragraph [ Text "Reviewed by: "; Text by ];
                    Paragraph
                      [
                        Text "Reviewed at: ";
                        Text (Option.value ~default:"-" item.reviewed_at);
                      ];
                    Paragraph
                      [
                        Text "Notes: ";
                        Text (Option.value ~default:"-" item.review_notes);
                      ];
                  ]
            | None -> doc
          in
          Content_dsl.render_document connector doc)
  | HeldItemsApprove id ->
      if Held_items.review ~db ~id ~action:"approved" () then
        Printf.sprintf "Approved held item #%d." id
      else
        Printf.sprintf
          "Failed to approve item #%d. It may not exist or may not be pending."
          id
  | HeldItemsReject (id, reason) ->
      if Held_items.review ~db ~id ~action:"rejected" ?notes:reason () then
        Printf.sprintf "Rejected held item #%d." id
      else
        Printf.sprintf
          "Failed to reject item #%d. It may not exist or may not be pending."
          id

let format_held_items_usage ~connector =
  let open Content_dsl in
  render_document connector
    [
      Paragraph [ Bold "/held-items"; Text " — manage held feature plans" ];
      Paragraph [ Code "/held-items"; Text " — list pending items" ];
      Paragraph [ Code "/held-items list"; Text " — list pending items" ];
      Paragraph [ Code "/held-items list --all"; Text " — list all items" ];
      Paragraph [ Code "/held-items view <id>"; Text " — show item details" ];
      Paragraph
        [ Code "/held-items approve <id>"; Text " — approve (admin only)" ];
      Paragraph
        [
          Code "/held-items reject <id> [reason]"; Text " — reject (admin only)";
        ];
    ]
