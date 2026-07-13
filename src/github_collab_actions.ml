(* Policy-gated GitHub collab write intents (comment / label / assign).
   See github_collab_actions.mli and docs/plans/2026-07-12-github-item-room-routing.md. *)

open Github_route_store

type action =
  | Comment of { item_key : string; body : string }
  | Label of { item_key : string; add : string list; remove : string list }
  | Assign of { item_key : string; add : string list; remove : string list }

type decision =
  | Allowed of { action : action; capability : string }
  | Denied of { reason : string }

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let string_list_to_json xs = `List (List.map (fun s -> `String s) xs)

let action_item_key = function
  | Comment { item_key; _ } -> item_key
  | Label { item_key; _ } -> item_key
  | Assign { item_key; _ } -> item_key

let capability_for_action = function
  | Comment _ -> "allow_reply"
  | Label _ -> "allow_label"
  | Assign _ -> "allow_assign"

let action_kind_string = function
  | Comment _ -> "comment"
  | Label _ -> "label"
  | Assign _ -> "assign"

let action_to_json = function
  | Comment { item_key; body } ->
      `Assoc
        (sort_assoc
           [
             ("kind", `String "comment");
             ("item_key", `String item_key);
             ("body", `String body);
           ])
  | Label { item_key; add; remove } ->
      `Assoc
        (sort_assoc
           [
             ("kind", `String "label");
             ("item_key", `String item_key);
             ("add", string_list_to_json add);
             ("remove", string_list_to_json remove);
           ])
  | Assign { item_key; add; remove } ->
      `Assoc
        (sort_assoc
           [
             ("kind", `String "assign");
             ("item_key", `String item_key);
             ("add", string_list_to_json add);
             ("remove", string_list_to_json remove);
           ])

let capability_granted (policy : capability_policy) = function
  | Comment _ -> policy.allow_reply
  | Label _ -> policy.allow_label
  | Assign _ -> policy.allow_assign

let authorize ~route ~action =
  match route with
  | None ->
      Denied
        {
          reason =
            Printf.sprintf
              "no route available to authorize %s (capability %s required)"
              (action_kind_string action)
              (capability_for_action action);
        }
  | Some (r : t) ->
      let cap = capability_for_action action in
      if capability_granted r.capability_policy action then
        Allowed { action; capability = cap }
      else
        Denied
          {
            reason =
              Printf.sprintf
                "capability %s not granted by route %s policy for %s" cap r.id
                (action_kind_string action);
          }

let validate_action = function
  | Comment { item_key; body } ->
      if String.trim item_key = "" then
        Error "comment item_key must be non-empty"
      else if String.trim body = "" then Error "comment body must be non-empty"
      else Ok ()
  | Label { item_key; add; remove } ->
      if String.trim item_key = "" then Error "label item_key must be non-empty"
      else if add = [] && remove = [] then
        Error "label action requires at least one label to add or remove"
      else Ok ()
  | Assign { item_key; add; remove } ->
      if String.trim item_key = "" then
        Error "assign item_key must be non-empty"
      else if add = [] && remove = [] then
        Error "assign action requires at least one assignee to add or remove"
      else Ok ()

let room_context ~room_id : Setup_plan.context =
  {
    room_id = Some room_id;
    session_key = None;
    connector = None;
    profile_id = None;
    extra = [];
  }

let store_pending ~db (plan : Setup_plan.t) =
  Setup_plan_apply.init_schema db;
  match Setup_plan_apply.store_plan ~db plan with
  | Ok () -> Ok plan
  | Error e -> Error e

let plan_action ~db ~principal ~room_id ~action ~base_revision ?route
    ?(now = Unix.gettimeofday ()) () =
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    match validate_action action with
    | Error e -> Error e
    | Ok () -> (
        match authorize ~route ~action with
        | Denied { reason } -> Error reason
        | Allowed { action; capability } ->
            let item_key = action_item_key action in
            let kind = action_kind_string action in
            let action_json = action_to_json action in
            let path = Printf.sprintf "github_collab/%s/%s" kind item_key in
            let current_state =
              `Assoc
                (sort_assoc
                   [
                     ("item_key", `String item_key);
                     ("room_id", `String room_id);
                     ("status", `String "pending_mutation");
                   ])
            in
            let planned_state =
              `Assoc
                (sort_assoc
                   ([
                      ("action", action_json);
                      ("capability", `String capability);
                      ("item_key", `String item_key);
                      ("room_id", `String room_id);
                      ("status", `String "planned");
                    ]
                   @
                   match route with
                   | None -> []
                   | Some (r : t) ->
                       [
                         ("route_id", `String r.id);
                         ("route_revision", `String r.revision);
                       ]))
            in
            let diff =
              [
                Setup_plan.Create { path; value = action_json };
                Setup_plan.Note
                  {
                    path;
                    message =
                      Printf.sprintf
                        "Policy-gated %s on %s via %s; confirm before apply. \
                         No live GitHub mutation at plan time."
                        kind item_key capability;
                  };
              ]
            in
            let readiness =
              [
                {
                  Setup_plan.name = "capability";
                  status = Setup_plan.Pass;
                  message = capability;
                };
                {
                  name = "item_key";
                  status = Setup_plan.Pass;
                  message = item_key;
                };
                {
                  name = "no_live_mutation";
                  status = Setup_plan.Pass;
                  message =
                    "plan only; live GitHub write requires confirm/apply";
                };
              ]
            in
            let warnings = [] in
            let op_fields =
              sort_assoc
                ([
                   ("op", `String kind);
                   ("item_key", `String item_key);
                   ("capability", `String capability);
                   ("action", action_json);
                 ]
                @
                match route with
                | None -> []
                | Some (r : t) ->
                    [
                      ("route_id", `String r.id);
                      ("route_revision", `String r.revision);
                    ])
            in
            let ops = `List [ `Assoc op_fields ] in
            let data =
              `Assoc
                (sort_assoc
                   [
                     ("base_revision", `String base_revision);
                     ("room_id", `String room_id);
                     ("item_key", `String item_key);
                     ("capability", `String capability);
                   ])
            in
            let ctx = room_context ~room_id in
            let plan =
              Setup_plan.make ~principal ~source:ctx ~destination:ctx
                ~current_state ~planned_state ~diff ~readiness ~warnings
                ~base_revision
                ~apply_payload:
                  {
                    kind = Setup_plan.Generic "github_collab_action";
                    ops;
                    data;
                  }
                ~now ()
            in
            store_pending ~db plan)
