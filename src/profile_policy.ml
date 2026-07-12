type requirement = {
  grant_type : string;
  required_permission : string;
  granted : bool;
  reason : string;
}

let requirement ~grant_type ~required_permission ~granted ~reason =
  { grant_type; required_permission; granted; reason }

let denial_message ~profile_id req =
  Printf.sprintf
    "Error: room profile '%s' is not permitted by profile policy. grant type: \
     %s; required permission: %s. %s"
    profile_id req.grant_type req.required_permission req.reason

let denials ~profile_id requirements =
  requirements
  |> List.filter (fun req -> not req.granted)
  |> List.map (denial_message ~profile_id)

let denial ~profile_id req =
  if req.granted then None else Some (denial_message ~profile_id req)

let tool_required_permission ~tool_name = "invoke:" ^ tool_name

let tool_denial ~profile_id ~tool_name ~allowed_tools ~denied_tools
    ?(equivalence_names = [ tool_name ]) () =
  let required_permission = tool_required_permission ~tool_name in
  (* Deny-wins over the full equivalence class (canonical + aliases). *)
  match
    Tool_authz.decide ~canonical:tool_name ~equivalence_names ~allowed_tools
      ~denied_tools ()
  with
  | Tool_authz.Allowed -> None
  | Tool_authz.Denied authz_msg ->
      let reason =
        if
          List.exists
            (fun n -> Tool_authz.name_in_list denied_tools n)
            equivalence_names
        then
          Printf.sprintf
            "Tool '%s' is denied via deny-wins equivalence class. Ask an \
             administrator to remove the deny entry or use a permitted tool. \
             (%s)"
            tool_name authz_msg
        else
          Printf.sprintf
            "Tool '%s' is not admitted by allowed_tools (deny-wins \
             equivalence). Ask an administrator to add an equivalent name or \
             use a permitted tool. (%s)"
            tool_name authz_msg
      in
      denial ~profile_id
        (requirement ~grant_type:"tool" ~required_permission ~granted:false
           ~reason)

let codebase_required_permission ~path = "access:" ^ path

let codebase_denial ~profile_id ~path ~configured_grants ~granted =
  if configured_grants = [] || granted then None
  else
    denial ~profile_id
      (requirement ~grant_type:"codebase"
         ~required_permission:(codebase_required_permission ~path)
         ~granted:false
         ~reason:
           (Printf.sprintf
              "Path '%s' is outside room_profile_codebase_grants. Configured \
               grants: %s."
              path
              (String.concat ", "
                 (List.map (fun grant -> "\"" ^ grant ^ "\"") configured_grants))))

let memory_scope_required_permission ~scope_id ~capability =
  Printf.sprintf "memory_scope:%d:%s" scope_id capability

let memory_scope_denial ~profile_id ~scope_id ~capability ~granted_capabilities
    =
  if List.mem capability granted_capabilities then None
  else
    let grants =
      match granted_capabilities with
      | [] -> "none"
      | capabilities -> String.concat ", " capabilities
    in
    denial ~profile_id
      (requirement ~grant_type:"memory_scope"
         ~required_permission:
           (memory_scope_required_permission ~scope_id ~capability)
         ~granted:false
         ~reason:
           (Printf.sprintf
              "Memory scope %d requires capability '%s'. Granted capabilities: \
               %s."
              scope_id capability grants))
