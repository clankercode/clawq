(* Deny-wins canonical/alias tool authorization. *)

type decision = Allowed | Denied of string

let name_in_list lst name = List.mem name lst

let decide ?canonical ~equivalence_names ~allowed_tools ~denied_tools () =
  let names =
    match equivalence_names with
    | [] -> ( match canonical with Some c -> [ c ] | None -> [])
    | ns -> ns
  in
  let display =
    match canonical with
    | Some c -> c
    | None -> ( match names with n :: _ -> n | [] -> "<unknown>")
  in
  (* 1. Deny-wins: any denied equivalent denies the whole class. *)
  let denied_hit = List.find_opt (fun n -> name_in_list denied_tools n) names in
  match denied_hit with
  | Some hit ->
      Denied
        (Printf.sprintf
           "Error: Tool '%s' is denied by policy (matched deny entry '%s' via \
            equivalence class)."
           display hit)
  | None ->
      (* 2. Nonempty allowlist admits if any equivalent is listed. *)
      if allowed_tools = [] then Allowed
      else if List.exists (fun n -> name_in_list allowed_tools n) names then
        Allowed
      else
        Denied
          (Printf.sprintf
             "Error: Tool '%s' is not in the allowed tools list (no equivalent \
              name admitted)."
             display)

let is_allowed ~equivalence_names ~allowed_tools ~denied_tools () =
  match decide ~equivalence_names ~allowed_tools ~denied_tools () with
  | Allowed -> true
  | Denied _ -> false

let denial_message ?canonical ~equivalence_names ~allowed_tools ~denied_tools ()
    =
  match
    decide ?canonical ~equivalence_names ~allowed_tools ~denied_tools ()
  with
  | Allowed -> None
  | Denied msg -> Some msg

let filter_names ~names ~all_names ~allowed_tools ~denied_tools =
  List.filter
    (fun name ->
      let equivalence_names = all_names name in
      is_allowed ~equivalence_names ~allowed_tools ~denied_tools ())
    names
