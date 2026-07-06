let format_routine_target ?profile_id ?thread_id ?routine_workspace_id () =
  let parts = ref [] in
  (match profile_id with
  | Some id -> parts := Printf.sprintf "profile=%d" id :: !parts
  | None -> ());
  (match thread_id with
  | Some id -> parts := ("thread=" ^ id) :: !parts
  | None -> ());
  (match routine_workspace_id with
  | Some id -> parts := ("workspace=" ^ id) :: !parts
  | None -> ());
  match List.rev !parts with
  | [] -> None
  | parts -> Some (String.concat " " parts)
