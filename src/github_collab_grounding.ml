(** Ground Room/thread questions with journal + optional live GitHub state
    (P19.M4.E1.T001).

    Read-only composition over [Github_item_context_resolve] and injectable
    [live_fetch]. Does not wake the agent. *)

module R = Github_item_context_resolve

type live_snapshot = {
  title : string option;
  state : string option;
  labels : string list;
  head_sha : string option;
  body_excerpt : string option;
}

type grounded = {
  room_id : string;
  item_key : string option;
  resolved : R.resolved;
  live : live_snapshot option;
  prompt_block : string;
}

type live_fetch = item_key:string -> (live_snapshot, string) result

(** {1 Defensive secret shape checks for live fields}

    The fetcher is responsible for redacting body excerpts, but prompt assembly
    still refuses token/PEM-shaped free text so a buggy fetcher cannot smuggle
    secrets into the agent block. *)

let looks_like_secret s =
  let sl = String.lowercase_ascii s in
  String_util.contains sl "bearer "
  || String_util.contains sl "ghp_"
  || String_util.contains sl "ghs_"
  || String_util.contains sl "github_pat_"
  || String_util.contains sl "gho_"
  || String_util.contains sl "ghu_"
  || String_util.contains s "BEGIN"
     && (String_util.contains s "PRIVATE KEY"
        || String_util.contains s "-----BEGIN")
  || String_util.contains_ci s "client_secret"
  || String_util.contains_ci s "webhook_secret"
  || String_util.contains_ci s "api_key="
  || String_util.contains_ci s "authorization:"
  || String_util.contains_ci s "password "

let safe_opt_text = function
  | None -> None
  | Some s ->
      let t = String.trim s in
      if t = "" then None
      else if looks_like_secret t then Some "***REDACTED***"
      else Some t

let opt_s = function Some s -> s | None -> "-"

(** {1 prompt_block assembly} *)

let format_live_section (snap : live_snapshot) =
  let title = safe_opt_text snap.title in
  let state = safe_opt_text snap.state in
  let head = safe_opt_text snap.head_sha in
  let body = safe_opt_text snap.body_excerpt in
  let labels =
    snap.labels
    |> List.filter_map (fun l ->
        let t = String.trim l in
        if t = "" then None
        else if looks_like_secret t then Some "***REDACTED***"
        else Some t)
  in
  let labels_s = match labels with [] -> "-" | xs -> String.concat "," xs in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "live_github:";
  add (Printf.sprintf "  title=%s" (opt_s title));
  add (Printf.sprintf "  state=%s" (opt_s state));
  add (Printf.sprintf "  labels=%s" labels_s);
  add (Printf.sprintf "  head_sha=%s" (opt_s head));
  add (Printf.sprintf "  body_excerpt=%s" (opt_s body));
  String.concat "\n" (List.rev !lines)

let format_clarifying (resolved : R.resolved) =
  match resolved.ambiguity with
  | [] -> None
  | candidates ->
      let listed =
        candidates |> List.map (fun k -> "  - " ^ k) |> String.concat "\n"
      in
      Some
        (String.concat "\n"
           [
             "clarification_needed: true";
             "reason=multiple_item_candidates";
             "candidates:";
             listed;
             "instruction=Ask the user which item_key to use; do not guess.";
           ])

let build_prompt_block ~(resolved : R.resolved) ~live =
  let parts = ref [] in
  let add s = parts := s :: !parts in
  add "[github_collab_grounding]";
  add (Printf.sprintf "room_id=%s" resolved.room_id);
  (match resolved.item_key with
  | Some k -> add (Printf.sprintf "item_key=%s" k)
  | None -> add "item_key=*");
  add
    (Printf.sprintf "live_present=%s"
       (if Option.is_some live then "true" else "false"));
  add
    (Printf.sprintf "ambiguous=%s"
       (if resolved.ambiguity = [] then "false" else "true"));
  add "";
  add "journal_context:";
  (* Indent context_block lines for nesting under journal_context. *)
  resolved.context_block |> String.split_on_char '\n'
  |> List.iter (fun line -> add ("  " ^ line));
  (match live with
  | Some snap ->
      add "";
      add (format_live_section snap)
  | None -> ());
  (match format_clarifying resolved with
  | Some clar ->
      add "";
      add clar
  | None ->
      if resolved.item_key = None && resolved.ambiguity = [] then (
        add "";
        add "clarification_needed: false";
        add "note=no_item_resolved"));
  String.concat "\n" (List.rev !parts)

let try_live_fetch ~live_fetch ~item_key =
  match (live_fetch, item_key) with
  | None, _ | _, None -> None
  | Some fetch, Some key -> (
      (* Soft-fail: journal context still grounds the agent when live GitHub is
         unavailable or access was revoked. Never surface fetch error text
         (may contain tokens). *)
      match fetch ~item_key:key with
      | Ok snap -> Some snap
      | Error _ -> None)

let ground ~db ~source ?live_fetch () =
  match R.resolve ~db ~source () with
  | Error e -> Error e
  | Ok resolved ->
      let live = try_live_fetch ~live_fetch ~item_key:resolved.item_key in
      let prompt_block = build_prompt_block ~resolved ~live in
      Ok
        {
          room_id = resolved.room_id;
          item_key = resolved.item_key;
          resolved;
          live;
          prompt_block;
        }
