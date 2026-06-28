(** Teams Adaptive Card editing support.

    Provides functions for editing existing Adaptive Cards in Teams
    conversations. Separated from teams.ml to keep file size within limits. *)

(** [edit_adaptive_card ~config ~service_url ~conversation_id ~activity_id ~card
     ()] edits an existing Adaptive Card in place. Raises [Failure] on non-2xx
    responses so callers can fall back to sending a fresh card. *)
let edit_adaptive_card ~(config : Runtime_config.teams_config) ~service_url
    ~conversation_id ~activity_id ~card () =
  let open Lwt.Syntax in
  let* token_opt = Teams_auth.fetch_token ~config in
  match token_opt with
  | None ->
      Logs.err (fun m -> m "Teams: cannot edit adaptive card, no OAuth token");
      Lwt.fail (Failure "No OAuth token available")
  | Some token ->
      let uri =
        Printf.sprintf "%s/v3/conversations/%s/activities/%s"
          (String.trim service_url)
          (Uri.pct_encode conversation_id)
          (Uri.pct_encode activity_id)
      in
      let headers = [ ("Authorization", "Bearer " ^ token) ] in
      let body = Yojson.Safe.to_string card in
      let* status, resp = Http_client.put_json ~uri ~headers ~body in
      if status < 200 || status >= 300 then begin
        Logs.warn (fun m ->
            m
              "Teams: edit_adaptive_card failed (HTTP %d) conv=%s activity=%s: \
               %s"
              status conversation_id activity_id resp);
        Lwt.fail
          (Failure (Printf.sprintf "edit_adaptive_card failed (HTTP %d)" status))
      end
      else Lwt.return_unit
