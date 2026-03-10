val postmortems_dir : unit -> string
(** ~/.clawq/postmortems/ — creates directory if needed *)

val format_history_text : Provider.message list -> string
(** Format recent messages from history into a readable text summary for
    postmortem evidence. Truncated to 3000 chars / 15 messages. *)

val write_doc :
  session_key:string ->
  pattern:string ->
  evidence_summary:string ->
  correction:string ->
  string Lwt.t
(** Write a postmortem markdown file to postmortems_dir. Returns the file path
    written. *)

val make_postmortem_prompt :
  session_key:string ->
  reason:string ->
  doc_path:string ->
  history_text:string ->
  unit ->
  string
(** Build the system prompt for a postmortem analysis agent. *)
