(* B772: repository-owned execution and publication policy.

   A repository declares how agent work against it may be executed and
   published in `.clawq/publication-policy.json` at its root. The policy is
   an input read by the trusted publisher and prompt builder — never an
   instruction the model can override. Existing background-task callers keep
   their behavior through [compat_default], an explicit policy encoding the
   legacy delegate assumptions, rather than hardcoded prompt text. *)

type publication_mode =
  | Reply_only  (** never create branches or PRs for this repo *)
  | Draft_pr  (** code-changing results become a draft PR *)

type t = {
  base_branch : string option;
      (** None: resolve from repository metadata (GitHub default_branch or the
          checkout's origin HEAD); never hardcode master. *)
  publication : publication_mode;
  branch_prefix : string;  (** restricted namespace for published branches *)
  allow_rebase : bool;
  allow_force_push : bool;
  allow_direct_merge : bool;
  allow_automerge : bool;
  forbidden_commands : string list;
  forbidden_labels : string list;
  validation : string list;  (** expectations, e.g. ["make test"] *)
}

let policy_file_name = ".clawq/publication-policy.json"

(* Explicit legacy-compatible policy: what the generic delegate path always
   assumed. Used when a repository declares nothing. *)
let compat_default =
  {
    base_branch = None;
    publication = Draft_pr;
    branch_prefix = "clawq/";
    allow_rebase = true;
    allow_force_push = false;
    allow_direct_merge = false;
    allow_automerge = true;
    forbidden_commands = [];
    forbidden_labels = [];
    validation = [];
  }

let publication_mode_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "reply_only" | "reply-only" | "reply" -> Some Reply_only
  | "draft_pr" | "draft-pr" | "draft" -> Some Draft_pr
  | _ -> None

let string_of_publication_mode = function
  | Reply_only -> "reply_only"
  | Draft_pr -> "draft_pr"

let of_json (json : Yojson.Safe.t) : (t, string) result =
  let open Yojson.Safe.Util in
  try
    let str_opt key =
      match member key json with
      | `String s when String.trim s <> "" -> Some s
      | _ -> None
    in
    let bool_or key default =
      match member key json with `Bool b -> b | _ -> default
    in
    let str_list key =
      match member key json with
      | `List items ->
          List.filter_map
            (function
              | `String s when String.trim s <> "" -> Some s | _ -> None)
            items
      | _ -> []
    in
    let publication =
      match str_opt "publication" with
      | None -> compat_default.publication
      | Some raw -> (
          match publication_mode_of_string raw with
          | Some mode -> mode
          | None ->
              failwith
                (Printf.sprintf
                   "invalid \"publication\" value %S (expected \"reply_only\" \
                    or \"draft_pr\")"
                   raw))
    in
    let branch_prefix =
      match str_opt "branch_prefix" with
      | Some p when String_util.contains p ".." || String.contains p ' ' ->
          failwith (Printf.sprintf "invalid \"branch_prefix\" %S" p)
      | Some p -> p
      | None -> compat_default.branch_prefix
    in
    Ok
      {
        base_branch = str_opt "base_branch";
        publication;
        branch_prefix;
        allow_rebase = bool_or "allow_rebase" compat_default.allow_rebase;
        allow_force_push = bool_or "allow_force_push" false;
        allow_direct_merge = bool_or "allow_direct_merge" false;
        allow_automerge =
          bool_or "allow_automerge" compat_default.allow_automerge;
        forbidden_commands = str_list "forbidden_commands";
        forbidden_labels = str_list "forbidden_labels";
        validation = str_list "validation";
      }
  with
  | Failure msg -> Error msg
  | exn -> Error (Printexc.to_string exn)

(** Load the repository's policy from [repo_path]. Missing file yields the
    explicit compatibility default; a malformed file is an error (a repo that
    declares a policy must not silently fall back to permissive defaults). *)
let load ~repo_path : (t, string) result =
  let path = Filename.concat repo_path policy_file_name in
  if not (Sys.file_exists path) then Ok compat_default
  else
    match Yojson.Safe.from_file path with
    | exception exn ->
        Error
          (Printf.sprintf
             "Failed to parse %s: %s. Fix the JSON or remove the file to use \
              the compatibility default policy."
             path (Printexc.to_string exn))
    | json -> (
        match of_json json with
        | Ok policy -> Ok policy
        | Error msg -> Error (Printf.sprintf "%s: %s" path msg))

(** Deterministic restricted branch name for a work item: retries of the same
    logical work reuse the same branch, making publication idempotent. *)
let work_item_branch (policy : t) ~work_item_id =
  Printf.sprintf "%swi-%d" policy.branch_prefix work_item_id

(** Execution-contract prompt fragment derived from policy. The agent works only
    in its worktree; publication is the trusted publisher's decision. *)
let prompt_fragment (policy : t) ~base_branch =
  let forbid flag text = if flag then None else Some text in
  let lines =
    [
      Some "## Execution contract (repository policy)";
      Some
        (Printf.sprintf
           "- Work only inside the current worktree. Base branch: %s."
           base_branch);
      Some
        "- Commit your changes locally with clear messages. Do NOT push, do \
         NOT open pull requests, do NOT merge: a trusted publisher decides \
         what is published after validation.";
      forbid policy.allow_rebase "- Do NOT rebase existing history.";
      forbid policy.allow_force_push "- Never force-push or rewrite history.";
      forbid policy.allow_direct_merge "- Never merge branches directly.";
      (if policy.forbidden_commands = [] then None
       else
         Some
           ("- Forbidden commands (never run): "
           ^ String.concat ", " policy.forbidden_commands));
      (if policy.validation = [] then None
       else
         Some
           ("- Before finishing, validate your work: "
           ^ String.concat "; " policy.validation));
      Some
        "- If no code change is needed, make no commits and reply with the \
         answer or plan instead.";
    ]
  in
  String.concat "\n" (List.filter_map Fun.id lines)
