(* Target resolution (spec §6.8): a create's [target] argument, a bot's name or
   a mention of it, resolved to exactly one live bot whose app is in
   [allowed_targets]. The label shown is always the operator's, never a Slack
   display name. Every Slack call goes through [Slack_standing_order_slack]. *)
open Lwt.Syntax
module T = Slack_standing_order_types
module SO = Slack_standing_order_slack
module G = Slack_standing_order_grant

type arg = User_ref of string | Name_ref of string

type error =
  | No_match of string list  (** The approved labels, in allowlist order. *)
  | Ambiguous of (string * string) list  (** (label, user_id) per candidate. *)
  | Not_allowlisted
  | Lookup_failed of SO.error  (** A failed Slack call. *)
  | Listing_incomplete
      (** users.list still had (or did not report) a cursor after [G.max_pages]
          pages. *)
  | Bad_argument

let user_id_prefixes = "UW"

(* [<@U…>] or [<@U…|label>]; every other [<…>] form (channel, here, links) is
   not a target. *)
let mention_id s =
  let n = String.length s in
  if n >= 4 && s.[1] = '@' && s.[n - 1] = '>' then
    let inner = String.sub s 2 (n - 3) in
    let id =
      match String.index_opt inner '|' with
      | Some i -> String.sub inner 0 i
      | None -> inner
    in
    if G.valid_id user_id_prefixes id then Some id else None
  else None

let parse_arg raw =
  let s = String.trim raw in
  if s = "" then None
  else if s.[0] = '<' then Option.map (fun id -> User_ref id) (mention_id s)
  else if G.valid_id user_id_prefixes s then Some (User_ref s)
  else
    let name =
      if s.[0] = '@' then String.trim (String.sub s 1 (String.length s - 1))
      else s
    in
    if name = "" then None else Some (Name_ref name)

(* Lowercase, drop a leading [@], collapse whitespace runs to one space. *)
let normalise s =
  let s = String.lowercase_ascii (String.trim s) in
  let s =
    if String.starts_with ~prefix:"@" s then String.sub s 1 (String.length s - 1)
    else s
  in
  String.split_on_char ' '
    (String.map (function '\t' | '\n' | '\r' -> ' ' | c -> c) s)
  |> List.filter (( <> ) "")
  |> String.concat " "

let names_match ~q name =
  let n = normalise name in
  n = q
  || String.starts_with ~prefix:(q ^ " ") n
  || String.starts_with ~prefix:(q ^ "(") n

(* A live, allowlisted bot: [is_bot] true, [deleted] typed false, an
   allowlisted [profile.api_app_id] and well-formed user and bot ids (an order
   whose ids don't validate could never be decoded). *)
let candidate ~(allowed : Slack_config_types.standing_order_target_config list)
    user : T.target option =
  let profile = G.get "profile" user in
  let user_id = G.field "id" user
  and app_id = G.field "api_app_id" profile
  and bot_id = G.field "bot_id" profile in
  if
    G.flag "is_bot" user
    && G.get "deleted" user = `Bool false
    && G.valid_id user_id_prefixes user_id
    && G.valid_id "B" bot_id
  then
    List.find_opt
      (fun (t : Slack_config_types.standing_order_target_config) ->
        t.app_id = app_id)
      allowed
    |> Option.map (fun (t : Slack_config_types.standing_order_target_config) ->
        { T.user_id; app_id; bot_id; label = t.label })
  else None

let names_of user (t : T.target) =
  let profile = G.get "profile" user in
  [
    G.field "name" user;
    G.field "real_name" user;
    G.field "display_name" profile;
    G.field "real_name" profile;
    t.label;
  ]

(* One users.info. A user Slack doesn't know is not an approved target. *)
let by_id ~(call : SO.call) ~allowed id =
  let+ result = call "users.info" [ ("user", `String id) ] in
  match result with
  | Error (Slack_error "user_not_found") -> Error Not_allowlisted
  | Error e -> Error (Lookup_failed e)
  | Ok json -> (
      match candidate ~allowed (G.get "user" json) with
      | Some t when t.user_id = id -> Ok t
      | _ -> Error Not_allowlisted)

(* Every users.list page, at most [G.max_pages]: a name search must see the
   whole listing, or it can't tell one match from several. *)
let by_name ~(call : SO.call) ~allowed ~q =
  let rec page cursor n acc =
    let* result =
      call "users.list" (("limit", `Int G.page_size) :: G.cursor_params cursor)
    in
    match result with
    | Error e -> Lwt.return (Error (Lookup_failed e))
    | Ok json -> (
        match G.get "members" json with
        | `List users -> (
            let acc =
              List.fold_left
                (fun acc user ->
                  match candidate ~allowed user with
                  | Some t
                    when List.exists (names_match ~q) (names_of user t)
                         && not
                              (List.exists
                                 (fun (c : T.target) -> c.user_id = t.user_id)
                                 acc) ->
                      t :: acc
                  | _ -> acc)
                acc users
            in
            match G.next_cursor json with
            | Some "" -> Lwt.return (Ok (List.rev acc))
            | Some next when n < G.max_pages -> page next (n + 1) acc
            | _ -> Lwt.return (Error Listing_incomplete))
        | _ -> Lwt.return (Error (Lookup_failed Transport)))
  in
  page "" 1 []

let resolve ~call
    ~(allowed : Slack_config_types.standing_order_target_config list) raw :
    (T.target, error) result Lwt.t =
  let labels () =
    List.map
      (fun (t : Slack_config_types.standing_order_target_config) -> t.label)
      allowed
  in
  match parse_arg raw with
  | None -> Lwt.return (Error Bad_argument)
  (* An empty allowlist denies everything, with no Slack call. *)
  | Some (User_ref _) when allowed = [] -> Lwt.return (Error Not_allowlisted)
  | Some (Name_ref _) when allowed = [] -> Lwt.return (Error (No_match []))
  | Some (User_ref id) -> by_id ~call ~allowed id
  | Some (Name_ref name) -> (
      let q = normalise name in
      if q = "" then Lwt.return (Error Bad_argument)
      else
        let+ found = by_name ~call ~allowed ~q in
        match found with
        | Error e -> Error e
        | Ok [ t ] -> Ok t
        | Ok [] -> Error (No_match (labels ()))
        | Ok several ->
            Error
              (Ambiguous
                 (List.map (fun (t : T.target) -> (t.label, t.user_id)) several))
      )
