let normalize_path path =
  let parts = String.split_on_char '/' path in
  let is_abs = String.length path > 0 && path.[0] = '/' in
  let rec resolve acc = function
    | [] -> List.rev acc
    | "." :: rest -> resolve acc rest
    | ".." :: rest -> (
        match acc with _ :: tl -> resolve tl rest | [] -> resolve [] rest)
    | "" :: rest -> resolve acc rest
    | part :: rest -> resolve (part :: acc) rest
  in
  let resolved = resolve [] parts in
  let joined = String.concat "/" resolved in
  if is_abs then "/" ^ joined else joined

(** [glob_match_segment pat s] matches a single path segment against a glob
    pattern segment. Supports [*] (match any chars) and [?] (match one char). *)
let glob_match_segment pat s =
  let pl = String.length pat and sl = String.length s in
  let rec go pi si =
    if pi = pl then si = sl
    else
      match pat.[pi] with
      | '*' ->
          let rec try_star j =
            if j > sl then false
            else if go (pi + 1) j then true
            else try_star (j + 1)
          in
          try_star si
      | '?' -> si < sl && go (pi + 1) (si + 1)
      | c -> si < sl && c = s.[si] && go (pi + 1) (si + 1)
  in
  go 0 0

(** [glob_match_segs pats parts] matches a list of glob pattern segments against
    a list of path segments. Supports [**] to match zero or more path segments.
*)
let rec glob_match_segs pats parts =
  match (pats, parts) with
  | [], [] -> true
  | [ "**" ], _ -> true
  | [], _ -> false
  | "**" :: rest_pats, parts -> (
      glob_match_segs rest_pats parts
      ||
      match parts with
      | [] -> false
      | _ :: rest_parts -> glob_match_segs ("**" :: rest_pats) rest_parts)
  | _, [] -> false
  | pat :: rest_pats, part :: rest_parts ->
      glob_match_segment pat part && glob_match_segs rest_pats rest_parts

(** [glob_matches_path ~pattern path] matches a full path against a glob pattern
    string. Both are split on [/] and matched segment-by-segment. *)
let glob_matches_path ~pattern path =
  let split s = String.split_on_char '/' s |> List.filter (fun x -> x <> "") in
  glob_match_segs (split pattern) (split path)
