type preferences = {
  favorites : string list;
  usage_counts : (string * int) list;
}

let empty = { favorites = []; usage_counts = [] }

let prefs_dir () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat home ".clawq"

let prefs_file () = Filename.concat (prefs_dir ()) "model_prefs.json"

let load () =
  let path = prefs_file () in
  if not (Sys.file_exists path) then empty
  else
    try
      let ic = open_in path in
      let s = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let json = Yojson.Safe.from_string s in
      let open Yojson.Safe.Util in
      let favorites =
        try json |> member "favorites" |> to_list |> List.map to_string
        with _ -> []
      in
      let usage_counts =
        try
          json |> member "usage_counts" |> to_assoc
          |> List.map (fun (k, v) -> (k, to_int v))
        with _ -> []
      in
      { favorites; usage_counts }
    with _ -> empty

let save prefs =
  let dir = prefs_dir () in
  (if not (Sys.file_exists dir) then try Unix.mkdir dir 0o755 with _ -> ());
  let path = prefs_file () in
  let json =
    `Assoc
      [
        ("favorites", `List (List.map (fun s -> `String s) prefs.favorites));
        ( "usage_counts",
          `Assoc (List.map (fun (k, v) -> (k, `Int v)) prefs.usage_counts) );
      ]
  in
  let oc = open_out path in
  output_string oc (Yojson.Safe.pretty_to_string json);
  output_string oc "\n";
  close_out oc

let add_favorite model =
  let prefs = load () in
  if List.mem model prefs.favorites then prefs
  else
    let prefs' = { prefs with favorites = model :: prefs.favorites } in
    save prefs';
    prefs'

let remove_favorite model =
  let prefs = load () in
  let favorites' = List.filter (fun m -> m <> model) prefs.favorites in
  let prefs' = { prefs with favorites = favorites' } in
  save prefs';
  prefs'

let toggle_favorite model =
  let prefs = load () in
  if List.mem model prefs.favorites then remove_favorite model
  else add_favorite model

let is_favorite model = List.mem model (load ()).favorites

let increment_usage model =
  let prefs = load () in
  let count =
    match List.assoc_opt model prefs.usage_counts with
    | None -> 1
    | Some n -> n + 1
  in
  let usage_counts' =
    (model, count) :: List.filter (fun (k, _) -> k <> model) prefs.usage_counts
  in
  let prefs' = { prefs with usage_counts = usage_counts' } in
  save prefs';
  prefs'

let get_usage_count model =
  match List.assoc_opt model (load ()).usage_counts with
  | None -> 0
  | Some n -> n

let ranked_by_usage () =
  let prefs = load () in
  List.sort (fun (_, a) (_, b) -> compare b a) prefs.usage_counts
  |> List.map fst

let ranked_models ?(include_favorites_first = true) () =
  let prefs = load () in
  let usage_ranked =
    List.sort (fun (_, a) (_, b) -> compare b a) prefs.usage_counts
    |> List.map fst
  in
  if include_favorites_first then
    let non_fav_usage =
      List.filter (fun m -> not (List.mem m prefs.favorites)) usage_ranked
    in
    prefs.favorites @ non_fav_usage
  else usage_ranked

let format_for_telegram ?(limit = 20) () =
  let prefs = load () in
  let ranked = ranked_models ~include_favorites_first:true () in
  let display =
    if List.length ranked > limit then
      let first = List.filter (fun m -> List.mem m prefs.favorites) ranked in
      let rest =
        List.filter (fun m -> not (List.mem m prefs.favorites)) ranked
      in
      first @ List.filteri (fun i _ -> i < limit - List.length first) rest
    else ranked
  in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "<b>Models</b>\n\n";
  if prefs.favorites = [] && prefs.usage_counts = [] then begin
    Buffer.add_string buf "No favorites or usage history yet.\n";
    Buffer.add_string buf
      "Use <code>/model set &lt;name&gt;</code> to set a model.\n";
    Buffer.add_string buf
      "Use <code>/model fav &lt;name&gt;</code> to favorite.\n"
  end
  else begin
    if prefs.favorites <> [] then begin
      Buffer.add_string buf "<b>Favorites:</b>\n";
      List.iter
        (fun m ->
          let star = "\xe2\xad\x90" in
          Buffer.add_string buf (Printf.sprintf "  %s <code>%s</code>\n" star m))
        prefs.favorites;
      Buffer.add_string buf "\n"
    end;
    let non_fav =
      List.filter (fun m -> not (List.mem m prefs.favorites)) display
    in
    if non_fav <> [] then begin
      Buffer.add_string buf "<b>Recent:</b>\n";
      Buffer.add_string buf "<blockquote expandable>\n";
      List.iter
        (fun m ->
          let count = get_usage_count m in
          Buffer.add_string buf
            (Printf.sprintf "<code>%s</code> (%d)\n" m count))
        non_fav;
      Buffer.add_string buf "</blockquote>\n"
    end
  end;
  Buffer.contents buf

let format_for_cli () =
  let prefs = load () in
  let buf = Buffer.create 512 in
  if prefs.favorites <> [] then begin
    Buffer.add_string buf "Favorites:\n";
    List.iter
      (fun m -> Buffer.add_string buf (Printf.sprintf "  * %s\n" m))
      prefs.favorites;
    Buffer.add_string buf "\n"
  end;
  if prefs.usage_counts <> [] then begin
    let ranked = ranked_by_usage () in
    Buffer.add_string buf "Usage history:\n";
    List.iter
      (fun m ->
        let count = get_usage_count m in
        Buffer.add_string buf (Printf.sprintf "  %s (%d uses)\n" m count))
      (List.filteri (fun i _ -> i < 10) ranked)
  end;
  if prefs.favorites = [] && prefs.usage_counts = [] then
    Buffer.contents buf ^ "No model preferences recorded."
  else Buffer.contents buf

let to_json () =
  let prefs = load () in
  `Assoc
    [
      ("favorites", `List (List.map (fun s -> `String s) prefs.favorites));
      ( "usage_counts",
        `Assoc (List.map (fun (k, v) -> (k, `Int v)) prefs.usage_counts) );
    ]
