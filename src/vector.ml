(* Vector index for semantic search using SQLite-backed embedding storage *)

let init_schema db =
  let exec sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc ->
        failwith
          (Printf.sprintf "SQLite error: %s (sql: %s)" (Sqlite3.Rc.to_string rc)
             sql)
  in
  exec
    "CREATE TABLE IF NOT EXISTS embeddings (id INTEGER PRIMARY KEY \
     AUTOINCREMENT, message_id INTEGER NOT NULL, session_key TEXT NOT NULL, \
     content_preview TEXT NOT NULL, embedding BLOB NOT NULL, created_at TEXT \
     NOT NULL DEFAULT (datetime('now')))";
  exec
    "CREATE INDEX IF NOT EXISTS idx_embeddings_session_key ON embeddings \
     (session_key)"

(* --- Cosine similarity --- *)

let cosine_similarity a b =
  let len_a = Array.length a in
  let len_b = Array.length b in
  if len_a <> len_b || len_a = 0 then 0.0
  else
    let dot = ref 0.0 in
    let norm_a = ref 0.0 in
    let norm_b = ref 0.0 in
    for i = 0 to len_a - 1 do
      dot := !dot +. (a.(i) *. b.(i));
      norm_a := !norm_a +. (a.(i) *. a.(i));
      norm_b := !norm_b +. (b.(i) *. b.(i))
    done;
    let denom = sqrt !norm_a *. sqrt !norm_b in
    if denom = 0.0 then 0.0 else !dot /. denom

(* --- Embedding serialization --- *)

let serialize_embedding (arr : float array) : string =
  let len = Array.length arr in
  let buf = Bytes.create (len * 8) in
  for i = 0 to len - 1 do
    Bytes.set_int64_le buf (i * 8) (Int64.bits_of_float arr.(i))
  done;
  Bytes.to_string buf

let deserialize_embedding (s : string) : float array =
  let byte_len = String.length s in
  if byte_len mod 8 <> 0 then [||]
  else
    let len = byte_len / 8 in
    let buf = Bytes.of_string s in
    Array.init len (fun i ->
        Int64.float_of_bits (Bytes.get_int64_le buf (i * 8)))

(* --- Embeddings API client --- *)

let fetch_embedding ~(config : Runtime_config.t) ~text =
  let open Lwt.Syntax in
  let provider_name =
    match config.memory.embedding_provider with
    | Some p -> p
    | None -> (
        match
          Runtime_config.effective_primary_provider config.agent_defaults
        with
        | Some p -> p
        | None -> "openai")
  in
  let provider =
    match List.assoc_opt provider_name config.providers with
    | Some p -> p
    | None ->
        failwith (Printf.sprintf "Vector: provider %S not found" provider_name)
  in
  let base_url =
    match provider.base_url with
    | Some u -> u
    | None -> "https://api.openai.com"
  in
  let model =
    match config.memory.embedding_model with
    | Some m -> m
    | None -> (
        match provider.default_model with
        | Some m -> m
        | None -> "text-embedding-3-small")
  in
  let uri = base_url ^ "/v1/embeddings" in
  let headers = [ ("Authorization", "Bearer " ^ provider.api_key) ] in
  let body =
    `Assoc [ ("model", `String model); ("input", `String text) ]
    |> Yojson.Safe.to_string
  in
  let* status, response_body = Http_client.post_json ~uri ~headers ~body in
  if status <> 200 then
    Lwt.fail_with
      (Printf.sprintf "Vector: embeddings API returned HTTP %d: %s" status
         response_body)
  else
    try
      let open Yojson.Safe.Util in
      let json = Yojson.Safe.from_string response_body in
      let data = json |> member "data" |> to_list in
      match data with
      | [] -> Lwt.fail_with "Vector: empty data array in embeddings response"
      | first :: _ ->
          let embedding =
            first |> member "embedding" |> to_list |> List.map to_float
            |> Array.of_list
          in
          Lwt.return embedding
    with exn ->
      Lwt.fail_with
        (Printf.sprintf "Vector: failed to parse embeddings response: %s"
           (Printexc.to_string exn))

(* --- Store embedding --- *)

let store ~db ~session_key ~message_id ~content_preview ~embedding =
  let sql =
    "INSERT INTO embeddings (message_id, session_key, content_preview, \
     embedding) VALUES (?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT message_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT content_preview));
  ignore
    (Sqlite3.bind stmt 4 (Sqlite3.Data.BLOB (serialize_embedding embedding)));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Vector: failed to store embedding: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

(* --- Vector search --- *)

let search ~db ~query_embedding ?session_key ~limit () =
  let sql, bind_session =
    match session_key with
    | Some _sk ->
        ( "SELECT content_preview, embedding FROM embeddings WHERE session_key \
           = ?",
          true )
    | None -> ("SELECT content_preview, embedding FROM embeddings", false)
  in
  let stmt = Sqlite3.prepare db sql in
  (if bind_session then
     match session_key with
     | Some sk -> ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT sk))
     | None -> ());
  let results = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let content_preview =
      match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let embedding_blob =
      match Sqlite3.column stmt 1 with Sqlite3.Data.BLOB s -> s | _ -> ""
    in
    let emb = deserialize_embedding embedding_blob in
    let sim = cosine_similarity query_embedding emb in
    results := (content_preview, sim) :: !results
  done;
  ignore (Sqlite3.finalize stmt);
  let sorted =
    List.sort (fun (_, s1) (_, s2) -> Float.compare s2 s1) !results
  in
  if limit > 0 then List.filteri (fun i _ -> i < limit) sorted else sorted

(* --- Hybrid search merge --- *)

let merge_results ~keyword_results ~vector_results ~keyword_weight
    ~vector_weight =
  let kw_w = Float.of_int keyword_weight /. 100.0 in
  let vec_w = Float.of_int vector_weight /. 100.0 in
  (* Assign rank-based scores to keyword results: 1.0, 0.9, 0.8, ... *)
  let n_kw = List.length keyword_results in
  let kw_scored =
    List.mapi
      (fun i content ->
        let score =
          if n_kw <= 1 then 1.0
          else Float.max 0.0 (1.0 -. (Float.of_int i *. 0.1))
        in
        (content, score))
      keyword_results
  in
  (* Normalize vector scores to [0,1] *)
  let vec_scored =
    match vector_results with
    | [] -> []
    | _ ->
        let max_sim =
          List.fold_left
            (fun acc (_, s) -> Float.max acc s)
            Float.neg_infinity vector_results
        in
        let min_sim =
          List.fold_left
            (fun acc (_, s) -> Float.min acc s)
            Float.infinity vector_results
        in
        let range = max_sim -. min_sim in
        List.map
          (fun (content, sim) ->
            let norm = if range = 0.0 then 1.0 else (sim -. min_sim) /. range in
            (content, norm))
          vector_results
  in
  (* Build a map from content -> (kw_score, vec_score) *)
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun (content, score) ->
      let kw_s, vec_s =
        match Hashtbl.find_opt tbl content with
        | Some (k, v) -> (k, v)
        | None -> (0.0, 0.0)
      in
      Hashtbl.replace tbl content (Float.max kw_s score, vec_s))
    kw_scored;
  List.iter
    (fun (content, score) ->
      let kw_s, vec_s =
        match Hashtbl.find_opt tbl content with
        | Some (k, v) -> (k, v)
        | None -> (0.0, 0.0)
      in
      Hashtbl.replace tbl content (kw_s, Float.max vec_s score))
    vec_scored;
  (* Compute final weighted scores and sort *)
  let combined =
    Hashtbl.fold
      (fun content (kw_s, vec_s) acc ->
        let final_score = (kw_w *. kw_s) +. (vec_w *. vec_s) in
        (content, final_score) :: acc)
      tbl []
  in
  let sorted =
    List.sort (fun (_, s1) (_, s2) -> Float.compare s2 s1) combined
  in
  List.map fst sorted
