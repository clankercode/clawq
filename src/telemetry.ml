type span_kind = Internal | Server | Client | Producer | Consumer

type span = {
  trace_id : string;
  span_id : string;
  parent_span_id : string option;
  name : string;
  kind : span_kind;
  start_time_ns : int64;
  end_time_ns : int64;
  attributes : (string * string) list;
  status_ok : bool;
}

type t = {
  endpoint : string;
  service_name : string;
  enabled : bool;
  buffer : span list ref;
  buffer_mutex : Lwt_mutex.t;
}

let disabled =
  {
    endpoint = "";
    service_name = "";
    enabled = false;
    buffer = ref [];
    buffer_mutex = Lwt_mutex.create ();
  }

let create ~endpoint ~service_name =
  if endpoint = "" then disabled
  else
    {
      endpoint;
      service_name;
      enabled = true;
      buffer = ref [];
      buffer_mutex = Lwt_mutex.create ();
    }

let random_hex n =
  Mirage_crypto_rng_unix.use_default ();
  let bytes = Mirage_crypto_rng.generate n in
  let buf = Buffer.create (n * 2) in
  String.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    bytes;
  Buffer.contents buf

let new_trace_id () = random_hex 16
let new_span_id () = random_hex 8
let now_ns () = Int64.of_float (Unix.gettimeofday () *. 1e9)

let start_span t ?parent_id:_ ~name:_ ~kind:_ ~attributes:_ () =
  if not t.enabled then ("", 0L)
  else
    let span_id = new_span_id () in
    let start_ns = now_ns () in
    (span_id, start_ns)

let span_kind_to_int = function
  | Internal -> 1
  | Server -> 2
  | Client -> 3
  | Producer -> 4
  | Consumer -> 5

let finish_span t ~span_id ~start_ns ~name ~kind ?parent_id ~attributes
    ~status_ok () =
  if not t.enabled then ()
  else if span_id = "" then ()
  else
    let span =
      {
        trace_id = new_trace_id ();
        span_id;
        parent_span_id = parent_id;
        name;
        kind;
        start_time_ns = start_ns;
        end_time_ns = now_ns ();
        attributes;
        status_ok;
      }
    in
    t.buffer := span :: !(t.buffer)

let span_to_otlp_json (span : span) =
  let attrs_json =
    `List
      (List.map
         (fun (k, v) ->
           `Assoc
             [
               ("key", `String k);
               ("value", `Assoc [ ("stringValue", `String v) ]);
             ])
         span.attributes)
  in
  let status_json =
    `Assoc [ ("code", `Int (if span.status_ok then 1 else 2)) ]
  in
  let fields =
    [
      ("traceId", `String span.trace_id);
      ("spanId", `String span.span_id);
      ("name", `String span.name);
      ("kind", `Int (span_kind_to_int span.kind));
      ("startTimeUnixNano", `String (Int64.to_string span.start_time_ns));
      ("endTimeUnixNano", `String (Int64.to_string span.end_time_ns));
      ("attributes", attrs_json);
      ("status", status_json);
    ]
  in
  let fields =
    match span.parent_span_id with
    | Some pid -> fields @ [ ("parentSpanId", `String pid) ]
    | None -> fields
  in
  `Assoc fields

let spans_to_otlp_json ~service_name spans =
  let resource_attrs =
    `List
      [
        `Assoc
          [
            ("key", `String "service.name");
            ("value", `Assoc [ ("stringValue", `String service_name) ]);
          ];
      ]
  in
  let scope_spans =
    `Assoc
      [
        ("scope", `Assoc [ ("name", `String "clawq") ]);
        ("spans", `List (List.map span_to_otlp_json spans));
      ]
  in
  `Assoc
    [
      ( "resourceSpans",
        `List
          [
            `Assoc
              [
                ("resource", `Assoc [ ("attributes", resource_attrs) ]);
                ("scopeSpans", `List [ scope_spans ]);
              ];
          ] );
    ]

let flush t =
  if not t.enabled then Lwt.return_unit
  else
    let open Lwt.Syntax in
    let* () =
      Lwt_util.with_lock_timeout ~label:"telemetry_flush"
        ~fatal_timeout:Lwt_util.short_fatal_timeout t.buffer_mutex (fun () ->
          let spans = !(t.buffer) in
          t.buffer := [];
          if spans = [] then Lwt.return_unit
          else
            let body =
              spans_to_otlp_json ~service_name:t.service_name spans
              |> Yojson.Safe.to_string
            in
            Lwt.catch
              (fun () ->
                let* _status, _body =
                  Http_client.post_json ~uri:t.endpoint
                    ~headers:[ ("Content-Type", "application/json") ]
                    ~body
                in
                Lwt.return_unit)
              (fun exn ->
                Logs.debug (fun m ->
                    m "Telemetry export failed: %s" (Printexc.to_string exn));
                Lwt.return_unit))
    in
    Lwt.return_unit

let buffer_size t = List.length !(t.buffer)
let maybe_flush t = if buffer_size t >= 100 then flush t else Lwt.return_unit
