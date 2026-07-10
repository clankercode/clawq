(* B768: kind -> session host lookup. The direct process-group host is always
   available; Herdr (B769) and tmux (B770) adapters register here. Tests may
   register fake hosts and must restore the table via [with_host]. *)

let table : (string, Session_host.t) Hashtbl.t = Hashtbl.create 4
let register (host : Session_host.t) = Hashtbl.replace table host.kind host
let () = register Session_host_direct.host

let find session_kind : Session_host.t option =
  let key = String.lowercase_ascii (String.trim session_kind) in
  let key = if key = "" then Session_host_direct.kind else key in
  Hashtbl.find_opt table key

let known_kinds () =
  Hashtbl.fold (fun key _ acc -> key :: acc) table []
  |> List.sort String.compare

let unknown_kind_error session_kind =
  Printf.sprintf
    "Unknown session host kind %S. Known kinds: %s. Use one of those, or leave \
     the host unset to use the default direct process host."
    session_kind
    (String.concat ", " (known_kinds ()))

(* Register [host] for the duration of [f], then restore the previous
   binding. Test-only helper in spirit, but safe generally. *)
let with_host (host : Session_host.t) f =
  let previous = Hashtbl.find_opt table host.kind in
  register host;
  Fun.protect
    ~finally:(fun () ->
      match previous with
      | Some prev -> Hashtbl.replace table host.kind prev
      | None -> Hashtbl.remove table host.kind)
    f
