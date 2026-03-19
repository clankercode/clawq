(* cdp_client.ml — Chrome DevTools Protocol client over pipe transport *)

let src = Logs.Src.create "cdp" ~doc:"CDP client"

module Log = (val Logs.src_log src)

type page = { target_id : string; session_id : string }

type t = {
  pid : int;
  read_ch : Lwt_io.input_channel;
  write_ch : Lwt_io.output_channel;
  write_mutex : Lwt_mutex.t;
  pending : (int, Yojson.Safe.t Lwt.u) Hashtbl.t;
  events : (string, Yojson.Safe.t -> unit) Hashtbl.t;
  mutable next_id : int;
  mutable pages : (string * page) list;
  mutable current_page : string;
  mutable scripts : (string * string) list;
  mutable last_activity : float;
  user_data_dir : string;
  mutable closed : bool;
}

let close_noerr fd = try Unix.close fd with _ -> ()

let find_chromium ?configured_path () =
  match configured_path with
  | Some p when Sys.file_exists p -> Some p
  | _ -> (
      let candidates =
        [
          "/usr/bin/chromium";
          "/usr/bin/google-chrome-stable";
          "/usr/bin/google-chrome";
          "/usr/bin/chromium-browser";
        ]
      in
      match List.find_opt Sys.file_exists candidates with
      | Some p -> Some p
      | None ->
          let path =
            try Sys.getenv "PATH"
            with Not_found -> "/usr/local/bin:/usr/bin:/bin"
          in
          let dirs =
            String.split_on_char ':' path |> List.filter (fun s -> s <> "")
          in
          List.find_map
            (fun dir ->
              let c = Filename.concat dir "chromium" in
              if Sys.file_exists c then Some c
              else
                let c2 = Filename.concat dir "google-chrome" in
                if Sys.file_exists c2 then Some c2 else None)
            dirs)

(* Read a single null-byte-delimited CDP message from the pipe *)
let read_message ch =
  let buf = Buffer.create 4096 in
  let rec loop () =
    let open Lwt.Syntax in
    let* c = Lwt_io.read_char_opt ch in
    match c with
    | None -> Lwt.return_none
    | Some '\000' ->
        let msg = Buffer.contents buf in
        if String.length msg = 0 then loop () else Lwt.return_some msg
    | Some c ->
        Buffer.add_char buf c;
        loop ()
  in
  loop ()

let start_read_loop t =
  let rec loop () =
    let open Lwt.Syntax in
    let* msg_opt = read_message t.read_ch in
    match msg_opt with
    | None ->
        Log.info (fun m -> m "[cdp] pipe EOF");
        Hashtbl.iter
          (fun _id resolver ->
            Lwt.wakeup_later_exn resolver (Failure "CDP pipe closed"))
          t.pending;
        Hashtbl.clear t.pending;
        Lwt.return_unit
    | Some raw -> (
        Log.debug (fun m ->
            m "[cdp] <- %s"
              (if String.length raw > 500 then String.sub raw 0 500 ^ "..."
               else raw));
        match Yojson.Safe.from_string raw with
        | json ->
            let open Yojson.Safe.Util in
            (match json |> member "id" with
            | `Int id -> (
                match Hashtbl.find_opt t.pending id with
                | Some resolver -> (
                    Hashtbl.remove t.pending id;
                    match json |> member "error" with
                    | `Null | `Assoc [] ->
                        Lwt.wakeup_later resolver (json |> member "result")
                    | err ->
                        let msg =
                          try err |> member "message" |> to_string
                          with _ -> Yojson.Safe.to_string err
                        in
                        Lwt.wakeup_later_exn resolver
                          (Failure ("CDP error: " ^ msg)))
                | None ->
                    Log.debug (fun m -> m "[cdp] response for unknown id %d" id)
                )
            | _ -> (
                match json |> member "method" with
                | `String meth -> (
                    match Hashtbl.find_opt t.events meth with
                    | Some handler -> handler (json |> member "params")
                    | None -> ())
                | _ -> ()));
            loop ()
        | exception exn ->
            Log.err (fun m ->
                m "[cdp] JSON parse error: %s" (Printexc.to_string exn));
            loop ())
  in
  loop ()

let send_raw t json_str =
  let open Lwt.Syntax in
  Lwt_util.with_lock_timeout ~label:"cdp_write"
    ~fatal_timeout:Lwt_util.short_fatal_timeout t.write_mutex (fun () ->
      Log.debug (fun m ->
          m "[cdp] -> %s"
            (if String.length json_str > 500 then
               String.sub json_str 0 500 ^ "..."
             else json_str));
      let* () = Lwt_io.write t.write_ch json_str in
      let* () = Lwt_io.write_char t.write_ch '\000' in
      Lwt_io.flush t.write_ch)

let send_command t ~method_ ?params ?session_id ?(timeout_s = 30.0) () =
  if t.closed then Lwt.fail_with "CDP client is closed"
  else
    let open Lwt.Syntax in
    let id = t.next_id in
    t.next_id <- t.next_id + 1;
    let fields = [ ("id", `Int id); ("method", `String method_) ] in
    let fields =
      match params with Some p -> fields @ [ ("params", p) ] | None -> fields
    in
    let fields =
      match session_id with
      | Some sid -> fields @ [ ("sessionId", `String sid) ]
      | None -> fields
    in
    let json_str = Yojson.Safe.to_string (`Assoc fields) in
    let promise, resolver = Lwt.wait () in
    Hashtbl.replace t.pending id resolver;
    let* () = send_raw t json_str in
    t.last_activity <- Unix.gettimeofday ();
    Lwt.pick
      [
        promise;
        (let* () = Lwt_unix.sleep timeout_s in
         Hashtbl.remove t.pending id;
         Lwt.fail_with
           (Printf.sprintf "CDP command %s timed out after %.0fs" method_
              timeout_s));
      ]

let current_session_id t =
  match List.assoc_opt t.current_page t.pages with
  | Some page -> Some page.session_id
  | None -> None

let send_page_command t ~method_ ?params ?(timeout_s = 30.0) () =
  match current_session_id t with
  | None -> Lwt.fail_with (Printf.sprintf "No page named %S" t.current_page)
  | Some session_id -> send_command t ~method_ ?params ~session_id ~timeout_s ()

let mkdir_p path =
  try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let launch ?configured_path ?(timeout_s = 30.0) () =
  let open Lwt.Syntax in
  match find_chromium ?configured_path () with
  | None ->
      Lwt.fail_with
        "Error: chromium not found. Install chromium or set \
         browser.chromium_path in config."
  | Some chromium_path -> (
      let hash =
        Printf.sprintf "%f_%d" (Unix.gettimeofday ()) (Unix.getpid ())
      in
      let hash = Digest.to_hex (Digest.string hash) in
      let user_data_dir =
        Filename.concat (Dot_dir.sub "browser-profiles") (String.sub hash 0 12)
      in
      mkdir_p (Dot_dir.ensure ());
      mkdir_p (Dot_dir.sub "browser-profiles");
      mkdir_p user_data_dir;
      mkdir_p (Dot_dir.sub "screenshots");
      (* Pipes for CDP: chrome reads FD 3, writes FD 4 *)
      let to_chrome_r, to_chrome_w = Unix.pipe ~cloexec:false () in
      let from_chrome_r, from_chrome_w = Unix.pipe ~cloexec:false () in
      let fd_3 : Unix.file_descr = Obj.magic 3 in
      let fd_4 : Unix.file_descr = Obj.magic 4 in
      let argv =
        [|
          chromium_path;
          "--headless=new";
          "--disable-gpu";
          "--no-sandbox";
          "--disable-dev-shm-usage";
          "--remote-debugging-pipe";
          "--disable-extensions";
          "--disable-background-networking";
          "--disable-default-apps";
          "--no-first-run";
          "--user-data-dir=" ^ user_data_dir;
        |]
      in
      let env = Unix.environment () in
      match Unix.fork () with
      | 0 -> (
          let setup () =
            ignore (Unix.setsid ());
            let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
            Unix.dup2 devnull Unix.stdin;
            Unix.dup2 devnull Unix.stdout;
            Unix.dup2 devnull Unix.stderr;
            close_noerr devnull;
            (* Set up FD 3 (chrome reads) and FD 4 (chrome writes) *)
            if to_chrome_r <> fd_3 then (
              Unix.dup2 to_chrome_r fd_3;
              close_noerr to_chrome_r);
            if from_chrome_w <> fd_4 then (
              Unix.dup2 from_chrome_w fd_4;
              close_noerr from_chrome_w);
            close_noerr to_chrome_w;
            close_noerr from_chrome_r;
            Unix.execve chromium_path argv env
          in
          try setup ()
          with exn ->
            let msg = Printexc.to_string exn ^ "\n" in
            ignore (Unix.write_substring Unix.stderr msg 0 (String.length msg));
            exit 127)
      | pid -> (
          close_noerr to_chrome_r;
          close_noerr from_chrome_w;
          let read_ch =
            Lwt_io.of_fd ~mode:Lwt_io.Input
              (Lwt_unix.of_unix_file_descr from_chrome_r)
          in
          let write_ch =
            Lwt_io.of_fd ~mode:Lwt_io.Output
              (Lwt_unix.of_unix_file_descr to_chrome_w)
          in
          let t =
            {
              pid;
              read_ch;
              write_ch;
              write_mutex = Lwt_mutex.create ();
              pending = Hashtbl.create 16;
              events = Hashtbl.create 16;
              next_id = 1;
              pages = [];
              current_page = "main";
              scripts = [];
              last_activity = Unix.gettimeofday ();
              user_data_dir;
              closed = false;
            }
          in
          Lwt.async (fun () ->
              Lwt.catch
                (fun () -> start_read_loop t)
                (fun exn ->
                  Log.err (fun m ->
                      m "[cdp] read loop error: %s" (Printexc.to_string exn));
                  Lwt.return_unit));
          let startup_deadline = Unix.gettimeofday () +. timeout_s in
          let rec wait_for_page_target () =
            let open Yojson.Safe.Util in
            let remaining_s = startup_deadline -. Unix.gettimeofday () in
            if remaining_s <= 0.0 then Lwt.return_none
            else
              let* targets_result =
                send_command t ~method_:"Target.getTargets"
                  ~timeout_s:remaining_s ()
              in
              let targets = targets_result |> member "targetInfos" |> to_list in
              match
                List.find_opt
                  (fun tgt ->
                    try tgt |> member "type" |> to_string = "page"
                    with _ -> false)
                  targets
              with
              | Some target -> Lwt.return_some target
              | None ->
                  let* () = Lwt_unix.sleep 0.1 in
                  wait_for_page_target ()
          in
          let* page_target = wait_for_page_target () in
          match page_target with
          | None ->
              Process_group.signal_group pid Sys.sigterm;
              Lwt.fail_with
                (Printf.sprintf
                   "No page target found after chromium launch within %.1fs"
                   timeout_s)
          | Some target ->
              let open Yojson.Safe.Util in
              let target_id = target |> member "targetId" |> to_string in
              let* attach_result =
                send_command t ~method_:"Target.attachToTarget"
                  ~params:
                    (`Assoc
                       [
                         ("targetId", `String target_id); ("flatten", `Bool true);
                       ])
                  ~timeout_s ()
              in
              let session_id =
                attach_result |> member "sessionId" |> to_string
              in
              let page = { target_id; session_id } in
              t.pages <- [ ("main", page) ];
              let* _ =
                send_command t ~method_:"Page.enable" ~session_id ~timeout_s ()
              in
              let* _ =
                send_command t ~method_:"Runtime.enable" ~session_id ~timeout_s
                  ()
              in
              Log.info (fun m ->
                  m "[cdp] browser launched (pid=%d, session=%s)" pid session_id);
              Lwt.return t))

let close t =
  if t.closed then Lwt.return_unit
  else begin
    t.closed <- true;
    Log.info (fun m -> m "[cdp] closing browser (pid=%d)" t.pid);
    Process_group.signal_group t.pid Sys.sigterm;
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 0.2 in
    Process_group.signal_group t.pid Sys.sigkill;
    let* () =
      Lwt.catch (fun () -> Lwt_io.close t.read_ch) (fun _ -> Lwt.return_unit)
    in
    let* () =
      Lwt.catch (fun () -> Lwt_io.close t.write_ch) (fun _ -> Lwt.return_unit)
    in
    (* Reap zombie *)
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            let* _ = Lwt_unix.waitpid [] t.pid in
            Lwt.return_unit)
          (fun _ -> Lwt.return_unit));
    (* Clean up user data dir *)
    (try
       ignore
         (Sys.command
            (Printf.sprintf "rm -rf %s" (Filename.quote t.user_data_dir)))
     with _ -> ());
    Lwt.return_unit
  end

(* --- High-level CDP wrappers --- *)

let navigate t ~url ?(timeout_s = 30.0) () =
  let open Lwt.Syntax in
  let load_promise, load_resolver = Lwt.wait () in
  let event_key = "Page.loadEventFired" in
  Hashtbl.replace t.events event_key (fun _params ->
      Lwt.wakeup_later load_resolver ());
  let* _ =
    send_page_command t ~method_:"Page.navigate"
      ~params:(`Assoc [ ("url", `String url) ])
      ~timeout_s ()
  in
  let* () =
    Lwt.pick
      [
        load_promise;
        (let* () = Lwt_unix.sleep timeout_s in
         Lwt.return_unit);
      ]
  in
  Hashtbl.remove t.events event_key;
  Lwt.return_unit

let evaluate t ~expression ?(timeout_s = 30.0) () =
  let open Lwt.Syntax in
  let params =
    `Assoc
      [
        ("expression", `String expression);
        ("returnByValue", `Bool true);
        ("awaitPromise", `Bool true);
      ]
  in
  let* result =
    send_page_command t ~method_:"Runtime.evaluate" ~params ~timeout_s ()
  in
  let open Yojson.Safe.Util in
  let res = result |> member "result" in
  let exc = result |> member "exceptionDetails" in
  if exc <> `Null then
    let text =
      try exc |> member "text" |> to_string with _ -> "JS exception"
    in
    Lwt.return (Error text)
  else
    let value = res |> member "value" in
    let typ = try res |> member "type" |> to_string with _ -> "undefined" in
    match typ with
    | "undefined" -> Lwt.return (Ok "undefined")
    | "string" -> Lwt.return (Ok (to_string value))
    | _ -> Lwt.return (Ok (Yojson.Safe.to_string value))

let screenshot t ?selector ?(full_page = true) ?(timeout_s = 30.0) () =
  let open Lwt.Syntax in
  let params =
    let base = [ ("format", `String "png") ] in
    let base =
      if full_page && selector = None then
        base @ [ ("captureBeyondViewport", `Bool true) ]
      else base
    in
    match selector with
    | Some sel -> (
        let* clip_result =
          evaluate t
            ~expression:
              (Printf.sprintf
                 {|(function() {
                  var el = document.querySelector(%s);
                  if (!el) return null;
                  var r = el.getBoundingClientRect();
                  return {x: r.x, y: r.y, width: r.width, height: r.height, scale: 1};
                })()|}
                 (Yojson.Safe.to_string (`String sel)))
            ~timeout_s ()
        in
        match clip_result with
        | Error _ | Ok "null" -> Lwt.return (`Assoc base)
        | Ok json_str -> (
            match Yojson.Safe.from_string json_str with
            | clip -> Lwt.return (`Assoc (base @ [ ("clip", clip) ]))
            | exception _ -> Lwt.return (`Assoc base)))
    | None -> Lwt.return (`Assoc base)
  in
  let* params = params in
  let* result =
    send_page_command t ~method_:"Page.captureScreenshot" ~params ~timeout_s ()
  in
  let open Yojson.Safe.Util in
  let data = result |> member "data" |> to_string in
  let decoded = Base64.decode_exn data in
  let timestamp =
    int_of_float (Unix.gettimeofday () *. 1000.0) |> string_of_int
  in
  let path =
    Filename.concat (Dot_dir.sub "screenshots") ("browser-" ^ timestamp ^ ".png")
  in
  let oc = open_out_bin path in
  output_string oc decoded;
  close_out oc;
  Lwt.return path

let get_content t ?selector ?(timeout_s = 30.0) () =
  let expr =
    match selector with
    | Some sel ->
        Printf.sprintf
          {|(function() {
            var el = document.querySelector(%s);
            return el ? el.innerText : "Error: element not found for selector " + %s;
          })()|}
          (Yojson.Safe.to_string (`String sel))
          (Yojson.Safe.to_string (`String sel))
    | None -> "document.body.innerText"
  in
  let open Lwt.Syntax in
  let* result = evaluate t ~expression:expr ~timeout_s () in
  match result with
  | Error e -> Lwt.return ("Error: " ^ e)
  | Ok content ->
      if String.length content > 20000 then
        Lwt.return (String.sub content 0 20000 ^ "\n... (truncated)")
      else Lwt.return content

let click t ~selector ?(timeout_s = 30.0) () =
  let expr =
    Printf.sprintf
      {|(function() {
        var el = document.querySelector(%s);
        if (!el) return "not_found";
        el.click();
        return "clicked";
      })()|}
      (Yojson.Safe.to_string (`String selector))
  in
  let open Lwt.Syntax in
  let* result = evaluate t ~expression:expr ~timeout_s () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok "not_found" ->
      Lwt.return
        (Error (Printf.sprintf "Element not found for selector %S" selector))
  | Ok s -> Lwt.return (Ok s)

let fill t ~selector ~text ?(timeout_s = 30.0) () =
  let expr =
    Printf.sprintf
      {|(function() {
        var el = document.querySelector(%s);
        if (!el) return "not_found";
        el.value = %s;
        el.dispatchEvent(new Event('input', {bubbles: true}));
        el.dispatchEvent(new Event('change', {bubbles: true}));
        return "filled";
      })()|}
      (Yojson.Safe.to_string (`String selector))
      (Yojson.Safe.to_string (`String text))
  in
  let open Lwt.Syntax in
  let* result = evaluate t ~expression:expr ~timeout_s () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok "not_found" ->
      Lwt.return
        (Error (Printf.sprintf "Element not found for selector %S" selector))
  | Ok s -> Lwt.return (Ok s)

let wait_for_selector t ~selector ?(timeout_s = 10.0) () =
  let open Lwt.Syntax in
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec poll () =
    let* result =
      evaluate t
        ~expression:
          (Printf.sprintf "!!document.querySelector(%s)"
             (Yojson.Safe.to_string (`String selector)))
        ~timeout_s:5.0 ()
    in
    match result with
    | Ok "true" -> Lwt.return (Ok ())
    | _ ->
        if Unix.gettimeofday () >= deadline then
          Lwt.return
            (Error (Printf.sprintf "Timeout waiting for selector %S" selector))
        else
          let* () = Lwt_unix.sleep 0.2 in
          poll ()
  in
  poll ()

let get_accessibility_tree t ?(max_depth = 5) ?(timeout_s = 10.0) () =
  let expr =
    Printf.sprintf
      {|(function() {
        var result = [];
        var idx = 0;
        function walk(node, depth) {
          if (depth > %d) return;
          var tag = node.tagName ? node.tagName.toLowerCase() : '';
          var role = node.getAttribute ? (node.getAttribute('role') || '') : '';
          var interactable = ['a','button','input','select','textarea','details','summary'].indexOf(tag) >= 0
            || role === 'button' || role === 'link' || role === 'textbox' || role === 'checkbox'
            || (node.getAttribute && node.getAttribute('onclick'))
            || (node.getAttribute && node.getAttribute('tabindex'));
          if (interactable && tag) {
            idx++;
            var info = idx + '. ' + tag;
            if (node.type) info += '[type=' + node.type + ']';
            if (node.id) info += '#' + node.id;
            if (node.className && typeof node.className === 'string')
              info += '.' + node.className.split(' ').join('.');
            var text = (node.innerText || node.value || node.placeholder || '').substring(0, 80);
            if (text) info += ' "' + text.replace(/\n/g, ' ') + '"';
            result.push(info);
          }
          for (var i = 0; i < node.children.length; i++) walk(node.children[i], depth + 1);
        }
        walk(document.body, 0);
        return result.join('\n');
      })()|}
      max_depth
  in
  let open Lwt.Syntax in
  let* result = evaluate t ~expression:expr ~timeout_s () in
  match result with
  | Ok s -> Lwt.return s
  | Error e -> Lwt.return ("Error: " ^ e)

let load_script t ~source ?name ?(autoload = true) ?(timeout_s = 10.0) () =
  let open Lwt.Syntax in
  if autoload then begin
    let* result =
      send_page_command t ~method_:"Page.addScriptToEvaluateOnNewDocument"
        ~params:(`Assoc [ ("source", `String source) ])
        ~timeout_s ()
    in
    let open Yojson.Safe.Util in
    let identifier = result |> member "identifier" |> to_string in
    let script_name =
      match name with
      | Some n -> n
      | None -> Printf.sprintf "script-%d" (List.length t.scripts)
    in
    t.scripts <- (script_name, identifier) :: t.scripts;
    (* Also evaluate immediately on current page *)
    let* _ = evaluate t ~expression:source ~timeout_s () in
    Lwt.return
      (Ok (Printf.sprintf "Script %S loaded (id=%s)" script_name identifier))
  end
  else begin
    let* result = evaluate t ~expression:source ~timeout_s () in
    match result with
    | Ok v -> Lwt.return (Ok v)
    | Error e -> Lwt.return (Error e)
  end

let list_scripts t = List.map (fun (name, id) -> (name, id)) t.scripts

let unload_script t ~name ?(timeout_s = 10.0) () =
  let open Lwt.Syntax in
  match List.assoc_opt name t.scripts with
  | None -> Lwt.return (Error (Printf.sprintf "No script named %S" name))
  | Some identifier ->
      let* _ =
        send_page_command t ~method_:"Page.removeScriptToEvaluateOnNewDocument"
          ~params:(`Assoc [ ("identifier", `String identifier) ])
          ~timeout_s ()
      in
      t.scripts <- List.filter (fun (n, _) -> n <> name) t.scripts;
      Lwt.return (Ok (Printf.sprintf "Script %S unloaded" name))

(* --- Tab management --- *)

let create_tab t ?name ~url ?(timeout_s = 30.0) () =
  let open Lwt.Syntax in
  let tab_name =
    match name with
    | Some n -> n
    | None -> Printf.sprintf "tab-%d" (List.length t.pages)
  in
  if List.mem_assoc tab_name t.pages then
    Lwt.fail_with (Printf.sprintf "Tab %S already exists" tab_name)
  else begin
    let* result =
      send_command t ~method_:"Target.createTarget"
        ~params:(`Assoc [ ("url", `String url) ])
        ~timeout_s ()
    in
    let open Yojson.Safe.Util in
    let target_id = result |> member "targetId" |> to_string in
    let* attach_result =
      send_command t ~method_:"Target.attachToTarget"
        ~params:
          (`Assoc [ ("targetId", `String target_id); ("flatten", `Bool true) ])
        ~timeout_s ()
    in
    let session_id = attach_result |> member "sessionId" |> to_string in
    let page = { target_id; session_id } in
    t.pages <- t.pages @ [ (tab_name, page) ];
    t.current_page <- tab_name;
    let* _ = send_command t ~method_:"Page.enable" ~session_id ~timeout_s () in
    let* _ =
      send_command t ~method_:"Runtime.enable" ~session_id ~timeout_s ()
    in
    Lwt.return tab_name
  end

let switch_tab t ~name =
  if not (List.mem_assoc name t.pages) then
    Error (Printf.sprintf "Tab %S not found" name)
  else begin
    t.current_page <- name;
    Ok ()
  end

let close_tab t ~name ?(timeout_s = 10.0) () =
  let open Lwt.Syntax in
  match List.assoc_opt name t.pages with
  | None -> Lwt.return (Error (Printf.sprintf "Tab %S not found" name))
  | Some page ->
      let* _ =
        send_command t ~method_:"Target.closeTarget"
          ~params:(`Assoc [ ("targetId", `String page.target_id) ])
          ~timeout_s ()
      in
      t.pages <- List.filter (fun (n, _) -> n <> name) t.pages;
      (if t.current_page = name then
         match t.pages with
         | (first_name, _) :: _ -> t.current_page <- first_name
         | [] -> t.current_page <- "");
      Lwt.return (Ok ())

let list_tabs t =
  List.map
    (fun (name, page) -> (name, page.target_id, name = t.current_page))
    t.pages

(* --- Session pool --- *)

let pool : (string, t) Hashtbl.t = Hashtbl.create 4

let get_or_launch ?configured_path ~session_key () =
  let open Lwt.Syntax in
  match Hashtbl.find_opt pool session_key with
  | Some t when not t.closed ->
      t.last_activity <- Unix.gettimeofday ();
      Lwt.return t
  | _ ->
      let* t = launch ?configured_path () in
      Hashtbl.replace pool session_key t;
      Lwt.return t

let close_session ~session_key () =
  let open Lwt.Syntax in
  match Hashtbl.find_opt pool session_key with
  | Some t ->
      Hashtbl.remove pool session_key;
      let* () = close t in
      Lwt.return_unit
  | None -> Lwt.return_unit

let close_all () =
  let open Lwt.Syntax in
  let sessions = Hashtbl.fold (fun k v acc -> (k, v) :: acc) pool [] in
  Hashtbl.clear pool;
  Lwt_list.iter_s (fun (_, t) -> close t) sessions

let cleanup_stale ?(max_idle_s = 300.0) () =
  let now = Unix.gettimeofday () in
  let stale =
    Hashtbl.fold
      (fun k t acc ->
        if now -. t.last_activity > max_idle_s then (k, t) :: acc else acc)
      pool []
  in
  Lwt_list.iter_s
    (fun (k, t) ->
      Hashtbl.remove pool k;
      close t)
    stale
