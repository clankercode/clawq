let is_loopback_host host =
  let h = String.lowercase_ascii (String.trim host) in
  h = "127.0.0.1" || h = "localhost" || h = "::1"

let starts_with_http_method s =
  let n = String.length s in
  (n >= 4 && String.sub s 0 4 = "GET ")
  || (n >= 5 && String.sub s 0 5 = "POST ")
  || (n >= 4 && String.sub s 0 4 = "PUT ")
  || (n >= 7 && String.sub s 0 7 = "DELETE ")
  || (n >= 5 && String.sub s 0 5 = "HEAD ")
  || (n >= 6 && String.sub s 0 6 = "PATCH ")

let scrub_telegram_tokens s =
  let marker = "/bot" in
  let mlen = String.length marker in
  let slen = String.length s in
  let buf = Buffer.create slen in
  let i = ref 0 in
  while !i < slen do
    if !i + mlen <= slen && String.sub s !i mlen = marker then begin
      let j = ref (!i + mlen) in
      while !j < slen && s.[!j] <> '/' do
        incr j
      done;
      if !j < slen then begin
        Buffer.add_string buf "/bot<REDACTED>";
        i := !j
      end
      else begin
        Buffer.add_char buf s.[!i];
        incr i
      end
    end
    else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let make_reporter () =
  let check_buf = Buffer.create 128 in
  let check_ppf = Format.formatter_of_buffer check_buf in
  let last_log_date = ref None in
  let report src level ~over k msgf =
    msgf (fun ?header ?tags:_ fmt ->
        Format.pp_print_flush check_ppf ();
        Buffer.clear check_buf;
        Format.kfprintf
          (fun ppf ->
            Format.pp_print_flush ppf ();
            let s = Buffer.contents check_buf in
            if
              Logs.Src.name src = "application"
              && level = Logs.Info && starts_with_http_method s
            then (
              over ();
              k ())
            else begin
              let s = scrub_telegram_tokens s in
              let dst = Format.err_formatter in
              let t = Unix.gettimeofday () in
              Daemon_util.maybe_emit_date_banner dst last_log_date t;
              Daemon_util.pp_header_with_ts dst t (level, header);
              let mc = Daemon_util.msg_color level in
              if mc <> "" then begin
                Format.pp_print_string dst mc;
                Format.pp_print_string dst s;
                Format.pp_print_string dst Daemon_util.ansi_reset
              end
              else Format.pp_print_string dst s;
              Format.pp_print_newline dst ();
              over ();
              k ()
            end)
          check_ppf fmt)
  in
  { Logs.report }

let silence_cohttp_info_sources () =
  List.iter
    (fun src ->
      let name = Logs.Src.name src in
      if String.length name >= 6 && String.sub name 0 6 = "cohttp" then
        Logs.Src.set_level src (Some Logs.Warning))
    (Logs.Src.list ())

let install () =
  (Lwt.async_exception_hook :=
     fun exn ->
       let bt = Printexc.get_backtrace () in
       let bt_msg = if bt = "" then " (no backtrace)" else "\n" ^ bt in
       Logs.err (fun m ->
           m "Uncaught async exception: %s%s" (Printexc.to_string exn) bt_msg);
       Format.pp_print_flush Format.err_formatter ());
  Logs.set_reporter (make_reporter ());
  Logs.set_level (Some Logs.Info);
  silence_cohttp_info_sources ()
