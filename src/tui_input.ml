(* tui_input.ml — Secure terminal input helpers *)

let read_secret prompt =
  if not (Unix.isatty Unix.stdin) then
    Error "Cannot prompt for secret: stdin is not a terminal"
  else begin
    Printf.printf "%s" prompt;
    flush stdout;
    let attr = Unix.tcgetattr Unix.stdin in
    let raw =
      {
        attr with
        Unix.c_echo = false;
        c_icanon = false;
        c_vmin = 1;
        c_vtime = 0;
      }
    in
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH raw;
    let restore () =
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH attr;
      Printf.printf "\n";
      flush stdout
    in
    let buf = Buffer.create 64 in
    let byte = Bytes.create 1 in
    Fun.protect ~finally:restore (fun () ->
        let interrupted = ref false in
        (try
           while true do
             let _ = Unix.read Unix.stdin byte 0 1 in
             let c = Bytes.get byte 0 in
             if c = '\n' || c = '\r' then raise Exit
             else if (c = '\127' || c = '\b') && Buffer.length buf > 0 then begin
               let len = Buffer.length buf in
               let contents = Buffer.contents buf in
               Buffer.clear buf;
               Buffer.add_string buf (String.sub contents 0 (len - 1));
               Printf.printf "\b \b";
               flush stdout
             end
             else if c = '\003' then begin
               (* Ctrl-C *)
               interrupted := true;
               raise Exit
             end
             else if c >= ' ' then begin
               Buffer.add_char buf c;
               (* U+2022 BULLET *)
               Printf.printf "\xE2\x80\xA2";
               flush stdout
             end
           done
         with Exit -> ());
        if !interrupted then Error "Interrupted."
        else
          let s = String.trim (Buffer.contents buf) in
          if s = "" then Error "No value entered." else Ok s)
  end

let redact s =
  let len = String.length s in
  if len <= 8 then String.make len '*'
  else String.sub s 0 4 ^ "..." ^ String.sub s (len - 4) 4
