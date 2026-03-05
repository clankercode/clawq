(* RFC 6238 TOTP implementation *)

let base32_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

let base32_encode bytes =
  let len = String.length bytes in
  let buf = Buffer.create ((len * 8 / 5) + 1) in
  let bits = ref 0 in
  let value = ref 0 in
  for i = 0 to len - 1 do
    value := (!value lsl 8) lor Char.code bytes.[i];
    bits := !bits + 8;
    while !bits >= 5 do
      bits := !bits - 5;
      let idx = (!value lsr !bits) land 0x1f in
      Buffer.add_char buf base32_alphabet.[idx]
    done
  done;
  if !bits > 0 then begin
    let idx = (!value lsl (5 - !bits)) land 0x1f in
    Buffer.add_char buf base32_alphabet.[idx]
  end;
  Buffer.contents buf

let base32_decode s =
  let s = String.uppercase_ascii s in
  let len = String.length s in
  let buf = Buffer.create (len * 5 / 8) in
  let bits = ref 0 in
  let value = ref 0 in
  for i = 0 to len - 1 do
    let c = s.[i] in
    if c <> '=' then begin
      let v =
        match c with
        | 'A' .. 'Z' -> Char.code c - Char.code 'A'
        | '2' .. '7' -> Char.code c - Char.code '2' + 26
        | _ -> -1
      in
      if v >= 0 then begin
        value := (!value lsl 5) lor v;
        bits := !bits + 5;
        if !bits >= 8 then begin
          bits := !bits - 8;
          Buffer.add_char buf (Char.chr ((!value lsr !bits) land 0xff))
        end
      end
    end
  done;
  Buffer.contents buf

let generate_secret () =
  Mirage_crypto_rng_unix.use_default ();
  let raw = Mirage_crypto_rng.generate 20 in
  base32_encode raw

let hotp ~secret ~counter =
  let key = base32_decode secret in
  let counter_bytes = Bytes.create 8 in
  let c = ref counter in
  for i = 7 downto 0 do
    Bytes.set counter_bytes i (Char.chr (Int64.to_int !c land 0xff));
    c := Int64.shift_right_logical !c 8
  done;
  let hmac =
    Digestif.SHA1.(
      hmac_string ~key (Bytes.to_string counter_bytes) |> to_raw_string)
  in
  let offset = Char.code hmac.[String.length hmac - 1] land 0x0f in
  let code =
    ((Char.code hmac.[offset] land 0x7f) lsl 24)
    lor ((Char.code hmac.[offset + 1] land 0xff) lsl 16)
    lor ((Char.code hmac.[offset + 2] land 0xff) lsl 8)
    lor (Char.code hmac.[offset + 3] land 0xff)
  in
  let otp = code mod 1_000_000 in
  Printf.sprintf "%06d" otp

let generate_totp ~secret ~time =
  let counter = Int64.div (Int64.of_float time) 30L in
  hotp ~secret ~counter

let verify_totp ~secret ~code ~time =
  let counter = Int64.div (Int64.of_float time) 30L in
  let check c = Eqaf.equal (hotp ~secret ~counter:c) code in
  check (Int64.sub counter 1L) || check counter || check (Int64.add counter 1L)

let time_remaining ~time =
  let elapsed = int_of_float (mod_float time 30.0) in
  30 - elapsed
