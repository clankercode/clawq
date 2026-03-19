(lang dune 3.21)
(name clawq)
(using coq 0.8)
(warnings (deprecated_coq_lang disabled))

(package
 (name clawq)
 (synopsis "Formally verified personal AI assistant runtime")
 (description "Coq-first assistant runtime with OCaml extraction pipeline")
  (depends
   dune
   coq
   ocaml
   cmdliner
   yojson
   sqlite3
   lwt
   cohttp-lwt-unix
   conduit-lwt-unix
   tls-lwt
   logs
   fmt
   mirage-crypto
   mirage-crypto-rng
   kdf
   digestif
   base64
   yaml
   (alcotest :with-test)))
