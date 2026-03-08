From Coq Require Import String.
From Clawq Require Import AuditChain.

Open Scope string_scope.

Module ConcreteCrypto <: AuditChain.CRYPTO.
  Definition hash (s : string) : string := "sha256:" ++ s.

  Definition hmac (key payload : string) : string :=
    "hmac:" ++ key ++ ":" ++ payload.

  Definition encode_signed_field (value : string) : string := "enc:" ++ value.
End ConcreteCrypto.

Module Concrete := AuditChain.Make(ConcreteCrypto).
Include Concrete.
