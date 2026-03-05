# GitHub Webhook Signature Verification

Source: https://docs.github.com/en/webhooks/using-webhooks/validating-webhook-deliveries

## Header

GitHub sends a signature in every webhook delivery as the HTTP header:

```
X-Hub-Signature-256: sha256=HEXDIGEST
```

The value always starts with the literal prefix `sha256=` followed by the lowercase hex-encoded HMAC-SHA256 digest.

There is also a legacy `X-Hub-Signature` header using SHA-1 (deprecated). Always use `X-Hub-Signature-256`.

## Algorithm

1. Configure a secret token when creating the webhook (a random high-entropy string).
2. For each incoming request, compute:
   ```
   HMAC-SHA256(key=secret_token, message=raw_request_body)
   ```
   The message is the **raw bytes** of the request body — do not parse or re-encode first.
3. Hex-encode the digest and prepend `sha256=`.
4. Compare the result with the `X-Hub-Signature-256` header value using a **constant-time** comparison function to prevent timing attacks.
5. Reject the request if they do not match.

## Critical implementation notes

- Use constant-time comparison (e.g., `hmac.compare_digest` in Python, `crypto.timingSafeEqual` in Node.js, `eqaf` in OCaml). Never use `==` or `String.equal` for this comparison.
- The raw request body must be used — not a re-serialized version.
- Treat the body as UTF-8 bytes when computing the digest.
- The secret token must be kept confidential (environment variable, not in source code).

## Test vector

To verify your implementation:

- Secret: `It's a Secret to Everybody`
- Payload: `Hello, World!`
- Expected HMAC-SHA256 hex digest: `757107ea0eb2509fc211221cce984b8a37570b6d7586c22c46f4379c8b043e17`
- Expected header value: `sha256=757107ea0eb2509fc211221cce984b8a37570b6d7586c22c46f4379c8b043e17`

## Code examples

### Python

```python
import hashlib
import hmac

def verify_signature(payload_body: bytes, secret_token: str, signature_header: str) -> bool:
    """Returns True if signature is valid, raises on mismatch."""
    if not signature_header:
        return False
    hash_object = hmac.new(secret_token.encode('utf-8'), msg=payload_body, digestmod=hashlib.sha256)
    expected_signature = "sha256=" + hash_object.hexdigest()
    return hmac.compare_digest(expected_signature, signature_header)
```

### Ruby

```ruby
def verify_signature(payload_body)
  signature = 'sha256=' + OpenSSL::HMAC.hexdigest(OpenSSL::Digest.new('sha256'), ENV['SECRET_TOKEN'], payload_body)
  return halt 500, "Signatures didn't match!" unless Rack::Utils.secure_compare(signature, request.env['HTTP_X_HUB_SIGNATURE_256'])
end
```

### OCaml (using digestif + eqaf)

```ocaml
let verify_signature ~secret ~body ~signature_header =
  (* signature_header is the value of X-Hub-Signature-256 *)
  match String.split_on_char '=' signature_header with
  | ["sha256"; hex] ->
    let key = Digestif.SHA256.hmac_string ~key:secret body in
    let expected = "sha256=" ^ Digestif.SHA256.to_hex key in
    Eqaf.equal expected signature_header
  | _ -> false
```

## HTTP headers sent with every webhook delivery

```
X-GitHub-Event: <event-name>
X-GitHub-Delivery: <guid>
X-Hub-Signature-256: sha256=<hexdigest>
X-Hub-Signature: sha1=<hexdigest>   (legacy, deprecated)
X-GitHub-Hook-ID: <integer>
X-GitHub-Hook-Installation-Target-ID: <integer>
X-GitHub-Hook-Installation-Target-Type: repository | organization | app
User-Agent: GitHub-Hookshot/<hash>
Content-Type: application/json
```

The `X-GitHub-Event` header value matches the event name exactly (e.g., `pull_request`, `issue_comment`, `pull_request_review_comment`, `pull_request_review`).
