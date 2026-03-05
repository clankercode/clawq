# GET Pull Request Changed Files

## Endpoint

```
GET /repos/{owner}/{repo}/pulls/{pull_number}/files
```

Full URL: `https://api.github.com/repos/{owner}/{repo}/pulls/{pull_number}/files`

---

## Required Headers

| Header | Value |
|--------|-------|
| `Authorization` | `Bearer <YOUR-TOKEN>` |
| `Accept` | `application/vnd.github+json` |
| `X-GitHub-Api-Version` | `2022-11-28` |

---

## Path Parameters

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `owner` | string | yes | Repository owner login (case-insensitive) |
| `repo` | string | yes | Repository name without `.git` extension |
| `pull_number` | integer | yes | The pull request number |

---

## Query Parameters (Pagination)

| Name | Type | Default | Max | Description |
|------|------|---------|-----|-------------|
| `per_page` | integer | 30 | 100 | Number of file objects per page |
| `page` | integer | 1 | — | Page number (1-based) |

### Pagination Notes

- Maximum of 100 results per page.
- GitHub's file diff API has an internal cap: PRs with more than **3000 changed files** will return a truncated list. The response does not signal truncation explicitly — you must check if `per_page` results are returned and paginate until fewer than `per_page` items come back.
- Use the `Link` response header to navigate pages:
  ```
  Link: <https://api.github.com/...?page=2>; rel="next",
        <https://api.github.com/...?page=4>; rel="last"
  ```

---

## Response

### Status Codes

| Code | Meaning |
|------|---------|
| 200 OK | Success, returns array of file diff objects |
| 422 Unprocessable Entity | PR has too many changed files to list |
| 500 Internal Server Error | Diff calculation failed (e.g., very large repos) |

### Response Body

Returns a JSON array. Each element is a file diff object. See `api-get-pr-files.json` for a full annotated example.

### File Object Field Reference

| Field | Type | Always Present | Description |
|-------|------|----------------|-------------|
| `sha` | string | yes | Blob SHA of the file at the head commit |
| `filename` | string | yes | File path relative to repo root |
| `status` | string (enum) | yes | Type of change (see enum values below) |
| `additions` | integer | yes | Lines added in this file |
| `deletions` | integer | yes | Lines removed in this file |
| `changes` | integer | yes | Total line changes (`additions + deletions`) |
| `blob_url` | string (URI) | yes | GitHub web URL to the file blob at head commit |
| `raw_url` | string (URI) | yes | Direct download URL for raw file content |
| `contents_url` | string (URI) | yes | REST API URL for file contents at this ref |
| `patch` | string | no | Unified diff patch; omitted for binary files or very large diffs |
| `previous_filename` | string | no | Only present when `status` is `"renamed"` or `"copied"` |

### status Enum Values

| Value | Meaning |
|-------|---------|
| `added` | File was created in this PR |
| `removed` | File was deleted in this PR |
| `modified` | File content was changed |
| `renamed` | File was moved/renamed (check `previous_filename`) |
| `copied` | File was copied from another path (check `previous_filename`) |
| `changed` | File metadata changed (e.g., permissions) but content may not have |
| `unchanged` | File appears in the diff context but has no changes |

### The `patch` Field

The `patch` field contains a unified diff string. Format:
```
@@ -<from_line>,<from_count> +<to_line>,<to_count> @@
-removed line
+added line
 context line
```

The `patch` field is **absent** when:
- The file is a binary file (images, compiled artifacts, etc.)
- The diff is too large (GitHub truncates very large diffs per-file)
- The `status` is `renamed` or `copied` with no content change

For binary files, `additions` and `deletions` will both be `0`, and `changes` will be `0`.

---

## Authentication

Required permission: **Pull requests read** (or **Contents read**).

---

## curl Example

```bash
curl -L \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer ghp_YOUR_TOKEN" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  "https://api.github.com/repos/OWNER/REPO/pulls/42/files?per_page=100&page=1"
```

---

## Notes for OCaml Parser

The response is a JSON array (`[...]`), not a JSON object. Parse with `Yojson.Safe.from_string` then match on `Yojson.Safe.t list`.

Key fields for a code review use case:
- `filename`: string — always present, use for display and routing
- `status`: string — determines what kind of change occurred
- `patch`: string option — may be absent; handle with `Option.value ~default:"(binary or large diff)"`
- `additions` / `deletions`: int — useful for size/scope indicators
- `previous_filename`: string option — only when `status` is `"renamed"` or `"copied"`

The `sha` field is the blob SHA, not the commit SHA. It can be used with the Contents API (`GET /repos/{owner}/{repo}/git/blobs/{file_sha}`) to fetch the full file content.
