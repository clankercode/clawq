# src/CLAUDE.md

## Tool Error Messages

Error messages returned from tools must be descriptive and actionable. Each error should:

1. Clearly state what went wrong.
2. Include instructions on how to repair the function call (e.g., which parameter was invalid, what format is expected, what values are acceptable).
3. Suggest alternative recovery paths when a simple fix isn't possible (e.g., "file not found — check the path or use `list_dir` to discover available files").

Bad example:
```
Error: invalid parameter
```

Good example:
```
Error: parameter "path" must be an absolute path starting with "/". Received: "foo/bar.txt". Use an absolute path like "/home/user/foo/bar.txt", or call the "list_dir" tool first to discover files relative to the workspace root.
```

This applies to all tool implementations in `tools_builtin.ml`, `skills.ml`, and any tools exposed via `mcp_server.ml`.
