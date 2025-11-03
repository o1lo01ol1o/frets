# Harmonic Analyzer Web Client

A lightweight React + TypeScript front end managed with [Bun](https://bun.sh) for the
`harmonic-function-server`. It posts directly to the server’s `/analyze` endpoint and renders the
annotated harmonic path.

## Prerequisites

- Bun v1.0 or newer (`curl https://bun.sh/install | bash`)
- The Haskell server running locally (`cabal run harmonic-function-server`) or reachable over HTTP.

The server now ships with permissive CORS headers, so the browser can call it from a different
origin during development.

## Getting started

```bash
cd web/harmonic-analyzer
bun install          # installs dependencies declared in package.json
bun run dev          # starts Vite on http://localhost:5173
```

Update the “Server URL” field in the UI if your backend runs on a different host or port.

To build a production bundle:

```bash
bun run build
```

The build artefacts land in `dist/`; serve them with any static file host.

## Request format

The form expects a JSON array of pitch-class sets. Each inner array can contain chromatic note names
(`"C#"`, `"Bb"`, …) or integers interpreted mod 12. Example:

```json
[
  ["C", "E", "G"],
  ["F", "A", "C"],
  ["G", "B", "D"],
  ["C", "E", "G"]
]
```

Long analyses can take several minutes; the request now stays open until the server responds. Use
the **Cancel request** button if you need to abort and try another progression.
