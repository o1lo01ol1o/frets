# Repository Guidelines

## Domain Philosophy
Treat the Haskell codebase as the single source of truth for musical algebra: model pitch classes, transformations, and fretboard behaviors as composable types, then expose them through backend endpoints. The JavaScript frontend should compose REST calls to those services instead of re-implementing musical logic in TypeScript.  IT IS IMPERATIVE THAT ALL IMPLEMENTATIONS BE ALGEBRAIC AND RESPECT THE SSOT PRINCIPLE.

## Project Structure & Module Organization
Core Haskell logic lives in `packages/fretboard-thoery/src`, with harmonic analysis under `Data/HarmonicAnalysis/*`, fretboard models in `Fretboard/*`, and server entry points in `app/`. Tests and benchmarks sit in `packages/fretboard-thoery/test` and `packages/fretboard-thoery/bench`. The diagram package mirrors that layout in `packages/fretboard-diagram/{src,app,test}`. The React UI is under `web/harmonic-analyzer/src`, serving assets from `public/`.

## Coding Style & Naming Conventions
Match the existing two-space indentation and align multi-line type signatures and records as seen in `packages/fretboard-thoery/src`. Keep module names in PascalCase, functions and values in `camelCase`, and export lists explicit. Include type signatures for every exported binding. Run `cabal-fmt` on `.cabal` files and `nixfmt` on Nix sources before committing. React components belong in `PascalCase.tsx`, shared helpers in `camelCase.ts`, and JSX should stay within 100 columns to mirror current formatting.

## Testing Guidelines
The test harness uses Tasty with HUnit plus Hedgehog/QuickCheck helpers (`packages/fretboard-thoery/test/Spec`). Add property tests alongside unit assertions when touching harmonic search or matrix math, and rely on `WindowedDebug.hs` for deterministically reproducing tricky paths. Always execute `cabal test ... --test-show-details=always` prior to review. Frontend updates must pass `bun run typecheck` and compile cleanly with `bun run build`; add vitest stories if you introduce complex UI logic.

## Commit & Pull Request Guidelines
Write imperative, descriptive commits (e.g. `Refine Neo-Riemannian tension weighting`) and wrap bodies at 72 columns when extra context is needed. Squash noisy WIP commits before opening a PR. Pull requests should summarize the musical or UX impact, link related issues, and attach screenshots or benchmark output for visual or performance changes. Confirm all relevant build, test, and bench commands locally, and note follow-up work explicitly if scope remains limited.
