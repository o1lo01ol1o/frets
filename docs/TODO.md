# TODO / Future Work

- [ ] Recast the windowed analysis (Best Local Germs) as a Viterbi-style dynamic program so we avoid enumerating the full cartesian product of window states. Treat each `(mode,function,tonality)` cell as a DP state, use Riemann matrix entries as emissions, and tension tables as transition costs. Keep the window depth by limiting the active states per step. This should bring windowed timings down from seconds to milliseconds once implemented.
