# Fretboard Theory - Neo-Riemannian Harmonic Analysis

A multi-package Haskell library for Neo-Riemannian harmonic analysis and guitar fretboard theory. This project provides tools for analyzing chord progressions using Riemann matrices, finding optimal harmonic paths, and working with guitar fretboards.

## Packages

This repository contains two main packages:

### `fretboard-thoery`
The core library providing:
- **Neo-Riemannian Analysis**: Tools for analyzing harmonic relationships using Riemann matrices
- **Path Finding**: Algorithms for finding optimal harmonic progressions
- **Tension Analysis**: Methods for calculating and analyzing harmonic tension
- **Fretboard Theory**: Guitar fretboard modeling and analysis
- **Chain of Thirds**: Analysis of tertian harmony structures

### `fretboard-diagram`
A visualization library for creating fretboard diagrams:
- **Diagram Generation**: Create SVG diagrams of guitar fretboards
- **Chord Visualization**: Display chord shapes on fretboards
- **Scale Visualization**: Show scale patterns on fretboards
- **Customizable Output**: Flexible styling and layout options

## Key Features

- **Riemann Matrix Operations**: Mathematical operations on Riemann matrices for harmonic analysis
- **Viterbi Algorithm**: Hidden Markov Model implementation for optimal path finding in harmonic spaces
- **Probability Theory**: Comprehensive probability calculations for harmonic analysis
- **Fretboard Modeling**: Complete guitar fretboard representation and manipulation
- **Modulation Analysis**: Tools for analyzing key changes and modulations
- **Tension Calculation**: Real-world tension data and calculation methods

## Installation

This project relies on a reproducible Nix + devenv toolchain. The steps below assume no prior experience with either tool.

### 1. Install Nix

The easiest path is the experimental installer, which supports Linux, macOS, Windows (WSL2), and Docker containers:

```bash
curl -L https://github.com/NixOS/experimental-nix-installer/releases/download/0.27.0/nix-installer.sh | sh -s -- install
```

We recommend this installer because it survives OS upgrades and has first-class Apple silicon support.

If you prefer the long-standing official installer:

```bash
sh <(curl -L https://nixos.org/nix/install)
```

#### Upgrade Bash on macOS

macOS ships an outdated Bash that can trigger Nix evaluation errors. After Nix is installed, upgrade Bash with:

```bash
nix-env --install --attr bashInteractive -f https://github.com/NixOS/nixpkgs/tarball/nixpkgs-unstable
```

### 2. Install devenv

devenv manages the project-specific development shell. The newcomer-friendly approach uses `nix-env`:

```bash
nix-env --install --attr devenv -f https://github.com/NixOS/nixpkgs/tarball/nixpkgs-unstable
```

If you already enabled Nix's experimental features (`nix-command` and `flakes`), you can instead run:

```bash
nix --extra-experimental-features "nix-command flakes" profile install nixpkgs#devenv
```

### 3. Bootstrap the project

1. Clone the repository:
   ```bash
   git clone https://github.com/o1lo01ol1o/fretboard-thoery.git
   cd fretboard-thoery
   ```
2. Enter the development environment:
   ```bash
   nix develop --impure
   ```
   The first run downloads the full toolchain; expect several minutes on a fresh machine. If you prefer, `devenv shell` provides the same environment.
3. Start the full stack (backend API + React frontend):
   ```bash
   full-stack
   ```
   The script builds the Haskell services (`cabal run exe:harmonic-function-server`) and the React UI (`bun run dev`). Leave this shell running; the first build may take a while as dependencies compile.
4. When the frontend prints a URL (typically `http://localhost:5173`), open it in your browser to explore the app. Press `Ctrl+C` in the terminal to stop both servers.

## Usage

### Basic Harmonic Analysis

```haskell
import Data.HarmonicAnalysis
import Data.HarmonicAnalysis.RiemannMatrix

-- Analyze a chord progression
progression = [majorTriad, minorTriad, dominantSeventh]
analysis = analyzeProgression progression
```

### Fretboard Visualization

```haskell
import qualified Fretboard
import qualified MyLib -- from fretboard-diagram

-- Create a fretboard diagram showing a C major chord
chord = CMajor
fretboard = standardTuning
diagram = createChordDiagram fretboard chord
```

### Path Finding

```haskell
import Data.HarmonicAnalysis.PathFinding

-- Find optimal path between two chords
startChord = CMajor
endChord = GDominant7
optimalPath = findHarmonicPath startChord endChord
```

## Development

### Project Structure

```
fretboard-thoery/
├── packages/
│   ├── fretboard-thoery/          # Core library
│   │   ├── src/                   # Library source code
│   │   ├── test/                  # Test suites
│   │   └── fretboard-thoery.cabal
│   └── fretboard-diagram/         # Diagram generation
│       ├── src/                   # Library source code
│       ├── app/                   # Executable
│       ├── test/                  # Test suites
│       └── fretboard-diagram.cabal
├── cabal.project                  # Multi-package configuration
├── devenv.nix                     # Development environment
└── README.md
```

### Available Scripts

The development environment provides several useful scripts:

- `cabal build all` - Build all packages
- `cabal test all` - Run all test suites
- `cabal repl fretboard-thoery` - Start REPL for core library
- `cabal repl fretboard-diagram` - Start REPL for diagram library

### Testing

The project includes comprehensive test suites:

- **Unit Tests**: Basic functionality testing
- **Property Tests**: QuickCheck property-based testing
- **Debug Tests**: Specialized debugging and profiling tests
- **Integration Tests**: Cross-module integration testing

Run specific test suites:
```bash
# Run tests for core library only
cabal test fretboard-thoery:fretboard-thoery-test

# Run tests for diagram library only
cabal test fretboard-diagram:fretboard-diagram-test
```

### Code Organization

#### Core Library (`fretboard-thoery`)
- `Chord` - Chord representation and operations
- `Fretboard` - Guitar fretboard modeling
- `Finger` - Fingering analysis and Template Haskell utilities
- `Modulation` - Key change and modulation analysis
- `Data.HarmonicAnalysis.*` - Harmonic analysis modules
  - `ChainOfThirds` - Tertian harmony analysis
  - `PathFinding` - Harmonic path algorithms
  - `RiemannMatrix` - Riemann matrix operations
  - `Tension` - Harmonic tension calculations
  - `Types` - Core type definitions

#### Diagram Library (`fretboard-diagram`)
- `MyLib` - Main diagram generation functions
- Executable for command-line diagram generation

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass: `cabal test all`
6. Submit a pull request

### Code Style

- Follow standard Haskell conventions
- Use `cabal-fmt` for cabal file formatting (included in pre-commit hooks)
- Add type signatures for all top-level functions
- Include documentation for exported functions

## Dependencies

### Core Dependencies
- `base` - Standard Haskell base library
- `containers` - Data structures
- `vector` - Efficient arrays
- `linear` - Linear algebra
- `mtl` - Monad transformer library
- `lens` - Functional references

### Specialized Dependencies
- `comfort-array` - Multi-dimensional arrays
- `lapack` - Linear algebra operations
- `hmm-lapack` - Hidden Markov Models
- `pqueue` - Priority queues for pathfinding

### Diagram Dependencies
- `diagrams-lib` - Core diagrams library
- `diagrams-svg` - SVG backend
- `diagrams-contrib` - Additional diagram utilities

## License

BSD-3-Clause

## Author

o1lo01ol1o (tim.pierson@gmail.com)

## Acknowledgments

This project builds upon concepts from:
- Neo-Riemannian theory in music analysis
- Graph theory applications in harmonic analysis
- Computer-aided composition techniques
