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

### Prerequisites

This project uses [devenv](https://devenv.sh/) for development environment management. You'll need:
- [Nix](https://nixos.org/download.html)
- [devenv](https://devenv.sh/getting-started/)

### Setup

1. Clone the repository:
```bash
git clone https://github.com/o1lo01ol1o/fretboard-thoery.git
cd fretboard-thoery
```

2. Enter the development environment:
```bash
devenv shell
```

3. Build the project:
```bash
cabal build all
```

4. Run tests:
```bash
cabal test all
```

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
