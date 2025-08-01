# HMatrix-Based Harmonic Analysis Implementation

This document describes the HMatrix-based implementation of the Neo-Riemannian harmonic analysis system, providing an optimized alternative to the original vector-based implementation while maintaining full API compatibility.

## Overview

The `Data.HarmonicAnalysis.HMatrix` module provides a drop-in replacement for the original harmonic analysis functionality, leveraging the HMatrix library for efficient linear algebra operations. This implementation offers the same interface and results as the original while providing additional optimization opportunities through vectorized computations.

## Key Features

### 1. Full API Compatibility

The HMatrix implementation provides identical function signatures and behavior to the original implementation:

```haskell
-- All these functions work exactly as before
analyzeMajorMinorTSD :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeMajorMinorDiatonic :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeModalTSD :: [Set.Set (Mod 12)] -> HarmonicPath
analyzeModalDiatonic :: [Set.Set (Mod 12)] -> HarmonicPath

-- Multi-candidate analysis
analyzeMajorMinorTSDMultiCandidate :: [Set.Set (Mod 12)] -> HarmonicAnalysisResult
-- ... and so on
```

### 2. Configuration System

The same configuration presets are available:

```haskell
-- Configuration presets
majorMinorTSDConfig :: HarmonicAnalysisConfig      -- 2×3 system (Major/Minor × T-S-D)
majorMinorDiatonicConfig :: HarmonicAnalysisConfig -- 2×7 system (Major/Minor × all functions)
modalTSDConfig :: HarmonicAnalysisConfig           -- 7×3 system (Greek modes × T-S-D)
modalDiatonicConfig :: HarmonicAnalysisConfig      -- 7×7 system (Greek modes × all functions)

-- Configuration factory with proper sum type
makeHarmonicConfig :: HarmonicConfigType -> HarmonicAnalysisConfig

-- Configuration sum type
data HarmonicConfigType
  = MajorMinorTSD        -- 2×3 system
  | MajorMinorDiatonic   -- 2×7 system
  | ModalTSD             -- 7×3 system
  | ModalDiatonic        -- 7×7 system
```

### 3. Weight Tables as HMatrix Vectors

Weight tables are represented as efficient HMatrix vectors:

```haskell
majorMinorTSDWeights :: Vector Double      -- 72 elements (2×3×12)
majorMinorDiatonicWeights :: Vector Double -- 168 elements (2×7×12)
modalTSDWeights :: Vector Double           -- 252 elements (7×3×12)
modalDiatonicWeights :: Vector Double      -- 588 elements (7×7×12)
```

## HMatrix-Specific Optimizations

### 1. Optimized Tension Computation

```haskell
optimizedTensionComputation :: TensionTable -> HarmonicPath -> Double
```

Uses HMatrix vector operations to compute harmonic tension more efficiently:
- Converts harmonic paths to position and tonality vectors
- Computes differences using vectorized operations
- Calculates total tension through vector summation

### 2. Vectorized Weight Computation

```haskell
vectorizedWeightComputation :: Vector Double -> [Set.Set (Mod 12)] -> Vector Double
```

Efficiently computes weighted sums for multiple pitch sets:
- Converts pitch sets to binary vectors
- Uses matrix multiplication for batch processing
- Returns vectorized results

## Usage Examples

### Basic Analysis

```haskell
import qualified Data.HarmonicAnalysis.HMatrix as HMatrix
import qualified Data.Set as Set

-- Analyze a simple chord progression: C - Am - F - G
let cMajor = Set.fromList [0, 4, 7]   -- C-E-G
    aMinor = Set.fromList [9, 0, 4]   -- A-C-E
    fMajor = Set.fromList [5, 9, 0]   -- F-A-C
    gMajor = Set.fromList [7, 11, 2]  -- G-B-D
    progression = [cMajor, aMinor, fMajor, gMajor]

-- Perform harmonic analysis
result = HMatrix.analyzeMajorMinorTSD progression
```

### Configuration-Specific Analysis

```haskell
-- Use specific configurations
let config = HMatrix.makeHarmonicConfig HMatrix.MajorMinorDiatonic
    result = HMatrix.harmonicAnalysisWithConfig 1 config progression

-- Or use preset configurations
modalResult = HMatrix.analyzeModalDiatonic progression
```

### Multi-Candidate Analysis

```haskell
-- Get all equally weighted analysis candidates
let candidates = HMatrix.analyzeMajorMinorTSDMultiCandidate progression
    paths = candidatePaths candidates
    maxWeight = maxWeight candidates
    hasTies = hasTies candidates
```

### Using HMatrix Optimizations

```haskell
-- Use optimized functions for performance-critical applications
let tension = HMatrix.optimizedTensionComputation tensionTable path
    weights = HMatrix.vectorizedWeightComputation weightTable pitchSets
```

## Implementation Architecture

### Core Design Principles

1. **Delegation Pattern**: The HMatrix implementation delegates most functionality to the original implementation, ensuring identical behavior while adding HMatrix optimizations where beneficial.

2. **Type Compatibility**: Uses the same data types (`HarmonicPath`, `RiemannMatrix`, `TensionTable`, etc.) as the original implementation.

3. **Lazy Optimization**: HMatrix optimizations are provided as separate functions, allowing users to choose when to use them without changing existing code.

### Internal Structure

```
Data.HarmonicAnalysis.HMatrix
├── API-compatible functions (delegate to original)
├── Configuration presets (with proper sum types)
├── Weight tables (as HMatrix vectors)
├── HMatrix-specific optimizations (tension, vectorized weights)
└── Simplified windowed analysis (placeholder)
```

## Testing

### Comprehensive Test Suite

The implementation includes extensive tests to ensure compatibility:

```haskell
-- Located in test/Spec/HarmonicAnalysisHMatrix.hs
tests = testGroup "HarmonicAnalysis HMatrix tests"
  [ unitTests           -- Basic functionality tests
  , configurationTests  -- Configuration-specific tests
  , comparisonTests     -- Direct comparison with original
  , propertyTests       -- Property-based testing
  ]
```

### Test Categories

1. **Unit Tests**: Verify basic functionality works correctly
2. **Configuration Tests**: Ensure all configuration presets work
3. **Comparison Tests**: Direct comparison with original implementation
4. **Property Tests**: QuickCheck-based property testing

### Key Test Assertions

- Path lengths are identical between implementations
- Weight tables match exactly
- All configuration options produce valid results
- Multi-candidate analysis returns same number of candidates
- HMatrix optimizations don't crash with various inputs
- Configuration sum types work correctly

## Performance Considerations

### When to Use HMatrix Implementation

- **Large batch processing**: When analyzing many chord progressions
- **Real-time applications**: Where vectorized operations provide speedup
- **Memory efficiency**: HMatrix vectors can be more memory-efficient
- **Linear algebra operations**: When you need to perform matrix operations on results

### Potential Performance Benefits

1. **Vectorized Weight Computation**: Batch processing using matrix multiplication
2. **Optimized Tension Calculation**: Vector operations instead of list traversals
3. **Memory Layout**: HMatrix vectors provide better memory locality
4. **Type Safety**: Proper sum types instead of magic integers for configuration

## Future Enhancements

### Current Limitations & Planned Optimizations

**Current Limitations:**
1. **Windowed Analysis**: Currently simplified - returns single analysis instead of true windowed approach
2. **Viterbi Algorithm**: Still uses original implementation (HMatrix version removed as placeholder)
3. **Matrix Operations**: Limited HMatrix usage in core path finding algorithms

**Planned Optimizations:**
1. **True Windowed Analysis**: Implement proper windowed path finding with HMatrix optimizations
2. **HMatrix-based Viterbi**: Complete matrix-based dynamic programming implementation
3. **Batch Analysis**: Efficient processing of multiple progressions simultaneously
4. **GPU Support**: Potential acceleration through HMatrix's BLAS backend
5. **Sparse Matrix Support**: For large configuration spaces with sparse weight tables

### Integration Opportunities

1. **Streaming Analysis**: Integration with streaming libraries
2. **Parallel Processing**: Multi-threaded analysis of large datasets
3. **C++ Backend**: Integration with high-performance C++ implementations

## Migration Guide

### From Original Implementation

The migration is straightforward due to API compatibility:

```haskell
-- Before
import qualified Data.HarmonicAnalysis as HA
result = HA.analyzeMajorMinorTSD progression

-- After
import qualified Data.HarmonicAnalysis.HMatrix as HMatrix
result = HMatrix.analyzeMajorMinorTSD progression  -- Identical result

-- Configuration creation (improved type safety)
-- Before: makeHarmonicConfig 1  -- magic number
-- After:  makeHarmonicConfig MajorMinorDiatonic  -- explicit type
```

### Gradual Adoption

You can adopt HMatrix optimizations incrementally:

```haskell
-- Use original for most functionality
import qualified Data.HarmonicAnalysis as HA

-- Use HMatrix for specific optimizations
import qualified Data.HarmonicAnalysis.HMatrix as HMatrix

-- Mix and match as needed
let basicResult = HA.analyzeMajorMinorTSD progression
    optimizedTension = HMatrix.optimizedTensionComputation table basicResult
    vectorizedWeights = HMatrix.vectorizedWeightComputation weights progressions
```

## Dependencies

The HMatrix implementation adds the following dependency:

```haskell
build-depends:
  ...,
  hmatrix
```

This provides efficient linear algebra operations through BLAS/LAPACK.

## Conclusion

The HMatrix implementation provides a seamless upgrade path for existing harmonic analysis code while offering targeted optimizations. It maintains full compatibility with the original API while providing:

1. **Type Safety**: Proper sum types instead of magic integers for configuration selection
2. **Targeted Optimizations**: HMatrix-based tension computation and vectorized weight operations
3. **API Compatibility**: Drop-in replacement for existing functions
4. **Future-Ready**: Foundation for more extensive HMatrix-based optimizations

**Current Status**: This is a solid foundation with real optimizations in tension computation and weight vectorization, plus improved type safety. The windowed analysis and core Viterbi algorithm remain areas for future HMatrix optimization.

The implementation demonstrates how modern Haskell libraries like HMatrix can be integrated incrementally into existing codebases, providing immediate benefits while establishing groundwork for more extensive optimizations.
