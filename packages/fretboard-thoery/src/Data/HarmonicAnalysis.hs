{-# LANGUAGE DataKinds #-}

-- | A small, user-focused API for running harmonic analysis using the
--   configurations described in the "Harmonic Analysis Network" paper.
--
--   Power users who need the full set of legacy functions can import
--   "Data.HarmonicAnalysis.Internal" directly.
module Data.HarmonicAnalysis
  ( AnalysisPreset (..)
  , analyze
  , analyzeAnnotated
  , analyzeWindowed
  , windowedConfig, annotatePath
  , Windowed.WindowedConfig (..)
  , Windowed.defaultWindowedConfig
  , HarmonicPath (..)
  , AnnotatedHarmonicPath (..)
  , HarmonicStep (..)
  , FunctionalHarmonyAnnotation (..)
  , Mode (..)
  , Function (..)
  )
where

import qualified Data.HarmonicAnalysis.Internal as Internal
import qualified Data.HarmonicAnalysis.WindowedPathFinding as Windowed
import Data.HarmonicAnalysis.Types
  ( AnnotatedHarmonicPath (..)
  , FunctionalHarmonyAnnotation (..)
  , Function (..)
  , HarmonicPath (..)
  , HarmonicStep (..)
  , Mode (..)
  , RuntimeConfig
  )
import Data.Mod (Mod)
import qualified Data.Set as Set

-- | Built-in configurations that mirror the presets from the Rubato
--   HarmonicAnalysisModel rubette.
data AnalysisPreset
  = MajorMinorTSD          -- ^ Major/Minor modes with Tonic/Subdominant/Dominant functions.
  | MajorMinorDiatonic     -- ^ Major/Minor modes with full diatonic functions.
  | ModalTSD               -- ^ Seven church modes with Tonic/Subdominant/Dominant functions.
  | ModalDiatonic          -- ^ Seven church modes with full diatonic functions.
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Run harmonic analysis for a progression using one of the presets.
analyze :: AnalysisPreset -> [Set.Set (Mod 12)] -> HarmonicPath
analyze preset = Internal.harmonicAnalysis (configFor preset)

-- | Run harmonic analysis and return annotated steps (mode/function labels).
analyzeAnnotated :: AnalysisPreset -> [Set.Set (Mod 12)] -> AnnotatedHarmonicPath
analyzeAnnotated preset = Internal.harmonicAnalysisAnnotated (configFor preset)

-- | Run the windowed "Best Local Germs" analysis described in the paper.
analyzeWindowed :: AnalysisPreset -> Windowed.WindowedConfig -> [Set.Set (Mod 12)] -> HarmonicPath
analyzeWindowed preset windowCfg progression =
  Internal.windowedHarmonicAnalysis (runtimeFor preset) windowCfg progression

-- | Annotate an existing harmonic path using the labels for a preset.
annotatePath :: AnalysisPreset -> [Set.Set (Mod 12)] -> HarmonicPath -> AnnotatedHarmonicPath
annotatePath preset progression path =
  Internal.annotateHarmonicPathForConfig (presetIndex preset) progression path

-- | Obtain the runtime configuration for a preset. This is exposed for
--   callers that want to tweak the tension table or weight table before
--   running analysis.
windowedConfig :: AnalysisPreset -> RuntimeConfig
windowedConfig = runtimeFor

--------------------------------------------------------------------------------
-- Helpers

configFor :: AnalysisPreset -> Internal.HarmonicAnalysisConfig
configFor preset = case preset of
  MajorMinorTSD      -> Internal.majorMinorTSDConfig
  MajorMinorDiatonic -> Internal.majorMinorDiatonicConfig
  ModalTSD           -> Internal.modalTSDConfig
  ModalDiatonic      -> Internal.modalDiatonicConfig

runtimeFor :: AnalysisPreset -> RuntimeConfig
runtimeFor preset = case preset of
  MajorMinorTSD      -> Internal.makeMajorMinorTSDConfig
  MajorMinorDiatonic -> Internal.makeMajorMinorDiatonicConfig
  ModalTSD           -> Internal.makeModalTSDConfig
  ModalDiatonic      -> Internal.makeModalDiatonicConfig

presetIndex :: AnalysisPreset -> Int
presetIndex preset = case preset of
  MajorMinorTSD      -> 0
  MajorMinorDiatonic -> 1
  ModalTSD           -> 2
  ModalDiatonic      -> 3

