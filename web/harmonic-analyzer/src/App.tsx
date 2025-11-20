import { Fragment, useCallback, useEffect, useMemo, useRef, useState } from "react";
import TonnetzWidget, { type RecordedChord } from "./TonnetzWidget";
import FretboardOverlayDiagram, {
  DEFAULT_CHORD_COLOR,
  DEFAULT_SCALE_DIFF_OUTLINE_COLOR,
  DEFAULT_SCALE_DIFF_STROKE_WIDTH,
  type FretboardOverlaySet
} from "./FretboardOverlayDiagram";

type AnalysisPreset =
  | "MajorMinorTSD"
  | "MajorMinorDiatonic"
  | "ModalTSD"
  | "ModalDiatonic";

type HarmonicStepResponse = {
  index: number;
  tonality: number;
  key: string;
  weight: number;
  pitchClasses: number[];
  pitchClassNames: string[];
  scalePitchClasses?: number[];
  scalePitchClassNames?: string[];
  scaleDeviationPitchClasses?: number[];
  scaleDeviationPitchClassNames?: string[];
  mode: string;
  function: string;
  degree: string;
  romanNumeral: string | null;
};

type AnalyzeResponse = {
  preset: string;
  steps: HarmonicStepResponse[];
};

type VoiceLeadingStringPosition = {
  string: number;
  fret: number;
  finger: string | null;
  pitchClass: string;
  pitchClassNumber: number;
  octave?: number | null;
  noteName?: string;
  label?: string | null;
};

type VoiceLeadingStep = {
  index: number;
  difficulty: number;
  transitionCost: number;
  pitchClasses: number[];
  pitchClassNames: string[];
  positions: VoiceLeadingStringPosition[];
};

type VoiceLeadingResponse = {
  totalCost: number;
  steps: VoiceLeadingStep[];
};

type ScaleNotesResponsePayload = {
  pitchClasses: number[];
  pitchClassNames: string[];
  diffPitchClasses?: number[];
  diffPitchClassNames?: string[];
};

type TuningOption = {
  id: string;
  label: string;
  strings: string[];
};

const PRESET_OPTIONS: Array<{ label: string; value: AnalysisPreset }> = [
  { label: "Major / Minor (Tonic-Subdominant-Dominant)", value: "MajorMinorTSD" },
  { label: "Major / Minor (Full diatonic)", value: "MajorMinorDiatonic" },
  { label: "Modal (T-S-D)", value: "ModalTSD" },
  { label: "Modal (Full diatonic)", value: "ModalDiatonic" }
];

const DEFAULT_GUITAR_TUNING = ["E2", "A2", "D3", "G3", "B3", "E4"] as const;

const GUITAR_STRING_OCTAVES = [2, 2, 3, 3, 3, 4] as const;
const BASS_STRING_OCTAVES = [1, 1, 2, 2] as const;

const makeGuitarTuning = (notes: string[]): string[] =>
  notes.map((note, index) => `${note}${GUITAR_STRING_OCTAVES[index] ?? GUITAR_STRING_OCTAVES[GUITAR_STRING_OCTAVES.length - 1]}`);

const makeBassTuning = (notes: string[]): string[] =>
  notes.map((note, index) => `${note}${BASS_STRING_OCTAVES[index] ?? BASS_STRING_OCTAVES[BASS_STRING_OCTAVES.length - 1]}`);

const TUNING_OPTIONS: TuningOption[] = [
  {
    id: "guitar-standard",
    label: "Guitar – Standard (E A D G B E)",
    strings: [...DEFAULT_GUITAR_TUNING]
  },
  {
    id: "guitar-drop-d",
    label: "Guitar – Drop D (D A D G B E)",
    strings: makeGuitarTuning(["D", "A", "D", "G", "B", "E"])
  },
  {
    id: "guitar-double-harmonic-major",
    label: "Guitar – Double Harmonic Major (Bb A D G Bb D)",
    strings: makeGuitarTuning(["Bb", "A", "D", "G", "Bb", "D"])
  },
  {
    id: "guitar-drop-csharp",
    label: "Guitar – Drop C# (C# A D G B E)",
    strings: makeGuitarTuning(["Cs", "A", "D", "G", "B", "E"])
  },
  {
    id: "guitar-drop-c",
    label: "Guitar – Drop C (C G C F A D)",
    strings: makeGuitarTuning(["C", "G", "C", "F", "A", "D"])
  },
  {
    id: "guitar-drop-b",
    label: "Guitar – Drop B (B F# B E G# C#)",
    strings: makeGuitarTuning(["B", "Fs", "B", "E", "Gs", "Cs"])
  },
  {
    id: "guitar-drop-a",
    label: "Guitar – Drop A (A E A D F# B)",
    strings: makeGuitarTuning(["A", "E", "A", "D", "Fs", "B"])
  },
  {
    id: "guitar-dadgad",
    label: "Guitar – DADGAD (D A D G A D)",
    strings: makeGuitarTuning(["D", "A", "D", "G", "A", "D"])
  },
  {
    id: "guitar-half-step-down",
    label: "Guitar – Half Step Down (Eb Ab Db Gb Bb Eb)",
    strings: makeGuitarTuning(["Eb", "Gs", "Cs", "Fs", "Bb", "Eb"])
  },
  {
    id: "guitar-full-step-down",
    label: "Guitar – Full Step Down (D G C F A D)",
    strings: makeGuitarTuning(["D", "G", "C", "F", "A", "D"])
  },
  {
    id: "guitar-half-step-up",
    label: "Guitar – Half Step Up (F Bb Eb G# C F)",
    strings: makeGuitarTuning(["F", "Bb", "Eb", "Gs", "C", "F"])
  },
  {
    id: "guitar-open-c",
    label: "Guitar – Open C (C G C G C E)",
    strings: makeGuitarTuning(["C", "G", "C", "G", "C", "E"])
  },
  {
    id: "guitar-open-d",
    label: "Guitar – Open D (D A D F# A D)",
    strings: makeGuitarTuning(["D", "A", "D", "Fs", "A", "D"])
  },
  {
    id: "guitar-open-e",
    label: "Guitar – Open E (E B E G# B E)",
    strings: makeGuitarTuning(["E", "B", "E", "Gs", "B", "E"])
  },
  {
    id: "guitar-open-f",
    label: "Guitar – Open F (F A C F C F)",
    strings: makeGuitarTuning(["F", "A", "C", "F", "C", "F"])
  },
  {
    id: "guitar-open-g",
    label: "Guitar – Open G (D G D G B D)",
    strings: makeGuitarTuning(["D", "G", "D", "G", "B", "D"])
  },
  {
    id: "guitar-open-a",
    label: "Guitar – Open A (E A E A C# E)",
    strings: makeGuitarTuning(["E", "A", "E", "A", "Cs", "E"])
  },
  {
    id: "bass-standard-4",
    label: "Bass – Standard 4-String (E A D G)",
    strings: makeBassTuning(["E", "A", "D", "G"])
  },
  {
    id: "bass-drop-d",
    label: "Bass – Drop D (D A D G)",
    strings: makeBassTuning(["D", "A", "D", "G"])
  }
];

const DEFAULT_TUNING_ID = "guitar-standard";

const defaultServer = (() => {
  if (typeof window === "undefined") {
    return "http://localhost:8080";
  }
  const { protocol, hostname } = window.location;
  if (protocol === "http:" || protocol === "https:") {
    const resolvedHost = hostname && hostname.trim().length > 0 ? hostname : "localhost";
    const scheme = protocol === "https:" ? "https" : "http";
    return `${scheme}://${resolvedHost}:8080`;
  }
  return "http://localhost:8080";
})();

const normalisePitchClass = (value: number): number => ((value % 12) + 12) % 12;

const normaliseUrl = (raw: string): string => raw.replace(/\/+$/, "");

const CHROMATIC_NAMES: readonly string[] = [
  "C",
  "Cs",
  "D",
  "Eb",
  "E",
  "F",
  "Fs",
  "G",
  "Gs",
  "A",
  "Bb",
  "B"
] as const;

const MODE_INTERVALS: Record<string, number[]> = {
  Ionian: [0, 2, 4, 5, 7, 9, 11],
  Major: [0, 2, 4, 5, 7, 9, 11],
  Dorian: [0, 2, 3, 5, 7, 9, 10],
  Phrygian: [0, 1, 3, 5, 7, 8, 10],
  Lydian: [0, 2, 4, 6, 7, 9, 11],
  Mixolydian: [0, 2, 4, 5, 7, 9, 10],
  Aeolian: [0, 2, 3, 5, 7, 8, 10],
  Minor: [0, 2, 3, 5, 7, 8, 10],
  Locrian: [0, 1, 3, 5, 6, 8, 10]
};

const resolveModeKey = (modeName?: string | null): string | null => {
  if (!modeName) {
    return null;
  }
  const trimmed = modeName.trim();
  if (trimmed.length === 0) {
    return null;
  }
  const normalized = trimmed.replace(/\s+/g, "");
  if (MODE_INTERVALS[trimmed]) {
    return trimmed;
  }
  if (MODE_INTERVALS[normalized]) {
    return normalized;
  }
  const canonical =
    normalized.charAt(0).toUpperCase() + normalized.slice(1).toLowerCase();
  return MODE_INTERVALS[canonical] ? canonical : null;
};

const splitPitchAndOctave = (token: string): { pitch: string; octave?: number } => {
  const trimmed = token.trim();
  if (!trimmed) {
    return { pitch: trimmed };
  }
  const match = trimmed.match(/^([A-Ga-g])([#sSbB]?)(-?\d+)?$/);
  if (match) {
    const letter = match[1]!.toUpperCase();
    const accidentalRaw = match[2] ?? "";
    const accidental = accidentalRaw
      .replace(/s/gi, "#")
      .replace(/b/gi, "b")
      .toUpperCase();
    const pitch = `${letter}${accidental}`.toUpperCase();
    const octave = match[3] ? Number.parseInt(match[3]!, 10) : undefined;
    return { pitch, octave: Number.isNaN(octave) ? undefined : octave };
  }
  return { pitch: trimmed.toUpperCase() };
};

const serializeTuningValue = (entry: string): string | number | { pitch: string; octave?: number } => {
  const { pitch, octave } = splitPitchAndOctave(entry);
  if (octave !== undefined) {
    return { pitch, octave };
  }
  return { pitch };
};

type AsyncState = "idle" | "loading" | "success" | "error";

function App() {
  const [serverUrl, setServerUrl] = useState<string>(defaultServer);
  const [preset, setPreset] = useState<AnalysisPreset>("ModalDiatonic");
  const [recordedChords, setRecordedChords] = useState<RecordedChord[]>([]);
  const [progression, setProgression] = useState<number[][]>([]);
  const [isRecording, setIsRecording] = useState(false);

  const [analysis, setAnalysis] = useState<AnalyzeResponse | null>(null);
  const [analysisState, setAnalysisState] = useState<AsyncState>("idle");
  const [analysisError, setAnalysisError] = useState<string | null>(null);

  const [selectedTuningId, setSelectedTuningId] = useState<string>(DEFAULT_TUNING_ID);
  const selectedTuning = useMemo<TuningOption>(() => {
    return TUNING_OPTIONS.find((option) => option.id === selectedTuningId) ?? TUNING_OPTIONS[0];
  }, [selectedTuningId]);

  const [voiceLeading, setVoiceLeading] = useState<VoiceLeadingResponse | null>(null);
  const [voiceState, setVoiceState] = useState<AsyncState>("idle");
  const [voiceError, setVoiceError] = useState<string | null>(null);
  const [activeLoopIndex, setActiveLoopIndex] = useState<number | null>(null);

  const scaleCacheRef = useRef<Map<string, ScaleNotesResponsePayload>>(new Map());
  const [, forceScaleCacheUpdate] = useState(0);

  const analysisAbortRef = useRef<AbortController | null>(null);
  const voiceAbortRef = useRef<AbortController | null>(null);

  const handleRecordedStack = useCallback(
    ({ chords, progression }: { chords: RecordedChord[]; progression: number[][] }) => {
      setRecordedChords(chords);
      setProgression(progression);
    },
    []
  );

  useEffect(() => {
    return () => {
      analysisAbortRef.current?.abort();
      voiceAbortRef.current?.abort();
    };
  }, []);

  const progressionSignature = useMemo(() => JSON.stringify(progression), [progression]);
  const tuningSignature = useMemo(() => JSON.stringify(selectedTuning.strings), [selectedTuning]);

  const analysisSteps = useMemo(() => {
    if (!analysis?.steps) {
      return [];
    }
    return [...analysis.steps].sort((left, right) => left.index - right.index);
  }, [analysis]);

  const voiceLeadingSteps = useMemo(() => {
    if (!voiceLeading?.steps) {
      return [];
    }
    return [...voiceLeading.steps].sort((left, right) => left.index - right.index);
  }, [voiceLeading]);

  useEffect(() => {
    if (isRecording) {
      return;
    }
    const trimmedServer = serverUrl.trim();
    if (progression.length === 0) {
      analysisAbortRef.current?.abort();
      setAnalysis(null);
      setAnalysisState("idle");
      setAnalysisError(null);
      return;
    }
    if (!trimmedServer) {
      setAnalysis(null);
      setAnalysisState("error");
      setAnalysisError("Server URL is required to analyze the recorded stack.");
      return;
    }
    const controller = new AbortController();
    analysisAbortRef.current?.abort();
    analysisAbortRef.current = controller;
    setAnalysisState("loading");
    setAnalysisError(null);
    const endpoint = `${normaliseUrl(trimmedServer)}/analyze`;
    fetch(endpoint, {
      method: "POST",
      headers: {
        "Content-Type": "application/json"
      },
      body: JSON.stringify({
        preset,
        progression
      }),
      signal: controller.signal
    })
      .then(async (response) => {
        if (!response.ok) {
          const detail = await response.json().catch(() => ({}));
          const message =
            (detail && typeof detail.error === "string" && detail.error) ||
            `${response.status} ${response.statusText}`;
          throw new Error(message);
        }
        return (await response.json()) as AnalyzeResponse;
      })
      .then((data) => {
        setAnalysis(data);
        setAnalysisState("success");
      })
      .catch((error) => {
        if (controller.signal.aborted) {
          return;
        }
        setAnalysis(null);
        setAnalysisState("error");
        setAnalysisError(error instanceof Error ? error.message : "Unable to analyze progression.");
      });
    return () => controller.abort();
  }, [progressionSignature, preset, serverUrl, isRecording]);

  useEffect(() => {
    if (isRecording) {
      return;
    }
    const trimmedServer = serverUrl.trim();
    if (progression.length === 0) {
      voiceAbortRef.current?.abort();
      setVoiceLeading(null);
      setVoiceState("idle");
      setVoiceError(null);
      return;
    }
    if (!trimmedServer) {
      setVoiceLeading(null);
      setVoiceState("error");
      setVoiceError("Server URL is required to compute voice-leading.");
      return;
    }
    const controller = new AbortController();
    voiceAbortRef.current?.abort();
    voiceAbortRef.current = controller;
    setVoiceState("loading");
    setVoiceError(null);
    const endpoint = `${normaliseUrl(trimmedServer)}/voice-leading`;
    const serializedTuning = selectedTuning.strings.map(serializeTuningValue);
    fetch(endpoint, {
      method: "POST",
      headers: {
        "Content-Type": "application/json"
      },
      body: JSON.stringify({
        tuning: serializedTuning,
        progression,
        maxCandidates: 12
      }),
      signal: controller.signal
    })
      .then(async (response) => {
        if (!response.ok) {
          const detail = await response.json().catch(() => ({}));
          const message =
            (detail && typeof detail.error === "string" && detail.error) ||
            `${response.status} ${response.statusText}`;
          throw new Error(message);
        }
        return (await response.json()) as VoiceLeadingResponse;
      })
      .then((data) => {
        setVoiceLeading(data);
        setVoiceState("success");
      })
      .catch((error) => {
        if (controller.signal.aborted) {
          return;
        }
        setVoiceLeading(null);
        setVoiceState("error");
        setVoiceError(error instanceof Error ? error.message : "Unable to compute voice-leading.");
      });
    return () => controller.abort();
  }, [progressionSignature, tuningSignature, serverUrl, isRecording, selectedTuning]);

  useEffect(() => {
    if (isRecording) {
      return;
    }
    if (analysisSteps.length === 0) {
      return;
    }
    const trimmedServer = serverUrl.trim();
    if (!trimmedServer) {
      return;
    }
    const pending: Array<{ key: string; mode: string; tonic: number }> = [];
    analysisSteps.forEach((step) => {
      const mode = step.mode;
      const tonic = step.tonality;
      if (typeof mode !== "string" || mode.trim().length === 0) {
        return;
      }
      if (typeof tonic !== "number") {
        return;
      }
      const key = `${mode}:${tonic}`;
      if (!scaleCacheRef.current.has(key)) {
        pending.push({ key, mode, tonic });
      }
    });
    if (pending.length === 0) {
      return;
    }
    const controller = new AbortController();
    (async () => {
      try {
        const results = await Promise.all(
          pending.map((request) =>
            fetchScaleNotes(trimmedServer, request.mode, request.tonic, controller.signal)
          )
        );
        let updated = false;
        results.forEach((payload, index) => {
          const request = pending[index];
          if (request && !scaleCacheRef.current.has(request.key)) {
            scaleCacheRef.current.set(request.key, payload);
            updated = true;
          }
        });
        if (updated) {
          forceScaleCacheUpdate((value) => value + 1);
        }
      } catch (error) {
        if (!controller.signal.aborted) {
          console.error("Failed to fetch scale overlays", error);
        }
      }
    })();
    return () => controller.abort();
  }, [analysisSteps, serverUrl, isRecording, forceScaleCacheUpdate]);

  const combinedRows = useMemo(() => {
    const length = Math.max(
      progression.length,
      analysisSteps.length,
      voiceLeadingSteps.length,
      recordedChords.length
    );
    return Array.from({ length }, (_, index) => ({
      analysis: analysisSteps[index] ?? null,
      voice: voiceLeadingSteps[index] ?? null,
      recorded: recordedChords[index] ?? null,
      pitchClasses: progression[index] ?? []
    }));
  }, [analysisSteps, voiceLeadingSteps, progression, recordedChords]);

  const showResults = progression.length > 0 && !isRecording;

  return (
    <div className="integrated-app">
      <header className="integrated-header">
        <div className="integrated-title">
          <h1>Harmonic Lab Console</h1>
          <p>Record Tonnetz gestures, inspect harmonic function, and visualise frettings in one view.</p>
        </div>
        <div className="integrated-controls">
          <label className="integrated-control">
            <span>Server URL</span>
            <input
              type="url"
              value={serverUrl}
              onChange={(event) => setServerUrl(event.target.value)}
              placeholder="http://localhost:8080"
              spellCheck={false}
            />
          </label>
          <label className="integrated-control">
            <span>Analysis Preset</span>
            <select value={preset} onChange={(event) => setPreset(event.target.value as AnalysisPreset)}>
              {PRESET_OPTIONS.map((option) => (
                <option key={option.value} value={option.value}>
                  {option.label}
                </option>
              ))}
            </select>
          </label>
          <label className="integrated-control">
            <span>Instrument Tuning</span>
            <select value={selectedTuningId} onChange={(event) => setSelectedTuningId(event.target.value)}>
              {TUNING_OPTIONS.map((option) => (
                <option key={option.id} value={option.id}>
                  {option.label}
                </option>
              ))}
            </select>
          </label>
        </div>
      </header>
      <div className="integrated-body">
        <section className="integrated-column integrated-column--left">
          <TonnetzWidget
            serverUrl={serverUrl}
            onRecordedStack={handleRecordedStack}
            onRecordingStateChange={setIsRecording}
            onLoopEvent={({ index, active }) => {
              setActiveLoopIndex((current) => (active ? index : current === index ? null : current));
            }}
          />
          {!isRecording && progression.length > 0 && (
            <div className="integrated-stack-summary">
              <h3>Recorded Stack</h3>
              <div className="stack-details">
                <div>
                  <strong>Chord count:</strong> {recordedChords.length}
                </div>
                <div>
                  <strong>Most recent:</strong>{" "}
                  {recordedChords.length > 0
                    ? (recordedChords[recordedChords.length - 1]?.noteNames ?? []).join(", ") ||
                      (recordedChords[recordedChords.length - 1]?.pitchClassNames ?? []).join(" ")
                    : "—"}
                </div>
              </div>
            </div>
          )}
        </section>
        {showResults && (
          <section className="integrated-column integrated-column--right">
            <div className="results-header">
              <div>
                <h2>Recorded Progression</h2>
                <p>Harmonic context and instrument layouts for each captured chord.</p>
              </div>
              <div className="results-meta">
                <span>{selectedTuning.strings.join(" • ")}</span>
              </div>
            </div>
            {analysisState === "loading" && <div className="status info">Analyzing recorded stack…</div>}
            {analysisState === "error" && analysisError && <div className="status error">{analysisError}</div>}
            {voiceState === "loading" && <div className="status info">Computing voice-leading layouts…</div>}
            {voiceState === "error" && voiceError && <div className="status error">{voiceError}</div>}
            <div className="progression-grid">
              {combinedRows.map((row, index) => {
                const analysisStep = row.analysis;
                const voiceStep = row.voice;
                const recorded = row.recorded;
                const chordLabel =
                  analysisStep?.pitchClassNames?.join(" ") ||
                  recorded?.noteNames?.join(", ") ||
                  recorded?.pitchClassNames?.join(" ") ||
                  row.pitchClasses.map((pc) => CHROMATIC_NAMES[normalisePitchClass(pc)] ?? String(pc)).join(" ");

                const overlays: FretboardOverlaySet[] = [];

                if (analysisStep) {
                  const modeName = analysisStep.mode;
                  const tonicValue = analysisStep.tonality;
                  const resolvedModeKey = resolveModeKey(modeName);
                  const tonicPitchClass =
                    typeof tonicValue === "number" ? normalisePitchClass(Number(tonicValue)) : null;
                  const computedIntervals = resolvedModeKey ? MODE_INTERVALS[resolvedModeKey] : undefined;
                  const computedScalePitchClasses =
                    tonicPitchClass !== null && computedIntervals
                      ? computedIntervals.map((interval) => normalisePitchClass(tonicPitchClass + interval))
                      : undefined;
                  const cacheKey =
                    typeof modeName === "string" && typeof tonicValue === "number"
                      ? `${modeName}:${tonicValue}`
                      : null;
                  const cachedScaleInfo = cacheKey ? scaleCacheRef.current.get(cacheKey) : undefined;
                  const cachedScaleNotes = cachedScaleInfo?.pitchClassNames ?? [];
                  const cachedScaleNumbers = cachedScaleInfo?.pitchClasses
                    ? cachedScaleInfo.pitchClasses.map((value) => normalisePitchClass(Number(value)))
                    : [];

                  const scaleNotes =
                    analysisStep.scalePitchClassNames && analysisStep.scalePitchClassNames.length > 0
                      ? analysisStep.scalePitchClassNames
                      : cachedScaleNotes;

                  const scalePitchClassesRaw = Array.isArray(analysisStep.scalePitchClasses)
                    ? analysisStep.scalePitchClasses.map((value) => normalisePitchClass(Number(value)))
                    : undefined;
                  const scalePitchClasses =
                    scalePitchClassesRaw && scalePitchClassesRaw.length > 0
                      ? scalePitchClassesRaw
                      : computedScalePitchClasses && computedScalePitchClasses.length > 0
                        ? computedScalePitchClasses
                        : cachedScaleNumbers;

                  const fallbackScaleNames =
                    scalePitchClasses.length > 0
                      ? scalePitchClasses.map((pc) => CHROMATIC_NAMES[pc] ?? String(pc))
                      : [];
                  const scalePitchClassNames =
                    analysisStep.scalePitchClassNames && analysisStep.scalePitchClassNames.length > 0
                      ? analysisStep.scalePitchClassNames
                      : fallbackScaleNames.length > 0
                        ? fallbackScaleNames
                        : scaleNotes;

                  const deviationRaw = Array.isArray(analysisStep.scaleDeviationPitchClasses)
                    ? analysisStep.scaleDeviationPitchClasses.map((value) => normalisePitchClass(Number(value)))
                    : cachedScaleInfo?.diffPitchClasses
                    ? cachedScaleInfo.diffPitchClasses.map((value) => normalisePitchClass(Number(value)))
                    : [];
                  const scaleDeviationPitchClasses = Array.from(new Set(deviationRaw)).sort(
                    (a, b) => a - b
                  );
                  const scaleDeviationNames =
                    analysisStep.scaleDeviationPitchClassNames &&
                    analysisStep.scaleDeviationPitchClassNames.length > 0
                      ? analysisStep.scaleDeviationPitchClassNames
                      : cachedScaleInfo?.diffPitchClassNames && cachedScaleInfo.diffPitchClassNames.length > 0
                        ? cachedScaleInfo.diffPitchClassNames
                        : scaleDeviationPitchClasses.map((pc) => CHROMATIC_NAMES[pc] ?? String(pc));

                  let resolvedRoot: string | undefined;
                  if (tonicPitchClass !== null && scalePitchClasses.length > 0) {
                    const idx = scalePitchClasses.findIndex((value) => value === tonicPitchClass);
                    if (idx >= 0 && scalePitchClassNames[idx]) {
                      resolvedRoot = scalePitchClassNames[idx];
                    }
                  }
                  if (!resolvedRoot && scalePitchClassNames.length > 0) {
                    resolvedRoot = scalePitchClassNames[0];
                  }
                  if (!resolvedRoot && scaleNotes.length > 0) {
                    resolvedRoot = scaleNotes[0];
                  }

                  const scaleLabel =
                    resolvedRoot && analysisStep.mode
                      ? `${resolvedRoot} ${analysisStep.mode}`
                      : analysisStep.key || analysisStep.mode || "Scale";

                  if (scalePitchClassNames.length > 0) {
                    overlays.push({
                      id: `scale-${index}`,
                      notes: scalePitchClassNames,
                      label: scaleLabel,
                      scaleMode: analysisStep.mode ?? undefined,
                      scaleRoot: resolvedRoot,
                      pitchClassNumbers: scalePitchClasses
                    });
                  }

                  if (scaleDeviationPitchClasses.length > 0) {
                    overlays.push({
                      id: `scale-diff-${index}`,
                      notes: scaleDeviationNames,
                      color: "transparent",
                      pitchClassNumbers: scaleDeviationPitchClasses,
                      useOutline: true,
                      outlineColor: DEFAULT_SCALE_DIFF_OUTLINE_COLOR,
                      strokeWidth: DEFAULT_SCALE_DIFF_STROKE_WIDTH,
                      scaleMode: analysisStep.mode ?? undefined,
                      scaleRoot: resolvedRoot
                    });
                  }
                }

                if (voiceStep?.positions?.length) {
                  overlays.push({
                    id: `chord-${index}`,
                    positions: voiceStep.positions,
                    color: DEFAULT_CHORD_COLOR,
                    label: "Chord"
                  });
                }

                let diagramContent: JSX.Element;
                if (overlays.length > 0) {
                  diagramContent = (
                    <FretboardOverlayDiagram
                      tuning={selectedTuning.strings}
                      overlays={overlays}
                      serverUrl={serverUrl}
                    />
                  );
                } else if (voiceState === "loading") {
                  diagramContent = <div className="status info">Preparing fretboard layout…</div>;
                } else {
                  diagramContent = <div className="status">No fretboard voicing returned for this chord.</div>;
                }

                return (
                  <div
                    key={`progression-row-${index}`}
                    className={
                      activeLoopIndex === index
                        ? "progression-row progression-row--active"
                        : "progression-row"
                    }
                  >
                    <div className="progression-row-info">
                      <div className="progression-row-header">
                        <span className="progression-row-index">{index + 1}</span>
                        <div>
                          <strong>{chordLabel || "Chord"}</strong>
                          {analysisStep?.function && (
                            <span className="progression-row-subtitle">{analysisStep.function}</span>
                          )}
                        </div>
                      </div>
                      <dl className="progression-row-details">
                        {[
                          {
                            key: "chord",
                            label: "Chord",
                            value: chordLabel || "—"
                          },
                          {
                            key: "degree",
                            label: "Degree",
                            value: analysisStep?.degree || "—"
                          },
                          {
                            key: "key",
                            label: "Key",
                            value: analysisStep?.key || "—"
                          },
                          {
                            key: "roman",
                            label: "Roman Numeral",
                            value: analysisStep?.romanNumeral || "—"
                          }
                        ].map((entry) => (
                          <Fragment key={`${index}-${entry.key}`}>
                            <dt className="analysis-label">{entry.label}</dt>
                            <dd>{entry.value}</dd>
                          </Fragment>
                        ))}
                      </dl>
                    </div>
                    <div className="progression-row-diagram">{diagramContent}</div>
                  </div>
                );
              })}
            </div>
          </section>
        )}
      </div>
    </div>
  );
}

async function fetchScaleNotes(
  serverUrl: string,
  mode: string,
  tonic: number,
  signal?: AbortSignal
): Promise<ScaleNotesResponsePayload> {
  const empty: ScaleNotesResponsePayload = {
    pitchClasses: [],
    pitchClassNames: [],
    diffPitchClasses: [],
    diffPitchClassNames: []
  };
  if (!serverUrl) {
    return empty;
  }
  try {
    const response = await fetch(`${normaliseUrl(serverUrl)}/scale-notes`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json"
      },
      body: JSON.stringify({ mode, tonic }),
      signal
    });
    if (!response.ok) {
      const detail = await response.json().catch(() => ({}));
      const errMessage =
        (detail && typeof detail.error === "string" && detail.error) ||
        `${response.status} ${response.statusText}`;
      throw new Error(errMessage);
    }
    const payload = (await response.json()) as ScaleNotesResponsePayload;
    return {
      pitchClasses: Array.isArray(payload.pitchClasses) ? payload.pitchClasses : [],
      pitchClassNames: Array.isArray(payload.pitchClassNames) ? payload.pitchClassNames : [],
      diffPitchClasses: Array.isArray(payload.diffPitchClasses) ? payload.diffPitchClasses : [],
      diffPitchClassNames: Array.isArray(payload.diffPitchClassNames) ? payload.diffPitchClassNames : []
    } satisfies ScaleNotesResponsePayload;
  } catch (error) {
    if (error instanceof DOMException && error.name === "AbortError") {
      return empty;
    }
    throw error;
  }
}

export default App;
