import { FormEvent, useEffect, useMemo, useRef, useState } from "react";
import TonnetzWidget from "./TonnetzWidget";

type PitchClassName = string;

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

type BackendFretboardOccurrence = {
  string: number;
  fret: number;
  pitchClass: number;
  pitchClassName: string;
  octave: number | null;
  noteName: string;
};

type BackendFretboardOccurrenceSet = {
  id: string;
  label?: string | null;
  pitchClasses: number[];
  pitchClassNames: string[];
  occurrences: BackendFretboardOccurrence[];
};

type FretboardOccurrencesResponsePayload = {
  noteSets: BackendFretboardOccurrenceSet[];
};

type BackendChordNameEntry = {
  id: string;
  pitchClasses: number[];
  name: string | null;
  aliases: string[];
};

type FretboardChordNamesResponsePayload = {
  chordNames: BackendChordNameEntry[];
};

type ScaleNotesResponsePayload = {
  pitchClasses: number[];
  pitchClassNames: string[];
  diffPitchClasses?: number[];
  diffPitchClassNames?: string[];
};

const PRESET_OPTIONS: Array<{ label: string; value: AnalysisPreset }> = [
  { label: "Major / Minor (Tonic-Subdominant-Dominant)", value: "MajorMinorTSD" },
  { label: "Major / Minor (Full diatonic)", value: "MajorMinorDiatonic" },
  { label: "Modal (T-S-D)", value: "ModalTSD" },
  { label: "Modal (Full diatonic)", value: "ModalDiatonic" }
];

const mozartExample = [
  ["E", "A", "Cs"],
  ["E", "A", "Cs"],
  ["D", "E", "Gs", "B"],
  ["Cs", "E", "A"]
];

const DEFAULT_PROGRESSION = JSON.stringify(mozartExample, null, 2);

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

const DEFAULT_TUNING_LIST = ["E2", "A2", "D3", "G3", "B3", "E4"] as const;
const DEFAULT_TUNING = JSON.stringify(DEFAULT_TUNING_LIST);
const DEFAULT_MAX_CANDIDATES = "12";

const DEFAULT_FRET_COUNT = 12;
const DEFAULT_CHORD_COLOR = "#ef4444";
const DEFAULT_SCALE_DIFF_OUTLINE_COLOR = "#FDE725";
const DEFAULT_SCALE_DIFF_STROKE_WIDTH = 3;
type SerializedTuningEntry = string | number | { pitch: string; octave?: number };

const VIRIDIS_COLORS: readonly string[] = [
  "#440154",
  "#482878",
  "#3E4A89",
  "#31688E",
  "#26828E",
  "#1F9E89",
  "#35B779",
  "#6CCE59",
  "#B4DD2C",
  "#FDE725"
] as const;

const normalizePitchClass = (value: number): number => ((value % 12) + 12) % 12;

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
  return MODE_INTERVALS[trimmed]
    ? trimmed
    : MODE_INTERVALS[normalized]
      ? normalized
      : MODE_INTERVALS[normalized.charAt(0).toUpperCase() + normalized.slice(1).toLowerCase()]
        ? normalized.charAt(0).toUpperCase() + normalized.slice(1).toLowerCase()
        : null;
};


type OverlayMarker = {
  string: number;
  fret: number;
  label?: string;
  group?: string;
  color?: string;
  outlineColor?: string;
  strokeWidth?: number;
};

type FretboardOverlaySet = {
  id: string;
  notes?: PitchClassName[];
  positions?: VoiceLeadingStringPosition[];
  color?: string;
  label?: string;
  maxFret?: number;
  scaleMode?: string;
  scaleRoot?: string;
  pitchClassNumbers?: number[];
  useOutline?: boolean;
  outlineColor?: string;
  strokeWidth?: number;
};

function buildMarkersForPositions(
  tuning: string[],
  positions: VoiceLeadingStringPosition[],
  group: string,
  color?: string
): OverlayMarker[] {
  const stringCount = tuning.length;
  return positions.map((position) => {
    const rawString = position.string ?? 0;
    const fret = position.fret ?? 0;
    let stringNumber: number;
    if (rawString >= 1 && rawString <= stringCount) {
      stringNumber = rawString;
    } else {
      stringNumber = mapBackendStringToFretboard(rawString, stringCount);
    }
    stringNumber = Math.max(1, Math.min(stringCount, stringNumber));
    const label =
      (position.label && position.label.length > 0 ? position.label : undefined) ??
      (position.noteName && position.noteName.length > 0 ? position.noteName : undefined) ??
      (position.finger && position.finger.length > 0 ? position.finger : undefined) ??
      position.pitchClass ??
      undefined;
    return {
      string: stringNumber,
      fret,
      label,
      group,
      color
    };
  });
}

function normaliseUrl(raw: string): string {
  return raw.replace(/\/+$/, "");
}

function parseJsonInput<T>(raw: string, description: string): T {
  try {
    return JSON.parse(raw) as T;
  } catch (err) {
    const message = err instanceof Error ? err.message : "Invalid JSON input.";
    throw new Error(`${description}: ${message}`);
  }
}

function splitPitchAndOctave(token: string): { pitch: string; octave?: number } {
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
}

function formatPitchDisplay(pitch: string, octave?: number): string {
  if (!pitch) {
    return "";
  }
  const letter = pitch[0];
  const accidental = pitch.slice(1).replace(/B/g, "b");
  const base = `${letter}${accidental}`;
  return octave !== undefined ? `${base}${octave}` : base;
}

function serializeTuningValue(entry: unknown): SerializedTuningEntry {
  if (typeof entry === "string" && entry.trim().length > 0) {
    const { pitch, octave } = splitPitchAndOctave(entry);
    const payload: { pitch: string; octave?: number } = { pitch };
    if (octave !== undefined) {
      payload.octave = octave;
    }
    return payload;
  }
  if (typeof entry === "number") {
    return entry;
  }
  if (entry && typeof entry === "object") {
    const obj = entry as { pitch?: unknown; octave?: unknown };
    if (typeof obj.pitch === "string" && obj.pitch.trim().length > 0) {
      const { pitch, octave } = splitPitchAndOctave(obj.pitch);
      const payload: { pitch: string; octave?: number } = { pitch };
      if (typeof obj.octave === "number") {
        payload.octave = obj.octave;
      } else if (octave !== undefined) {
        payload.octave = octave;
      }
      return payload;
    }
    return obj as SerializedTuningEntry;
  }
  throw new Error("Tuning entries must be strings, numbers, or pitch objects.");
}

function normaliseTuningEntries(entries: unknown[]): string[] {
  return entries.map((entry, index) => {
    if (typeof entry === "string" && entry.trim().length > 0) {
      const { pitch, octave } = splitPitchAndOctave(entry);
      return formatPitchDisplay(pitch, octave);
    }
    if (typeof entry === "number") {
      return entry.toString(10);
    }
    if (entry && typeof entry === "object") {
      const obj = entry as { pitch?: unknown; octave?: unknown };
      if (typeof obj.pitch === "string") {
        const { pitch, octave } = splitPitchAndOctave(obj.pitch);
        const octaveValue = typeof obj.octave === "number" ? obj.octave : octave;
        return formatPitchDisplay(pitch, octaveValue);
      }
    }
    throw new Error(`Tuning entry at index ${index} must be a string, number, or pitch object.`);
  });
}

function isPitchClassMatrix(value: unknown): value is unknown[][] {
  return Array.isArray(value) && value.every((item) => Array.isArray(item));
}

function App() {
  const [serverUrl, setServerUrl] = useState<string>(defaultServer);
  const [preset, setPreset] = useState<AnalysisPreset>("ModalDiatonic");
  const [progressionText, setProgressionText] = useState<string>(DEFAULT_PROGRESSION);
  const [isSubmitting, setSubmitting] = useState<boolean>(false);
  const [status, setStatus] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [response, setResponse] = useState<AnalyzeResponse | null>(null);
  const controllerRef = useRef<AbortController | null>(null);

  const [voiceTuningText, setVoiceTuningText] = useState<string>(DEFAULT_TUNING);
  const [voiceProgressionText, setVoiceProgressionText] = useState<string>(DEFAULT_PROGRESSION);
  const [voiceMaxCandidates, setVoiceMaxCandidates] = useState<string>(DEFAULT_MAX_CANDIDATES);
  const [voiceStatus, setVoiceStatus] = useState<string | null>(null);
  const [voiceError, setVoiceError] = useState<string | null>(null);
  const [voiceResponse, setVoiceResponse] = useState<VoiceLeadingResponse | null>(null);
  const [voiceTuningDisplay, setVoiceTuningDisplay] = useState<string[]>([...DEFAULT_TUNING_LIST]);
  const [isVoiceSubmitting, setVoiceSubmitting] = useState<boolean>(false);
  const [isDemoRunning, setDemoRunning] = useState<boolean>(false);
  const voiceControllerRef = useRef<AbortController | null>(null);
  const [overlayText, setOverlayText] = useState<string>("[]");

  const { customOverlays, overlayParseError } = useMemo(() => {
    if (overlayText.trim().length === 0) {
      return { customOverlays: [] as FretboardOverlaySet[], overlayParseError: null };
    }

    try {
      const parsed = JSON.parse(overlayText);
      if (!Array.isArray(parsed)) {
        throw new Error("Overlay configuration must be a JSON array.");
      }
      const overlays: FretboardOverlaySet[] = parsed
        .map((raw, index) => {
          if (!raw || typeof raw !== "object") {
            throw new Error(`Overlay entry at index ${index} must be an object.`);
          }
          const id = typeof raw.id === "string" && raw.id.trim().length > 0 ? raw.id : `custom-${index}`;
          const color = typeof raw.color === "string" ? raw.color : undefined;
          const label = typeof raw.label === "string" ? raw.label : undefined;
          const maxFret = typeof raw.maxFret === "number" ? raw.maxFret : undefined;
          const notes = Array.isArray(raw.notes) ? raw.notes.map((note: unknown) => String(note)) : undefined;
          const positions = Array.isArray(raw.positions)
            ? (raw.positions as VoiceLeadingStringPosition[])
            : undefined;
          if ((!notes || notes.length === 0) && (!positions || positions.length === 0)) {
            throw new Error(
              `Overlay entry "${id}" must include a non-empty "notes" array or a non-empty "positions" array.`
            );
          }
          return { id, color, label, maxFret, notes, positions } satisfies FretboardOverlaySet;
        })
        .filter(Boolean);

      return { customOverlays: overlays, overlayParseError: null };
    } catch (err) {
      const message = err instanceof Error ? err.message : "Unable to parse overlay configuration.";
      return { customOverlays: [] as FretboardOverlaySet[], overlayParseError: message };
    }
  }, [overlayText]);

  const targetUrl = useMemo(() => `${normaliseUrl(serverUrl)}/analyze`, [serverUrl]);
  const voiceUrl = useMemo(() => `${normaliseUrl(serverUrl)}/voice-leading`, [serverUrl]);

  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    controllerRef.current?.abort();
    setSubmitting(true);
    setStatus("Submitting request… This can take several minutes for long progressions.");
    setError(null);
    setResponse(null);

    let payload;
    try {
      payload = JSON.parse(progressionText);
      if (!Array.isArray(payload) || payload.some((item) => !Array.isArray(item))) {
        throw new Error("Progression must be a JSON array of arrays.");
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : "Could not parse progression JSON.");
      setStatus(null);
      setSubmitting(false);
      return;
    }

    const controller = new AbortController();
    controllerRef.current = controller;

    try {
      const res = await fetch(targetUrl, {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify({
          preset,
          progression: payload
        }),
        signal: controller.signal
      });

      if (!res.ok) {
        const detail = await res.json().catch(() => ({}));
        const errorMessage =
          (detail && typeof detail.error === "string" && detail.error) ||
          `${res.status} ${res.statusText}`;
        throw new Error(errorMessage);
      }

      const data = (await res.json()) as AnalyzeResponse;
      setResponse(data);
      setStatus("Analysis complete.");
    } catch (err: unknown) {
      if (err instanceof DOMException && err.name === "AbortError") {
        setError("Request cancelled.");
      } else {
        setError(err instanceof Error ? err.message : "Unexpected error.");
      }
      setStatus(null);
    } finally {
      setSubmitting(false);
      controllerRef.current = null;
    }
  };

  const handleVoiceLeadingSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    voiceControllerRef.current?.abort();
    setVoiceStatus("Fetching optimal fretboard voicings…");
    setVoiceError(null);
    setVoiceResponse(null);

    let tuningEntries: unknown[];
    let progressionPayload: unknown[][];

    try {
      const tuningValue = parseJsonInput<unknown>(voiceTuningText, "Tuning");
      if (!Array.isArray(tuningValue)) {
        throw new Error("Tuning must be a JSON array of pitch-class names or numbers.");
      }
      tuningEntries = tuningValue;
      const displayTuning = normaliseTuningEntries(tuningEntries);
      setVoiceTuningDisplay(displayTuning);

      const progressionValue = parseJsonInput<unknown>(voiceProgressionText, "Progression");
      if (!isPitchClassMatrix(progressionValue)) {
        throw new Error("Progression must be a JSON array of pitch-class arrays.");
      }
      progressionPayload = progressionValue;
    } catch (err) {
      setVoiceError(err instanceof Error ? err.message : "Invalid voice-leading input.");
      setVoiceStatus(null);
      return;
    }

    let maxCandidatesNumber: number | undefined;
    if (voiceMaxCandidates.trim().length > 0) {
      const parsed = Number.parseInt(voiceMaxCandidates, 10);
      if (!Number.isInteger(parsed) || parsed <= 0) {
        setVoiceError("Max candidates must be a positive integer.");
        setVoiceStatus(null);
        return;
      }
      maxCandidatesNumber = parsed;
    }

    const controller = new AbortController();
    voiceControllerRef.current = controller;
    setVoiceSubmitting(true);

    try {
      const serializedTuning = tuningEntries.map(serializeTuningValue);

      const res = await fetch(voiceUrl, {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify({
          tuning: serializedTuning,
          progression: progressionPayload,
          maxCandidates: maxCandidatesNumber
        }),
        signal: controller.signal
      });

      if (!res.ok) {
        const detail = await res.json().catch(() => ({}));
        const errMessage =
          (detail && typeof detail.error === "string" && detail.error) ||
          `${res.status} ${res.statusText}`;
        throw new Error(errMessage);
      }

      const data = (await res.json()) as VoiceLeadingResponse;
      console.debug("voice-leading payload", data);
      setVoiceResponse(data);
      setVoiceStatus("Voice leading computed successfully.");
    } catch (err) {
      if (err instanceof DOMException && err.name === "AbortError") {
        setVoiceError("Voice-leading request cancelled.");
      } else {
        setVoiceError(err instanceof Error ? err.message : "Unexpected error.");
      }
      setVoiceStatus(null);
    } finally {
      setVoiceSubmitting(false);
      voiceControllerRef.current = null;
    }
  };

  const runMozartDemo = async () => {
    if (isDemoRunning) {
      return;
    }
    setDemoRunning(true);
    setError(null);
    setStatus("Running Mozart demo…");
    setVoiceError(null);
    setVoiceStatus(null);
    setVoiceResponse(null);
    setVoiceSubmitting(true);

    const tuningStringList = [...DEFAULT_TUNING_LIST];
    const progressionData = JSON.parse(DEFAULT_PROGRESSION) as unknown[][];

    setProgressionText(DEFAULT_PROGRESSION);
    setVoiceProgressionText(DEFAULT_PROGRESSION);
    setVoiceTuningText(JSON.stringify(tuningStringList));
    setVoiceTuningDisplay([...tuningStringList]);

    try {
      const analyzeRes = await fetch(targetUrl, {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify({
          preset,
          progression: progressionData
        })
      });

      if (!analyzeRes.ok) {
        const detail = await analyzeRes.json().catch(() => ({}));
        const analyzeError =
          (detail && typeof detail.error === "string" && detail.error) ||
          `${analyzeRes.status} ${analyzeRes.statusText}`;
        throw new Error(analyzeError);
      }

      const analyzeData = (await analyzeRes.json()) as AnalyzeResponse;
      setResponse(analyzeData);
      setStatus("Analysis complete.");

      const serializedTuning = tuningStringList.map((value) => serializeTuningValue(value));

      const voiceRes = await fetch(voiceUrl, {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify({
          tuning: serializedTuning,
          progression: progressionData,
          maxCandidates: Number.parseInt(voiceMaxCandidates, 10) || undefined
        })
      });

      if (!voiceRes.ok) {
        const detail = await voiceRes.json().catch(() => ({}));
        const voiceErrorMessage =
          (detail && typeof detail.error === "string" && detail.error) ||
          `${voiceRes.status} ${voiceRes.statusText}`;
        throw new Error(voiceErrorMessage);
      }

      const voiceData = (await voiceRes.json()) as VoiceLeadingResponse;
      setVoiceResponse(voiceData);
      setVoiceStatus("Voice leading computed successfully.");
    } catch (err) {
      const message = err instanceof Error ? err.message : "Unexpected error.";
      setError(message);
      setVoiceError(message);
      setStatus(null);
      setVoiceStatus(null);
    } finally {
      setVoiceSubmitting(false);
      setDemoRunning(false);
    }
  };

  return (
    <div className="app">
      <h1>Harmonic Function Analyzer</h1>
      <p>
        Submit a series of pitch-class sets (strings or integers mod 12) to call the backend
        <code>analyze</code> endpoint. The request is made directly from your browser, so ensure the
        server is reachable (CORS enabled).
      </p>

      <form onSubmit={handleSubmit}>
        <div className="field">
          <label htmlFor="server">Server URL</label>
          <input
            id="server"
            type="url"
            required
            value={serverUrl}
            onChange={(event) => setServerUrl(event.target.value)}
            placeholder="http://localhost:8080"
            autoComplete="off"
          />
        </div>

        <div className="field">
          <label htmlFor="preset">Preset</label>
          <select
            id="preset"
            value={preset}
            onChange={(event) => setPreset(event.target.value as AnalysisPreset)}
          >
            {PRESET_OPTIONS.map((option) => (
              <option key={option.value} value={option.value}>
                {option.label}
              </option>
            ))}
          </select>
        </div>

        <div className="field">
          <label htmlFor="progression">Progression (JSON array of pitch-class arrays)</label>
          <textarea
            id="progression"
            value={progressionText}
            onChange={(event) => setProgressionText(event.target.value)}
            spellCheck={false}
          />
          <small>
            Strings are treated as chromatic names (&ldquo;C#&rdquo;, &ldquo;Bb&rdquo;), numbers as
            mod-12 pitch classes.
          </small>
        </div>

        <div className="actions">
          <button type="submit" disabled={isSubmitting}>
            {isSubmitting ? "Analyzing…" : "Analyze progression"}
          </button>
          {isSubmitting && (
            <button
              type="button"
              className="cancel-button"
              onClick={() => {
                controllerRef.current?.abort();
              }}
            >
              Cancel request
            </button>
          )}
          {status && <span className="status success">{status}</span>}
          {error && <span className="status error">{error}</span>}
        </div>
      </form>

      {response && (
        <div className="field">
          <label>Response</label>
          <pre>{JSON.stringify(response, null, 2)}</pre>
        </div>
      )}

      <section>
        <h2>Tonnetz Tiling Explorer</h2>
        <p>
          Render the Ammann–Beekner tiling for a chosen interval set and diatonic degree. The backend
          provides pitch assignments for each lattice vertex; click or drag across polygons to hear the
          corresponding chords.
        </p>
        <TonnetzWidget serverUrl={serverUrl} />
      </section>

      <hr className="divider" />

      <section>
        <h2>Voice-Leading Fretting Optimizer</h2>
        <p>
          Submit a tuning and a list of pitch-class sets to call the <code>voice-leading</code>{" "}
          endpoint. The response is rendered as a sequence of interactive fretboard diagrams using{" "}
          <a href="https://moonwave99.github.io/fretboard.js/documentation-fretboard.html">Fretboard.js</a>.
        </p>
        <p className="hint">
          Try the Mozart example progression (same as the analyzer above) to see a sample fretboard
          layout.
        </p>

        <form onSubmit={handleVoiceLeadingSubmit}>
          <div className="field">
            <label htmlFor="vl-tuning">Tuning (JSON array)</label>
            <input
              id="vl-tuning"
              value={voiceTuningText}
              onChange={(event) => setVoiceTuningText(event.target.value)}
              spellCheck={false}
            />
          </div>

          <div className="field">
            <label htmlFor="vl-progression">Progression (JSON array of pitch-class arrays)</label>
            <textarea
              id="vl-progression"
              value={voiceProgressionText}
              onChange={(event) => setVoiceProgressionText(event.target.value)}
              spellCheck={false}
            />
          </div>

        <div className="field">
          <label htmlFor="vl-max">Max candidates per chord</label>
          <input
            id="vl-max"
            type="number"
              min={1}
              value={voiceMaxCandidates}
              onChange={(event) => setVoiceMaxCandidates(event.target.value)}
            />
            <small>
              Controls how many candidate frettings are considered for each chord (defaults to 12).
            </small>
        </div>

        <div className="field">
          <label htmlFor="vl-overlays">Additional overlays (JSON)</label>
          <textarea
            id="vl-overlays"
            value={overlayText}
            onChange={(event) => setOverlayText(event.target.value)}
            spellCheck={false}
            placeholder='[
  { "label": "Scale notes", "color": "#38bdf8", "notes": ["C", "D", "E", "F", "G", "A", "B"] }
]'
          />
          <small>
            Provide extra note sets or explicit positions to overlay. Each entry may include
            <code>notes</code>, <code>color</code>, <code>label</code>, and <code>maxFret</code>.
          </small>
          {overlayParseError && <span className="status error">{overlayParseError}</span>}
        </div>

        <div className="actions">
          <button type="submit" disabled={isVoiceSubmitting}>
              {isVoiceSubmitting ? "Computing…" : "Optimize frettings"}
            </button>
            <button
              type="button"
              className="secondary-button"
              onClick={() => {
    setVoiceTuningText(DEFAULT_TUNING);
    setVoiceProgressionText(DEFAULT_PROGRESSION);
    setVoiceTuningDisplay(normaliseTuningEntries(tuningStringList));
              }}
            >
              Load Mozart example
            </button>
            {isVoiceSubmitting && (
              <button
                type="button"
                className="cancel-button"
                onClick={() => {
                  voiceControllerRef.current?.abort();
                }}
              >
              Cancel voice-leading
            </button>
          )}
          {voiceStatus && <span className="status success">{voiceStatus}</span>}
          {voiceError && <span className="status error">{voiceError}</span>}
        </div>
        <div className="actions">
          <button
            type="button"
            onClick={runMozartDemo}
            disabled={isDemoRunning || isVoiceSubmitting}
          >
            {isDemoRunning ? "Running demo…" : "Run Mozart demo"}
          </button>
        </div>
      </form>

      {voiceResponse && (
        <VoiceLeadingResult
          response={voiceResponse}
            tuning={voiceTuningDisplay}
            analysisSteps={response?.steps}
            customOverlays={customOverlays}
            serverUrl={serverUrl}
          />
        )}
      </section>
    </div>
  );
}

type VoiceLeadingResultProps = {
  response: VoiceLeadingResponse;
  tuning: string[];
  analysisSteps?: HarmonicStepResponse[];
  customOverlays?: FretboardOverlaySet[];
  serverUrl: string;
};

function VoiceLeadingResult({ response, tuning, analysisSteps, customOverlays, serverUrl }: VoiceLeadingResultProps) {
  const steps = Array.isArray(response?.steps) ? response.steps : [];
  const scaleCacheRef = useRef<Map<string, ScaleNotesResponsePayload>>(new Map());
  const [, setScaleCacheVersion] = useState(0);

  useEffect(() => {
    if (!serverUrl || !analysisSteps) {
      return;
    }

    const pending: Array<{ key: string; mode: string; tonic: number }> = [];
    analysisSteps.forEach((step) => {
      const modeName = step.mode;
      const tonicValue = step.tonality;
      const key = `${modeName}:${tonicValue}`;
      const hasScale = Array.isArray(step.scalePitchClassNames) && step.scalePitchClassNames.length > 0;
      if (!hasScale && !scaleCacheRef.current.has(key) && !pending.some((item) => item.key === key)) {
        pending.push({ key, mode: modeName, tonic: tonicValue });
      }
    });

    if (pending.length === 0) {
      return;
    }

    const controller = new AbortController();
    (async () => {
      try {
        const results = await Promise.all(
          pending.map((request) => fetchScaleNotes(serverUrl, request.mode, request.tonic, controller.signal))
        );
        let updated = false;
        results.forEach((payload, index) => {
          const request = pending[index];
          if (!scaleCacheRef.current.has(request.key)) {
            scaleCacheRef.current.set(request.key, payload);
            updated = true;
          }
        });
        if (updated) {
          setScaleCacheVersion((value) => value + 1);
        }
      } catch (err) {
        if (!(err instanceof DOMException && err.name === "AbortError")) {
          console.error("Failed to fetch scale notes", err);
        }
      }
    })();

    return () => controller.abort();
  }, [analysisSteps, serverUrl, setScaleCacheVersion]);

  if (steps.length === 0) {
    return (
      <div className="voice-results">
        <div className="voice-summary">
          No frettings returned by the server for the supplied progression.
        </div>
      </div>
    );
  }

  return (
    <div className="voice-results">
      <div className="voice-summary">
        <strong>Total cost:</strong> {response.totalCost}
      </div>
      <div className="voice-steps">
        {steps.map((step, idx) => {
          const pitchNames = Array.isArray(step.pitchClassNames)
            ? step.pitchClassNames
            : [];
          const analysisStep = analysisSteps?.[idx];
          const modeName = analysisStep?.mode;
          const tonicValue = analysisStep?.tonality;
          const resolvedModeKey = resolveModeKey(modeName);
          const tonicPitchClass = typeof tonicValue === "number" ? normalizePitchClass(tonicValue) : null;
          const computedIntervals = resolvedModeKey ? MODE_INTERVALS[resolvedModeKey] : undefined;
          const computedScalePitchClasses =
            tonicPitchClass !== null && computedIntervals
              ? computedIntervals.map((interval) => normalizePitchClass(tonicPitchClass + interval))
              : undefined;
          const cacheKey = modeName != null && tonicValue != null ? `${modeName}:${tonicValue}` : null;
          const cachedScaleInfo = cacheKey ? scaleCacheRef.current.get(cacheKey) : undefined;
          const cachedScaleNotes = cachedScaleInfo?.pitchClassNames ?? [];
          const cachedScaleNumbers = cachedScaleInfo?.pitchClasses
            ? cachedScaleInfo.pitchClasses.map((value) => normalizePitchClass(Number(value)))
            : [];
          const scaleNotes =
            analysisStep?.scalePitchClassNames && analysisStep.scalePitchClassNames.length > 0
              ? analysisStep.scalePitchClassNames
              : cachedScaleNotes;
          const scalePitchClassesRaw = Array.isArray(analysisStep?.scalePitchClasses)
            ? analysisStep!.scalePitchClasses!.map((value) => normalizePitchClass(Number(value)))
            : undefined;
          const scalePitchClasses = scalePitchClassesRaw && scalePitchClassesRaw.length > 0
            ? scalePitchClassesRaw
            : computedScalePitchClasses && computedScalePitchClasses.length > 0
              ? computedScalePitchClasses
              : cachedScaleNumbers;
          const scalePitchClassNames =
            analysisStep?.scalePitchClassNames && analysisStep.scalePitchClassNames.length > 0
              ? analysisStep.scalePitchClassNames
              : computedScalePitchClasses
              ? computedScalePitchClasses.map((pc) => CHROMATIC_NAMES[pc] ?? String(pc))
              : scaleNotes.length > 0
              ? scaleNotes
              : cachedScaleNotes;
          const scaleDeviationPitchClassesRaw = Array.isArray(analysisStep?.scaleDeviationPitchClasses)
            ? analysisStep!.scaleDeviationPitchClasses!.map((value) => normalizePitchClass(Number(value)))
            : cachedScaleInfo?.diffPitchClasses
            ? cachedScaleInfo.diffPitchClasses.map((value) => normalizePitchClass(Number(value)))
            : [];
          const scaleDeviationPitchClasses = Array.from(
            new Set(scaleDeviationPitchClassesRaw.map((value) => normalizePitchClass(Number(value))))
          ).sort((a, b) => a - b);
          const scaleDeviationNames =
            analysisStep?.scaleDeviationPitchClassNames && analysisStep.scaleDeviationPitchClassNames.length > 0
              ? analysisStep.scaleDeviationPitchClassNames
              : cachedScaleInfo?.diffPitchClassNames && cachedScaleInfo.diffPitchClassNames.length > 0
              ? cachedScaleInfo.diffPitchClassNames
              : scaleDeviationPitchClasses.map((pc) => CHROMATIC_NAMES[pc] ?? String(pc));
          const tonalityPc = typeof step.tonality === "number" ? ((step.tonality % 12) + 12) % 12 : undefined;
          let resolvedRoot: string | undefined;
          if (
            tonalityPc !== undefined &&
            Array.isArray(scalePitchClasses) &&
            Array.isArray(scalePitchClassNames)
          ) {
            const idx = scalePitchClasses.findIndex((value) => ((value % 12) + 12) % 12 === tonalityPc);
            if (idx >= 0 && typeof scalePitchClassNames[idx] === "string") {
              resolvedRoot = scalePitchClassNames[idx];
            }
          }
          if (!resolvedRoot && Array.isArray(scaleNotes) && scaleNotes.length > 0) {
            resolvedRoot = scaleNotes[0];
          }

          const scaleLabel =
            resolvedRoot
              ? analysisStep?.mode
                ? `${resolvedRoot} ${analysisStep.mode}`
                : `${resolvedRoot} scale`
              : analysisStep?.key ?? (analysisStep?.mode ?? "Scale");

          const baseOverlays: FretboardOverlaySet[] = [];

          let scaleOverlay: FretboardOverlaySet | null = null;
          if (scaleNotes.length > 0) {
            scaleOverlay = {
              id: `scale-${step.index}`,
              notes: scaleNotes,
              label: scaleLabel,
              scaleMode: analysisStep?.mode,
              scaleRoot: resolvedRoot,
              pitchClassNumbers: scalePitchClasses
            };
            console.debug("scale overlay", {
              chordIndex: step.index,
              scalePitchClasses,
              scalePitchClassNames,
              scaleNotes,
              scaleLabel
            });
            baseOverlays.push(scaleOverlay);
          }

          if (scaleDeviationPitchClasses.length > 0) {
            const scaleDiffOverlay: FretboardOverlaySet = {
              id: `scale-diff-${step.index}`,
              notes: scaleDeviationNames,
              color: "transparent",
              pitchClassNumbers: scaleDeviationPitchClasses,
              useOutline: true,
              outlineColor: DEFAULT_SCALE_DIFF_OUTLINE_COLOR,
              strokeWidth: DEFAULT_SCALE_DIFF_STROKE_WIDTH,
              scaleMode: analysisStep?.mode,
              scaleRoot: resolvedRoot
            } satisfies FretboardOverlaySet;
            console.debug("scale diff overlay", {
              chordIndex: step.index,
              scaleDeviationPitchClasses,
              scaleDeviationNames
            });
            baseOverlays.push(scaleDiffOverlay);
          }

          const chordOverlay: FretboardOverlaySet = {
            id: `chord-${step.index}`,
            positions: step.positions,
            color: DEFAULT_CHORD_COLOR,
            label: "Chord"
          };
          baseOverlays.push(chordOverlay);

          const overlaysWithCustom: FretboardOverlaySet[] = baseOverlays.concat(
            (customOverlays ?? []).map((overlay, overlayIdx) => ({
              ...overlay,
              id: `${overlay.id}-${step.index}-${overlayIdx}`
            }))
          );

          return (
            <div key={step.index} className="voice-step">
              <div className="voice-step-header">
                <h3>Chord {step.index + 1}</h3>
                <div className="voice-metrics">
                  <span>Difficulty: {step.difficulty}</span>
                  <span>Transition: {step.transitionCost}</span>
                </div>
              </div>
              <div className="voice-meta">
                <div>
                  <strong>Pitch classes:</strong>
                  {pitchNames.length > 0 ? ` ${pitchNames.join(", ")}` : " —"}
                </div>
                {scaleOverlay && (
                  <div>
                    <strong>Scale:</strong>{" "}
                    {scaleOverlay.scaleRoot ? `${scaleOverlay.scaleRoot} ` : ""}
                    {scaleOverlay.scaleMode ?? scaleLabel}
                  </div>
                )}
              </div>
              <FretboardOverlay tuning={tuning} overlays={overlaysWithCustom} serverUrl={serverUrl} />
            </div>
          );
        })}
      </div>
    </div>
  );
}

type FretboardOverlayProps = {
  tuning: string[];
  overlays: FretboardOverlaySet[];
  serverUrl: string;
};

function FretboardOverlay({ tuning, overlays, serverUrl }: FretboardOverlayProps) {
  const containerRef = useRef<HTMLDivElement | null>(null);
  const [backendMarkers, setBackendMarkers] = useState<OverlayMarker[]>([]);
  const [fetchError, setFetchError] = useState<string | null>(null);
  const [isLoading, setLoading] = useState<boolean>(false);
  const lastRequestRef = useRef<string | null>(null);
  const [chordNames, setChordNames] = useState<Record<string, BackendChordNameEntry>>({});
  const [chordNameError, setChordNameError] = useState<string | null>(null);
  const [isChordLoading, setChordLoading] = useState<boolean>(false);
  const chordRequestRef = useRef<string | null>(null);

  const noteSetOverlays = useMemo(
    () =>
      overlays.filter(
        (overlay) =>
          (Array.isArray(overlay.notes) && overlay.notes.length > 0) ||
          (Array.isArray(overlay.pitchClassNumbers) && overlay.pitchClassNumbers.length > 0)
      ),
    [overlays]
  );

  const primaryScaleOverlay = useMemo(
    () => overlays.find((overlay) => overlay.scaleMode || overlay.scaleRoot),
    [overlays]
  );

  const scaleDescriptor = useMemo(() => {
    if (!primaryScaleOverlay) {
      return null;
    }
    const parts: string[] = [];
    if (primaryScaleOverlay.scaleRoot) {
      parts.push(primaryScaleOverlay.scaleRoot);
    }
    if (primaryScaleOverlay.scaleMode) {
      parts.push(primaryScaleOverlay.scaleMode);
    } else if (primaryScaleOverlay.label) {
      parts.push(primaryScaleOverlay.label);
    }
    const descriptor = parts.join(" ").trim();
    return descriptor.length > 0 ? descriptor : null;
  }, [primaryScaleOverlay]);

  const serializedTuning = useMemo<SerializedTuningEntry[]>(
    () => tuning.map((value) => serializeTuningValue(value)),
    [tuning]
  );

  const tuningSignature = useMemo(() => JSON.stringify(serializedTuning), [serializedTuning]);

  const noteSetPayload = useMemo(
    () =>
      noteSetOverlays.map((overlay) => {
        const base: Record<string, unknown> = {
          id: overlay.id,
          label: overlay.label ?? null
        };
        if (overlay.pitchClassNumbers && overlay.pitchClassNumbers.length > 0) {
          const uniquePitchClasses = Array.from(
            new Set(overlay.pitchClassNumbers.map((value) => normalizePitchClass(Number(value))))
          ).sort((a, b) => a - b);
          base.pitchClasses = uniquePitchClasses;
        } else {
          base.pitchClasses = overlay.notes ?? [];
        }
        if (overlay.scaleRoot) {
          base.tonic = overlay.scaleRoot;
        }
        if (overlay.scaleMode) {
          base.mode = overlay.scaleMode;
        }
        return base;
      }),
    [noteSetOverlays]
  );

  const noteSetSignature = useMemo(() => JSON.stringify(noteSetPayload), [noteSetPayload]);

  const overlaySignature = useMemo(
    () =>
      JSON.stringify(
        overlays.map((overlay) => ({
          id: overlay.id,
          color: overlay.color ?? null,
          label: overlay.label ?? null,
          maxFret: overlay.maxFret ?? null,
          scaleMode: overlay.scaleMode ?? null,
          scaleRoot: overlay.scaleRoot ?? null,
          pitchClassNumbers: overlay.pitchClassNumbers ?? null,
          useOutline: overlay.useOutline ?? false,
          outlineColor: overlay.outlineColor ?? null,
          strokeWidth: overlay.strokeWidth ?? null,
          notesLength: overlay.notes?.length ?? 0,
          positionsLength: overlay.positions?.length ?? 0
        }))
      ),
    [overlays]
  );

  useEffect(() => {
    if (!serverUrl || noteSetPayload.length === 0) {
      setChordNames({});
      setChordNameError(null);
      setChordLoading(false);
      chordRequestRef.current = null;
      return;
    }

    const controller = new AbortController();
    const endpointBase = normaliseUrl(serverUrl);
    const requestBody = {
      noteSets: noteSetPayload
    };

    const signature = JSON.stringify({ server: endpointBase, requestBody });
    if (signature === chordRequestRef.current) {
      return () => controller.abort();
    }
    chordRequestRef.current = signature;

    const fetchChordNames = async () => {
      try {
        setChordLoading(true);
        setChordNameError(null);
        const response = await fetch(`${endpointBase}/fretboard/chord-names`, {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(requestBody),
          signal: controller.signal
        });

        if (!response.ok) {
          const detail = await response.json().catch(() => ({}));
          const errMessage =
            (detail && typeof detail.error === "string" && detail.error) ||
            `${response.status} ${response.statusText}`;
          throw new Error(errMessage);
        }

        const payload = (await response.json()) as FretboardChordNamesResponsePayload;
        const map: Record<string, BackendChordNameEntry> = {};
        for (const entry of payload.chordNames ?? []) {
          if (entry && typeof entry.id === "string") {
            map[entry.id] = entry;
          }
        }
        setChordNames(map);
        setChordLoading(false);
      } catch (err) {
        if (err instanceof DOMException && err.name === "AbortError") {
          setChordLoading(false);
          chordRequestRef.current = null;
          return;
        }
        setChordNames({});
        setChordLoading(false);
        chordRequestRef.current = null;
        setChordNameError(err instanceof Error ? err.message : "Unable to resolve chord names.");
      }
    };

    fetchChordNames();
    return () => {
      controller.abort();
      setChordLoading(false);
      chordRequestRef.current = null;
    };
  }, [serverUrl, noteSetSignature, noteSetPayload]);

  const chordOverlayNames = useMemo(
    () =>
      noteSetOverlays
        .filter((overlay) => overlay.id.startsWith("chord-"))
        .map((overlay) => chordNames[overlay.id]?.name)
        .filter((value): value is string => typeof value === "string" && value.trim().length > 0),
    [noteSetOverlays, chordNames]
  );

  const directMarkers = useMemo(() => {
    return overlays.flatMap((overlay) => {
      if (overlay.positions && overlay.positions.length > 0) {
        return buildMarkersForPositions(tuning, overlay.positions, overlay.id, overlay.color);
      }
      return [];
    });
  }, [overlays, tuning]);

  const maxFret = useMemo(() => {
    const positionFrets = overlays.flatMap((overlay) =>
      overlay.positions ? overlay.positions.map((position) => position.fret ?? 0) : []
    );
    const explicitLimits = overlays
      .map((overlay) => overlay.maxFret)
      .filter((value): value is number => typeof value === "number");
    const candidates = [DEFAULT_FRET_COUNT, ...positionFrets, ...explicitLimits];
    return Math.max(...candidates);
  }, [overlays]);

  useEffect(() => {
    if (!serverUrl || noteSetPayload.length === 0) {
      setBackendMarkers([]);
      setFetchError(null);
      setLoading(false);
      lastRequestRef.current = null;
      return;
    }

    const controller = new AbortController();
    const endpointBase = normaliseUrl(serverUrl);
    const requestBody = {
      tuning: serializedTuning,
      maxFrets: maxFret,
      noteSets: noteSetPayload
    };

    const signature = JSON.stringify({ server: endpointBase, requestBody });
    if (signature === lastRequestRef.current) {
      return () => controller.abort();
    }
    lastRequestRef.current = signature;

    const fetchOccurrences = async () => {
      try {
        setLoading(true);
        setFetchError(null);
        const response = await fetch(`${endpointBase}/fretboard/occurrences`, {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(requestBody),
          signal: controller.signal
        });

        if (!response.ok) {
          const detail = await response.json().catch(() => ({}));
          const errMessage =
            (detail && typeof detail.error === "string" && detail.error) ||
            `${response.status} ${response.statusText}`;
          throw new Error(errMessage);
        }

        const payload = (await response.json()) as FretboardOccurrencesResponsePayload;
        const overlayMap = new Map(overlays.map((overlay) => [overlay.id, overlay]));
        const isScaleOverlay = (overlayInfo: FretboardOverlaySet | undefined) =>
          Boolean(overlayInfo?.scaleMode || overlayInfo?.scaleRoot);

        const colorMapCache = new Map<string, Map<number, string>>();
        const getViridisColor = (index: number) => VIRIDIS_COLORS[index % VIRIDIS_COLORS.length];
        const createColorMap = (values: number[]) => {
          const unique = Array.from(new Set(values.map((value) => ((value % 12) + 12) % 12)));
          const map = new Map<number, string>();
          unique.forEach((pitchClass, idx) => {
            map.set(pitchClass, getViridisColor(idx));
          });
          return map;
        };

        const markersFromBackend = payload.noteSets.flatMap((noteSet) => {
          const overlayInfo = overlayMap.get(noteSet.id);
          const groupId = overlayInfo?.id ?? noteSet.id;
          const baseColor = overlayInfo?.color;
          const useViridis = isScaleOverlay(overlayInfo);
          const useOutline = overlayInfo?.useOutline ?? false;
          const outlineColor = overlayInfo?.outlineColor ?? baseColor ?? DEFAULT_SCALE_DIFF_OUTLINE_COLOR;
          const strokeWidth = overlayInfo?.strokeWidth ?? DEFAULT_SCALE_DIFF_STROKE_WIDTH;
          const chordInfo = chordNames[noteSet.id];
          const chordNameCandidate =
            chordInfo?.name && chordInfo.name.trim().length > 0
              ? chordInfo.name
              : chordInfo?.aliases?.find((alias) => alias && alias.trim().length > 0);
          const overlayLabel =
            overlayInfo?.label ??
            (useOutline ? undefined : chordNameCandidate) ??
            noteSet.label ??
            undefined;
          const colorLookup = (() => {
            if (!useViridis) {
              return undefined;
            }
            const cached = colorMapCache.get(noteSet.id);
            if (cached) {
              return cached;
            }
            const created = createColorMap(noteSet.pitchClasses ?? []);
            colorMapCache.set(noteSet.id, created);
            return created;
          })();

          return noteSet.occurrences.map((occurrence) => {
            const stringNumber = mapBackendStringToFretboard(occurrence.string, tuning.length);
            const pitchClass = ((occurrence.pitchClass % 12) + 12) % 12;
            const assignedColor = useOutline
              ? "transparent"
              : colorLookup?.get(pitchClass) ?? baseColor;
            const markerGroup = colorLookup && !useOutline ? `${groupId}-${pitchClass}` : groupId;
            const markerLabel =
              useViridis && (occurrence.noteName || occurrence.pitchClassName)
                ? occurrence.noteName ?? occurrence.pitchClassName
                : overlayLabel ?? occurrence.noteName ?? occurrence.pitchClassName;
            return {
              string: stringNumber,
              fret: occurrence.fret,
              label: markerLabel,
              group: markerGroup,
              color: assignedColor,
              outlineColor: useOutline ? outlineColor : undefined,
              strokeWidth: useOutline ? strokeWidth : undefined
            } satisfies OverlayMarker;
          });
        });

        setBackendMarkers(markersFromBackend);
        setLoading(false);
      } catch (err) {
        if (err instanceof DOMException && err.name === "AbortError") {
          setLoading(false);
          lastRequestRef.current = null;
          return;
        }
        setBackendMarkers([]);
        setLoading(false);
        lastRequestRef.current = null;
        setFetchError(err instanceof Error ? err.message : "Unable to load fretboard overlays.");
      }
    };

    fetchOccurrences();
    return () => {
      controller.abort();
      setLoading(false);
      lastRequestRef.current = null;
    };
  }, [serverUrl, maxFret, overlaySignature, noteSetSignature, tuningSignature, chordNames]);

  const markers = useMemo(
    () => [...backendMarkers, ...directMarkers],
    [backendMarkers, directMarkers]
  );

  useEffect(() => {
    let cancelled = false;
    let fretboardInstance: any = null;

    async function renderFretboard() {
      const module = await import("@moonwave99/fretboard.js");
      if (cancelled || !containerRef.current) {
        return;
      }

      const FretboardClass: any =
        (module && "default" in module ? (module as { default: unknown }).default : undefined) ??
        (module as { Fretboard?: unknown }).Fretboard ??
        module;

      if (typeof FretboardClass !== "function" || !containerRef.current) {
        return;
      }

      containerRef.current.innerHTML = "";

      fretboardInstance = new FretboardClass({
        el: containerRef.current,
        stringCount: tuning.length,
        tuning,
        fretCount: maxFret,
        width: 420,
        height: 200,
        dotSize: 18,
        dotTextSize: 11,
        dotStrokeColor: "#1f2937",
        dotFill: "#ffffff"
      });

      if (markers.length === 0) {
        fretboardInstance.render([]);
        return;
      }

      renderFretboardDots(fretboardInstance, markers);
    }

    renderFretboard().catch((err) => {
      if (!cancelled) {
        console.error("Failed to render fretboard diagram", err);
      }
    });

    return () => {
      cancelled = true;
      if (containerRef.current) {
        containerRef.current.innerHTML = "";
      }
      if (typeof fretboardInstance?.clear === "function") {
        fretboardInstance.clear();
      }
      fretboardInstance = null;
    };
  }, [markers, maxFret, tuning]);

  const showMeta =
    Boolean(scaleDescriptor) ||
    chordOverlayNames.length > 0 ||
    isChordLoading ||
    chordNameError;

  return (
    <div className="fretboard-wrapper">
      {showMeta && (
        <div className="fretboard-meta">
          {scaleDescriptor && (
            <div>
              <strong>Scale:</strong> {scaleDescriptor}
            </div>
          )}
          {chordOverlayNames.length > 0 && (
            <div>
              <strong>Chord:</strong> {chordOverlayNames.join(", ")}
            </div>
          )}
          {isChordLoading && chordOverlayNames.length === 0 && (
            <div className="status">Identifying chord…</div>
          )}
          {chordNameError && <div className="status error">{chordNameError}</div>}
        </div>
      )}
      {fetchError && <div className="status error">{fetchError}</div>}
      {!fetchError && isLoading && <div className="status">Loading overlays…</div>}
      <div className="fretboard-container" ref={containerRef} />
    </div>
  );
}

export default App;

function mapBackendStringToFretboard(stringIndex: number, stringCount: number): number {
  const candidate = stringCount - stringIndex;
  return Math.max(1, Math.min(stringCount, candidate));
}

function renderFretboardDots(instance: any, markers: OverlayMarker[]) {
  instance.setDots(markers).render();
  instance.style({
    filter: () => true,
    text: (dot: OverlayMarker) => (dot.label ? String(dot.label) : "")
  });

  const groupStyles = new Map<string, { fill?: string; stroke?: string; strokeWidth?: number }>();
  markers.forEach((marker) => {
    if (!marker.group) {
      return;
    }
    const existing = groupStyles.get(marker.group) ?? {};
    if (marker.color !== undefined && existing.fill === undefined) {
      existing.fill = marker.color;
    }
    if (marker.outlineColor !== undefined) {
      existing.stroke = marker.outlineColor;
    }
    if (typeof marker.strokeWidth === "number") {
      existing.strokeWidth = marker.strokeWidth;
    }
    groupStyles.set(marker.group, existing);
  });

  groupStyles.forEach((style, group) => {
    const params: Record<string, unknown> = { filter: { group } };
    if (style.fill !== undefined) {
      params.fill = style.fill;
    }
    if (style.stroke !== undefined) {
      params.stroke = style.stroke;
      if (style.strokeWidth !== undefined) {
        params["stroke-width"] = style.strokeWidth;
      }
    }
    instance.style(params);
  });
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
}
