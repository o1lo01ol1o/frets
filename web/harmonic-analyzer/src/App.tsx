import { FormEvent, useEffect, useMemo, useRef, useState } from "react";

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

const defaultServer =
  typeof window === "undefined"
    ? "http://localhost:8080"
    : `${window.location.protocol}//${window.location.hostname}:8080`;

const DEFAULT_TUNING_LIST = ["E", "A", "D", "G", "B", "E"] as const;
const DEFAULT_TUNING = JSON.stringify(DEFAULT_TUNING_LIST);
const DEFAULT_MAX_CANDIDATES = "12";

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

function normaliseTuningEntries(entries: unknown[]): string[] {
  return entries.map((entry, index) => {
    if (typeof entry === "string" && entry.trim().length > 0) {
      return entry.trim();
    }
    if (typeof entry === "number") {
      return entry.toString(10);
    }
    throw new Error(`Tuning entry at index ${index} must be a string or number.`);
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
  const voiceControllerRef = useRef<AbortController | null>(null);

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
      const res = await fetch(voiceUrl, {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify({
          tuning: tuningEntries,
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
      console.log('voice-leading payload', data);
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
                setVoiceTuningDisplay([...DEFAULT_TUNING_LIST]);
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
        </form>

        {voiceResponse && (
      <VoiceLeadingResult
          response={voiceResponse}
          tuning={voiceTuningDisplay}
        />
        )}
      </section>
    </div>
  );
}

type VoiceLeadingResultProps = {
  response: VoiceLeadingResponse;
  tuning: string[];
};

function VoiceLeadingResult({ response, tuning }: VoiceLeadingResultProps) {
  const steps = Array.isArray(response?.steps) ? response.steps : [];

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
        {steps.map((step) => {
          const pitchNames = Array.isArray(step.pitchClassNames)
            ? step.pitchClassNames
            : [];
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
              </div>
              <FretboardDiagram step={step} tuning={tuning} />
            </div>
          );
        })}
      </div>
    </div>
  );
}

type FretboardDiagramProps = {
  step: VoiceLeadingStep;
  tuning: string[];
};

function FretboardDiagram({ step, tuning }: FretboardDiagramProps) {
  const containerRef = useRef<HTMLDivElement | null>(null);

  useEffect(() => {
    let cancelled = false;
    let fretboardInstance: { render(markers: unknown[]): void } | null = null;

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
        fretCount: 12,
        width: 420,
        height: 160
      });

      const markers = Array.isArray(step.positions)
        ? step.positions.map((position) => ({
            string: tuning.length - position.string,
            fret: position.fret,
            label: position.finger ?? position.pitchClass
          }))
        : [];

      console.log('fretboard markers', markers);

      fretboardInstance.setDots(markers).render();
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
      fretboardInstance = null;
    };
  }, [step, tuning]);

  return <div className="fretboard-container" ref={containerRef} />;
}

export default App;
