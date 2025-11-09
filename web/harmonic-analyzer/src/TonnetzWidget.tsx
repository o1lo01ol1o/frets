import * as d3 from "d3";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import JZZ, { type MidiOpenResult, type MidiOut } from "jzz";
import TinyDefault, { Tiny as TinyNamed } from "jzz-synth-tiny";
import PianoRollView from "./PianoRollView";

const TinyInitializer = typeof TinyNamed === "function" ? TinyNamed : TinyDefault;

if (typeof window !== "undefined") {
  try {
    TinyInitializer(JZZ);
  } catch (err) {
    console.warn("Failed to initialize Tiny synth", err);
  }
}

type TonnetzIntervalOption = {
  id: string;
  label: string;
  steps: number[];
};

type TonnetzStructureOptions = {
  id: string;
  label: string;
  intervals: TonnetzIntervalOption[];
};

type TonnetzOptionsResponse = {
  structures: TonnetzStructureOptions[];
};

type TonnetzTilingRequest = {
  structure: string;
  interval: number[];
  degree: string;
  baseMidi?: number;
};

type TonnetzVertexResponse = {
  coordinate: number[];
  midi: number;
  pitchClass: number;
  pitchClassName: string;
  noteName: string;
  octave: number;
};

type TonnetzPolygonResponse = {
  faceVertices: [number, number][];
  vertexCoordinates: number[][];
  pitchClasses: number[];
  midiNotes: number[];
  chord?: {
    name: string;
    aliases: string[];
  } | null;
};

type TonnetzTilingResponse = {
  structure: string;
  interval: number[];
  degree: string;
  baseMidi: number;
  vertices: TonnetzVertexResponse[];
  polygons: TonnetzPolygonResponse[];
};

type ChordNameInfo = {
  name: string | null;
  aliases: string[];
};

const PITCH_CLASS_NAMES: readonly string[] = [
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

const normalisePitchClass = (value: number): number => ((value % 12) + 12) % 12;

const pitchClassNameFromNumber = (value: number): string => PITCH_CLASS_NAMES[normalisePitchClass(value)];

const midiToNoteName = (midi: number): string => {
  const pc = normalisePitchClass(midi);
  const octave = Math.floor(midi / 12) - 1;
  return `${pitchClassNameFromNumber(pc)}${octave}`;
};

const clampMidi = (note: number): number => Math.max(0, Math.min(127, Math.round(note)));

export type RecordedChord = {
  id: string;
  midiNotes: number[];
  pitchClassNames: string[];
  noteNames: string[];
  chordName?: string | null;
  aliases: string[];
  transpose: number;
};

type RecordingEvent =
  | { type: "start"; at: number }
  | { type: "stop"; at: number }
  | { type: "chord-on"; at: number; id: string; notes: number[] }
  | { type: "chord-off"; at: number; id: string };

type PlaybackQuantizationOption = "quarter" | "eighth" | "sixteenth" | "triplet" | "sextuplet";

type PlaybackOptionsPayload = {
  quantization?: PlaybackQuantizationOption;
  loopMeter?: {
    beats: number;
    beatUnit: number;
  };
  tempoBpm?: number;
};

type RenderedChordEvent = {
  id: string;
  onsetSeconds: number;
  durationSeconds: number;
  midiNotes: number[];
};

export type RenderedPlaybackResponse = {
  midiBase64: string;
  totalSeconds: number;
  events: RenderedChordEvent[];
};

const DEGREE_OPTIONS = ["I", "II", "III", "IV", "V", "VI", "VII"] as const;
const SVG_SIZE = 720;
const SVG_PADDING = 48;
const MIDI_CHANNEL = 0;

const QUANTIZATION_OPTIONS: Array<{ value: PlaybackQuantizationOption | "none"; label: string }> = [
  { value: "none", label: "No quantization" },
  { value: "quarter", label: "1/4 notes" },
  { value: "eighth", label: "1/8 notes" },
  { value: "sixteenth", label: "1/16 notes" },
  { value: "triplet", label: "Triplets" },
  { value: "sextuplet", label: "Sextuplets" }
] as const;

const DEFAULT_TEMPO_BPM = 120;

const resolveDefaultStructure = (
  structures: TonnetzStructureOptions[]
): TonnetzStructureOptions | null => {
  if (structures.length === 0) {
    return null;
  }
  const byLabel = structures.find((structure) => /tetrad/i.test(structure.label));
  if (byLabel) {
    return byLabel;
  }
  const byId = structures.find((structure) => /tetrad/i.test(structure.id));
  if (byId) {
    return byId;
  }
  return structures[0] ?? null;
};

const coordinateKey = (coord: number[]): string => coord.join(",");

const equalMidiSets = (a: number[] | null, b: number[]): boolean => {
  if (!a || a.length !== b.length) {
    return false;
  }
  return a.every((value, index) => value === b[index]);
};


type TonnetzWidgetProps = {
  serverUrl: string;
  onRecordedStack?: (payload: { chords: RecordedChord[]; progression: number[][] }) => void;
  onRecordingStateChange?: (isRecording: boolean) => void;
  onLoopEvent?: (payload: { index: number; active: boolean }) => void;
};

type VertexRenderDatum = {
  vertex: TonnetzVertexResponse;
  position: [number, number];
};

type SynthStatus = "idle" | "initializing" | "ready" | "error";

const AUTO_MIXER_SELECTION = "__auto__";
const NO_MIXER_SELECTION = "__none__";

const parseMeterValue = (beatsInput: string, unitInput: string): { beats: number; beatUnit: number } | undefined => {
  const beats = Number.parseInt(beatsInput, 10);
  const beatUnit = Number.parseInt(unitInput, 10);
  if (Number.isNaN(beats) || Number.isNaN(beatUnit) || beats <= 0 || beatUnit <= 0) {
    return undefined;
  }
  return { beats, beatUnit };
};

export function TonnetzWidget({
  serverUrl,
  onRecordedStack,
  onRecordingStateChange,
  onLoopEvent
}: TonnetzWidgetProps) {
  const containerRef = useRef<HTMLDivElement | null>(null);
  const svgRef = useRef<SVGSVGElement | null>(null);
  const optionsAbortRef = useRef<AbortController | null>(null);
  const tilingAbortRef = useRef<AbortController | null>(null);
  const midiOutRef = useRef<MidiOut | null>(null);
  const midiMixerOutRef = useRef<MidiOut | null>(null);
  const activeMidiRef = useRef<number[] | null>(null);
  const midiOpenPromiseRef = useRef<Promise<MidiOut | null> | null>(null);
  const midiMixerOpenPromiseRef = useRef<Promise<MidiOut | null> | null>(null);
  const selectedMixerOutputRef = useRef<string>(AUTO_MIXER_SELECTION);
  const chordNameCacheRef = useRef<Map<string, ChordNameInfo>>(new Map());
  const recordedChordsRef = useRef<RecordedChord[]>([]);
  const recordingStartRef = useRef<number | null>(null);
  const recordingEventsRef = useRef<RecordingEvent[]>([]);
  const activeChordRecordingRef = useRef<{ id: string; startedAt: number; notes: number[] } | null>(null);
  const lastRecordingRef = useRef<RecordingEvent[] | null>(null);
  const loopTimeoutsRef = useRef<number[]>([]);
  const isLoopingRef = useRef(false);
  const playbackAbortRef = useRef<AbortController | null>(null);
  const loopActiveNotesRef = useRef<Set<number>>(new Set());
  const previousPlaybackOptionsRef = useRef<{
    quantization: PlaybackQuantizationOption | "none";
    meterBeats: string;
    meterUnit: string;
    tempo: number;
  }>({
    quantization: "none",
    meterBeats: "4",
    meterUnit: "4",
    tempo: DEFAULT_TEMPO_BPM
  });

  const [options, setOptions] = useState<TonnetzStructureOptions[]>([]);
  const [structureId, setStructureId] = useState<string | null>(null);
  const [intervalId, setIntervalId] = useState<string | null>(null);
  const [degree, setDegree] = useState<string>("I");
  const [transposeSemitones, setTransposeSemitones] = useState(0);
  const [activeControlTab, setActiveControlTab] = useState<"structure" | "transpose" | "mixer" | "render">("structure");
  const [tiling, setTiling] = useState<TonnetzTilingResponse | null>(null);
  const [loadingOptions, setLoadingOptions] = useState(false);
  const [loadingTiling, setLoadingTiling] = useState(false);
  const [optionsError, setOptionsError] = useState<string | null>(null);
  const [tilingError, setTilingError] = useState<string | null>(null);
  const [synthStatus, setSynthStatus] = useState<SynthStatus>("idle");
  const [synthError, setSynthError] = useState<string | null>(null);
  const [midiMixerStatus, setMidiMixerStatus] = useState<SynthStatus>("idle");
  const [midiMixerError, setMidiMixerError] = useState<string | null>(null);
  const [availableMixerOutputs, setAvailableMixerOutputs] = useState<string[]>([]);
  const [selectedMixerOutput, setSelectedMixerOutput] = useState<string>(AUTO_MIXER_SELECTION);
  const [isRecording, setIsRecording] = useState(false);
  const [recordedChords, setRecordedChords] = useState<RecordedChord[]>([]);
  const [quantizationOption, setQuantizationOption] =
    useState<PlaybackQuantizationOption | "none">("none");
  const [meterBeatsInput, setMeterBeatsInput] = useState<string>("4");
  const [meterUnitInput, setMeterUnitInput] = useState<string>("4");
  const [tempoBpm, setTempoBpm] = useState<number>(DEFAULT_TEMPO_BPM);
  const [renderedPlayback, setRenderedPlayback] = useState<RenderedPlaybackResponse | null>(null);
  const [playbackError, setPlaybackError] = useState<string | null>(null);
  const [isRenderingPlayback, setIsRenderingPlayback] = useState(false);
  const [isLooping, setIsLooping] = useState(false);
  const [currentChordDescription, setCurrentChordDescription] = useState<{
    name: string | null;
    aliases: string[];
    pitchClassNames: string[];
    noteNames: string[];
    transpose: number;
  } | null>(null);
  const [activeView, setActiveView] = useState<"tonnetz" | "piano-roll">("tonnetz");
  const [loopProgress, setLoopProgress] = useState(0);

  const hasRecordedSession =
    lastRecordingRef.current != null && lastRecordingRef.current.length > 0;

  useEffect(() => {
    selectedMixerOutputRef.current = selectedMixerOutput;
  }, [selectedMixerOutput]);

  const loopIterationStartRef = useRef<number | null>(null);
  const loopAnimationFrameRef = useRef<number | null>(null);
  const renderedPlaybackRef = useRef<RenderedPlaybackResponse | null>(null);
  const activeLoopEventRef = useRef<number | null>(null);

  useEffect(() => {
    renderedPlaybackRef.current = renderedPlayback ?? null;
    if (!renderedPlayback) {
      setLoopProgress(0);
    }
  }, [renderedPlayback]);

  const pianoRollEvents = useMemo(
    () => (renderedPlayback?.events ?? []).map((event) => ({
      ...event,
      midiNotes: event.midiNotes.map(clampMidi)
    })),
    [renderedPlayback]
  );
  const totalPlaybackSeconds = renderedPlayback?.totalSeconds ?? 0;
  const hasRenderedPlayback = pianoRollEvents.length > 0;

  useEffect(() => {
    if (!hasRenderedPlayback && activeView !== "tonnetz") {
      setActiveView("tonnetz");
    }
  }, [hasRenderedPlayback, activeView]);

  useEffect(() => {
    recordedChordsRef.current = recordedChords;
  }, [recordedChords]);

  useEffect(() => {
    onRecordingStateChange?.(isRecording);
  }, [isRecording, onRecordingStateChange]);

  const emitRecordedStack = useCallback(
    (chords: RecordedChord[]) => {
      if (!onRecordedStack) {
        return;
      }
      const progression = chords.map((entry) =>
        Array.from(
          new Set(entry.midiNotes.map((value) => normalisePitchClass(value)))
        ).sort((a, b) => a - b)
      );
      onRecordedStack({ chords, progression });
    },
    [onRecordedStack]
  );

  useEffect(() => {
    if (!onRecordedStack) {
      return;
    }
    if (isRecording) {
      return;
    }
    if (recordedChords.length === 0) {
      onRecordedStack({ chords: [], progression: [] });
      return;
    }
    emitRecordedStack(recordedChords);
  }, [recordedChords, isRecording, onRecordedStack, emitRecordedStack]);

  const stopChord = useCallback(() => {
    const outputs = [midiOutRef.current, midiMixerOutRef.current].filter(
      (candidate): candidate is MidiOut => candidate != null
    );
    const active = activeMidiRef.current;
    if (outputs.length === 0 || !active || active.length === 0) {
      activeMidiRef.current = null;
      return;
    }
    outputs.forEach((out) => {
      active.forEach((note) => {
        try {
          out.noteOff(MIDI_CHANNEL, clampMidi(note));
        } catch {
          /* ignore */
        }
      });
    });
    activeMidiRef.current = null;
  }, []);

  const ensureMidiMixerOut = useCallback(async (): Promise<MidiOut | null> => {
    if (selectedMixerOutputRef.current === NO_MIXER_SELECTION) {
      setMidiMixerStatus("idle");
      setMidiMixerError(null);
      return null;
    }
    if (midiMixerOutRef.current) {
      return midiMixerOutRef.current;
    }
    if (midiMixerOpenPromiseRef.current) {
      return midiMixerOpenPromiseRef.current;
    }
    setMidiMixerStatus("initializing");
    setMidiMixerError(null);
    const engine = JZZ();
    const discoveredOutputs: string[] = [];
    const captureOutputs = () => {
      const info =
        typeof (engine as unknown as { info?: () => { outputs?: Array<{ name?: string }> } }).info === "function"
          ? (engine as unknown as { info: () => { outputs?: Array<{ name?: string }> } }).info()
          : { outputs: [] as Array<{ name?: string }> };
      if (info && Array.isArray(info.outputs)) {
        discoveredOutputs.splice(
          0,
          discoveredOutputs.length,
          ...info.outputs
            .map((port) =>
              typeof port?.name === "string" ? port.name : port?.name ? String(port.name) : ""
            )
            .filter((name): name is string => name.length > 0 && name !== "Web Audio")
        );
      }
    };

    captureOutputs();
    setAvailableMixerOutputs([...discoveredOutputs]);

    const promise = new Promise<MidiOut | null>((resolve) => {
      const selection = selectedMixerOutputRef.current;
      const attempts: Array<
        | string
        | RegExp
        | ((ports: Array<{ name: string }>) => Array<{ name: string }>)
        | undefined
      > = [];
      if (selection && selection !== AUTO_MIXER_SELECTION && selection !== NO_MIXER_SELECTION) {
        attempts.push(selection);
      }
      attempts.push(/midi mixer/i, /mixer/i);
      attempts.push((ports: Array<{ name: string }>) =>
        ports.filter((port) => typeof port.name === "string" && port.name !== "Web Audio")
      );
      attempts.push(undefined);

      let idx = 0;
      let lastError: unknown = null;

      const tryNext = () => {
        if (idx >= attempts.length) {
          midiMixerOpenPromiseRef.current = null;
          captureOutputs();
          setAvailableMixerOutputs([...discoveredOutputs]);
          const message =
            typeof lastError === "string"
              ? lastError
              : lastError instanceof Error
                ? lastError.message
                : "Unable to open MIDI output";
          setMidiMixerStatus("error");
          setMidiMixerError(
            discoveredOutputs.length > 0
              ? `${message}. Available outputs: ${discoveredOutputs.join(", ")}`
              : `${message}. No MIDI outputs reported.`
          );
          resolve(null);
          return;
        }

        const arg = attempts[idx++];
        const opener =
          typeof arg === "undefined"
            ? engine.openMidiOut()
            : ((engine.openMidiOut as unknown as (this: unknown, input: unknown) => MidiOpenResult).call(
                engine,
                arg
              ));
        opener
          .or((error) => {
            lastError = error;
            tryNext();
          })
          .and(function (this: MidiOut) {
            midiMixerOutRef.current = this;
            midiMixerOpenPromiseRef.current = null;
            captureOutputs();
            setAvailableMixerOutputs([...discoveredOutputs]);
            setMidiMixerStatus("ready");
            setMidiMixerError(null);

            const portInfo =
              typeof (this as unknown as { info?: () => { name?: string } }).info === "function"
                ? (this as unknown as { info: () => { name?: string } }).info()
                : null;
            const portName = typeof portInfo?.name === "string" ? portInfo.name : undefined;
            if (
              portName &&
              (selectedMixerOutputRef.current === AUTO_MIXER_SELECTION ||
                selectedMixerOutputRef.current === "" ||
                selectedMixerOutputRef.current == null)
            ) {
              setSelectedMixerOutput(portName);
            }
            resolve(this);
          });
      };

      tryNext();
    });

    midiMixerOpenPromiseRef.current = promise;
    return promise;
  }, []);

  const ensureMidiOut = useCallback(async (): Promise<MidiOut | null> => {
    if (midiOutRef.current) {
      return midiOutRef.current;
    }
    if (midiOpenPromiseRef.current) {
      return midiOpenPromiseRef.current;
    }
    setSynthStatus("initializing");
    setSynthError(null);
    try {
      JZZ.synth.Tiny.register("Web Audio");
    } catch {
      /* ignore duplicate registration */
    }
    const engine = JZZ();
    const promise = new Promise<MidiOut | null>((resolve) => {
      const attempts: Array<string | undefined> = ["Web Audio", undefined];
      let idx = 0;
      let lastError: unknown = null;
      const tryNext = () => {
        if (idx >= attempts.length) {
          midiOpenPromiseRef.current = null;
          setSynthStatus("error");
          const message =
            typeof lastError === "string"
              ? lastError
              : lastError instanceof Error
                ? lastError.message
                : "Unable to open MIDI output";
          setSynthError(message);
          resolve(null);
          return;
        }
        const name = attempts[idx++];
        const opener = name ? engine.openMidiOut(name) : engine.openMidiOut();
        opener
          .or((error) => {
            lastError = error;
            tryNext();
          })
          .and(function (this: MidiOut) {
            midiOutRef.current = this;
            midiOpenPromiseRef.current = null;
            setSynthStatus("ready");
            resolve(this);
          });
      };
      tryNext();
    });
    midiOpenPromiseRef.current = promise;
    return promise;
  }, []);

  useEffect(() => {
    if (selectedMixerOutput === NO_MIXER_SELECTION) {
      if (midiMixerOutRef.current) {
        try {
          midiMixerOutRef.current.close();
        } catch {
          /* ignore */
        }
        midiMixerOutRef.current = null;
      }
      midiMixerOpenPromiseRef.current = null;
      setMidiMixerStatus("idle");
      setMidiMixerError(null);
      return;
    }
    const currentInfo =
      midiMixerOutRef.current && typeof (midiMixerOutRef.current as unknown as { info?: () => { name?: string } }).info === "function"
        ? (midiMixerOutRef.current as unknown as { info: () => { name?: string } }).info()
        : null;
    const currentName = typeof currentInfo?.name === "string" ? currentInfo.name : undefined;
    if (selectedMixerOutput === AUTO_MIXER_SELECTION && midiMixerOutRef.current) {
      setMidiMixerStatus("ready");
      setMidiMixerError(null);
      return;
    }
    if (
      selectedMixerOutput &&
      selectedMixerOutput !== AUTO_MIXER_SELECTION &&
      selectedMixerOutput === currentName
    ) {
      setMidiMixerStatus("ready");
      setMidiMixerError(null);
      return;
    }
    if (midiMixerOutRef.current) {
      try {
        midiMixerOutRef.current.close();
      } catch {
        /* ignore */
      }
      midiMixerOutRef.current = null;
    }
    midiMixerOpenPromiseRef.current = null;
    if (selectedMixerOutput === AUTO_MIXER_SELECTION || (selectedMixerOutput && selectedMixerOutput.length > 0)) {
      void ensureMidiMixerOut();
    }
  }, [selectedMixerOutput, ensureMidiMixerOut]);

  useEffect(() => {
    if (
      selectedMixerOutput &&
      selectedMixerOutput !== AUTO_MIXER_SELECTION &&
      selectedMixerOutput !== NO_MIXER_SELECTION &&
      selectedMixerOutput.length > 0 &&
      availableMixerOutputs.length > 0 &&
      !availableMixerOutputs.includes(selectedMixerOutput)
    ) {
      setSelectedMixerOutput(AUTO_MIXER_SELECTION);
    }
  }, [availableMixerOutputs, selectedMixerOutput]);

  const playChord = useCallback(
    (midiNotes: number[]) => {
      if (midiNotes.length === 0) {
        return;
      }
      const outputs = [midiOutRef.current, midiMixerOutRef.current].filter(
        (candidate): candidate is MidiOut => candidate != null
      );
      if (outputs.length === 0) {
        return;
      }
      if (equalMidiSets(activeMidiRef.current, midiNotes)) {
        return;
      }
      stopChord();
      outputs.forEach((out) => {
        midiNotes.forEach((note) => {
          try {
            out.noteOn(MIDI_CHANNEL, clampMidi(note), 110);
          } catch {
            /* ignore */
          }
        });
      });
      activeMidiRef.current = midiNotes.map(clampMidi);
    },
    [stopChord]
  );

  const clearLoopTimeouts = useCallback(() => {
    loopTimeoutsRef.current.forEach((identifier) => {
      window.clearTimeout(identifier);
    });
    loopTimeoutsRef.current = [];
  }, []);

  const updateLoopProgress = useCallback(() => {
    const playback = renderedPlaybackRef.current;
    if (!playback || !isLoopingRef.current) {
      if (loopAnimationFrameRef.current != null) {
        window.cancelAnimationFrame(loopAnimationFrameRef.current);
        loopAnimationFrameRef.current = null;
      }
      setLoopProgress(0);
      return;
    }
    const total = playback.totalSeconds > 0 ? playback.totalSeconds : 1;
    const iterationStart = loopIterationStartRef.current;
    if (iterationStart == null) {
      loopIterationStartRef.current = performance.now();
      setLoopProgress(0);
    } else {
      const elapsedSeconds = (performance.now() - iterationStart) / 1000;
      const progressValue = Math.min(1, elapsedSeconds / total);
      setLoopProgress(progressValue);
    }
    loopAnimationFrameRef.current = window.requestAnimationFrame(updateLoopProgress);
  }, []);

  const scheduleLoopPlayback = useCallback(
    async (playback: RenderedPlaybackResponse) => {
      const [synthOut, mixerOut] = await Promise.all([ensureMidiOut(), ensureMidiMixerOut()]);
      if (!isLoopingRef.current) {
        return;
      }
      const outputs = [synthOut, mixerOut].filter(
        (candidate): candidate is MidiOut => candidate != null
      );
      if (outputs.length === 0) {
        setPlaybackError("No MIDI outputs available for loop playback.");
        isLoopingRef.current = false;
        setIsLooping(false);
        return;
      }
      clearLoopTimeouts();
      loopIterationStartRef.current = performance.now();
      setLoopProgress(0);
      if (loopAnimationFrameRef.current == null) {
        loopAnimationFrameRef.current = window.requestAnimationFrame(updateLoopProgress);
      }
      const queueTimeout = (delayMs: number, action: () => void) => {
        const id = window.setTimeout(action, Math.max(0, delayMs));
        loopTimeoutsRef.current.push(id);
      };
      playback.events.forEach((event, eventIndex) => {
        const onsetMs = event.onsetSeconds * 1000;
        const durationMs = event.durationSeconds * 1000;
        const notes = event.midiNotes.map(clampMidi);
        queueTimeout(onsetMs, () => {
          if (!isLoopingRef.current) {
            return;
          }
          activeLoopEventRef.current = eventIndex;
          onLoopEvent?.({ index: eventIndex, active: true });
          outputs.forEach((out) => {
            notes.forEach((note) => {
              try {
                out.noteOn(MIDI_CHANNEL, note, 110);
                loopActiveNotesRef.current.add(note);
              } catch {
                /* ignore */
              }
            });
          });
        });
        queueTimeout(onsetMs + durationMs, () => {
          outputs.forEach((out) => {
            notes.forEach((note) => {
              try {
                out.noteOff(MIDI_CHANNEL, note);
              } catch {
                /* ignore */
              }
            });
          });
          notes.forEach((note) => {
            loopActiveNotesRef.current.delete(note);
          });
          if (activeLoopEventRef.current === eventIndex) {
            onLoopEvent?.({ index: eventIndex, active: false });
          }
        });
      });
      queueTimeout(Math.max(0, playback.totalSeconds * 1000), () => {
        if (!isLoopingRef.current) {
          return;
        }
        void scheduleLoopPlayback(playback);
      });
    },
    [
      ensureMidiMixerOut,
      ensureMidiOut,
      clearLoopTimeouts,
      setPlaybackError,
      setIsLooping,
      updateLoopProgress,
      setLoopProgress
    ]
  );

  const stopLoopPlayback = useCallback(() => {
    isLoopingRef.current = false;
    clearLoopTimeouts();
    setIsLooping(false);
    const outputs = [midiOutRef.current, midiMixerOutRef.current].filter(
      (candidate): candidate is MidiOut => candidate != null
    );
    if (outputs.length > 0 && loopActiveNotesRef.current.size > 0) {
      outputs.forEach((out) => {
        loopActiveNotesRef.current.forEach((note) => {
          try {
            out.noteOff(MIDI_CHANNEL, note);
          } catch {
            /* ignore */
          }
        });
      });
    }
    loopActiveNotesRef.current.clear();
    stopChord();
    if (loopAnimationFrameRef.current != null) {
      window.cancelAnimationFrame(loopAnimationFrameRef.current);
      loopAnimationFrameRef.current = null;
    }
    loopIterationStartRef.current = null;
    if (activeLoopEventRef.current != null) {
      onLoopEvent?.({ index: activeLoopEventRef.current, active: false });
      activeLoopEventRef.current = null;
    }
    setLoopProgress(0);
  }, [clearLoopTimeouts, setLoopProgress, stopChord]);

  const startLoopPlayback = useCallback(
    (playback: RenderedPlaybackResponse) => {
      stopLoopPlayback();
      isLoopingRef.current = true;
      setIsLooping(true);
      setPlaybackError(null);
      renderedPlaybackRef.current = playback;
      loopIterationStartRef.current = performance.now();
      setLoopProgress(0);
      if (loopAnimationFrameRef.current != null) {
        window.cancelAnimationFrame(loopAnimationFrameRef.current);
      }
      loopAnimationFrameRef.current = window.requestAnimationFrame(updateLoopProgress);
      void scheduleLoopPlayback(playback);
    },
    [scheduleLoopPlayback, setLoopProgress, setPlaybackError, stopLoopPlayback, updateLoopProgress]
  );

  const handleLoopPlay = useCallback(() => {
    const playback = renderedPlaybackRef.current ?? renderedPlayback;
    if (playback && playback.events.length > 0) {
      startLoopPlayback(playback);
    }
  }, [renderedPlayback, startLoopPlayback]);

  const handleLoopStop = useCallback(() => {
    stopLoopPlayback();
  }, [stopLoopPlayback]);

  useEffect(() => {
    return () => {
      stopLoopPlayback();
    };
  }, [stopLoopPlayback]);

  const endActiveChord = useCallback((timestamp: number) => {
    const startTime = recordingStartRef.current;
    const active = activeChordRecordingRef.current;
    if (startTime == null || !active) {
      return;
    }
    const relative = Math.max(0, timestamp - startTime);
    recordingEventsRef.current.push({
      type: "chord-off",
      at: relative,
      id: active.id
    });
    activeChordRecordingRef.current = null;
  }, []);

  const resetRecordingState = useCallback(() => {
    recordingStartRef.current = null;
    recordingEventsRef.current = [];
    activeChordRecordingRef.current = null;
  }, []);

  const submitRecording = useCallback(
    async (events: RecordingEvent[]) => {
      const trimmedUrl = serverUrl.trim();
      if (!trimmedUrl) {
        setPlaybackError("Server URL is not configured.");
        return;
      }
      stopLoopPlayback();
      playbackAbortRef.current?.abort();
      const controller = new AbortController();
      playbackAbortRef.current = controller;
      setIsRenderingPlayback(true);
      setPlaybackError(null);
      const normalizedEvents = events.map((event) => {
        const at = Number(event.at.toFixed(3));
        if (event.type === "chord-on") {
          return {
            type: "chord-on",
            at,
            id: event.id,
            notes: event.notes.map(clampMidi)
          };
        }
        if (event.type === "chord-off") {
          return {
            type: "chord-off",
            at,
            id: event.id
          };
        }
        return {
          type: event.type,
          at
        };
      });
      const sanitizedTempoCandidate =
        Number.isFinite(tempoBpm) && tempoBpm > 0 ? tempoBpm : DEFAULT_TEMPO_BPM;
      const sanitizedTempo = Math.min(400, Math.max(20, sanitizedTempoCandidate));
      const optionsPayload: PlaybackOptionsPayload = {
        tempoBpm: sanitizedTempo
      };
      if (quantizationOption !== "none") {
        optionsPayload.quantization = quantizationOption;
      }
      const meterPayload = parseMeterValue(meterBeatsInput, meterUnitInput);
      if (meterPayload) {
        optionsPayload.loopMeter = meterPayload;
      }
      try {
        const response = await fetch(
          `${trimmedUrl.replace(/\/+$/, "")}/tonnetz/recording/render`,
          {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
              events: normalizedEvents,
              options: optionsPayload
            }),
            signal: controller.signal
          }
        );
        if (!response.ok) {
          const message = await response.text();
          let errorMessage = message;
          try {
            const parsed = JSON.parse(message) as { error?: string };
            if (parsed && typeof parsed.error === "string") {
              errorMessage = parsed.error;
            }
          } catch {
            /* ignore malformed error bodies */
          }
          throw new Error(
            errorMessage !== "" ? errorMessage : `Playback rendering failed (${response.status})`
          );
        }
        const data = (await response.json()) as RenderedPlaybackResponse;
        setRenderedPlayback(data);
        setPlaybackError(null);
      } catch (error) {
        if (error instanceof DOMException && error.name === "AbortError") {
          return;
        }
        const message =
          error instanceof Error ? error.message : "Failed to render loop playback.";
        setPlaybackError(message);
        setRenderedPlayback(null);
      } finally {
        setIsRenderingPlayback(false);
        playbackAbortRef.current = null;
      }
    },
    [meterBeatsInput, meterUnitInput, quantizationOption, serverUrl, stopLoopPlayback, tempoBpm]
  );

  const startRecordingSession = useCallback(() => {
    stopLoopPlayback();
    resetRecordingState();
    setRenderedPlayback(null);
    setPlaybackError(null);
    setRecordedChords([]);
    lastRecordingRef.current = null;
    const start = performance.now();
    recordingStartRef.current = start;
    recordingEventsRef.current = [
      {
        type: "start",
        at: 0
      }
    ];
    activeChordRecordingRef.current = null;
  }, [resetRecordingState, stopLoopPlayback]);

  const finishRecordingSession = useCallback(() => {
    const startTime = recordingStartRef.current;
    if (startTime == null) {
      return;
    }
    const now = performance.now();
    endActiveChord(now);
    const snapshot = recordingEventsRef.current.slice();
    snapshot.push({
      type: "stop",
      at: Math.max(0, now - startTime)
    });
    resetRecordingState();
    const recordedChordEvents = snapshot.filter((event) => event.type === "chord-on");
    if (recordedChordEvents.length === 0) {
      setPlaybackError("No chords were captured during this recording.");
      setRenderedPlayback(null);
      return;
    }
    lastRecordingRef.current = snapshot;
    void submitRecording(snapshot);
  }, [endActiveChord, resetRecordingState, submitRecording]);

  useEffect(() => {
    if (typeof window === "undefined") {
      return;
    }
    setSynthStatus("idle");
    setSynthError(null);
    setMidiMixerStatus("idle");
    setMidiMixerError(null);
    try {
      JZZ.synth.Tiny.register("Web Audio");
    } catch {
      /* registration may fail if already registered */
    }
    return () => {
      optionsAbortRef.current?.abort();
      tilingAbortRef.current?.abort();
      stopLoopPlayback();
      playbackAbortRef.current?.abort();
      playbackAbortRef.current = null;
      stopChord();
      if (midiOutRef.current) {
        try {
          midiOutRef.current.close();
        } catch {
          /* ignore */
        }
        midiOutRef.current = null;
      }
      if (midiMixerOutRef.current) {
        try {
          midiMixerOutRef.current.close();
        } catch {
          /* ignore */
        }
        midiMixerOutRef.current = null;
      }
      midiOpenPromiseRef.current = null;
      setSynthStatus("idle");
      setSynthError(null);
      setMidiMixerStatus("idle");
      setMidiMixerError(null);
      setAvailableMixerOutputs([]);
      midiMixerOpenPromiseRef.current = null;
    };
  }, [stopChord, stopLoopPlayback]);

  useEffect(() => {
    chordNameCacheRef.current.clear();
  }, [serverUrl]);

  useEffect(() => {
    stopChord();
    setCurrentChordDescription(null);
  }, [transposeSemitones, stopChord]);

  useEffect(() => {
    if (!serverUrl) {
      setOptions([]);
      setStructureId(null);
      setIntervalId(null);
      return;
    }
    optionsAbortRef.current?.abort();
    const controller = new AbortController();
    optionsAbortRef.current = controller;
    setLoadingOptions(true);
    setOptionsError(null);
    (async () => {
      try {
        const response = await fetch(`${serverUrl.replace(/\/+$/, "")}/tonnetz/options`, {
          method: "GET",
          headers: { "Content-Type": "application/json" },
          signal: controller.signal
        });
        if (!response.ok) {
          throw new Error(`Server responded with ${response.status}`);
        }
        const payload = (await response.json()) as TonnetzOptionsResponse;
        const structures = Array.isArray(payload.structures) ? payload.structures : [];
        setOptions(structures);
        const defaultStructure = resolveDefaultStructure(structures);
        if (defaultStructure) {
          setStructureId((prev) => prev ?? defaultStructure.id);
          if (defaultStructure.intervals.length > 0) {
            setIntervalId((prev) => prev ?? defaultStructure.intervals[0]!.id);
          }
        }
      } catch (err) {
        if (controller.signal.aborted) {
          return;
        }
        const message = err instanceof Error ? err.message : "Failed to load Tonnetz options.";
        setOptions([]);
        setStructureId(null);
        setIntervalId(null);
        setOptionsError(message);
      } finally {
        setLoadingOptions(false);
      }
    })();
    return () => {
      controller.abort();
    };
  }, [serverUrl]);

  useEffect(() => {
    if (!structureId && options.length > 0) {
      const defaultStructure = resolveDefaultStructure(options);
      if (defaultStructure) {
        setStructureId(defaultStructure.id);
      }
    }
  }, [options, structureId]);

  useEffect(() => {
    if (!structureId) {
      return;
    }
    const structure = options.find((candidate) => candidate.id === structureId);
    if (!structure) {
      return;
    }
    if (!intervalId && structure.intervals.length > 0) {
      setIntervalId(structure.intervals[0]!.id);
      return;
    }
    if (intervalId && !structure.intervals.some((candidate) => candidate.id === intervalId)) {
      setIntervalId(structure.intervals[0]?.id ?? null);
    }
  }, [structureId, intervalId, options]);

  useEffect(() => {
    if (!serverUrl || !structureId || !intervalId) {
      setTiling(null);
      return;
    }
    const structure = options.find((candidate) => candidate.id === structureId);
    const intervalOption = structure?.intervals.find((candidate) => candidate.id === intervalId);
    if (!structure || !intervalOption) {
      return;
    }
    tilingAbortRef.current?.abort();
    const controller = new AbortController();
    tilingAbortRef.current = controller;
    setLoadingTiling(true);
    setTilingError(null);
    (async () => {
      try {
        const payload: TonnetzTilingRequest = {
          structure: structure.label,
          interval: intervalOption.steps,
          degree,
          baseMidi: 60
        };
        const response = await fetch(`${serverUrl.replace(/\/+$/, "")}/tonnetz/tiling`, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
          signal: controller.signal
        });
        if (!response.ok) {
          throw new Error(`Server responded with ${response.status}`);
        }
        const tilingResponse = (await response.json()) as TonnetzTilingResponse;
        setTiling(tilingResponse);
      } catch (err) {
        if (controller.signal.aborted) {
          return;
        }
        const message = err instanceof Error ? err.message : "Failed to load Tonnetz tiling.";
        setTiling(null);
        setTilingError(message);
      } finally {
        setLoadingTiling(false);
      }
    })();
    return () => {
      controller.abort();
    };
  }, [serverUrl, structureId, intervalId, degree, options]);

  const vertexByCoordinate = useMemo(() => {
    const map = new Map<string, TonnetzVertexResponse>();
    if (tiling) {
      tiling.vertices.forEach((vertex) => {
        map.set(coordinateKey(vertex.coordinate), vertex);
      });
    }
    return map;
  }, [tiling]);

  const vertexPositionLookup = useMemo(() => {
    if (!tiling) {
      return new Map<string, [number, number]>();
    }
    const mapping = new Map<string, [number, number]>();
    tiling.polygons.forEach((polygon) => {
      polygon.vertexCoordinates.forEach((coord, index) => {
        const key = coordinateKey(coord);
        if (!mapping.has(key) && polygon.faceVertices[index]) {
          mapping.set(key, polygon.faceVertices[index]!);
        }
      });
    });
    return mapping;
  }, [tiling]);

  const buildChordLabels = useCallback(
    (polygon: TonnetzPolygonResponse, transpose: number) => {
      const dedupe = <T,>(values: T[]): T[] => {
        const seen = new Set<T>();
        const result: T[] = [];
        values.forEach((value) => {
          if (!seen.has(value)) {
            seen.add(value);
            result.push(value);
          }
        });
        return result;
      };

      const pitchClassNames = dedupe(
        polygon.vertexCoordinates.map((coord, index) => {
          const vertex = vertexByCoordinate.get(coordinateKey(coord));
          if (vertex) {
            return pitchClassNameFromNumber(vertex.pitchClass + transpose);
          }
          const fallbackPc = polygon.pitchClasses[index] ?? 0;
          return pitchClassNameFromNumber(fallbackPc + transpose);
        })
      );

      const noteNames = dedupe(
        polygon.vertexCoordinates.map((coord, index) => {
          const vertex = vertexByCoordinate.get(coordinateKey(coord));
          if (vertex) {
            return midiToNoteName(vertex.midi + transpose);
          }
          const fallbackMidi = polygon.midiNotes[index] ?? 60;
          return midiToNoteName(fallbackMidi + transpose);
        })
      );

      return { pitchClassNames, noteNames };
    },
    [vertexByCoordinate]
  );

  const fetchChordName = useCallback(
    async (pitchClasses: number[], transpose: number): Promise<ChordNameInfo> => {
      const cache = chordNameCacheRef.current;
      const transposed = Array.from(
        new Set(pitchClasses.map((value) => normalisePitchClass(value + transpose)))
      ).sort((a, b) => a - b);
      const cacheKey = transposed.join("-");
      if (cache.has(cacheKey)) {
        return cache.get(cacheKey)!;
      }

      const defaultInfo: ChordNameInfo = { name: null, aliases: [] };
      const trimmedUrl = serverUrl.trim();
      if (!trimmedUrl) {
        cache.set(cacheKey, defaultInfo);
        return defaultInfo;
      }

      try {
        const response = await fetch(`${trimmedUrl.replace(/\/+$/, "")}/fretboard/chord-names`, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            noteSets: [
              {
                id: "tonnetz",
                pitchClasses: transposed
              }
            ]
          })
        });
        if (!response.ok) {
          throw new Error(`Chord name lookup failed (${response.status})`);
        }
        const data = (await response.json()) as {
          chordNames?: Array<{ id: string; name: string | null; aliases: string[] }>;
        };
        const entry =
          Array.isArray(data.chordNames) && data.chordNames.length > 0
            ? data.chordNames.find((item) => item.id === "tonnetz") ?? data.chordNames[0]
            : undefined;
        const info: ChordNameInfo = {
          name: entry?.name ?? null,
          aliases: entry?.aliases ?? []
        };
        cache.set(cacheKey, info);
        return info;
      } catch (error) {
        cache.set(cacheKey, defaultInfo);
        return defaultInfo;
      }
    },
    [serverUrl]
  );

  const recordChord = useCallback(
    (
      midiNotes: number[],
      labels: { pitchClassNames: string[]; noteNames: string[] },
      fallback: ChordNameInfo,
      transpose: number
    ): string | null => {
      if (!isRecording) {
        return null;
      }
      const entry: RecordedChord = {
        id: `${Date.now()}-${Math.random().toString(36).slice(2, 10)}`,
        midiNotes: midiNotes.map(clampMidi),
        pitchClassNames: [...labels.pitchClassNames],
        noteNames: [...labels.noteNames],
        chordName: fallback.name,
        aliases: [...fallback.aliases],
        transpose
      };
      setRecordedChords((prev) => [...prev, entry]);
      const startTime = recordingStartRef.current;
      if (startTime != null) {
        const timestamp = performance.now();
        endActiveChord(timestamp);
        recordingEventsRef.current.push({
          type: "chord-on",
          at: Math.max(0, timestamp - startTime),
          id: entry.id,
          notes: entry.midiNotes
        });
        activeChordRecordingRef.current = {
          id: entry.id,
          startedAt: timestamp,
          notes: entry.midiNotes
        };
      }
      return entry.id;
    },
    [endActiveChord, isRecording]
  );

  useEffect(() => {
    const container = containerRef.current;
    if (!container) {
      return;
    }
    d3.select(container).selectAll("svg").remove();
    if (!tiling) {
      return;
    }
    const allVertices = tiling.polygons.flatMap((polygon) => polygon.faceVertices);
    if (allVertices.length === 0) {
      return;
    }
    const xExtent = d3.extent(allVertices, (d) => d[0]);
    const yExtent = d3.extent(allVertices, (d) => d[1]);
    if (xExtent[0] === undefined || xExtent[1] === undefined || yExtent[0] === undefined || yExtent[1] === undefined) {
      return;
    }
    const width = SVG_SIZE;
    const height = SVG_SIZE;
    const innerWidth = width - SVG_PADDING * 2;
    const innerHeight = height - SVG_PADDING * 2;

    const xScale = d3.scaleLinear().domain([xExtent[0], xExtent[1]]).range([0, innerWidth]);
    const yScale = d3.scaleLinear().domain([yExtent[1], yExtent[0]]).range([0, innerHeight]);

    const svg = d3
      .select(container)
      .append("svg")
      .attr("width", width)
      .attr("height", height)
      .attr("class", "tonnetz-svg");

    svgRef.current = svg.node();

    const root = svg
      .append("g")
      .attr("class", "tonnetz-root")
      .attr("transform", `translate(${SVG_PADDING}, ${SVG_PADDING})`);

    const polygonLayer = root.append("g").attr("class", "tonnetz-polygons");
    const vertexLayer = root.append("g").attr("class", "tonnetz-vertices");

    const lineGenerator = d3
      .line<[number, number]>()
      .x((d) => xScale(d[0]))
      .y((d) => yScale(d[1]))
      .curve(d3.curveLinearClosed);

    const colorScale = d3.scaleSequential(d3.interpolatePuBuGn).domain([0, 1]);

    const polygonSelection = polygonLayer
      .selectAll<SVGPathElement, TonnetzPolygonResponse>("path")
      .data(tiling.polygons)
      .join("path")
      .attr("d", (d) => lineGenerator(d.faceVertices) ?? "")
      .attr("fill", "#ffffff")
      .attr("stroke", "#6b7280")
      .attr("stroke-width", 1.5)
      .attr("cursor", synthStatus === "error" ? "not-allowed" : "pointer");

    const vertexData: VertexRenderDatum[] = tiling.vertices
      .map((vertex) => {
        const position = vertexPositionLookup.get(coordinateKey(vertex.coordinate));
        if (!position) {
          return null;
        }
        return { vertex, position };
      })
      .filter((value): value is VertexRenderDatum => value !== null);

    const vertexSelection = vertexLayer
      .selectAll<SVGCircleElement, VertexRenderDatum>("circle")
      .data(vertexData)
      .join("circle")
      .attr("cx", (d) => xScale(d.position[0]))
      .attr("cy", (d) => yScale(d.position[1]))
      .attr("r", 10)
      .attr("stroke", "#4b5563")
      .attr("stroke-width", 1.5)
      .attr("fill", "#ffffff");

    vertexLayer
      .selectAll<SVGTextElement, VertexRenderDatum>("text")
      .data(vertexData)
      .join("text")
      .attr("x", (d) => xScale(d.position[0]))
      .attr("y", (d) => yScale(d.position[1]) + 3)
      .attr("text-anchor", "middle")
      .attr("font-size", "11px")
      .attr("fill", "#374151")
      .text((d) => pitchClassNameFromNumber(d.vertex.pitchClass + transposeSemitones));

    let activePolygon: TonnetzPolygonResponse | null = null;
    let pointerDown = false;

    const pitchClassSet = (polygon: TonnetzPolygonResponse): Set<number> =>
      new Set(polygon.pitchClasses.map((value) => normalisePitchClass(value + transposeSemitones)));
    const midiSetForPolygon = (polygon: TonnetzPolygonResponse): Set<number> =>
      new Set(polygon.midiNotes.map((value) => clampMidi(value + transposeSemitones)));

    const updatePolygonStyles = (selected: TonnetzPolygonResponse | null) => {
      if (!selected) {
        polygonSelection.attr("fill", "#ffffff");
        vertexSelection.attr("fill", "#ffffff");
        return;
      }
      const selectedPitchClasses = pitchClassSet(selected);
      polygonSelection.attr("fill", (polygon) => {
        const comparison = pitchClassSet(polygon);
        let overlap = 0;
        selectedPitchClasses.forEach((value) => {
          if (comparison.has(value)) {
            overlap += 1;
          }
        });
        const ratio = selectedPitchClasses.size === 0 ? 0 : overlap / selectedPitchClasses.size;
        return ratio === 0 ? "#ffffff" : colorScale(ratio);
      });

      const selectedMidi = midiSetForPolygon(selected);
      vertexSelection.attr("fill", (d) => (selectedMidi.has(clampMidi(d.vertex.midi + transposeSemitones)) ? "#facc15" : "#ffffff"));
    };

    const activatePolygon = (polygon: TonnetzPolygonResponse) => {
      const transposedMidi = polygon.midiNotes.map((value) => clampMidi(value + transposeSemitones));
      const labels = buildChordLabels(polygon, transposeSemitones);
      const fallbackInfo: ChordNameInfo = {
        name: null,
        aliases: []
      };
      stopLoopPlayback();
      activePolygon = polygon;
      updatePolygonStyles(polygon);
      setCurrentChordDescription({
        name: fallbackInfo.name,
        aliases: [...fallbackInfo.aliases],
        pitchClassNames: [...labels.pitchClassNames],
        noteNames: [...labels.noteNames],
        transpose: transposeSemitones
      });
      const recordedId = recordChord(transposedMidi, labels, fallbackInfo, transposeSemitones);

      void Promise.all([ensureMidiOut(), ensureMidiMixerOut()])
        .then(([synthOut, mixerOut]) => {
          if (activePolygon !== polygon) {
            return;
          }
          if (!synthOut && !mixerOut) {
            return;
          }
          playChord(transposedMidi);
        })
        .catch(() => {
          /* ignore synth initialisation errors here */
        });

      fetchChordName(polygon.pitchClasses, transposeSemitones)
        .then((info) => {
          const resolvedName = info.name ?? fallbackInfo.name ?? null;
          const resolvedAliases =
            info.aliases.length > 0 ? info.aliases : fallbackInfo.aliases;
          if (recordedId) {
            setRecordedChords((prev) =>
              prev.map((entry) =>
                entry.id === recordedId
                  ? {
                      ...entry,
                      chordName: resolvedName,
                      aliases: [...resolvedAliases]
                    }
                  : entry
              )
            );
          }
          if (activePolygon === polygon) {
            setCurrentChordDescription({
              name: resolvedName,
              aliases: [...resolvedAliases],
              pitchClassNames: [...labels.pitchClassNames],
              noteNames: [...labels.noteNames],
              transpose: transposeSemitones
            });
          }
        })
        .catch(() => {
          // leave fallback naming
        });
    };

    const clearActive = () => {
      activePolygon = null;
      pointerDown = false;
      updatePolygonStyles(null);
      stopChord();
      if (recordingStartRef.current != null) {
        endActiveChord(performance.now());
      }
      setCurrentChordDescription(null);
    };

    polygonSelection.on("pointerdown", (event, polygon) => {
      event.preventDefault();
      pointerDown = true;
      activatePolygon(polygon);
    });

    polygonSelection.on("pointerenter", (_event, polygon) => {
      if (pointerDown) {
        activatePolygon(polygon);
      }
    });

    svg.on("pointerup pointerleave pointercancel", () => {
      clearActive();
    });

    return () => {
      clearActive();
      svg.remove();
    };
  }, [tiling, playChord, stopChord, vertexPositionLookup, synthStatus, buildChordLabels, recordChord, fetchChordName, ensureMidiOut, ensureMidiMixerOut, transposeSemitones, endActiveChord, stopLoopPlayback]);

  const structureOptions = options;
  const intervalOptions =
    structureOptions.find((candidate) => candidate.id === structureId)?.intervals ?? [];
  const activeStructure = structureOptions.find((candidate) => candidate.id === structureId) ?? null;
  const activeInterval = intervalOptions.find((entry) => entry.id === intervalId) ?? null;
  const activeIntervalSteps = activeInterval ? activeInterval.steps.join("-") : null;

  const toggleRecording = () => {
    if (isRecording) {
      finishRecordingSession();
      setIsRecording(false);
    } else {
      startRecordingSession();
      setIsRecording(true);
    }
  };

  const handleRenderLoop = () => {
    if (isRecording) {
      return;
    }
    const snapshot = lastRecordingRef.current;
    if (!snapshot || snapshot.length === 0) {
      setPlaybackError("Record a stack before rendering playback.");
      return;
    }
    void submitRecording(snapshot);
  };

  useEffect(() => {
    const previous = previousPlaybackOptionsRef.current;
    if (
      previous.quantization === quantizationOption &&
      previous.meterBeats === meterBeatsInput &&
      previous.meterUnit === meterUnitInput &&
      previous.tempo === tempoBpm
    ) {
      return;
    }
    previousPlaybackOptionsRef.current = {
      quantization: quantizationOption,
      meterBeats: meterBeatsInput,
      meterUnit: meterUnitInput,
      tempo: tempoBpm
    };
    if (isRecording || isRenderingPlayback) {
      return;
    }
    const snapshot = lastRecordingRef.current;
    if (!snapshot || snapshot.length === 0) {
      return;
    }
    void submitRecording(snapshot);
  }, [
    isRecording,
    isRenderingPlayback,
    meterBeatsInput,
    meterUnitInput,
    quantizationOption,
    submitRecording,
    tempoBpm
  ]);

  const clearRecorded = () => {
    stopLoopPlayback();
    playbackAbortRef.current?.abort();
    playbackAbortRef.current = null;
    setIsRenderingPlayback(false);
    setRecordedChords([]);
    setRenderedPlayback(null);
    setPlaybackError(null);
    resetRecordingState();
    lastRecordingRef.current = null;
  };

  return (
    <div className="tonnetz-widget">
      <div className="tonnetz-control-sections">
        <div className="tonnetz-controls">
          <div className="tonnetz-controls-tabs" role="tablist">
            <button
              type="button"
              className={activeControlTab === "structure" ? "tonnetz-controls-tab active" : "tonnetz-controls-tab"}
              role="tab"
              aria-selected={activeControlTab === "structure"}
              onClick={() => setActiveControlTab("structure")}
            >
              Structure
            </button>
            <button
              type="button"
              className={activeControlTab === "transpose" ? "tonnetz-controls-tab active" : "tonnetz-controls-tab"}
              role="tab"
              aria-selected={activeControlTab === "transpose"}
              onClick={() => setActiveControlTab("transpose")}
            >
              Transpose
            </button>
            <button
              type="button"
              className={activeControlTab === "mixer" ? "tonnetz-controls-tab active" : "tonnetz-controls-tab"}
              role="tab"
              aria-selected={activeControlTab === "mixer"}
              onClick={() => setActiveControlTab("mixer")}
            >
              Mixer
            </button>
            <button
              type="button"
              className={activeControlTab === "render" ? "tonnetz-controls-tab active" : "tonnetz-controls-tab"}
              role="tab"
              aria-selected={activeControlTab === "render"}
              onClick={() => setActiveControlTab("render")}
            >
              Render
            </button>
          </div>

          <div className="tonnetz-controls-panels">
            {activeControlTab === "structure" && (
              <div className="tonnetz-controls-panel" role="tabpanel" aria-label="Structure controls">
                <div className="field">
                  <label htmlFor="tonnetz-structure">Structure</label>
                  <select
                    id="tonnetz-structure"
                    value={structureId ?? ""}
                    onChange={(event) => setStructureId(event.target.value)}
                    disabled={loadingOptions || options.length === 0}
                  >
                    {structureOptions.map((structure) => (
                      <option key={structure.id} value={structure.id}>
                        {structure.label}
                      </option>
                    ))}
                  </select>
                </div>

                <div className="field">
                  <label htmlFor="tonnetz-interval">Interval Set</label>
                  <select
                    id="tonnetz-interval"
                    value={intervalId ?? ""}
                    onChange={(event) => setIntervalId(event.target.value)}
                    disabled={loadingOptions || intervalOptions.length === 0}
                  >
                    {intervalOptions.map((interval) => (
                      <option key={interval.id} value={interval.id}>
                        {interval.label}
                      </option>
                    ))}
                  </select>
                </div>

                <div className="field">
                  <label htmlFor="tonnetz-degree">Scale Degree</label>
                  <select
                    id="tonnetz-degree"
                    value={degree}
                    onChange={(event) => setDegree(event.target.value)}
                    disabled={loadingOptions}
                  >
                    {DEGREE_OPTIONS.map((option) => (
                      <option key={option} value={option}>
                        {option}
                      </option>
                    ))}
                  </select>
                </div>

                <div className="tonnetz-control-summary">
                  <div>
                    <strong>Structure:</strong> {activeStructure?.label ?? "—"}
                  </div>
                  <div>
                    <strong>Interval Set:</strong> {activeIntervalSteps ?? "—"}
                  </div>
                  <div>
                    <strong>Scale Degree:</strong> {degree}
                  </div>
                </div>
              </div>
            )}

            {activeControlTab === "transpose" && (
              <div className="tonnetz-controls-panel" role="tabpanel" aria-label="Transpose controls">
                <div className="field tonnetz-transpose">
                  <label htmlFor="tonnetz-transpose">Transpose (semitones)</label>
                  <div className="tonnetz-transpose-controls">
                    <button
                      type="button"
                      className="tonnetz-transpose-step"
                      onClick={() => setTransposeSemitones((value) => value - 1)}
                    >
                      –
                    </button>
                    <input
                      id="tonnetz-transpose"
                      type="number"
                      step={1}
                      value={transposeSemitones}
                      onChange={(event) => {
                        const value = Number.parseInt(event.target.value, 10);
                        if (!Number.isNaN(value)) {
                          setTransposeSemitones(value);
                        }
                      }}
                      aria-label="Transpose in semitones"
                    />
                    <button
                      type="button"
                      className="tonnetz-transpose-step"
                      onClick={() => setTransposeSemitones((value) => value + 1)}
                    >
                      +
                    </button>
                    <button
                      type="button"
                      className="tonnetz-transpose-reset"
                      onClick={() => setTransposeSemitones(0)}
                    >
                      Reset
                    </button>
                  </div>
                </div>
                <div className="tonnetz-control-summary">
                  <strong>Current transpose:</strong> {transposeSemitones > 0 ? `+${transposeSemitones}` : transposeSemitones} st
                </div>
              </div>
            )}

            {activeControlTab === "mixer" && (
              <div className="tonnetz-controls-panel" role="tabpanel" aria-label="Mixer controls">
                <div className="field">
                  <label htmlFor="tonnetz-mixer-output">Mixer Output</label>
                  <select
                    id="tonnetz-mixer-output"
                    value={selectedMixerOutput}
                    onChange={(event) => setSelectedMixerOutput(event.target.value)}
                  >
                    <option value={AUTO_MIXER_SELECTION}>Auto (prefer "MIDI Mixer")</option>
                    <option value={NO_MIXER_SELECTION}>None</option>
                    {availableMixerOutputs.map((name) => (
                      <option key={name} value={name}>
                        {name}
                      </option>
                    ))}
                  </select>
                </div>
                <div className="tonnetz-control-summary">
                  <div>
                    <strong>Synth:</strong>{" "}
                    {synthStatus === "ready"
                      ? "Ready"
                      : synthStatus === "initializing"
                        ? "Initialising"
                        : synthStatus === "error"
                          ? `Error${synthError ? ` – ${synthError}` : ""}`
                          : "Idle"}
                  </div>
                  <div>
                    <strong>Mixer:</strong>{" "}
                    {midiMixerStatus === "ready"
                      ? selectedMixerOutput === NO_MIXER_SELECTION
                        ? "Disabled"
                        : "Ready"
                      : midiMixerStatus === "initializing"
                        ? "Initialising"
                        : midiMixerStatus === "error"
                          ? `Error${midiMixerError ? ` – ${midiMixerError}` : ""}`
                          : selectedMixerOutput === NO_MIXER_SELECTION
                            ? "Disabled"
                            : "Idle"}
                  </div>
                </div>
              </div>
            )}

            {activeControlTab === "render" && (
              <div className="tonnetz-controls-panel" role="tabpanel" aria-label="Render controls">
                <div className="field">
                  <label className="tonnetz-playback-select">
                    Quantization
                    <select
                      value={quantizationOption}
                      onChange={(event) =>
                        setQuantizationOption(event.target.value as PlaybackQuantizationOption | "none")
                      }
                      disabled={isRecording}
                    >
                      {QUANTIZATION_OPTIONS.map((option) => (
                        <option key={option.value} value={option.value}>
                          {option.label}
                        </option>
                      ))}
                    </select>
                  </label>
                </div>
                <div className="field tonnetz-meter-input">
                  <label>
                    Loop Meter
                    <div className="tonnetz-meter-fields">
                      <input
                        type="number"
                        min={1}
                        value={meterBeatsInput}
                        onChange={(event) => setMeterBeatsInput(event.target.value)}
                        disabled={isRecording}
                      />
                      <span>/</span>
                      <input
                        type="number"
                        min={1}
                        value={meterUnitInput}
                        onChange={(event) => setMeterUnitInput(event.target.value)}
                        disabled={isRecording}
                      />
                    </div>
                  </label>
                </div>
                <div className="field">
                  <label>
                    Tempo (BPM)
                    <input
                      type="number"
                      min={20}
                      max={400}
                      value={tempoBpm}
                      onChange={(event) => {
                        const value = Number.parseInt(event.target.value, 10);
                        if (Number.isNaN(value)) {
                          setTempoBpm(DEFAULT_TEMPO_BPM);
                          return;
                        }
                        const clamped = Math.min(400, Math.max(20, value));
                        setTempoBpm(clamped);
                      }}
                      disabled={isRecording}
                    />
                  </label>
                </div>
                <div className="tonnetz-render-actions">
                  <button
                    type="button"
                    onClick={handleRenderLoop}
                    disabled={isRecording || !hasRecordedSession || isRenderingPlayback}
                  >
                    Render Loop
                  </button>
                  <div className="tonnetz-render-status">
                    {isRenderingPlayback && <div className="status info">Rendering loop…</div>}
                    {playbackError && <div className="status error">{playbackError}</div>}
                    {renderedPlayback && !isRenderingPlayback && !playbackError && (
                      <div className="status info">
                        Loop ready: {renderedPlayback.events.length} chords ·{" "}
                        {renderedPlayback.totalSeconds.toFixed(2)} s ·{" "}
                        <a
                          href={`data:audio/midi;base64,${renderedPlayback.midiBase64}`}
                          download="tonnetz-loop.mid"
                        >
                          Download MIDI
                        </a>
                      </div>
                    )}
                  </div>
                </div>
              </div>
            )}
          </div>
        </div>
      </div>

      <div className="tonnetz-record-controls">
        <div className="tonnetz-record-section">
          <button
            type="button"
            className={isRecording ? "tonnetz-record-button active" : "tonnetz-record-button"}
            onClick={toggleRecording}
            disabled={synthStatus !== "ready"}
          >
            {isRecording ? "Stop Recording" : "Record"}
          </button>
          <button type="button" onClick={clearRecorded} disabled={recordedChords.length === 0}>
            Clear Stack
          </button>
        </div>

        <div className="tonnetz-record-section tonnetz-status-section">
          <span className="tonnetz-synth-status">
            Synth: {
              synthStatus === "ready"
                ? "Ready"
                : synthStatus === "initializing"
                  ? "Initialising"
                  : synthStatus === "error"
                    ? `Error${synthError ? ` – ${synthError}` : ""}`
                    : "Idle"
            }
          </span>
          <span className="tonnetz-mixer-status">
            Mixer: {
              midiMixerStatus === "ready"
                ? selectedMixerOutput === NO_MIXER_SELECTION
                  ? "Disabled"
                  : "Ready"
                : midiMixerStatus === "initializing"
                  ? "Initialising"
                  : midiMixerStatus === "error"
                    ? `Error${midiMixerError ? ` – ${midiMixerError}` : ""}`
                    : selectedMixerOutput === NO_MIXER_SELECTION
                      ? "Disabled"
                      : "Idle"
            }
          </span>
          {isRenderingPlayback && <div className="status info">Rendering loop…</div>}
          {playbackError && <div className="status error">{playbackError}</div>}
          {renderedPlayback && !isRenderingPlayback && !playbackError && (
            <div className="status info">
              Loop ready: {renderedPlayback.events.length} chords ·{" "}
              {renderedPlayback.totalSeconds.toFixed(2)} s ·{" "}
              <a
                href={`data:audio/midi;base64,${renderedPlayback.midiBase64}`}
                download="tonnetz-loop.mid"
              >
                Download MIDI
              </a>{" "}
              · Use the Piano Roll tab to view and control the loop.
            </div>
          )}
        </div>
      </div>

      <div className="tonnetz-tab-bar" role="tablist">
        <button
          type="button"
          className={activeView === "tonnetz" ? "tonnetz-tab active" : "tonnetz-tab"}
          role="tab"
          aria-selected={activeView === "tonnetz"}
          onClick={() => setActiveView("tonnetz")}
        >
          Tonnetz
        </button>
        <button
          type="button"
          className={
            activeView === "piano-roll"
              ? "tonnetz-tab active"
              : hasRenderedPlayback
                ? "tonnetz-tab"
                : "tonnetz-tab disabled"
          }
          role="tab"
          aria-selected={activeView === "piano-roll"}
          aria-disabled={!hasRenderedPlayback}
          disabled={!hasRenderedPlayback}
          onClick={() => {
            if (hasRenderedPlayback) {
              setActiveView("piano-roll");
            }
          }}
        >
          Piano Roll
        </button>
      </div>

      <div className="tonnetz-tab-panels">
        {activeView === "tonnetz" && (
          <div className="tonnetz-panel" role="tabpanel">
            {optionsError && <div className="status error">{optionsError}</div>}
            {tilingError && <div className="status error">{tilingError}</div>}
            {loadingTiling && <div className="status info">Loading tiling…</div>}
            <div className="tonnetz-canvas" ref={containerRef} />
          </div>
        )}
        {activeView === "piano-roll" && (
          <div className="tonnetz-panel" role="tabpanel">
            {hasRenderedPlayback ? (
              <PianoRollView
                events={pianoRollEvents}
                totalSeconds={totalPlaybackSeconds}
                progress={loopProgress}
                isPlaying={isLooping}
                onPlay={handleLoopPlay}
                onStop={handleLoopStop}
                midiDownloadHref={
                  renderedPlayback?.midiBase64
                    ? `data:audio/midi;base64,${renderedPlayback.midiBase64}`
                    : null
                }
              />
            ) : (
              <div className="status info">Render a loop to view the piano roll.</div>
            )}
          </div>
        )}
      </div>

      {activeView === "tonnetz" && currentChordDescription && (
        <div className="tonnetz-current">
          <h3>Current Chord</h3>
          <div className="tonnetz-current-name">
            {currentChordDescription.name ?? currentChordDescription.pitchClassNames.join(" ")}
          </div>
          {currentChordDescription.aliases.length > 0 && (
            <div className="tonnetz-current-aliases">
              Aliases: {currentChordDescription.aliases.join(", ")}
            </div>
          )}
          <div className="tonnetz-current-notes">
            Notes: {currentChordDescription.noteNames.join(", ")}
          </div>
          <div className="tonnetz-current-transpose">
            Transpose: {currentChordDescription.transpose > 0 ? `+${currentChordDescription.transpose}` : currentChordDescription.transpose} st
          </div>
        </div>
      )}

      {recordedChords.length > 0 && (
        <div className="tonnetz-recorded">
          <h3>Recorded Stack</h3>
          <ol>
            {recordedChords.map((entry) => (
              <li key={entry.id}>
                <span className="tonnetz-recorded-chord">
                  {entry.chordName ?? entry.pitchClassNames.join(" ")}
                </span>
                {entry.aliases.length > 0 && (
                  <span className="tonnetz-recorded-aliases">
                    Aliases: {entry.aliases.join(", ")}
                  </span>
                )}
                <span className="tonnetz-recorded-notes">{entry.noteNames.join(", ")}</span>
                <span className="tonnetz-recorded-transpose">
                  Transpose: {entry.transpose > 0 ? `+${entry.transpose}` : entry.transpose} st
                </span>
              </li>
            ))}
          </ol>
        </div>
      )}
    </div>
  );
}

export default TonnetzWidget;
