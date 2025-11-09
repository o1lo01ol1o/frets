import { useMemo } from "react";

const NOTE_NAMES: readonly string[] = [
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

type RenderedChordEvent = {
  onsetSeconds: number;
  durationSeconds: number;
  midiNotes: number[];
};

type PianoRollViewProps = {
  events: RenderedChordEvent[];
  totalSeconds: number;
  progress: number;
  isPlaying: boolean;
  onPlay: () => void;
  onStop: () => void;
  midiDownloadHref: string | null;
};

type FlattenedNote = {
  id: string;
  onset: number;
  duration: number;
  midi: number;
};

const normalisePitchClass = (value: number): number => ((value % 12) + 12) % 12;

const midiToNoteName = (midi: number): string => {
  const pitchClass = NOTE_NAMES[normalisePitchClass(midi)] ?? "?";
  const octave = Math.floor(midi / 12) - 1;
  return `${pitchClass}${octave}`;
};

export default function PianoRollView({
  events,
  totalSeconds,
  progress,
  isPlaying,
  onPlay,
  onStop,
  midiDownloadHref
}: PianoRollViewProps) {
  const uniqueId = useMemo(() => Math.random().toString(36).slice(2, 10), []);
  const gridPatternId = `piano-roll-grid-${uniqueId}`;
  const backgroundGradientId = `piano-roll-bg-${uniqueId}`;
  const notes = useMemo<FlattenedNote[]>(() => {
    const flattened: FlattenedNote[] = [];
    events.forEach((event, eventIndex) => {
      const onset = Math.max(0, event.onsetSeconds);
      const duration = Math.max(0.05, event.durationSeconds);
      event.midiNotes.forEach((midi, noteIndex) => {
        flattened.push({
          id: `${eventIndex}-${noteIndex}-${midi}`,
          midi,
          onset,
          duration
        });
      });
    });
    return flattened;
  }, [events]);

  const { minMidi, maxMidi } = useMemo(() => {
    if (notes.length === 0) {
      return { minMidi: 60, maxMidi: 72 };
    }
    return notes.reduce(
      (acc, note) => ({
        minMidi: Math.min(acc.minMidi, note.midi),
        maxMidi: Math.max(acc.maxMidi, note.midi)
      }),
      { minMidi: notes[0]!.midi, maxMidi: notes[0]!.midi }
    );
  }, [notes]);

  const pitchPadding = 2;
  const pitchRange = Math.max(1, maxMidi - minMidi + 1);
  const viewHeight = pitchRange + pitchPadding * 2;
  const viewWidth = Math.max(1, totalSeconds > 0 ? totalSeconds : 1);
  const progressPosition = Math.min(1, Math.max(0, progress)) * viewWidth;

  const timeMarkers = useMemo(() => {
    const markers: number[] = [];
    const total = Math.max(viewWidth, 1);
    const step = total > 8 ? 1 : total > 4 ? 0.5 : 0.25;
    let value = 0;
    while (value <= total + 0.0001) {
      markers.push(Number(value.toFixed(3)));
      value += step;
    }
    return markers;
  }, [viewWidth]);

  const disablePlayback = notes.length === 0;

  return (
    <div className="piano-roll">
      <div className="piano-roll-toolbar">
        <div className="piano-roll-controls">
          <button type="button" onClick={onPlay} disabled={disablePlayback || isPlaying}>
            Play Loop
          </button>
          <button type="button" onClick={onStop} disabled={!isPlaying}>
            Stop Loop
          </button>
          {midiDownloadHref && (
            <a className="piano-roll-download" href={midiDownloadHref} download="tonnetz-loop.mid">
              Download MIDI
            </a>
          )}
        </div>
        <div className="piano-roll-meta">
          <span>{notes.length} notes</span>
          <span>{viewWidth.toFixed(2)} s total</span>
          <span>{Math.round(Math.max(0, Math.min(1, progress)) * 100)}% progress</span>
        </div>
      </div>
      <div className="piano-roll-canvas-wrapper">
        <svg className="piano-roll-svg" viewBox={`0 0 ${viewWidth} ${viewHeight}`} preserveAspectRatio="none">
          <defs>
            <pattern id={gridPatternId} width={1} height={1} patternUnits="userSpaceOnUse">
              <path d={`M0 0 L0 ${viewHeight}`} stroke="#cbd5f5" strokeWidth={0.02} />
            </pattern>
            <linearGradient id={backgroundGradientId} x1="0" y1="0" x2="0" y2="1">
              <stop offset="0%" stopColor="#f8fafc" />
              <stop offset="100%" stopColor="#eef2ff" />
            </linearGradient>
          </defs>
          <rect
            x={0}
            y={0}
            width={viewWidth}
            height={viewHeight}
            fill={`url(#${backgroundGradientId})`}
            rx={1.5}
            ry={1.5}
          />
          <rect
            x={0}
            y={pitchPadding}
            width={viewWidth}
            height={pitchRange}
            fill={`url(#${gridPatternId})`}
            opacity={0.3}
          />
          {timeMarkers.map((marker) => (
            <g key={`marker-${marker}`}>
              <line
                x1={marker}
                x2={marker}
                y1={0}
                y2={viewHeight}
                stroke="#e2e8f0"
                strokeWidth={marker % 1 === 0 ? 0.04 : 0.02}
              />
              <text
                x={marker + 0.05}
                y={viewHeight - 0.4}
                fontSize={0.7}
                fill="#475569"
              >
                {marker.toFixed(2)}s
              </text>
            </g>
          ))}
          {notes.map((note) => {
            const y = pitchPadding + (maxMidi - note.midi);
            const height = 0.8;
            const width = Math.max(note.duration, 0.05);
            return (
              <g key={note.id}>
                <rect
                  x={note.onset}
                  y={y - height / 2}
                  width={width}
                  height={height}
                  rx={0.2}
                  ry={0.2}
                  fill="#38bdf8"
                  opacity={0.85}
                />
                {width > 0.3 && (
                  <text
                    x={note.onset + width / 2}
                    y={y}
                    fontSize={0.55}
                    textAnchor="middle"
                    dominantBaseline="middle"
                    fill="#0f172a"
                  >
                    {midiToNoteName(note.midi)}
                  </text>
                )}
              </g>
            );
          })}
          <line x1={progressPosition} x2={progressPosition} y1={0} y2={viewHeight} stroke="#ef4444" strokeWidth={0.08} />
        </svg>
      </div>
    </div>
  );
}
