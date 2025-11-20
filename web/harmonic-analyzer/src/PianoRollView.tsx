import { useMemo } from "react";
import type { ChangeEvent } from "react";

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

type FlattenedNote = {
  id: string;
  onset: number;
  duration: number;
  midi: number;
};

type TrimWindow = {
  startSeconds: number;
  endSeconds: number;
};

type BeatGrid = {
  tempoBpm: number;
  meterBeats: number;
  meterBeatUnit: number;
  startOffsetSeconds: number;
  measureStarts: number[];
  beatStarts: number[];
  subdivisionStarts: number[];
  subdivisionsPerBeat: number | null;
};

type PianoRollGridMode = "seconds" | "beat-subdivision";

type PianoRollViewProps = {
  events: RenderedChordEvent[];
  totalSeconds: number;
  sourceTotalSeconds: number;
  progress: number;
  isPlaying: boolean;
  onPlay: () => void;
  onStop: () => void;
  midiDownloadHref: string | null;
  beatGrid: BeatGrid | null;
  appliedTrimWindow: TrimWindow | null;
  pendingTrimWindow: TrimWindow | null;
  onPendingTrimWindowChange: (next: TrimWindow) => void;
  onApplyTrimWindow: () => void;
  onResetTrimWindow: () => void;
  gridMode: PianoRollGridMode;
  onGridModeChange: (mode: PianoRollGridMode) => void;
  snapToSubdivision: boolean;
  onSnapToSubdivisionChange: (value: boolean) => void;
  isApplyingTrim: boolean;
};

const MIN_TRIM_DURATION = 0.01;
const TRIM_EPSILON = 1e-4;

const normalisePitchClass = (value: number): number => ((value % 12) + 12) % 12;

const midiToNoteName = (midi: number): string => {
  const pitchClass = NOTE_NAMES[normalisePitchClass(midi)] ?? "?";
  const octave = Math.floor(midi / 12) - 1;
  return `${pitchClass}${octave}`;
};

const normalizeWindow = (window: TrimWindow, total: number): TrimWindow => {
  if (!Number.isFinite(total) || total <= 0) {
    return { startSeconds: 0, endSeconds: 0 };
  }
  const minDuration = Math.min(MIN_TRIM_DURATION, total);
  let start = Number.isFinite(window.startSeconds) ? window.startSeconds : 0;
  let end = Number.isFinite(window.endSeconds) ? window.endSeconds : total;
  start = Math.max(0, Math.min(start, total));
  end = Math.max(start + minDuration, Math.min(end, total));
  if (end > total) {
    end = total;
    start = Math.max(0, end - minDuration);
  }
  if (end - start < minDuration) {
    end = Math.min(total, start + minDuration);
  }
  return {
    startSeconds: start,
    endSeconds: end
  };
};

const formatSeconds = (value: number): string =>
  Number.isFinite(value) ? (value >= 10 ? value.toFixed(1) : value.toFixed(2)) : "0.00";

const toKey = (value: number): string => value.toFixed(6);

export default function PianoRollView({
  events,
  totalSeconds,
  sourceTotalSeconds,
  progress,
  isPlaying,
  onPlay,
  onStop,
  midiDownloadHref,
  beatGrid,
  appliedTrimWindow,
  pendingTrimWindow,
  onPendingTrimWindowChange,
  onApplyTrimWindow,
  onResetTrimWindow,
  gridMode,
  onGridModeChange,
  snapToSubdivision,
  onSnapToSubdivisionChange,
  isApplyingTrim
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
  const normalizedProgress = Math.min(1, Math.max(0, progress));
  const progressPosition = normalizedProgress * viewWidth;

  const sourceRange = sourceTotalSeconds > 0 ? sourceTotalSeconds : totalSeconds > 0 ? totalSeconds : 0;

  const normalizedApplied = normalizeWindow(
    appliedTrimWindow ?? { startSeconds: 0, endSeconds: sourceRange },
    sourceRange
  );
  const normalizedPending = normalizeWindow(
    pendingTrimWindow ?? appliedTrimWindow ?? { startSeconds: 0, endSeconds: sourceRange },
    sourceRange
  );

  const pendingDuration = normalizedPending.endSeconds - normalizedPending.startSeconds;
  const hasTrimChanges =
    Math.abs(normalizedPending.startSeconds - normalizedApplied.startSeconds) > TRIM_EPSILON ||
    Math.abs(normalizedPending.endSeconds - normalizedApplied.endSeconds) > TRIM_EPSILON;
  const disableApply = !hasTrimChanges || isApplyingTrim || sourceRange <= 0;
  const disableReset =
    appliedTrimWindow == null &&
    normalizedPending.startSeconds <= TRIM_EPSILON &&
    Math.abs(normalizedPending.endSeconds - sourceRange) <= TRIM_EPSILON;

  const beatLengthSeconds =
    beatGrid && beatGrid.tempoBpm > 0
      ? (() => {
          const beat = (60 / beatGrid.tempoBpm) * (4 / beatGrid.meterBeatUnit);
          return Number.isFinite(beat) && beat > 0 ? beat : null;
        })()
      : null;

  const sliderStepRaw =
    snapToSubdivision && beatLengthSeconds
      ? beatLengthSeconds /
        ((beatGrid?.subdivisionsPerBeat && beatGrid.subdivisionsPerBeat > 0 ? beatGrid.subdivisionsPerBeat : 1))
      : Math.max(0.01, Math.min(0.5, sourceRange > 0 ? sourceRange / 200 : 0.01));
  const sliderStep = Number.isFinite(sliderStepRaw) && sliderStepRaw > 0 ? sliderStepRaw : 0.01;
  const numberInputStep = snapToSubdivision && sliderStep > 0.01 ? sliderStep : 0.01;
  const minDuration = Math.min(MIN_TRIM_DURATION, sourceRange > 0 ? sourceRange : MIN_TRIM_DURATION);
  const sliderMax = Math.max(sourceRange, 0.01);

  const snapValueToGrid = (value: number): number => {
    if (!snapToSubdivision || !beatLengthSeconds) {
      return value;
    }
    const subdivisions =
      beatGrid?.subdivisionsPerBeat && beatGrid.subdivisionsPerBeat > 0 ? beatGrid.subdivisionsPerBeat : 1;
    const step = beatLengthSeconds / subdivisions;
    if (!Number.isFinite(step) || step <= 0) {
      return value;
    }
    return Math.round(value / step) * step;
  };

  const appliedDuration = Math.max(
    0.001,
    normalizedApplied.endSeconds - normalizedApplied.startSeconds
  );
  const toViewX = (seconds: number): number =>
    ((seconds - normalizedApplied.startSeconds) / appliedDuration) * viewWidth;

  const clampToView = (x: number): number => Math.max(0, Math.min(viewWidth, x));

  const appliedStartXRaw = toViewX(normalizedApplied.startSeconds);
  const appliedEndXRaw = toViewX(normalizedApplied.endSeconds);
  const pendingStartX = toViewX(normalizedPending.startSeconds);
  const pendingEndX = toViewX(normalizedPending.endSeconds);

  const appliedStartX = clampToView(appliedStartXRaw);
  const appliedEndX = clampToView(appliedEndXRaw);
  const pendingStartClamped = clampToView(pendingStartX);
  const pendingEndClamped = clampToView(pendingEndX);
  const pendingStartOutside = pendingStartX < 0 ? "left" : pendingStartX > viewWidth ? "right" : null;
  const pendingEndOutside = pendingEndX < 0 ? "left" : pendingEndX > viewWidth ? "right" : null;

  const showPendingMarkers =
    hasTrimChanges &&
    Number.isFinite(pendingStartX) &&
    Number.isFinite(pendingEndX) &&
    sourceRange > 0;

  const trimLabelY = 0.9;
  const trimLabelFontSize = 0.6;

  const applyPendingWindow = (next: TrimWindow) => {
    const normalized = normalizeWindow(next, sourceRange);
    onPendingTrimWindowChange(normalized);
  };

  const updateStart = (raw: number) => {
    if (!Number.isFinite(raw)) {
      return;
    }
    let startValue = snapValueToGrid(raw);
    startValue = Math.max(0, Math.min(startValue, sliderMax));
    let endValue = normalizedPending.endSeconds;
    if (startValue > endValue - minDuration) {
      endValue = Math.min(sliderMax, startValue + minDuration);
    }
    applyPendingWindow({
      startSeconds: startValue,
      endSeconds: endValue
    });
  };

  const updateEnd = (raw: number) => {
    if (!Number.isFinite(raw)) {
      return;
    }
    let endValue = snapValueToGrid(raw);
    endValue = Math.max(0, Math.min(endValue, sliderMax));
    let startValue = normalizedPending.startSeconds;
    if (endValue < startValue + minDuration) {
      startValue = Math.max(0, endValue - minDuration);
    }
    applyPendingWindow({
      startSeconds: startValue,
      endSeconds: endValue
    });
  };

  const handleStartRangeChange = (event: ChangeEvent<HTMLInputElement>) => {
    updateStart(Number(event.target.value));
  };

  const handleEndRangeChange = (event: ChangeEvent<HTMLInputElement>) => {
    updateEnd(Number(event.target.value));
  };

  const handleStartInputChange = (event: ChangeEvent<HTMLInputElement>) => {
    const nextValue = Number(event.target.value);
    if (Number.isNaN(nextValue)) {
      return;
    }
    updateStart(nextValue);
  };

  const handleEndInputChange = (event: ChangeEvent<HTMLInputElement>) => {
    const nextValue = Number(event.target.value);
    if (Number.isNaN(nextValue)) {
      return;
    }
    updateEnd(nextValue);
  };

  const secondsMarkers = useMemo(() => {
    if (gridMode !== "seconds") {
      return [] as Array<{ position: number; label: string }>;
    }
    const markers: Array<{ position: number; label: string }> = [];
    const total = Math.max(viewWidth, 1);
    const step = total > 8 ? 1 : total > 4 ? 0.5 : 0.25;
    for (let value = 0; value <= total + 1e-6; value += step) {
      const clamped = Number(value.toFixed(4));
      if (clamped > total + 1e-3) {
        break;
      }
      markers.push({ position: clamped, label: `${clamped.toFixed(2)}s` });
    }
    return markers;
  }, [gridMode, viewWidth]);

  const beatOverlay = useMemo(() => {
    if (gridMode !== "beat-subdivision" || !beatGrid || !beatLengthSeconds) {
      return null as
        | {
            measureMarkers: Array<{ position: number; measureNumber: number }>;
            beatMarkers: Array<{ position: number }>;
            subdivisionMarkers: number[];
          }
        | null;
    }
    const measureLength = beatLengthSeconds * (beatGrid.meterBeats > 0 ? beatGrid.meterBeats : 1);
    const measureMarkers = beatGrid.measureStarts.map((position, index) => {
      const globalValue = position + beatGrid.startOffsetSeconds;
      const measureNumber =
        measureLength > 0 && Number.isFinite(measureLength)
          ? Math.floor(globalValue / measureLength) + 1
          : index + 1;
      return { position, measureNumber };
    });
    const measureKeys = new Set(measureMarkers.map((marker) => toKey(marker.position)));
    const beatMarkers = beatGrid.beatStarts
      .filter((position) => !measureKeys.has(toKey(position)))
      .map((position) => ({ position }));
    const subdivisionMarkers = beatGrid.subdivisionStarts.filter(
      (position) => position >= 0 && position <= viewWidth
    );
    return {
      measureMarkers,
      beatMarkers,
      subdivisionMarkers
    };
  }, [beatGrid, beatLengthSeconds, gridMode, viewWidth]);

  const disablePlayback = notes.length === 0;
  const disableBeatGridToggle = !beatGrid || !beatLengthSeconds;

  const gridPatternOpacity = gridMode === "seconds" ? 0.3 : 0.12;

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
          <span>Loop {formatSeconds(totalSeconds)} s</span>
          <span>Source {formatSeconds(sourceRange)} s</span>
          <span>{Math.round(normalizedProgress * 100)}% progress</span>
        </div>
      </div>
      <div className="piano-roll-trim-summary">
        Trim window: {formatSeconds(normalizedPending.startSeconds)}s → {formatSeconds(normalizedPending.endSeconds)}s (
        {formatSeconds(pendingDuration)}s)
      </div>
      <div className="piano-roll-trim-controls">
        <div className="piano-roll-trim-row">
          <label htmlFor={`piano-roll-trim-start-${uniqueId}`}>
            Start
            <input
              id={`piano-roll-trim-start-${uniqueId}`}
              type="range"
              min={0}
              max={sliderMax}
              step={sliderStep}
              value={normalizedPending.startSeconds}
              onChange={handleStartRangeChange}
            />
          </label>
          <input
            className="piano-roll-trim-value"
            type="number"
            min={0}
            max={sliderMax}
            step={numberInputStep}
            value={normalizedPending.startSeconds.toFixed(3)}
            onChange={handleStartInputChange}
          />
        </div>
        <div className="piano-roll-trim-row">
          <label htmlFor={`piano-roll-trim-end-${uniqueId}`}>
            End
            <input
              id={`piano-roll-trim-end-${uniqueId}`}
              type="range"
              min={0}
              max={sliderMax}
              step={sliderStep}
              value={normalizedPending.endSeconds}
              onChange={handleEndRangeChange}
            />
          </label>
          <input
            className="piano-roll-trim-value"
            type="number"
            min={0}
            max={sliderMax}
            step={numberInputStep}
            value={normalizedPending.endSeconds.toFixed(3)}
            onChange={handleEndInputChange}
          />
        </div>
        <div className="piano-roll-trim-actions">
          <button type="button" onClick={onApplyTrimWindow} disabled={disableApply}>
            Apply Trim
          </button>
          <button type="button" onClick={onResetTrimWindow} disabled={disableReset}>
            Reset
          </button>
          <label className="piano-roll-trim-snap">
            <input
              type="checkbox"
              checked={snapToSubdivision && !disableBeatGridToggle}
              disabled={disableBeatGridToggle}
              onChange={(event) => onSnapToSubdivisionChange(event.target.checked)}
            />
            Snap to beat subdivision
          </label>
          <div className="piano-roll-grid-toggle">
            <button
              type="button"
              className={gridMode === "seconds" ? "active" : ""}
              onClick={() => onGridModeChange("seconds")}
            >
              Seconds
            </button>
            <button
              type="button"
              className={gridMode === "beat-subdivision" && !disableBeatGridToggle ? "active" : ""}
              onClick={() => onGridModeChange("beat-subdivision")}
              disabled={disableBeatGridToggle}
            >
              Beat Grid
            </button>
          </div>
        </div>
      </div>
      {notes.length === 0 && (
        <div className="status info">
          No notes in the current trim window. Adjust the start or end handles to include events.
        </div>
      )}
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
            opacity={gridPatternOpacity}
          />
          {gridMode === "seconds" &&
            secondsMarkers.map((marker) => (
              <g key={`seconds-marker-${marker.position.toFixed(3)}`}>
                <line
                  x1={marker.position}
                  x2={marker.position}
                  y1={0}
                  y2={viewHeight}
                  stroke="#e2e8f0"
                  strokeWidth={marker.position % 1 === 0 ? 0.05 : 0.02}
                />
                <text x={marker.position + 0.05} y={viewHeight - 0.4} fontSize={0.7} fill="#475569">
                  {marker.label}
                </text>
              </g>
            ))}
          {gridMode === "beat-subdivision" && beatOverlay && (
            <>
              {beatOverlay.subdivisionMarkers.map((position) => (
                <line
                  key={`subdivision-${toKey(position)}`}
                  x1={position}
                  x2={position}
                  y1={pitchPadding}
                  y2={pitchPadding + pitchRange}
                  stroke="#dbeafe"
                  strokeWidth={0.02}
                />
              ))}
              {beatOverlay.beatMarkers.map((marker) => (
                <line
                  key={`beat-${toKey(marker.position)}`}
                  x1={marker.position}
                  x2={marker.position}
                  y1={pitchPadding}
                  y2={pitchPadding + pitchRange}
                  stroke="#94a3b8"
                  strokeWidth={0.04}
                />
              ))}
              {beatOverlay.measureMarkers.map((marker) => (
                <g key={`measure-${toKey(marker.position)}`}>
                  <line
                    x1={marker.position}
                    x2={marker.position}
                    y1={0}
                    y2={viewHeight}
                    stroke="#1d4ed8"
                    strokeWidth={0.06}
                  />
                  <text
                    x={marker.position + 0.05}
                    y={viewHeight - 0.6}
                    fontSize={0.7}
                    fill="#1d4ed8"
                  >
                    M{marker.measureNumber}
                  </text>
                </g>
              ))}
            </>
          )}
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
          <g>
            <line
              x1={appliedStartX}
              x2={appliedStartX}
              y1={0}
              y2={viewHeight}
              stroke="#2563eb"
              strokeWidth={0.06}
            />
            <text
              x={appliedStartX + 0.12}
              y={trimLabelY}
              fontSize={trimLabelFontSize}
              fill="#2563eb"
            >
              Start {formatSeconds(normalizedApplied.startSeconds)}s
            </text>
            <line
              x1={appliedEndX}
              x2={appliedEndX}
              y1={0}
              y2={viewHeight}
              stroke="#2563eb"
              strokeWidth={0.06}
            />
            <text
              x={appliedEndX - 0.12}
              y={trimLabelY}
              fontSize={trimLabelFontSize}
              fill="#2563eb"
              textAnchor="end"
            >
              End {formatSeconds(normalizedApplied.endSeconds)}s
            </text>
            {showPendingMarkers && (
              <>
                <line
                  x1={pendingStartClamped}
                  x2={pendingStartClamped}
                  y1={0}
                  y2={viewHeight}
                  stroke="#ec4899"
                  strokeDasharray="0.2 0.2"
                  strokeWidth={0.05}
                />
                <line
                  x1={pendingEndClamped}
                  x2={pendingEndClamped}
                  y1={0}
                  y2={viewHeight}
                  stroke="#ec4899"
                  strokeDasharray="0.2 0.2"
                  strokeWidth={0.05}
                />
                <text
                  x={
                    pendingStartClamped +
                    (pendingStartOutside === "right" ? -0.12 : 0.12)
                  }
                  y={trimLabelY + 0.7}
                  fontSize={trimLabelFontSize}
                  fill="#ec4899"
                  textAnchor={pendingStartOutside === "right" ? "end" : "start"}
                >
                  New start {formatSeconds(normalizedPending.startSeconds)}s
                </text>
                <text
                  x={
                    pendingEndClamped +
                    (pendingEndOutside === "right" ? -0.12 : 0.12)
                  }
                  y={trimLabelY + 1.4}
                  fontSize={trimLabelFontSize}
                  fill="#ec4899"
                  textAnchor={pendingEndOutside === "right" ? "end" : "start"}
                >
                  New end {formatSeconds(normalizedPending.endSeconds)}s
                </text>
                {pendingStartOutside && (
                  <polygon
                    points={
                      pendingStartOutside === "left"
                        ? `0,0.45 0.35,0.75 0,1.05`
                        : `${viewWidth},0.45 ${viewWidth - 0.35},0.75 ${viewWidth},1.05`
                    }
                    fill="#ec4899"
                    opacity={0.75}
                  />
                )}
                {pendingEndOutside && (
                  <polygon
                    points={
                      pendingEndOutside === "left"
                        ? `0,1.45 0.35,1.75 0,2.05`
                        : `${viewWidth},1.45 ${viewWidth - 0.35},1.75 ${viewWidth},2.05`
                    }
                    fill="#ec4899"
                    opacity={0.75}
                  />
                )}
              </>
            )}
          </g>
          <line
            x1={progressPosition}
            x2={progressPosition}
            y1={0}
            y2={viewHeight}
            stroke="#ef4444"
            strokeWidth={0.08}
          />
        </svg>
      </div>
    </div>
  );
}
