import { useEffect, useMemo, useRef, useState } from "react";

export type OverlayMarker = {
  string: number;
  fret: number;
  label?: string;
  group?: string;
  color?: string;
  outlineColor?: string;
  strokeWidth?: number;
};

export type FretboardOverlaySet = {
  id: string;
  notes?: string[];
  positions?: Array<{
    string: number;
    fret: number;
    finger: string | null;
    pitchClass: string;
    pitchClassNumber: number;
    octave?: number | null;
    noteName?: string;
    label?: string | null;
  }>;
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

export const DEFAULT_FRET_COUNT = 12;
export const DEFAULT_CHORD_COLOR = "#ef4444";
export const DEFAULT_SCALE_DIFF_OUTLINE_COLOR = "#FDE725";
export const DEFAULT_SCALE_DIFF_STROKE_WIDTH = 3;

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

const normalisePitchClass = (value: number): number => ((value % 12) + 12) % 12;

const normaliseUrl = (raw: string): string => raw.replace(/\/+$/, "");

type SerializedTuningEntry = string | number | { pitch: string; octave?: number };

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

const serializeTuningValue = (entry: string): SerializedTuningEntry => {
  const { pitch, octave } = splitPitchAndOctave(entry);
  if (octave !== undefined) {
    return { pitch, octave };
  }
  return { pitch };
};

export const buildMarkersForPositions = (
  tuning: string[],
  positions: Array<{
    string: number;
    fret: number;
    finger: string | null;
    pitchClass: string;
    noteName?: string;
    label?: string | null;
  }>,
  group: string,
  color?: string
): OverlayMarker[] => {
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
};

const mapBackendStringToFretboard = (stringIndex: number, stringCount: number): number => {
  const candidate = stringCount - stringIndex;
  return Math.max(1, Math.min(stringCount, candidate));
};

type FretboardInstance = {
  setDots: (markers: OverlayMarker[]) => FretboardInstance;
  render: (markers?: OverlayMarker[]) => FretboardInstance;
  style: (...args: unknown[]) => unknown;
};

type FretboardConstructor = new (options: Record<string, unknown>) => FretboardInstance;

let fretboardCtorPromise: Promise<FretboardConstructor | null> | null = null;

const loadFretboard = async (): Promise<FretboardConstructor | null> => {
  if (!fretboardCtorPromise) {
    fretboardCtorPromise = (async () => {
      try {
        const module = await import("@moonwave99/fretboard.js");
        const ctor =
          (module && "default" in module ? (module as { default: unknown }).default : undefined) ??
          (module as { Fretboard?: unknown }).Fretboard ??
          module;
        return typeof ctor === "function" ? (ctor as FretboardConstructor) : null;
      } catch (error) {
        console.error("Failed to load Fretboard.js", error);
        return null;
      }
    })();
  }
  try {
    return await fretboardCtorPromise;
  } catch (error) {
    console.error("Failed to load Fretboard.js", error);
    fretboardCtorPromise = null;
    return null;
  }
};

const renderFretboardDots = (instance: FretboardInstance, markers: OverlayMarker[]) => {
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
};

type FretboardOverlayDiagramProps = {
  tuning: string[];
  overlays: FretboardOverlaySet[];
  serverUrl: string;
};

const useSerializedTuning = (tuning: string[]): SerializedTuningEntry[] =>
  useMemo(() => tuning.map((value) => serializeTuningValue(value)), [tuning]);

const useNoteSetPayload = (overlays: FretboardOverlaySet[]) =>
  useMemo(
    () =>
      overlays
        .filter(
          (overlay) =>
            (Array.isArray(overlay.notes) && overlay.notes.length > 0) ||
            (Array.isArray(overlay.pitchClassNumbers) && overlay.pitchClassNumbers.length > 0)
        )
        .map((overlay) => {
          const base: Record<string, unknown> = {
            id: overlay.id,
            label: overlay.label ?? null
          };
          if (overlay.pitchClassNumbers && overlay.pitchClassNumbers.length > 0) {
            const uniquePitchClasses = Array.from(
              new Set(overlay.pitchClassNumbers.map((value) => normalisePitchClass(Number(value))))
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
    [overlays]
  );

const useOverlaySignature = (overlays: FretboardOverlaySet[]) =>
  useMemo(
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

const createColorMap = (values: number[]): Map<number, string> => {
  const unique = Array.from(new Set(values.map((value) => ((value % 12) + 12) % 12)));
  const map = new Map<number, string>();
  unique.forEach((pitchClass, idx) => {
    map.set(pitchClass, VIRIDIS_COLORS[idx % VIRIDIS_COLORS.length]!);
  });
  return map;
};

const extractMarkersFromOccurrences = (
  overlays: FretboardOverlaySet[],
  noteSets: BackendFretboardOccurrenceSet[],
  chordNames: Record<string, BackendChordNameEntry>,
  stringCount: number
): OverlayMarker[] => {
  const overlayMap = new Map(overlays.map((overlay) => [overlay.id, overlay]));
  const colorMapCache = new Map<string, Map<number, string>>();

  return noteSets.flatMap((noteSet) => {
    const overlayInfo = overlayMap.get(noteSet.id);
    const groupId = overlayInfo?.id ?? noteSet.id;
    const baseColor = overlayInfo?.color;
    const useViridis = Boolean(overlayInfo?.scaleMode || overlayInfo?.scaleRoot);
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

    let colorMap: Map<number, string> | null = null;
    if (useViridis) {
      const cacheKey = JSON.stringify(noteSet.pitchClasses ?? overlayInfo?.pitchClassNumbers ?? []);
      if (colorMapCache.has(cacheKey)) {
        colorMap = colorMapCache.get(cacheKey)!;
      } else {
        const sourceValues =
          noteSet.pitchClasses && noteSet.pitchClasses.length > 0
            ? noteSet.pitchClasses
            : overlayInfo?.pitchClassNumbers ?? [];
        colorMap = createColorMap(sourceValues);
        colorMapCache.set(cacheKey, colorMap);
      }
    }

    return noteSet.occurrences.map((occurrence) => {
      const stringNumber = mapBackendStringToFretboard(occurrence.string, stringCount);
      const pitchClassNumber = normalisePitchClass(occurrence.pitchClass ?? 0);
      const fillColor = useOutline
        ? "transparent"
        : useViridis
        ? colorMap?.get(pitchClassNumber) ?? baseColor
        : baseColor;
      const markerGroup = useViridis && !useOutline ? `${groupId}-${pitchClassNumber}` : groupId;
      const markerLabel =
        useViridis && (occurrence.noteName || occurrence.pitchClassName)
          ? occurrence.noteName ?? occurrence.pitchClassName
          : overlayLabel ?? occurrence.noteName ?? occurrence.pitchClassName;

      return {
        string: stringNumber,
        fret: occurrence.fret ?? 0,
        label: markerLabel,
        group: markerGroup,
        color: fillColor ?? "#0ea5e9",
        outlineColor: useOutline ? outlineColor : undefined,
        strokeWidth: useOutline ? strokeWidth : undefined
      } satisfies OverlayMarker;
    });
  });
};

const FretboardOverlayDiagram = ({ tuning, overlays, serverUrl }: FretboardOverlayDiagramProps) => {
  const containerRef = useRef<HTMLDivElement | null>(null);
  const fretboardInstanceRef = useRef<FretboardInstance | null>(null);
  const [backendMarkers, setBackendMarkers] = useState<OverlayMarker[]>([]);
  const [fetchError, setFetchError] = useState<string | null>(null);
  const [isLoading, setLoading] = useState<boolean>(false);
  const [chordNames, setChordNames] = useState<Record<string, BackendChordNameEntry>>({});
  const [chordNameError, setChordNameError] = useState<string | null>(null);
  const [isChordLoading, setChordLoading] = useState<boolean>(false);
  const lastRequestRef = useRef<string | null>(null);
  const chordRequestRef = useRef<string | null>(null);

  const serializedTuning = useSerializedTuning(tuning);
  const tuningSignature = useMemo(() => JSON.stringify(serializedTuning), [serializedTuning]);
  const noteSetPayload = useNoteSetPayload(overlays);
  const noteSetSignature = useMemo(() => JSON.stringify(noteSetPayload), [noteSetPayload]);
  const overlaySignature = useOverlaySignature(overlays);
  const maxFrets = useMemo(
    () =>
      Math.max(
        DEFAULT_FRET_COUNT,
        ...overlays
          .map((overlay) => overlay.maxFret)
          .filter((value): value is number => typeof value === "number")
      ),
    [overlays]
  );
  const diagramSignature = useMemo(
    () => JSON.stringify({ tuningSignature, maxFrets }),
    [tuningSignature, maxFrets]
  );

  useEffect(() => {
    setBackendMarkers([]);
  }, [overlaySignature]);

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

    (async () => {
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
        if (controller.signal.aborted) {
          setChordLoading(false);
          chordRequestRef.current = null;
          return;
        }
        setChordNames({});
        setChordLoading(false);
        chordRequestRef.current = null;
        setChordNameError(err instanceof Error ? err.message : "Unable to resolve chord names.");
      }
    })();

    return () => {
      controller.abort();
      setChordLoading(false);
      chordRequestRef.current = null;
    };
  }, [serverUrl, noteSetSignature, noteSetPayload]);

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
      maxFrets: Math.max(
        DEFAULT_FRET_COUNT,
        ...overlays
          .map((overlay) => overlay.maxFret)
          .filter((value): value is number => typeof value === "number")
      ),
      noteSets: noteSetPayload
    };

    const signature = JSON.stringify({ server: endpointBase, requestBody });
    if (signature === lastRequestRef.current) {
      return () => controller.abort();
    }
    lastRequestRef.current = signature;

    (async () => {
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
        const markers = extractMarkersFromOccurrences(
          overlays,
          payload.noteSets ?? [],
          chordNames,
          tuning.length
        );
        setBackendMarkers(markers);
        setLoading(false);
      } catch (err) {
        if (controller.signal.aborted) {
          setLoading(false);
          lastRequestRef.current = null;
          return;
        }
        setBackendMarkers([]);
        setLoading(false);
        lastRequestRef.current = null;
        setFetchError(err instanceof Error ? err.message : "Unable to fetch fretboard occurrences.");
      }
    })();

    return () => {
      controller.abort();
      setLoading(false);
      lastRequestRef.current = null;
    };
  }, [serverUrl, noteSetSignature, overlays, chordNames, serializedTuning]);

  const directMarkers = useMemo(() => {
    return overlays.flatMap((overlay) => {
      if (overlay.positions && overlay.positions.length > 0) {
        return buildMarkersForPositions(tuning, overlay.positions, overlay.id, overlay.color);
      }
      return [];
    });
  }, [overlays, tuning]);

  const combinedMarkers = useMemo(
    () => [...backendMarkers, ...directMarkers],
    [backendMarkers, directMarkers]
  );
  const combinedMarkersRef = useRef<OverlayMarker[]>([]);

  useEffect(() => {
    combinedMarkersRef.current = combinedMarkers;
  }, [combinedMarkers]);

  useEffect(() => {
    let cancelled = false;

    const initialise = async () => {
      const container = containerRef.current;
      if (!container) {
        return;
      }
      const ctor = await loadFretboard();
      if (cancelled || !ctor || !containerRef.current) {
        return;
      }
      containerRef.current.innerHTML = "";
      try {
        const instance = new ctor({
          el: containerRef.current,
          stringCount: tuning.length,
          tuning,
          fretCount: maxFrets,
          width: containerRef.current.clientWidth || 720,
          height: 240,
          dotSize: 18,
          dotTextSize: 11,
          dotStrokeColor: "#1f2937",
          dotFill: "#ffffff"
        });
        fretboardInstanceRef.current = instance;
        renderFretboardDots(instance, combinedMarkersRef.current);
      } catch (error) {
        console.error("Failed to initialise fretboard diagram", error);
      }
    };

    initialise().catch((error) => console.error("Failed to initialise fretboard diagram", error));

    return () => {
      cancelled = true;
      fretboardInstanceRef.current = null;
      if (containerRef.current) {
        containerRef.current.innerHTML = "";
      }
    };
  }, [diagramSignature, maxFrets, tuning]);

  useEffect(() => {
    const instance = fretboardInstanceRef.current;
    if (!instance) {
      return;
    }
    renderFretboardDots(instance, combinedMarkers);
  }, [combinedMarkers]);

  return (
    <div className="fretboard-wrapper">
      {fetchError && <div className="status error">{fetchError}</div>}
      {!fetchError && isLoading && <div className="status">Loading overlays…</div>}
      {chordNameError && <div className="status error">{chordNameError}</div>}
      {isChordLoading && chordNameError === null && <div className="status">Identifying chord…</div>}
      <div className="fretboard-container" ref={containerRef} />
    </div>
  );
};

export default FretboardOverlayDiagram;
