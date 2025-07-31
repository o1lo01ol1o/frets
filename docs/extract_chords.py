#!/usr/bin/env python3
"""
Extract chords from MIDI file using music21 for testing harmonic analysis.
"""

import sys
from music21 import converter, chord, key, roman, stream, interval, pitch
from music21.chord import Chord as Music21Chord
from music21.harmony import ChordSymbol
import json

def midi_note_to_pitch_class(midi_note):
    """Convert MIDI note number to pitch class name."""
    pitch_classes = ['C', 'Cs', 'D', 'Ds', 'E', 'F', 'Fs', 'G', 'Gs', 'A', 'As', 'B']
    return pitch_classes[midi_note % 12]

def analyze_chord_quality(chord_obj):
    """Analyze a music21 chord and extract quality information."""
    if not isinstance(chord_obj, Music21Chord):
        return None

    # Skip intervals (less than 3 notes)
    if len(chord_obj.pitches) < 3:
        return None

    # Get the chord symbol representation
    try:
        # Get chord symbol using music21's chord symbol functionality
        try:
            chord_symbol = chord_obj.pitchedCommonName
        except:
            chord_symbol = str(chord_obj)

        root_name = chord_obj.root().name

        # Convert sharps to our notation
        root_name = root_name.replace('#', 's').replace('-', 'b')

        # Determine quality
        quality = None
        seventh = None
        additions = []

        # Basic quality detection
        if chord_obj.isMajorTriad():
            quality = "Maj"
        elif chord_obj.isMinorTriad():
            quality = "Min"
        elif chord_obj.isDiminishedTriad():
            quality = "Dim"
        elif chord_obj.isAugmentedTriad():
            quality = "Aug"

        # Check for seventh - using safer methods
        if chord_obj.seventh is not None:
            # Analyze seventh quality based on interval
            try:
                seventh_interval = chord_obj.seventh
                if hasattr(seventh_interval, 'semitones'):
                    semitones = seventh_interval.semitones
                elif hasattr(seventh_interval, 'cents'):
                    semitones = seventh_interval.cents / 100
                else:
                    semitones = 0

                # Classify seventh based on semitones from root
                if abs(semitones - 11) < 0.5:  # Major 7th
                    seventh = "Maj7"
                elif abs(semitones - 10) < 0.5:  # Minor 7th
                    if quality == "Maj":
                        seventh = "Dom7"
                    else:
                        seventh = "Min7"
                elif abs(semitones - 9) < 0.5:  # Diminished 7th
                    seventh = "Dim7"
            except:
                # Fallback: analyze by chord type names
                chord_name = str(chord_obj).lower()
                if 'major-seventh' in chord_name or 'maj7' in chord_name:
                    seventh = "Maj7"
                elif 'dominant-seventh' in chord_name or 'dom7' in chord_name:
                    seventh = "Dom7"
                elif 'minor-seventh' in chord_name or 'min7' in chord_name:
                    seventh = "Min7"
                elif 'diminished-seventh' in chord_name or 'dim7' in chord_name:
                    seventh = "Dim7"

        # Get all pitch classes in the chord
        pitch_classes = [p.pitchClass for p in chord_obj.pitches]

        return {
            'root': root_name,
            'quality': quality,
            'seventh': seventh,
            'pitches': pitch_classes,
            'chord_symbol': chord_symbol,
            'notes': [p.name.replace('#', 's').replace('-', 'b') for p in chord_obj.pitches]
        }

    except Exception as e:
        print(f"Error analyzing chord: {e}", file=sys.stderr)
        return None

def extract_chords_from_midi(midi_path):
    """Extract chords from MIDI file."""
    try:
        # Load the MIDI file
        score = converter.parse(midi_path)
        print(f"Loaded MIDI file: {midi_path}")
        print(f"Duration: {score.duration.quarterLength} quarter notes")

        # Flatten the score to get all notes in sequence
        flat_score = score.flatten()

        # Extract chords using music21's chord detection
        chords_found = []

        # Method 1: Look for existing chord objects
        for element in flat_score:
            if isinstance(element, Music21Chord):
                chord_info = analyze_chord_quality(element)
                if chord_info:
                    chord_info['offset'] = float(element.offset)
                    chord_info['duration'] = float(element.duration.quarterLength)
                    chords_found.append(chord_info)

        # Method 2: If no chords found, try chord analysis on segments
        if not chords_found:
            print("No explicit chords found, attempting harmonic analysis...")

            # Analyze harmony using music21's built-in analysis
            try:
                # Get key signature
                key_obj = score.analyze('key')
                print(f"Detected key: {key_obj}")

                # Chordify the score (combine simultaneous notes into chords)
                chordified = score.chordify()

                for chord_obj in chordified.flatten().getElementsByClass(Music21Chord):
                    chord_info = analyze_chord_quality(chord_obj)
                    if chord_info:
                        chord_info['offset'] = float(chord_obj.offset)
                        chord_info['duration'] = float(chord_obj.duration.quarterLength)
                        chords_found.append(chord_info)

            except Exception as e:
                print(f"Harmonic analysis failed: {e}", file=sys.stderr)

        # Method 3: If still no chords, analyze note combinations manually
        if not chords_found:
            print("Attempting manual chord detection from simultaneous notes...")

            # Group notes by onset time with a small tolerance for timing
            notes_by_time = {}
            tolerance = 0.1  # Small timing tolerance

            for note in flat_score.notes:
                offset = float(note.offset)
                # Find if there's a nearby time bucket
                found_bucket = None
                for existing_offset in notes_by_time.keys():
                    if abs(offset - existing_offset) <= tolerance:
                        found_bucket = existing_offset
                        break

                if found_bucket is not None:
                    offset = found_bucket
                elif offset not in notes_by_time:
                    notes_by_time[offset] = []

                if hasattr(note, 'pitches'):  # It's a chord
                    notes_by_time[offset].extend(note.pitches)
                else:  # It's a single note
                    notes_by_time[offset].append(note.pitch)

            # Create chords from simultaneous notes (3+ notes only)
            for offset, pitches in notes_by_time.items():
                if len(pitches) >= 3:  # At least 3 notes for a chord
                    try:
                        # Remove duplicate pitches
                        unique_pitches = list(set(pitches))
                        if len(unique_pitches) >= 3:
                            chord_obj = Music21Chord(unique_pitches)
                            chord_info = analyze_chord_quality(chord_obj)
                            if chord_info:
                                chord_info['offset'] = offset
                                chord_info['duration'] = 1.0  # Default duration
                                chords_found.append(chord_info)
                    except:
                        continue

        return sorted(chords_found, key=lambda x: x['offset'])

    except Exception as e:
        print(f"Error processing MIDI file: {e}", file=sys.stderr)
        return []

def format_for_haskell(chords):
    """Format chords for use in Haskell tests."""
    haskell_chords = []

    for chord_info in chords:
        root = chord_info['root']
        quality = chord_info.get('quality', 'Maj')
        seventh = chord_info.get('seventh')

        # Convert to Haskell constructor format
        haskell_chord = f"Chord {root} Nothing"

        if quality:
            haskell_chord += f" (Just {quality})"
        else:
            haskell_chord += " Nothing"

        if seventh:
            haskell_chord += f" (Just {seventh})"
        else:
            haskell_chord += " Nothing"

        # Add remaining fields (simplified for now)
        haskell_chord += " Nothing Nothing Nothing Nothing Nothing Nothing Nothing"

        haskell_chords.append(haskell_chord)

    return haskell_chords

def main():
    midi_file = "rubato-src-20070720/test/sonata-first-sentence-only.mid"

    print("Extracting chords from MIDI file...")
    print("=" * 50)

    chords = extract_chords_from_midi(midi_file)

    if not chords:
        print("No chords found in the MIDI file.")
        return

    # Filter to only include actual chords (3+ notes)
    actual_chords = [c for c in chords if len(c.get('notes', [])) >= 3]

    print(f"\nFound {len(actual_chords)} actual chords (3+ notes):")
    print("=" * 50)

    # Print detailed chord information
    for i, chord_info in enumerate(actual_chords):
        print(f"Chord {i+1}:")
        print(f"  Offset: {chord_info['offset']:.2f}")
        print(f"  Duration: {chord_info.get('duration', 'N/A'):.2f}")
        print(f"  Root: {chord_info['root']}")
        print(f"  Quality: {chord_info.get('quality', 'Unknown')}")
        print(f"  Seventh: {chord_info.get('seventh', 'None')}")
        print(f"  Notes: {chord_info.get('notes', [])}")
        print(f"  Chord Symbol: {chord_info.get('chord_symbol', 'N/A')}")
        print()

    # Generate Haskell format (only for actual chords)
    if actual_chords:
        print("Haskell format:")
        print("=" * 50)
        haskell_chords = format_for_haskell(actual_chords)

        print("testSonataChords :: [Chord]")
        print("testSonataChords =")
        print("  [ " + ",\n    ".join(haskell_chords) + "\n  ]")
    else:
        print("No actual chords (3+ notes) found for Haskell format.")

    # Save to JSON for further processing
    with open("extracted_chords.json", "w") as f:
        json.dump(actual_chords, f, indent=2)

    print(f"\nDetailed chord data saved to: extracted_chords.json")

if __name__ == "__main__":
    main()
