import json
from collections import defaultdict

weights = {
    (0,0): [1,0,0,0,1,0,0,1,0,0,0,0],
    (0,1): [1,0,0,0,0,1,0,0,0,1,0,0],
    (0,2): [0,0,1,0,0,0,0,1,0,0,0,1],
    (1,0): [1,0,0,1,0,0,0,1,0,0,0,0],
    (1,1): [1,0,0,0,0,1,0,0,1,0,0,0],
    (1,2): [0,0,1,0,0,0,0,1,0,0,0,1],
}

def contributions(chords):
    totals = defaultdict(int)
    for chord in chords:
        pitches = {p % 12 for p in chord['pitches']}
        for mode in [0,1]:
            for func in [0,1,2]:
                row = mode*3 + func
                table = weights[(mode,func)]
                for col in range(12):
                    cell = sum(table[(p - col) % 12] for p in pitches)
                    totals[(row,col)] += cell
    return totals

with open('docs/extracted_chords.json') as f:
    chords = json.load(f)

print('Chord pitch sets:', [sorted({p%12 for p in ch['pitches']}) for ch in chords])
for k,v in sorted(contributions(chords).items(), key=lambda kv: kv[1], reverse=True)[:12]:
    print(k, v)
