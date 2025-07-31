# A Harmonic Analysis Network

**Authors:** Ethem Alpaydin and Guerino Mazzola

**Journal:** Musicæ Scientiæ
**Volume:** 19(2)
**Pages:** 192-214
**Year:** 2015

## Introduction

These are the steps of a music theorist's analysis with the score in front of their eyes and the music in their ears.

The harmonic motion, created by a chords' function in a tonality, has an emotional projection: the tension that rises or falls, suspends or resolves, is perceived by listeners: a rise in tension raises an expectation for resolution, whereas a resolution is a decrease in harmonic tension. The fine balance between increase and decrease in tension is a critical component in the aesthetics of harmonic language.

### Functional Harmony and Models of Tonality

Hugo Riemann, a 19th century music theorist, established the basis for functional harmonic analysis. Riemann considered minor and major third intervals as duals of each other and he based his tonality concept on chords made out of major and minor thirds, which he named as primary triads. His depiction of the relationship between tones and tonalities can be seen in Figure 1 (Riemann, 1992). This is an original tonality model.

**Figure 1:** Riemann's visualization of tonalities' relationship.

Neo-Riemannian theory extended Riemann's model assuming equal temperament (and thus enharmonic equivalence) and octave equivalence. In this case, the infinite surface in Figure 1 is turned into a five-dimensional torus. Neo-Riemannian theory relates harmonies based on transformations that form an algebraic group (Iber, 1995). Though Riemann himself thought about these logical connections between chords, his thinking was based on just-tuning. More recently, models of tonality have used other visual analogies (Chew, 2001; Krumhansl, 1990).

### Literature on AHA

As a music theory practice, the extent and content of harmony analysis in tonal music is not precisely defined. For this reason, AHA tools also have different means and ends. A lot of harmonic analysis-related research concentrates on key-finding only, where the tonality of a music segment (monophonic or polyphonic) is found. When we consider all key-finding research, some consider modulation as well, whereas others presume mono-key segments. Since segmentation and harmonic analysis are not separate processes, key-finding in segmented/mono-key music is an easier problem.

AHA has been an important area of research with the growing amount of online digital audio and music. AHA can be useful in a number of ways listening to and enjoying music is an a music segment (monophonic or polyphonic) is found.

One important problem to overcome in "parsing" harmony is prolongation. Prolongation corresponds to a hierarchy of harmonic functions. That a chord's function can be prolonged means that (Caplin, 1998):

- the chord has structural relevance;
- the chords over which this function is prolonged has a surface function (which is embedding) and a deeper-structural function which is the function of the chord that is prolonged.

Thus, prolongation induces a hierarchy of chords and functions. This understanding is central to Schenker's theory. In Schenkerian analysis, "the" structure in the tonal music is found through prolongational analysis.

#### Grammar-based - Hierarchical Approaches

Literature about automated harmony analysis started with linguistic-based mechanisms. The first such effort, Winograd's parser, is based on a formal grammar that describes a subset of simple tonal music (Winograd, 1968). The input to the parser is a sequence of chords that are prepared from the music's score to a human. The program is a two-person parser, which in the first pass labels chords and in the second labels chords' functions. The program deals with simple textures only.

The most profound study in this direction has been the Generative Theory of Tonal Music (GTTM) which is a collaborative work of a composer and a linguist (Lerdahl...).

HarmlTrace cannot handle modulation but only change of mode. Second, it cannot parse chords made up of any group of notes as in the case of our approach. Finally, our network allows users to play with, transition parameters from chord to chord, which results in different an analysis.

#### Probabilistic-Linear Approaches

AHA, statistical models and methods are used. As If Krumhansl made experiments with experienced listeners in figure out the fit of each pitch class (in the 12-tone equal temperament).

The Rubato Composer environment for composition, analysis, and performance is based on representing musical objects as mathematical structures; such objects are called denotators which are points in general spaces, called items. They are recursively built via the use of category theory concepts such as limits, colimits, and powersets, and are based upon mathematical modules over general (not necessarily commutative) rings (Milmeister, 2009). Denotators vastly generalize the XML data formally. Rubato's functional components that extend Rubato are called rubettes. A rubette may perform a particularly well-defined musical task communicating and a Rubato Composer environment for composition, analysis, and performance is based on's functionalities are extensible by adding new rubettes as plug-in units (Milmeister, 2009).

The set of rubettes that we developed in the Rubato Composer environment can be used by music theorists for analysis and stylistic experiments and also by composers for creative purposes that have an analytical foundation. In the design of the analytical procedure, we tried to a musical analysis and a musical functional analysis and a musical analysis and a musical step as much as possible, so that the user can have full control over the analytical procedure.

In the current version of Rubato Composer, the Harmo...

**Figure 2:** Properties dialog.

In our model of Riemannian harmony: tonality is represented by two components: (a) a scale, (a: subset $S \subset P$, and (b) a mode for $S$, which means the selection of a tone pitch class ($S$), that ($B$) the definition of a data $S$) a function $X: CHORD \to F$.

**Figure 3:** View dialog.

**Figure 4:** Properties dialog.

where 0 is for major mode and 5 is for minor mode (Minor scale starts with the 5th pitch of the major scale; functions = (0,5,7) where 0 is Tonic. 5 is subdominant and 7 is dominant function. Through the view functionality, not only the Riemann matrix size is seen, but also weight-of-thirds table's values can be set. This weight-of-thirds table will be used during evaluation of Riemann matrix values for each chord in the HarmonicAnalysisModel rubette. The weight-of-thirds table can be edited by the user, saved as a file or loaded from a file. The use of the weight-of-thirds table is discussed in the "HarmonicWeight rubette" section. Default settings assign the tonics load with equal weight for each pitch class in the key. For example, a major row has a weight of 1 for pitch classes C, E, G, B and 0 for all other pitch classes. One may like to change this so that C as the tonic has higher weight than G which has higher weight than E and assign B is the least weight. Another useful set of values would be Krumhansl's key-profile values.

In Figures 4 and 5 another setting for the HarmonicAnalysisModel rubette can be seen. In that setting, not only T, S and D, but all seven diatonic functions are chosen.

#### PitchSystem

A pitch system with non-zero period oct is a set $S$ of pitches that is oct-periodic. That is to say, for each $s \in S$ it contains the orbit $s + Z \cdot oct$. A pitch system is the set of orbits $S / Z \cdot oct$ which we denote by $S / oct$. For example, a keys on a piano comprise a pitch system and the period is the octave. All of the white keys on the piano are another pitch system with the same period, the octave.

Currently the only possible PitchSystem specification in the properties of the rubette is the 12-tone equal tempered scale (usually encoded by keynums in MIDI).

#### Scale

A scale in a pitch system (S, oct) is a finite non-empty subset $X$ of $S^{ct}$. In other words, a set of pitches that is oct-periodic and have a finite set of orbits. Pentatonic scales, whole-tone scales, quarter-tone scales are examples of such scales.

#### Mode

A mode is a pair (X, $f$) of a scale X and an element which is the tone (or final) of X. One may also add a second pitch class that is the tenor and denote the mode as (X, $f$) where it is the tenor. Major mode is $(\{(0, 2, 4, 5, 7, 9, 11)\}, 0)$.

**Figure 5:** View dialog.

For different pitch systems, modes and functions will be defined accordingly. For scales that use microtones, HarmonicAnalysisModelRubette should also be fed into MidiFileInRubette whose output will include Pitchbend events to represent the microtones. At this stage, this has not been implemented in the suite.

**Figure 6:** Use-duration checkbox in the properties.

**Figure 7:** Use-duration "off" only notes with identical onset on by the user. This takes into account overlap in duration. "use-duration" is off, only notes with identical onset are considered as a chord. in six seconds case, when "use-duration" is off, there are three chords.

### ChordSequencer Rubette

The ChordSequencer rubette converts a MIDI score file to a chord sequence, listing the chords for the user. This list can be edited by the user. The software creates a first list following a choice by the user regarding the role of durations to define chords. The user can turn on the use-duration property as seen in Figure 6.

For example, the four pitches in Figure 7 there four four pitches with three different onsets.

**Figure 8:** List of chords in the MIDI file.

**Figure 9:** Notes selected to be joined as a chord.

**Figure 10:** New chord sequence with duration and onsets of notes updated by the user.

associating weights for a chord as a probability distribution in the three dimensional space of tonics (often identified with tonalities) (T), modes (M) and functions (F). Thus, the Riemann matrix table for a given chord Ch is a function (WeightCh) T × M × F → [0,1].

**Figure 11:** Minimal thirds chains and Riemann matrix values for the selected chord. The notes in the third chain remain within the span of an octave. Row names are encoded as M(f) where M is mode and F is function.

chain content of the chord Ch. This is defined as follows: third chain is a chord Th, whose n elements can be ordered in a sequence Th1, Th2, ..., Thn of pitch classes such that Thi+1 - Thi = s for i = 1 to n-1, where s = 3 (minor third) or s = 4 (major third). A minimal third chain for Ch is a minimum length third chain, containing all pitches of the chord each only once and possibly containing other pitches not in the chord (see Figure 11). Note that any possible chord can be embedded in at least one minimal third chain; for example, a chord as part of the chromatic third chain {0, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8, 11}. A list of all possible third chains, ordered with respect to length is in the Topos of Music (Mazzola, 2002, pp. 1175-1181).

Let Ci a chord Let Z = {Z1, Z2, ...} be the set of all minimal third chains Z for Ch. The be the set of a chord.

**Figure 12:** Weights of thirds used to calculate Riemann matrices for chords. This table can be set through the HarmonicAnalysisModel rubette. This table is used for the Mozart analysis in the "Rubettes working together" section.

The weight of the chord Ch for a particular matrix point (t, m, f) is an average of all weights of minimal third chains for Ch.

In the Riemann matrix high values correspond to a higher probability that the chord will be associated with that tonality mode and function if other parameters are not taken into account. successive chords and their associated functions in a tonality. The contextual consideration is done on the later stage when all possible harmonic paths are computed.

### HarmonicPath Rubette

Having calculated the Riemann matrix for each chord in a chord sequence, what is the tonality, mode and function to be attributed to every chord. As seen in Figure 11, every Riemann matrix has, at least, card(T × M × F) = 72 such "points". A harmonic path is a sequence (p1, p2, ...) of points in one point is selected from each matrix Mi of the sequence M1, M2, ...

**Figure 13:** Tension tables for updating tonality, mode, function tensions. This setup is used in the Mozart analysis in the "Rubettes working together" section.

distance (p_i, p_{i+1}) = ...

**Figure 14:** Three Riemann matrices in a window of size 3. A possible path is shown as a red line.

The practical problem in harmonic path calculation is the number of possible paths for chords. To work around this problem a search is evaluated locally, within a window. The window's size is given through causal depth c and final depth f. Thus, window size is c + f + 1. The algorithm for finding the path with maximum weight in a window:

- Find the path with maximum weight between matrices m-c, m-c+1, ...

**Figure 15:** Setting window size, tonalities to exclude, and thresholds for Riemann matrix values. This setting is used for the Mozart analysis in the "Rubettes working together" section.

Noll and Garbers gave more control to the user to have access to all parts of the rubette, not only the tension settings for dyadic transitions as in the case of original HarmRubette (Noll & Garbers, 2004). One way of access is during run-time through the scripting language F-Script. Our network, doing the analysis in four consecutive steps, does not have such a run-time interface. In the original HarmRubette, default settings for dyadic transitions are sensible for tonal music genre. In the redesign, more specific control and responsibility in harmonic analysis is given to the user, both in our network and in Noll and Garbers' implementation. In their reimplementation, Noll and Garbers used the Viterbi algorithm for path finding (Noll & Garbers, 2004). The Viterbi algorithm is a practical algorithm to evaluate the most probable path through a set of states, with associated probabilities and observed outcomes for each state.

**Figure 16:** Five rubettes making a network for harmonic analysis.

**Figure 17:** Mozart's Piano Sonata in A, K.331/300i, I. 1-8 (figure reproduced with permission from Classical form by William E. Caplin, Oxford University Press, p. 52.)

and the Harmonic. HarmonicPath rubettes both take as input the analysis specification from the HarmonicAnalysisModel (Mazzola, 2002, pp. 1175-1181). This specification defines the Riemann matrix size and points. The input to the last rubette in the rubette network (see Figure 16, the HarmonicPath rubette) is the Riemann matrix sequence. The output of the HarmonicPath rubette is the result of the chord analysis. The rubettes' outputs can be displayed using the Display rubette, present in Rubato as XML in the denotator form.

### An Example: Mozart Piano Sonata in A

The process of harmonic analysis has been, in practice, a mixture of objective and subjective evaluation. Our aim is to make the analysis process well-defined and not only to make all decision parameters explicit, but also to bring them to the front. Such a parametrization and mechanism of the function of aesthetic judgment does not mean that it cannot be processed analytically.

From Mozart Piano Sonata in A, K.331/300i, first movement, the first 8 measures are shown in Figure 17. It is an 8-bar sentence in A major having a half-cadence in the middle and a perfect authentic cadence in the end. The Rubato analysis is as shown in Figure 18.

**Figure 18:** Rubette analysis of Mozart's Piano Sonata in A, K.331/300i, I mm 1-8. Causal depth is one and final depth is two and thus window size is four.

In Figure 17. In fact, this result is difficult to find out from the chord's content due to missing C#. The motion in the bass over the whole bar F#-G#-A has significance. The rubette analysis computes the most significant upper partials of the chord, i.e. A(3/00), A(1/00), A(5/00) and a Dominant of A major. Tonic of A major, Tonic of A major and Tonic of E major. The result is choosing an A major tonic function for the 11th chord, for the 12th chord, the result is the same however. A major tonic function. However, notice that a different optimum path is computed, i.e. A(3/0)-A(4/0)-A(1/0)-E(1/0), which is Tonic of A major, Tonic of A major, Dominant of A major and Tonic of E major. This is because, for an F#-F-A chord, A major gets higher weight than F#m7, and F# is a distant key in the region of A key.

It is not the goal of our approach to literally match traditional analytical annotations. First of all, the analysis of chord-slices for each onset constitutes a very basic level of description, which is not sensitive towards a distinction between proper harmonic function other verticalities, nor to the identification of broader harmonic units. But the study of the behavior of the automatically calculated analyses under systematic variation of the analytical parameters leads to a demanding experimental paradigm for empirical work. Eventually this may lead to alternative concepts of harmonic function on computational grounds.

### Another Example

The following example in Figure 19 is a music snippet where the K-S algorithm fails (Temperley, 2002). The passage is clearly not in E major, but due to the dominance of the pitch E, will be judged to be in key E by the K-S algorithm. Our network evaluates the passage as being in C major with the weight of the E-chord set as in Figure 20. Even if the later relations hold, all on the strong beat. It is still evaluated as C major. The reason that the evaluation does not change to E is due to the settings on the tonality change tension table. Pitch E, given the

**Figure 19:** An example where the Krumhansl-Schmuckler key finding algorithm fails.

weight-of-thirds table as in Figure 20, contributes equally to C major and E major. When the weight-of-thirds table is changed to give more emphasis to the tonic (through the HarmonicAnalysisModel rubette), as in Figure 21, the evaluation changes to E major. (Note that the contribution of pitch E to E major and E minor are equal in the weight-of-thirds table. Thus, E minor and E major are equally weighted decisions by the analysis networks.)

**Figure 20:** The weight-of-thirds table used and the analysis result.

**Figure 21:** The weight-of-thirds table and the analysis result.

## Comparison with Other Models

### A Comparison with Lerdahl's Tension Model

TPS is Fred Lerdahl's extension of GTTM. In TPS Lerdahl concentrates on pitch-related musical material and organization: chordal and regional spaces, harmonic prolongation, scales, chordal tension, melodic attraction. TPS differentiates between two different types of harmonic tension and defines a metric to calculate both sequential surface tension and hierarchical structural tension. If x, y are triads, the distance d(x, y) between x and y is the sum of differences in the triads based on the diatonic basic space and non-common pitch classes (Lerdahl, 2001). While surface tension is between adjacent chords, hierarchical tension is between distinct chords, horizontal after the predominant structure is reversed. Quantization of tension is used to calculate tonal tension for each chord in a tonal piece.

Experimental results based on listeners' emotional responses regulated via their expectations (Krumhansl & Lerdahl, 2001). The difficulty of the tension model is the addition of the structural tension, which the structural tension, one should already have made the structural analysis of the piece. Noll and Garbers also underline a theoretical problem with Lerdahl's chordal distance formula (Noll & Garbers, 2004).

Our tension model differs from Lerdahl's model in two ways. First our chords are not necessary to be triads. Second, our tension parameters are user-configurable, the default values being sensible with respect to tonal music. For example, all other things being equal, moving from tonic to dominant increases tension, and the reversed movement decreases tension. Moving from one diatonic basic space to another also increases tension directly proportional to the cycle-of-fifths distance of the spaces.

### A Comparison with Sapp's "Stack of Thirds" Approach

Sapp's computational chord-root identification, which he considers as the first step towards AHA, arranges chords as a "stack of thirds." To find the root of the chord, he considers all possible such stacks, and gives a compactness score for every combination (Sapp, 2007). Sapp's consideration of minor 3rd and major 3rd intervals as primary intervals for tonal music of the common practice period also supports our minimal-3rd-chain approach.

## Future Work

- The window size used in path calculation is not dynamic, but fixed by the user prior to analysis. A dynamic window size will be more efficient in capturing prolonged harmonics.

A second usage of harmonic analysis is to find out how important a note or a group of notes is for the overall harmony. Omitting a note and finding out how the overall harmonic analysis changes would be a measure of the harmonic importance of the note. Knowing the importance of note(s) in a chord and in the overall harmony, the performer may decide to emphasize those notes. The user could change the overall harmony. The original HarmRubette. A third enhancement would be to make the weight formula for paths, i.e. equation (2) user-configurable. The user could then choose among different weight formulas and also be able to add their own weight formula. This is implemented by Noll and Garbers (2004). A fourth enhancement could allow different weight calculation for chords, not necessary to be the same for all chords.

## References

Bas De Haas, W., Magalhaes, J.P., Wiering, F., & Veltkamp, R.C. (2011). Automatic functional harmonic analysis (Technical Report UU-CS-2011-023). Utrecht, The Netherlands: Department of Information and Computing Sciences, Utrecht University.

Bigand, E., Delbé, C., Poulin-Charronnat, B., Leman, M., & Tillmann, B. (2014). Empirical evidence for musical syntax processing? Computer simulations reveal the contribution of auditory short-term memory. Frontiers in Systems Neuroscience.

Caplin, W.E. (1998). Classical form (1st ed.). New York: Oxford University Press.

Chew, E. (2001). Modeling tonality: Applications to music cognition. In Proceedings of the 23rd annual meeting of the Cognitive Science Society (pp. 206-211). Mahwah, NJ: Lawrence Erlbaum Associates.

... (additional references truncated for brevity)
