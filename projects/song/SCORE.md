# Score

What `main.av` asks the speaker to play, beat by beat. A beat is one bass note, 400 ms; the beat count is the stage's (only the bass moves it on). Notes are C major pentatonic: 0–4 = C3 D3 E3 G3 A3, 5–9 = C4–A4, 10–14 = C5–A5, 15–17 = C6 D6 E6. Lengths: ♪ eighth 200 ms, ♩ quarter 400 ms, 𝅗𝅥 half 800 ms, 𝅗𝅥. 1200 ms, `·` a rest (a breath: `Run.turn()`).

| beats | bass | melody | counter | chords |
|---|---|---|---|---|
| 0–3 | C G C G | | | |
| 4–11 | C G … | **motif** (question): E♪ G♪ A♩ G♩ · E♩ D♩ C𝅗𝅥 · | | |
| 12–19 | C G … | **answer**: E♪ G♪ A♩ C′♩ · A♩ G♩ E𝅗𝅥 · | A4 G4 E4 G4 (𝅗𝅥 each, falling while the melody rises) | |
| 20–27 | C G … | **middle**, higher: C′♩ D′♪ C′♪ A♩ · G♩ A♪ G♪ E𝅗𝅥 · | E4 D4 E4 G4 | |
| 28–35 | **A E A E …** (relative minor) | **climax**: A♪ C′♪ D′♩ E′𝅗𝅥 · D′♪ C′♪ A♩ G♩ · | E4 D4 C4 D4 | three jobs at once: **A minor** A4 + C5 + E5, each note when its job lands, then **C over G** G4 + C5 + E5 |
| 36–39 | **G D G D** (back home) | **homecoming**: E♪ G♪ A♩ G♩ · E♩ D♩ … | E4 D4 D4 … | (ringing) |
| 40–47 | C G … | … C (𝅗𝅥., the resolution) | … C4 (𝅗𝅥.) | |
| after 48 | | | | **coda**: C4, E4, G4, C5 spread an eighth apart, 3 s each |

Melody notes are in octave 5 (C5 = 10), so `C′` = C6. The chords' entrance, beat 28, is the climax's first beat; when each chord note sounds depends on its job.
