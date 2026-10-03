# Song

`song` is a piece of music written as an Aver program. Six players are processes; a stage and a synthesiser are the modules that answer them. What you hear is what the program did: when a note starts comes from the program's own waits (beats on the clock, entrances on the bar count, chords on their jobs), and every sample of `song.wav` is computed by pure Aver functions over `Int` and written with `Disk.appendBytes`.

## The piece

About 24 seconds in C major pentatonic, in five phrases: the bass alone, the melody's motif (a question), its answer with the counter line underneath, a higher middle, a climax over two chords worked out by jobs (A minor, then C over G) while the bass moves to A and back through G, the motif again resolving on a long C, and a spread final chord. [SCORE.md](SCORE.md) lists it beat by beat.

| players | what |
|---|---|
| `pulse` | the bass, 48 beats 400 ms apart, C and G in turn, A and E under the climax, G and D on the way back; its beats are what moves the beat count on |
| `melody` | enters at beat 4: motif, answer, middle, climax, motif again |
| `counter` | enters at beat 12: slow half notes moving against the melody, down to C |
| `voice 3`, `voice 5`, `voice 8` | a family of three; at the climax each asks for two chords, the stage begins one job per ask, and each voice sounds its chord note when its job lands (the bigger chord takes longer) |
| `coda` | waits until all six players are done, spreads C–E–G–C and closes the file |

## Modules

- `main.av` (`Song`): the players. A player is a process because it requests operations its program answers.
- `band.av` (`Band`, capability): what the players ask the stage for: `beat`, `cue`, `tune`, `chord`, `done`, `coda`.
- `stage.av` (`Stage`, `answers [Band]`): pure answers. A beat waits `ms` on the clock (`Run.Wake.Until`), a cue waits until the pulse reaches the bar (`Run.Wake.Settled`), a note is answered now, a chord begins a `Voicing` job and waits on it.
- `voicing.av` (`Voicing`, job kind): voicing one chord off the turn (`aver.toml` binds it to `Stage.voicing`).
- `speaker.av` (`Speaker`, capability): `play(note, ms)` and `close()`.
- `synth.av` (`Synth`, `answers [Speaker]`): places each request on the clock and writes the samples.
- `wave.av` (`Wave`, pure): pitches, a triangle wave, the envelope, the mix, 16-bit PCM and the WAV header, with laws.

## How the sound is made

- **Rate and format:** 22 050 Hz, mono, 16-bit little-endian PCM.
- **A note:** a triangle wave at the note's pentatonic pitch (C3 = note 0, five notes per octave), a 10 ms attack and a straight fade to silence over the note's length. Integers only.
- **The clock:** the first `Speaker.play` starts the clock (`Time.unixMs`). Every later request first brings the file up to "now": it renders the samples from what is already written up to the current moment, appends them, then starts the new note at that sample.
- **The mix:** the synthesiser keeps the voices that are still sounding. A sample is the plain sum of every voice sounding at it, clipped to the 16-bit range (`Wave.mixAt`). Notes that start together (the chord jobs landing, the final chord) simply overlap in that sum; a voice is dropped once the file is past its end.
- **The header:** the file starts with a header for an empty piece. `Speaker.close` lets every voice ring to its end, then reads the file back and writes it again with the header for its real length (`Wave.header`). It is the simplest honest way: the length is not known until the piece is over.

## Laws and the certificate

`wave.av` states, for every input:

- `clamp16`: the result always fits 16 bits (so a mixed sample, `clamp16` of a sum, always does);
- `readLe16(le16(clamp16(s))) == clamp16(s)`: a sample reads back from its two octets;
- `header`: always 44 octets; octets 40–43 are `le32` of the data length, octets 4–7 are `le32` of the data length plus 36.

The lengths of what `render`, `tone` and `pcm16` produce (a span of n samples renders n samples, a note of `ms` milliseconds is `ms × 22050 / 1000` samples, two octets per sample) are checked by `verify` examples rather than laws: the Lean source model has no definition for the sample loop, so a law about it could not be credited.

```bash
aver compile wave.av --module-root . --target wasm-gc --certify -o /tmp/wave-cert
aver cert check /tmp/wave-cert/wave.wasm /tmp/wave-cert/cert      # needs aver-cert next to aver and Lean
```

At the time of writing: 27 exports certified (the bytes compute the plan), law-claims 5 of 5 credited, bridged laws 4 of 5 (the read-back of a sample is proved on the plan but not yet tied to the source), source bridges 8 of 9.

## Running it

Run it from this directory; it writes `song.wav` here.

```bash
aver check main.av --module-root .
aver run main.av --module-root .            # VM
aver run main.av --module-root . --wasm-gc  # compiled, about three times less CPU
aver run main.av --module-root . --record recordings
aver replay recordings/*.json --check-args            # same bytes, every one of them
aver replay recordings/*.json --check-args --wasm-gc
```

The piece takes as long as it takes to play (about 24 s of audio, about 21 s of wall clock): its rhythm is the program's own waits. The VM is fast enough; the synthesiser's work runs inside the turn and costs about 9 s of CPU on the VM.

To listen: `afplay song.wav` (macOS) or any player. A replay with `--check-args` compares the bytes of every write, so a passing replay means the same `song.wav`, byte for byte; a fresh run lands notes on the wall clock and differs slightly.

