# Challenge: Text Analyzer

## Goal

Write an Aver program (`solution.av`) that analyzes text strings and computes statistics.

## Domain

A text analyzer takes a string of text and produces statistics about it. The system must support these operations:

1. **Word count** — number of words (split by spaces)
2. **Unique words** — list of distinct words (case-sensitive)
3. **Word frequency** — how many times each word appears (as a Map)
4. **Most frequent word** — the word with the highest count
5. **Contains word** — check if a specific word appears in the text
6. **Average word length** — mean character count across all words

## Requirements

### Data model
- Use a `record` for analysis results (wordCount, uniqueCount, averageLength)
- Use `Map<String, Int>` for word frequencies
- Use `Option` for operations that may have no result (most frequent word on empty text)

### Functions to implement
- `wordCount(text)` → Int
- `uniqueWords(text)` → List<String>
- `wordFrequency(text)` → Map<String, Int>
- `mostFrequent(text)` → Option<String>
- `containsWord(text, word)` → Bool
- `averageWordLength(text)` → Float
- `analyze(text)` → TextStats record with summary

### Conventions
- Read `README.md` and `docs/` for the language specification and API
- Look at `examples/calculator.av` as a reference for style and structure
- Every function must have a `? "..."` description
- Use `verify` blocks to prove your functions work
- Use at least one `decision` block to justify a design choice
- Use a `module` block with `intent`
- **No `if`/`else`** — use `match` for all branching
- **No loops** — use `List.map`, `List.filter`, `List.fold` for iteration
- All constructors are namespaced: `Result.Ok(x)`, `Option.Some(x)`, `Option.None`

### Main function
- `fn main()` with `! [Console]` that analyzes a sample text and prints results
- Print results using `Console.print`

## Verification

Your solution must pass:
```bash
aver check challenges/text-analyzer/solution.av
aver verify challenges/text-analyzer/solution.av
aver run challenges/text-analyzer/solution.av
```

## What success looks like

A single `solution.av` file that:
- Compiles without type errors (`check`)
- All verify cases pass (`verify`)
- Runs and prints meaningful output (`run`)
- Follows Aver conventions (namespaced calls, Option/Result for errors, match for branching, descriptions on functions)
- Contains at least one `decision` block
- Contains at least 10 verify cases covering edge cases (empty text, single word, repeated words)
