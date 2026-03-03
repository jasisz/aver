# Challenge: Text Analyzer

## Goal

Build a text analysis tool — **twice**.

1. First, implement it in a language of your choice (`solution_compare.*`) — write it naturally, using whatever patterns you normally would
2. Then, learn Aver and implement it again (`solution.av`) — write idiomatic Aver, not a line-by-line translation

## What the system should do

Given a string of text (words separated by spaces), your program should compute:

1. **Word count** — total number of words
2. **Unique words** — deduplicated list of words (case-sensitive)
3. **Word frequency** — how many times each word appears
4. **Most frequent word** — the word with the highest count (handle ties and empty text)
5. **Contains word** — check if a specific word appears
6. **Average word length** — mean character count across all words
7. **Longest word** — find the longest word in the text (handle empty text)
8. **Summarize** — a single function that takes text and returns a structured summary with key statistics

Think about: what happens with empty text? Single-word text? How do you represent "no result"? What data structures best fit word frequencies?

## Main function

In both versions, write a `main` that analyzes a few sample texts (including an empty one) and prints the results.

## Verification

In the Aver version, include `verify` blocks that prove your functions work — cover normal text, single word, empty text, repeated words, and edge cases.

In the comparison version, write equivalent tests using the language's native test framework.

Your Aver solution must pass:
```
aver check challenges/text-analyzer/solution.av
aver verify challenges/text-analyzer/solution.av
aver run challenges/text-analyzer/solution.av
```

## Comparison notes

After implementing both versions, write `notes.md` with your comparison:

- Which language made the problem easier to express? Why?
- Where did Aver's constraints (no if/else, no loops, no mutation) help or hurt?
- How did error handling compare (Result/match vs exceptions/try-catch/etc.)?
- How did testability compare (verify blocks vs your language's test framework)?
- What would you steal from Aver for your chosen language, and vice versa?
- Which version do you prefer reading? Which do you prefer writing?
- Lines of code comparison — is one significantly shorter?

Be honest and specific. We want genuine comparison, not flattery.

## Getting started

1. Read `README.md` — the complete language reference
2. Read `docs/services.md` — full API for all built-in namespaces
3. Study `examples/calculator.av` — conventions and style
