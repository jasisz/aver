# Challenge: Text Analyzer

## Goal

Build an Aver program (`solution.av`) that analyzes text and computes statistics.

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

Write `fn main()` that analyzes a few sample texts (including an empty one) and prints the results.

## Verification

Include `verify` blocks that prove your functions work — cover normal text, single word, empty text, repeated words, and edge cases.

Your solution must pass:
```
aver check challenges/text-analyzer/solution.av
aver verify challenges/text-analyzer/solution.av
aver run challenges/text-analyzer/solution.av
```

## Getting started

1. Read `README.md` — the complete language reference
2. Read `docs/services.md` — full API for all built-in namespaces
3. Study `examples/calculator.av` — conventions and style
