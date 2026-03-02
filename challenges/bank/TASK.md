# Challenge: Bank Account System

## Goal

Build a simple banking system — **twice**.

1. First, implement it in Aver (`solution.av`)
2. Then, implement the same thing in a language of your choice (`solution_compare.*`)

## What the system should do

A bank manages accounts. Each account has an owner, balance, and unique identifier.

Your program should support:

1. **Create account** with an initial balance
2. **Find account** by id
3. **Deposit** money into an account
4. **Withdraw** money (fail if insufficient funds)
5. **Transfer** between two accounts (fail if insufficient funds or invalid accounts)
6. **Total assets** — sum of all balances in the bank
7. **Transaction log** — each deposit/withdraw/transfer should produce a record of what happened (who, what, amount, success/failure), and you should be able to collect a history of operations
8. **Account statement** — given an account id and a transaction log, produce a summary: opening balance, list of transactions affecting that account, closing balance

Think about: how do you represent a collection of accounts for efficient lookup? How do you handle the various error conditions? How do you model a transaction log immutably?

## Main function

In both versions, write a `main` that runs a realistic sequence of operations — create accounts, make deposits/withdrawals/transfers (including some that fail), then print a statement for one account.

## Verification

In the Aver version, include `verify` blocks that prove your functions work — cover happy paths, error cases (insufficient funds, account not found, self-transfer), and edge cases.

In the comparison version, write equivalent tests using the language's native test framework.

Your Aver solution must pass:
```
aver check challenges/bank/solution.av
aver verify challenges/bank/solution.av
aver run challenges/bank/solution.av
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
