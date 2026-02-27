# Aver — Decisions

This page is partially generated from `decision` blocks.

- Update source decisions in `.av` files.
- Regenerate this section with:

```bash
aver decisions --docs
```

## Generated Decisions

<!-- BEGIN AUTO-GENERATED: decisions -->

## ConstructorContract (2026-02-26)

**Chosen:** UpperCamelCalleeContract
**Rejected:** TypeAscriptionBased, KeywordConstructors, UniformParens
**Impacts:** Parser, Interpreter, TypeChecker, AllModules

> Constructors need a single, unambiguous parsing rule that does not conflict with function calls or type annotations. UpperCamel callee distinguishes constructors from functions at parse time with one token of lookahead. Records use named args (=), variants use positional args. Zero-arg constructors are bare singletons without parens. This removes the need for TypeAscription and all cross-cutting heuristics in the parser.

Author: `Aver core team`

## EffectContractsAreTransparentAcrossModules (2026-02-25)

**Chosen:** TransparentAcrossModules
**Rejected:** OpaqueModuleContracts, ImplicitModuleCapabilities
**Impacts:** ModuleSystem, Effects, TypeChecker, RuntimeGate

> Current Aver model keeps module imports and capability effects as two explicit contracts. `depends [X]` controls which module code is visible, while `! [Effect]` controls which side effects a function may use. Effect requirements propagate through cross-module calls, so callers must declare the effects required by imported functions.

Author: `Aver core team`

## RecordsAreDataMethodsInModules (2026-02-25)

**Chosen:** DataOnlyRecords
**Rejected:** RecordMethods, ImplicitThis
**Impacts:** TypeSystem, RecordModel, ModuleNamespaces

> Keeping records as data-only keeps the object model small and predictable. Module namespaces already provide a clear place for behavior via Module.fn(record, ...). This avoids introducing two parallel styles too early: methods on records and module functions.

Author: `Aver core team`

## DecisionBlocksAsLanguageFeature (2024-02-01)

**Chosen:** FirstClassDecisions
**Rejected:** MarkdownADR, Comments, ExternalWiki
**Impacts:** Lexer, Parser, Checker, AIDeveloperExperience

> Architecture Decision Records written in markdown files are invisible to the language tooling and decay silently. Making decisions a first-class parse node means they can be indexed, searched, and rendered by any tool that reads Aver. When an AI resumes work on a codebase, it can run aver decisions to reconstruct the design rationale without reading prose.

Author: `Aver core team`

## TreeWalkingInterpreter (2024-01-30)

**Chosen:** TreeWalker
**Rejected:** BytecodeVM, LLVMBackend, Transpilation
**Impacts:** Interpreter, Performance

> A tree-walking interpreter can be built and debugged in an afternoon; bytecode compilation requires weeks of infrastructure. The primary goal of Aver is to validate language design and AI tooling, not to achieve maximum runtime throughput. Switching to bytecode or LLVM later is straightforward once the semantics are stable.

Author: `Aver core team`

## ColocatedVerify (2024-01-25)

**Chosen:** ColocatedVerifyBlocks
**Rejected:** SeparateTestFiles, ExternalTestFramework
**Impacts:** Checker, AllFunctions

> Tests that live in a separate directory rot faster because nobody reads them when changing the function. A verify block immediately below its function is the minimum viable specification: it says what the function must do. Co-location also makes it trivial for an AI to regenerate or extend tests without searching the repository.

Author: `Aver core team`

## SignificantIndentation (2024-01-20)

**Chosen:** Indentation
**Rejected:** Braces, BeginEnd, Keywords
**Impacts:** Lexer, Parser, AllModules

> Braces are syntactic noise that adds no meaning and forces style debates. Indentation is already how humans read code, so making it structural removes a class of inconsistency. The lexer emits explicit INDENT and DEDENT tokens, keeping the parser context-free and easy to extend.

Author: `Aver core team`

## ResultOverExceptions (2024-01-15)

**Chosen:** Result
**Rejected:** Exceptions, Nullable, Panic
**Impacts:** Calculator, Interpreter, Checker

> Exceptions hide control flow: a function that throws is indistinguishable from one that does not at the call site. Result types make every error path visible in the signature, which is essential when an AI reads code without running it. The match expression and the ? operator together give ergonomic error handling without sacrificing explicitness.

Author: `Aver core team`

## HostLanguage (2024-01-10)

**Chosen:** Rust
**Rejected:** Python, Go, Haskell
**Impacts:** Lexer, Parser, Interpreter, Checker

> Rust provides memory safety without a garbage collector, which matters for a language runtime. Its algebraic types (enum, Result, Option) map directly onto Aver concepts and kept the implementation honest. The rich ecosystem (clap, colored, thiserror) let us ship a polished CLI without boilerplate.

Author: `Aver core team`

<!-- END AUTO-GENERATED: decisions -->
