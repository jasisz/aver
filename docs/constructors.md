# Aver — Constructor Contract

Agreed 2026-02-26. This is the iron-clad contract governing how constructors work in Aver.

## The rules

### 1. `:` is exclusively for type annotations

Colon appears only in declarations: function parameters (`x: Int`), record fields (`name: String`), return types (`-> Result<Int, String>`). Never in expressions.

### 2. Naming convention is load-bearing

| Convention | Meaning | Examples |
|---|---|---|
| `lowerCamel` or `lower_snake` | Function name | `parse`, `fromString`, `readAge` |
| `UpperCamel` | Type / constructor / namespace | `User`, `Shape.Circle`, `Result.Ok` |

The parser uses the first character of the callee to distinguish function calls from constructor invocations. This is not a style guide — it is grammar.

### 3. Constructor = UpperCamel callee

Any call-site where the callee starts with an uppercase letter is a constructor. This includes:
- Bare names: `User(...)`
- Qualified names: `Shape.Circle(...)`, `Result.Ok(...)`

The final segment determines the kind: `Map.fromList(...)` is a function call (`fromList` is lower), `Shape.Circle(...)` is a constructor (`Circle` is upper).

### 4. Records use named arguments with `=`

Product types (records) are constructed with explicit field names:

```aver
record User
    name: String
    age: Int

u = User(name = "Alice", age = 30)
```

All fields are required exactly once. No defaults, no partial construction.

### 5. Sum type variants use positional arguments

Variant constructors take positional arguments matching the type definition order:

```aver
type Shape
    Circle(Float)
    Rect(Float, Float)
    Point

c = Shape.Circle(3.14)
r = Shape.Rect(2.0, 5.0)
```

### 6. Zero-argument constructors are singletons (bare values)

A constructor with no parameters is a value, not a function call. No parentheses:

```aver
p = Shape.Point
n = Option.None
```

Rationale: `Shape.Point` is always the same value. Writing `Shape.Point()` would imply construction where there is none — like writing `42()` to "construct" an integer.

Pattern matching is symmetric:

```aver
match shape
    Shape.Circle(r) -> r * r * 3.14
    Shape.Rect(w, h) -> w * h
    Shape.Point -> 0.0
```

### 7. Named and positional arguments never mix

This is not an independent rule — it follows automatically from rules 4 and 5:
- Records → always named (rule 4)
- Variants → always positional (rule 5)

There is no third kind of constructor. The parser sees `=` after the first argument name → record. No `=` → variant. One token of lookahead, zero ambiguity.

### 8. Opaque types are non-constructable

Some record types are internal to a service and cannot be constructed by user code:

```aver
// Tcp.Connection is opaque — use Tcp.connect(host, port) instead
conn = Tcp.connect("localhost", 8080)?
```

The parser rejects `Tcp.Connection(...)` with an actionable error message.

## Parser decision tree

```
callee starts with UpperCamel?
├── NO  → function call: f(args...)
└── YES → constructor
    ├── followed by `(`?
    │   ├── YES → has arguments
    │   │   ├── first arg is `Ident =`? → record create: User(name = "A", age = 1)
    │   │   └── otherwise             → variant create: Shape.Circle(3.14)
    │   └── NO  → zero-arg singleton: Option.None, Shape.Point
    └── (opaque check: reject if type is non-constructable)
```

## What this replaces

- `Expr::TypeAscription` — removed (2026-02-26). `:` no longer appears in expressions.
- Ad-hoc lookahead heuristics for distinguishing records from function calls.
