# Aver — Constructor Contract

Agreed 2026-02-26. These are the fixed rules for how constructors work in Aver.

## The rules

### 1. `:` is exclusively for type annotations

A colon appears only in declarations: function parameters (`x: Int`), record fields (`name: String`) and return types (`-> Result<Int, String>`). It never appears in an expression.

### 2. Naming convention is load-bearing

| Convention | Meaning | Examples |
|---|---|---|
| `lowerCamel` or `lower_snake` | Function name | `parse`, `fromString`, `readAge` |
| `UpperCamel` | Type / constructor / namespace | `User`, `Shape.Circle`, `Result.Ok` |

The parser looks at the first character of the callee to tell a function call from a constructor. The convention is part of the grammar, so breaking it changes what the code means.

### 3. Constructor = UpperCamel callee

A call whose callee starts with an uppercase letter is a constructor. That covers:
- Bare names: `User(...)`
- Qualified names: `Shape.Circle(...)`, `Result.Ok(...)`

The last segment decides. `Map.fromList(...)` is a function call because `fromList` is lowercase. `Shape.Circle(...)` is a constructor because `Circle` is uppercase.

### 4. Records use named arguments with `=`

Records (product types) are built with explicit field names:

```aver
record User
    name: String
    age: Int

u = User(name = "Alice", age = 30)
```

Every field must be given exactly once. There are no defaults and no partial construction.

### 5. Sum type variants use positional arguments

Variant constructors take positional arguments in the order the type definition lists them:

```aver
type Shape
    Circle(Float)
    Rect(Float, Float)
    Point

c = Shape.Circle(3.14)
r = Shape.Rect(2.0, 5.0)
```

### 6. Zero-argument constructors are singletons (bare values)

A constructor with no parameters is a value, so it takes no parentheses:

```aver
p = Shape.Point
n = Option.None
```

Rationale: `Shape.Point` is always the same value. `Shape.Point()` would suggest that something gets constructed, when nothing does. It would be like writing `42()` to "construct" an integer.

Pattern matching uses the same shapes:

```aver
match shape
    Shape.Circle(r) -> r * r * 3.14
    Shape.Rect(w, h) -> w * h
    Shape.Point -> 0.0
```

This applies to constructors only. Records are plain data values: you bind the whole record in a pattern and read fields by name. There is no positional record pattern like `User(name, age)`.

### 7. Named and positional arguments never mix

This follows from rules 4 and 5 and adds nothing new:
- Records → always named (rule 4)
- Variants → always positional (rule 5)

There are only these two kinds of constructor. If the parser sees `=` after the first argument name, it is a record. Without `=`, it is a variant. One token of lookahead is enough and nothing is ambiguous.

### 8. Dotted record constructors

Dotted record constructors such as `MyNs.Point(x = 1, y = 2)` are supported. Any `Namespace.Type(...)` form whose last segment is UpperCamel follows the same rules as a bare record type (rule 4: named arguments with `=`).

```aver
p = Geom.Point(x = 0, y = 0)
```

A module can make its own types opaque with `exposes opaque [TypeName]` in the module declaration. Outside the defining module, an opaque type cannot be constructed, its fields cannot be read, and it cannot be pattern-matched. See [language.md](language.md#opaque-types).

Some standard-library types are opaque too. `Tcp.Connection`, for example, is a stateful resource owned by the provider and created by `Tcp.connect`. The type checker rejects field reads, construction and pattern matches on it.

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

- `Expr::TypeAscription`, removed on 2026-02-26. `:` no longer appears in expressions. Typed bindings (`name: Type = expr`) use `:` in declaration position instead.
- Ad-hoc lookahead heuristics that tried to tell records from function calls.
