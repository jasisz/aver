pub struct CoreBenchCase {
    pub slug: &'static str,
    pub name: &'static str,
    pub source: &'static str,
}

const FIBONACCI: &str = "\
fn fib(n: Int) -> Int
    match n < 2
        true -> n
        false -> fib(n - 1) + fib(n - 2)

fn main() -> Int
    fib(25)
";

const SUM_TCO: &str = "\
fn sum(n: Int, acc: Int) -> Int
    match n == 0
        true -> acc
        false -> sum(n - 1, acc + n)

fn main() -> Int
    sum(1000000, 0)
";

const COUNTDOWN: &str = "\
fn countdown(n: Int) -> Int
    match n == 0
        true -> 0
        false -> countdown(n - 1)

fn main() -> Int
    countdown(1000000)
";

const FACTORIAL_TCO: &str = "\
fn fact(n: Int, acc: Int) -> Int
    match n == 0
        true -> acc
        false -> fact(n - 1, acc * n)

fn main() -> Int
    fact(20, 1)
";

const NESTED_MATCH: &str = "\
fn abs(n: Int) -> Int\n    match n < 0\n        true -> 0 - n\n        false -> n\n\nfn classify(n: Int) -> Int\n    match n == 0\n        true -> 0\n        false -> abs(n) * 2\n\nfn run(n: Int, acc: Int) -> Int\n    match n == 0\n        true -> acc\n        false -> run(n - 1, acc + classify(n - 500))\n\nfn main() -> Int\n    run(1000, 0)\n";

const RESULT_CHAIN: &str = "\
fn safeDivide(a: Int, b: Int) -> Result<Int, Int>\n    match b == 0\n        true -> Result.Err(0)\n        false -> Result.Ok(a / b)\n\nfn unwrapOr(r: Result<Int, Int>, d: Int) -> Int\n    match r\n        Result.Ok(v) -> v\n        Result.Err(_) -> d\n\nfn chain(n: Int, acc: Int) -> Int\n    match n == 0\n        true -> acc\n        false -> chain(n - 1, acc + unwrapOr(safeDivide(n * 10, n), 0))\n\nfn main() -> Int\n    chain(10000, 0)\n";

const LIST_WALK: &str = "\
fn listLen(xs: List<Int>, acc: Int) -> Int\n    match xs\n        [] -> acc\n        [h, ..t] -> listLen(t, acc + 1)\n\nfn main() -> Int\n    listLen([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 0)\n";

const SHAPES: &str = "\
type Shape\n    Circle(Float)\n    Rect(Float, Float)\n    Point\n\nfn area(s: Shape) -> Float\n    match s\n        Shape.Circle(r) -> r * r * 3.14159\n        Shape.Rect(w, h) -> w * h\n        Shape.Point -> 0.0\n\nfn sumAreas(n: Int, acc: Float) -> Float\n    match n == 0\n        true -> acc\n        false -> sumAreas(n - 1, acc + area(Shape.Circle(1.0)) + area(Shape.Rect(3.0, 4.0)))\n\nfn main() -> Float\n    sumAreas(10000, 0.0)\n";

const RECORD_HEAVY: &str = "\
record Point\n    x: Int\n    y: Int\n\nfn moveRight(p: Point) -> Point\n    Point.update(p, x = p.x + 1)\n\nfn walk(n: Int, p: Point) -> Int\n    match n == 0\n        true -> p.x + p.y\n        false -> walk(n - 1, moveRight(p))\n\nfn main() -> Int\n    walk(10000, Point(x = 0, y = 0))\n";

const STRING_BUILD: &str = "\
fn repeat(n: Int, s: String, acc: String) -> String\n    match n == 0\n        true -> acc\n        false -> repeat(n - 1, s, acc + s)\n\nfn main() -> Int\n    r = repeat(1000, \"ab\", \"\")\n    String.len(r)\n";

const ERROR_PIPELINE: &str = "\
fn validate(n: Int) -> Result<Int, String>\n    match n < 0\n        true -> Result.Err(\"negative\")\n        false -> Result.Ok(n)\n\nfn transform(n: Int) -> Result<Int, String>\n    match n > 1000\n        true -> Result.Err(\"too large\")\n        false -> Result.Ok(n * 2)\n\nfn pipeline(n: Int) -> Result<Int, String>\n    v = validate(n)?\n    transform(v)\n\nfn run(n: Int, acc: Int) -> Int\n    match n == 0\n        true -> acc\n        false -> run(n - 1, acc + Result.withDefault(pipeline(n), 0))\n\nfn main() -> Int\n    run(10000, 0)\n";

const LIST_BUILTINS: &str = "\
fn buildAndMeasure(n: Int, acc: List<Int>) -> Int\n    match n == 0\n        true -> List.len(acc)\n        false -> buildAndMeasure(n - 1, List.prepend(n, acc))\n\nfn main() -> Int\n    buildAndMeasure(1000, [])\n";

const LIST_APPEND_SCAN: &str = "\
fn build(n: Int, acc: List<Int>) -> List<Int>\n    match n == 0\n        true -> acc\n        false -> build(n - 1, List.append(acc, n))\n\nfn sum(xs: List<Int>, i: Int, acc: Int) -> Int\n    match List.get(xs, i)\n        Option.None -> acc\n        Option.Some(v) -> sum(xs, i + 1, acc + v)\n\nfn main() -> Int\n    sum(build(1000, []), 0, 0)\n";

const MIXED_REAL: &str = "\
record Order\n    id: Int\n    amount: Int\n    valid: Bool\n\nfn processOrder(o: Order) -> Result<Int, String>\n    match o.valid\n        true -> Result.Ok(o.amount * 2)\n        false -> Result.Err(\"invalid\")\n\nfn processAll(n: Int, acc: Int) -> Int\n    match n == 0\n        true -> acc\n        false -> processAll(n - 1, acc + Result.withDefault(processOrder(Order(id = n, amount = n * 10, valid = true)), 0))\n\nfn main() -> Int\n    processAll(10000, 0)\n";

pub const CORE_BENCH_CASES: &[CoreBenchCase] = &[
    CoreBenchCase {
        slug: "fib_25",
        name: "fib(25)",
        source: FIBONACCI,
    },
    CoreBenchCase {
        slug: "sum_tco_1m",
        name: "sum_tco(1M)",
        source: SUM_TCO,
    },
    CoreBenchCase {
        slug: "countdown_1m",
        name: "countdown(1M)",
        source: COUNTDOWN,
    },
    CoreBenchCase {
        slug: "factorial_20",
        name: "factorial(20)",
        source: FACTORIAL_TCO,
    },
    CoreBenchCase {
        slug: "nested_match_1k",
        name: "nested_match(1K)",
        source: NESTED_MATCH,
    },
    CoreBenchCase {
        slug: "result_chain_10k",
        name: "result_chain(10K)",
        source: RESULT_CHAIN,
    },
    CoreBenchCase {
        slug: "list_walk_10",
        name: "list_walk(10)",
        source: LIST_WALK,
    },
    CoreBenchCase {
        slug: "shapes_10k",
        name: "shapes(10K)",
        source: SHAPES,
    },
    CoreBenchCase {
        slug: "record_heavy_10k",
        name: "record_heavy(10K)",
        source: RECORD_HEAVY,
    },
    CoreBenchCase {
        slug: "string_build_1k",
        name: "string_build(1K)",
        source: STRING_BUILD,
    },
    CoreBenchCase {
        slug: "error_pipeline_10k",
        name: "error_pipeline(10K)",
        source: ERROR_PIPELINE,
    },
    CoreBenchCase {
        slug: "list_builtins_1k",
        name: "list_builtins(1K)",
        source: LIST_BUILTINS,
    },
    CoreBenchCase {
        slug: "list_append_scan_1k",
        name: "list_append_scan(1K)",
        source: LIST_APPEND_SCAN,
    },
    CoreBenchCase {
        slug: "mixed_real_10k",
        name: "mixed_real(10K)",
        source: MIXED_REAL,
    },
];
