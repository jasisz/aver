use criterion::{Criterion, criterion_group, criterion_main};

use aver::nan_value::{Arena, NanValue};

/// Fibonacci(25) computed directly with NanValue — no runtime overhead,
/// just measuring value creation/matching/arithmetic speed.
fn fib_nan(n: i64, arena: &Arena) -> NanValue {
    if n <= 1 {
        return NanValue::new_int_inline(n);
    }
    let a = fib_nan(n - 1, arena).as_int(arena);
    let b = fib_nan(n - 2, arena).as_int(arena);
    NanValue::new_int_inline(a + b)
}

/// Same fib but with old-style Value enum for comparison.
#[derive(Clone)]
enum OldValue {
    Int(i64),
}

fn fib_old(n: i64) -> OldValue {
    if n <= 1 {
        return OldValue::Int(n);
    }
    let OldValue::Int(a) = fib_old(n - 1);
    let OldValue::Int(b) = fib_old(n - 2);
    OldValue::Int(a + b)
}

fn bench_fib_nan(c: &mut Criterion) {
    let arena = Arena::new();
    c.bench_function("fib(25) NanValue raw", |b| {
        b.iter(|| fib_nan(25, &arena));
    });
}

fn bench_fib_old(c: &mut Criterion) {
    c.bench_function("fib(25) OldValue raw", |b| {
        b.iter(|| fib_old(25));
    });
}

/// Build 10k records and access fields — measures arena allocation speed.
fn bench_record_arena(c: &mut Criterion) {
    c.bench_function("record create+access 10k arena", |b| {
        b.iter(|| {
            let mut arena = Arena::new();
            let mut sum: i64 = 0;
            for i in 0..10_000i64 {
                let idx = arena.push_record(
                    0,
                    vec![
                        NanValue::new_int_inline(i),
                        NanValue::new_int_inline(i * 2),
                        NanValue::new_int_inline(i * 3),
                    ],
                );
                let v = NanValue::new_record(idx);
                let (_, fields) = arena.get_record(v.arena_index());
                sum += fields[0].as_int(&arena);
                sum += fields[1].as_int(&arena);
                sum += fields[2].as_int(&arena);
            }
            sum
        });
    });
}

/// Same but with Vec<(String, OldValue)> like old map-based env.
fn bench_record_old(c: &mut Criterion) {
    c.bench_function("record create+access 10k old", |b| {
        b.iter(|| {
            let mut sum: i64 = 0;
            for i in 0..10_000i64 {
                let fields: Vec<(String, OldValue)> = vec![
                    ("x".to_string(), OldValue::Int(i)),
                    ("y".to_string(), OldValue::Int(i * 2)),
                    ("z".to_string(), OldValue::Int(i * 3)),
                ];
                // Linear scan by name (like old map-based env)
                for (_name, val) in &fields {
                    let OldValue::Int(v) = val;
                    sum += v;
                }
            }
            sum
        });
    });
}

/// Measure value size impact — create 1M values.
fn bench_value_creation_nan(c: &mut Criterion) {
    c.bench_function("create 1M NanValues (int)", |b| {
        b.iter(|| {
            let mut values = Vec::with_capacity(1_000_000);
            for i in 0..1_000_000i64 {
                values.push(NanValue::new_int_inline(i));
            }
            values
        });
    });
}

fn bench_value_creation_old(c: &mut Criterion) {
    c.bench_function("create 1M OldValues (int)", |b| {
        b.iter(|| {
            let mut values = Vec::with_capacity(1_000_000);
            for i in 0..1_000_000i64 {
                values.push(OldValue::Int(i));
            }
            values
        });
    });
}

criterion_group!(
    benches,
    bench_fib_nan,
    bench_fib_old,
    bench_record_arena,
    bench_record_old,
    bench_value_creation_nan,
    bench_value_creation_old,
);
criterion_main!(benches);
