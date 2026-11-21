use std::time::Duration;

use aver_rt::AverInt;
use criterion::{Criterion, black_box, criterion_group, criterion_main};
use num_bigint::{BigInt, BigUint, Sign};

/// A value of `len` 64-bit digits, one in `gap` of them nonzero (the top one
/// always), with the given sign: the bit-sliced vectors a Lean kernel in
/// Aver met in CFSG certificates are 2290 digits with about 4% nonzero.
fn value(len: usize, gap: u64, negative: bool) -> AverInt {
    let mut seed = 0x9e37_79b9_7f4a_7c15u64;
    let mut digits = Vec::new();
    for i in 0..len {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        let d = if seed % gap == 0 || i + 1 == len {
            seed | 1
        } else {
            0
        };
        digits.push(d as u32);
        digits.push((d >> 32) as u32);
    }
    let sign = if negative { Sign::Minus } else { Sign::Plus };
    AverInt::from_bigint(BigInt::from_biguint(sign, BigUint::new(digits)))
}

fn bit_reads(c: &mut Criterion) {
    let x = value(2290, 1, false);
    let neg = value(2290, 1, true);
    let mut group = c.benchmark_group("bit reads/2290 digits");
    group.sample_size(20);
    group.measurement_time(Duration::from_secs(2));
    group.bench_function("low_u64", |b| b.iter(|| black_box(&x).low_u64()));
    let m = AverInt::from_i64(1 << 20);
    group.bench_function("mod 2^20", |b| b.iter(|| black_box(&x).rem_euclid(&m)));
    let w = AverInt::from_i64(4096);
    group.bench_function("popCount negative, 4096", |b| {
        b.iter(|| black_box(&neg).pop_count(&w))
    });
    group.finish();
}

fn long_by_short(c: &mut Criterion) {
    let mut group = c.benchmark_group("long by short");
    group.sample_size(20);
    group.measurement_time(Duration::from_secs(2));
    for (len, short, gap, what) in [
        (2290, 21, 25, "sparse"),
        (9500, 13, 25, "sparse"),
        (2290, 21, 5, "a fifth nonzero"),
        (2290, 21, 1, "dense"),
    ] {
        let (x, y) = (value(len, gap, false), value(short, 1, false));
        group.bench_function(format!("{len} x {short} digits, {what}"), |b| {
            b.iter(|| black_box(&x).mul(black_box(&y)))
        });
    }
    group.finish();
}

criterion_group!(benches, bit_reads, long_by_short);
criterion_main!(benches);
