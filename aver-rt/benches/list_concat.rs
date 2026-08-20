use std::time::Duration;

use aver_rt::{AverList, list_uncons};
use criterion::{Criterion, black_box, criterion_group, criterion_main};

const PART_LEN: usize = 41;
// Issue #1020 used 1,400 parts / 57,400 elements. Two hundred preserves the
// same three shapes and made the pre-fix quadratic case finish in ~122 ms, so
// the regression remains locally runnable instead of turning Criterion's
// sampling pass into a multi-minute job.
const PARTS: usize = 200;

fn part() -> AverList<i64> {
    AverList::from_vec((1..=PART_LEN as i64).collect())
}

fn flat() -> AverList<i64> {
    AverList::from_vec((0..PARTS).flat_map(|_| 1..=PART_LEN as i64).collect())
}

fn right_concat() -> AverList<i64> {
    let part = part();
    let mut list = AverList::empty();
    for _ in 0..PARTS {
        list = AverList::concat(&part, &list);
    }
    list
}

fn left_concat() -> AverList<i64> {
    let part = part();
    let mut list = AverList::empty();
    for _ in 0..PARTS {
        list = AverList::concat(&list, &part);
    }
    list
}

fn walk(list: &AverList<i64>) -> i64 {
    let mut rest = list.clone();
    let mut total = 0;
    while let Some((head, tail)) = list_uncons(&rest) {
        total += *head;
        rest = tail;
    }
    total
}

fn list_concat_walk(c: &mut Criterion) {
    let flat = flat();
    let right = right_concat();
    let left = left_concat();
    let expected = PARTS as i64 * (1..=PART_LEN as i64).sum::<i64>();

    assert_eq!(walk(&flat), expected);
    assert_eq!(walk(&right), expected);
    assert_eq!(walk(&left), expected);

    let mut group = c.benchmark_group(format!("list concat walk/{} elements", flat.len()));
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(3));
    group.bench_function("flat", |b| b.iter(|| walk(black_box(&flat))));
    group.bench_function("concat, right", |b| b.iter(|| walk(black_box(&right))));
    group.bench_function("concat, left", |b| b.iter(|| walk(black_box(&left))));
    group.finish();
}

criterion_group!(benches, list_concat_walk);
criterion_main!(benches);
