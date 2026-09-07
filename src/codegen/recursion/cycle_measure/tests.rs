use super::*;
use crate::ast::TopLevel;

fn functions(source: &str) -> Vec<FnDef> {
    crate::source::parse_source(source)
        .unwrap()
        .into_iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect()
}

#[test]
fn slice_aliases_preserve_overlap_across_mutual_calls() {
    let fns = functions(
        r#"fn first(xs: List<Int>, ys: List<Int>) -> Int
    match xs
        [] -> 0
        [head, ..tail] -> match List.take(tail, 10)
            alias -> second(List.drop(alias, 0), tail)
fn second(xs: List<Int>, ys: List<Int>) -> Int
    first(xs, ys)
"#,
    );
    let refs: Vec<_> = fns.iter().collect();
    let candidates = vec![
        vec![
            Candidate {
                index: 0,
                kind: MeasureKind::Structural
            },
            Candidate {
                index: 1,
                kind: MeasureKind::Structural
            },
        ];
        2
    ];
    let measures = measure_for_cycle(&refs, &candidates, false).unwrap();
    for measure in measures {
        assert_eq!(
            measure.params,
            vec![Candidate {
                index: 0,
                kind: MeasureKind::Structural
            }]
        );
    }
}

#[test]
fn slice_origins_keep_the_length_and_sizeof_distinction() {
    let fns = functions(
        r#"fn walk(xs: List<List<Int>>) -> Int
    match xs
        [] -> 0
        [head, ..tail] -> visit(List.drop(head, 0), tail)
"#,
    );
    let names = HashSet::from(["visit".to_string()]);
    for heads_are_parts in [false, true] {
        let sites = call_sites(&fns[0], &names, heads_are_parts);
        assert_eq!(sites.len(), 1);
        let head = relation(sites[0].args[0], &fns[0], &sites[0].scope);
        let tail = relation(sites[0].args[1], &fns[0], &sites[0].scope);
        assert!(tail.strict());
        if heads_are_parts {
            assert!(head.strict());
            let (Relation::Part { path: a, .. }, Relation::Part { path: b, .. }) = (head, tail)
            else {
                panic!("expected parts")
            };
            assert!(!overlap(&a, &b));
        } else {
            assert_eq!(head, Relation::Unknown);
        }
    }
}

#[test]
fn unknown_bindings_and_match_aliases_erase_stale_origins() {
    // The surface checker rejects shadowing; the analysis must remain
    // conservative even for an internal AST that contains it.
    for source in [
        "fn walk(xs: List<Int>) -> Int\n    alias = xs\n    alias = List.concat(xs, xs)\n    walk(alias)\n",
        "fn walk(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [head, ..tail] -> match List.concat(xs, xs)\n            tail -> walk(tail)\n",
    ] {
        let fns = functions(source);
        let names = HashSet::from(["walk".to_string()]);
        let sites = call_sites(&fns[0], &names, false);
        assert_eq!(sites.len(), 1);
        assert_eq!(
            relation(sites[0].args[0], &fns[0], &sites[0].scope),
            Relation::Unknown
        );
    }
}
