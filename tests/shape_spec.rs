//! `aver shape` smoke tests — pins the Kind classifier + layer
//! inference for three flagship examples that exercise the four
//! discriminating dimensions of `ModuleShape`.

use std::path::Path;

use aver::diagnostics::shape::{
    self, ApiShape, Entry, Kind, Purity, RenderOptions, StateShape, TypeSurface,
};

fn analyze(rel: &str) -> shape::ShapeReport {
    let path = Path::new(rel);
    shape::analyze_path(path, None).unwrap_or_else(|e| panic!("analyze_path({rel}): {e}"))
}

#[test]
fn natural_is_smart_constructor() {
    // `examples/refinement/natural/natural.av` declares
    // `exposes opaque [Natural]` + smart constructor `fromInt`.
    let r = analyze("examples/refinement/natural/natural.av");
    assert!(matches!(r.shape.purity, Purity::Pure));
    assert!(matches!(r.shape.state_shape, StateShape::Stateless));
    assert!(matches!(r.shape.type_surface, TypeSurface::UserOpaque));
    assert_eq!(r.kind, Kind::SmartConstructor);
}

#[test]
fn weather_is_orchestration_with_shell_effects() {
    // weather.av declares `effects [Console, Http, HttpServer, Tcp]`.
    // `HttpServer.listen*` is a shell/lifecycle effect (long-running,
    // not Oracle-classifiable by design — see memory), so purity must
    // be ShellEffectful, NOT something that suggests "compiler can't
    // recognize it" (the classifier knows every Aver effect).
    let r = shape::analyze_path(
        std::path::Path::new("examples/services/weather.av"),
        Some("examples"),
    )
    .expect("weather analyze");
    assert!(
        matches!(r.shape.purity, Purity::ShellEffectful),
        "weather mixes Oracle (Http.get, Tcp.*) with shell (HttpServer.listenWith) — must be ShellEffectful, got {:?}",
        r.shape.purity
    );
    assert!(matches!(r.shape.entry, Entry::Main));
    // Tcp.Connection is threaded internally (via WeatherContext) but the
    // exposed surface is `[main]` — not a service-client API. Kind must
    // be Orchestration, not ServiceClient.
    assert_eq!(r.kind, Kind::Orchestration);
}

#[test]
fn redis_is_service_client() {
    let r = analyze("examples/services/redis.av");
    assert!(
        matches!(r.shape.purity, Purity::ClassifiedEffectful),
        "redis must be classified-effectful, got {:?}",
        r.shape.purity
    );
    assert!(
        matches!(r.shape.type_surface, TypeSurface::RuntimeHandle),
        "redis threads Tcp.Connection through all API"
    );
    assert!(
        matches!(r.shape.api_shape, ApiShape::ServiceBoundary),
        "RuntimeHandle should force ServiceBoundary"
    );
    assert_eq!(r.kind, Kind::ServiceClient);
}

#[test]
fn render_text_includes_kind_and_layer() {
    let r = analyze("examples/services/redis.av");
    let text = shape::render_text(&r, &RenderOptions { summary: false });
    assert!(text.contains("Module:  Redis"));
    assert!(text.contains("Kind:    ServiceClient"));
    assert!(text.contains("ModuleShape:"));
    assert!(text.contains("purity        ClassifiedEffectful"));
    assert!(text.contains("type_surface  RuntimeHandle"));
    assert!(text.contains("Histogram"));
    assert!(text.contains("Layer:"));
    assert!(text.contains("basis: built-in v0"));
}

#[test]
fn render_text_summary_omits_per_fn_list() {
    let r = analyze("examples/services/redis.av");
    let full = shape::render_text(&r, &RenderOptions { summary: false });
    let summary = shape::render_text(&r, &RenderOptions { summary: true });
    assert!(full.contains("Functions:"));
    assert!(!summary.contains("Functions:"));
    // Both must still carry Kind + histogram.
    for s in &[&full, &summary] {
        assert!(s.contains("Kind:"));
        assert!(s.contains("Histogram"));
    }
}

#[test]
fn render_json_has_facts_vector_kind_and_layer() {
    let r = analyze("examples/services/redis.av");
    let json = shape::render_json(&r);
    let obj = json.as_object().expect("json must be an object");
    for key in [
        "module",
        "facts",
        "vector",
        "kind",
        "histogram",
        "layer",
        "fns",
    ] {
        assert!(obj.contains_key(key), "json missing key: {key}");
    }
    assert_eq!(obj["module"], "Redis");
    assert_eq!(obj["kind"]["name"], "ServiceClient");
    assert_eq!(obj["vector"]["type_surface"], "RuntimeHandle");
    assert_eq!(obj["layer"]["basis"], "built-in v0");
}

#[test]
fn corpus_mode_walks_directory_and_aggregates() {
    use aver::diagnostics::shape::{self, CorpusEntry};
    // examples/services/ has redis.av (ServiceClient), weather.av,
    // http_demo.av, disk_demo.av, independent_products.av. Some need
    // module-root=examples for `Services.Redis` resolution; those that
    // can't resolve land as Skipped, the corpus walk still completes.
    let entries = shape::analyze_dir(
        std::path::Path::new("examples/services"),
        Some("examples"),
        &shape::builtin_v0_layer_fingerprints(),
        "built-in v0",
    )
    .expect("analyze_dir");
    assert!(
        entries.len() >= 3,
        "expected at least 3 .av files in examples/services"
    );
    // At least one file must analyze cleanly.
    let analyzed: Vec<_> = entries
        .iter()
        .filter(|e| matches!(e, CorpusEntry::Analyzed { .. }))
        .collect();
    assert!(!analyzed.is_empty(), "expected at least one analyzed file");

    let summary = shape::summarize_corpus(&entries);
    assert!(summary.total_files >= 3);
    assert_eq!(
        summary.analyzed_files + summary.skipped_files,
        summary.total_files
    );
    if summary.analyzed_files > 0 {
        assert!(
            summary.total_fns > 0,
            "analyzed files should yield fn classifications"
        );
    }
}

#[test]
fn corpus_mode_renders_text_with_summary() {
    use aver::diagnostics::shape::{self, RenderOptions};
    let entries = shape::analyze_dir(
        std::path::Path::new("examples/services"),
        Some("examples"),
        &shape::builtin_v0_layer_fingerprints(),
        "built-in v0",
    )
    .expect("analyze_dir");
    let full = shape::render_corpus_text(&entries, &RenderOptions { summary: false });
    let only_summary = shape::render_corpus_text(&entries, &RenderOptions { summary: true });
    // Both views must carry the global summary block.
    for s in &[&full, &only_summary] {
        assert!(s.contains("Corpus summary:"));
        assert!(s.contains("Kind distribution") || s.contains("analyzed,"));
    }
    // Full view also lists per-file rows.
    assert!(
        full.contains("Corpus: ") || full.contains("layer:"),
        "full view must include per-file table"
    );
}

#[test]
fn project_layer_override_loads_from_config() {
    use aver::config::ShapeLayerFingerprint;
    // A clearly-non-default fingerprint: claim that "Parse" is 100%
    // helpers. The classifier should pick this up over the built-in v0.
    let entries = vec![
        ShapeLayerFingerprint {
            name: "Parse".to_string(),
            match_pct: 0.0,
            recursion_pct: 0.0,
            pipeline_pct: 0.0,
            orchestration_pct: 0.0,
            helpers_pct: 100.0,
        },
        ShapeLayerFingerprint {
            name: "Domain".to_string(),
            match_pct: 100.0,
            recursion_pct: 0.0,
            pipeline_pct: 0.0,
            orchestration_pct: 0.0,
            helpers_pct: 0.0,
        },
    ];
    let fps = shape::fingerprints_from_config(&entries).unwrap();
    assert_eq!(fps.len(), 2);
    // Sanity: shape::Layer round-trips through Layer::parse for the names
    // we just declared.
    use aver::diagnostics::shape::Layer;
    assert_eq!(fps[0].layer, Layer::Parse);
    assert_eq!(fps[1].layer, Layer::Domain);
}

#[test]
fn project_layer_override_rejects_unknown_layer_name() {
    use aver::config::ShapeLayerFingerprint;
    let entries = vec![ShapeLayerFingerprint {
        name: "DefinitelyNotALayer".to_string(),
        match_pct: 0.0,
        recursion_pct: 0.0,
        pipeline_pct: 0.0,
        orchestration_pct: 0.0,
        helpers_pct: 100.0,
    }];
    let err = shape::fingerprints_from_config(&entries).unwrap_err();
    assert!(
        err.contains("DefinitelyNotALayer") && err.contains("known Layer"),
        "expected typo-detection error, got: {}",
        err,
    );
}

#[test]
fn pure_module_with_demo_main_is_not_orchestration() {
    // quicksort.av has a `main()` that prints the sorted result, but
    // the rest of the module is pure recursive helpers. Pre-redesign
    // it classified as Orchestration (any module with main + classified
    // effects); post-redesign the effectful-fn ratio is below 30% so
    // the demo main doesn't drag the module into Orchestration.
    let r = analyze("examples/data/quicksort.av");
    assert!(r.has_main, "quicksort.av declares main()");
    assert!(
        r.effectful_fn_ratio < 0.3,
        "expected < 30% effectful non-main fns, got {:.2}",
        r.effectful_fn_ratio,
    );
    assert!(
        !matches!(r.kind, Kind::Orchestration),
        "quicksort is library-with-demo, must NOT be Orchestration; got {:?}",
        r.kind
    );
}

#[test]
fn layer_verdict_includes_runners_up_and_margin() {
    use aver::diagnostics::shape;
    let r = analyze("examples/services/redis.av");
    let verdict = r
        .layer
        .clone()
        .expect("redis has fns, layer must be inferred");
    // Top-3 candidates with distances.
    assert!(
        verdict.candidates.len() >= 2,
        "expected at least 2 candidates, got {}",
        verdict.candidates.len()
    );
    // Margin is the distance gap between best and runner-up.
    let best_dist = verdict.candidates[0].1;
    let runner_dist = verdict.candidates[1].1;
    assert!(
        (verdict.margin - (runner_dist - best_dist)).abs() < 1e-9,
        "margin should equal runner-up distance minus best distance",
    );
    // Render must include "next:" runners-up line.
    let text = shape::render_text(&r, &shape::RenderOptions { summary: false });
    assert!(
        text.contains("next:"),
        "render_text must include runners-up line, got:\n{}",
        text
    );
}

#[test]
fn low_confidence_or_low_margin_marks_layer_uncertain() {
    // calculator.av has 3 non-main fns — small-N penalty caps
    // confidence at 0.2, which trips the uncertain flag.
    let r = analyze("examples/core/calculator.av");
    if let Some(verdict) = &r.layer {
        if verdict.confidence < 0.4 || verdict.margin < 10.0 {
            assert!(
                verdict.uncertain,
                "low-confidence/low-margin verdict must be uncertain"
            );
            let text = shape::render_text(&r, &shape::RenderOptions { summary: false });
            assert!(
                text.contains("Layer: uncertain"),
                "uncertain verdict must surface 'Layer: uncertain' wording, got:\n{}",
                text
            );
            assert!(
                text.contains("best:"),
                "uncertain wording must include 'best:' label, got:\n{}",
                text
            );
        }
    }
}

#[test]
fn small_module_layer_confidence_is_penalized() {
    // <5 fns → confidence capped at 0.2 regardless of fit. We can't
    // easily synthesize a 3-fn module from disk without a fixture
    // file, so target an existing tiny module.
    let r = analyze("examples/core/result_pipeline.av");
    if let Some(verdict) = &r.layer {
        if r.histogram.total_fns < 5 {
            assert!(
                verdict.confidence <= 0.21,
                "tiny module confidence must be penalized, got {:.2} on {} fns",
                verdict.confidence,
                r.histogram.total_fns,
            );
        }
    }
}
