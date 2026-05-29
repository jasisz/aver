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
    assert!(text.contains("Module: Redis"));
    assert!(text.contains("Kind:  ServiceClient"));
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
    for key in ["module", "facts", "vector", "kind", "histogram", "layer", "fns"] {
        assert!(obj.contains_key(key), "json missing key: {key}");
    }
    assert_eq!(obj["module"], "Redis");
    assert_eq!(obj["kind"]["name"], "ServiceClient");
    assert_eq!(obj["vector"]["type_surface"], "RuntimeHandle");
    assert_eq!(obj["layer"]["basis"], "built-in v0");
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
