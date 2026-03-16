/// Random number generation backed by the OS entropy source.
use rand::Rng;

/// Random integer in `[min, max]` inclusive.
///
/// # Errors
/// Returns `Err` if `min > max`.
pub fn random_int(min: i64, max: i64) -> Result<i64, String> {
    if min > max {
        return Err(format!(
            "Random.int: min ({}) must be <= max ({})",
            min, max
        ));
    }
    Ok(rand::rng().random_range(min..=max))
}

/// Random float in `[0.0, 1.0)`.
pub fn random_float() -> f64 {
    rand::rng().random_range(0.0..1.0)
}
