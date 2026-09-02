//! The plan-equals-source bridge statement, rendered from declared structure.
//!
//! A certificate does not transport the text of a bridge theorem. It transports
//! the STRUCTURE — the certified export, the transpiled source function, and one
//! encoder spec per parameter plus one for the result — and both sides render
//! the statement from that structure with this module. The producer writes the
//! rendered text into `Bridge.lean`; the checker renders it again and pins the
//! package's corollary at exactly that type.
//!
//! That is the whole point of the split. A statement accepted as text can say
//! anything the gates do not forbid — `f x = f x` is a single `_root_.`-first
//! line naming its declared model, and it proves nothing. A statement RENDERED
//! by the checker can only ever say what this file says: the plan named by the
//! export, at the encoded arguments, is the encoded source result. A manifest
//! that permutes a record's accessors, points at another export's plan, or
//! declares an encoder kind this file does not know renders a different text (or
//! no text at all), and the pin then fails to elaborate — which declines the
//! package rather than crediting the claim.
//!
//! This module is compiled unconditionally, like [`crate::format`], so the
//! producer feature and the verifier feature share one renderer rather than two
//! that have to be kept byte-identical by hand.

/// Manifest key carrying an encoder's kind tag.
pub const ENCODER_KIND_KEY: &str = "kind";
/// Manifest key carrying a record encoder's Lean type.
pub const ENCODER_TYPE_KEY: &str = "type";
/// Manifest key carrying a record encoder's accessor list.
pub const ENCODER_FIELDS_KEY: &str = "fields";

/// The three encoder kinds the v1 projection-compute face admits. The set is
/// CLOSED: a manifest naming anything else is refused, never rendered.
pub const ENCODER_KIND_INT: &str = "int";
pub const ENCODER_KIND_BOOL: &str = "bool";
pub const ENCODER_KIND_RECORD: &str = "record";

/// Longest name a bridge entry may carry, matching the law surface's cap.
pub const MAX_BRIDGE_NAME_LEN: usize = 200;
/// Longest rendered statement the bridge surface admits, matching the law
/// surface's cap: it bounds what the anti-injection gate has to police inside
/// one pinned type.
pub const MAX_BRIDGE_STATEMENT_LEN: usize = 2000;

/// The `_root_.` prefix every Lean name inside a bridge entry carries, so the
/// rendered statement means the same at the root (where the checker's pin
/// elaborates) as inside the package's own namespaces.
pub const ROOT_PREFIX: &str = "_root_.";

/// How one source value of the face's admitted shapes is encoded as the wall's
/// `RecordComputeBridge.SVal`. These are the ONLY three shapes the v1
/// projection-compute face carries; anything else (a nested record, a record
/// with a non-Int field, a Float or String leaf) has no `SVal` image and gets no
/// bridge rather than an invented encoding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceEncoder {
    /// `Int ↦ SVal.i`.
    Int,
    /// `Bool ↦ SVal.b`.
    Bool,
    /// An all-Int record ↦ `SVal.r` of its accessors in DECLARATION order,
    /// which is the order the emitter packs the wasm struct in.
    Record {
        /// `_root_.`-qualified Lean type of the record.
        lean_type: String,
        /// `_root_.`-qualified accessors, each a field of `lean_type`.
        accessors: Vec<String>,
    },
}

impl SourceEncoder {
    /// The manifest kind tag of this encoder.
    pub fn kind(&self) -> &'static str {
        match self {
            SourceEncoder::Int => ENCODER_KIND_INT,
            SourceEncoder::Bool => ENCODER_KIND_BOOL,
            SourceEncoder::Record { .. } => ENCODER_KIND_RECORD,
        }
    }

    /// The Lean type a binder of this encoder is declared at.
    pub fn binder_type(&self) -> &str {
        match self {
            SourceEncoder::Int => "Int",
            SourceEncoder::Bool => "Bool",
            SourceEncoder::Record { lean_type, .. } => lean_type.as_str(),
        }
    }

    /// The `SVal` term for the source value `value` (already a Lean term).
    pub fn encode(&self, value: &str) -> String {
        match self {
            SourceEncoder::Int => {
                format!("_root_.RecordComputeBridge.SVal.i ({value})")
            }
            SourceEncoder::Bool => {
                format!("_root_.RecordComputeBridge.SVal.b ({value})")
            }
            SourceEncoder::Record { accessors, .. } => {
                let mut leaves = String::new();
                for (index, accessor) in accessors.iter().enumerate() {
                    if index > 0 {
                        leaves.push_str(", ");
                    }
                    leaves.push_str(accessor);
                    leaves.push_str(" (");
                    leaves.push_str(value);
                    leaves.push(')');
                }
                format!("_root_.RecordComputeBridge.SVal.r [{leaves}]")
            }
        }
    }

    /// Whether every name this encoder splices into the rendered statement is a
    /// `_root_.`-qualified plain Lean identifier, and — for a record — whether
    /// each accessor is a field OF the declared type rather than of some
    /// unrelated one. The renderer copies these names verbatim, so this is the
    /// gate that keeps the rendered text a plain term.
    pub fn is_well_formed(&self) -> bool {
        match self {
            SourceEncoder::Int | SourceEncoder::Bool => true,
            SourceEncoder::Record {
                lean_type,
                accessors,
            } => {
                is_root_qualified_name(lean_type)
                    && !accessors.is_empty()
                    && accessors.iter().all(|accessor| {
                        is_root_qualified_name(accessor)
                            && accessor
                                .strip_prefix(lean_type.as_str())
                                .and_then(|rest| rest.strip_prefix('.'))
                                .is_some_and(|field| !field.is_empty() && !field.contains('.'))
                    })
            }
        }
    }
}

/// Whether `value` is a `_root_.`-qualified plain dotted Lean identifier.
pub fn is_root_qualified_name(value: &str) -> bool {
    value.len() <= MAX_BRIDGE_NAME_LEN
        && value.starts_with(ROOT_PREFIX)
        && is_plain_dotted_name(value)
}

/// Whether `value` is a plain dotted Lean identifier: every `.`-separated
/// segment nonempty, starting with an ASCII letter or `_`, and continuing with
/// ASCII alphanumerics or `_`.
pub fn is_plain_dotted_name(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= MAX_BRIDGE_NAME_LEN
        && value.split('.').all(|segment| {
            let mut chars = segment.chars();
            matches!(chars.next(), Some(first) if first.is_ascii_alphabetic() || first == '_')
                && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
        })
}

/// The binder names a bridge of this arity quantifies over.
pub fn binder_names(arity: usize) -> Vec<String> {
    (0..arity).map(|index| format!("x{index}")).collect()
}

/// `[enc x0, enc x1, …]` — the encoded argument list.
pub fn encoded_args(params: &[SourceEncoder]) -> String {
    let mut encoded = String::new();
    for (index, encoder) in params.iter().enumerate() {
        if index > 0 {
            encoded.push_str(", ");
        }
        encoded.push_str(&encoder.encode(&format!("x{index}")));
    }
    format!("[{encoded}]")
}

/// `_root_.<Module>.<fn> x0 x1` — the source call at the binders.
pub fn source_call(model: &str, arity: usize) -> String {
    let args = binder_names(arity).join(" ");
    if args.is_empty() {
        format!("{ROOT_PREFIX}{model}")
    } else {
        format!("{ROOT_PREFIX}{model} {args}")
    }
}

/// The Lean name of the plan a bridged export's obligation evaluates.
pub fn plan_body_name(export: &str) -> String {
    format!("{ROOT_PREFIX}AverCert.Plans.{export}Plan.body")
}

/// The bridge statement for one export: the plan its obligation evaluates,
/// applied to the encoded arguments, is the encoded source result.
///
/// This is the single definition of what a bridge SAYS. Both the producer's
/// `Bridge.lean` and the checker's `bridge_pin_<i>` are rendered from it, so the
/// two agree by construction rather than by comparison.
pub fn render_bridge_statement(
    export: &str,
    model: &str,
    params: &[SourceEncoder],
    result: &SourceEncoder,
) -> String {
    let mut binders = String::new();
    for (index, encoder) in params.iter().enumerate() {
        if index > 0 {
            binders.push(' ');
        }
        binders.push_str("(x");
        binders.push_str(&index.to_string());
        binders.push_str(" : ");
        binders.push_str(encoder.binder_type());
        binders.push(')');
    }
    let quantifier = if binders.is_empty() {
        String::new()
    } else {
        format!("∀ {binders}, ")
    };
    format!(
        "{quantifier}_root_.AverCert.StandardFace.recordComputeModel {} {} = \
         _root_.Option.some ({})",
        plan_body_name(export),
        encoded_args(params),
        result.encode(&source_call(model, params.len())),
    )
}

/// The statement gate both the producer and the checker apply to the RENDERED
/// text: one plain term-position line, with balanced delimiters so it cannot
/// escape the single `(...)` the pin wraps it in.
///
/// The renderer only ever splices gated names into a fixed skeleton, so this is
/// a backstop rather than the primary defence — but it is the backstop that
/// makes the pin's shape independent of any future encoder.
pub fn statement_is_single_plain_line(statement: &str, max_len: usize) -> bool {
    if statement.is_empty()
        || statement.len() > max_len
        || statement.chars().any(char::is_control)
        || statement.contains(":=")
        || statement.contains("--")
        || statement.contains("/-")
    {
        return false;
    }
    let mut depth: Vec<char> = Vec::new();
    for character in statement.chars() {
        let matched = match character {
            '(' | '[' | '{' | '⟨' => {
                depth.push(character);
                true
            }
            ')' => depth.pop() == Some('('),
            ']' => depth.pop() == Some('['),
            '}' => depth.pop() == Some('{'),
            '⟩' => depth.pop() == Some('⟨'),
            _ => true,
        };
        if !matched {
            return false;
        }
    }
    depth.is_empty()
}

/// Whether every dotted name in a statement is spelled `_root_.`-first.
///
/// A bridge pin elaborates at the ROOT namespace with no `open`, so an
/// unqualified dotted name in its statement would be resolved against whatever
/// the package's own namespaces declare. Undotted tokens are binders and
/// keywords and are left alone.
pub fn statement_is_root_qualified(statement: &str) -> bool {
    statement
        .split(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '\''))
        .filter(|token| token.contains('.'))
        .all(|token| token.starts_with(ROOT_PREFIX))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fraction() -> SourceEncoder {
        SourceEncoder::Record {
            lean_type: "_root_.Domain.Rational.Fraction".to_string(),
            accessors: vec![
                "_root_.Domain.Rational.Fraction.top".to_string(),
                "_root_.Domain.Rational.Fraction.bottom".to_string(),
            ],
        }
    }

    /// The rendered text is the claim. This pins it verbatim for every encoder
    /// kind, in every position, so a change to the renderer has to be a
    /// deliberate edit of an expected string rather than a silent reshaping of
    /// what every certificate says.
    #[test]
    fn every_encoder_kind_renders_its_exact_statement() {
        assert_eq!(
            render_bridge_statement(
                "addOne",
                "CertificateHello.addOne",
                &[SourceEncoder::Int],
                &SourceEncoder::Int
            ),
            "∀ (x0 : Int), _root_.AverCert.StandardFace.recordComputeModel \
             _root_.AverCert.Plans.addOnePlan.body \
             [_root_.RecordComputeBridge.SVal.i (x0)] = _root_.Option.some \
             (_root_.RecordComputeBridge.SVal.i (_root_.CertificateHello.addOne x0))"
        );
        assert_eq!(
            render_bridge_statement(
                "Domain_Rational_lessThan",
                "Domain.Rational.lessThan",
                &[fraction(), fraction()],
                &SourceEncoder::Bool,
            ),
            "∀ (x0 : _root_.Domain.Rational.Fraction) (x1 : _root_.Domain.Rational.Fraction), \
             _root_.AverCert.StandardFace.recordComputeModel \
             _root_.AverCert.Plans.Domain_Rational_lessThanPlan.body \
             [_root_.RecordComputeBridge.SVal.r [_root_.Domain.Rational.Fraction.top (x0), \
             _root_.Domain.Rational.Fraction.bottom (x0)], \
             _root_.RecordComputeBridge.SVal.r [_root_.Domain.Rational.Fraction.top (x1), \
             _root_.Domain.Rational.Fraction.bottom (x1)]] = _root_.Option.some \
             (_root_.RecordComputeBridge.SVal.b (_root_.Domain.Rational.lessThan x0 x1))"
        );
        // Nullary: no quantifier, and the source call is the bare name.
        assert_eq!(
            render_bridge_statement(
                "Domain_Rational_zeroFraction",
                "Domain.Rational.zeroFraction",
                &[],
                &fraction(),
            ),
            "_root_.AverCert.StandardFace.recordComputeModel \
             _root_.AverCert.Plans.Domain_Rational_zeroFractionPlan.body [] = _root_.Option.some \
             (_root_.RecordComputeBridge.SVal.r \
             [_root_.Domain.Rational.Fraction.top (_root_.Domain.Rational.zeroFraction), \
             _root_.Domain.Rational.Fraction.bottom (_root_.Domain.Rational.zeroFraction)])"
        );
    }

    /// The three ways a hostile manifest could try to make the renderer say
    /// something else, and the fact that it cannot: the statement changes, so
    /// the pin no longer has the package corollary's type.
    #[test]
    fn structure_edits_change_the_rendered_statement() {
        let honest = render_bridge_statement(
            "Domain_Rational_plus",
            "Domain.Rational.plus",
            &[fraction(), fraction()],
            &fraction(),
        );
        // A different plan.
        assert_ne!(
            honest,
            render_bridge_statement(
                "Domain_Rational_minus",
                "Domain.Rational.plus",
                &[fraction(), fraction()],
                &fraction(),
            )
        );
        // A different source function.
        assert_ne!(
            honest,
            render_bridge_statement(
                "Domain_Rational_plus",
                "Domain.Rational.minus",
                &[fraction(), fraction()],
                &fraction(),
            )
        );
        // Permuted record accessors.
        let permuted = SourceEncoder::Record {
            lean_type: "_root_.Domain.Rational.Fraction".to_string(),
            accessors: vec![
                "_root_.Domain.Rational.Fraction.bottom".to_string(),
                "_root_.Domain.Rational.Fraction.top".to_string(),
            ],
        };
        assert_ne!(
            honest,
            render_bridge_statement(
                "Domain_Rational_plus",
                "Domain.Rational.plus",
                &[permuted, fraction()],
                &fraction(),
            )
        );
        // A tautology is unrepresentable: the left-hand side is always the
        // plan's model, never the source call.
        assert!(honest.contains("recordComputeModel _root_.AverCert.Plans."));
    }

    #[test]
    fn record_encoder_accessors_must_belong_to_the_declared_type() {
        assert!(fraction().is_well_formed());
        assert!(SourceEncoder::Int.is_well_formed());
        assert!(SourceEncoder::Bool.is_well_formed());
        // An accessor of an unrelated type.
        assert!(
            !SourceEncoder::Record {
                lean_type: "_root_.Domain.Rational.Fraction".to_string(),
                accessors: vec!["_root_.Other.Type.top".to_string()],
            }
            .is_well_formed()
        );
        // A nested accessor is not a field of the type.
        assert!(
            !SourceEncoder::Record {
                lean_type: "_root_.Domain.Rational.Fraction".to_string(),
                accessors: vec!["_root_.Domain.Rational.Fraction.top.inner".to_string()],
            }
            .is_well_formed()
        );
        // Unqualified names would mean whatever the package's namespaces say.
        assert!(
            !SourceEncoder::Record {
                lean_type: "Domain.Rational.Fraction".to_string(),
                accessors: vec!["Domain.Rational.Fraction.top".to_string()],
            }
            .is_well_formed()
        );
        // A record with no leaves has no `SVal.r` image.
        assert!(
            !SourceEncoder::Record {
                lean_type: "_root_.Domain.Rational.Fraction".to_string(),
                accessors: Vec::new(),
            }
            .is_well_formed()
        );
    }

    #[test]
    fn rendered_statements_pass_the_gates_they_are_pinned_under() {
        let statement = render_bridge_statement(
            "Domain_Rational_plus",
            "Domain.Rational.plus",
            &[fraction(), fraction()],
            &fraction(),
        );
        assert!(statement_is_single_plain_line(
            &statement,
            MAX_BRIDGE_STATEMENT_LEN
        ));
        assert!(statement_is_root_qualified(&statement));
    }
}
