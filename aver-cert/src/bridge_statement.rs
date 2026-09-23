//! The plan-equals-source bridge statement, rendered from declared structure.
//!
//! A certificate does not transport the text of a bridge theorem. It transports
//! the STRUCTURE — the certified export, the transpiled source function, the
//! statement kind, and one encoder per parameter plus one for the result — and
//! both sides render the statement from that structure with this module. The
//! producer writes the rendered text into `Bridge.lean`; the checker renders it
//! again and pins the package's corollary at exactly that type.
//!
//! That is the whole point of the split. A statement accepted as text can say
//! anything the gates do not forbid — `f x = f x` is a single `_root_.`-first
//! line naming its declared model, and it proves nothing. A statement RENDERED
//! by the checker can only ever say what this file says: the model of the
//! obligation named by the export, at the encoded arguments, returns the
//! encoded source result. A manifest that permutes a record's accessors, points
//! at another export, or declares an encoder kind this file does not know
//! renders a different text (or no text at all), and the pin then fails to
//! elaborate — which declines the package rather than crediting the claim.
//!
//! The statement is over the plan grammar of statement schema 9
//! (`GrammarBridge.lean`), in one of two kinds:
//!
//! * `exact` — above some fuel, the model at every encoded argument list
//!   returns exactly the encoded source result. Proved for plans whose call
//!   closure has no recursion.
//! * `adequate` — every result the model returns (at any fuel) on an encoded
//!   argument list is the encoded source result. Proved for any plan, recursive
//!   or not; together with the obligation's `holds` it means that whatever the
//!   bytes return on represented source arguments represents the source
//!   result (`GrammarBridge.adequate_transfer`). It is never a totality claim.
//!
//! Both kinds also say that every encoded argument list inhabits the plan's
//! parameter types, so the obligation's `holds` applies to it.
//!
//! This module is compiled unconditionally, like [`crate::format`], so the
//! producer feature and the verifier feature share one renderer rather than two
//! that have to be kept byte-identical by hand.

/// Manifest key carrying an encoder's kind tag.
pub const ENCODER_KIND_KEY: &str = "kind";

/// The closed encoder kind set. A manifest naming anything else is refused,
/// never rendered.
pub const ENCODER_KIND_INT: &str = "int";
pub const ENCODER_KIND_BOOL: &str = "bool";
pub const ENCODER_KIND_FLOAT: &str = "float";
pub const ENCODER_KIND_STRING: &str = "string";
pub const ENCODER_KIND_RECORD: &str = "record";
pub const ENCODER_KIND_SUM: &str = "sum";
pub const ENCODER_KIND_OPTION: &str = "option";
pub const ENCODER_KIND_RESULT: &str = "result";
pub const ENCODER_KIND_TUPLE: &str = "tuple";
pub const ENCODER_KIND_LIST: &str = "list";
pub const ENCODER_KIND_VECTOR: &str = "vector";

/// The two statement kinds, by manifest tag.
pub const BRIDGE_KIND_EXACT: &str = "exact";
pub const BRIDGE_KIND_ADEQUATE: &str = "adequate";

/// Longest name a bridge entry may carry, matching the law surface's cap.
pub const MAX_BRIDGE_NAME_LEN: usize = 200;
/// Longest rendered statement the bridge surface admits. It bounds what the
/// anti-injection gate has to police inside one pinned type.
pub const MAX_BRIDGE_STATEMENT_LEN: usize = 16000;
/// Deepest encoder nesting a manifest may declare.
pub const MAX_ENCODER_DEPTH: usize = 8;
/// Most nodes one encoder tree may carry.
pub const MAX_ENCODER_NODES: usize = 256;

/// The `_root_.` prefix every Lean name inside a bridge entry carries, so the
/// rendered statement means the same at the root (where the checker's pin
/// elaborates) as inside the package's own namespaces.
pub const ROOT_PREFIX: &str = "_root_.";

const SVAL: &str = "_root_.AverCert.Grammar.SVal";
const TY: &str = "_root_.AverCert.Grammar.Ty";

/// Which of the two statement kinds a bridge claims.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BridgeKind {
    Exact,
    Adequate,
}

impl BridgeKind {
    pub fn tag(self) -> &'static str {
        match self {
            BridgeKind::Exact => BRIDGE_KIND_EXACT,
            BridgeKind::Adequate => BRIDGE_KIND_ADEQUATE,
        }
    }

    pub fn from_tag(tag: &str) -> Option<Self> {
        match tag {
            BRIDGE_KIND_EXACT => Some(BridgeKind::Exact),
            BRIDGE_KIND_ADEQUATE => Some(BridgeKind::Adequate),
            _ => None,
        }
    }
}

/// How one source value is encoded as the wall's `Grammar.SVal`. The set
/// mirrors the plan grammar's value forms; a type with no form here (a
/// recursive type, a map, an opaque value) has no encoder and gets no bridge
/// rather than an invented encoding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceEncoder {
    /// `Int ↦ SVal.i`.
    Int,
    /// `Bool ↦ SVal.b`.
    Bool,
    /// `Float ↦ SVal.f` of its IEEE-754 bits.
    Float,
    /// `String ↦ SVal.s` of its UTF-8 bytes (`GrammarBridge.strBytes`).
    Str,
    /// A record (type id `tid` of the plan's type table) ↦ `SVal.record` of
    /// its fields in DECLARATION order, which is the order the emitter packs
    /// the struct in. A one-field record is a newtype and has one field here.
    Record {
        tid: u32,
        /// `_root_.`-qualified Lean structure.
        lean_type: String,
        /// `_root_.`-qualified accessor and the field's encoder, in order.
        fields: Vec<(String, SourceEncoder)>,
    },
    /// A user sum ↦ `SVal.variant tid c` of the constructor's fields, where
    /// `c` is the constructor's position here (declaration order).
    Sum {
        tid: u32,
        lean_type: String,
        /// `_root_.`-qualified constructor and its field encoders.
        ctors: Vec<(String, Vec<SourceEncoder>)>,
    },
    /// `Option T ↦ SVal.none` / `SVal.some`, instantiated at the element's
    /// plan type.
    Option(Box<SourceEncoder>),
    /// Aver `Result<T, E>` (Lean `Except E T`) ↦ `SVal.ok` / `SVal.err`.
    Result {
        ok: Box<SourceEncoder>,
        err: Box<SourceEncoder>,
    },
    /// A tuple (Lean nested `×`) ↦ `SVal.record tid` of its components.
    Tuple { tid: u32, elems: Vec<SourceEncoder> },
    /// `List T ↦ SVal.nil` / `SVal.cons`.
    List(Box<SourceEncoder>),
    /// `Array T ↦ SVal.vec` of the elements.
    Vector(Box<SourceEncoder>),
}

impl SourceEncoder {
    /// The manifest kind tag of this encoder.
    pub fn kind(&self) -> &'static str {
        match self {
            SourceEncoder::Int => ENCODER_KIND_INT,
            SourceEncoder::Bool => ENCODER_KIND_BOOL,
            SourceEncoder::Float => ENCODER_KIND_FLOAT,
            SourceEncoder::Str => ENCODER_KIND_STRING,
            SourceEncoder::Record { .. } => ENCODER_KIND_RECORD,
            SourceEncoder::Sum { .. } => ENCODER_KIND_SUM,
            SourceEncoder::Option(_) => ENCODER_KIND_OPTION,
            SourceEncoder::Result { .. } => ENCODER_KIND_RESULT,
            SourceEncoder::Tuple { .. } => ENCODER_KIND_TUPLE,
            SourceEncoder::List(_) => ENCODER_KIND_LIST,
            SourceEncoder::Vector(_) => ENCODER_KIND_VECTOR,
        }
    }

    /// The Lean type a binder of this encoder is declared at.
    pub fn binder_type(&self) -> String {
        match self {
            SourceEncoder::Int => "_root_.Int".into(),
            SourceEncoder::Bool => "_root_.Bool".into(),
            SourceEncoder::Float => "_root_.Float".into(),
            SourceEncoder::Str => "_root_.String".into(),
            SourceEncoder::Record { lean_type, .. } | SourceEncoder::Sum { lean_type, .. } => {
                lean_type.clone()
            }
            SourceEncoder::Option(elem) => format!("(_root_.Option {})", elem.binder_type()),
            SourceEncoder::Result { ok, err } => {
                format!("(_root_.Except {} {})", err.binder_type(), ok.binder_type())
            }
            SourceEncoder::Tuple { elems, .. } => format!(
                "({})",
                elems
                    .iter()
                    .map(SourceEncoder::binder_type)
                    .collect::<Vec<_>>()
                    .join(" × ")
            ),
            SourceEncoder::List(elem) => format!("(_root_.List {})", elem.binder_type()),
            SourceEncoder::Vector(elem) => format!("(_root_.Array {})", elem.binder_type()),
        }
    }

    /// The plan-grammar type (`Grammar.Ty`) of an encoded value.
    pub fn grammar_ty(&self) -> String {
        match self {
            SourceEncoder::Int => format!("{TY}.int"),
            SourceEncoder::Bool => format!("{TY}.bool"),
            SourceEncoder::Float => format!("{TY}.float"),
            SourceEncoder::Str => format!("{TY}.string"),
            SourceEncoder::Record { tid, .. } | SourceEncoder::Tuple { tid, .. } => {
                format!("({TY}.record {tid})")
            }
            SourceEncoder::Sum { tid, .. } => format!("({TY}.sum {tid})"),
            SourceEncoder::Option(elem) => format!("({TY}.option {})", elem.grammar_ty()),
            SourceEncoder::Result { ok, err } => {
                format!("({TY}.result {} {})", ok.grammar_ty(), err.grammar_ty())
            }
            SourceEncoder::List(elem) => format!("({TY}.list {})", elem.grammar_ty()),
            SourceEncoder::Vector(elem) => format!("({TY}.vec {})", elem.grammar_ty()),
        }
    }

    /// The `SVal` term for the source value `value` (already a Lean term).
    /// `fresh` numbers the pattern binders a sum, option, result, list or
    /// vector encoder introduces, so nested encoders never shadow each other.
    pub fn encode(&self, value: &str, fresh: &mut usize) -> String {
        let bind = |fresh: &mut usize| {
            let name = format!("y{fresh}");
            *fresh += 1;
            name
        };
        match self {
            SourceEncoder::Int => format!("{SVAL}.i ({value})"),
            SourceEncoder::Bool => format!("{SVAL}.b ({value})"),
            SourceEncoder::Float => format!("{SVAL}.f (_root_.Float.toBits ({value}))"),
            SourceEncoder::Str => {
                format!("{SVAL}.s (_root_.AverCert.GrammarBridge.strBytes ({value}))")
            }
            SourceEncoder::Record { tid, fields, .. } => {
                let leaves = fields
                    .iter()
                    .map(|(accessor, enc)| enc.encode(&format!("{accessor} ({value})"), fresh))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{SVAL}.record {tid} [{leaves}]")
            }
            SourceEncoder::Tuple { tid, elems } => {
                let components = tuple_components(value, elems.len());
                let leaves = elems
                    .iter()
                    .zip(components)
                    .map(|(enc, component)| enc.encode(&component, fresh))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{SVAL}.record {tid} [{leaves}]")
            }
            SourceEncoder::Sum { tid, ctors, .. } => {
                let mut arms = String::new();
                for (index, (ctor, fields)) in ctors.iter().enumerate() {
                    let names: Vec<String> = fields.iter().map(|_| bind(fresh)).collect();
                    let leaves = fields
                        .iter()
                        .zip(&names)
                        .map(|(enc, name)| enc.encode(name, fresh))
                        .collect::<Vec<_>>()
                        .join(", ");
                    arms.push_str(" | ");
                    arms.push_str(ctor);
                    for name in &names {
                        arms.push(' ');
                        arms.push_str(name);
                    }
                    arms.push_str(&format!(" => {SVAL}.variant {tid} {index} [{leaves}]"));
                }
                format!("(match ({value}) with{arms})")
            }
            SourceEncoder::Option(elem) => {
                let y = bind(fresh);
                let ty = elem.grammar_ty();
                format!(
                    "(match ({value}) with | _root_.Option.none => {SVAL}.none {ty} \
                     | _root_.Option.some {y} => {SVAL}.some {ty} ({}))",
                    elem.encode(&y, fresh)
                )
            }
            SourceEncoder::Result { ok, err } => {
                let y = bind(fresh);
                let z = bind(fresh);
                let (t, e) = (ok.grammar_ty(), err.grammar_ty());
                format!(
                    "(match ({value}) with | _root_.Except.ok {y} => {SVAL}.ok {t} {e} ({}) \
                     | _root_.Except.error {z} => {SVAL}.err {t} {e} ({}))",
                    ok.encode(&y, fresh),
                    err.encode(&z, fresh)
                )
            }
            SourceEncoder::List(elem) => {
                let y = bind(fresh);
                let acc = bind(fresh);
                let ty = elem.grammar_ty();
                format!(
                    "(_root_.List.foldr (fun {y} {acc} => {SVAL}.cons {ty} ({}) {acc}) \
                     ({SVAL}.nil {ty}) ({value}))",
                    elem.encode(&y, fresh)
                )
            }
            SourceEncoder::Vector(elem) => {
                let y = bind(fresh);
                format!(
                    "{SVAL}.vec {} (_root_.List.map (fun {y} => {}) (_root_.Array.toList ({value})))",
                    elem.grammar_ty(),
                    elem.encode(&y, fresh)
                )
            }
        }
    }

    /// Nesting depth (a scalar is 1).
    pub fn depth(&self) -> usize {
        1 + self.children().map(SourceEncoder::depth).max().unwrap_or(0)
    }

    /// Node count.
    pub fn size(&self) -> usize {
        1 + self.children().map(SourceEncoder::size).sum::<usize>()
    }

    fn children(&self) -> Box<dyn Iterator<Item = &SourceEncoder> + '_> {
        match self {
            SourceEncoder::Int
            | SourceEncoder::Bool
            | SourceEncoder::Float
            | SourceEncoder::Str => Box::new(std::iter::empty()),
            SourceEncoder::Record { fields, .. } => Box::new(fields.iter().map(|(_, e)| e)),
            SourceEncoder::Sum { ctors, .. } => {
                Box::new(ctors.iter().flat_map(|(_, fs)| fs.iter()))
            }
            SourceEncoder::Option(e) | SourceEncoder::List(e) | SourceEncoder::Vector(e) => {
                Box::new(std::iter::once(e.as_ref()))
            }
            SourceEncoder::Result { ok, err } => Box::new([ok.as_ref(), err.as_ref()].into_iter()),
            SourceEncoder::Tuple { elems, .. } => Box::new(elems.iter()),
        }
    }

    /// Whether every name this encoder splices into the rendered statement is
    /// a `_root_.`-qualified plain Lean identifier, every accessor is a field
    /// OF the record it declares and every constructor a constructor OF the
    /// sum it declares, and the tree stays within the depth and size caps. The
    /// renderer copies these names verbatim, so this is the gate that keeps the
    /// rendered text a plain term.
    pub fn is_well_formed(&self) -> bool {
        self.depth() <= MAX_ENCODER_DEPTH && self.size() <= MAX_ENCODER_NODES && self.names_ok()
    }

    fn names_ok(&self) -> bool {
        let member_of = |owner: &str, name: &str| {
            is_root_qualified_name(name)
                && name
                    .strip_prefix(owner)
                    .and_then(|rest| rest.strip_prefix('.'))
                    .is_some_and(|member| !member.is_empty() && !member.contains('.'))
        };
        match self {
            SourceEncoder::Int
            | SourceEncoder::Bool
            | SourceEncoder::Float
            | SourceEncoder::Str => true,
            SourceEncoder::Record {
                lean_type, fields, ..
            } => {
                is_root_qualified_name(lean_type)
                    && !fields.is_empty()
                    && fields
                        .iter()
                        .all(|(accessor, enc)| member_of(lean_type, accessor) && enc.names_ok())
            }
            SourceEncoder::Sum {
                lean_type, ctors, ..
            } => {
                is_root_qualified_name(lean_type)
                    && !ctors.is_empty()
                    && ctors.iter().all(|(ctor, fields)| {
                        member_of(lean_type, ctor) && fields.iter().all(SourceEncoder::names_ok)
                    })
            }
            SourceEncoder::Tuple { elems, .. } => {
                elems.len() >= 2 && elems.iter().all(SourceEncoder::names_ok)
            }
            SourceEncoder::Option(e) | SourceEncoder::List(e) | SourceEncoder::Vector(e) => {
                e.names_ok()
            }
            SourceEncoder::Result { ok, err } => ok.names_ok() && err.names_ok(),
        }
    }

    /// The manifest JSON of this encoder (the verifier reads it back with the
    /// same closed key set).
    pub fn to_json(&self) -> String {
        let q = json_quote;
        match self {
            SourceEncoder::Int
            | SourceEncoder::Bool
            | SourceEncoder::Float
            | SourceEncoder::Str => format!("{{\"kind\": {}}}", q(self.kind())),
            SourceEncoder::Record {
                tid,
                lean_type,
                fields,
            } => format!(
                "{{\"kind\": \"record\", \"tid\": {tid}, \"type\": {}, \"fields\": [{}]}}",
                q(lean_type),
                fields
                    .iter()
                    .map(|(accessor, enc)| format!(
                        "{{\"accessor\": {}, \"encoder\": {}}}",
                        q(accessor),
                        enc.to_json()
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            SourceEncoder::Sum {
                tid,
                lean_type,
                ctors,
            } => format!(
                "{{\"kind\": \"sum\", \"tid\": {tid}, \"type\": {}, \"ctors\": [{}]}}",
                q(lean_type),
                ctors
                    .iter()
                    .map(|(ctor, fields)| format!(
                        "{{\"ctor\": {}, \"fields\": [{}]}}",
                        q(ctor),
                        fields
                            .iter()
                            .map(SourceEncoder::to_json)
                            .collect::<Vec<_>>()
                            .join(", ")
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            SourceEncoder::Option(e) => {
                format!("{{\"kind\": \"option\", \"elem\": {}}}", e.to_json())
            }
            SourceEncoder::Result { ok, err } => format!(
                "{{\"kind\": \"result\", \"ok\": {}, \"err\": {}}}",
                ok.to_json(),
                err.to_json()
            ),
            SourceEncoder::Tuple { tid, elems } => format!(
                "{{\"kind\": \"tuple\", \"tid\": {tid}, \"elems\": [{}]}}",
                elems
                    .iter()
                    .map(SourceEncoder::to_json)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            SourceEncoder::List(e) => format!("{{\"kind\": \"list\", \"elem\": {}}}", e.to_json()),
            SourceEncoder::Vector(e) => {
                format!("{{\"kind\": \"vector\", \"elem\": {}}}", e.to_json())
            }
        }
    }
}

/// The components of an `n`-tuple term, in order (Lean's `×` nests to the
/// right: `(a, b, c)` is `(a, (b, c))`).
pub fn tuple_components(value: &str, n: usize) -> Vec<String> {
    let mut out = Vec::with_capacity(n);
    let mut rest = format!("({value})");
    for index in 0..n {
        if index + 1 == n {
            out.push(rest.clone());
        } else {
            out.push(format!("(_root_.Prod.fst {rest})"));
            rest = format!("(_root_.Prod.snd {rest})");
        }
    }
    out
}

fn json_quote(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// Whether `value` is a `_root_.`-qualified plain dotted Lean identifier.
pub fn is_root_qualified_name(value: &str) -> bool {
    value.len() <= MAX_BRIDGE_NAME_LEN
        && value.starts_with(ROOT_PREFIX)
        && is_plain_dotted_name(value)
}

/// Whether `value` is a plain dotted Lean identifier: every `.`-separated
/// segment nonempty, starting with an ASCII letter or `_`, and continuing with
/// ASCII alphanumerics, `_` or a trailing-prime `'` (the transpiler's
/// reserved-word escape).
pub fn is_plain_dotted_name(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= MAX_BRIDGE_NAME_LEN
        && value.split('.').all(|segment| {
            let mut chars = segment.chars();
            let head_ok =
                matches!(chars.next(), Some(first) if first.is_ascii_alphabetic() || first == '_');
            let body: String = chars.collect();
            let body = body.trim_end_matches('\'');
            head_ok && body.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
        })
}

/// Whether `value` is a certified export name as a bridge entry may carry it:
/// one plain identifier segment, no dot and no prime (export names are the
/// Aver path flattened with `_`).
pub fn is_plain_export_name(value: &str) -> bool {
    is_plain_dotted_name(value) && !value.contains('.') && !value.contains('\'')
}

/// The binder names a bridge of this arity quantifies over.
pub fn binder_names(arity: usize) -> Vec<String> {
    (0..arity).map(|index| format!("x{index}")).collect()
}

/// `[enc x0, enc x1, …]` — the encoded argument list. `fresh` continues the
/// numbering of pattern binders across the whole statement.
pub fn encoded_args(params: &[SourceEncoder], fresh: &mut usize) -> String {
    let encoded = params
        .iter()
        .enumerate()
        .map(|(index, encoder)| encoder.encode(&format!("x{index}"), fresh))
        .collect::<Vec<_>>()
        .join(", ");
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

/// `(x0 : T0) (x1 : T1)` — the parameter binders.
pub fn param_binders(params: &[SourceEncoder]) -> String {
    params
        .iter()
        .enumerate()
        .map(|(index, encoder)| format!("(x{index} : {})", encoder.binder_type()))
        .collect::<Vec<_>>()
        .join(" ")
}

/// The obligation of `export`, as the statement names it.
pub fn export_obligation(export: &str) -> String {
    format!("_root_.AverCert.GrammarBridge.exportObligation _root_.AverCert.manifest \"{export}\"")
}

/// The bridge statement for one export.
///
/// This is the single definition of what a bridge SAYS. Both the producer's
/// `Bridge.lean` and the checker's `bridge_pin_<i>` are rendered from it, so the
/// two agree by construction rather than by comparison.
pub fn render_bridge_statement(
    export: &str,
    model: &str,
    kind: BridgeKind,
    params: &[SourceEncoder],
    result: &SourceEncoder,
) -> String {
    let binders = param_binders(params);
    let forall = |body: String| {
        if binders.is_empty() {
            body
        } else {
            format!("∀ {binders}, {body}")
        }
    };
    let mut fresh = 0;
    let typed_args = encoded_args(params, &mut fresh);
    let typing = forall(format!(
        "_root_.AverCert.GrammarBridge.ArgsTyped o {typed_args}"
    ));
    let args = encoded_args(params, &mut fresh);
    let image = result.encode(&source_call(model, params.len()), &mut fresh);
    let model_at = format!("_root_.AverCert.Schema.Obligation.model o fuel {args}");
    let tail = match kind {
        BridgeKind::Adequate => {
            let quantified = if binders.is_empty() {
                format!("(fuel : _root_.Nat) (v : {SVAL})")
            } else {
                format!("(fuel : _root_.Nat) {binders} (v : {SVAL})")
            };
            format!("∀ {quantified}, {model_at} = _root_.Option.some v → v = {image}")
        }
        BridgeKind::Exact => format!(
            "∃ (k : _root_.Nat), ∀ (fuel : _root_.Nat), k ≤ fuel → {}",
            forall(format!("{model_at} = _root_.Option.some ({image})"))
        ),
    };
    format!(
        "∃ o, {} = _root_.Option.some o ∧ ({typing}) ∧ ({tail})",
        export_obligation(export)
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
            tid: 0,
            lean_type: "_root_.Domain.Rational.Fraction".to_string(),
            fields: vec![
                (
                    "_root_.Domain.Rational.Fraction.top".to_string(),
                    SourceEncoder::Int,
                ),
                (
                    "_root_.Domain.Rational.Fraction.bottom".to_string(),
                    SourceEncoder::Int,
                ),
            ],
        }
    }

    fn op() -> SourceEncoder {
        SourceEncoder::Sum {
            tid: 1,
            lean_type: "_root_.CertGoals.Op".to_string(),
            ctors: vec![
                (
                    "_root_.CertGoals.Op.add".to_string(),
                    vec![SourceEncoder::Int],
                ),
                ("_root_.CertGoals.Op.zero".to_string(), vec![]),
            ],
        }
    }

    /// The rendered text is the claim. This pins it verbatim for both kinds,
    /// so a change to the renderer has to be a deliberate edit of an expected
    /// string rather than a silent reshaping of what every certificate says.
    #[test]
    fn both_kinds_render_their_exact_statement() {
        assert_eq!(
            render_bridge_statement(
                "addOne",
                "CertificateHello.addOne",
                BridgeKind::Exact,
                &[SourceEncoder::Int],
                &SourceEncoder::Int
            ),
            "∃ o, _root_.AverCert.GrammarBridge.exportObligation _root_.AverCert.manifest \
             \"addOne\" = _root_.Option.some o ∧ (∀ (x0 : _root_.Int), \
             _root_.AverCert.GrammarBridge.ArgsTyped o [_root_.AverCert.Grammar.SVal.i (x0)]) ∧ \
             (∃ (k : _root_.Nat), ∀ (fuel : _root_.Nat), k ≤ fuel → ∀ (x0 : _root_.Int), \
             _root_.AverCert.Schema.Obligation.model o fuel [_root_.AverCert.Grammar.SVal.i (x0)] \
             = _root_.Option.some (_root_.AverCert.Grammar.SVal.i \
             (_root_.CertificateHello.addOne x0)))"
        );
        assert_eq!(
            render_bridge_statement(
                "sumFrom",
                "RecGen.sumFrom",
                BridgeKind::Adequate,
                &[SourceEncoder::Int],
                &SourceEncoder::Int
            ),
            "∃ o, _root_.AverCert.GrammarBridge.exportObligation _root_.AverCert.manifest \
             \"sumFrom\" = _root_.Option.some o ∧ (∀ (x0 : _root_.Int), \
             _root_.AverCert.GrammarBridge.ArgsTyped o [_root_.AverCert.Grammar.SVal.i (x0)]) ∧ \
             (∀ (fuel : _root_.Nat) (x0 : _root_.Int) (v : _root_.AverCert.Grammar.SVal), \
             _root_.AverCert.Schema.Obligation.model o fuel [_root_.AverCert.Grammar.SVal.i (x0)] \
             = _root_.Option.some v → v = _root_.AverCert.Grammar.SVal.i (_root_.RecGen.sumFrom x0))"
        );
        // Nullary: no parameter binders, and the source call is the bare name.
        assert_eq!(
            render_bridge_statement(
                "Domain_Rational_zeroFraction",
                "Domain.Rational.zeroFraction",
                BridgeKind::Exact,
                &[],
                &fraction(),
            ),
            "∃ o, _root_.AverCert.GrammarBridge.exportObligation _root_.AverCert.manifest \
             \"Domain_Rational_zeroFraction\" = _root_.Option.some o ∧ \
             (_root_.AverCert.GrammarBridge.ArgsTyped o []) ∧ (∃ (k : _root_.Nat), \
             ∀ (fuel : _root_.Nat), k ≤ fuel → _root_.AverCert.Schema.Obligation.model o fuel [] \
             = _root_.Option.some (_root_.AverCert.Grammar.SVal.record 0 \
             [_root_.AverCert.Grammar.SVal.i (_root_.Domain.Rational.Fraction.top \
             (_root_.Domain.Rational.zeroFraction)), _root_.AverCert.Grammar.SVal.i \
             (_root_.Domain.Rational.Fraction.bottom (_root_.Domain.Rational.zeroFraction))]))"
        );
    }

    #[test]
    fn every_encoder_kind_renders_its_value_form() {
        let mut fresh = 0;
        assert_eq!(
            op().encode("x0", &mut fresh),
            "(match (x0) with | _root_.CertGoals.Op.add y0 => \
             _root_.AverCert.Grammar.SVal.variant 1 0 [_root_.AverCert.Grammar.SVal.i (y0)] \
             | _root_.CertGoals.Op.zero => _root_.AverCert.Grammar.SVal.variant 1 1 [])"
        );
        let mut fresh = 0;
        assert_eq!(
            SourceEncoder::Option(Box::new(SourceEncoder::Bool)).encode("x0", &mut fresh),
            "(match (x0) with | _root_.Option.none => _root_.AverCert.Grammar.SVal.none \
             _root_.AverCert.Grammar.Ty.bool | _root_.Option.some y0 => \
             _root_.AverCert.Grammar.SVal.some _root_.AverCert.Grammar.Ty.bool \
             (_root_.AverCert.Grammar.SVal.b (y0)))"
        );
        let mut fresh = 0;
        assert_eq!(
            SourceEncoder::Result {
                ok: Box::new(SourceEncoder::Int),
                err: Box::new(SourceEncoder::Str)
            }
            .encode("x0", &mut fresh),
            "(match (x0) with | _root_.Except.ok y0 => _root_.AverCert.Grammar.SVal.ok \
             _root_.AverCert.Grammar.Ty.int _root_.AverCert.Grammar.Ty.string \
             (_root_.AverCert.Grammar.SVal.i (y0)) | _root_.Except.error y1 => \
             _root_.AverCert.Grammar.SVal.err _root_.AverCert.Grammar.Ty.int \
             _root_.AverCert.Grammar.Ty.string (_root_.AverCert.Grammar.SVal.s \
             (_root_.AverCert.GrammarBridge.strBytes (y1))))"
        );
        let mut fresh = 0;
        assert_eq!(
            SourceEncoder::Tuple {
                tid: 4,
                elems: vec![
                    SourceEncoder::Int,
                    SourceEncoder::Bool,
                    SourceEncoder::Float
                ]
            }
            .encode("x0", &mut fresh),
            "_root_.AverCert.Grammar.SVal.record 4 [_root_.AverCert.Grammar.SVal.i \
             ((_root_.Prod.fst (x0))), _root_.AverCert.Grammar.SVal.b ((_root_.Prod.fst \
             (_root_.Prod.snd (x0)))), _root_.AverCert.Grammar.SVal.f (_root_.Float.toBits \
             ((_root_.Prod.snd (_root_.Prod.snd (x0)))))]"
        );
        let mut fresh = 0;
        assert_eq!(
            SourceEncoder::List(Box::new(SourceEncoder::Int)).encode("x0", &mut fresh),
            "(_root_.List.foldr (fun y0 y1 => _root_.AverCert.Grammar.SVal.cons \
             _root_.AverCert.Grammar.Ty.int (_root_.AverCert.Grammar.SVal.i (y0)) y1) \
             (_root_.AverCert.Grammar.SVal.nil _root_.AverCert.Grammar.Ty.int) (x0))"
        );
        let mut fresh = 0;
        assert_eq!(
            SourceEncoder::Vector(Box::new(SourceEncoder::Int)).encode("x0", &mut fresh),
            "_root_.AverCert.Grammar.SVal.vec _root_.AverCert.Grammar.Ty.int \
             (_root_.List.map (fun y0 => _root_.AverCert.Grammar.SVal.i (y0)) \
             (_root_.Array.toList (x0)))"
        );
    }

    /// The ways a hostile manifest could try to make the renderer say something
    /// else, and the fact that it cannot: the statement changes, so the pin no
    /// longer has the package corollary's type.
    #[test]
    fn structure_edits_change_the_rendered_statement() {
        let render = |export: &str, model: &str, kind, params: &[SourceEncoder]| {
            render_bridge_statement(export, model, kind, params, &fraction())
        };
        let honest = render(
            "Domain_Rational_plus",
            "Domain.Rational.plus",
            BridgeKind::Exact,
            &[fraction(), fraction()],
        );
        // A different export (another obligation).
        assert_ne!(
            honest,
            render(
                "Domain_Rational_minus",
                "Domain.Rational.plus",
                BridgeKind::Exact,
                &[fraction(), fraction()]
            )
        );
        // A different source function.
        assert_ne!(
            honest,
            render(
                "Domain_Rational_plus",
                "Domain.Rational.minus",
                BridgeKind::Exact,
                &[fraction(), fraction()]
            )
        );
        // The weaker kind.
        assert_ne!(
            honest,
            render(
                "Domain_Rational_plus",
                "Domain.Rational.plus",
                BridgeKind::Adequate,
                &[fraction(), fraction()]
            )
        );
        // Permuted record accessors.
        let permuted = SourceEncoder::Record {
            tid: 0,
            lean_type: "_root_.Domain.Rational.Fraction".to_string(),
            fields: vec![
                (
                    "_root_.Domain.Rational.Fraction.bottom".to_string(),
                    SourceEncoder::Int,
                ),
                (
                    "_root_.Domain.Rational.Fraction.top".to_string(),
                    SourceEncoder::Int,
                ),
            ],
        };
        assert_ne!(
            honest,
            render(
                "Domain_Rational_plus",
                "Domain.Rational.plus",
                BridgeKind::Exact,
                &[permuted, fraction()]
            )
        );
        // A tautology is unrepresentable: the left-hand side is always the
        // named obligation's model, never the source call.
        assert!(honest.contains("_root_.AverCert.Schema.Obligation.model o fuel"));
    }

    #[test]
    fn encoders_must_name_their_own_members() {
        assert!(fraction().is_well_formed());
        assert!(op().is_well_formed());
        assert!(SourceEncoder::Int.is_well_formed());
        // An accessor of an unrelated type.
        assert!(
            !SourceEncoder::Record {
                tid: 0,
                lean_type: "_root_.Domain.Rational.Fraction".to_string(),
                fields: vec![("_root_.Other.Type.top".to_string(), SourceEncoder::Int)],
            }
            .is_well_formed()
        );
        // A constructor of an unrelated sum.
        assert!(
            !SourceEncoder::Sum {
                tid: 1,
                lean_type: "_root_.CertGoals.Op".to_string(),
                ctors: vec![("_root_.CertGoals.Tag.a".to_string(), vec![])],
            }
            .is_well_formed()
        );
        // Unqualified names would mean whatever the package's namespaces say.
        assert!(
            !SourceEncoder::Record {
                tid: 0,
                lean_type: "Domain.Rational.Fraction".to_string(),
                fields: vec![(
                    "Domain.Rational.Fraction.top".to_string(),
                    SourceEncoder::Int
                )],
            }
            .is_well_formed()
        );
        // A record with no fields and a one-component tuple have no image.
        assert!(
            !SourceEncoder::Record {
                tid: 0,
                lean_type: "_root_.Domain.Rational.Fraction".to_string(),
                fields: Vec::new(),
            }
            .is_well_formed()
        );
        assert!(
            !SourceEncoder::Tuple {
                tid: 0,
                elems: vec![SourceEncoder::Int]
            }
            .is_well_formed()
        );
        // Depth cap.
        let mut deep = SourceEncoder::Int;
        for _ in 0..MAX_ENCODER_DEPTH {
            deep = SourceEncoder::Option(Box::new(deep));
        }
        assert!(!deep.is_well_formed());
    }

    #[test]
    fn rendered_statements_pass_the_gates_they_are_pinned_under() {
        for kind in [BridgeKind::Exact, BridgeKind::Adequate] {
            let statement = render_bridge_statement(
                "Domain_Rational_plus",
                "Domain.Rational.plus",
                kind,
                &[
                    fraction(),
                    op(),
                    SourceEncoder::Option(Box::new(fraction())),
                ],
                &SourceEncoder::List(Box::new(op())),
            );
            assert!(statement_is_single_plain_line(
                &statement,
                MAX_BRIDGE_STATEMENT_LEN
            ));
            assert!(statement_is_root_qualified(&statement), "{statement}");
        }
    }

    #[test]
    fn reserved_word_primes_are_plain_names() {
        assert!(is_plain_dotted_name("_root_.Models.Type'.field"));
        assert!(!is_plain_dotted_name("_root_.Models.'x"));
        assert!(!is_plain_dotted_name("_root_.Models.x'y"));
    }
}
