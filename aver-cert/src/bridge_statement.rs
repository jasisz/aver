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
        self.grammar_ty_as(false)
    }

    /// [`Self::grammar_ty`], with `nat_lit` type ids when `raw`.
    fn grammar_ty_as(&self, raw: bool) -> String {
        let tid_of = |tid: &u32| {
            if raw {
                format!("(nat_lit {tid})")
            } else {
                tid.to_string()
            }
        };
        match self {
            SourceEncoder::Int => format!("{TY}.int"),
            SourceEncoder::Bool => format!("{TY}.bool"),
            SourceEncoder::Float => format!("{TY}.float"),
            SourceEncoder::Str => format!("{TY}.string"),
            SourceEncoder::Record { tid, .. } | SourceEncoder::Tuple { tid, .. } => {
                format!("({TY}.record {})", tid_of(tid))
            }
            SourceEncoder::Sum { tid, .. } => format!("({TY}.sum {})", tid_of(tid)),
            SourceEncoder::Option(elem) => format!("({TY}.option {})", elem.grammar_ty_as(raw)),
            SourceEncoder::Result { ok, err } => {
                format!(
                    "({TY}.result {} {})",
                    ok.grammar_ty_as(raw),
                    err.grammar_ty_as(raw)
                )
            }
            SourceEncoder::List(elem) => format!("({TY}.list {})", elem.grammar_ty_as(raw)),
            SourceEncoder::Vector(elem) => format!("({TY}.vec {})", elem.grammar_ty_as(raw)),
        }
    }

    /// The `SVal` term for the source value `value` (already a Lean term).
    /// `fresh` numbers the pattern binders a sum, option, result, list or
    /// vector encoder introduces, so nested encoders never shadow each other.
    /// Type ids and constructor positions are ordinary numerals: this is the
    /// form of the producer's own proof targets.
    pub fn encode(&self, value: &str, fresh: &mut usize) -> String {
        self.encode_as(value, fresh, false)
    }

    /// [`Self::encode`] with every numeral a `nat_lit`: the form a PINNED
    /// statement uses, so no `OfNat` instance takes part in what it says.
    pub fn encode_pinned(&self, value: &str, fresh: &mut usize) -> String {
        self.encode_as(value, fresh, true)
    }

    fn encode_as(&self, value: &str, fresh: &mut usize, raw: bool) -> String {
        let num = |n: usize| {
            if raw {
                format!("(nat_lit {n})")
            } else {
                n.to_string()
            }
        };
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
                    .map(|(accessor, enc)| {
                        enc.encode_as(&format!("{accessor} ({value})"), fresh, raw)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{SVAL}.record {} [{leaves}]", num(*tid as usize))
            }
            SourceEncoder::Tuple { tid, elems } => {
                let components = tuple_components(value, elems.len());
                let leaves = elems
                    .iter()
                    .zip(components)
                    .map(|(enc, component)| enc.encode_as(&component, fresh, raw))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{SVAL}.record {} [{leaves}]", num(*tid as usize))
            }
            SourceEncoder::Sum { tid, ctors, .. } => {
                let mut arms = String::new();
                for (index, (ctor, fields)) in ctors.iter().enumerate() {
                    let names: Vec<String> = fields.iter().map(|_| bind(fresh)).collect();
                    let leaves = fields
                        .iter()
                        .zip(&names)
                        .map(|(enc, name)| enc.encode_as(name, fresh, raw))
                        .collect::<Vec<_>>()
                        .join(", ");
                    arms.push_str(" | ");
                    arms.push_str(ctor);
                    for name in &names {
                        arms.push(' ');
                        arms.push_str(name);
                    }
                    arms.push_str(&format!(
                        " => {SVAL}.variant {} {} [{leaves}]",
                        num(*tid as usize),
                        num(index)
                    ));
                }
                format!("(match ({value}) with{arms})")
            }
            SourceEncoder::Option(elem) => {
                let y = bind(fresh);
                let ty = elem.grammar_ty_as(raw);
                format!(
                    "(match ({value}) with | _root_.Option.none => {SVAL}.none {ty} \
                     | _root_.Option.some {y} => {SVAL}.some {ty} ({}))",
                    elem.encode_as(&y, fresh, raw)
                )
            }
            SourceEncoder::Result { ok, err } => {
                let y = bind(fresh);
                let z = bind(fresh);
                let (t, e) = (ok.grammar_ty_as(raw), err.grammar_ty_as(raw));
                format!(
                    "(match ({value}) with | _root_.Except.ok {y} => {SVAL}.ok {t} {e} ({}) \
                     | _root_.Except.error {z} => {SVAL}.err {t} {e} ({}))",
                    ok.encode_as(&y, fresh, raw),
                    err.encode_as(&z, fresh, raw)
                )
            }
            SourceEncoder::List(elem) => {
                let y = bind(fresh);
                let acc = bind(fresh);
                let ty = elem.grammar_ty_as(raw);
                format!(
                    "(_root_.List.foldr (fun {y} {acc} => {SVAL}.cons {ty} ({}) {acc}) \
                     ({SVAL}.nil {ty}) ({value}))",
                    elem.encode_as(&y, fresh, raw)
                )
            }
            SourceEncoder::Vector(elem) => {
                let y = bind(fresh);
                format!(
                    "{SVAL}.vec {} (_root_.List.map (fun {y} => {}) (_root_.Array.toList ({value})))",
                    elem.grammar_ty_as(raw),
                    elem.encode_as(&y, fresh, raw)
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
/// ASCII alphanumerics, `_` or `'`. The prime is the transpiler's escape of a
/// Lean keyword (`none'`), and a name derived from an escaped one carries it
/// mid-segment (the law theorem `at'_law_rulesOnlyTurnOn` of a function
/// `at`). After a letter, Lean lexes `'` as part of the identifier, so a
/// segment of this shape can never open a character literal.
pub fn is_plain_dotted_name(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= MAX_BRIDGE_NAME_LEN
        && value.split('.').all(|segment| {
            let mut chars = segment.chars();
            matches!(chars.next(), Some(first) if first.is_ascii_alphabetic() || first == '_')
                && chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '\'')
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

/// The binder of a pinned statement: one variable `x` of the tuple of the
/// parameter types (`Unit` for a nullary function), and the term of each
/// parameter as a component of `x`.
fn pinned_binder(params: &[SourceEncoder]) -> (String, Vec<String>) {
    match params.len() {
        0 => ("_root_.Unit".to_string(), Vec::new()),
        1 => (params[0].binder_type(), vec!["x".to_string()]),
        n => {
            let mut ty = params[n - 1].binder_type();
            for param in params[..n - 1].iter().rev() {
                ty = format!("(_root_.Prod {} {ty})", param.binder_type());
            }
            (ty, tuple_components("x", n))
        }
    }
}

/// The bridge statement for one export: what the checker pins and what the
/// package's `_certified` corollary must state.
///
/// This is the single definition of what a bridge SAYS. The statement is an
/// application of the wall's own `GrammarBridge.Exact` / `GrammarBridge.Adequate`
/// — whose `≤`, quantifiers and numerals were elaborated inside the wall — to
/// the manifest, the export name, and two functions of one tuple binder: the
/// encoded argument list and the encoded source result. Every numeral in those
/// functions is a `nat_lit`, and every name is `_root_`-qualified, so neither a
/// namespace nor an instance the package declares can change what it says.
pub fn render_bridge_statement(
    export: &str,
    model: &str,
    kind: BridgeKind,
    params: &[SourceEncoder],
    result: &SourceEncoder,
) -> String {
    let (binder, components) = pinned_binder(params);
    let mut fresh = 0;
    let args = params
        .iter()
        .zip(&components)
        .map(|(encoder, component)| encoder.encode_pinned(component, &mut fresh))
        .collect::<Vec<_>>()
        .join(", ");
    let call = if components.is_empty() {
        format!("{ROOT_PREFIX}{model}")
    } else {
        format!("{ROOT_PREFIX}{model} {}", components.join(" "))
    };
    let image = result.encode_pinned(&call, &mut fresh);
    let definition = match kind {
        BridgeKind::Exact => "_root_.AverCert.GrammarBridge.Exact",
        BridgeKind::Adequate => "_root_.AverCert.GrammarBridge.Adequate",
    };
    format!(
        "{definition} _root_.AverCert.manifest \"{export}\" \
         (fun (x : {binder}) => [{args}]) (fun (x : {binder}) => {image})"
    )
}

/// The proof term that turns the producer's expanded bridge theorem
/// (`theorem`, stated by [`render_bridge_statement_expanded`]) into the
/// pinned statement: the same facts, with the parameters read off the tuple
/// binder.
pub fn pinned_from_expanded(theorem: &str, kind: BridgeKind, arity: usize) -> String {
    let components = match arity {
        0 => Vec::new(),
        1 => vec!["x".to_string()],
        n => tuple_components("x", n),
    };
    let applied = |head: &str| {
        if components.is_empty() {
            head.to_string()
        } else {
            format!("{head} {}", components.join(" "))
        }
    };
    let x = if arity == 0 { "_" } else { "x" };
    let (definition, term) = match kind {
        BridgeKind::Exact => (
            "_root_.AverCert.GrammarBridge.Exact",
            format!(
                "match {theorem} with | ⟨o, ho, ht, k, hk⟩ => \
                 ⟨o, ho, fun {x} => {}, k, fun fuel hf {x} => {}⟩",
                applied("ht"),
                applied("hk fuel hf")
            ),
        ),
        BridgeKind::Adequate => (
            "_root_.AverCert.GrammarBridge.Adequate",
            format!(
                "match {theorem} with | ⟨o, ho, ht, hk⟩ => \
                 ⟨o, ho, fun {x} => {}, fun fuel {x} v h => {} v h⟩",
                applied("ht"),
                applied("hk fuel")
            ),
        ),
    };
    format!("(by unfold {definition}; exact ({term}))")
}

/// The producer's own proof target for one bridge: the pinned statement with
/// the wall definition unfolded, one binder per parameter and ordinary
/// numerals — the form its tactic scripts are written against. It is never
/// pinned: the `_certified` corollary restates it as [`render_bridge_statement`]
/// through [`pinned_from_expanded`], so a package instance that changed what
/// this text means makes that restatement fail rather than weakening the pin.
pub fn render_bridge_statement_expanded(
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

/// The statement gate both the producer and the checker apply to a statement
/// before the witness pins it: one plain term-position line, with balanced
/// delimiters so it cannot escape the single `(...)` it is elaborated in.
///
/// Delimiters are counted the way Lean reads them. The contents of a string
/// literal, a character literal and a `«…»` identifier are skipped, so a `"("`
/// in a statement is not an opening parenthesis. What this gate cannot lex
/// exactly, it refuses: an interpolated or raw string (`s!"…"`, `r"…"`), a
/// backtick (name literals and quotations), and any literal left unterminated.
///
/// The witness does not rely on this gate for the shape of a pin: it elaborates
/// each statement as a definition of its own and conjoins the definition, so no
/// text can re-associate the conjunction. The gate keeps the statement one
/// term, so the definition cannot end early and add a command.
pub fn statement_is_single_plain_line(statement: &str, max_len: usize) -> bool {
    if statement.is_empty()
        || statement.len() > max_len
        || statement.chars().any(char::is_control)
        || statement.contains(":=")
        || statement.contains("--")
        || statement.contains("/-")
        || statement.contains('`')
    {
        return false;
    }
    let chars: Vec<char> = statement.chars().collect();
    let identifier_char =
        |c: char| c.is_alphanumeric() || matches!(c, '_' | '\'' | '!' | '?' | '.');
    let mut depth: Vec<char> = Vec::new();
    let mut at = 0;
    while at < chars.len() {
        let character = chars[at];
        let previous = at.checked_sub(1).map(|p| chars[p]);
        match character {
            '"' => {
                // `s!"…"`, `m!"…"` and `r"…"` / `r#"…"#` read their body with
                // rules of their own: refuse rather than approximate them.
                if matches!(previous, Some('!' | '#'))
                    || (previous == Some('r')
                        && at.checked_sub(2).is_none_or(|p| !identifier_char(chars[p])))
                {
                    return false;
                }
                at += 1;
                loop {
                    match chars.get(at) {
                        None => return false,
                        Some('\\') => at += 2,
                        Some('"') => break,
                        Some(_) => at += 1,
                    }
                }
            }
            '\'' if !previous.is_some_and(identifier_char) => {
                // A character literal: one character or one escape, then `'`.
                at += 1;
                match chars.get(at) {
                    None | Some('\'') => return false,
                    Some('\\') => {
                        at += 1;
                        while chars.get(at).is_some_and(|c| *c != '\'') {
                            at += 1;
                        }
                    }
                    Some(_) => at += 1,
                }
                if chars.get(at) != Some(&'\'') {
                    return false;
                }
            }
            '«' => {
                at += 1;
                while chars.get(at).is_some_and(|c| *c != '»') {
                    at += 1;
                }
                if at >= chars.len() {
                    return false;
                }
            }
            '»' => return false,
            '(' | '[' | '{' | '⟨' => depth.push(character),
            ')' | ']' | '}' | '⟩' => {
                let opener = match character {
                    ')' => '(',
                    ']' => '[',
                    '}' => '{',
                    _ => '⟨',
                };
                if depth.pop() != Some(opener) {
                    return false;
                }
            }
            _ => {}
        }
        at += 1;
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

/// The identifier tokens of a law statement, in first-appearance order: a
/// token is a maximal run of Lean identifier characters, stripped of leading
/// and trailing dots.
pub fn statement_tokens(statement: &str) -> Vec<&str> {
    let mut found: Vec<&str> = Vec::new();
    for token in
        statement.split(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '\''))
    {
        let token = token.trim_matches('.');
        if !token.is_empty() && !found.contains(&token) {
            found.push(token);
        }
    }
    found
}

/// The bridges a law statement mentions: the positions in `models` (the
/// declared bridges' source functions) of every model the statement names as
/// `_root_.<model>`, in first-appearance order. This is the ONE rule the
/// producer writes a law's `bridges` list by and the checker holds the list
/// to, so the bridges a `_bridged` corollary conjoins are exactly those of the
/// functions its statement speaks about — never a chosen subset or an
/// unrelated bridge.
///
/// A law statement is elaborated at the root namespace, and only a
/// `_root_.`-spelled name is certain to mean the root constant there: a bare
/// `Tiny.addTwo` would be a field read of a binder named `Tiny`. So only that
/// spelling counts as a mention, and [`law_names_model_unqualified`] refuses
/// any other spelling of a model name in a law that lists bridges.
pub fn law_mentioned_bridges(statement: &str, models: &[&str]) -> Vec<usize> {
    let mut covering = Vec::new();
    for token in statement_tokens(statement) {
        let Some(named) = token.strip_prefix(ROOT_PREFIX) else {
            continue;
        };
        if let Some(index) = models.iter().position(|model| *model == named)
            && !covering.contains(&index)
        {
            covering.push(index);
        }
    }
    covering
}

/// Whether a statement token spells `model` without `_root_.`: the bare
/// name, or a name ending in `.<model>` that does not start with `_root_.`
/// (such as `Evil.Tiny.addTwo`). Read inside a namespace, or through a
/// binder, such a token can mean another constant than `model`. A
/// `_root_.`-spelled token names exactly the root constant it spells.
pub fn token_names_model_unqualified(token: &str, model: &str) -> bool {
    !token.starts_with(ROOT_PREFIX)
        && (token == model
            || token
                .strip_suffix(model)
                .is_some_and(|head| head.ends_with('.')))
}

/// The first model of `models` that a law statement spells without
/// `_root_.` ([`token_names_model_unqualified`]), if any.
pub fn law_names_model_unqualified<'a>(statement: &str, models: &[&'a str]) -> Option<&'a str> {
    let tokens = statement_tokens(statement);
    models
        .iter()
        .find(|model| {
            tokens
                .iter()
                .any(|token| token_names_model_unqualified(token, model))
        })
        .copied()
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
    /// The pinned text: the wall's definitions, one tuple binder, `nat_lit`
    /// numerals, nothing a package instance or namespace can reinterpret.
    #[test]
    fn pinned_statements_go_through_the_wall_definitions() {
        assert_eq!(
            render_bridge_statement(
                "addOne",
                "CertificateHello.addOne",
                BridgeKind::Exact,
                &[SourceEncoder::Int],
                &SourceEncoder::Int
            ),
            "_root_.AverCert.GrammarBridge.Exact _root_.AverCert.manifest \"addOne\" \
             (fun (x : _root_.Int) => [_root_.AverCert.Grammar.SVal.i (x)]) \
             (fun (x : _root_.Int) => _root_.AverCert.Grammar.SVal.i \
             (_root_.CertificateHello.addOne x))"
        );
        let pinned = render_bridge_statement(
            "Domain_Rational_plus",
            "Domain.Rational.plus",
            BridgeKind::Adequate,
            &[fraction(), op()],
            &fraction(),
        );
        assert!(
            pinned.starts_with(
                "_root_.AverCert.GrammarBridge.Adequate _root_.AverCert.manifest \
             \"Domain_Rational_plus\" (fun (x : (_root_.Prod _root_.Domain.Rational.Fraction \
             _root_.CertGoals.Op)) => [_root_.AverCert.Grammar.SVal.record (nat_lit 0) ["
            ),
            "{pinned}"
        );
        assert!(pinned.contains("_root_.AverCert.Grammar.SVal.variant (nat_lit 1) (nat_lit 0)"));
        assert!(
            pinned.contains(
                "(_root_.Domain.Rational.plus (_root_.Prod.fst (x)) (_root_.Prod.snd (x)))"
            )
        );
        // No `≤`, no bare numeral: the only order and numbers in a pinned
        // statement are the wall's.
        assert!(!pinned.contains('≤'));
        assert!(!pinned.contains(" 0 ") && !pinned.contains(" 1 "));
        assert_eq!(
            pinned_from_expanded("_root_.AverCert.Bridge.f", BridgeKind::Exact, 0),
            "(by unfold _root_.AverCert.GrammarBridge.Exact; exact (match \
             _root_.AverCert.Bridge.f with | ⟨o, ho, ht, k, hk⟩ => ⟨o, ho, fun _ => ht, k, \
             fun fuel hf _ => hk fuel hf⟩))"
        );
    }

    #[test]
    fn both_kinds_render_their_exact_statement() {
        assert_eq!(
            render_bridge_statement_expanded(
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
            render_bridge_statement_expanded(
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
            render_bridge_statement_expanded(
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
        // A tautology is unrepresentable: the statement is always the wall's
        // own `Exact`/`Adequate` of the named export's obligation.
        assert!(honest.starts_with("_root_.AverCert.GrammarBridge.Exact _root_.AverCert.manifest"));
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
    fn the_statement_gate_lexes_literals_as_lean_does() {
        let gate = |s: &str| statement_is_single_plain_line(s, MAX_BRIDGE_STATEMENT_LEN);
        // Parentheses inside string literals do not count: this statement
        // closes the wrapping parenthesis early in Lean.
        assert!(!gate("\"(\" = \"(\" ) ∨ ( M.f 0 = M.f 0 ∧ \")\" = \")\""));
        assert!(!gate("'(' = '(' ) ∨ ( True"));
        assert!(!gate("«(» = 0 ) ∨ ( True"));
        // Balanced statements with delimiters inside literals pass.
        assert!(gate("f \"(\" = \")\""));
        assert!(gate("g '(' = ')' ∧ h '\\'' = 0"));
        assert!(gate("∀ (x' : Int), f x' = x'"));
        assert!(gate("M.«weird)name» 0 = 0"));
        assert!(gate("s \"a\\\"b(\" = t"));
        // What the gate cannot lex exactly, it refuses.
        assert!(!gate("s!\"{x}\" = t"));
        assert!(!gate("r\"(\" = t"));
        assert!(!gate("r#\"(\"# = t"));
        assert!(!gate("`(x) = y"));
        assert!(!gate("f \"unterminated"));
        assert!(!gate("f 'x = y"));
        assert!(!gate("f «x = y"));
        // An identifier ending in `r` before a string is not a raw string.
        assert!(gate("ctr \"x\" = y"));
    }

    #[test]
    fn reserved_word_primes_are_plain_names() {
        assert!(is_plain_dotted_name("_root_.Models.Type'.field"));
        assert!(!is_plain_dotted_name("_root_.Models.'x"));
        // A name derived from an escaped one carries the prime mid-segment.
        assert!(is_plain_dotted_name("_root_.Models.at'_law_x"));
        assert!(!is_plain_dotted_name("_root_.Models..x"));
        assert!(!is_plain_dotted_name("_root_.Models.x\"y"));
    }
}
