// ---- model evaluation (anti-vacuity guard values) ------------------------
// The descent (`n-1`) is the pinned shape; the base is data and the combinator
// (`+` or `*`) is read from the model — so these compute the model value for any
// admitted base and operator without a per-function evaluator.

/// `f n = if n≤0 then base else other <op> f (n-1)` (body-consumed self-recursion),
/// where `<op>` is `+` or `*`. Both combinators commute, so operand order does not
/// affect the value; only the operator and the non-recursive operand do. Computed
/// with checked `i128`: a large multiplier or base can exceed the range, and the
/// caller declines fail-closed rather than emit a wrong (or overflowing) guard.
fn eval_body_recursion(
    n: i64,
    base: i64,
    other: BodyOperand,
    combinator: Combinator,
) -> Option<i128> {
    if n <= 0 {
        Some(base as i128)
    } else {
        let o = match other {
            BodyOperand::Input => n as i128,
            BodyOperand::Const(k) => k as i128,
        };
        let rec = eval_body_recursion(n - 1, base, other, combinator)?;
        match combinator {
            Combinator::Add => o.checked_add(rec),
            Combinator::Mul => o.checked_mul(rec),
        }
    }
}

#[derive(Default)]
struct ModelInfo {
    /// Keyed by the FLAT (wasm-export-space) name: for a def inside
    /// `namespace P` the key is `P` with dots replaced by underscores,
    /// joined to the bare name with `_` — exactly the compiler's dependency
    /// flattening. Definitions in the entry module's namespace are keyed by
    /// their bare export name because the compiler does not flatten that prefix.
    fns: std::collections::HashMap<String, FnSig>,
    /// Keyed by the qualified Lean name (`P.Ty` inside `namespace P`).
    inductives: std::collections::HashMap<String, InductiveInfo>,
    /// Record (`structure`) declarations, keyed by the qualified Lean name.
    /// Read by the plan-equals-source bridge renderer, which needs the source
    /// field ACCESSORS in declaration order to encode a record argument.
    structures: std::collections::HashMap<String, StructureInfo>,
    /// Signatures of PARAMETERLESS model defs (`def zeroFraction : Fraction :=`),
    /// keyed like `fns`. They are deliberately kept out of `fns`: that map
    /// backs `model_lean_name`, whose resolution decides which exports survive
    /// the model-citation gate, so admitting a new shape there would widen the
    /// certified set of every existing package. The plan-equals-source bridge
    /// needs only the signature, so it reads both maps and nothing else moves.
    nullary_fns: std::collections::HashMap<String, FnSig>,
    /// Flat forms (dots replaced by underscores) of every dependency namespace.
    /// An export name shaped like `<prefix>_<rest>` for one of these prefixes
    /// may be a dependency function, so its Lean identifier cannot be assumed
    /// to be the export name itself.
    module_prefix_flats: Vec<String>,
    /// Flat keys claimed by two DIFFERENT qualified names. A lookup on such a
    /// key must fail (fail-closed): citing either candidate would be a guess.
    ambiguous: std::collections::HashSet<String>,
}

struct FnSig {
    /// Fully qualified Lean identifier of this def (`P.name`).
    lean_name: String,
    /// Dotted namespace prefix the def was parsed under.
    prefix: String,
    params: Vec<String>,
    ret: String,
}

struct InductiveInfo {
    ctors: Vec<CtorInfo>,
}

/// One transpiled `structure` declaration: its fields in DECLARATION order,
/// each as `(accessor name, type as written)`. Declaration order is the order
/// the emitter packs the wasm struct in, so it is the order a bridge encoder
/// must list the record's Int leaves in.
struct StructureInfo {
    fields: Vec<(String, String)>,
}

struct CtorInfo {
    name: String,
    fields: Vec<String>,
}

/// The compiler prelude, emitted into every model tree alongside the user's
/// own modules (`AverCommon.lean` plus the build files). It carries its own
/// namespaces (`AverString`, `AverMap`, `AverFloat`, …) and dotted top-level
/// defs (`String.charAtAv`, `Float.fromInt`, …), none of which is ever a user
/// export's model.
///
/// Reading it into the name space would be actively wrong, not just noisy:
/// its namespaces would populate `module_prefix_flats` even for a program with
/// no dependency modules at all, and a user export whose name happens to
/// collide with a prelude def's flattened key (`AverList_get`) would be marked
/// ambiguous and DECLINE. The certificate cites only user models, so only user
/// model files may define the name space.
fn is_user_model_file(path: &str) -> bool {
    path.ends_with(".lean") && path != "AverCommon.lean" && path != "lakefile.lean"
}

fn entry_model_root(model_files: &[(String, String)]) -> Option<String> {
    let lakefile = model_files
        .iter()
        .find_map(|(path, contents)| (path == "lakefile.lean").then_some(contents))?;
    lakefile.lines().find_map(|line| {
        let rest = line.trim().strip_prefix("roots := #[`")?;
        let root = rest.split([',', ']']).next()?.trim();
        (!root.is_empty()).then(|| root.to_string())
    })
}

fn model_file_root(path: &str) -> Option<String> {
    path.strip_suffix(".lean").map(|stem| stem.replace('/', "."))
}

impl ModelInfo {
    /// Parse the USER model modules only — see `is_user_model_file`.
    fn from_files(model_files: &[(String, String)]) -> Self {
        let mut info = Self::default();
        let entry_root = entry_model_root(model_files);
        for (path, content) in model_files {
            if !is_user_model_file(path) {
                continue;
            }
            let entry_namespace = if model_file_root(path).as_ref() == entry_root.as_ref() {
                entry_root.as_deref()
            } else {
                None
            };
            info.parse_lean(content, entry_namespace);
        }
        info
    }

    /// The Lean identifier the generated certificate must cite for the model
    /// of export `name`, or `None` when it cannot be derived (then the export
    /// must decline rather than cite a guess).
    ///
    /// - A parsed model def with this flat key resolves to its qualified name.
    /// - Two distinct defs flattening to the same key are ambiguous: `None`.
    /// - With no parsed def, the export name itself is citable only when no
    ///   model namespace flattens to a prefix of it — otherwise the export may
    ///   be a dependency-module function whose bare-underscore name does not
    ///   exist in Lean, so `None`.
    fn model_lean_name(&self, name: &str) -> Option<String> {
        if self.ambiguous.contains(name) {
            return None;
        }
        if let Some(sig) = self.fns.get(name) {
            return Some(sig.lean_name.clone());
        }
        let may_be_dep_fn = self.module_prefix_flats.iter().any(|prefix| {
            name.len() > prefix.len() + 1
                && name.starts_with(prefix.as_str())
                && name.as_bytes()[prefix.len()] == b'_'
        });
        if may_be_dep_fn {
            None
        } else {
            Some(name.to_string())
        }
    }

    /// Resolve a type name AS WRITTEN in a model def signature (relative to
    /// that def's namespace) to the qualified inductive it denotes: first in
    /// the def's own namespace, then as a fully qualified / entry-level name.
    fn resolve_inductive<'a>(
        &'a self,
        prefix: &str,
        written: &str,
    ) -> Option<(String, &'a InductiveInfo)> {
        if !prefix.is_empty() {
            let qualified = format!("{prefix}.{written}");
            if let Some(ind) = self.inductives.get(&qualified) {
                return Some((qualified, ind));
            }
        }
        self.inductives
            .get(written)
            .map(|ind| (written.to_string(), ind))
    }

    /// Resolve a type name AS WRITTEN in a model def signature (relative to
    /// that def's namespace) to the qualified `structure` it denotes. Same
    /// two-step lookup as [`Self::resolve_inductive`].
    fn resolve_structure<'a>(
        &'a self,
        prefix: &str,
        written: &str,
    ) -> Option<(String, &'a StructureInfo)> {
        if !prefix.is_empty()
            && let Some(found) = self.structures.get(&format!("{prefix}.{written}"))
        {
            return Some((format!("{prefix}.{written}"), found));
        }
        self.structures
            .get(written)
            .map(|found| (written.to_string(), found))
    }

    /// Whether `qualified` is the fully qualified name of a def the user model
    /// modules declare. Read by the law/bridge coverage scan.
    fn is_model_fn(&self, qualified: &str) -> bool {
        self.fns
            .values()
            .chain(self.nullary_fns.values())
            .any(|sig| sig.lean_name == qualified)
    }

    /// The parsed signature of the model def a certified export cites, when it
    /// resolves unambiguously. `None` keeps the caller fail-closed.
    fn model_fn_sig(&self, name: &str) -> Option<&FnSig> {
        if self.ambiguous.contains(name) {
            return None;
        }
        self.fns.get(name).or_else(|| self.nullary_fns.get(name))
    }

    fn parse_lean(&mut self, content: &str, entry_namespace: Option<&str>) {
        let lines: Vec<&str> = content.lines().collect();
        // Namespace stack: `namespace X` pushes (X may be dotted), a matching
        // `end X` pops. A bare `end` (a `mutual` block) never pops.
        let mut ns_stack: Vec<String> = Vec::new();
        let mut i = 0usize;
        while i < lines.len() {
            let line = lines[i].trim();
            if let Some(rest) = line.strip_prefix("namespace ")
                && let Some(name) = rest.split_whitespace().next()
                && rest.trim() == name
            {
                ns_stack.push(name.to_string());
                let prefix = ns_stack.join(".");
                if entry_namespace != Some(prefix.as_str()) {
                    let flat = prefix.replace('.', "_");
                    if !self.module_prefix_flats.contains(&flat) {
                        self.module_prefix_flats.push(flat);
                    }
                }
                i += 1;
                continue;
            }
            if let Some(rest) = line.strip_prefix("end ")
                && ns_stack.last().map(String::as_str) == Some(rest.trim())
            {
                ns_stack.pop();
                i += 1;
                continue;
            }
            let prefix = ns_stack.join(".");
            // `structure X where` followed by one indented `field : Type` line
            // per field, in declaration order. Anything else about the block
            // (a different header shape, a field line that is not exactly
            // `name : Type`) ends the field walk, so a shape this parser does
            // not understand yields a partial or absent record rather than a
            // wrong one — and the bridge renderer that reads it cross-checks
            // the field count against the byte-derived record declaration.
            if let Some(name) = line
                .strip_prefix("structure ")
                .and_then(|rest| rest.strip_suffix(" where"))
                .map(str::trim)
                .filter(|name| !name.is_empty() && !name.contains(char::is_whitespace))
            {
                let qualified = qualify(&prefix, name);
                i += 1;
                let mut fields = Vec::new();
                while i < lines.len() {
                    let raw = lines[i];
                    if !raw.starts_with(' ') {
                        break;
                    }
                    let Some((field, ty)) = raw.trim().split_once(" : ") else {
                        break;
                    };
                    let field = field.trim();
                    let plain_field = !field.is_empty()
                        && field
                            .chars()
                            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '\'');
                    if !plain_field {
                        break;
                    }
                    fields.push((field.to_string(), ty.trim().to_string()));
                    i += 1;
                }
                self.structures.insert(qualified, StructureInfo { fields });
                continue;
            }
            if let Some(name) = line
                .strip_prefix("inductive ")
                .and_then(|s| s.split_whitespace().next())
            {
                let qualified = qualify(&prefix, name);
                i += 1;
                let mut ctors = Vec::new();
                while i < lines.len() {
                    let l = lines[i].trim();
                    if !l.starts_with('|') {
                        break;
                    }
                    let rest = l.trim_start_matches('|').trim();
                    let ctor_name = rest.split_whitespace().next().unwrap_or("").to_string();
                    let mut fields = Vec::new();
                    let mut tail = rest[ctor_name.len()..].trim();
                    while let Some(start) = tail.find("(_ : ") {
                        let after = &tail[start + 5..];
                        if let Some(end) = after.find(')') {
                            fields.push(after[..end].trim().to_string());
                            tail = &after[end + 1..];
                        } else {
                            break;
                        }
                    }
                    ctors.push(CtorInfo {
                        name: ctor_name,
                        fields,
                    });
                    i += 1;
                }
                self.inductives.insert(qualified, InductiveInfo { ctors });
                continue;
            }
            if line.starts_with("def ")
                && line.contains(" : ")
                && line.ends_with(":=")
                && let Some((name, mut sig, nullary)) = parse_def_sig(line)
            {
                let qualified = qualify(&prefix, &name);
                let flat = if entry_namespace == Some(prefix.as_str()) {
                    name.clone()
                } else {
                    qualified.replace('.', "_")
                };
                sig.lean_name = qualified;
                sig.prefix = prefix.clone();
                if self.ambiguous.contains(&flat) {
                    i += 1;
                    continue;
                }
                let table = if nullary {
                    &mut self.nullary_fns
                } else {
                    &mut self.fns
                };
                match table.get(&flat) {
                    Some(existing) if existing.lean_name != sig.lean_name => {
                        // Two distinct qualified names collide on one flat
                        // key: neither may ever resolve (fail-closed).
                        table.remove(&flat);
                        self.ambiguous.insert(flat);
                    }
                    _ => {
                        table.insert(flat, sig);
                    }
                }
            }
            i += 1;
        }
    }
}

fn qualify(prefix: &str, name: &str) -> String {
    if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{prefix}.{name}")
    }
}

/// Parse one single-line def signature. `lean_name`/`prefix` are filled by the
/// caller, which knows the namespace the line was parsed under. The third
/// component says whether the def takes NO parameters, which the caller keys a
/// separate table on — see `ModelInfo::nullary_fns`.
fn parse_def_sig(line: &str) -> Option<(String, FnSig, bool)> {
    let rest = line.strip_prefix("def ")?;
    let name = rest.split_whitespace().next()?.to_string();
    let after_name = rest[name.len()..].trim();
    let before_assign = after_name.strip_suffix(":=")?.trim();
    // `def f (a : Int) : Int :=` splits at the LAST ` : `; a parameterless
    // `def zero : Fraction :=` has no such separator and starts at the colon.
    let (params_part, ret, nullary) = match before_assign.rfind(" : ") {
        Some(at) => (
            before_assign[..at].trim(),
            before_assign[at + 3..].trim().to_string(),
            false,
        ),
        None => (
            "",
            before_assign.strip_prefix(':')?.trim().to_string(),
            true,
        ),
    };
    if ret.is_empty() {
        return None;
    }
    let mut params = Vec::new();
    let mut tail = params_part;
    while let Some(start) = tail.find('(') {
        let after = &tail[start + 1..];
        let mut depth = 1usize;
        let mut end = None;
        for (at, ch) in after.char_indices() {
            match ch {
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth == 0 {
                        end = Some(at);
                        break;
                    }
                }
                _ => {}
            }
        }
        let end = end?;
        let param = &after[..end];
        if let Some((_, ty)) = param.split_once(" : ") {
            params.push(ty.trim().to_string());
        }
        tail = &after[end + 1..];
    }
    let nullary = nullary && params.is_empty();
    Some((
        name.clone(),
        FnSig {
            lean_name: name,
            prefix: String::new(),
            params,
            ret,
        },
        nullary,
    ))
}

#[cfg(test)]
mod model_name_space_tests {
    /// A single-file (no dependency module) program's model tree still ships
    /// the compiler prelude, which carries its own namespaces and dotted
    /// top-level defs. None of it may enter the dependency name space: a flat
    /// program has no dependency prefixes, and its bare Wasm exports resolve to
    /// definitions in the entry module namespace.
    #[test]
    fn prelude_namespaces_stay_out_of_the_flat_key_space() {
        // Abridged from a real `AverCommon.lean`: a namespace block and the
        // dotted top-level defs the prelude actually emits.
        let prelude = "\
def String.charAtAv (s : String) (i : Int) : Option String :=\n\
  none\n\
namespace AverList\n\
def get (xs : List Int) (i : Int) : Int :=\n\
  0\n\
end AverList\n\
namespace AverMap\n\
def get (m : Int) (k : Int) : Int :=\n\
  0\n\
end AverMap\n";
        let entry = "\
namespace Program
\
def addTwo (n : Int) : Int :=\n\
  n + 2\n\
end Program\n";
        let info = super::ModelInfo::from_files(&[
            ("AverCommon.lean".to_string(), prelude.to_string()),
            (
                "lakefile.lean".to_string(),
                "roots := #[`Program, `AverCommon]\n".to_string(),
            ),
            ("Program.lean".to_string(), entry.to_string()),
        ]);

        assert!(
            info.module_prefix_flats.is_empty(),
            "a program with no dependency modules must have no module prefixes, got {:?}",
            info.module_prefix_flats
        );
        assert!(
            info.ambiguous.is_empty(),
            "the prelude must not make any key ambiguous, got {:?}",
            info.ambiguous
        );
        for prelude_key in ["AverList_get", "AverMap_get", "String_charAtAv"] {
            assert!(
                !info.fns.contains_key(prelude_key),
                "prelude def leaked into the flat key space as `{prelude_key}`"
            );
        }
        assert_eq!(
            info.model_lean_name("addTwo").as_deref(),
            Some("Program.addTwo"),
            "an entry export resolves to its module-qualified Lean name"
        );
        // The regression this guards: an export colliding with a prelude def's
        // flattened key must still resolve, not decline as ambiguous.
        assert_eq!(
            info.model_lean_name("AverList_get").as_deref(),
            Some("AverList_get"),
            "a flat export must not be shadowed by a prelude namespace"
        );
    }

    /// A real dependency module DOES define the name space: its namespace is a
    /// module prefix, and its functions resolve to their qualified names.
    #[test]
    fn dependency_module_namespaces_define_the_flat_key_space() {
        let dep = "\
namespace Nested.Deep.Util\n\
def bump (n : Int) : Int :=\n\
  n + 2\n\
end Nested.Deep.Util\n";
        let info = super::ModelInfo::from_files(&[
            ("AverCommon.lean".to_string(), "def unrelated : Int := 0\n".to_string()),
            ("Nested/Deep/Util.lean".to_string(), dep.to_string()),
        ]);

        assert_eq!(info.module_prefix_flats, vec!["Nested_Deep_Util".to_string()]);
        assert_eq!(
            info.model_lean_name("Nested_Deep_Util_bump").as_deref(),
            Some("Nested.Deep.Util.bump"),
            "a dependency-module export resolves to its qualified name"
        );
        // Fail-closed: an export shaped like this module's prefix but with no
        // parsed definition must NOT fall back to citing itself.
        assert_eq!(
            info.model_lean_name("Nested_Deep_Util_missing"),
            None,
            "an unresolvable dependency-shaped export must decline"
        );
    }
}
