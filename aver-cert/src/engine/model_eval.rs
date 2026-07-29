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

/// `f n acc = if n≤0 then acc else f (n-1) (acc + n)` (accumulator tail recursion).
fn eval_accumulator(n: i64, acc: i64) -> i64 {
    if n <= 0 {
        acc
    } else {
        eval_accumulator(n - 1, acc + n)
    }
}

#[derive(Default)]
struct ModelInfo {
    /// Keyed by the FLAT (wasm-export-space) name: for a def inside
    /// `namespace P` the key is `P` with dots replaced by underscores,
    /// joined to the bare name with `_` — exactly the compiler's dependency
    /// flattening. Entry-level defs are keyed by their bare name unchanged.
    fns: std::collections::HashMap<String, FnSig>,
    /// Keyed by the QUALIFIED Lean name (`P.Ty` inside `namespace P`, the
    /// bare name at entry level).
    inductives: std::collections::HashMap<String, InductiveInfo>,
    /// Flat forms (dots replaced by underscores) of every namespace the model
    /// files open. An export name shaped like `<prefix>_<rest>` for one of
    /// these prefixes MAY be a dependency-module function, so its Lean
    /// identifier can never be assumed to be the export name itself.
    module_prefix_flats: Vec<String>,
    /// Flat keys claimed by two DIFFERENT qualified names. A lookup on such a
    /// key must fail (fail-closed): citing either candidate would be a guess.
    ambiguous: std::collections::HashSet<String>,
}

struct FnSig {
    /// Fully qualified Lean identifier of this def (equals the bare name at
    /// entry level, `P.name` inside `namespace P`).
    lean_name: String,
    /// Dotted namespace prefix the def was parsed under (empty at entry level).
    prefix: String,
    params: Vec<String>,
    ret: String,
}

struct InductiveInfo {
    ctors: Vec<CtorInfo>,
}

struct CtorInfo {
    name: String,
    fields: Vec<String>,
}

impl ModelInfo {
    fn from_files(model_files: &[(String, String)]) -> Self {
        let mut info = Self::default();
        for (path, content) in model_files {
            if !path.ends_with(".lean") {
                continue;
            }
            info.parse_lean(content);
        }
        info
    }

    /// The Lean identifier the generated certificate must cite for the model
    /// of export `name`, or `None` when it cannot be derived (then the export
    /// must decline rather than cite a guess).
    ///
    /// - A parsed model def with this flat key resolves to its qualified name
    ///   (identity for entry-level defs).
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

    fn parse_lean(&mut self, content: &str) {
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
                let flat = prefix.replace('.', "_");
                if !self.module_prefix_flats.contains(&flat) {
                    self.module_prefix_flats.push(flat);
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
                && let Some((name, mut sig)) = parse_def_sig(line)
            {
                let qualified = qualify(&prefix, &name);
                let flat = qualified.replace('.', "_");
                sig.lean_name = qualified;
                sig.prefix = prefix.clone();
                if self.ambiguous.contains(&flat) {
                    i += 1;
                    continue;
                }
                match self.fns.get(&flat) {
                    Some(existing) if existing.lean_name != sig.lean_name => {
                        // Two distinct qualified names collide on one flat
                        // key: neither may ever resolve (fail-closed).
                        self.fns.remove(&flat);
                        self.ambiguous.insert(flat);
                    }
                    _ => {
                        self.fns.insert(flat, sig);
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
/// caller, which knows the namespace the line was parsed under.
fn parse_def_sig(line: &str) -> Option<(String, FnSig)> {
    let rest = line.strip_prefix("def ")?;
    let name = rest.split_whitespace().next()?.to_string();
    let after_name = rest[name.len()..].trim();
    let before_assign = after_name.strip_suffix(":=")?.trim();
    let ret_colon = before_assign.rfind(" : ")?;
    let params_part = before_assign[..ret_colon].trim();
    let ret = before_assign[ret_colon + 3..].trim().to_string();
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
    Some((
        name.clone(),
        FnSig {
            lean_name: name,
            prefix: String::new(),
            params,
            ret,
        },
    ))
}
