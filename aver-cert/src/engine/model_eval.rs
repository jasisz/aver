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
    fns: std::collections::HashMap<String, FnSig>,
    inductives: std::collections::HashMap<String, InductiveInfo>,
}

struct FnSig {
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

    fn parse_lean(&mut self, content: &str) {
        let lines: Vec<&str> = content.lines().collect();
        let mut i = 0usize;
        while i < lines.len() {
            let line = lines[i].trim();
            if let Some(name) = line
                .strip_prefix("inductive ")
                .and_then(|s| s.split_whitespace().next())
            {
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
                self.inductives
                    .insert(name.to_string(), InductiveInfo { ctors });
                continue;
            }
            if line.starts_with("def ")
                && line.contains(" : ")
                && line.ends_with(":=")
                && let Some((name, sig)) = parse_def_sig(line)
            {
                self.fns.insert(name, sig);
            }
            i += 1;
        }
    }
}

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
    Some((name, FnSig { params, ret }))
}
