//! `aver check` and `aver compile` of a program with `lazy f(…) knowing
//! f.law(…)`: the compiled program answers the law's projection of a deferred
//! call from the law, without making the call, so the law has to hold for
//! every value. Each such law is proved here, by the proof-step producers and
//! the proof kernel, as `aver proof --backend aver` proves it, and the program
//! is refused when one does not close. With `--trust-laws` an unproved law is
//! let through after its samples pass, and the compiled program checks it at
//! every deferred call it makes.

use std::path::Path;

use colored::Colorize;

/// What the program's `knowing` stand on: the laws proved, and the laws let
/// through unproved by `--trust-laws`, by name.
#[derive(Default)]
pub(super) struct Stands {
    pub proved: Vec<String>,
    pub trusted: Vec<String>,
}

/// Whether any module of the program the entry `file` names has a `knowing`.
/// Reads and parses the modules only.
fn any(file: &str, module_root: &str) -> bool {
    let Ok(source) = std::fs::read_to_string(file) else {
        return false;
    };
    let Ok(items) = super::parse_file(&source, module_root, file) else {
        return false;
    };
    if aver::codegen::knowing::in_items(&items) {
        return true;
    }
    aver::source::load_program(
        Path::new(file),
        &source,
        &items,
        module_root,
        aver::source::LoadMode::Tolerant,
    )
    .is_ok_and(|program| {
        program
            .dependencies()
            .iter()
            .any(|m| aver::codegen::knowing::in_items(&m.items))
    })
}

/// The laws the program's `knowing` stand on, each checked; the reasons the
/// program is refused, when it is.
pub(super) fn check(
    file: &str,
    module_root_override: Option<&str>,
    trust_laws: bool,
) -> Result<Stands, Vec<String>> {
    let module_root = super::resolve_module_root(module_root_override);
    if !any(file, &module_root) {
        return Ok(Stands::default());
    }
    let started = std::time::Instant::now();
    let (ctx, _) = super::build_codegen_context(
        file,
        None,
        module_root_override,
        false,
        &super::super::cli::CompilePolicyMode::Embed,
        None,
        false,
        false,
        true,
        true,
        true,
        true, // proof_command
    );
    let mut refused = Vec::new();
    let mut laws: Vec<(String, Option<String>, String)> = Vec::new();
    for use_ in aver::codegen::knowing::uses(&ctx) {
        match aver::codegen::knowing::check(&ctx, &use_) {
            Ok(law) => {
                if !laws.iter().any(|(key, _, _)| *key == law.key) {
                    laws.push((law.key, law.scope, law.law.name.clone()));
                }
            }
            Err(why) => refused.push(format!(
                "{}line {}: `knowing`: {why}",
                use_.scope.map(|m| format!("{m}, ")).unwrap_or_default(),
                use_.call.line
            )),
        }
    }
    let judged = super::proof_aver::judge(&ctx, None, false);
    let mut stands = Stands::default();
    for (key, scope, law) in laws {
        match judged.closed(&key) {
            Ok(()) => stands.proved.push(key),
            Err(why) if !trust_laws => refused.push(format!(
                "law `{key}`, which a `knowing` stands on, is {why}. The compiled program \
                 answers it without making the call, so it must hold for every value: prove it \
                 (`aver proof --backend aver`), or build with `--trust-laws` to run its samples \
                 now and check it at every deferred call the program makes"
            )),
            Err(_) => match samples(file, &module_root, scope.as_deref(), &key, &law) {
                Ok(()) => stands.trusted.push(key),
                Err(why) => refused.push(why),
            },
        }
    }
    if std::env::var_os("AVER_KNOWING_TIMING").is_some() {
        eprintln!(
            "knowing: {} law(s) checked in {:.1} ms",
            stands.proved.len() + stands.trusted.len(),
            started.elapsed().as_secs_f64() * 1000.0
        );
    }
    match refused.is_empty() {
        true => Ok(stands),
        false => Err(refused),
    }
}

/// [`check`] for a command that stops on a refusal: prints why and exits,
/// and says which laws the build trusts.
pub(super) fn check_or_exit(
    file: &str,
    module_root_override: Option<&str>,
    trust_laws: bool,
) -> Stands {
    match check(file, module_root_override, trust_laws) {
        Ok(stands) => {
            if !stands.trusted.is_empty() {
                eprintln!(
                    "{}",
                    format!(
                        "built with laws that are not proved, checked while the program runs: {}",
                        stands.trusted.join(", ")
                    )
                    .yellow()
                );
            }
            stands
        }
        Err(refused) => {
            for why in refused {
                eprintln!("{}", why.red());
            }
            std::process::exit(1);
        }
    }
}

/// Runs the samples of the law `law` of the module `scope` (`None`: the
/// entry), as `aver verify` runs them.
fn samples(
    file: &str,
    module_root: &str,
    scope: Option<&str>,
    key: &str,
    law: &str,
) -> Result<(), String> {
    let path = match scope {
        None => file.to_string(),
        Some(module) => aver::source::find_module_file(module, module_root)
            .map(|p| p.to_string_lossy().into_owned())
            .ok_or_else(|| format!("law `{key}`: cannot find module `{module}`"))?,
    };
    let source = std::fs::read_to_string(&path).map_err(|e| format!("{path}: {e}"))?;
    let items = super::parse_file(&source, module_root, &path)?;
    let config = super::load_runtime_policy(module_root).ok().flatten();
    let results = aver::diagnostics::vm_verify::run_verify_for_items_vm(
        items,
        config,
        Some(module_root),
        &path,
    )?;
    let fn_name = key.rsplit('.').nth(1).unwrap_or_default();
    let ran: Vec<_> = results
        .iter()
        .filter(|r| r.is_law && r.block_label == format!("{fn_name} law {law}"))
        .collect();
    let failed: usize = ran.iter().map(|r| r.failed).sum();
    match (ran.is_empty(), failed) {
        (true, _) => Err(format!("law `{key}` has no samples that ran")),
        (false, 0) => Ok(()),
        (false, n) => Err(format!(
            "law `{key}` is not proved, and {n} of its samples fail (`aver verify` shows them)"
        )),
    }
}
