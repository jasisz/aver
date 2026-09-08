//! Scope-unit tests assemble already checked independent modules, then rebuild
//! their joint symbol table. Full import loading is covered by proof fixtures.

use super::*;
use crate::codegen::dafny::tests::ctx_from_source;

fn context(entry: &str, library: &str) -> CodegenContext {
    let mut ctx = ctx_from_source(entry, "Entry");
    let foreign = ctx_from_source(library, "Lib");
    let mut module = crate::codegen::test_module("Lib", &[], foreign.type_defs);
    module.fn_defs = foreign.fn_defs;
    module.exposes = module
        .fn_defs
        .iter()
        .map(|fd| fd.name.clone())
        .chain(
            module
                .type_defs
                .iter()
                .map(|td| crate::codegen::common::type_def_name(td).to_string()),
        )
        .collect();
    // Typechecking above used no imports. This synthetic header is the exact
    // visibility edge the symbol-table resolution test needs below.
    ctx.items.extend(
        crate::source::parse_source("module Entry\n    depends [Lib]\n    effects []\n").unwrap(),
    );
    ctx.modules.push(module);
    ctx.module_prefixes.insert("Lib".to_string());
    ctx.symbol_table = crate::ir::SymbolTable::build(&ctx.items, &ctx.modules);
    // The independently checked contexts' proof IDs precede the joint table;
    // these plain, nonrecursive fixtures need no proof-lowering contracts.
    ctx.proof_ir = Default::default();
    ctx
}

fn checker(ctx: &CodegenContext) -> Checker<'_> {
    Checker {
        ctx,
        functions: HashSet::new(),
        checking_functions: Vec::new(),
        laws: BTreeMap::new(),
        checking_laws: BTreeSet::new(),
        checked_laws: BTreeSet::new(),
        checked_types: HashSet::new(),
    }
}

fn call(name: &str, arg: &str) -> Spanned<Expr> {
    Spanned::bare(Expr::FnCall(
        Box::new(Spanned::bare(Expr::Ident(name.to_string()))),
        vec![Spanned::bare(Expr::Ident(arg.to_string()))],
    ))
}

#[test]
fn imported_formals_and_caller_arguments_keep_distinct_named_type_identities() {
    let ctx = context(
        "record Cell\n    value: Bool\nfn local(value: Cell) -> Bool\n    value.value\n",
        "record Cell\n    value: Int\nfn identity(value: Cell) -> Cell\n    value\n",
    );
    let mut checker = checker(&ctx);
    let imported = checker.annotation("Lib.Cell").unwrap();
    let local = checker.annotation("Cell").unwrap();
    assert_ne!(imported.named_id(), local.named_id());
    let env = Env::from([
        ("foreign".to_string(), imported.clone()),
        ("entry".to_string(), local),
    ]);
    assert_eq!(
        checker
            .expression(&call("Lib.identity", "foreign"), &env)
            .unwrap(),
        imported
    );
    assert!(
        checker
            .expression(&call("Lib.identity", "entry"), &env)
            .is_err()
    );
    assert_eq!(ctx.active_module_scope(), None);
    assert_eq!(
        checker.expression(&call("local", "entry"), &env).unwrap(),
        Type::Bool
    );
}

#[test]
fn foreign_body_resolves_private_helpers_in_the_declaring_module() {
    let mut ctx = context(
        "fn helper(n: Int) -> Int\n    n + 100\n",
        "fn helper(n: Int) -> Int\n    n + 1\nfn read(n: Int) -> Int\n    helper(n)\n",
    );
    ctx.modules[0].exposes.retain(|name| name == "read");
    ctx.symbol_table = crate::ir::SymbolTable::build(&ctx.items, &ctx.modules);
    let root_helper = ctx.symbol_table.resolve_fn_id_in("helper", None).unwrap();
    let foreign_helper = ctx
        .symbol_table
        .resolve_fn_id_in("helper", Some("Lib"))
        .unwrap();
    let mut checker = checker(&ctx);
    let env = Env::from([("n".to_string(), Type::Int)]);
    assert_eq!(
        checker.expression(&call("Lib.read", "n"), &env).unwrap(),
        Type::Int
    );
    assert!(checker.functions.contains(&foreign_helper));
    assert!(!checker.functions.contains(&root_helper));
    assert_eq!(ctx.active_module_scope(), None);
}

#[test]
fn failed_foreign_body_check_restores_caller_scope() {
    let ctx = context(
        "fn local(n: Int) -> Int\n    n + 1\n",
        "fn unsafeBody(n: Int) -> Int\n    ignored = Float.sqrt(4.0)\n    n\n",
    );
    let mut checker = checker(&ctx);
    let env = Env::from([("n".to_string(), Type::Int)]);
    assert!(
        checker
            .expression(&call("Lib.unsafeBody", "n"), &env)
            .is_err()
    );
    assert_eq!(ctx.active_module_scope(), None);
    // A new validation attempt must see the original caller, never Lib's
    // namespace left behind by the failed body traversal.
    let mut fresh = self::checker(&ctx);
    assert_eq!(
        fresh.expression(&call("local", "n"), &env).unwrap(),
        Type::Int
    );
}
