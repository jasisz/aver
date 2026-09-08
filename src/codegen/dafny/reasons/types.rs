//! Strict structural typing for guided proof admission. Named types resolve
//! through the symbol table, and every field is checked before the type enters
//! the subset. Inference holes exist only in empty containers/unused constructor
//! alternatives; they never admit source `Unknown`, type variables, or recovery.

use super::*;

pub(super) fn hole() -> Type {
    Type::Var("$guidance_empty".to_string())
}

pub(super) fn merge(left: &Type, right: &Type) -> Result<Type, String> {
    if *left == hole() {
        return Ok(right.clone());
    }
    if *right == hole() || left == right {
        return Ok(left.clone());
    }
    Ok(match (left, right) {
        (Type::List(a), Type::List(b)) => Type::List(Box::new(merge(a, b)?)),
        (Type::Option(a), Type::Option(b)) => Type::Option(Box::new(merge(a, b)?)),
        (Type::Result(a, b), Type::Result(c, d)) => {
            Type::Result(Box::new(merge(a, c)?), Box::new(merge(b, d)?))
        }
        (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => Type::Tuple(
            a.iter()
                .zip(b)
                .map(|(a, b)| merge(a, b))
                .collect::<Result<_, _>>()?,
        ),
        _ => {
            return Err(format!(
                "unsupported expression type: {left:?} versus {right:?}"
            ));
        }
    })
}

impl<'a> Checker<'a> {
    pub(super) fn annotation(&mut self, source: &str) -> Result<Type, String> {
        let ty = crate::types::parse_type_str_strict(source)
            .map_err(|_| format!("unsupported type {source}"))?;
        self.supported_type(ty)
    }

    fn supported_type(&mut self, ty: Type) -> Result<Type, String> {
        Ok(match ty {
            Type::Int | Type::Bool | Type::Str => ty,
            Type::List(inner) => Type::List(Box::new(self.supported_type(*inner)?)),
            Type::Option(inner) => Type::Option(Box::new(self.supported_type(*inner)?)),
            Type::Result(ok, err) => Type::Result(
                Box::new(self.supported_type(*ok)?),
                Box::new(self.supported_type(*err)?),
            ),
            Type::Tuple(items) => Type::Tuple(
                items
                    .into_iter()
                    .map(|item| self.supported_type(item))
                    .collect::<Result<_, _>>()?,
            ),
            // syntax-discovery-only: annotation() supplies freshly parsed source
            // types, without HIR IDs. Resolve the name immediately to TypeId;
            // scope, fields and the returned type all use that canonical ID.
            Type::Named { name, .. } => {
                let scope = self.ctx.active_module_scope();
                let id = self
                    .ctx
                    .symbol_table
                    .resolve_type_id_in(&name, scope.as_deref())
                    .ok_or_else(|| format!("no declared type {name}"))?;
                let entry = self.ctx.symbol_table.type_entry(id);
                if entry.key.scope_str() != scope.as_deref() || entry.is_capability_resource {
                    return Err(format!(
                        "imported or provider type {name} is outside this pilot"
                    ));
                }
                if crate::codegen::common::find_refined_type_scoped(
                    self.ctx,
                    &entry.key.name,
                    scope.as_deref(),
                )
                .is_some()
                {
                    return Err(format!("refined type {name} is outside this pilot"));
                }
                let resolved = Type::named_resolved(id, entry.key.canonical());
                let definition = self.definition(&resolved)?;
                if self.checked_types.insert(id) {
                    let fields: Vec<&str> = match definition {
                        TypeDef::Product { fields, .. } => {
                            let mut names = HashSet::new();
                            if fields
                                .iter()
                                .any(|(name, _)| !names.insert(aver_name_to_dafny(name)))
                            {
                                return Err(format!("ambiguous emitted fields in {name}"));
                            }
                            fields.iter().map(|(_, ty)| ty.as_str()).collect()
                        }
                        TypeDef::Sum { variants, .. } => {
                            let mut names = HashSet::new();
                            if variants.is_empty()
                                || variants.iter().any(|variant| !names.insert(&variant.name))
                            {
                                return Err(format!("empty or ambiguous variants in {name}"));
                            }
                            variants
                                .iter()
                                .flat_map(|variant| variant.fields.iter().map(String::as_str))
                                .collect()
                        }
                    };
                    for field in fields {
                        self.annotation(field)?;
                    }
                }
                resolved
            }
            _ => return Err(format!("unsupported first-order type {ty:?}")),
        })
    }

    fn definition(&self, ty: &Type) -> Result<&'a TypeDef, String> {
        let Type::Named { id: Some(id), name } = ty else {
            return Err("a declared local datatype is required".to_string());
        };
        let key = &self.ctx.symbol_table.type_entry(*id).key;
        let definitions = match key.scope_str() {
            Some(scope) => self
                .ctx
                .modules
                .iter()
                .find(|module| module.prefix == scope)
                .map(|module| module.type_defs.as_slice())
                .unwrap_or_default(),
            None => self.ctx.type_defs.as_slice(),
        };
        definitions
            .iter()
            .find(|td| crate::codegen::common::type_def_name(td) == key.name)
            .ok_or_else(|| format!("no source datatype definition for {name}"))
    }

    fn variant(&mut self, name: &str) -> Result<(Type, Vec<Type>), String> {
        let (owner, variant) = name.rsplit_once('.').ok_or("unqualified constructor")?;
        let ty = self.annotation(owner)?;
        let TypeDef::Sum { variants, .. } = self.definition(&ty)? else {
            return Err(format!("{owner} is not an algebraic datatype"));
        };
        let fields = variants
            .iter()
            .find(|v| v.name == variant)
            .ok_or_else(|| format!("unknown constructor {name}"))?
            .fields
            .iter()
            .map(|field| self.annotation(field))
            .collect::<Result<_, _>>()?;
        Ok((ty, fields))
    }

    pub(super) fn constructor(
        &mut self,
        name: &str,
        args: &[Spanned<Expr>],
        env: &Env,
    ) -> Result<Type, String> {
        match (name, args) {
            ("Option.None", []) => return Ok(Type::Option(Box::new(hole()))),
            ("Option.Some", [value]) => {
                return Ok(Type::Option(Box::new(self.expression(value, env)?)));
            }
            ("Result.Ok", [value]) => {
                return Ok(Type::Result(
                    Box::new(self.expression(value, env)?),
                    Box::new(hole()),
                ));
            }
            ("Result.Err", [value]) => {
                return Ok(Type::Result(
                    Box::new(hole()),
                    Box::new(self.expression(value, env)?),
                ));
            }
            _ => {}
        }
        let (ty, fields) = self.variant(name)?;
        if fields.len() != args.len() {
            return Err(format!("unsupported constructor arity {name}"));
        }
        for (arg, field) in args.iter().zip(fields) {
            self.expect(arg, env, field)?;
        }
        Ok(ty)
    }

    pub(super) fn field(&mut self, ty: &Type, field: &str) -> Result<Type, String> {
        let TypeDef::Product { fields, .. } = self.definition(ty)? else {
            return Err("only record field projections are supported".to_string());
        };
        let (_, annotation) = fields
            .iter()
            .find(|(name, _)| name == field)
            .ok_or_else(|| format!("unknown record field {field}"))?;
        self.annotation(annotation)
    }

    pub(super) fn record(
        &mut self,
        name: &str,
        base: Option<&Spanned<Expr>>,
        values: &[(String, Spanned<Expr>)],
        env: &Env,
    ) -> Result<Type, String> {
        let ty = self.annotation(name)?;
        let TypeDef::Product { fields, .. } = self.definition(&ty)? else {
            return Err(format!("{name} is not a record"));
        };
        if let Some(base) = base {
            self.expect(base, env, ty.clone())?;
        }
        let mut seen = HashSet::new();
        for (field, value) in values {
            if !seen.insert(field.as_str()) {
                return Err(format!("duplicate record field {field}"));
            }
            let expected = self.field(&ty, field)?;
            self.expect(value, env, expected)?;
        }
        if base.is_none() && fields.len() != seen.len() {
            return Err(format!("missing record fields in {name}"));
        }
        Ok(ty)
    }

    pub(super) fn pattern(
        &mut self,
        pattern: &Pattern,
        ty: &Type,
        env: &mut Env,
        covered: &mut BTreeSet<String>,
    ) -> Result<(), String> {
        let mut added = Env::new();
        self.pattern_inner(pattern, ty, &mut added, covered)?;
        for (name, ty) in added {
            // Exact source shadowing is legal. Distinct source spellings must
            // never collapse to one emitted Dafny binder.
            env.remove(&name);
            bind(env, &name, ty)?;
        }
        Ok(())
    }

    fn pattern_inner(
        &mut self,
        pattern: &Pattern,
        ty: &Type,
        added: &mut Env,
        covered: &mut BTreeSet<String>,
    ) -> Result<(), String> {
        match (pattern, ty) {
            (Pattern::Wildcard, _) => {
                covered.insert("*".to_string());
            }
            (Pattern::Ident(name), _) => {
                bind(added, name, ty.clone())?;
                covered.insert("*".to_string());
            }
            (Pattern::Literal(Literal::Bool(value)), Type::Bool) => {
                covered.insert(value.to_string());
            }
            (Pattern::Literal(Literal::Int(_) | Literal::BigInt(_)), Type::Int)
            | (Pattern::Literal(Literal::Str(_)), Type::Str) => {}
            (Pattern::EmptyList, Type::List(_)) => {
                covered.insert("empty".to_string());
            }
            (Pattern::Cons(head, tail), Type::List(inner)) => {
                if head != "_" {
                    bind(added, head, *inner.clone())?;
                }
                if tail != "_" {
                    bind(added, tail, ty.clone())?;
                }
                covered.insert("cons".to_string());
            }
            (Pattern::Tuple(patterns), Type::Tuple(types)) if patterns.len() == types.len() => {
                let mut total = true;
                for (pattern, ty) in patterns.iter().zip(types) {
                    let mut field_covered = BTreeSet::new();
                    self.pattern_inner(pattern, ty, added, &mut field_covered)?;
                    total &= self.exhaustive(ty, &field_covered).is_ok();
                }
                if total {
                    covered.insert("*".to_string());
                }
            }
            (Pattern::Constructor(name, names), _) => {
                let fields = match (name.as_str(), ty) {
                    ("Option.None", Type::Option(_)) => vec![],
                    ("Option.Some", Type::Option(inner)) => vec![*inner.clone()],
                    ("Result.Ok", Type::Result(ok, _)) => vec![*ok.clone()],
                    ("Result.Err", Type::Result(_, err)) => vec![*err.clone()],
                    _ => {
                        let (owner, fields) = self.variant(name)?;
                        merge(&owner, ty)?;
                        fields
                    }
                };
                if fields.len() != names.len() {
                    return Err(format!("unsupported pattern arity {name}"));
                }
                for (name, field) in names.iter().zip(fields) {
                    if name != "_" {
                        bind(added, name, field)?;
                    }
                }
                covered.insert(name.rsplit('.').next().unwrap_or(name).to_string());
            }
            _ => return Err("unsupported or mismatched pattern".to_string()),
        }
        Ok(())
    }

    pub(super) fn exhaustive(&self, ty: &Type, covered: &BTreeSet<String>) -> Result<(), String> {
        if covered.contains("*") {
            return Ok(());
        }
        let variants: Vec<&str> = match ty {
            Type::Bool => vec!["true", "false"],
            Type::List(_) => vec!["empty", "cons"],
            Type::Option(_) => vec!["None", "Some"],
            Type::Result(_, _) => vec!["Ok", "Err"],
            Type::Named { .. } => match self.definition(ty)? {
                TypeDef::Sum { variants, .. } => variants
                    .iter()
                    .map(|variant| variant.name.as_str())
                    .collect(),
                _ => return Err("record match requires a wildcard or binder".to_string()),
            },
            _ => return Err("match requires a catch-all arm".to_string()),
        };
        if variants.iter().all(|name| covered.contains(*name)) {
            Ok(())
        } else {
            Err("match must cover every constructor".to_string())
        }
    }
}
