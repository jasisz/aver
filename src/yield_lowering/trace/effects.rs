//! Observe effectful segments of the actual protocol. The source observer and
//! this visitor share only the meaning of an oracle answer; the code visited
//! here is the generated Start/Answer body, not the retained source body.
use super::*;

impl Model<'_> {
    pub(super) fn observe_segments(
        &self,
        items: &mut [TopLevel],
        root: &FnDef,
    ) -> Result<(), String> {
        let mut reached = Vec::new();
        let mut imports = Vec::new();
        self.reachable(root, &mut reached, &mut imports)?;
        let imported: Vec<_> = imports.iter().map(|p| self.import_signature(p)).collect();
        reached.extend(imported.iter());
        for function in reached {
            let drive = self.child_drive(function);
            // Only root and composition drivers actually present in the model.
            if !items
                .iter()
                .any(|item| matches!(item, TopLevel::FnDef(fd) if fd.name == drive))
            {
                continue;
            }
            let mut compiler = source::Compiler::for_protocol(self, function);
            for item in items.iter_mut() {
                let TopLevel::FnDef(fd) = item else { continue };
                let body = std::sync::Arc::make_mut(&mut fd.body);
                for stmt in body.stmts_mut() {
                    let (Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) = stmt;
                    observe(expr, &drive, &mut compiler)?;
                }
            }
        }
        Ok(())
    }
}

fn observe(
    expr: &mut Spanned<Expr>,
    drive: &str,
    compiler: &mut source::Compiler<'_>,
) -> Result<(), String> {
    if let Expr::FnCall(callee, args) = &expr.node
        && build::dotted_name(callee).as_deref() == Some(drive)
        && args.len() == 5
    {
        let cursor = source::Cursor {
            inputs: args[1].clone(),
            position: args[2].clone(),
            events: args[3].clone(),
            consumed: args[4].clone(),
        };
        *expr = compiler.eval(&args[0], cursor, &|_, outcome, cursor| {
            Ok(build::call(
                drive,
                vec![
                    outcome,
                    cursor.inputs,
                    cursor.position,
                    cursor.events,
                    cursor.consumed,
                ],
                0,
            ))
        })?;
        return Ok(());
    }
    let mut result = Ok(());
    crate::codegen::expr_walk::for_each_child_mut(expr, &mut |child| {
        if result.is_ok() {
            result = observe(child, drive, compiler);
        }
    });
    result
}
