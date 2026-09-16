//! Local helper observations compose through the actual nested-call routers.
//! Each splice equation is an ordinary universally quantified Aver law. Its
//! right side reads the real protocol; the source observer stays independent.
use super::*;
pub(super) mod samples;

impl Model<'_> {
    pub(super) fn has_recursion(&self) -> bool {
        let mut reached = Vec::new();
        let mut imports = Vec::new();
        self.source(&self.protocol.fn_name).is_some_and(|root| {
            self.reachable(root, &mut reached, &mut imports).is_ok()
                && (reached.iter().any(|fd| calls_itself(fd))
                    || imports
                        .iter()
                        .any(|p| p.trace.as_ref().is_some_and(|t| t.recursive)))
        })
    }

    pub(super) fn composition_dependencies(&self, protocol: &ProcessProtocol) -> Vec<String> {
        let mut reached = Vec::new();
        let mut imports = Vec::new();
        let Some(root) = self.source(&self.protocol.fn_name) else {
            return vec![];
        };
        if self.reachable(root, &mut reached, &mut imports).is_err()
            || (!reached
                .iter()
                .any(|fd| fd.name != root.name && calls_itself(fd))
                && !imports
                    .iter()
                    .any(|p| p.trace.as_ref().is_some_and(|t| t.recursive)))
        {
            return vec![];
        }
        let mut dependencies = Vec::new();
        for nest in &protocol.nests {
            if let Some(child) = self.source(&nest.callee) {
                let law = format!("{}.correspondence", self.source_name(child));
                if !dependencies.contains(&law) {
                    dependencies.push(law);
                }
                dependencies.push(format!(
                    "{}Splice{}Observed.splice",
                    self.prefix, nest.router
                ));
            } else if let Some(child) = self.imported.get(&nest.callee) {
                let signature = self.import_signature(child);
                let law = format!("{}.correspondence", self.source_name(&signature));
                if !dependencies.contains(&law) {
                    dependencies.push(law);
                }
                dependencies.push(format!(
                    "{}Splice{}Observed.splice",
                    self.prefix, nest.router
                ));
            }
        }
        dependencies
    }

    pub(super) fn child_drive(&self, fd: &FnDef) -> String {
        if fd.name == self.protocol.fn_name {
            format!("{}Drive", self.prefix)
        } else {
            format!("{}Drive{}", self.prefix, build::capitalize(&fd.name))
        }
    }

    pub(super) fn composition(
        &self,
        reached: &[&FnDef],
        imports: &[&ProcessProtocol],
    ) -> Result<String, String> {
        if !reached
            .iter()
            .any(|fd| fd.name != self.protocol.fn_name && calls_itself(fd))
            && !imports
                .iter()
                .any(|p| p.trace.as_ref().is_some_and(|t| t.recursive))
        {
            return Ok(String::new());
        }
        let u = &self.upper;
        let cursor = [
            ("inputs".into(), format!("List<{u}Input>")),
            ("position".into(), "Int".into()),
            ("events".into(), format!("List<{u}Event>")),
            ("consumed".into(), "Int".into()),
        ];
        let cursor_args = "inputs, position, events, consumed";
        let mut out = String::new();
        let signatures: Vec<_> = imports.iter().map(|p| self.import_signature(p)).collect();
        for (protocol, signature) in imports.iter().zip(&signatures) {
            out.push_str(&self.import_composition(protocol, signature)?);
        }
        for fd in reached.iter().copied() {
            let Some(protocol) = self.local_protocols.iter().find(|p| p.fn_name == fd.name) else {
                continue;
            };
            let drive = self.child_drive(fd);
            let result = self.result_type(fd);
            if fd.name != self.protocol.fn_name {
                out.push_str(&self.drive(protocol, fd, &drive));
                let params: Vec<_> = fd
                    .params
                    .iter()
                    .enumerate()
                    .map(|(i, (_, ty))| (format!("arg{i}"), ty.clone()))
                    .chain(cursor.iter().cloned())
                    .collect();
                let args = names(&params);
                let start_args = names(&params[..fd.params.len()]);
                let entry = format!("{drive}Start");
                out.push_str(&format!(
                    "\nfn {entry}({}) -> {result}\n    {drive}({}({start_args}), {cursor_args})\n",
                    declarations(&params),
                    protocol.start
                ));
                let source = self.source_name(fd);
                out.push_str(&law(
                    self,
                    &source,
                    "correspondence",
                    &params,
                    &format!("{source}({args}) == {entry}({args})"),
                    &self.composition_dependencies(protocol),
                )?);
            }
            for nest in &protocol.nests {
                let (child, child_protocol) =
                    if let Some(child) = reached.iter().find(|child| child.name == nest.callee) {
                        (
                            *child,
                            self.local_protocols
                                .iter()
                                .find(|p| p.fn_name == child.name)
                                .ok_or("helper protocol missing")?,
                        )
                    } else {
                        let index = imports
                            .iter()
                            .position(|p| p.fn_name == nest.callee)
                            .ok_or("imported helper protocol missing")?;
                        (&signatures[index], imports[index])
                    };
                let router = self
                    .segments
                    .iter()
                    .find(|f| f.name == nest.router)
                    .ok_or("nested router missing")?;
                let child_drive = self.child_drive(child);
                let child_result = self.result_type(child);
                let splice = format!("{}Splice{}", self.prefix, nest.router);
                let lifted = format!("{splice}Drive");
                let composed = format!("{splice}Observed");
                let fields: Vec<_> = router
                    .params
                    .iter()
                    .skip(1)
                    .enumerate()
                    .map(|(i, (_, ty))| (format!("field{i}"), ty.clone()))
                    .collect();
                let field_args = names(&fields);
                let extra_args = if fields.is_empty() {
                    String::new()
                } else {
                    format!(", {field_args}")
                };
                let extra_params = if fields.is_empty() {
                    String::new()
                } else {
                    format!(", {}", declarations(&fields))
                };
                out.push_str(&format!("\nfn {splice}(observed: {child_result}{extra_params}) -> {result}\n    match observed.value\n        Option.Some(value) -> {drive}({router}({outcome}.Done(value){extra_args}), observed.remaining, observed.position, observed.events, observed.consumed)\n        Option.None -> {result}(remaining = observed.remaining, position = observed.position, consumed = observed.consumed, events = observed.events, value = Option.None, pending = observed.pending, valid = observed.valid)\n", router=nest.router, outcome=child_protocol.outcome));
                let params: Vec<_> = [("outcome".into(), child_protocol.outcome.clone())]
                    .into_iter()
                    .chain(cursor.iter().cloned())
                    .chain(fields.clone())
                    .collect();
                out.push_str(&format!("\nfn {composed}({params}) -> {result}\n    {splice}({child_drive}(outcome, {cursor_args}){extra_args})\n\nfn {lifted}({params}) -> {result}\n    {drive}({router}(outcome{extra_args}), {cursor_args})\n", params=declarations(&params), router=nest.router));
                // The outcome sample exercises a completed helper; the theorem
                // quantifies over every outcome, including all waiting states.
                let witness = format!(
                    "{}.Done({})",
                    child_protocol.outcome,
                    samples::witness(self, &child.return_type)?
                );
                out.push_str(&law_with_outcome(
                    self,
                    &composed,
                    &params,
                    &format!(
                        "{composed}({args}) == {lifted}({args})",
                        args = names(&params)
                    ),
                    &witness,
                )?);
            }
        }
        Ok(out)
    }
}

pub(super) fn declarations(params: &[(String, String)]) -> String {
    params
        .iter()
        .map(|(n, t)| format!("{n}: {t}"))
        .collect::<Vec<_>>()
        .join(", ")
}
pub(super) fn names(params: &[(String, String)]) -> String {
    params
        .iter()
        .map(|(n, _)| n.as_str())
        .collect::<Vec<_>>()
        .join(", ")
}
pub(super) fn law(
    model: &Model<'_>,
    target: &str,
    label: &str,
    params: &[(String, String)],
    claim: &str,
    using: &[String],
) -> Result<String, String> {
    let mut out = format!("\nverify {target} law {label}\n");
    for (name, ty) in params {
        out.push_str(&format!(
            "    given {name}: {ty} = [{}]\n",
            samples::witness(model, ty)?
        ));
    }
    out.push_str(&format!(
        "    using [{}]\n    {claim} holds\n",
        using.join(", ")
    ));
    Ok(out)
}
fn law_with_outcome(
    model: &Model<'_>,
    target: &str,
    params: &[(String, String)],
    claim: &str,
    witness: &str,
) -> Result<String, String> {
    let mut out = format!(
        "\nverify {target} law splice\n    given outcome: {} = [{witness}]\n",
        params[0].1
    );
    for (name, ty) in &params[1..] {
        out.push_str(&format!(
            "    given {name}: {ty} = [{}]\n",
            samples::witness(model, ty)?
        ));
    }
    out.push_str(&format!("    using []\n    {claim} holds\n"));
    Ok(out)
}
