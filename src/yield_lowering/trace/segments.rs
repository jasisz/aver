//! Module-owned observations of the real protocol's effectful segments.
//!
//! A segment may stop on an in-place effect before producing a protocol outcome.
//! Its concrete result therefore carries both the outcome and the observation
//! cursor. Import adapters preserve the caller's original unconsumed tape.
use super::*;
use crate::yield_lowering::ProcessTraceSegment;

impl<'a> Model<'a> {
    pub(super) fn observed_segments(&self) -> Vec<&'a FnDef> {
        std::iter::once(&self.protocol.start)
            .chain(self.protocol.kinds.iter().map(|kind| &kind.answer_fn))
            .filter_map(|name| {
                self.segments
                    .iter()
                    .find(|fd| fd.name == *name && !fd.effects.is_empty())
            })
            .collect()
    }

    pub(super) fn segment_exports(&self, requested: bool) -> Vec<ProcessTraceSegment> {
        self.observed_segments()
            .into_iter()
            .map(|fd| {
                let observer = self.source_name(fd);
                let contracted =
                    requested && self.contractible(&self.observation_params(&fd.params));
                ProcessTraceSegment {
                    function: fd.name.clone(),
                    result: self.result_type(fd),
                    params: fd.params.clone(),
                    cursor: if contracted {
                        format!("{observer}Cursor")
                    } else {
                        String::new()
                    },
                    samples: self.segment_sample_names(fd),
                    observer,
                }
            })
            .collect()
    }

    /// Parameters of one observation: the segment's own arguments, renamed
    /// positionally, followed by the shared observation cursor.
    fn observation_params(&self, params: &[(String, String)]) -> Vec<(String, String)> {
        params
            .iter()
            .enumerate()
            .map(|(index, (_, ty))| (format!("arg{index}"), ty.clone()))
            .chain([
                ("inputs".into(), format!("List<{}Input>", self.upper)),
                ("position".into(), "Int".into()),
                ("events".into(), format!("List<{}Event>", self.upper)),
                ("consumed".into(), "Int".into()),
            ])
            .collect()
    }

    /// Whether a law over this observation can be written here at all: every
    /// parameter needs a sample, and a protocol state has one only where its
    /// owner published it.
    fn contractible(&self, params: &[(String, String)]) -> bool {
        params
            .iter()
            .all(|(_, ty)| composition::samples::witness(self, ty).is_ok())
    }

    /// The cursor predicate of one observation: the shared validity template
    /// read on the observation's own result, and the wrapper a law is stated
    /// about.
    fn cursor_predicate(&self, name: &str, result: &str, params: &[(String, String)]) -> String {
        format!(
            r#"
fn {name}CursorValid(inputs: List<{u}Input>, consumed: Int, observed: {result}) -> Bool
    used = observed.consumed - consumed
    Bool.and(used >= 0, Bool.and(used <= List.len(inputs), observed.remaining == List.drop(inputs, used)))

fn {name}Cursor({declared}) -> Bool
    {name}CursorValid(inputs, consumed, {name}({args}))
"#,
            u = self.upper,
            declared = composition::declarations(params),
            args = composition::names(params),
        )
    }

    /// Interface of the module-owned segment observations, checked where the
    /// segments are defined so a caller never reopens an observer's body.
    ///
    /// `segmentCursor` states that an observation leaves the tape at the suffix
    /// its own cursor reports; `eventsPrefix` that the incoming event history
    /// is only a prefix and no other field reads it; `step` that one step of
    /// the protocol observer is that observation followed by the generic
    /// continuation, for every answered segment, and for the start that the
    /// protocol entry from a cursor is. All three are keyed on the generated
    /// observation shape and say nothing about what a segment computes.
    ///
    /// Each statement is made about a wrapper (the cursor predicate, the
    /// prefixed form, the protocol observer), never about the observation
    /// itself. The wrappers date from when a function that owned a law was
    /// emitted with dependent matchers, so every proof that opened the
    /// observation had to normalize a heavier term (#1404); a law no longer
    /// changes the definition it is about, and the wrappers stay because
    /// they name the interface a caller cites without opening a body.
    ///
    /// Returns the lifted cursor laws checked here, for the protocol
    /// observer's own cursor contract to cite.
    pub(super) fn segment_contracts(
        &self,
        items: &mut Vec<TopLevel>,
        imports: &[&'a ProcessProtocol],
    ) -> Result<Vec<String>, String> {
        let u = &self.upper;
        let mut text = String::new();
        // Lifted imported observations first. An observer of this module that
        // reads one states its own cursor through the lifted cursor, and a law
        // cites only laws before it.
        let mut lifted = Vec::new();
        for protocol in imports {
            let Some(trace) = protocol.trace.as_ref() else {
                continue;
            };
            for segment in &trace.segments {
                let signature = self.segment_signature(protocol, segment);
                let name = self.source_name(&signature);
                let params = self.observation_params(&segment.params);
                if segment.cursor.is_empty() || !self.contractible(&params) {
                    continue;
                }
                text.push_str(&self.cursor_predicate(
                    &name,
                    &self.result_type(&signature),
                    &params,
                ));
                text.push_str(&composition::law(
                    self,
                    &format!("{name}Cursor"),
                    "segmentCursor",
                    &params,
                    &format!("{name}Cursor({})", composition::names(&params)),
                    &[format!("{}.segmentCursor", segment.cursor)],
                )?);
                lifted.push(name);
            }
        }
        // The protocol observer's own cursor contract reads the lifted
        // observations' cursors; its own segments it still opens itself.
        let cursors: Vec<String> = lifted
            .iter()
            .map(|name| format!("{name}Cursor.segmentCursor"))
            .collect();
        for fd in self.observed_segments() {
            let observer = self.source_name(fd);
            let result = self.result_type(fd);
            let params = self.observation_params(&fd.params);
            if !self.contractible(&params) {
                continue;
            }
            let args = composition::names(&params);
            let empty = params
                .iter()
                .map(|(name, _)| match name.as_str() {
                    "events" => "[]",
                    other => other,
                })
                .collect::<Vec<_>>()
                .join(", ");
            text.push_str(&self.cursor_predicate(&observer, &result, &params));
            text.push_str(&format!(
                "\nfn {observer}Prefixed(events: List<{u}Event>, observed: {result}) -> {result}\n    {result}.update(observed, events = List.concat(events, observed.events))\n"
            ));
            text.push_str(&composition::law(
                self,
                &format!("{observer}Cursor"),
                "segmentCursor",
                &params,
                &format!("{observer}Cursor({args})"),
                &lifted
                    .iter()
                    .filter(|name| reads(items, &observer, name))
                    .map(|name| format!("{name}Cursor.segmentCursor"))
                    .collect::<Vec<_>>(),
            )?);
            text.push_str(&composition::law(
                self,
                &format!("{observer}Prefixed"),
                "eventsPrefix",
                &params,
                &format!("{observer}({args}) == {observer}Prefixed(events, {observer}({empty}))"),
                &[],
            )?);
        }
        text.push_str(&self.drive_steps()?);
        if text.is_empty() {
            return Ok(cursors);
        }
        let tokens = crate::lexer::Lexer::new(&text)
            .tokenize()
            .map_err(|e| e.to_string())?;
        items.extend(
            crate::parser::Parser::new_compiler_generated(tokens)
                .parse()
                .map_err(|e| e.to_string())?,
        );
        Ok(cursors)
    }

    /// One protocol step per answered request segment: the answer, rewritten
    /// as that observation followed by the generic continuation. The token,
    /// event, position and consumed expressions are the ones the protocol
    /// observer itself prints for the kind.
    ///
    /// The start segment's step is the protocol entry from a cursor: that
    /// entry is the start observation followed by the generic continuation.
    /// An importer's entry agreement reads it, with the start cursor, instead
    /// of opening the owner's entry. A law stated about a function no longer
    /// changes its body (#1404), so stating one about the entry moves nothing.
    fn drive_steps(&self) -> Result<String, String> {
        let u = &self.upper;
        let drive = format!("{}Drive", self.prefix);
        let root = self
            .source(&self.protocol.fn_name)
            .ok_or("missing retained source")?;
        let root_result = self.result_type(root);
        let mut out = String::new();
        for fd in self.observed_segments() {
            let observer = self.source_name(fd);
            let params = self.observation_params(&fd.params);
            if !self.contractible(&params) {
                continue;
            }
            let result = self.result_type(fd);
            let step = format!("{drive}Step{}", build::capitalize(&fd.name));
            out.push_str(&format!("\nfn {step}(observed: {result}) -> {root_result}\n    match observed.value\n        Option.Some(value) -> {drive}(value, observed.remaining, observed.position, observed.events, observed.consumed)\n        Option.None -> {root_result}(remaining = observed.remaining, position = observed.position, consumed = observed.consumed, events = observed.events, value = Option.None, pending = observed.pending, valid = observed.valid)\n"));
            let label = format!("step{}", build::capitalize(&fd.name));
            // The start segment is stepped by the protocol entry itself: the
            // trace from a cursor is the start observation followed by the
            // generic continuation.
            if fd.name == self.protocol.start {
                let args = composition::names(&params);
                out.push_str(&composition::law(
                    self,
                    &drive,
                    &label,
                    &params,
                    &format!(
                        "__{}ProtocolTraceFrom({args}) == {step}({observer}({args}))",
                        self.protocol.fn_name
                    ),
                    &[],
                )?);
                continue;
            }
            let kind = self
                .protocol
                .kinds
                .iter()
                .find(|kind| kind.answer_fn == fd.name)
                .ok_or("observed segment answers no request kind")?;
            let trace_name = kind
                .operation
                .as_ref()
                .and_then(|operation| self.operations.get(operation))
                .map_or(kind.name.as_str(), |kind| kind.name.as_str());
            let call_args: Vec<String> = (0..kind.arg_types.len())
                .map(|index| format!("arg{index}"))
                .collect();
            let mut binders: Vec<(String, String)> = kind
                .arg_types
                .iter()
                .enumerate()
                .map(|(index, ty)| (format!("arg{index}"), ty.clone()))
                .collect();
            binders.push(("state".into(), kind.state.clone()));
            let (token, observed_args, position, events) = match &kind.answer_type {
                Some(answer) if kind.operation.is_some() => {
                    binders.push(("answer".into(), answer.clone()));
                    let mut event_args = vec!["position".to_string()];
                    event_args.extend(call_args.clone());
                    event_args.push("answer".into());
                    (
                        format!("{u}Input.Answer{trace_name}(answer)"),
                        if answer == "Unit" {
                            "state"
                        } else {
                            "state, answer"
                        },
                        "position + 1",
                        format!(
                            "List.concat(events, [{u}Event.Observed{trace_name}({})])",
                            event_args.join(", ")
                        ),
                    )
                }
                _ => (
                    format!("{u}Input.Advance"),
                    "state",
                    "position",
                    "events".to_string(),
                ),
            };
            let mut fields = call_args;
            fields.push("state".into());
            binders.extend([
                ("rest".into(), format!("List<{u}Input>")),
                ("position".into(), "Int".into()),
                ("events".into(), format!("List<{u}Event>")),
                ("consumed".into(), "Int".into()),
            ]);
            let claim = format!(
                "{drive}({outcome}.Waiting({request}.{name}({fields})), List.prepend({token}, rest), position, events, consumed) == {step}({observer}({observed_args}, rest, {position}, {events}, consumed + 1))",
                outcome = self.protocol.outcome,
                request = self.protocol.request,
                name = kind.name,
                fields = fields.join(", "),
            );
            out.push_str(&composition::law(
                self,
                &drive,
                &label,
                &binders,
                &claim,
                &[],
            )?);
        }
        Ok(out)
    }

    /// Public sample functions this module offers for a segment observer's
    /// parameters, one entry per parameter in order, empty where the parameter
    /// needs none or this module cannot build one.
    ///
    /// Only a nominal type earns a sample: everything else is written out by
    /// any module that needs it. A protocol state, by contrast, is built from
    /// constructors that stay inside the module that declared them, so the
    /// owner is the only place a value of one can be written.
    pub(super) fn segment_sample_names(&self, fd: &'a FnDef) -> Vec<String> {
        let observer = self.source_name(fd);
        fd.params
            .iter()
            .enumerate()
            .map(|(index, (_, ty))| {
                let nominal = matches!(
                    crate::types::parse_type_str_strict(ty),
                    Ok(Type::Named { .. })
                );
                if nominal && composition::samples::witness(self, ty).is_ok() {
                    format!("{observer}Sample{index}")
                } else {
                    String::new()
                }
            })
            .collect()
    }

    /// The sample functions themselves. Each one is a constant: a value of the
    /// parameter's type, written with the constructors visible here.
    pub(super) fn segment_samples(&self) -> String {
        let mut out = String::new();
        for fd in self.observed_segments() {
            for ((_, ty), name) in fd.params.iter().zip(self.segment_sample_names(fd)) {
                let Ok(value) = composition::samples::witness(self, ty) else {
                    continue;
                };
                if name.is_empty() {
                    continue;
                }
                out.push_str(&format!("\nfn {name}() -> {ty}\n    {value}\n"));
            }
        }
        out
    }

    pub(super) fn imported_segment(
        &self,
        name: &str,
    ) -> Option<(&'a ProcessProtocol, &'a ProcessTraceSegment)> {
        self.imported.values().find_map(|protocol| {
            protocol
                .trace
                .as_ref()?
                .segments
                .iter()
                .find_map(|segment| (segment.function == name).then_some((protocol, segment)))
        })
    }

    pub(super) fn segment_signature(
        &self,
        protocol: &ProcessProtocol,
        segment: &ProcessTraceSegment,
    ) -> FnDef {
        let suffix: String = segment
            .function
            .bytes()
            .map(|byte| format!("{byte:02x}"))
            .collect();
        build::fn_def(
            format!("segment{suffix}"),
            segment.params.clone(),
            protocol.outcome.clone(),
            None,
            vec![],
            build::ident("Unit", 0),
            0,
        )
    }

    pub(super) fn segment_adapters(&self, protocol: &ProcessProtocol) -> String {
        let trace = protocol.trace.as_ref().expect("supported import");
        let adapter = self.source_name(&self.import_signature(protocol));
        let mut out = String::new();
        for segment in &trace.segments {
            let signature = self.segment_signature(protocol, segment);
            let name = self.source_name(&signature);
            let result = self.result_type(&signature);
            let params: Vec<_> = segment
                .params
                .iter()
                .enumerate()
                .map(|(i, (_, ty))| (format!("arg{i}"), ty.clone()))
                .chain([
                    ("inputs".into(), format!("List<{}Input>", self.upper)),
                    ("position".into(), "Int".into()),
                    ("events".into(), format!("List<{}Event>", self.upper)),
                    ("consumed".into(), "Int".into()),
                ])
                .collect();
            let args: Vec<_> = (0..segment.params.len())
                .map(|i| format!("arg{i}"))
                .chain([
                    format!("{adapter}Inputs(inputs)"),
                    "position".into(),
                    "emptyEvents".into(),
                    "consumed".into(),
                ])
                .collect();
            out.push_str(&format!(
                "\nfn {name}({params}) -> {result}\n    emptyEvents: List<{event}> = []\n    observed = {observer}({args})\n    {result}(remaining = List.drop(inputs, observed.consumed - consumed), position = observed.position, consumed = observed.consumed, events = List.concat(events, {adapter}Events(observed.events)), value = observed.value, pending = {adapter}Pending(observed.pending), valid = observed.valid)\n",
                params = composition::declarations(&params),
                event = trace.event,
                observer = segment.observer,
                args = args.join(", "),
            ));
        }
        out
    }
}

/// Whether the compiled function named `caller` calls `callee`. An observer
/// that reads a lifted imported observation states its own cursor through that
/// observation's cursor, and this is how the citation is found.
fn reads(items: &[TopLevel], caller: &str, callee: &str) -> bool {
    let Some(fd) = items.iter().find_map(|item| match item {
        TopLevel::FnDef(fd) if fd.name == caller => Some(fd),
        _ => None,
    }) else {
        return false;
    };
    let mut found = false;
    for stmt in fd.body.stmts() {
        let (Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) = stmt;
        crate::codegen::expr_walk::walk(expr, &mut |expr| {
            if let Expr::FnCall(target, _) = &expr.node
                && build::dotted_name(target).as_deref() == Some(callee)
            {
                found = true;
            }
        });
    }
    found
}
