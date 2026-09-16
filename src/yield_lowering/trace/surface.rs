//! Concrete observation types and an observer of the real protocol entry points.
use super::*;

impl Model<'_> {
    pub(super) fn surface(&self, functions: &[&FnDef]) -> String {
        let u = &self.upper;
        let mut out = format!("type {u}Input\n    Advance\n    Foreign\n");
        for kind in &self.kinds {
            if let Some(ty) = &kind.answer_type {
                out.push_str(&format!("    Answer{}({ty})\n", kind.name));
            }
        }
        out.push_str(&format!("\ntype {u}Query\n    Yield\n"));
        for kind in &self.kinds {
            if kind.operation.is_some() {
                out.push_str(&format!("    {}{}\n", kind.name, payload(&kind.arg_types)));
            }
        }
        out.push_str(&format!("\ntype {u}Event\n"));
        // A marker keeps the sum inhabited even for a process that only yields;
        // neither observer ever emits it.
        out.push_str("    Empty\n");
        for kind in &self.kinds {
            if let Some(answer) = &kind.answer_type {
                let mut fields = vec!["Int".to_string()];
                fields.extend(kind.arg_types.clone());
                fields.push(answer.clone());
                out.push_str(&format!("    Observed{}{}\n", kind.name, payload(&fields)));
            }
        }
        for fd in functions {
            let result = self.result_type(fd);
            out.push_str(&format!("\nrecord {result}\n    remaining: List<{u}Input>\n    position: Int\n    consumed: Int\n    events: List<{u}Event>\n    value: Option<{}>\n    pending: Option<{u}Query>\n    valid: Bool\n", fd.return_type));
        }
        out
    }

    pub(super) fn drive(&self, protocol: &ProcessProtocol, root: &FnDef, driver: &str) -> String {
        let u = &self.upper;
        let result = self.result_type(root);
        let done = format!(
            "{result}(remaining = inputs, position = position, consumed = consumed, events = events, value = Option.Some(value), pending = Option.None, valid = true)"
        );
        let mut out = format!(
            "\nfn {driver}(outcome: {}, inputs: List<{u}Input>, position: Int, events: List<{u}Event>, consumed: Int) -> {result}\n    match outcome\n        {}.Done(value) -> {done}\n        {}.Waiting(request) -> match request\n",
            protocol.outcome, protocol.outcome, protocol.outcome
        );
        for kind in &protocol.kinds {
            let trace_kind = kind
                .operation
                .as_ref()
                .and_then(|op| self.operations.get(op));
            let trace_name = trace_kind.map_or(kind.name.as_str(), |kind| kind.name.as_str());
            let args: Vec<String> = (0..kind.arg_types.len())
                .map(|n| format!("arg{n}"))
                .collect();
            let mut fields = args.clone();
            fields.push("state".into());
            let query = if kind.operation.is_some() {
                format!("{u}Query.{}{}", trace_name, payload(&args))
            } else {
                format!("{u}Query.Yield")
            };
            let halted = |valid: bool| {
                format!(
                    "{result}(remaining = inputs, position = position, consumed = consumed, events = events, value = Option.None, pending = Option.Some({query}), valid = {valid})"
                )
            };
            let (token, answer_args, position, events) = if kind.operation.is_some() {
                let mut event_args = vec!["position".to_string()];
                event_args.extend(args);
                event_args.push("answer".into());
                let answer_args = if kind.answer_type.as_deref() == Some("Unit") {
                    "state"
                } else {
                    "state, answer"
                };
                (
                    format!("{u}Input.Answer{}(answer)", trace_name),
                    answer_args,
                    "position + 1",
                    format!(
                        "List.concat(events, [{u}Event.Observed{}({})])",
                        trace_name,
                        event_args.join(", ")
                    ),
                )
            } else {
                (
                    format!("{u}Input.Advance"),
                    "state",
                    "position",
                    "events".into(),
                )
            };
            out.push_str(&format!("            {}.{}({}) -> match inputs\n                [] -> {}\n                [input, ..rest] -> match input\n                    {token} -> {driver}({}({answer_args}), rest, {position}, {events}, consumed + 1)\n", protocol.request, kind.name, fields.join(", "), halted(true), kind.answer_fn));
            let accepted = if kind.operation.is_some() {
                format!("Answer{}", trace_name)
            } else {
                "Advance".into()
            };
            for pattern in self.other_inputs(&accepted) {
                if let Pattern::Constructor(name, fields) = pattern {
                    out.push_str(&format!(
                        "                    {name}{} -> {}\n",
                        payload(&fields),
                        halted(false)
                    ));
                }
            }
        }
        out
    }

    pub(super) fn driver(&self, root: &FnDef) -> String {
        let u = &self.upper;
        let p = &self.prefix;
        let result = self.result_type(root);
        let mut out = self.drive(self.protocol, root, &format!("{p}Drive"));
        let params: Vec<_> = root
            .params
            .iter()
            .map(|(name, ty)| format!("{name}: {ty}"))
            .chain([format!("__traceInputs: List<{u}Input>")])
            .collect();
        let args: Vec<_> = root.params.iter().map(|(name, _)| name.clone()).collect();
        let mut source_args = args.clone();
        source_args.extend(["__traceInputs".into(), "0".into(), "[]".into(), "0".into()]);
        out.push_str(&format!("\nfn __{}SourceTrace({}) -> {result}\n    __{}SourceTraceFrom({})\n\nfn __{}ProtocolTrace({}) -> {result}\n    __{}ProtocolTraceFrom({})\n", root.name, params.join(", "), root.name, source_args.join(", "), root.name, params.join(", "), root.name, source_args.join(", ")));
        let mut from_params = params.clone();
        from_params.extend([
            "__tracePosition: Int".into(),
            format!("__traceEvents: List<{u}Event>"),
            "__traceConsumed: Int".into(),
        ]);
        let mut from_args = args.clone();
        from_args.extend([
            "__traceInputs".into(),
            "__tracePosition".into(),
            "__traceEvents".into(),
            "__traceConsumed".into(),
        ]);
        out.push_str(&format!("\nfn __{}SourceTraceFrom({}) -> {result}\n    {}({})\n\nfn __{}ProtocolTraceFrom({}) -> {result}\n    {p}Drive({}({}), __traceInputs, __tracePosition, __traceEvents, __traceConsumed)\n", root.name, from_params.join(", "), self.source_name(root), from_args.join(", "), root.name, from_params.join(", "), self.protocol.start, args.join(", ")));
        out
    }
}

fn payload(fields: &[String]) -> String {
    if fields.is_empty() {
        String::new()
    } else {
        format!("({})", fields.join(", "))
    }
}
