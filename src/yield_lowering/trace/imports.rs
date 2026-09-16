//! Adapters between two monomorphic observers. Unknown input kinds become a
//! rejected token, preserving length; returning uses the consumed prefix length
//! to recover the caller's original unconsumed tokens without losing types.
use super::*;
use std::fmt::Write;

impl Model<'_> {
    pub(super) fn import_signature(&self, protocol: &ProcessProtocol) -> FnDef {
        let suffix: String = protocol
            .fn_name
            .bytes()
            .map(|b| format!("{b:02x}"))
            .collect();
        build::fn_def(
            format!("import{suffix}"),
            protocol.params.clone(),
            protocol.return_type.clone(),
            None,
            vec![],
            build::ident("Unit", 0),
            0,
        )
    }

    pub(super) fn adapter(&self, protocol: &ProcessProtocol) -> Result<String, String> {
        let trace = protocol.trace.as_ref().ok_or("import has no observer")?;
        let signature = self.import_signature(protocol);
        let name = self.source_name(&signature);
        let result = self.result_type(&signature);
        let u = &self.upper;
        let convert_input = format!("{name}Input");
        let convert_inputs = format!("{name}Inputs");
        let convert_query = format!("{name}Query");
        let convert_pending = format!("{name}Pending");
        let convert_event = format!("{name}Event");
        let convert_events = format!("{name}Events");
        let mut out = format!(
            "\nfn {convert_input}(input: {u}Input) -> {}\n    match input\n        {u}Input.Advance -> {}.Advance\n        {u}Input.Foreign -> {}.Foreign\n",
            trace.input, trace.input, trace.input
        );
        for kind in &self.kinds {
            let Some(_) = &kind.operation else { continue };
            let value = match trace
                .operations
                .iter()
                .find(|other| other.operation == kind.operation)
            {
                Some(other) => format!("{}.Answer{}(answer)", trace.input, other.name),
                None => format!("{}.Foreign", trace.input),
            };
            writeln!(
                out,
                "        {u}Input.Answer{}(answer) -> {value}",
                kind.name
            )
            .unwrap();
        }
        out.push_str(&format!("\nfn {convert_inputs}(inputs: List<{u}Input>) -> List<{}>\n    match inputs\n        [] -> []\n        [input, ..rest] -> List.prepend({convert_input}(input), {convert_inputs}(rest))\n", trace.input));
        out.push_str(&format!("\nfn {convert_query}(query: {}) -> {u}Query\n    match query\n        {}.Yield -> {u}Query.Yield\n", trace.query, trace.query));
        for kind in trace.operations.iter().filter(|k| k.operation.is_some()) {
            let parent = self
                .operations
                .get(kind.operation.as_ref().unwrap())
                .ok_or("helper operation missing from parent protocol")?;
            let args: Vec<_> = (0..kind.arg_types.len())
                .map(|i| format!("arg{i}"))
                .collect();
            let payload = if args.is_empty() {
                String::new()
            } else {
                format!("({})", args.join(", "))
            };
            writeln!(
                out,
                "        {}.{}{payload} -> {u}Query.{}{payload}",
                trace.query, kind.name, parent.name
            )
            .unwrap();
        }
        out.push_str(&format!("\nfn {convert_event}(event: {}) -> {u}Event\n    match event\n        {}.Empty -> {u}Event.Empty\n", trace.event, trace.event));
        for kind in trace.operations.iter().filter(|k| k.operation.is_some()) {
            let parent = self
                .operations
                .get(kind.operation.as_ref().unwrap())
                .ok_or("helper operation missing from parent protocol")?;
            let args: Vec<_> = ["position".into()]
                .into_iter()
                .chain((0..kind.arg_types.len()).map(|i| format!("arg{i}")))
                .chain(["answer".into()])
                .collect();
            writeln!(
                out,
                "        {}.Observed{}({}) -> {u}Event.Observed{}({})",
                trace.event,
                kind.name,
                args.join(", "),
                parent.name,
                args.join(", ")
            )
            .unwrap();
        }
        out.push_str(&format!("\nfn {convert_events}(events: List<{}>) -> List<{u}Event>\n    match events\n        [] -> []\n        [event, ..rest] -> List.prepend({convert_event}(event), {convert_events}(rest))\n", trace.event));
        out.push_str(&format!("\nfn {convert_pending}(pending: Option<{}>) -> Option<{u}Query>\n    match pending\n        Option.Some(query) -> Option.Some({convert_query}(query))\n        Option.None -> Option.None\n", trace.query));
        let params: Vec<_> = protocol
            .params
            .iter()
            .enumerate()
            .map(|(i, (_, t))| format!("__arg{i}: {t}"))
            .chain([
                format!("inputs: List<{u}Input>"),
                "position: Int".into(),
                format!("events: List<{u}Event>"),
                "consumed: Int".into(),
            ])
            .collect();
        let args: Vec<_> = protocol
            .params
            .iter()
            .enumerate()
            .map(|(i, _)| format!("__arg{i}"))
            .chain([
                format!("{convert_inputs}(inputs)"),
                "position".into(),
                "emptyEvents".into(),
                "consumed".into(),
            ])
            .collect();
        out.push_str(&format!("\nfn {name}Lift(observed: {}, inputs: List<{u}Input>, events: List<{u}Event>, consumed: Int) -> {result}\n    {result}(remaining = List.drop(inputs, observed.consumed - consumed), position = observed.position, consumed = observed.consumed, events = List.concat(events, {convert_events}(observed.events)), value = observed.value, pending = {convert_pending}(observed.pending), valid = observed.valid)\n", trace.result));
        out.push_str(&format!("\nfn {name}({}) -> {result}\n    emptyEvents: List<{}> = []\n    {name}Lift({}({}), inputs, events, consumed)\n", params.join(", "), trace.event, trace.source, args.join(", ")));
        Ok(out)
    }
}
