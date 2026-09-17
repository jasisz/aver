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

    pub(super) fn segment_exports(&self) -> Vec<ProcessTraceSegment> {
        self.observed_segments()
            .into_iter()
            .map(|fd| ProcessTraceSegment {
                function: fd.name.clone(),
                observer: self.source_name(fd),
                result: self.result_type(fd),
                params: fd.params.clone(),
            })
            .collect()
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
                    "[]".into(),
                    "consumed".into(),
                ])
                .collect();
            out.push_str(&format!(
                "\nfn {name}({params}) -> {result}\n    observed = {observer}({args})\n    {result}(remaining = List.drop(inputs, observed.consumed - consumed), position = observed.position, consumed = observed.consumed, events = List.concat(events, {adapter}Events(observed.events)), value = observed.value, pending = {adapter}Pending(observed.pending), valid = observed.valid)\n",
                params = composition::declarations(&params),
                observer = segment.observer,
                args = args.join(", "),
            ));
        }
        out
    }
}
