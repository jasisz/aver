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
                samples: self.segment_sample_names(fd),
            })
            .collect()
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
