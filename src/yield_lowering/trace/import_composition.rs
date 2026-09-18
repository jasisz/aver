//! Check the owning-module observation adapter before composing a caller.
//! Laws cite the exported correspondence and cursor contract; module metadata
//! only names dependencies and never grants proof credit.
use super::composition::{declarations, law, names};
use super::*;

impl Model<'_> {
    pub(super) fn import_composition(
        &self,
        protocol: &ProcessProtocol,
        fd: &FnDef,
    ) -> Result<String, String> {
        let trace = protocol.trace.as_ref().ok_or("import has no observer")?;
        let cursor_law = trace
            .cursor
            .as_ref()
            .ok_or("import composition needs an owning-module cursor law")?;
        let source_law = trace
            .correspondence
            .as_ref()
            .ok_or("import composition needs an owning-module correspondence law")?;
        let name = self.source_name(fd);
        let drive = self.child_drive(fd);
        let result = self.result_type(fd);
        let u = &self.upper;
        // What the owning module checked about each observation this adapter
        // drives through: the suffix its cursor reports, the prefix of its
        // event history, and the protocol step that produces it. A caller reads
        // these instead of the observation's body. The start observation is not
        // among them — an adapter is entered with an outcome already in hand —
        // and citing it would carry the whole entry point into the cone of
        // every law that cites this one. An observation whose owner checked
        // none of these contributes nothing, and the law falls back to
        // unfolding.
        let interface = trace
            .segments
            .iter()
            .filter(|segment| !segment.cursor.is_empty() && segment.function != protocol.start)
            .flat_map(|segment| {
                let short = segment
                    .function
                    .rsplit_once('.')
                    .map_or(segment.function.as_str(), |(_, short)| short);
                [
                    format!("{}.segmentCursor", segment.cursor),
                    format!("{}Prefixed.eventsPrefix", segment.observer),
                    format!("{}.step{}", trace.drive, build::capitalize(short)),
                ]
            })
            .map(|law| format!(", {law}"))
            .collect::<String>();
        let mut out = self.drive(protocol, fd, &drive);
        out.push_str(&format!(r#"
verify {name}Events law append
    given left: List<{event}> = [[]]
    given right: List<{event}> = [[]]
    using []
    {name}Events(List.concat(left, right)) == List.concat({name}Events(left), {name}Events(right)) holds

verify {name}Events law singleton
    given event: {event} = [{event}.Empty]
    using []
    {name}Events([event]) == [{name}Event(event)] holds

fn {name}Mapped(outcome: {outcome}, inputs: List<{u}Input>, position: Int, events: List<{u}Event>, childEvents: List<{event}>, consumed: Int) -> {result}
    {name}Lift({child_drive}(outcome, {name}Inputs(inputs), position, childEvents, consumed), inputs, events, consumed)

fn {name}Direct(outcome: {outcome}, inputs: List<{u}Input>, position: Int, events: List<{u}Event>, childEvents: List<{event}>, consumed: Int) -> {result}
    {drive}(outcome, inputs, position, List.concat(events, {name}Events(childEvents)), consumed)

verify {name}Direct law mapping
    given outcome: {outcome} = [{outcome}.Done({sample})]
    given inputs: List<{u}Input> = [[]]
    given position: Int = [0]
    given events: List<{u}Event> = [[]]
    given childEvents: List<{event}> = [[]]
    given consumed: Int = [0]
    using [{cursor_law}.boundedSuffix, {name}Events.append, {name}Events.singleton{interface}]
    {name}Direct(outcome, inputs, position, events, childEvents, consumed) == {name}Mapped(outcome, inputs, position, events, childEvents, consumed) holds
"#, outcome=protocol.outcome, event=trace.event, child_drive=trace.drive, sample=composition::samples::witness(self, &fd.return_type)?));
        let params: Vec<_> = fd
            .params
            .iter()
            .enumerate()
            .map(|(i, (_, ty))| (format!("arg{i}"), ty.clone()))
            .chain([
                ("inputs".into(), format!("List<{u}Input>")),
                ("position".into(), "Int".into()),
                ("events".into(), format!("List<{u}Event>")),
                ("consumed".into(), "Int".into()),
            ])
            .collect();
        let entry = format!("{drive}Start");
        let args = names(&params);
        let start_args = names(&params[..fd.params.len()]);
        out.push_str(&format!("\nfn {entry}({}) -> {result}\n    {drive}({}({start_args}), inputs, position, events, consumed)\n", declarations(&params), protocol.start));
        let correspondence = law(
            self,
            &name,
            "correspondence",
            &params,
            &format!("{name}({args}) == {entry}({args})"),
            &[
                source_law.clone(),
                format!("{name}Direct.mapping"),
                format!("{cursor_law}.boundedSuffix"),
            ],
        )?;
        let source_from = source_law
            .rsplit_once('.')
            .ok_or("invalid imported law identity")?
            .0;
        let source_args = if start_args.is_empty() {
            String::new()
        } else {
            format!("{start_args}, ")
        };
        out.push_str(&format!("\nfn {name}SourceAgrees({}) -> Bool\n    emptyEvents: List<{event}> = []\n    {source_from}({source_args}{name}Inputs(inputs), position, emptyEvents, consumed) == {}({source_args}{name}Inputs(inputs), position, emptyEvents, consumed)\n\nfn {name}MappingAgrees({}) -> Bool\n    emptyEvents: List<{event}> = []\n    {entry}({args}) == {name}Lift({}({source_args}{name}Inputs(inputs), position, emptyEvents, consumed), inputs, events, consumed)\n", declarations(&params), trace.protocol_from, declarations(&params), trace.protocol_from, event=trace.event));
        let explanations = format!(
            "    because {name}SourceAgrees({args})\n    because {name}MappingAgrees({args})\n    using"
        );
        out.push_str(&correspondence.replacen("    using", &explanations, 1));
        Ok(out)
    }
}
