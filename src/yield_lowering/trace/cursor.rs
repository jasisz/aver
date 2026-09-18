//! Cursor contracts for exporting observations across a module boundary.
//! The ordinary Aver law quantifies over every outcome, tape and cursor; its
//! proof uses induction on the checked protocol observer, including Waiting.
use super::*;

impl Model<'_> {
    /// `cursors` are the segment cursor laws checked before this contract:
    /// the observer drives through those observations, and its own bound is
    /// read from theirs instead of from their bodies.
    pub(super) fn cursor_contract(
        &self,
        items: &mut Vec<TopLevel>,
        root: &FnDef,
        cursors: &[String],
    ) -> Result<(), String> {
        let drive_name = self.child_drive(root);
        let drive = items
            .iter()
            .find_map(|item| match item {
                TopLevel::FnDef(fd) if fd.name == drive_name => Some(fd),
                _ => None,
            })
            .ok_or("cursor contract has no protocol observer")?;
        let valid = format!("{}CursorValid", self.prefix);
        let cursor = format!("{}Cursor", self.prefix);
        let u = &self.upper;
        let result = self.result_type(root);
        let params = drive
            .params
            .iter()
            .map(|(n, t)| format!("{n}: {t}"))
            .collect::<Vec<_>>()
            .join(", ");
        let args = drive
            .params
            .iter()
            .map(|(n, _)| n.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        let text = format!(
            r#"
fn {valid}(inputs: List<{u}Input>, consumed: Int, observed: {result}) -> Bool
    used = observed.consumed - consumed
    Bool.and(used >= 0, Bool.and(used <= List.len(inputs), observed.remaining == List.drop(inputs, used)))

fn {cursor}({params}) -> Bool
    {valid}(inputs, consumed, {drive_name}({args}))

verify {cursor} law boundedSuffix
    given outcome: {outcome} = [{outcome}.Done({sample})]
    given inputs: List<{u}Input> = [[]]
    given position: Int = [0]
    given events: List<{u}Event> = [[]]
    given consumed: Int = [0]
    using [{using}]
    {cursor}({args}) holds
"#,
            using = cursors.join(", "),
            outcome = self.protocol.outcome,
            sample = composition::samples::witness(self, &root.return_type)?
        );
        let tokens = crate::lexer::Lexer::new(&text)
            .tokenize()
            .map_err(|e| e.to_string())?;
        items.extend(
            crate::parser::Parser::new_compiler_generated(tokens)
                .parse()
                .map_err(|e| e.to_string())?,
        );
        Ok(())
    }
}
