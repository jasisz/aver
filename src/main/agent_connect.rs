//! `aver agent-connect` — hand the bundled Aver agent material to whatever
//! agent setup the reader already uses.
//!
//! The language guide (`.claude/commands/aver.md`), the toolchain guide
//! (`.claude/commands/aver-tooling.md`), and the curated public cut
//! (`tools/website/llms.txt`) are `include_str!`-ed into the binary and
//! listed in the crate's `include`, so `cargo install aver-lang` carries
//! them. This command copies them out:
//!
//! - default: two skills under `.claude/skills/` in the current directory,
//!   plus a marked pointer section in `AGENTS.md`
//! - `--global`: the same two skills under `~/.claude/skills/`
//! - `--print`: the curated language guide on stdout, nothing written
//!
//! Every file this command owns carries [`MANAGED_MARKER`]. A file without
//! it belongs to somebody else and is refused, never overwritten. `AGENTS.md`
//! is edited only between [`SECTION_START`] and [`SECTION_END`]; every byte
//! outside those markers is preserved. A marker counts only when it is alone
//! on its own line, and only one pair may be present — a marker named inside
//! a sentence is prose, and a second pair is ambiguous, so both are left
//! alone rather than rewritten on a guess.

use std::fmt;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// Language guide — the source `tools/website/build_llms.sh` turns into `llms.txt`.
const LANGUAGE_GUIDE: &str = include_str!("../../.claude/commands/aver.md");

/// Toolchain guide — the source `tools/website/build_llms.sh` turns into `docs/cli.md`.
const TOOLING_GUIDE: &str = include_str!("../../.claude/commands/aver-tooling.md");

/// The curated public language guide, emitted verbatim by `--print`.
const LLMS_TXT: &str = include_str!("../../tools/website/llms.txt");

/// Fingerprint written into every file this command creates. Its absence in
/// an existing file is what makes the command refuse instead of clobber.
pub(super) const MANAGED_MARKER: &str =
    "<!-- aver agent-connect: managed file. Re-run `aver agent-connect` to refresh. -->";

/// Opening marker of the `AGENTS.md` section this command owns.
pub(super) const SECTION_START: &str = "<!-- aver agent-connect: start -->";

/// Closing marker of the `AGENTS.md` section this command owns.
pub(super) const SECTION_END: &str = "<!-- aver agent-connect: end -->";

/// One skill directory under `.claude/skills/`.
struct Skill {
    /// Directory name and the `name:` frontmatter field.
    name: &'static str,
    /// One-line `description:` frontmatter field — what the skill is for.
    description: &'static str,
    /// The bundled guide, copied verbatim under the frontmatter.
    body: &'static str,
}

const SKILLS: &[Skill] = &[
    Skill {
        name: "aver",
        description: "Write or review Aver source (.av): current syntax for functions, types, \
                      match, effects, modules, verify blocks, and decision blocks.",
        body: LANGUAGE_GUIDE,
    },
    Skill {
        name: "aver-tooling",
        description: "Drive the aver toolchain: run, check, verify, context, shape, compile, \
                      proof, replay, and aver.toml policy.",
        body: TOOLING_GUIDE,
    },
];

/// What happened to one file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Outcome {
    Created,
    Updated,
    Unchanged,
}

impl Outcome {
    fn verb(self) -> &'static str {
        match self {
            Self::Created => "created",
            Self::Updated => "updated",
            Self::Unchanged => "unchanged",
        }
    }
}

/// One line of the summary.
#[derive(Debug, Clone)]
pub(super) struct Change {
    pub(super) path: PathBuf,
    pub(super) outcome: Outcome,
}

#[derive(Debug)]
pub(super) enum ConnectError {
    /// A target file exists but was not written by this command.
    ForeignFile(PathBuf),
    /// `AGENTS.md` marker lines do not describe one rewritable section.
    MalformedSection {
        path: PathBuf,
        problem: MalformedSection,
    },
    /// `--global` with no home directory in the environment.
    NoHome,
    Io {
        path: PathBuf,
        source: io::Error,
    },
}

impl fmt::Display for ConnectError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ForeignFile(path) => write!(
                formatter,
                "refusing to overwrite '{}': it exists and was not written by `aver agent-connect` \
                 (no `aver agent-connect: managed file` marker). Move or delete it, then re-run.",
                path.display()
            ),
            Self::MalformedSection { path, problem } => write!(
                formatter,
                "refusing to edit '{}': it has {}, so the section this command owns cannot be \
                 identified. Repair the markers, then re-run.",
                path.display(),
                problem.reason()
            ),
            Self::NoHome => write!(
                formatter,
                "`aver agent-connect --global` needs a home directory, but neither HOME nor \
                 USERPROFILE is set."
            ),
            Self::Io { path, source } => {
                write!(formatter, "could not write '{}': {source}", path.display())
            }
        }
    }
}

/// Where one run writes.
pub(super) struct Destination {
    /// Directory that holds one subdirectory per skill.
    skills_root: PathBuf,
    /// `AGENTS.md` to point at the skills, absent in `--global` mode.
    agents_file: Option<PathBuf>,
    /// Prefix stripped from summary lines, so a project run reads
    /// `.claude/skills/...` rather than an absolute path.
    display_base: Option<PathBuf>,
    /// Closing usage line naming what was attached.
    scope: &'static str,
}

impl Destination {
    /// Attach to `root`: skills plus the `AGENTS.md` pointer.
    pub(super) fn project(root: &Path) -> Self {
        Self {
            skills_root: root.join(".claude").join("skills"),
            agents_file: Some(root.join("AGENTS.md")),
            display_base: Some(root.to_path_buf()),
            scope: "this project",
        }
    }

    /// Attach to `home`: skills only, no project file is touched.
    pub(super) fn global(home: &Path) -> Self {
        Self {
            skills_root: home.join(".claude").join("skills"),
            agents_file: None,
            display_base: None,
            scope: "your agent setup",
        }
    }

    fn display(&self, path: &Path) -> String {
        match &self.display_base {
            Some(base) => path
                .strip_prefix(base)
                .unwrap_or(path)
                .display()
                .to_string(),
            None => path.display().to_string(),
        }
    }
}

/// Entry point for the CLI dispatcher. Exits nonzero on refusal or IO error.
pub(super) fn cmd_agent_connect(global: bool, print: bool) {
    if print {
        print!("{LLMS_TXT}");
        return;
    }

    let destination = match resolve_destination(global) {
        Ok(destination) => destination,
        Err(error) => fail(&error),
    };

    match connect(&destination) {
        Ok(changes) => {
            for change in &changes {
                println!(
                    "{} {}",
                    change.outcome.verb(),
                    destination.display(&change.path)
                );
            }
            println!(
                "Aver guides attached to {}. Next: `aver --help`, or `aver context <entry.av> \
                 --budget 10kb` to read a program before opening its files.",
                destination.scope
            );
        }
        Err(error) => fail(&error),
    }
}

fn fail(error: &ConnectError) -> ! {
    eprintln!("error: {error}");
    std::process::exit(1)
}

fn resolve_destination(global: bool) -> Result<Destination, ConnectError> {
    if global {
        let home = std::env::var_os("HOME")
            .or_else(|| std::env::var_os("USERPROFILE"))
            .filter(|value| !value.is_empty())
            .ok_or(ConnectError::NoHome)?;
        Ok(Destination::global(Path::new(&home)))
    } else {
        let cwd = std::env::current_dir().map_err(|source| ConnectError::Io {
            path: PathBuf::from("."),
            source,
        })?;
        Ok(Destination::project(&cwd))
    }
}

/// Write the skills and, for a project run, the `AGENTS.md` section.
pub(super) fn connect(destination: &Destination) -> Result<Vec<Change>, ConnectError> {
    let mut changes = Vec::new();
    for skill in SKILLS {
        let path = destination.skills_root.join(skill.name).join("SKILL.md");
        let outcome = write_managed(&path, &render_skill(skill))?;
        changes.push(Change { path, outcome });
    }

    if let Some(agents) = &destination.agents_file {
        let outcome = write_agents_section(agents)?;
        changes.push(Change {
            path: agents.clone(),
            outcome,
        });
    }

    Ok(changes)
}

/// Frontmatter + fingerprint + the bundled guide, verbatim.
fn render_skill(skill: &Skill) -> String {
    format!(
        "---\nname: {}\ndescription: {}\n---\n\n{}\n\n{}\n",
        skill.name,
        yaml_double_quoted(skill.description),
        MANAGED_MARKER,
        skill.body.trim_end()
    )
}

/// One YAML double-quoted scalar.
///
/// A description is a sentence, and a sentence may contain a colon followed by
/// a space — which is exactly what YAML reads as the end of a key in a plain
/// (unquoted) scalar. An unquoted description therefore does not merely look
/// wrong, it makes the whole frontmatter block unparseable, and a skill whose
/// frontmatter does not parse never loads. Quoting settles every plain-scalar
/// rule at once, so the descriptions can be written as ordinary prose.
fn yaml_double_quoted(value: &str) -> String {
    let mut quoted = String::with_capacity(value.len() + 2);
    quoted.push('"');
    for character in value.chars() {
        match character {
            '"' => quoted.push_str("\\\""),
            '\\' => quoted.push_str("\\\\"),
            '\n' => quoted.push_str("\\n"),
            '\t' => quoted.push_str("\\t"),
            '\r' => quoted.push_str("\\r"),
            _ => quoted.push(character),
        }
    }
    quoted.push('"');
    quoted
}

/// Create the file, refresh it if we wrote it before, or refuse it if we did not.
fn write_managed(path: &Path, contents: &str) -> Result<Outcome, ConnectError> {
    let existing = match fs::read_to_string(path) {
        Ok(text) => Some(text),
        Err(error) if error.kind() == io::ErrorKind::NotFound => None,
        Err(source) => {
            return Err(ConnectError::Io {
                path: path.to_path_buf(),
                source,
            });
        }
    };

    match existing {
        Some(text) if !text.contains(MANAGED_MARKER) => {
            Err(ConnectError::ForeignFile(path.to_path_buf()))
        }
        Some(text) if text == contents => Ok(Outcome::Unchanged),
        Some(_) => {
            write_file(path, contents)?;
            Ok(Outcome::Updated)
        }
        None => {
            write_file(path, contents)?;
            Ok(Outcome::Created)
        }
    }
}

fn write_file(path: &Path, contents: &str) -> Result<(), ConnectError> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|source| ConnectError::Io {
            path: parent.to_path_buf(),
            source,
        })?;
    }
    fs::write(path, contents).map_err(|source| ConnectError::Io {
        path: path.to_path_buf(),
        source,
    })
}

fn write_agents_section(path: &Path) -> Result<Outcome, ConnectError> {
    let existing = match fs::read_to_string(path) {
        Ok(text) => Some(text),
        Err(error) if error.kind() == io::ErrorKind::NotFound => None,
        Err(source) => {
            return Err(ConnectError::Io {
                path: path.to_path_buf(),
                source,
            });
        }
    };

    let updated = upsert_section(existing.as_deref(), &agents_section()).map_err(|problem| {
        ConnectError::MalformedSection {
            path: path.to_path_buf(),
            problem,
        }
    })?;

    match existing {
        Some(text) if text == updated => Ok(Outcome::Unchanged),
        Some(_) => {
            write_file(path, &updated)?;
            Ok(Outcome::Updated)
        }
        None => {
            write_file(path, &updated)?;
            Ok(Outcome::Created)
        }
    }
}

/// Why the markers in a file do not describe one rewritable section.
///
/// Every variant means the same thing operationally: the span this command
/// owns cannot be identified, so it rewrites nothing and says so.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum MalformedSection {
    /// One marker line of the pair is present, its partner is not.
    Unpaired,
    /// The end marker line comes before the start marker line.
    Inverted,
    /// More than one marker line of the same kind, so which pair bounds the
    /// managed section would be a guess.
    Repeated,
}

impl MalformedSection {
    /// The middle of the refusal message — what the file actually has.
    fn reason(self) -> &'static str {
        match self {
            Self::Unpaired => "one `aver agent-connect` marker line without its partner",
            Self::Inverted => "an `aver agent-connect: end` marker line before its `start`",
            Self::Repeated => "more than one `aver agent-connect` marker line of the same kind",
        }
    }
}

/// The pointer block written between the markers.
pub(super) fn agents_section() -> String {
    format!(
        "{SECTION_START}\n\
         ## Aver\n\
         \n\
         This project is written in [Aver](https://github.com/jasisz/aver): a statically typed \
         language for code that is cheap to generate and has to be cheap to trust. Source files \
         are `.av`.\n\
         \n\
         Two skills carry the material, installed by `aver agent-connect`:\n\
         \n\
         - `.claude/skills/aver/SKILL.md` — the language: syntax, types, `match`, classified \
         effects, modules, `verify` blocks, `decision` blocks.\n\
         - `.claude/skills/aver-tooling/SKILL.md` — the toolchain: `run`, `check`, `verify`, \
         `context`, `shape`, `compile`, `proof`, `replay`, and `aver.toml` policy.\n\
         \n\
         The two tools worth reaching for first are `aver --help` for the command surface and \
         `aver context <entry.av> --budget 10kb` to read a program before opening its files. \
         `aver agent-connect --print` writes the same language guide to stdout for an agent that \
         prefers one file.\n\
         \n\
         This section is maintained by `aver agent-connect`; edits inside the markers are \
         overwritten on the next run.\n\
         {SECTION_END}"
    )
}

/// Byte spans of the lines whose entire content is `marker`.
///
/// A marker delimits the managed section only when it stands alone on its own
/// line. A marker named inside a sentence is prose the reader wrote — the
/// guide this very command installs contains such a sentence — and prose is
/// not a boundary. Without that rule, the first run on a file that merely
/// mentions both marker names would splice the pointer block over the
/// sentence and delete everything between the two names.
///
/// The returned span covers the line's content, not its terminator, so
/// splicing at it leaves the newline that ended the line in place.
fn marker_lines(text: &str, marker: &str) -> Vec<(usize, usize)> {
    let mut spans = Vec::new();
    let mut offset = 0;
    for line in text.split_inclusive('\n') {
        let content = line.trim_end_matches('\n').trim_end_matches('\r');
        if content.trim() == marker {
            spans.push((offset, offset + content.len()));
        }
        offset += line.len();
    }
    spans
}

/// Append `block` to `text`, separated by one blank line.
fn append_block(text: &str, block: &str) -> String {
    if text.is_empty() {
        return format!("{block}\n");
    }
    let mut out = String::with_capacity(text.len() + block.len() + 2);
    out.push_str(text);
    if !out.ends_with('\n') {
        out.push('\n');
    }
    out.push('\n');
    out.push_str(block);
    out.push('\n');
    out
}

/// Replace the marked section, or append it, leaving every other byte alone.
///
/// Exactly one start marker line and one end marker line after it is the only
/// shape that gets rewritten. Anything else — a half pair, an inverted pair, a
/// second pair from a merge — is refused, because picking a span among them
/// would mean deleting bytes on a guess.
pub(super) fn upsert_section(
    existing: Option<&str>,
    block: &str,
) -> Result<String, MalformedSection> {
    let Some(text) = existing else {
        return Ok(format!("{block}\n"));
    };

    let starts = marker_lines(text, SECTION_START);
    let ends = marker_lines(text, SECTION_END);

    match (starts.len(), ends.len()) {
        (0, 0) => Ok(append_block(text, block)),
        (1, 1) => {
            let (section_start, _) = starts[0];
            let (_, section_end) = ends[0];
            if section_end <= section_start {
                return Err(MalformedSection::Inverted);
            }
            Ok(format!(
                "{}{}{}",
                &text[..section_start],
                block,
                &text[section_end..]
            ))
        }
        (0, _) | (_, 0) => Err(MalformedSection::Unpaired),
        _ => Err(MalformedSection::Repeated),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn block() -> String {
        format!("{SECTION_START}\nPOINTER\n{SECTION_END}")
    }

    #[test]
    fn a_missing_agents_file_becomes_the_block_alone() {
        let out = upsert_section(None, &block()).expect("fresh file");

        assert_eq!(out, format!("{}\n", block()));
    }

    #[test]
    fn an_empty_agents_file_becomes_the_block_alone() {
        let out = upsert_section(Some(""), &block()).expect("empty file");

        assert_eq!(out, format!("{}\n", block()));
    }

    #[test]
    fn an_existing_agents_file_keeps_its_bytes_and_gains_the_block() {
        let original = "# House rules\n\nNever run the release script by hand.\n";

        let out = upsert_section(Some(original), &block()).expect("append");

        assert!(
            out.starts_with(original),
            "the original bytes must survive verbatim, got:\n{out}"
        );
        assert_eq!(out, format!("{original}\n{}\n", block()));
    }

    #[test]
    fn a_file_without_a_trailing_newline_still_gets_a_separated_block() {
        let out = upsert_section(Some("no newline here"), &block()).expect("append");

        assert_eq!(out, format!("no newline here\n\n{}\n", block()));
    }

    #[test]
    fn only_the_marked_block_is_replaced() {
        let before = "# House rules\n\n";
        let after = "\n\n## Afterwards\n\nStill here.\n";
        let stale = format!("{before}{SECTION_START}\nSTALE POINTER\n{SECTION_END}{after}");

        let out = upsert_section(Some(&stale), &block()).expect("replace");

        assert_eq!(out, format!("{before}{}{after}", block()));
        assert!(!out.contains("STALE POINTER"));
    }

    #[test]
    fn a_second_run_over_an_appended_block_is_a_no_op() {
        let original = "# House rules\n\nNever run the release script by hand.\n";

        let first = upsert_section(Some(original), &block()).expect("append");
        let second = upsert_section(Some(&first), &block()).expect("replace");

        assert_eq!(first, second);
    }

    #[test]
    fn a_start_marker_without_an_end_marker_is_refused() {
        let broken = format!("# House rules\n\n{SECTION_START}\nhalf a section\n");

        assert_eq!(
            upsert_section(Some(&broken), &block()),
            Err(MalformedSection::Unpaired)
        );
    }

    #[test]
    fn an_end_marker_without_a_start_marker_is_refused() {
        let broken = format!("# House rules\n\n{SECTION_END}\n");

        assert_eq!(
            upsert_section(Some(&broken), &block()),
            Err(MalformedSection::Unpaired)
        );
    }

    #[test]
    fn an_end_marker_before_its_start_is_refused() {
        let inverted = format!("{SECTION_END}\n\nupside down\n\n{SECTION_START}\n");

        assert_eq!(
            upsert_section(Some(&inverted), &block()),
            Err(MalformedSection::Inverted)
        );
    }

    #[test]
    fn a_second_pair_of_markers_is_refused_rather_than_guessed_at() {
        // A merge can leave two managed blocks. Splicing from the first start
        // to the last end would delete everything between them.
        let doubled = format!(
            "{SECTION_START}\nfirst\n{SECTION_END}\n\nMINE, KEEP ME\n\n{SECTION_START}\nsecond\n{SECTION_END}\n"
        );

        assert_eq!(
            upsert_section(Some(&doubled), &block()),
            Err(MalformedSection::Repeated)
        );
    }

    #[test]
    fn a_sentence_naming_both_markers_is_prose_not_a_section() {
        // This is the sentence the installed guide itself carries. Before the
        // markers were line-anchored, the first run deleted its middle.
        let original = "# House rules\n\nAver keeps its pointer in `AGENTS.md` between \
                        `<!-- aver agent-connect: start -->` and `<!-- aver agent-connect: end -->`.\n";

        let out = upsert_section(Some(original), &block()).expect("append, not replace");

        assert!(
            out.starts_with(original),
            "the sentence must survive verbatim, got:\n{out}"
        );
        assert_eq!(out, format!("{original}\n{}\n", block()));
    }

    #[test]
    fn a_later_inline_mention_does_not_extend_the_replaced_span() {
        let before = "# House rules\n\n";
        let after = "\n\nSee `<!-- aver agent-connect: end -->` for where my section stops.\n";
        let stale = format!("{before}{SECTION_START}\nSTALE POINTER\n{SECTION_END}{after}");

        let out = upsert_section(Some(&stale), &block()).expect("replace the block only");

        assert_eq!(out, format!("{before}{}{after}", block()));
    }

    #[test]
    fn an_indented_marker_line_still_bounds_the_section() {
        let stale = format!("  {SECTION_START}\nSTALE\n  {SECTION_END}\n");

        let out = upsert_section(Some(&stale), &block()).expect("replace");

        assert_eq!(out, format!("{}\n", block()));
    }

    #[test]
    fn the_real_pointer_block_survives_a_round_trip() {
        let original = "# Repo notes\n\nkeep me\n";

        let first = upsert_section(Some(original), &agents_section()).expect("append");
        let second = upsert_section(Some(&first), &agents_section()).expect("replace");

        assert_eq!(first, second);
        assert!(first.starts_with(original));
        assert!(first.contains(".claude/skills/aver/SKILL.md"));
    }

    #[test]
    fn every_rendered_skill_carries_frontmatter_the_marker_and_the_guide() {
        for skill in SKILLS {
            let rendered = render_skill(skill);

            assert!(
                rendered.starts_with(&format!("---\nname: {}\ndescription: ", skill.name)),
                "{} must open with skill frontmatter",
                skill.name
            );
            assert!(rendered.contains(MANAGED_MARKER), "{}", skill.name);
            assert!(
                rendered.contains(skill.body.trim_end()),
                "{} must carry the bundled guide verbatim",
                skill.name
            );
        }
    }

    #[test]
    fn every_description_is_written_as_a_quoted_yaml_scalar() {
        // Both descriptions are sentences containing ": ", which YAML reads as
        // a key/value split inside a plain scalar. Unquoted, the frontmatter
        // does not parse and the skill silently never loads.
        for skill in SKILLS {
            let rendered = render_skill(skill);
            let line = rendered
                .lines()
                .nth(2)
                .expect("frontmatter has a description line");

            let value = line.strip_prefix("description: ").unwrap_or_else(|| {
                panic!("{}: expected a description line, got {line}", skill.name)
            });
            assert!(
                value.starts_with('"') && value.ends_with('"') && value.len() >= 2,
                "{} must quote its description, got {value}",
                skill.name
            );
            assert_eq!(
                unquote_yaml(value),
                skill.description,
                "{} must round-trip through the quoting",
                skill.name
            );
        }
    }

    #[test]
    fn quoting_escapes_what_a_yaml_double_quoted_scalar_cannot_carry_raw() {
        assert_eq!(yaml_double_quoted("plain: text"), "\"plain: text\"");
        assert_eq!(
            yaml_double_quoted(r#"a "quote" and a \ backslash"#),
            r#""a \"quote\" and a \\ backslash""#
        );
        assert_eq!(yaml_double_quoted("two\nlines"), "\"two\\nlines\"");
    }

    /// Read back a double-quoted YAML scalar, escapes and all.
    fn unquote_yaml(value: &str) -> String {
        let inner = &value[1..value.len() - 1];
        let mut out = String::with_capacity(inner.len());
        let mut characters = inner.chars();
        while let Some(character) = characters.next() {
            if character != '\\' {
                out.push(character);
                continue;
            }
            match characters.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some(escaped) => out.push(escaped),
                None => panic!("trailing backslash in {value}"),
            }
        }
        out
    }

    #[test]
    fn a_foreign_skill_file_is_refused_and_left_alone() {
        let temp = tempfile::tempdir().expect("temp dir");
        let path = temp.path().join("SKILL.md");
        fs::write(&path, "my own notes\n").expect("seed foreign file");

        let error = write_managed(&path, "replacement").expect_err("must refuse");

        assert!(matches!(error, ConnectError::ForeignFile(_)));
        assert_eq!(
            fs::read_to_string(&path).expect("re-read"),
            "my own notes\n",
            "a refused file must not be touched"
        );
        assert!(error.to_string().contains("refusing to overwrite"));
    }

    #[test]
    fn a_managed_skill_file_is_created_then_refreshed_then_left_unchanged() {
        let temp = tempfile::tempdir().expect("temp dir");
        let path = temp.path().join("nested").join("SKILL.md");
        let first = format!("{MANAGED_MARKER}\nversion one\n");
        let second = format!("{MANAGED_MARKER}\nversion two\n");

        assert_eq!(
            write_managed(&path, &first).expect("create"),
            Outcome::Created
        );
        assert_eq!(
            write_managed(&path, &second).expect("update"),
            Outcome::Updated
        );
        assert_eq!(
            write_managed(&path, &second).expect("no-op"),
            Outcome::Unchanged
        );
        assert_eq!(fs::read_to_string(&path).expect("re-read"), second);
    }

    #[test]
    fn a_project_run_writes_both_skills_and_the_agents_pointer() {
        let temp = tempfile::tempdir().expect("temp dir");
        let destination = Destination::project(temp.path());

        let changes = connect(&destination).expect("first run");

        assert_eq!(changes.len(), 3);
        assert!(changes.iter().all(|c| c.outcome == Outcome::Created));
        assert!(temp.path().join(".claude/skills/aver/SKILL.md").is_file());
        assert!(
            temp.path()
                .join(".claude/skills/aver-tooling/SKILL.md")
                .is_file()
        );

        let again = connect(&destination).expect("second run");
        assert!(
            again.iter().all(|c| c.outcome == Outcome::Unchanged),
            "re-running must produce no diff"
        );
    }

    #[test]
    fn a_global_run_writes_only_the_skills() {
        let temp = tempfile::tempdir().expect("temp dir");
        let destination = Destination::global(temp.path());

        let changes = connect(&destination).expect("global run");

        assert_eq!(changes.len(), 2);
        assert!(!temp.path().join("AGENTS.md").exists());
        assert!(
            temp.path()
                .join(".claude/skills/aver-tooling/SKILL.md")
                .is_file()
        );
    }

    #[test]
    fn the_printed_guide_is_the_curated_language_guide() {
        assert!(LLMS_TXT.starts_with("# Aver\n"));
        assert!(LLMS_TXT.contains("### Decision blocks"));
    }
}
