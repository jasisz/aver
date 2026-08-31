//! `aver agent-connect` — the installed binary hands its own agent material
//! to whatever setup the reader already uses.
//!
//! Every case here runs the real binary in a throwaway directory. The two
//! claims worth pinning are conservative ones: bytes the command does not own
//! are never rewritten (a foreign `SKILL.md` is refused by name, an existing
//! `AGENTS.md` keeps every byte outside its marked section), and running the
//! command twice leaves the tree exactly as the first run left it.

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn format_output(output: &Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

/// Run `aver agent-connect <args>` with `cwd` as the working directory and
/// `home` as `$HOME`, so nothing escapes into the developer's real setup.
fn agent_connect(cwd: &Path, home: &Path, args: &[&str]) -> Output {
    Command::new(aver_bin())
        .current_dir(cwd)
        .arg("agent-connect")
        .args(args)
        .env("HOME", home)
        .env("USERPROFILE", home)
        .output()
        .expect("expected `aver agent-connect` to spawn")
}

fn stdout_of(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).to_string()
}

const SECTION_START: &str = "<!-- aver agent-connect: start -->";
const SECTION_END: &str = "<!-- aver agent-connect: end -->";
const MANAGED_MARKER: &str = "aver agent-connect: managed file";

/// Read the leading `---` block under YAML's own rules, not under the
/// renderer's. A skill whose frontmatter does not parse is a skill that never
/// loads, which is a silent failure on the reader's machine — so the rules are
/// spelled out here, in the test, rather than borrowed from the code they
/// judge. Panics naming the offending line when the block is not valid YAML.
fn parse_frontmatter(text: &str) -> Vec<(String, String)> {
    let body = text
        .strip_prefix("---\n")
        .unwrap_or_else(|| panic!("frontmatter must open with `---`, got:\n{}", head(text)));
    let block = body
        .split_once("\n---\n")
        .unwrap_or_else(|| panic!("frontmatter must close with `---`, got:\n{}", head(text)))
        .0;

    block.lines().map(parse_frontmatter_line).collect()
}

fn parse_frontmatter_line(line: &str) -> (String, String) {
    let (key, rest) = line
        .split_once(':')
        .unwrap_or_else(|| panic!("frontmatter line is not `key: value`: {line}"));
    assert!(
        !key.is_empty()
            && key
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-'),
        "frontmatter key must be a plain identifier: {line}"
    );
    let value = rest
        .strip_prefix(' ')
        .unwrap_or_else(|| panic!("frontmatter key must be followed by a space: {line}"));

    if let Some(quoted) = value.strip_prefix('"') {
        return (key.to_string(), unescape_double_quoted(quoted, line));
    }

    // A plain (unquoted) scalar. YAML ends one at a colon followed by a space,
    // so a value containing `": "` is not one value but a parse error.
    assert!(
        !value.contains(": ") && !value.ends_with(':'),
        "unquoted value contains a colon, which YAML reads as a second key — quote it: {line}"
    );
    assert!(
        !value.starts_with([
            '-', '?', ':', ',', '[', ']', '{', '}', '#', '&', '*', '!', '|', '>', '\'', '%', '@',
            '`'
        ]),
        "unquoted value starts with a YAML indicator character — quote it: {line}"
    );
    (key.to_string(), value.to_string())
}

fn unescape_double_quoted(quoted: &str, line: &str) -> String {
    let mut out = String::with_capacity(quoted.len());
    let mut characters = quoted.chars();
    loop {
        match characters.next() {
            None => panic!("double-quoted value is never closed: {line}"),
            Some('"') => {
                assert!(
                    characters.next().is_none(),
                    "trailing text after the closing quote: {line}"
                );
                return out;
            }
            Some('\\') => match characters.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some(escaped @ ('"' | '\\' | '/')) => out.push(escaped),
                Some(other) => panic!("unsupported escape `\\{other}`: {line}"),
                None => panic!("trailing backslash: {line}"),
            },
            Some(other) => out.push(other),
        }
    }
}

fn head(text: &str) -> &str {
    &text[..text.len().min(200)]
}

fn assert_loadable_skill(text: &str, name: &str) {
    let fields = parse_frontmatter(text);

    let value = |key: &str| {
        fields
            .iter()
            .find(|(k, _)| k == key)
            .unwrap_or_else(|| panic!("{name}: frontmatter has no `{key}`"))
            .1
            .clone()
    };

    assert_eq!(
        value("name"),
        name,
        "the skill name must match its directory"
    );
    let description = value("description");
    assert!(!description.trim().is_empty(), "{name}: empty description");
    assert!(
        !description.contains('\n'),
        "{name}: the description must be one line"
    );
}

#[test]
fn a_fresh_project_gets_both_skills_and_an_agents_pointer() {
    let project = tempfile::tempdir().expect("project dir");
    let home = tempfile::tempdir().expect("home dir");

    let run = agent_connect(project.path(), home.path(), &[]);
    assert!(run.status.success(), "{}", format_output(&run));

    let language = project.path().join(".claude/skills/aver/SKILL.md");
    let tooling = project.path().join(".claude/skills/aver-tooling/SKILL.md");
    let agents = project.path().join("AGENTS.md");

    let language_text = fs::read_to_string(&language).expect("language skill");
    assert_loadable_skill(&language_text, "aver");
    assert!(language_text.contains(MANAGED_MARKER));
    assert!(
        language_text.contains("### Decision blocks"),
        "the language guide body must be carried into the skill"
    );

    let tooling_text = fs::read_to_string(&tooling).expect("tooling skill");
    assert_loadable_skill(&tooling_text, "aver-tooling");
    assert!(tooling_text.contains(MANAGED_MARKER));
    assert!(tooling_text.contains("aver context"));

    let agents_text = fs::read_to_string(&agents).expect("AGENTS.md");
    assert!(agents_text.starts_with(SECTION_START));
    assert!(agents_text.contains(".claude/skills/aver/SKILL.md"));
    assert!(agents_text.contains(".claude/skills/aver-tooling/SKILL.md"));
    assert!(agents_text.trim_end().ends_with(SECTION_END));

    // Summary: one line per file, then one usage line. Paths are printed
    // relative to the project, not as absolute noise.
    let stdout = stdout_of(&run);
    assert!(
        stdout.contains("created .claude/skills/aver/SKILL.md"),
        "{stdout}"
    );
    assert!(
        stdout.contains("created .claude/skills/aver-tooling/SKILL.md"),
        "{stdout}"
    );
    assert!(stdout.contains("created AGENTS.md"), "{stdout}");
    assert!(stdout.contains("aver --help"), "{stdout}");
}

#[test]
fn an_existing_agents_file_keeps_every_byte_outside_the_marked_block() {
    let project = tempfile::tempdir().expect("project dir");
    let home = tempfile::tempdir().expect("home dir");
    let agents = project.path().join("AGENTS.md");
    let original = "# House rules\n\nNever run the release script by hand.\n\n## Build\n\n`make`\n";
    fs::write(&agents, original).expect("seed AGENTS.md");

    let run = agent_connect(project.path(), home.path(), &[]);
    assert!(run.status.success(), "{}", format_output(&run));

    let updated = fs::read_to_string(&agents).expect("AGENTS.md");
    assert!(
        updated.starts_with(original),
        "the pre-existing bytes must survive verbatim, got:\n{updated}"
    );
    assert!(updated.contains(SECTION_START));

    // Strip the block back out: what is left is the original file plus the
    // blank line that separated the appended section from it.
    let start = updated.find(SECTION_START).expect("start marker");
    let end = updated.rfind(SECTION_END).expect("end marker") + SECTION_END.len();
    let outside = format!("{}{}", &updated[..start], &updated[end..]);
    assert_eq!(outside, format!("{original}\n\n"));

    assert!(stdout_of(&run).contains("updated AGENTS.md"));
}

#[test]
fn an_agents_file_that_only_talks_about_the_markers_is_appended_to() {
    // The guide this command installs contains a sentence naming both markers,
    // so a reader's AGENTS.md may quote it. That sentence is prose, not a
    // section: the run must append below it and change none of its bytes.
    let project = tempfile::tempdir().expect("project dir");
    let home = tempfile::tempdir().expect("home dir");
    let agents = project.path().join("AGENTS.md");
    let original = format!(
        "# House rules\n\nAver keeps a pointer section in `AGENTS.md` between `{SECTION_START}` \
         and `{SECTION_END}`.\n\n## Build\n\n`make`\n"
    );
    fs::write(&agents, &original).expect("seed AGENTS.md");

    let run = agent_connect(project.path(), home.path(), &[]);
    assert!(run.status.success(), "{}", format_output(&run));

    let updated = fs::read_to_string(&agents).expect("AGENTS.md");
    assert!(
        updated.starts_with(&original),
        "the sentence must survive verbatim, got:\n{updated}"
    );
    assert_eq!(
        updated,
        format!("{original}\n{}\n", block_of(&updated)),
        "only a trailing block may be added"
    );
    assert!(updated.contains("## Build"), "{updated}");
}

#[test]
fn a_file_with_two_marked_sections_is_refused_rather_than_collapsed() {
    let project = tempfile::tempdir().expect("project dir");
    let home = tempfile::tempdir().expect("home dir");
    let agents = project.path().join("AGENTS.md");
    let original = format!(
        "{SECTION_START}\nfirst\n{SECTION_END}\n\nMINE, KEEP ME\n\n{SECTION_START}\nsecond\n{SECTION_END}\n"
    );
    fs::write(&agents, &original).expect("seed AGENTS.md");

    let run = agent_connect(project.path(), home.path(), &[]);

    assert!(
        !run.status.success(),
        "two sections must be refused:\n{}",
        format_output(&run)
    );
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(stderr.contains("more than one"), "{stderr}");
    assert_eq!(
        fs::read_to_string(&agents).expect("re-read AGENTS.md"),
        original,
        "a refused file must be untouched"
    );
}

/// The trailing managed block inside `text`, markers included. `rfind`,
/// because the text above it may quote the marker names in prose.
fn block_of(text: &str) -> String {
    let start = text.rfind(SECTION_START).expect("start marker");
    let end = text.rfind(SECTION_END).expect("end marker") + SECTION_END.len();
    text[start..end].to_string()
}

#[test]
fn a_second_run_leaves_the_tree_byte_identical() {
    let project = tempfile::tempdir().expect("project dir");
    let home = tempfile::tempdir().expect("home dir");
    fs::write(project.path().join("AGENTS.md"), "# Notes\n\nkeep me\n").expect("seed AGENTS.md");

    let first = agent_connect(project.path(), home.path(), &[]);
    assert!(first.status.success(), "{}", format_output(&first));

    let snapshot: Vec<String> = [
        ".claude/skills/aver/SKILL.md",
        ".claude/skills/aver-tooling/SKILL.md",
        "AGENTS.md",
    ]
    .iter()
    .map(|rel| fs::read_to_string(project.path().join(rel)).expect("read after first run"))
    .collect();

    let second = agent_connect(project.path(), home.path(), &[]);
    assert!(second.status.success(), "{}", format_output(&second));

    for (index, rel) in [
        ".claude/skills/aver/SKILL.md",
        ".claude/skills/aver-tooling/SKILL.md",
        "AGENTS.md",
    ]
    .iter()
    .enumerate()
    {
        assert_eq!(
            fs::read_to_string(project.path().join(rel)).expect("read after second run"),
            snapshot[index],
            "{rel} changed on the second run"
        );
    }

    let stdout = stdout_of(&second);
    assert_eq!(
        stdout.matches("unchanged ").count(),
        3,
        "every file should report unchanged:\n{stdout}"
    );
}

#[test]
fn print_emits_the_language_guide_and_writes_nothing() {
    let project = tempfile::tempdir().expect("project dir");
    let home = tempfile::tempdir().expect("home dir");

    let run = agent_connect(project.path(), home.path(), &["--print"]);
    assert!(run.status.success(), "{}", format_output(&run));

    let stdout = stdout_of(&run);
    assert!(
        stdout.starts_with("# Aver\n"),
        "{}",
        &stdout[..80.min(stdout.len())]
    );
    assert!(stdout.contains("### Decision blocks"));
    assert!(stdout.contains("aver context decisions/architecture.av --decisions-only"));
    assert!(stdout.contains("## Further reading"));

    assert_eq!(
        fs::read_dir(project.path())
            .expect("read project dir")
            .count(),
        0,
        "--print must not write anything"
    );
    assert_eq!(fs::read_dir(home.path()).expect("read home dir").count(), 0);
}

#[test]
fn global_writes_into_the_overridden_home_and_leaves_the_project_alone() {
    let project = tempfile::tempdir().expect("project dir");
    let home = tempfile::tempdir().expect("home dir");

    let run = agent_connect(project.path(), home.path(), &["--global"]);
    assert!(run.status.success(), "{}", format_output(&run));

    for name in ["aver", "aver-tooling"] {
        let skill = home
            .path()
            .join(".claude/skills")
            .join(name)
            .join("SKILL.md");
        let text = fs::read_to_string(&skill).unwrap_or_else(|_| panic!("{name} skill"));
        assert_loadable_skill(&text, name);
    }
    assert!(
        !project.path().join("AGENTS.md").exists(),
        "--global must not touch the project"
    );
    assert_eq!(
        fs::read_dir(project.path())
            .expect("read project dir")
            .count(),
        0
    );

    // Global paths are printed in full — there is no project to relativize against.
    let stdout = stdout_of(&run);
    assert!(
        stdout.contains(&format!(
            "created {}",
            home.path().join(".claude/skills/aver/SKILL.md").display()
        )),
        "{stdout}"
    );
    assert!(stdout.contains("your agent setup"), "{stdout}");
}

#[test]
fn a_skill_file_we_did_not_write_is_refused_by_name() {
    let project = tempfile::tempdir().expect("project dir");
    let home = tempfile::tempdir().expect("home dir");
    let skill = project.path().join(".claude/skills/aver/SKILL.md");
    fs::create_dir_all(skill.parent().expect("parent")).expect("create skill dir");
    fs::write(&skill, "---\nname: aver\n---\n\nmy own notes\n").expect("seed foreign skill");

    let run = agent_connect(project.path(), home.path(), &[]);

    assert!(
        !run.status.success(),
        "a foreign file must make the command fail:\n{}",
        format_output(&run)
    );
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(stderr.contains("refusing to overwrite"), "{stderr}");
    assert!(stderr.contains("SKILL.md"), "{stderr}");
    assert_eq!(
        fs::read_to_string(&skill).expect("re-read foreign skill"),
        "---\nname: aver\n---\n\nmy own notes\n",
        "the refused file must be untouched"
    );
    assert!(
        !project.path().join("AGENTS.md").exists(),
        "a refusal must not half-apply"
    );
}

#[test]
fn print_and_global_cannot_be_combined() {
    let project = tempfile::tempdir().expect("project dir");
    let home = tempfile::tempdir().expect("home dir");

    let run = agent_connect(project.path(), home.path(), &["--print", "--global"]);

    assert!(!run.status.success(), "{}", format_output(&run));
}
