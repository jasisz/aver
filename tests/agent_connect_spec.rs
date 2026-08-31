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
    assert!(
        language_text.starts_with("---\nname: aver\ndescription: "),
        "the skill must open with frontmatter, got:\n{}",
        &language_text[..language_text.len().min(200)]
    );
    assert!(language_text.contains(MANAGED_MARKER));
    assert!(
        language_text.contains("### Decision blocks"),
        "the language guide body must be carried into the skill"
    );

    let tooling_text = fs::read_to_string(&tooling).expect("tooling skill");
    assert!(tooling_text.starts_with("---\nname: aver-tooling\ndescription: "));
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

    assert!(home.path().join(".claude/skills/aver/SKILL.md").is_file());
    assert!(
        home.path()
            .join(".claude/skills/aver-tooling/SKILL.md")
            .is_file()
    );
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
