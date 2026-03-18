/// Terminal service — raw mode, cursor control, colored output, key input.
///
/// Built on `crossterm`. All public functions return `Result<_, String>` or `Option<String>`.
use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    style::{self, Color, SetForegroundColor},
    terminal,
};
use std::io::{self, BufWriter, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use crossterm::{QueueableCommand, execute};

thread_local! {
    static STDOUT_BUF: std::cell::RefCell<BufWriter<io::Stdout>> =
        std::cell::RefCell::new(BufWriter::with_capacity(65536, io::stdout()));
}

/// Global flag: true while raw mode is active.
static RAW_MODE_ACTIVE: AtomicBool = AtomicBool::new(false);

pub fn enable_raw_mode() -> Result<(), String> {
    terminal::enable_raw_mode().map_err(|e| e.to_string())?;
    RAW_MODE_ACTIVE.store(true, Ordering::SeqCst);
    Ok(())
}

pub fn disable_raw_mode() -> Result<(), String> {
    RAW_MODE_ACTIVE.store(false, Ordering::SeqCst);
    terminal::disable_raw_mode().map_err(|e| e.to_string())
}

/// Restore terminal state if raw mode is active.
/// Call this from panic hooks or cleanup paths.
pub fn restore_terminal() {
    if RAW_MODE_ACTIVE.swap(false, Ordering::SeqCst) {
        let _ = execute!(io::stdout(), cursor::Show);
        let _ = execute!(io::stdout(), style::ResetColor);
        let _ = terminal::disable_raw_mode();
        let _ = io::stdout().write_all(b"\n");
        let _ = io::stdout().flush();
    }
}

pub fn clear() -> Result<(), String> {
    STDOUT_BUF.with(|buf| {
        buf.borrow_mut()
            .queue(terminal::Clear(terminal::ClearType::All))
            .map(|_| ())
            .map_err(|e| e.to_string())
    })
}

pub fn move_to(x: i64, y: i64) -> Result<(), String> {
    let x = u16::try_from(x).map_err(|_| format!("Terminal.moveTo: x={} out of range", x))?;
    let y = u16::try_from(y).map_err(|_| format!("Terminal.moveTo: y={} out of range", y))?;
    STDOUT_BUF.with(|buf| {
        buf.borrow_mut()
            .queue(cursor::MoveTo(x, y))
            .map(|_| ())
            .map_err(|e| e.to_string())
    })
}

pub fn print_at_cursor(s: &str) -> Result<(), String> {
    STDOUT_BUF.with(|buf| {
        buf.borrow_mut()
            .write_all(s.as_bytes())
            .map_err(|e| e.to_string())
    })
}

pub fn set_color(color: &str) -> Result<(), String> {
    let c = parse_color(color)?;
    STDOUT_BUF.with(|buf| {
        buf.borrow_mut()
            .queue(SetForegroundColor(c))
            .map(|_| ())
            .map_err(|e| e.to_string())
    })
}

pub fn reset_color() -> Result<(), String> {
    STDOUT_BUF.with(|buf| {
        buf.borrow_mut()
            .queue(style::ResetColor)
            .map(|_| ())
            .map_err(|e| e.to_string())
    })
}

/// Non-blocking key read. Returns `None` if no key is available.
pub fn read_key() -> Option<String> {
    if !event::poll(Duration::ZERO).ok()? {
        return None;
    }
    let event = event::read().ok()?;
    match event {
        Event::Key(KeyEvent {
            code, modifiers, ..
        }) => {
            // Ctrl+C always produces "esc" so the game can exit
            if modifiers.contains(KeyModifiers::CONTROL) && code == KeyCode::Char('c') {
                return Some("esc".to_string());
            }
            match code {
                KeyCode::Up => Some("up".to_string()),
                KeyCode::Down => Some("down".to_string()),
                KeyCode::Left => Some("left".to_string()),
                KeyCode::Right => Some("right".to_string()),
                KeyCode::Esc => Some("esc".to_string()),
                KeyCode::Enter => Some("enter".to_string()),
                KeyCode::Char(c) => Some(c.to_string()),
                _ => None,
            }
        }
        _ => None,
    }
}

pub fn size() -> Result<(i64, i64), String> {
    let (w, h) = terminal::size().map_err(|e| e.to_string())?;
    Ok((w as i64, h as i64))
}

pub fn hide_cursor() -> Result<(), String> {
    STDOUT_BUF.with(|buf| {
        buf.borrow_mut()
            .queue(cursor::Hide)
            .map(|_| ())
            .map_err(|e| e.to_string())
    })
}

pub fn show_cursor() -> Result<(), String> {
    STDOUT_BUF.with(|buf| {
        buf.borrow_mut()
            .queue(cursor::Show)
            .map(|_| ())
            .map_err(|e| e.to_string())
    })
}

pub fn flush() -> Result<(), String> {
    STDOUT_BUF.with(|buf| buf.borrow_mut().flush().map_err(|e| e.to_string()))
}

/// RAII guard that restores the terminal on drop.
/// Lives on the stack in `cmd_run`; cleanup fires even on panic or early exit.
#[derive(Default)]
pub struct TerminalGuard;

impl TerminalGuard {
    pub fn new() -> Self {
        Self
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        restore_terminal();
    }
}

fn parse_color(s: &str) -> Result<Color, String> {
    match s {
        "red" => Ok(Color::Red),
        "green" => Ok(Color::Green),
        "yellow" => Ok(Color::Yellow),
        "blue" => Ok(Color::Blue),
        "white" => Ok(Color::White),
        "cyan" => Ok(Color::Cyan),
        "magenta" => Ok(Color::Magenta),
        "black" => Ok(Color::Black),
        _ => Err(format!(
            "Terminal.setColor: unknown color '{}' (expected: red, green, yellow, blue, white, cyan, magenta, black)",
            s
        )),
    }
}
