/// Terminal service — raw mode, cursor control, colored output, key input.
///
/// Built on `crossterm`. Every public operation reports adapter failures as
/// `Result<_, String>`; a successful key poll still uses `Option<String>` to
/// distinguish "no key available" from an input error.
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

/// What a thread writes, made at its first write. A thread that ends
/// flushes it.
type Out = BufWriter<io::Stdout>;

thread_local! {
    static STDOUT_BUF: std::cell::RefCell<Option<Out>> = const { std::cell::RefCell::new(None) };
}

fn with_out<R>(write: impl FnOnce(&mut Out) -> R) -> R {
    STDOUT_BUF.with(|buf| {
        write(
            buf.borrow_mut()
                .get_or_insert_with(|| BufWriter::with_capacity(65536, io::stdout())),
        )
    })
}

/// Puts `out` in place of this thread's buffer and gives the old one back
/// (see [`crate::as_own_thread`]).
pub(crate) fn swap_out(out: Option<Out>) -> Option<Out> {
    STDOUT_BUF.with(|buf| std::mem::replace(&mut *buf.borrow_mut(), out))
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
    with_out(|out| {
        out.queue(terminal::Clear(terminal::ClearType::All))
            .map(|_| ())
            .map_err(|e| e.to_string())
    })
}

pub fn move_to(x: i64, y: i64) -> Result<(), String> {
    let x = u16::try_from(x).map_err(|_| format!("Terminal.moveTo: x={} out of range", x))?;
    let y = u16::try_from(y).map_err(|_| format!("Terminal.moveTo: y={} out of range", y))?;
    with_out(|out| {
        out.queue(cursor::MoveTo(x, y))
            .map(|_| ())
            .map_err(|e| e.to_string())
    })
}

pub fn print_at_cursor(s: &str) -> Result<(), String> {
    with_out(|out| out.write_all(s.as_bytes()).map_err(|e| e.to_string()))
}

pub fn set_color(color: &str) -> Result<(), String> {
    let c = parse_color(color)?;
    with_out(|out| {
        out.queue(SetForegroundColor(c))
            .map(|_| ())
            .map_err(|e| e.to_string())
    })
}

pub fn reset_color() -> Result<(), String> {
    with_out(|out| {
        out.queue(style::ResetColor)
            .map(|_| ())
            .map_err(|e| e.to_string())
    })
}

/// Non-blocking key read. Returns `Ok(None)` if no key is available.
pub fn read_key() -> Result<Option<String>, String> {
    if !event::poll(Duration::ZERO).map_err(|e| e.to_string())? {
        return Ok(None);
    }
    let event = event::read().map_err(|e| e.to_string())?;
    Ok(match event {
        Event::Key(KeyEvent {
            code, modifiers, ..
        }) => {
            // Ctrl+C always produces "esc" so the game can exit
            if modifiers.contains(KeyModifiers::CONTROL) && code == KeyCode::Char('c') {
                return Ok(Some("esc".to_string()));
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
    })
}

pub fn size() -> Result<(i64, i64), String> {
    let (w, h) = terminal::size().map_err(|e| e.to_string())?;
    Ok((w as i64, h as i64))
}

pub fn hide_cursor() -> Result<(), String> {
    with_out(|out| {
        out.queue(cursor::Hide)
            .map(|_| ())
            .map_err(|e| e.to_string())
    })
}

pub fn show_cursor() -> Result<(), String> {
    with_out(|out| {
        out.queue(cursor::Show)
            .map(|_| ())
            .map_err(|e| e.to_string())
    })
}

pub fn flush() -> Result<(), String> {
    with_out(|out| out.flush().map_err(|e| e.to_string()))
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

#[cfg(test)]
mod tests {
    use super::*;

    fn buffered() -> Vec<u8> {
        STDOUT_BUF.with(|buf| {
            buf.borrow()
                .as_ref()
                .map(|out| out.buffer().to_vec())
                .unwrap_or_default()
        })
    }

    #[test]
    fn code_run_as_its_own_thread_writes_to_a_buffer_of_its_own() {
        print_at_cursor("caller").unwrap();
        crate::as_own_thread(|| {
            assert!(buffered().is_empty());
            print_at_cursor("\n").unwrap();
            assert_eq!(buffered(), b"\n");
        });
        assert_eq!(buffered(), b"caller");
        // Leave nothing to print: drop the caller's bytes unwritten.
        if let Some(out) = swap_out(None) {
            let _ = out.into_parts();
        }
    }
}
