use crate::{AverDisplay, AverList, aver_display};
use std::cell::RefCell;

thread_local! {
    static CONSOLE_CAPTURE: RefCell<Option<(Vec<u8>, Vec<u8>)>> = const { RefCell::new(None) };
}

pub fn capture_console_output<F, R>(f: F) -> (R, Vec<u8>, Vec<u8>)
where
    F: FnOnce() -> R,
{
    CONSOLE_CAPTURE.with(|capture| {
        let mut slot = capture.borrow_mut();
        assert!(
            slot.is_none(),
            "capture_console_output: nested capture is not supported"
        );
        *slot = Some((Vec::new(), Vec::new()));
    });
    let result = f();
    let (stdout, stderr) = CONSOLE_CAPTURE
        .with(|capture| capture.borrow_mut().take())
        .unwrap_or_default();
    (result, stdout, stderr)
}

fn write_console_stdout(text: &str) {
    CONSOLE_CAPTURE.with(|capture| {
        if let Some((stdout, _)) = capture.borrow_mut().as_mut() {
            stdout.extend_from_slice(text.as_bytes());
            stdout.push(b'\n');
        } else {
            println!("{text}");
        }
    });
}

fn write_console_stderr(text: &str) {
    CONSOLE_CAPTURE.with(|capture| {
        if let Some((_, stderr)) = capture.borrow_mut().as_mut() {
            stderr.extend_from_slice(text.as_bytes());
            stderr.push(b'\n');
        } else {
            eprintln!("{text}");
        }
    });
}

pub fn console_print<T: AverDisplay>(val: &T) {
    write_console_stdout(&aver_display(val));
}

pub fn console_error<T: AverDisplay>(val: &T) {
    write_console_stderr(&aver_display(val));
}

pub fn console_warn<T: AverDisplay>(val: &T) {
    write_console_stderr(&format!("[warn] {}", aver_display(val)));
}

pub fn read_line() -> Result<String, String> {
    let mut buf = String::new();
    match std::io::stdin().read_line(&mut buf) {
        Ok(0) => Err("EOF".to_string()),
        Ok(_) => {
            if buf.ends_with('\n') {
                buf.pop();
                if buf.ends_with('\r') {
                    buf.pop();
                }
            }
            Ok(buf)
        }
        Err(e) => Err(e.to_string()),
    }
}

pub fn time_now() -> String {
    let (secs, nanos) = unix_parts_now();
    format_utc_rfc3339_like(secs, nanos)
}

pub fn time_unix_ms() -> i64 {
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    {
        // `std::time::SystemTime::now()` panics on
        // wasm32-unknown-unknown ("time not implemented on this
        // platform"). Use `Date.now()` from JS — same epoch, same
        // unit, well within i64. Symmetric with the time_sleep
        // wasm32 no-op fallback added in 0.10.0.
        return js_sys::Date::now() as i64;
    }
    #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
    {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("Time.unixMs: system clock error");
        i64::try_from(now.as_millis()).expect("Time.unixMs: value out of i64 range")
    }
}

pub fn time_sleep(ms: i64) {
    if ms < 0 {
        panic!("Time.sleep: ms must be non-negative");
    }
    // Browsers have no synchronous sleep primitive on
    // wasm32-unknown-unknown — std::thread::sleep panics with
    // "can't sleep". Treat it as a no-op there; the playground's
    // recorder still captures the call + duration in args, and
    // replay reproduces the trace faithfully. Native builds keep
    // real blocking sleep.
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    {
        let _ = ms;
        return;
    }
    #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
    std::thread::sleep(std::time::Duration::from_millis(ms as u64));
}

pub fn string_slice(s: &str, from: i64, to: i64) -> String {
    let start = from.max(0) as usize;
    let end = to.max(0) as usize;
    if start >= end {
        return String::new();
    }

    let mut start_byte = None;
    let mut end_byte = None;
    let mut char_index = 0usize;

    for (byte_index, _) in s.char_indices() {
        if char_index == start {
            start_byte = Some(byte_index);
        }
        if char_index == end {
            end_byte = Some(byte_index);
            break;
        }
        char_index += 1;
    }

    if start_byte.is_none() && char_index == start {
        start_byte = Some(s.len());
    }
    if end_byte.is_none() && char_index == end {
        end_byte = Some(s.len());
    }

    let start_byte = start_byte.unwrap_or(s.len());
    let end_byte = end_byte.unwrap_or(s.len());
    if start_byte >= end_byte {
        return String::new();
    }

    s[start_byte..end_byte].to_string()
}

pub fn read_text(path: &str) -> Result<String, String> {
    std::fs::read_to_string(path).map_err(|e| e.to_string())
}

pub fn write_text(path: &str, content: &str) -> Result<(), String> {
    std::fs::write(path, content)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

pub fn append_text(path: &str, content: &str) -> Result<(), String> {
    use std::io::Write;

    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .map_err(|e| e.to_string())?;
    file.write_all(content.as_bytes())
        .map_err(|e| e.to_string())
}

pub fn read_bytes(path: &str) -> Result<Vec<u8>, String> {
    std::fs::read(path).map_err(|e| e.to_string())
}

/// Read at most `length` octets starting at `offset`.
///
/// Reaching EOF is successful: the returned vector may be shorter than the
/// requested length, and an offset at or beyond EOF returns an empty vector.
pub fn read_bytes_at(
    path: &str,
    offset: &crate::AverInt,
    length: &crate::AverInt,
) -> Result<Vec<u8>, String> {
    use std::io::{Read, Seek};

    let offset = offset
        .to_i64()
        .ok_or_else(|| "Disk.readBytesAt: offset must fit a 64-bit integer".to_string())?;
    let length = length
        .to_i64()
        .ok_or_else(|| "Disk.readBytesAt: length must fit a 64-bit integer".to_string())?;
    if offset < 0 {
        return Err("Disk.readBytesAt: offset must be non-negative".to_string());
    }
    if length < 0 {
        return Err("Disk.readBytesAt: length must be non-negative".to_string());
    }

    let mut file = std::fs::File::open(path).map_err(|error| error.to_string())?;
    file.seek(std::io::SeekFrom::Start(offset as u64))
        .map_err(|error| error.to_string())?;
    let mut bytes = Vec::new();
    file.take(length as u64)
        .read_to_end(&mut bytes)
        .map_err(|error| error.to_string())?;
    Ok(bytes)
}

pub fn file_size(path: &str) -> Result<crate::AverInt, String> {
    std::fs::metadata(path)
        .map(|metadata| crate::AverInt::from_bigint(num_bigint::BigInt::from(metadata.len())))
        .map_err(|error| error.to_string())
}

pub fn write_bytes(path: &str, content: &[u8]) -> Result<(), String> {
    std::fs::write(path, content)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

pub fn append_bytes(path: &str, content: &[u8]) -> Result<(), String> {
    use std::io::Write;

    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .map_err(|e| e.to_string())?;
    file.write_all(content).map_err(|e| e.to_string())
}

/// Force the named path's bytes AND metadata to stable storage.
///
/// The path may be a file or a directory. Syncing a directory is what
/// makes a newly created file's directory entry durable — an fsync of
/// the file alone does not do that on POSIX, so a crash-safe "create
/// then name it" sequence syncs the file and then its parent directory.
///
/// The descriptor is opened read-only on purpose: it only has to name
/// the file, and the kernel flushes the file's dirty pages whoever
/// opened it. A read-only open also lets a directory path through,
/// which a write open would refuse.
///
/// `File::sync_all` is `fcntl(F_FULLFSYNC)` on macOS, so the bytes go
/// past the drive's write cache rather than only to the disk buffer.
pub fn sync_path(path: &str) -> Result<(), String> {
    std::fs::File::open(path)
        .and_then(|file| file.sync_all())
        .map_err(|error| error.to_string())
}

pub fn path_exists(path: &str) -> bool {
    std::path::Path::new(path).exists()
}

pub fn delete_file(path: &str) -> Result<(), String> {
    let p = std::path::Path::new(path);
    if p.is_dir() {
        return Err(
            "Disk.delete: path is a directory — use Disk.deleteDir to remove directories"
                .to_string(),
        );
    }
    std::fs::remove_file(p)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

pub fn delete_dir(path: &str) -> Result<(), String> {
    let p = std::path::Path::new(path);
    if !p.is_dir() {
        return Err(
            "Disk.deleteDir: path is not a directory — use Disk.delete to remove files".to_string(),
        );
    }
    std::fs::remove_dir_all(p)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

pub fn list_dir(path: &str) -> Result<AverList<String>, String> {
    let entries = std::fs::read_dir(path).map_err(|e| e.to_string())?;
    let mut result = Vec::new();
    for entry in entries {
        let entry = entry.map_err(|e| e.to_string())?;
        result.push(entry.file_name().to_string_lossy().into_owned());
    }
    Ok(AverList::from_vec(result))
}

pub fn make_dir(path: &str) -> Result<(), String> {
    std::fs::create_dir_all(path)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

pub fn env_get(key: &str) -> Option<String> {
    std::env::var(key).ok()
}

pub fn env_set(key: &str, value: &str) -> Result<(), String> {
    validate_env_key(key)?;
    if value.contains('\0') {
        return Err("Env.set: value must not contain NUL".to_string());
    }

    unsafe {
        std::env::set_var(key, value);
    }
    Ok(())
}

pub fn cli_args() -> AverList<String> {
    AverList::from_vec(std::env::args().skip(1).collect())
}

fn validate_env_key(key: &str) -> Result<(), String> {
    if key.is_empty() {
        return Err("Env.set: key must not be empty".to_string());
    }
    if key.contains('=') {
        return Err("Env.set: key must not contain '='".to_string());
    }
    if key.contains('\0') {
        return Err("Env.set: key must not contain NUL".to_string());
    }
    Ok(())
}

fn unix_parts_now() -> (i64, u32) {
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    {
        // `SystemTime::now()` panics on wasm32-unknown-unknown.
        // Decompose the JS millisecond clock into (secs, nanos).
        let ms = js_sys::Date::now();
        let secs = (ms / 1000.0) as i64;
        let nanos = ((ms.rem_euclid(1000.0)) * 1_000_000.0) as u32;
        return (secs, nanos);
    }
    #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
    {
        match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
            Ok(d) => (
                i64::try_from(d.as_secs()).expect("Time.now: seconds out of i64 range"),
                d.subsec_nanos(),
            ),
            Err(e) => {
                let d = e.duration();
                let secs = i64::try_from(d.as_secs()).expect("Time.now: seconds out of i64 range");
                let nanos = d.subsec_nanos();
                if nanos == 0 {
                    (-secs, 0)
                } else {
                    (-(secs + 1), 1_000_000_000 - nanos)
                }
            }
        }
    }
}

fn format_utc_rfc3339_like(unix_secs: i64, nanos: u32) -> String {
    let days = unix_secs.div_euclid(86_400);
    let sod = unix_secs.rem_euclid(86_400);
    let hour = sod / 3_600;
    let minute = (sod % 3_600) / 60;
    let second = sod % 60;
    let millis = nanos / 1_000_000;
    let (year, month, day) = civil_from_days(days);
    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}Z",
        year, month, day, hour, minute, second, millis
    )
}

fn civil_from_days(days_since_epoch: i64) -> (i32, u32, u32) {
    let z = days_since_epoch + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1_460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let day = doy - (153 * mp + 2) / 5 + 1;
    let month = mp + if mp < 10 { 3 } else { -9 };
    let year = y + if month <= 2 { 1 } else { 0 };
    (year as i32, month as u32, day as u32)
}

#[cfg(test)]
mod disk_binary_tests {
    use super::{append_bytes, file_size, read_bytes, read_bytes_at, write_bytes};
    use crate::AverInt;

    fn temp_file() -> std::path::PathBuf {
        let nonce = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("clock after epoch")
            .as_nanos();
        std::env::temp_dir().join(format!("aver-rt-disk-bytes-{nonce}.bin"))
    }

    #[test]
    fn positional_reads_are_bounded_and_eof_is_short() {
        let path = temp_file();
        let path_text = path.to_string_lossy();
        write_bytes(&path_text, &[0, 127, 128, 255]).expect("write bytes");
        append_bytes(&path_text, &[1, 2]).expect("append bytes");

        assert_eq!(
            read_bytes(&path_text).expect("read bytes"),
            [0, 127, 128, 255, 1, 2]
        );
        assert_eq!(
            read_bytes_at(&path_text, &AverInt::from_i64(2), &AverInt::from_i64(99))
                .expect("short EOF read"),
            [128, 255, 1, 2]
        );
        assert_eq!(
            read_bytes_at(
                &path_text,
                &AverInt::from_i64(0),
                &AverInt::from_i64(5_000_000_000),
            )
            .expect("large upper bound with early EOF"),
            [0, 127, 128, 255, 1, 2]
        );
        assert!(
            read_bytes_at(&path_text, &AverInt::from_i64(99), &AverInt::from_i64(4))
                .expect("past EOF read")
                .is_empty()
        );
        assert_eq!(
            file_size(&path_text).expect("file size"),
            AverInt::from_i64(6)
        );
        assert!(
            read_bytes_at(&path_text, &AverInt::from_i64(-1), &AverInt::from_i64(1))
                .expect_err("negative offset")
                .contains("offset must be non-negative")
        );
        assert!(
            read_bytes_at(&path_text, &AverInt::from_i64(0), &AverInt::from_i64(-1))
                .expect_err("negative length")
                .contains("length must be non-negative")
        );

        let _ = std::fs::remove_file(path);
    }
}
