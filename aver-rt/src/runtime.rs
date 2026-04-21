use crate::{AverDisplay, AverList, aver_display};

pub fn console_print<T: AverDisplay>(val: &T) {
    println!("{}", aver_display(val));
}

pub fn console_error<T: AverDisplay>(val: &T) {
    eprintln!("{}", aver_display(val));
}

pub fn console_warn<T: AverDisplay>(val: &T) {
    eprintln!("[warn] {}", aver_display(val));
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
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("Time.unixMs: system clock error");
    i64::try_from(now.as_millis()).expect("Time.unixMs: value out of i64 range")
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
