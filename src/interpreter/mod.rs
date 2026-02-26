use std::collections::{HashMap, HashSet};
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::Path;
use std::time::Duration;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::services::{console, disk, http, http_server, tcp};
use crate::source::{canonicalize_path, find_module_file, parse_source};
use crate::types::{float, int, list, string};
// Re-export value types so existing `use aver::interpreter::Value` imports keep working.
pub use crate::value::{aver_display, aver_repr, Env, RuntimeError, Value};

#[derive(Debug, Clone)]
struct CallFrame {
    name: String,
    effects: Vec<String>,
}

#[derive(Debug, Clone)]
struct ServerRequest {
    method: String,
    path: String,
    body: String,
    headers: Vec<(String, String)>,
}

#[derive(Debug, Clone)]
struct ServerResponse {
    status: i64,
    body: String,
    headers: Vec<(String, String)>,
}

pub struct Interpreter {
    pub env: Env,
    module_cache: HashMap<String, Value>,
    call_stack: Vec<CallFrame>,
    /// Named effect aliases: `effects AppIO = [Console, Disk]`
    effect_aliases: HashMap<String, Vec<String>>,
}

// ─────────────────────────────────────────────────────────────────────────────

impl Interpreter {
    pub fn new() -> Self {
        let mut global = HashMap::new();

        console::register(&mut global);
        http::register(&mut global);
        http_server::register(&mut global);
        disk::register(&mut global);
        tcp::register(&mut global);
        int::register(&mut global);
        float::register(&mut global);
        string::register(&mut global);
        list::register(&mut global);

        // Result and Option namespaces — constructors for Ok/Err/Some/None
        {
            let mut members = HashMap::new();
            members.insert(
                "Ok".to_string(),
                Value::Builtin("__ctor:Result.Ok".to_string()),
            );
            members.insert(
                "Err".to_string(),
                Value::Builtin("__ctor:Result.Err".to_string()),
            );
            global.insert(
                "Result".to_string(),
                Value::Namespace {
                    name: "Result".to_string(),
                    members,
                },
            );
        }
        {
            let mut members = HashMap::new();
            members.insert(
                "Some".to_string(),
                Value::Builtin("__ctor:Option.Some".to_string()),
            );
            members.insert("None".to_string(), Value::None);
            global.insert(
                "Option".to_string(),
                Value::Namespace {
                    name: "Option".to_string(),
                    members,
                },
            );
        }

        Interpreter {
            env: vec![global],
            module_cache: HashMap::new(),
            call_stack: Vec::new(),
            effect_aliases: HashMap::new(),
        }
    }

    /// Register a named effect set alias.
    pub fn register_effect_set(&mut self, name: String, effects: Vec<String>) {
        self.effect_aliases.insert(name, effects);
    }

    /// Expand effect names one level: aliases → concrete effect names.
    fn expand_effects(&self, effects: &[String]) -> Vec<String> {
        let mut result = Vec::new();
        for e in effects {
            if let Some(expanded) = self.effect_aliases.get(e) {
                result.extend(expanded.iter().cloned());
            } else {
                result.push(e.clone());
            }
        }
        result
    }

    // -------------------------------------------------------------------------
    // Environment management
    // -------------------------------------------------------------------------
    fn push_env(&mut self, base: HashMap<String, Value>) {
        self.env.push(base);
    }

    fn pop_env(&mut self) {
        if self.env.len() > 1 {
            self.env.pop();
        }
    }

    pub fn lookup(&self, name: &str) -> Result<Value, RuntimeError> {
        for scope in self.env.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Ok(v.clone());
            }
        }
        Err(RuntimeError::Error(format!(
            "Undefined variable: '{}'",
            name
        )))
    }

    pub fn define(&mut self, name: String, val: Value) {
        if let Some(scope) = self.env.last_mut() {
            scope.insert(name, val);
        }
    }

    pub fn define_module_path(&mut self, path: &str, val: Value) -> Result<(), RuntimeError> {
        let parts: Vec<&str> = path.split('.').filter(|s| !s.is_empty()).collect();
        if parts.is_empty() {
            return Err(RuntimeError::Error("Empty module path".to_string()));
        }
        if parts.len() == 1 {
            self.define(parts[0].to_string(), val);
            return Ok(());
        }

        let scope = self
            .env
            .last_mut()
            .ok_or_else(|| RuntimeError::Error("No active scope".to_string()))?;
        Self::insert_namespace_path(scope, &parts, val)
    }

    fn insert_namespace_path(
        scope: &mut HashMap<String, Value>,
        parts: &[&str],
        val: Value,
    ) -> Result<(), RuntimeError> {
        if parts.len() == 1 {
            scope.insert(parts[0].to_string(), val);
            return Ok(());
        }

        let head = parts[0];
        let tail = &parts[1..];

        if let Some(existing) = scope.remove(head) {
            match existing {
                Value::Namespace { name, mut members } => {
                    Self::insert_namespace_path(&mut members, tail, val)?;
                    scope.insert(head.to_string(), Value::Namespace { name, members });
                    Ok(())
                }
                _ => Err(RuntimeError::Error(format!(
                    "Cannot mount module '{}': '{}' is not a namespace",
                    parts.join("."),
                    head
                ))),
            }
        } else {
            let mut members = HashMap::new();
            Self::insert_namespace_path(&mut members, tail, val)?;
            scope.insert(
                head.to_string(),
                Value::Namespace {
                    name: head.to_string(),
                    members,
                },
            );
            Ok(())
        }
    }

    /// Walk the scope stack from innermost outward and update the first binding found.
    /// Returns Err if no binding exists (use `define` for new bindings).
    pub fn assign(&mut self, name: &str, val: Value) -> Result<(), RuntimeError> {
        for scope in self.env.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), val);
                return Ok(());
            }
        }
        Err(RuntimeError::Error(format!(
            "Cannot assign to '{}': variable not declared",
            name
        )))
    }

    fn module_cache_key(path: &Path) -> String {
        canonicalize_path(path).to_string_lossy().to_string()
    }

    fn module_decl(items: &[TopLevel]) -> Option<&Module> {
        items.iter().find_map(|item| {
            if let TopLevel::Module(m) = item {
                Some(m)
            } else {
                None
            }
        })
    }

    fn exposed_set(items: &[TopLevel]) -> Option<HashSet<String>> {
        Self::module_decl(items).and_then(|m| {
            if m.exposes.is_empty() {
                None
            } else {
                Some(m.exposes.iter().cloned().collect())
            }
        })
    }

    fn cycle_display(loading: &[String], next: &str) -> String {
        let mut chain = loading
            .iter()
            .map(|key| {
                Path::new(key)
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or(key)
                    .to_string()
            })
            .collect::<Vec<_>>();
        chain.push(
            Path::new(next)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or(next)
                .to_string(),
        );
        chain.join(" -> ")
    }

    pub fn load_module(
        &mut self,
        name: &str,
        base_dir: &str,
        loading: &mut Vec<String>,
    ) -> Result<Value, RuntimeError> {
        let path = find_module_file(name, base_dir).ok_or_else(|| {
            RuntimeError::Error(format!("Module '{}' not found in '{}'", name, base_dir))
        })?;
        let cache_key = Self::module_cache_key(&path);

        if let Some(cached) = self.module_cache.get(&cache_key) {
            return Ok(cached.clone());
        }

        if loading.contains(&cache_key) {
            return Err(RuntimeError::Error(format!(
                "Circular import: {}",
                Self::cycle_display(loading, &cache_key)
            )));
        }

        loading.push(cache_key.clone());
        let result = (|| -> Result<Value, RuntimeError> {
            let src = std::fs::read_to_string(&path).map_err(|e| {
                RuntimeError::Error(format!("Cannot read '{}': {}", path.display(), e))
            })?;
            let items = parse_source(&src).map_err(|e| {
                RuntimeError::Error(format!("Parse error in '{}': {}", path.display(), e))
            })?;

            if let Some(module) = Self::module_decl(&items) {
                let expected = name.rsplit('.').next().unwrap_or(name);
                if module.name != expected {
                    return Err(RuntimeError::Error(format!(
                        "Module name mismatch: expected '{}' (from '{}'), found '{}' in '{}'",
                        expected,
                        name,
                        module.name,
                        path.display()
                    )));
                }
            }

            let mut sub = Interpreter::new();

            if let Some(module) = Self::module_decl(&items) {
                for dep_name in &module.depends {
                    let dep_ns = self.load_module(dep_name, base_dir, loading)?;
                    sub.define_module_path(dep_name, dep_ns)?;
                }
            }

            for item in &items {
                if let TopLevel::TypeDef(td) = item {
                    sub.register_type_def(td);
                }
            }
            for item in &items {
                if let TopLevel::FnDef(fd) = item {
                    sub.exec_fn_def(fd)?;
                }
            }

            let exposed = Self::exposed_set(&items);
            let mut members = HashMap::new();
            for item in &items {
                if let TopLevel::FnDef(fd) = item {
                    let include = match &exposed {
                        Some(set) => set.contains(&fd.name),
                        None => !fd.name.starts_with('_'),
                    };
                    if include {
                        let val = sub.lookup(&fd.name).map_err(|_| {
                            RuntimeError::Error(format!("Failed to export '{}.{}'", name, fd.name))
                        })?;
                        members.insert(fd.name.clone(), val);
                    }
                }
            }

            Ok(Value::Namespace {
                name: name.to_string(),
                members,
            })
        })();
        loading.pop();

        match result {
            Ok(ns) => {
                self.module_cache.insert(cache_key, ns.clone());
                Ok(ns)
            }
            Err(e) => Err(e),
        }
    }

    // -------------------------------------------------------------------------
    // Builtins
    // -------------------------------------------------------------------------
    fn builtin_effects(name: &str) -> &'static [&'static str] {
        let e = console::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = http::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = http_server::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = disk::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = tcp::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = int::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = float::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = string::effects(name);
        if !e.is_empty() {
            return e;
        }
        list::effects(name)
    }

    fn current_allowed_effects(&self) -> &[String] {
        self.call_stack
            .last()
            .map(|frame| frame.effects.as_slice())
            .unwrap_or(&[])
    }

    fn runtime_chain_with(&self, callee_name: &str) -> String {
        let mut chain = if self.call_stack.is_empty() {
            vec!["<top-level>".to_string()]
        } else {
            self.call_stack
                .iter()
                .map(|frame| frame.name.clone())
                .collect()
        };
        chain.push(callee_name.to_string());
        chain.join(" -> ")
    }

    fn ensure_effects_allowed<'a, I>(
        &self,
        callee_name: &str,
        required_effects: I,
    ) -> Result<(), RuntimeError>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let allowed = self.current_allowed_effects();
        let mut missing = Vec::new();
        for effect in required_effects {
            if !allowed.iter().any(|e| e == effect) {
                missing.push(effect.to_string());
            }
        }

        if missing.is_empty() {
            return Ok(());
        }

        let caller = self
            .call_stack
            .last()
            .map(|frame| frame.name.as_str())
            .unwrap_or("<top-level>");
        let chain = self.runtime_chain_with(callee_name);
        Err(RuntimeError::Error(format!(
            "Runtime effect violation: '{}' cannot call '{}' (missing effect(s): {}) [chain: {}]",
            caller,
            callee_name,
            missing.join(", "),
            chain
        )))
    }

    fn http_server_listen(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.http_server_listen_internal(args, false)
    }

    fn http_server_listen_with(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.http_server_listen_internal(args, true)
    }

    fn http_server_listen_internal(
        &mut self,
        args: Vec<Value>,
        with_context: bool,
    ) -> Result<Value, RuntimeError> {
        let expected = if with_context { 3 } else { 2 };
        if args.len() != expected {
            let sig = if with_context {
                "HttpServer.listenWith(port, context, handler)"
            } else {
                "HttpServer.listen(port, handler)"
            };
            return Err(RuntimeError::Error(format!(
                "{} expects {} arguments, got {}",
                sig,
                expected,
                args.len()
            )));
        }

        let port = match &args[0] {
            Value::Int(n) if (0..=65535).contains(n) => *n as u16,
            Value::Int(n) => {
                return Err(RuntimeError::Error(format!(
                    "HttpServer.listen: port {} is out of range (0-65535)",
                    n
                )))
            }
            _ => {
                return Err(RuntimeError::Error(
                    "HttpServer.listen: port must be an Int".to_string(),
                ))
            }
        };
        let (context, handler) = if with_context {
            (Some(args[1].clone()), args[2].clone())
        } else {
            (None, args[1].clone())
        };

        let listener = TcpListener::bind(("0.0.0.0", port)).map_err(|e| {
            RuntimeError::Error(format!(
                "HttpServer.listen: failed to bind on {}: {}",
                port, e
            ))
        })?;

        for incoming in listener.incoming() {
            let mut stream = match incoming {
                Ok(s) => s,
                Err(e) => {
                    return Err(RuntimeError::Error(format!(
                        "HttpServer.listen: failed to accept connection: {}",
                        e
                    )));
                }
            };

            if let Err(e) = stream.set_read_timeout(Some(Duration::from_secs(30))) {
                let _ = Self::write_http_response(
                    &mut stream,
                    &ServerResponse {
                        status: 500,
                        body: format!("HttpServer: failed to set read timeout: {}", e),
                        headers: vec![],
                    },
                );
                continue;
            }
            if let Err(e) = stream.set_write_timeout(Some(Duration::from_secs(30))) {
                let _ = Self::write_http_response(
                    &mut stream,
                    &ServerResponse {
                        status: 500,
                        body: format!("HttpServer: failed to set write timeout: {}", e),
                        headers: vec![],
                    },
                );
                continue;
            }

            let request = match Self::parse_http_request(&mut stream) {
                Ok(req) => req,
                Err(msg) => {
                    let _ = Self::write_http_response(
                        &mut stream,
                        &ServerResponse {
                            status: 400,
                            body: format!("Bad Request: {}", msg),
                            headers: vec![],
                        },
                    );
                    continue;
                }
            };

            let request_value = Self::http_request_to_value(&request);
            let callback_effects = Self::callable_declared_effects(&handler);
            let callback_entry = format!("<HttpServer {} {}>", request.method, request.path);
            let callback_args = match &context {
                Some(ctx) => vec![ctx.clone(), request_value],
                None => vec![request_value],
            };
            let callback_result = self.call_value_with_effects_pub(
                handler.clone(),
                callback_args,
                &callback_entry,
                callback_effects,
            );

            let response = match callback_result {
                Ok(value) => match Self::http_response_from_value(value) {
                    Ok(resp) => resp,
                    Err(e) => ServerResponse {
                        status: 500,
                        body: format!("HttpServer handler return error: {}", e),
                        headers: vec![],
                    },
                },
                Err(e) => ServerResponse {
                    status: 500,
                    body: format!("HttpServer handler execution error: {}", e),
                    headers: vec![],
                },
            };

            let _ = Self::write_http_response(&mut stream, &response);
        }

        Ok(Value::Unit)
    }

    fn parse_http_request(stream: &mut TcpStream) -> Result<ServerRequest, String> {
        const BODY_LIMIT: usize = 10 * 1024 * 1024; // 10 MB

        let reader_stream = stream
            .try_clone()
            .map_err(|e| format!("cannot clone TCP stream: {}", e))?;
        let mut reader = BufReader::new(reader_stream);

        let mut request_line = String::new();
        let line_len = reader
            .read_line(&mut request_line)
            .map_err(|e| format!("cannot read request line: {}", e))?;
        if line_len == 0 {
            return Err("empty request".to_string());
        }

        let request_line = request_line.trim_end_matches(&['\r', '\n'][..]);
        let mut request_parts = request_line.split_whitespace();
        let method = request_parts
            .next()
            .ok_or_else(|| "missing HTTP method".to_string())?
            .to_string();
        let path = request_parts
            .next()
            .ok_or_else(|| "missing request path".to_string())?
            .to_string();
        let _version = request_parts
            .next()
            .ok_or_else(|| "missing HTTP version".to_string())?;

        let mut headers = Vec::new();
        let mut content_length = 0usize;

        loop {
            let mut line = String::new();
            let bytes = reader
                .read_line(&mut line)
                .map_err(|e| format!("cannot read header line: {}", e))?;
            if bytes == 0 {
                break;
            }

            let trimmed = line.trim_end_matches(&['\r', '\n'][..]);
            if trimmed.is_empty() {
                break;
            }

            let (name, value) = trimmed
                .split_once(':')
                .ok_or_else(|| format!("malformed header: '{}'", trimmed))?;
            let name = name.trim().to_string();
            let value = value.trim().to_string();

            if name.eq_ignore_ascii_case("Content-Length") {
                content_length = value
                    .parse::<usize>()
                    .map_err(|_| format!("invalid Content-Length value: '{}'", value))?;
                if content_length > BODY_LIMIT {
                    return Err(format!("request body exceeds {} bytes limit", BODY_LIMIT));
                }
            }

            headers.push((name, value));
        }

        let mut body_bytes = vec![0_u8; content_length];
        if content_length > 0 {
            reader
                .read_exact(&mut body_bytes)
                .map_err(|e| format!("cannot read request body: {}", e))?;
        }
        let body = String::from_utf8_lossy(&body_bytes).into_owned();

        Ok(ServerRequest {
            method,
            path,
            body,
            headers,
        })
    }

    fn http_request_to_value(req: &ServerRequest) -> Value {
        let headers = req
            .headers
            .iter()
            .map(|(name, value)| Value::Record {
                type_name: "Header".to_string(),
                fields: vec![
                    ("name".to_string(), Value::Str(name.clone())),
                    ("value".to_string(), Value::Str(value.clone())),
                ],
            })
            .collect::<Vec<_>>();

        Value::Record {
            type_name: "HttpRequest".to_string(),
            fields: vec![
                ("method".to_string(), Value::Str(req.method.clone())),
                ("path".to_string(), Value::Str(req.path.clone())),
                ("body".to_string(), Value::Str(req.body.clone())),
                ("headers".to_string(), Value::List(headers)),
            ],
        }
    }

    fn http_response_from_value(val: Value) -> Result<ServerResponse, RuntimeError> {
        let (type_name, fields) = match val {
            Value::Record { type_name, fields } => (type_name, fields),
            _ => {
                return Err(RuntimeError::Error(
                    "HttpServer handler must return HttpResponse record".to_string(),
                ))
            }
        };

        if type_name != "HttpResponse" {
            return Err(RuntimeError::Error(format!(
                "HttpServer handler must return HttpResponse, got {}",
                type_name
            )));
        }

        let mut status = None;
        let mut body = None;
        let mut headers = Vec::new();

        for (name, value) in fields {
            match name.as_str() {
                "status" => {
                    if let Value::Int(n) = value {
                        status = Some(n);
                    } else {
                        return Err(RuntimeError::Error(
                            "HttpResponse.status must be Int".to_string(),
                        ));
                    }
                }
                "body" => {
                    if let Value::Str(s) = value {
                        body = Some(s);
                    } else {
                        return Err(RuntimeError::Error(
                            "HttpResponse.body must be String".to_string(),
                        ));
                    }
                }
                "headers" => {
                    headers = Self::parse_http_response_headers(value)?;
                }
                _ => {}
            }
        }

        Ok(ServerResponse {
            status: status.ok_or_else(|| {
                RuntimeError::Error("HttpResponse.status is required".to_string())
            })?,
            body: body
                .ok_or_else(|| RuntimeError::Error("HttpResponse.body is required".to_string()))?,
            headers,
        })
    }

    fn parse_http_response_headers(val: Value) -> Result<Vec<(String, String)>, RuntimeError> {
        let list = match val {
            Value::List(items) => items,
            _ => {
                return Err(RuntimeError::Error(
                    "HttpResponse.headers must be List<Header>".to_string(),
                ))
            }
        };

        let mut out = Vec::new();
        for item in list {
            let fields = match item {
                Value::Record { fields, .. } => fields,
                _ => {
                    return Err(RuntimeError::Error(
                        "HttpResponse.headers entries must be Header records".to_string(),
                    ))
                }
            };

            let mut name = None;
            let mut value = None;
            for (field_name, field_val) in fields {
                match (field_name.as_str(), field_val) {
                    ("name", Value::Str(s)) => name = Some(s),
                    ("value", Value::Str(s)) => value = Some(s),
                    _ => {}
                }
            }

            let name = name.ok_or_else(|| {
                RuntimeError::Error("HttpResponse header missing String 'name'".to_string())
            })?;
            let value = value.ok_or_else(|| {
                RuntimeError::Error("HttpResponse header missing String 'value'".to_string())
            })?;
            out.push((name, value));
        }

        Ok(out)
    }

    fn status_reason(status: i64) -> &'static str {
        match status {
            200 => "OK",
            201 => "Created",
            204 => "No Content",
            301 => "Moved Permanently",
            302 => "Found",
            304 => "Not Modified",
            400 => "Bad Request",
            401 => "Unauthorized",
            403 => "Forbidden",
            404 => "Not Found",
            405 => "Method Not Allowed",
            409 => "Conflict",
            422 => "Unprocessable Entity",
            429 => "Too Many Requests",
            500 => "Internal Server Error",
            501 => "Not Implemented",
            502 => "Bad Gateway",
            503 => "Service Unavailable",
            _ => "OK",
        }
    }

    fn write_http_response(
        stream: &mut TcpStream,
        response: &ServerResponse,
    ) -> std::io::Result<()> {
        let mut headers = response
            .headers
            .iter()
            .filter(|(name, _)| {
                !name.eq_ignore_ascii_case("Content-Length")
                    && !name.eq_ignore_ascii_case("Connection")
            })
            .cloned()
            .collect::<Vec<_>>();

        if !headers
            .iter()
            .any(|(name, _)| name.eq_ignore_ascii_case("Content-Type"))
        {
            headers.push((
                "Content-Type".to_string(),
                "text/plain; charset=utf-8".to_string(),
            ));
        }

        headers.push((
            "Content-Length".to_string(),
            response.body.as_bytes().len().to_string(),
        ));
        headers.push(("Connection".to_string(), "close".to_string()));

        let mut head = format!(
            "HTTP/1.1 {} {}\r\n",
            response.status,
            Self::status_reason(response.status)
        );
        for (name, value) in headers {
            head.push_str(&format!("{}: {}\r\n", name, value));
        }
        head.push_str("\r\n");

        stream.write_all(head.as_bytes())?;
        stream.write_all(response.body.as_bytes())?;
        stream.flush()?;
        Ok(())
    }

    fn call_builtin(&mut self, name: &str, mut args: Vec<Value>) -> Result<Value, RuntimeError> {
        match name {
            "__ctor:Result.Ok" => {
                if args.len() != 1 {
                    return Err(RuntimeError::Error(format!(
                        "Result.Ok() takes 1 argument, got {}",
                        args.len()
                    )));
                }
                return Ok(Value::Ok(Box::new(args.remove(0))));
            }
            "__ctor:Result.Err" => {
                if args.len() != 1 {
                    return Err(RuntimeError::Error(format!(
                        "Result.Err() takes 1 argument, got {}",
                        args.len()
                    )));
                }
                return Ok(Value::Err(Box::new(args.remove(0))));
            }
            "__ctor:Option.Some" => {
                if args.len() != 1 {
                    return Err(RuntimeError::Error(format!(
                        "Option.Some() takes 1 argument, got {}",
                        args.len()
                    )));
                }
                return Ok(Value::Some(Box::new(args.remove(0))));
            }
            "List.map" => {
                if args.len() != 2 {
                    return Err(RuntimeError::Error(format!(
                        "List.map() takes 2 arguments (list, fn), got {}",
                        args.len()
                    )));
                }
                let list = match args[0].clone() {
                    Value::List(items) => items,
                    _ => {
                        return Err(RuntimeError::Error(
                            "List.map() first argument must be a List".to_string(),
                        ))
                    }
                };
                let func = args[1].clone();
                let mut result = Vec::new();
                for item in list {
                    let val = self.call_value(func.clone(), vec![item])?;
                    result.push(val);
                }
                Ok(Value::List(result))
            }
            "List.filter" => {
                if args.len() != 2 {
                    return Err(RuntimeError::Error(format!(
                        "List.filter() takes 2 arguments (list, fn), got {}",
                        args.len()
                    )));
                }
                let list = match args[0].clone() {
                    Value::List(items) => items,
                    _ => {
                        return Err(RuntimeError::Error(
                            "List.filter() first argument must be a List".to_string(),
                        ))
                    }
                };
                let func = args[1].clone();
                let mut result = Vec::new();
                for item in list {
                    let keep = self.call_value(func.clone(), vec![item.clone()])?;
                    match keep {
                        Value::Bool(true) => result.push(item),
                        Value::Bool(false) => {}
                        _ => {
                            return Err(RuntimeError::Error(
                                "List.filter() predicate must return Bool".to_string(),
                            ))
                        }
                    }
                }
                Ok(Value::List(result))
            }
            "List.fold" => {
                if args.len() != 3 {
                    return Err(RuntimeError::Error(format!(
                        "List.fold() takes 3 arguments (list, init, fn), got {}",
                        args.len()
                    )));
                }
                let list = match args[0].clone() {
                    Value::List(items) => items,
                    _ => {
                        return Err(RuntimeError::Error(
                            "List.fold() first argument must be a List".to_string(),
                        ))
                    }
                };
                let init = args[1].clone();
                let func = args[2].clone();
                let mut acc = init;
                for item in list {
                    acc = self.call_value(func.clone(), vec![acc, item])?;
                }
                Ok(acc)
            }
            name if name.starts_with("__ctor:") => {
                // Format: __ctor:TypeName:VariantName
                let parts: Vec<&str> = name.splitn(3, ':').collect();
                let type_name = parts.get(1).copied().unwrap_or("").to_string();
                let variant = parts.get(2).copied().unwrap_or("").to_string();
                Ok(Value::Variant {
                    type_name,
                    variant,
                    fields: args,
                })
            }

            "Disk.makeDir" => {
                let [path_val] = args.as_slice() else {
                    return Err(RuntimeError::Error(format!(
                        "Disk.makeDir() takes 1 argument (path), got {}",
                        args.len()
                    )));
                };
                let Value::Str(path) = path_val else {
                    return Err(RuntimeError::Error(
                        "Disk.makeDir: path must be a String".to_string(),
                    ));
                };
                match std::fs::create_dir_all(path) {
                    Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
                    Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
                }
            }
            "HttpServer.listen" => self.http_server_listen(args),
            "HttpServer.listenWith" => self.http_server_listen_with(args),

            _ => {
                if let Some(r) = console::call(name, args.clone()) {
                    return r;
                }
                if let Some(r) = http::call(name, args.clone()) {
                    return r;
                }
                if let Some(r) = http_server::call(name, args.clone()) {
                    return r;
                }
                if let Some(r) = disk::call(name, args.clone()) {
                    return r;
                }
                if let Some(r) = tcp::call(name, args.clone()) {
                    return r;
                }
                if let Some(r) = int::call(name, args.clone()) {
                    return r;
                }
                if let Some(r) = float::call(name, args.clone()) {
                    return r;
                }
                if let Some(r) = string::call(name, args.clone()) {
                    return r;
                }
                if let Some(r) = list::call(name, args) {
                    return r;
                }
                Err(RuntimeError::Error(format!(
                    "Unknown builtin function: '{}'",
                    name
                )))
            }
        }
    }

    // -------------------------------------------------------------------------
    // Top-level execution
    // -------------------------------------------------------------------------
    #[allow(dead_code)]
    pub fn exec_items(&mut self, items: &[TopLevel]) -> Result<Value, RuntimeError> {
        for item in items {
            match item {
                TopLevel::FnDef(fd) => self.exec_fn_def(fd)?,
                TopLevel::Module(_) => {}
                TopLevel::Verify(_) => {}
                TopLevel::Decision(_) => {}
                TopLevel::TypeDef(td) => self.register_type_def(td),
                TopLevel::EffectSet { name, effects } => {
                    self.register_effect_set(name.clone(), effects.clone());
                }
                TopLevel::Stmt(s) => {
                    self.exec_stmt(s)?;
                }
            }
        }
        Ok(Value::Unit)
    }

    /// Register a user-defined type: sum type variants and record constructors.
    pub fn register_type_def(&mut self, td: &TypeDef) {
        match td {
            TypeDef::Sum {
                name: type_name,
                variants,
            } => {
                let mut members = HashMap::new();
                for variant in variants {
                    if variant.fields.is_empty() {
                        // Zero-arg variant: stored directly as a Value
                        members.insert(
                            variant.name.clone(),
                            Value::Variant {
                                type_name: type_name.clone(),
                                variant: variant.name.clone(),
                                fields: vec![],
                            },
                        );
                    } else {
                        // Constructor function
                        members.insert(
                            variant.name.clone(),
                            Value::Builtin(format!("__ctor:{}:{}", type_name, variant.name)),
                        );
                    }
                }
                self.define(
                    type_name.clone(),
                    Value::Namespace {
                        name: type_name.clone(),
                        members,
                    },
                );
            }
            TypeDef::Product { .. } => {
                // Product types are constructed via Expr::RecordCreate — no env registration needed
            }
        }
    }

    pub fn exec_fn_def(&mut self, fd: &FnDef) -> Result<(), RuntimeError> {
        // Capture current closure
        let mut closure = HashMap::new();
        for scope in &self.env {
            for (k, v) in scope {
                closure.insert(k.clone(), v.clone());
            }
        }

        let val = Value::Fn {
            name: fd.name.clone(),
            params: fd.params.clone(),
            effects: self.expand_effects(&fd.effects),
            body: fd.body.clone(),
            closure,
        };
        self.define(fd.name.clone(), val);
        Ok(())
    }

    pub fn exec_stmt(&mut self, stmt: &Stmt) -> Result<Value, RuntimeError> {
        match stmt {
            Stmt::Val(name, expr) => {
                let val = self.eval_expr(expr)?;
                self.define(name.clone(), val);
                Ok(Value::Unit)
            }
            Stmt::Var(name, expr, _reason) => {
                let val = self.eval_expr(expr)?;
                self.define(name.clone(), val);
                Ok(Value::Unit)
            }
            Stmt::Assign(name, expr) => {
                let val = self.eval_expr(expr)?;
                self.assign(name, val)?;
                Ok(Value::Unit)
            }
            Stmt::Expr(expr) => self.eval_expr(expr),
        }
    }

    pub fn exec_body(&mut self, stmts: &[Stmt]) -> Result<Value, RuntimeError> {
        let mut last = Value::Unit;
        for stmt in stmts {
            last = self.exec_stmt(stmt)?;
        }
        Ok(last)
    }

    // -------------------------------------------------------------------------
    // Expression evaluation
    // -------------------------------------------------------------------------
    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal(lit) => Ok(self.eval_literal(lit)),
            Expr::Ident(name) => self.lookup(name),
            Expr::Attr(obj, field) => {
                let obj_val = self.eval_expr(obj)?;
                match obj_val {
                    Value::Record { fields, .. } => fields
                        .into_iter()
                        .find(|(k, _)| k == field)
                        .map(|(_, v)| Ok(v))
                        .unwrap_or_else(|| {
                            Err(RuntimeError::Error(format!("Unknown field '{}'", field)))
                        }),
                    Value::Namespace { name, members } => {
                        members.get(field).cloned().ok_or_else(|| {
                            RuntimeError::Error(format!("Unknown member '{}.{}'", name, field))
                        })
                    }
                    _ => Err(RuntimeError::Error(format!(
                        "Field access '{}' is not supported on this value",
                        field
                    ))),
                }
            }
            Expr::FnCall(fn_expr, args) => {
                let fn_val = self.eval_expr(fn_expr)?;
                let mut arg_vals = Vec::new();
                for a in args {
                    arg_vals.push(self.eval_expr(a)?);
                }
                self.call_value(fn_val, arg_vals)
            }
            Expr::BinOp(op, left, right) => {
                let lv = self.eval_expr(left)?;
                let rv = self.eval_expr(right)?;
                self.eval_binop(op, lv, rv)
            }
            Expr::Match(subject, arms) => {
                let sv = self.eval_expr(subject)?;
                self.eval_match(sv, arms)
            }
            Expr::Pipe(left, right) => {
                let left_val = self.eval_expr(left)?;
                let fn_val = self.eval_expr(right)?;
                self.call_value(fn_val, vec![left_val])
            }
            Expr::Constructor(name, arg) => {
                let arg_val = match arg {
                    Some(a) => self.eval_expr(a)?,
                    None => Value::Unit,
                };
                match name.as_str() {
                    "Ok" => Ok(Value::Ok(Box::new(arg_val))),
                    "Err" => Ok(Value::Err(Box::new(arg_val))),
                    "Some" => Ok(Value::Some(Box::new(arg_val))),
                    "None" => Ok(Value::None),
                    _ => Err(RuntimeError::Error(format!(
                        "Unknown constructor: {}",
                        name
                    ))),
                }
            }
            Expr::ErrorProp(inner) => {
                let val = self.eval_expr(inner)?;
                match val {
                    Value::Ok(v) => Ok(*v),
                    Value::Err(e) => Err(RuntimeError::ErrProp(e)),
                    _ => Err(RuntimeError::Error(
                        "Operator '?' can only be applied to Result".to_string(),
                    )),
                }
            }
            Expr::InterpolatedStr(parts) => {
                let mut result = String::new();
                for part in parts {
                    match part {
                        StrPart::Literal(s) => result.push_str(s),
                        StrPart::Expr(src) => {
                            // Re-lex and parse the expression source for string interpolation
                            let mut lexer = Lexer::new(src);
                            let tokens = lexer.tokenize().map_err(|e| {
                                RuntimeError::Error(format!("Error in interpolation: {}", e))
                            })?;
                            let mut parser = Parser::new(tokens);
                            let expr_node = parser.parse_expr().map_err(|e| {
                                RuntimeError::Error(format!("Error in interpolation: {}", e))
                            })?;
                            let val = self.eval_expr(&expr_node)?;
                            result.push_str(&aver_repr(&val));
                        }
                    }
                }
                Ok(Value::Str(result))
            }
            Expr::List(elements) => {
                let mut values = Vec::new();
                for elem in elements {
                    values.push(self.eval_expr(elem)?);
                }
                Ok(Value::List(values))
            }
            Expr::TypeAscription(inner, _) => self.eval_expr(inner),
            Expr::RecordCreate { type_name, fields } => {
                let mut field_vals = Vec::new();
                for (name, expr) in fields {
                    let val = self.eval_expr(expr)?;
                    field_vals.push((name.clone(), val));
                }
                Ok(Value::Record {
                    type_name: type_name.clone(),
                    fields: field_vals,
                })
            }
        }
    }

    fn eval_literal(&self, lit: &Literal) -> Value {
        match lit {
            Literal::Int(i) => Value::Int(*i),
            Literal::Float(f) => Value::Float(*f),
            Literal::Str(s) => Value::Str(s.clone()),
            Literal::Bool(b) => Value::Bool(*b),
        }
    }

    fn call_value(&mut self, fn_val: Value, args: Vec<Value>) -> Result<Value, RuntimeError> {
        match fn_val {
            Value::Builtin(name) => {
                self.ensure_effects_allowed(&name, Self::builtin_effects(&name).iter().copied())?;
                self.call_builtin(&name, args)
            }
            Value::Fn {
                name,
                params,
                effects,
                body,
                closure,
            } => {
                if args.len() != params.len() {
                    return Err(RuntimeError::Error(format!(
                        "Function '{}' expects {} arguments, got {}",
                        name,
                        params.len(),
                        args.len()
                    )));
                }
                self.ensure_effects_allowed(&name, effects.iter().map(String::as_str))?;

                let mut new_env = closure;
                for ((param_name, _), arg_val) in params.iter().zip(args.into_iter()) {
                    new_env.insert(param_name.clone(), arg_val);
                }

                self.call_stack.push(CallFrame {
                    name: name.clone(),
                    effects,
                });
                self.push_env(new_env);
                let result = match &body {
                    FnBody::Expr(e) => self.eval_expr(e),
                    FnBody::Block(stmts) => self.exec_body(stmts),
                };
                self.pop_env();
                self.call_stack.pop();
                match result {
                    Ok(v) => Ok(v),
                    Err(RuntimeError::ErrProp(e)) => Ok(Value::Err(e)),
                    Err(e) => Err(e),
                }
            }
            _ => Err(RuntimeError::Error(format!(
                "Cannot call value: {:?}",
                fn_val
            ))),
        }
    }

    fn eval_binop(&self, op: &BinOp, left: Value, right: Value) -> Result<Value, RuntimeError> {
        match op {
            BinOp::Add => self.op_add(left, right),
            BinOp::Sub => self.op_sub(left, right),
            BinOp::Mul => self.op_mul(left, right),
            BinOp::Div => self.op_div(left, right),
            BinOp::Eq => Ok(Value::Bool(self.aver_eq(&left, &right))),
            BinOp::Neq => Ok(Value::Bool(!self.aver_eq(&left, &right))),
            BinOp::Lt => self.op_compare(&left, &right, "<"),
            BinOp::Gt => self.op_compare(&left, &right, ">"),
            BinOp::Lte => self.op_compare(&left, &right, "<="),
            BinOp::Gte => self.op_compare(&left, &right, ">="),
        }
    }

    pub fn aver_eq(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => x == y,
            (Value::Float(x), Value::Float(y)) => x == y,
            (Value::Str(x), Value::Str(y)) => x == y,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            (Value::Unit, Value::Unit) => true,
            (Value::None, Value::None) => true,
            (Value::Ok(x), Value::Ok(y)) => self.aver_eq(x, y),
            (Value::Err(x), Value::Err(y)) => self.aver_eq(x, y),
            (Value::Some(x), Value::Some(y)) => self.aver_eq(x, y),
            (Value::List(xs), Value::List(ys)) => {
                xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| self.aver_eq(x, y))
            }
            (
                Value::Variant {
                    type_name: t1,
                    variant: v1,
                    fields: f1,
                },
                Value::Variant {
                    type_name: t2,
                    variant: v2,
                    fields: f2,
                },
            ) => {
                t1 == t2
                    && v1 == v2
                    && f1.len() == f2.len()
                    && f1.iter().zip(f2.iter()).all(|(x, y)| self.aver_eq(x, y))
            }
            (
                Value::Record {
                    type_name: t1,
                    fields: f1,
                },
                Value::Record {
                    type_name: t2,
                    fields: f2,
                },
            ) => {
                t1 == t2
                    && f1.len() == f2.len()
                    && f1
                        .iter()
                        .zip(f2.iter())
                        .all(|((k1, v1), (k2, v2))| k1 == k2 && self.aver_eq(v1, v2))
            }
            _ => false,
        }
    }

    fn op_add(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        match (&a, &b) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
            (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 + y)),
            (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x + *y as f64)),
            (Value::Str(x), Value::Str(y)) => Ok(Value::Str(format!("{}{}", x, y))),
            _ => Err(RuntimeError::Error(
                "Operator '+' does not support these types".to_string(),
            )),
        }
    }

    fn op_sub(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        match (&a, &b) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x - y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x - y)),
            (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 - y)),
            (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x - *y as f64)),
            _ => Err(RuntimeError::Error(
                "Operator '-' does not support these types".to_string(),
            )),
        }
    }

    fn op_mul(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        match (&a, &b) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x * y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x * y)),
            (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 * y)),
            (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x * *y as f64)),
            _ => Err(RuntimeError::Error(
                "Operator '*' does not support these types".to_string(),
            )),
        }
    }

    fn op_div(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        match (&a, &b) {
            (Value::Int(x), Value::Int(y)) => {
                if *y == 0 {
                    Err(RuntimeError::Error("Division by zero".to_string()))
                } else {
                    Ok(Value::Int(x / y))
                }
            }
            (Value::Float(x), Value::Float(y)) => {
                if *y == 0.0 {
                    Err(RuntimeError::Error("Division by zero".to_string()))
                } else {
                    Ok(Value::Float(x / y))
                }
            }
            (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 / y)),
            (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x / *y as f64)),
            _ => Err(RuntimeError::Error(
                "Operator '/' does not support these types".to_string(),
            )),
        }
    }

    fn op_compare(&self, a: &Value, b: &Value, op: &str) -> Result<Value, RuntimeError> {
        let result = match (a, b) {
            (Value::Int(x), Value::Int(y)) => match op {
                "<" => x < y,
                ">" => x > y,
                "<=" => x <= y,
                ">=" => x >= y,
                _ => unreachable!(),
            },
            (Value::Float(x), Value::Float(y)) => match op {
                "<" => x < y,
                ">" => x > y,
                "<=" => x <= y,
                ">=" => x >= y,
                _ => unreachable!(),
            },
            (Value::Int(x), Value::Float(y)) => {
                let x = *x as f64;
                match op {
                    "<" => x < *y,
                    ">" => x > *y,
                    "<=" => x <= *y,
                    ">=" => x >= *y,
                    _ => unreachable!(),
                }
            }
            (Value::Float(x), Value::Int(y)) => {
                let y = *y as f64;
                match op {
                    "<" => *x < y,
                    ">" => *x > y,
                    "<=" => *x <= y,
                    ">=" => *x >= y,
                    _ => unreachable!(),
                }
            }
            (Value::Str(x), Value::Str(y)) => match op {
                "<" => x < y,
                ">" => x > y,
                "<=" => x <= y,
                ">=" => x >= y,
                _ => unreachable!(),
            },
            _ => {
                return Err(RuntimeError::Error(format!(
                    "Operator '{}' does not support these types",
                    op
                )))
            }
        };
        Ok(Value::Bool(result))
    }

    fn eval_match(&mut self, subject: Value, arms: &[MatchArm]) -> Result<Value, RuntimeError> {
        for arm in arms {
            if let Some(bindings) = self.match_pattern(&arm.pattern, &subject) {
                // Create new scope with pattern bindings
                let mut new_scope = HashMap::new();
                for (k, v) in bindings {
                    new_scope.insert(k, v);
                }
                self.push_env(new_scope);
                let result = self.eval_expr(&arm.body);
                self.pop_env();
                return result;
            }
        }
        Err(RuntimeError::Error(format!(
            "No match found for value {}",
            aver_repr(&subject)
        )))
    }

    fn match_pattern(&self, pattern: &Pattern, value: &Value) -> Option<HashMap<String, Value>> {
        match pattern {
            Pattern::Wildcard => Some(HashMap::new()),
            Pattern::Literal(lit) => {
                let matches = match (lit, value) {
                    (Literal::Int(i), Value::Int(v)) => i == v,
                    (Literal::Float(f), Value::Float(v)) => f == v,
                    (Literal::Str(s), Value::Str(v)) => s == v,
                    (Literal::Bool(b), Value::Bool(v)) => b == v,
                    _ => false,
                };
                if matches {
                    Some(HashMap::new())
                } else {
                    None
                }
            }
            Pattern::Ident(name) => {
                let mut bindings = HashMap::new();
                bindings.insert(name.clone(), value.clone());
                Some(bindings)
            }
            Pattern::EmptyList => match value {
                Value::List(items) if items.is_empty() => Some(HashMap::new()),
                _ => None,
            },
            Pattern::Cons(head, tail) => match value {
                Value::List(items) if !items.is_empty() => {
                    let mut map = HashMap::new();
                    if head != "_" {
                        map.insert(head.clone(), items[0].clone());
                    }
                    if tail != "_" {
                        map.insert(tail.clone(), Value::List(items[1..].to_vec()));
                    }
                    Some(map)
                }
                _ => None,
            },
            Pattern::Constructor(ctor_name, bindings) => {
                match (ctor_name.as_str(), value) {
                    ("Option.None", Value::None) => Some(HashMap::new()),
                    ("Result.Ok", Value::Ok(inner)) => {
                        let mut map = HashMap::new();
                        if let Some(name) = bindings.first() {
                            if name != "_" {
                                map.insert(name.clone(), *inner.clone());
                            }
                        }
                        Some(map)
                    }
                    ("Result.Err", Value::Err(inner)) => {
                        let mut map = HashMap::new();
                        if let Some(name) = bindings.first() {
                            if name != "_" {
                                map.insert(name.clone(), *inner.clone());
                            }
                        }
                        Some(map)
                    }
                    ("Option.Some", Value::Some(inner)) => {
                        let mut map = HashMap::new();
                        if let Some(name) = bindings.first() {
                            if name != "_" {
                                map.insert(name.clone(), *inner.clone());
                            }
                        }
                        Some(map)
                    }
                    // User-defined variant: match by variant name, qualified or unqualified
                    (
                        ctor,
                        Value::Variant {
                            type_name,
                            variant,
                            fields,
                            ..
                        },
                    ) => {
                        let matches = if ctor.contains('.') {
                            // Qualified: "Shape.Circle"
                            let mut parts = ctor.splitn(2, '.');
                            parts.next().map_or(false, |t| t == type_name)
                                && parts.next().map_or(false, |v| v == variant)
                        } else {
                            // Unqualified (builtins like Ok/Err handled above; this catches
                            // any legacy unqualified user-defined patterns)
                            ctor == variant
                        };
                        if !matches {
                            return None;
                        }
                        if !bindings.is_empty() && bindings.len() != fields.len() {
                            return None;
                        }
                        let mut map = HashMap::new();
                        for (name, val) in bindings.iter().zip(fields.iter()) {
                            if name != "_" {
                                map.insert(name.clone(), val.clone());
                            }
                        }
                        Some(map)
                    }
                    // Record destructuring: positional, matched by type_name
                    (
                        ctor,
                        Value::Record {
                            type_name,
                            fields: rf,
                            ..
                        },
                    ) if ctor == type_name => {
                        if !bindings.is_empty() && bindings.len() != rf.len() {
                            return None;
                        }
                        let mut map = HashMap::new();
                        for (name, (_, val)) in bindings.iter().zip(rf.iter()) {
                            if name != "_" {
                                map.insert(name.clone(), val.clone());
                            }
                        }
                        Some(map)
                    }
                    _ => None,
                }
            }
        }
    }

    pub fn callable_declared_effects(fn_val: &Value) -> Vec<String> {
        match fn_val {
            Value::Fn { effects, .. } => effects.clone(),
            _ => vec![],
        }
    }

    // Public wrapper so main.rs can call call_value
    pub fn call_value_pub(
        &mut self,
        fn_val: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.call_value(fn_val, args)
    }

    pub fn call_value_with_effects_pub(
        &mut self,
        fn_val: Value,
        args: Vec<Value>,
        entry_name: &str,
        allowed_effects: Vec<String>,
    ) -> Result<Value, RuntimeError> {
        self.call_stack.push(CallFrame {
            name: entry_name.to_string(),
            effects: allowed_effects,
        });
        let result = self.call_value(fn_val, args);
        self.call_stack.pop();
        result
    }

    // -------------------------------------------------------------------------
    // Run a complete source file
    // -------------------------------------------------------------------------
    #[allow(dead_code)]
    pub fn run_file(&mut self, source: &str) -> Result<Value, RuntimeError> {
        let items = parse_source(source).map_err(RuntimeError::Error)?;

        // First pass: register all top-level definitions
        for item in &items {
            match item {
                TopLevel::FnDef(fd) => self.exec_fn_def(fd)?,
                TopLevel::Stmt(s) => {
                    self.exec_stmt(s)?;
                }
                _ => {}
            }
        }

        // Second pass: run main() if it exists
        let main_fn = self.lookup("main");
        if let Ok(fn_val) = main_fn {
            let allowed = Self::callable_declared_effects(&fn_val);
            self.call_value_with_effects_pub(fn_val, vec![], "<main>", allowed)?;
        }

        Ok(Value::Unit)
    }
}
