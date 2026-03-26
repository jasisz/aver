use std::collections::HashMap;
use std::sync::Mutex;

use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::*;
use tower_lsp_server::{Client, LanguageServer};

use crate::completion;
use crate::definition;
use crate::diagnostics;
use crate::hover as hover_mod;
use crate::lenses;
use crate::modules;
use crate::position::utf16_col_to_byte_idx;
use crate::signature;
use crate::symbols;

pub struct AverBackend {
    client: Client,
    /// Cache of document contents, keyed by URI string.
    documents: Mutex<HashMap<String, String>>,
    /// Workspace root path (from initialize params). Used as module_root for `find_module_file`.
    root_dir: Mutex<Option<String>>,
}

impl AverBackend {
    pub fn new(client: Client) -> Self {
        AverBackend {
            client,
            documents: Mutex::new(HashMap::new()),
            root_dir: Mutex::new(None),
        }
    }

    fn get_base_dir(&self, uri: &Uri) -> Option<String> {
        // Prefer workspace root (where the project lives), fall back to file's parent dir
        let root = self.root_dir.lock().unwrap().clone();
        root.or_else(|| modules::base_dir_from_uri(uri))
    }

    async fn publish_diagnostics_for(&self, uri: &Uri, source: &str) {
        let base_dir = self.get_base_dir(uri);
        let diags = diagnostics::diagnose(source, base_dir.as_deref());
        self.client
            .publish_diagnostics(uri.clone(), diags, None)
            .await;
    }
}

impl LanguageServer for AverBackend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Extract workspace root for module resolution
        let root = params
            .workspace_folders
            .as_ref()
            .and_then(|folders| folders.first())
            .and_then(|folder| folder.uri.to_file_path())
            .map(|p| p.to_string_lossy().to_string())
            .or_else(|| {
                // Fallback: deprecated root_uri
                #[allow(deprecated)]
                params
                    .root_uri
                    .as_ref()
                    .and_then(|uri| uri.to_file_path())
                    .map(|p| p.to_string_lossy().to_string())
            });

        if let Some(ref dir) = root {
            *self.root_dir.lock().unwrap() = Some(dir.clone());
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                document_symbol_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(false),
                }),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string()]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: Default::default(),
                }),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        let root = self.root_dir.lock().unwrap().clone();
        self.client
            .log_message(
                MessageType::INFO,
                format!("Aver LSP initialized, root_dir={:?}", root),
            )
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        {
            let mut docs = self.documents.lock().unwrap();
            docs.insert(uri.to_string(), text.clone());
        }
        self.publish_diagnostics_for(&uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        // FULL sync: last change contains the entire document
        if let Some(change) = params.content_changes.into_iter().last() {
            let text = change.text;
            {
                let mut docs = self.documents.lock().unwrap();
                docs.insert(uri.to_string(), text.clone());
            }
            self.publish_diagnostics_for(&uri, &text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        {
            let mut docs = self.documents.lock().unwrap();
            docs.remove(&uri.to_string());
        }
        // Clear diagnostics for closed file
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let source = {
            let docs = self.documents.lock().unwrap();
            docs.get(&uri.to_string()).cloned().unwrap_or_default()
        };

        let base_dir = self.get_base_dir(&uri);

        // Try to figure out if we're completing after a dot (namespace member)
        let line_text = source.lines().nth(position.line as usize).unwrap_or("");
        let byte_col = utf16_col_to_byte_idx(line_text, position.character);
        let before_cursor = &line_text[..byte_col];

        // Check for namespace.member pattern
        if let Some(dot_pos) = before_cursor.rfind('.') {
            let before_dot = &before_cursor[..dot_pos];
            // Extract the namespace name (last word before the dot)
            let namespace = before_dot
                .rsplit(|c: char| !c.is_alphanumeric() && c != '_')
                .next()
                .unwrap_or("");

            if !namespace.is_empty() {
                // Try built-in namespaces first
                let items = completion::namespace_completions(namespace);
                if !items.is_empty() {
                    return Ok(Some(CompletionResponse::Array(items)));
                }
                // Try user-defined types (Shape.Circle, User.update, etc.)
                let items = completion::user_type_completions(&source, namespace);
                if !items.is_empty() {
                    return Ok(Some(CompletionResponse::Array(items)));
                }
                // Try module dependencies (ModuleName.func)
                if let Some(base) = &base_dir {
                    let items = completion::module_completions(&source, namespace, base);
                    if !items.is_empty() {
                        return Ok(Some(CompletionResponse::Array(items)));
                    }
                }
            }
        }

        if inside_effect_list(&source, position.line as usize, position.character) {
            return Ok(Some(CompletionResponse::Array(
                completion::effect_completions(),
            )));
        }

        // Default: offer namespaces + user functions + module names
        let mut items = completion::all_namespaces();
        items.extend(completion::user_fn_completions(&source));

        // Add module names from depends as completion targets
        if let Some(base) = &base_dir {
            items.extend(completion::depends_module_completions(&source, base));
        }

        // Add keywords
        let keywords = [
            "fn", "match", "module", "depends", "exposes", "opaque", "intent", "verify",
            "decision", "type", "record", "law", "given", "true", "false",
        ];
        for kw in &keywords {
            items.push(CompletionItem {
                label: kw.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            });
        }

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        let source = {
            let docs = self.documents.lock().unwrap();
            docs.get(&uri.to_string()).cloned().unwrap_or_default()
        };

        Ok(symbols::document_symbols(&source))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        let source = {
            let docs = self.documents.lock().unwrap();
            docs.get(&uri.to_string()).cloned().unwrap_or_default()
        };

        Ok(format_document_edits(&source))
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = params.text_document.uri;
        let source = {
            let docs = self.documents.lock().unwrap();
            docs.get(&uri.to_string()).cloned().unwrap_or_default()
        };
        let base_dir = self.get_base_dir(&uri);

        Ok(Some(lenses::code_lenses(
            &source,
            base_dir.as_deref(),
            &uri.to_string(),
        )))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let source = {
            let docs = self.documents.lock().unwrap();
            docs.get(&uri.to_string()).cloned().unwrap_or_default()
        };

        let word = match hover_mod::word_at_position(
            &source,
            position.line as usize,
            position.character,
        ) {
            Some(w) => w,
            None => return Ok(None),
        };

        let base_dir = self.get_base_dir(&uri);
        Ok(definition::goto_definition(
            &word,
            &source,
            &uri,
            base_dir.as_deref(),
        ))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let source = {
            let docs = self.documents.lock().unwrap();
            docs.get(&uri.to_string()).cloned().unwrap_or_default()
        };

        let base_dir = self.get_base_dir(&uri);
        Ok(signature::signature_help(
            &source,
            position.line as usize,
            position.character,
            base_dir.as_deref(),
        ))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let source = {
            let docs = self.documents.lock().unwrap();
            docs.get(&uri.to_string()).cloned().unwrap_or_default()
        };

        let word = match hover_mod::word_at_position(
            &source,
            position.line as usize,
            position.character,
        ) {
            Some(w) => w,
            None => return Ok(None),
        };

        let base_dir = self.get_base_dir(&uri);
        Ok(hover_mod::hover_for_word(
            &word,
            &source,
            base_dir.as_deref(),
        ))
    }
}

fn format_document_edits(source: &str) -> Option<Vec<TextEdit>> {
    let formatted = aver::format::try_format_source(source).ok()?;
    if formatted == source {
        return None;
    }

    Some(vec![TextEdit {
        range: full_document_range(source),
        new_text: formatted,
    }])
}

fn full_document_range(source: &str) -> Range {
    let lines: Vec<&str> = source.split('\n').collect();
    let end_line = lines.len().saturating_sub(1) as u32;
    let end_character = lines
        .last()
        .map(|line| line.encode_utf16().count() as u32)
        .unwrap_or(0);

    Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: end_line,
            character: end_character,
        },
    }
}

fn inside_effect_list(source: &str, line: usize, character: u32) -> bool {
    let mut prefix = String::new();
    for (idx, current_line) in source.lines().enumerate() {
        if idx < line {
            prefix.push_str(current_line);
            prefix.push('\n');
            continue;
        }
        if idx == line {
            let byte_col = utf16_col_to_byte_idx(current_line, character);
            prefix.push_str(&current_line[..byte_col]);
        }
        break;
    }

    let bytes = prefix.as_bytes();
    let mut stack: Vec<bool> = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'!' {
            let mut j = i + 1;
            while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                j += 1;
            }
            if j < bytes.len() && bytes[j] == b'[' {
                stack.push(true);
                i = j + 1;
                continue;
            }
        }

        match bytes[i] {
            b'[' => stack.push(false),
            b']' => {
                let _ = stack.pop();
            }
            _ => {}
        }
        i += 1;
    }

    stack.last().copied().unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::{format_document_edits, full_document_range, inside_effect_list};

    #[test]
    fn detects_effect_list_context_across_lines() {
        let src = "fn log() -> Unit\n    ! [\n        Console.print,\n        Time.now\n";
        assert!(inside_effect_list(src, 2, 15));
    }

    #[test]
    fn ignores_normal_list_literals() {
        let src = "fn demo() -> List<Int>\n    xs = [1, 2, 3]\n    xs\n";
        assert!(!inside_effect_list(src, 1, 10));
    }

    #[test]
    fn formatting_returns_full_document_edit() {
        let src = "module Demo\r\nfn x() -> Int\r\n    1\t \r\n";
        let edits = format_document_edits(src).expect("expected formatting edits");
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "module Demo\n\nfn x() -> Int\n    1\n");
    }

    #[test]
    fn full_document_range_handles_trailing_newline() {
        let range = full_document_range("a\n");
        assert_eq!(range.start.line, 0);
        assert_eq!(range.end.line, 1);
        assert_eq!(range.end.character, 0);
    }
}
