use std::collections::HashMap;
use std::sync::Mutex;

use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::*;
use tower_lsp_server::{Client, LanguageServer};

use crate::completion;
use crate::definition;
use crate::diagnostics;
use crate::hover as hover_mod;
use crate::modules;
use crate::signature;

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
        let before_cursor = if (position.character as usize) <= line_text.len() {
            &line_text[..position.character as usize]
        } else {
            line_text
        };

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

        // Default: offer namespaces + user functions + module names
        let mut items = completion::all_namespaces();
        items.extend(completion::user_fn_completions(&source));

        // Add module names from depends as completion targets
        if let Some(base) = &base_dir {
            items.extend(completion::depends_module_completions(&source, base));
        }

        // Add keywords
        let keywords = [
            "fn", "match", "module", "depends", "exposes", "intent", "verify", "decision", "type",
            "record", "true", "false",
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
            position.character as usize,
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
            position.character as usize,
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
            position.character as usize,
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
