mod backend;
mod completion;
mod definition;
mod diagnostics;
mod hover;
mod lenses;
mod modules;
mod position;
mod signature;
mod symbols;

use backend::AverBackend;
use tower_lsp_server::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(AverBackend::new);

    Server::new(stdin, stdout, socket).serve(service).await;
}
