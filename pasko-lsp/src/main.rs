use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
}

// Inspired on tower_lsp_boilerplate

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                hover_provider: Some(HoverProviderCapability::Simple(false)),
                completion_provider: None, // Some(CompletionOptions::default()),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "did open").await;
        self.on_change(
            params.text_document.uri,
            &params.text_document.text,
            Some(params.text_document.version),
        )
        .await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "did change")
            .await;
        self.on_change(
            params.text_document.uri,
            &params.content_changes[0].text,
            Some(params.text_document.version),
        )
        .await
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "did save").await;
        if let Some(text) = params.text {
            self.on_change(params.text_document.uri, &text, None).await;
        }
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "did close")
            .await;
    }
}

struct LspEmitter<'a> {
    uri: Url,
    lsp_diagnostics: &'a mut Vec<lsp_types::Diagnostic>,
}

impl<'a> LspEmitter<'a> {
    fn new(uri: Url, lsp_diagnostics: &'a mut Vec<lsp_types::Diagnostic>) -> LspEmitter<'a> {
        LspEmitter {
            uri: uri,
            lsp_diagnostics,
        }
    }
}

impl<'a> pasko_frontend::diagnostics::DiagnosticEmitter for LspEmitter<'a> {
    fn emit(
        &mut self,
        diag: &pasko_frontend::diagnostics::Diagnostic,
        linemap: &pasko_frontend::span::LineMap,
    ) {
        let message = &diag.message;

        let main_location: pasko_frontend::span::SpanLoc = diag.locus;

        let start_position = Position::new(
            linemap.offset_to_line_0based(main_location.0) as u32,
            linemap.offset_to_column_0based(main_location.0) as u32,
        );
        let end_position = Position::new(
            linemap.offset_to_line_0based(main_location.1) as u32,
            linemap.offset_to_column_0based(main_location.1) as u32,
        );
        let range = Range::new(start_position, end_position);

        // Self::new(range, None, None, None, message, None, None)

        let severity = match diag.kind {
            pasko_frontend::diagnostics::DiagnosticKind::Error => DiagnosticSeverity::ERROR,
            pasko_frontend::diagnostics::DiagnosticKind::Warning => DiagnosticSeverity::WARNING,
            pasko_frontend::diagnostics::DiagnosticKind::Info => DiagnosticSeverity::INFORMATION,
        };

        let related_information = {
            if diag.extra_locus.is_none() {
                None
            } else {
                let mut related_information = Vec::new();
                for extra_locus in diag.extra_locus.as_ref().unwrap() {
                    let start_position = Position::new(
                        linemap.offset_to_line_0based(extra_locus.0.0) as u32,
                        linemap.offset_to_column_0based(extra_locus.0.0) as u32,
                    );
                    let end_position = Position::new(
                        linemap.offset_to_line_0based(extra_locus.0.1) as u32,
                        linemap.offset_to_column_0based(extra_locus.0.1) as u32,
                    );
                    let range = Range::new(start_position, end_position);
                    let location = Location {
                        uri: self.uri.clone(),
                        range,
                    };
                    related_information.push(DiagnosticRelatedInformation {
                        message: extra_locus.1.clone(),
                        location,
                    });
                }
                Some(related_information)
            }
        };

        self.lsp_diagnostics.push(lsp_types::Diagnostic::new(
            range,
            Some(severity),
            None,
            None,
            message.to_string(),
            related_information,
            None,
        ));

        // if let Some(extra_locus) = &diag.extra_locus {
        //     for extra_locus in extra_locus {
        //         let start_position = Position::new(
        //             linemap.offset_to_line_0based(extra_locus.0) as u32,
        //             linemap.offset_to_column_0based(extra_locus.0) as u32,
        //         );
        //         let end_position = Position::new(
        //             linemap.offset_to_line_0based(extra_locus.1) as u32,
        //             linemap.offset_to_column_0based(extra_locus.1) as u32,
        //         );
        //         let range = Range::new(start_position, end_position);

        //         self.lsp_diagnostics.push(lsp_types::Diagnostic::new(
        //             range,
        //             Some(severity),
        //             None,
        //             None,
        //             message.to_string(),
        //             None,
        //             None,
        //         ));
        //     }
        // }

        if let Some(extras) = &diag.extra_diagnostics {
            for d in extras.iter() {
                self.emit(d, linemap);
            }
        }
    }
}

impl Backend {
    async fn on_change(&self, uri: Url, input: &String, version: Option<i32>) {
        self.client
            .log_message(MessageType::INFO, format!("input: {}", input))
            .await;
        let mut diagnostics = pasko_frontend::diagnostics::Diagnostics::new();

        // Parse input.
        let parse_result = pasko_frontend::parser::parse_pasko_program(&input, &mut diagnostics);

        // Create the diagnostic emitter used by the semantic checks.
        // FIXME: Tabstop?
        let tabstop = 4usize;
        let linemap = pasko_frontend::span::LineMap::new(&input, tabstop);
        let mut lsp_diagnostics = Vec::new();
        let mut lsp_emitter = LspEmitter::new(uri.clone(), &mut lsp_diagnostics);

        if let Some(mut program) = parse_result {
            let mut semantic_context = pasko_frontend::semantic::SemanticContext::new(&linemap);
            pasko_frontend::semantic::check_program(
                &mut program,
                &mut semantic_context,
                &mut diagnostics,
            );
        }
        diagnostics.report(&mut lsp_emitter, &linemap);

        for d in &lsp_diagnostics {
            self.client
                .log_message(MessageType::INFO, format!("diagnostic: {}", d.message))
                .await;
        }

        self.client
            .publish_diagnostics(uri, lsp_diagnostics, version)
            .await;
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend { client });
    Server::new(stdin, stdout, socket).serve(service).await;
}
