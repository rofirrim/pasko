use pasko_frontend::visitor::Visitable;
use std::collections::HashMap;
use std::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct FileInfo {
    line_map: pasko_frontend::span::LineMap,
    program: Option<pasko_frontend::span::SpannedBox<pasko_frontend::ast::Program>>,
    semantic_context: Option<pasko_frontend::semantic::SemanticContext>,
}

struct Backend {
    client: Client,
    file_info: Mutex<HashMap<Url, FileInfo>>,
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
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: None, // Some(CompletionOptions::default()),
                definition_provider: Some(OneOf::Left(true)),
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

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "did close")
            .await;

        // Clear any stored info.
        let uri = params.text_document.uri;
        {
            let mut file_info = self.file_info.lock().unwrap();
            file_info.remove(&uri);
        }
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri: Url = params.text_document_position_params.text_document.uri;
        let position: Position = params.text_document_position_params.position;

        let offset = {
            let file_info = self.file_info.lock().unwrap();
            file_info.get(&uri).and_then(|file_info| {
                file_info.line_map.line_and_col_to_offset(
                    (position.line + 1) as usize,
                    (position.character + 1) as usize,
                )
            })
        };

        let file_info = self.file_info.lock().unwrap();
        let result = offset
            .and_then(|offset| {
                file_info.get(&uri).and_then(|file_info| {
                    self.search_definition_location_of_identifier(
                        &file_info.program,
                        &file_info.semantic_context,
                        offset,
                    )
                })
            })
            .and_then(|found| {
                let line_map = file_info
                    .get(&uri)
                    .and_then(|file_info| Some(&file_info.line_map));

                line_map.and_then(|line_map| {
                    let start_position = Position::new(
                        line_map.offset_to_line_0based(found.0) as u32,
                        line_map.offset_to_column_0based(found.0) as u32,
                    );
                    let end_position = Position::new(
                        line_map.offset_to_line_0based(found.1) as u32,
                        line_map.offset_to_column_0based(found.1) as u32,
                    );
                    Some(GotoDefinitionResponse::Scalar(Location::new(
                        uri,
                        Range::new(start_position, end_position),
                    )))
                })
            });

        Ok(result)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let offset = {
            let file_info = self.file_info.lock().unwrap();
            file_info.get(&uri).and_then(|file_info| {
                file_info.line_map.line_and_col_to_offset(
                    (position.line + 1) as usize,
                    (position.character + 1) as usize,
                )
            })
        };

        let file_info = self.file_info.lock().unwrap();
        let file_info = file_info.get(&uri);
        let result = offset
            .and_then(|offset| {
                let file_info = file_info?;
                self.search_identifier(&file_info.program, &file_info.semantic_context, offset)
            })
            .and_then(|sym_id| {
                let file_info = file_info?;
                let semantic_context = file_info.semantic_context.as_ref()?;
                let sym = semantic_context.symbol_map.get_symbol(sym_id);
                self.describe_symbol(sym, semantic_context)
            })
            .map(|contents| Hover {
                contents,
                range: None,
            });

        Ok(result)
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
                        linemap.offset_to_line_0based(extra_locus.0 .0) as u32,
                        linemap.offset_to_column_0based(extra_locus.0 .0) as u32,
                    );
                    let end_position = Position::new(
                        linemap.offset_to_line_0based(extra_locus.0 .1) as u32,
                        linemap.offset_to_column_0based(extra_locus.0 .1) as u32,
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

        // Clear earlier info
        {
            let mut file_info = self.file_info.lock().unwrap();
            file_info.remove(&uri);
        }

        // Parse input.
        let mut program = pasko_frontend::parser::parse_pasko_program(&input, &mut diagnostics);

        // Create the diagnostic emitter used by the semantic checks.
        let mut lsp_diagnostics = Vec::new();
        let mut lsp_emitter = LspEmitter::new(uri.clone(), &mut lsp_diagnostics);

        let semantic_context = program.as_mut().and_then(|program| {
            let mut semantic_context = pasko_frontend::semantic::SemanticContext::new();
            pasko_frontend::semantic::check_program(
                program,
                &mut semantic_context,
                &mut diagnostics,
            );

            Some(semantic_context)
        });

        // FIXME: Tabstop?
        let tabstop = 4usize;
        let line_map = pasko_frontend::span::LineMap::new(&input, tabstop);

        // Emit diagnostics for the client.
        diagnostics.report(&mut lsp_emitter, &line_map);

        // Remember the tree and the semantic information of this input.
        {
            let mut file_info = self.file_info.lock().unwrap();
            let new_file_info = FileInfo {
                line_map,
                program,
                semantic_context,
            };
            file_info.insert(uri.clone(), new_file_info);
        }

        for d in &lsp_diagnostics {
            self.client
                .log_message(MessageType::INFO, format!("diagnostic: {}", d.message))
                .await;
        }

        self.client
            .publish_diagnostics(uri, lsp_diagnostics, version)
            .await;
    }

    fn search_identifier(
        &self,
        program: &Option<pasko_frontend::span::SpannedBox<pasko_frontend::ast::Program>>,
        semantic_context: &Option<pasko_frontend::semantic::SemanticContext>,
        offset: usize,
    ) -> Option<pasko_frontend::symbol::SymbolId> {
        let program = program.as_ref()?;
        let semantic_context = semantic_context.as_ref()?;
        let mut ast_identifier_search = ASTIdentifierSearch {
            offset,
            semantic_context,
            found_symbol: None,
        };

        program
            .get()
            .walk_mut(&mut ast_identifier_search, program.loc(), program.id());

        ast_identifier_search.found_symbol
    }

    fn search_definition_location_of_identifier(
        &self,
        program: &Option<pasko_frontend::span::SpannedBox<pasko_frontend::ast::Program>>,
        semantic_context: &Option<pasko_frontend::semantic::SemanticContext>,
        offset: usize,
    ) -> Option<pasko_frontend::span::SpanLoc> {
        self.search_identifier(program, semantic_context, offset)
            .and_then(|symbol_id| {
                semantic_context
                    .as_ref()?
                    .symbol_map
                    .get_symbol(symbol_id)
                    .get_defining_point()
            })
    }

    fn describe_symbol(
        &self,
        sym: &pasko_frontend::symbol::Symbol,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
    ) -> Option<HoverContents> {
        match sym.get_kind() {
            pasko_frontend::symbol::SymbolKind::Variable => {
                let type_name = semantic_context
                    .type_system
                    .get_type_name(sym.get_type()?, &semantic_context.symbol_map);
                Some(HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: [
                        "variable `", sym.get_name(), "`\n",
                        "---\n",
                        "type: `", &type_name, "`\n",
                    ].join(""),
                }))
            }
            _ => None,
        }
    }
}

struct ASTIdentifierSearch<'a> {
    offset: usize,
    semantic_context: &'a pasko_frontend::semantic::SemanticContext,

    // Output
    found_symbol: Option<pasko_frontend::symbol::SymbolId>,
}

impl<'a> pasko_frontend::visitor::VisitorMut for ASTIdentifierSearch<'a> {
    fn unhandled_node_pre(
        &self,
        _class: &str,
        span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) -> bool {
        // Limit ourselves to nodes that include the offset we are looking for
        span.0 <= self.offset && self.offset < span.1 && self.found_symbol.is_none()
    }

    fn visit_assig_variable(
        &mut self,
        _n: &pasko_frontend::ast::AssigVariable,
        _span: &pasko_frontend::span::SpanLoc,
        id: pasko_frontend::span::SpanId,
    ) {
        if self.found_symbol.is_some() {
            return;
        }
        self.found_symbol = self.semantic_context.get_ast_symbol(id);
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        file_info: Mutex::new(HashMap::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
