use pasko_frontend::visitor::Visitable;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::ControlFlow;
use std::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct FileInfo {
    line_map: pasko_frontend::span::LineMap,
    program: Option<pasko_frontend::span::SpannedBox<pasko_frontend::ast::Program>>,
    semantic_context: Option<RefCell<pasko_frontend::semantic::SemanticContext>>,
    input: String,
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
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: Some(CompletionOptionsCompletionItem {
                        label_details_support: Some(true),
                    }),
                    ..Default::default()
                }),
                type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
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

        let offset = self.compute_offset_from_position(&uri, position);

        let file_info = self.file_info.lock().unwrap();
        let result = offset
            .and_then(|offset| {
                file_info.get(&uri).and_then(|file_info| {
                    let ctx = file_info.semantic_context.as_ref()?;
                    self.search_definition_location_of_identifier(
                        &file_info.program,
                        Some(&ctx.borrow()),
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

    async fn goto_type_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri: Url = params.text_document_position_params.text_document.uri;
        let position: Position = params.text_document_position_params.position;

        let offset = self.compute_offset_from_position(&uri, position);

        let file_info = self.file_info.lock().unwrap();
        let result = offset
            .and_then(|offset| {
                file_info.get(&uri).and_then(|file_info| {
                    let ctx = file_info.semantic_context.as_ref()?;
                    self.search_type_definition_location_of_identifier(
                        &file_info.program,
                        Some(&ctx.borrow()),
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

        let offset = self.compute_offset_from_position(&uri, position);

        let file_info = self.file_info.lock().unwrap();
        let file_info = file_info.get(&uri);
        let result = offset
            .and_then(|offset| {
                let file_info = file_info?;
                let ctx = file_info.semantic_context.as_ref()?;
                self.search_identifier(&file_info.program, Some(&ctx.borrow()), offset)
            })
            .and_then(|sym_id| {
                let file_info = file_info?;
                let ctx = file_info.semantic_context.as_ref()?;
                let ctx = &ctx.borrow();
                let sym = ctx.symbol_map.get_symbol(sym_id);
                self.describe_symbol(sym, ctx)
            })
            .map(|contents| Hover {
                contents,
                range: None,
            });

        Ok(result)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let offset = self.compute_offset_from_position(&uri, position);

        let file_info = self.file_info.lock().unwrap();
        let file_info = file_info.get(&uri);

        let mut items = Vec::new();

        let period = params
            .context
            .map(|ctx| ctx.trigger_character)
            .flatten()
            .map(|c| c == ".")
            .unwrap_or(false);

        if period {
            offset
                .and_then(|offset: usize| {
                    let file_info = file_info?;
                    let ctx = file_info.semantic_context.as_ref()?;
                    let ctx = ctx.borrow();
                    let scope = self.search_scope(&file_info.program, Some(&ctx), offset);

                    scope.map(|s| (s, offset))
                })
                .and_then(|(scopeid, offset)| {
                    let file_info = file_info?;
                    let access =
                        self.search_variable_access(&file_info.program, &file_info.input, offset);

                    access.map(|a| (scopeid, a))
                })
                .and_then(|(scope_id, t)| {
                    // eprintln!("text around the requested completion |{t}|");
                    let mut diagnostics = pasko_frontend::diagnostics::Diagnostics::new();
                    let var_access =
                        pasko_frontend::parser::parse_pasko_assig(&t, &mut diagnostics);

                    var_access.map(|x| (diagnostics, x, scope_id))
                })
                .and_then(|(mut diagnostics, mut var_access, scope_id)| {
                    let file_info = file_info?;
                    let ctx = file_info.semantic_context.as_ref()?;
                    let mut ctx = ctx.borrow_mut();

                    let old_scope_id = ctx.scope.get_current_scope_id();
                    ctx.scope.set_current_scope_id(scope_id);

                    pasko_frontend::semantic::check_assig(
                        &mut var_access,
                        &mut ctx,
                        &mut diagnostics,
                    );

                    ctx.scope.set_current_scope_id(old_scope_id);

                    Some(var_access)
                })
                .and_then(|var_access| {
                    let file_info = file_info?;
                    let ctx = file_info.semantic_context.as_ref()?;
                    let ctx = ctx.borrow();

                    // let mut dumper =
                    //     pasko_frontend::dump::ASTDumper::new(&ctx, &file_info.line_map);
                    // dumper.set_print_ranges();
                    // var_access
                    //     .get()
                    //     .walk_mut(&mut dumper, var_access.loc(), var_access.id());
                    // eprintln!("{}", dumper);

                    let ty = ctx.get_ast_type(var_access.id())?;

                    if ctx.type_system.is_record_type(ty, &ctx.symbol_map) {
                        let fields = ctx
                            .type_system
                            .record_type_get_all_fields(ty, &ctx.symbol_map);
                        fields.iter().for_each(|sym_id| {
                            let sym = ctx.symbol_map.get_symbol(*sym_id);

                            let completion_kind = Some(CompletionItemKind::FIELD);
                            let detail = Some("field".to_string());
                            let documentation = self
                                .describe_field_markup(sym, &ctx)
                                .map(|x| Documentation::MarkupContent(x));

                            items.push(CompletionItem {
                                label: sym.get_name().clone(),
                                detail,
                                documentation,
                                kind: completion_kind,
                                ..Default::default()
                            });
                        });
                    }

                    Some(())
                });
        } else {
            offset
                .and_then(|offset: usize| {
                    let file_info = file_info?;
                    let ctx = file_info.semantic_context.as_ref()?;
                    let ctx = ctx.borrow();
                    self.search_scope(&file_info.program, Some(&ctx), offset)
                })
                .and_then(|scope: pasko_frontend::scope::ScopeId| {
                    let file_info = file_info?;
                    let ctx = file_info.semantic_context.as_ref()?;
                    let ctx = ctx.borrow();
                    let symbols = ctx.scope.get_all_symbols_in_scope(scope);
                    Some(symbols)
                })
                .and_then(|symbols: Vec<pasko_frontend::symbol::SymbolId>| {
                    let file_info = file_info?;
                    let ctx = file_info.semantic_context.as_ref()?;
                    let ctx = ctx.borrow();
                    symbols.iter().for_each(|sym_id| {
                        let sym = ctx.symbol_map.get_symbol(*sym_id);

                        let completion_kind;
                        let mut detail = None;
                        let mut documentation = None;
                        match sym.get_kind() {
                            pasko_frontend::symbol::SymbolKind::Variable => {
                                completion_kind = Some(CompletionItemKind::VARIABLE);
                                detail = Some("variable".to_string());
                                documentation = self
                                    .describe_variable_markup(sym, &ctx)
                                    .map(|x| Documentation::MarkupContent(x));
                            }
                            pasko_frontend::symbol::SymbolKind::Const => {
                                completion_kind = Some(CompletionItemKind::CONSTANT);
                                detail = Some("constant".to_string());
                            }
                            pasko_frontend::symbol::SymbolKind::Function => {
                                completion_kind = Some(CompletionItemKind::FUNCTION);
                                detail = Some("function".to_string());
                                documentation = Some(Documentation::MarkupContent(
                                    self.describe_function_markup(sym, &ctx),
                                ));
                            }
                            pasko_frontend::symbol::SymbolKind::Procedure => {
                                completion_kind = Some(CompletionItemKind::FUNCTION);
                                detail = Some("procedure".to_string());
                                documentation = Some(Documentation::MarkupContent(
                                    self.describe_procedure_markup(sym, &ctx),
                                ));
                            }
                            pasko_frontend::symbol::SymbolKind::Type => {
                                completion_kind = Some(CompletionItemKind::STRUCT);
                                detail = Some("type".to_string());
                                documentation = self
                                    .describe_type_markup(sym, &ctx)
                                    .map(|x| Documentation::MarkupContent(x));
                            }
                            _ => {
                                completion_kind = None;
                            }
                        };

                        if let Some(completion_kind) = completion_kind {
                            items.push(CompletionItem {
                                label: sym.get_name().clone(),
                                detail,
                                documentation,
                                kind: Some(completion_kind),
                                ..Default::default()
                            });
                        }
                    });
                    Some(())
                });
        }

        let results = Some(items);
        Ok(results.map(CompletionResponse::Array))
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
        // self.client
        //     .log_message(MessageType::INFO, format!("input: {}", input))
        //     .await;
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

        // if let Some(semantic_context) = semantic_context.as_ref() {
        //     if let Some(program) = program.as_ref() {
        //         let mut dumper = pasko_frontend::dump::ASTDumper::new(semantic_context, &line_map);
        //         dumper.set_print_ranges();
        //         program
        //             .get()
        //             .walk_mut(&mut dumper, program.loc(), program.id());
        //         eprintln!("{}", dumper);
        //     }
        // }

        // Emit diagnostics for the client.
        diagnostics.report(&mut lsp_emitter, &line_map);

        // Remember the tree and the semantic information of this input.
        {
            let mut file_info = self.file_info.lock().unwrap();
            let new_file_info = FileInfo {
                line_map,
                program,
                semantic_context: semantic_context.map(|x| RefCell::new(x)),
                // Not ideal but we need it to recover spans without assuming the input
                input: input.clone(),
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
        semantic_context: Option<&pasko_frontend::semantic::SemanticContext>,
        offset: usize,
    ) -> Option<pasko_frontend::symbol::SymbolId> {
        let program = program.as_ref()?;
        let semantic_context = semantic_context?;
        let mut ast_identifier_search = ASTIdentifierSearch {
            search: ASTSymbolSearch {
                offset,
                semantic_context,
                found_symbol: None,
            },
        };

        program
            .get()
            .walk_mut(&mut ast_identifier_search, program.loc(), program.id());

        ast_identifier_search.search.found_symbol
    }

    fn search_variable_access(
        &self,
        program: &Option<pasko_frontend::span::SpannedBox<pasko_frontend::ast::Program>>,
        input: &String,
        offset: usize,
    ) -> Option<String> {
        let program = program.as_ref()?;

        let mut assignment_search = ASTVariableAccess {
            offset,
            input,
            text: None,
        };

        program
            .get()
            .walk_mut(&mut assignment_search, program.loc(), program.id());

        assignment_search.text
    }

    fn search_scope(
        &self,
        program: &Option<pasko_frontend::span::SpannedBox<pasko_frontend::ast::Program>>,
        semantic_context: Option<&pasko_frontend::semantic::SemanticContext>,
        offset: usize,
    ) -> Option<pasko_frontend::scope::ScopeId> {
        let program = program.as_ref()?;
        let semantic_context = semantic_context?;

        let mut procedure_or_function_search = ASTFunctionOrProcedureSearch {
            search: ASTSymbolSearch {
                offset,
                semantic_context,
                found_symbol: None,
            },
        };

        program.get().walk_mut(
            &mut procedure_or_function_search,
            program.loc(),
            program.id(),
        );

        let scope =
            if let Some(func_or_proc_sym_id) = procedure_or_function_search.search.found_symbol {
                let func_or_proc_sym = semantic_context.symbol_map.get_symbol(func_or_proc_sym_id);
                func_or_proc_sym.get_region_scope()?
            } else {
                semantic_context.scope.get_program_scope_id()
            };
        Some(scope)
    }

    fn compute_offset_from_position(&self, uri: &Url, position: Position) -> Option<usize> {
        let file_info = self.file_info.lock().unwrap();
        file_info.get(&uri).and_then(|file_info| {
            file_info.line_map.line_and_col_to_offset(
                (position.line + 1) as usize,
                (position.character + 1) as usize,
            )
        })
    }

    fn search_definition_location_of_identifier(
        &self,
        program: &Option<pasko_frontend::span::SpannedBox<pasko_frontend::ast::Program>>,
        semantic_context: Option<&pasko_frontend::semantic::SemanticContext>,
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

    fn search_type_definition_location_of_identifier(
        &self,
        program: &Option<pasko_frontend::span::SpannedBox<pasko_frontend::ast::Program>>,
        semantic_context: Option<&pasko_frontend::semantic::SemanticContext>,
        offset: usize,
    ) -> Option<pasko_frontend::span::SpanLoc> {
        self.search_identifier(program, semantic_context, offset)
            .and_then(|symbol_id| {
                let sym = semantic_context.as_ref()?.symbol_map.get_symbol(symbol_id);
                match sym.get_kind() {
                    pasko_frontend::symbol::SymbolKind::Variable => sym.get_type().and_then(|ty| {
                        if semantic_context.as_ref()?.type_system.is_named_type(ty) {
                            let named_type_sym = semantic_context
                                .as_ref()?
                                .type_system
                                .named_type_get_symbol(ty);
                            let named_type_sym = semantic_context
                                .as_ref()?
                                .symbol_map
                                .get_symbol(named_type_sym);
                            named_type_sym.get_defining_point()
                        } else {
                            sym.get_defining_point()
                        }
                    }),
                    _ => None,
                }
            })
    }

    fn describe_procedure(
        &self,
        sym: &pasko_frontend::symbol::Symbol,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
    ) -> String {
        let mut result = String::new();
        if let Some(return_sym) = sym.get_return_symbol() {
            result += "→ ";

            let return_sym = semantic_context.symbol_map.get_symbol(return_sym);
            let return_type_name = semantic_context.type_system.get_type_name(
                return_sym
                    .get_type()
                    .unwrap_or_else(|| semantic_context.type_system.get_error_type()),
                &semantic_context.symbol_map,
            );

            result += "`";
            result += &return_type_name;
            result += "`";
            result += "\n\n";
        }

        if let Some(params) = sym.get_formal_parameters() {
            result += "Parameters:\n";
            for param_decls in params {
                for param_decl in param_decls {
                    let param_sym = semantic_context.symbol_map.get_symbol(param_decl);
                    result += "  - `";
                    result += param_sym.get_name();
                    result += "`";

                    let type_id = param_sym
                        .get_type()
                        .unwrap_or_else(|| semantic_context.type_system.get_error_type());

                    result += ": `";
                    result += &semantic_context
                        .type_system
                        .get_type_name(type_id, &semantic_context.symbol_map);
                    result += "`\n";
                }
            }
            result += "\n";
        }
        result
    }

    fn describe_function_markup(
        &self,
        sym: &pasko_frontend::symbol::Symbol,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
    ) -> MarkupContent {
        let procedure_desc = self.describe_procedure(sym, semantic_context);
        MarkupContent {
            kind: MarkupKind::Markdown,
            value: [
                "function `",
                sym.get_name(),
                "`\n",
                "---\n",
                &procedure_desc,
                "\n",
            ]
            .join(""),
        }
    }

    fn describe_procedure_markup(
        &self,
        sym: &pasko_frontend::symbol::Symbol,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
    ) -> MarkupContent {
        let procedure_desc = self.describe_procedure(sym, semantic_context);
        MarkupContent {
            kind: MarkupKind::Markdown,
            value: [
                "procedure `",
                sym.get_name(),
                "`\n",
                "---\n",
                &procedure_desc,
                "\n",
            ]
            .join(""),
        }
    }

    fn describe_variable_markup(
        &self,
        sym: &pasko_frontend::symbol::Symbol,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
    ) -> Option<MarkupContent> {
        let type_name = semantic_context
            .type_system
            .get_type_name(sym.get_type()?, &semantic_context.symbol_map);
        Some(MarkupContent {
            kind: MarkupKind::Markdown,
            value: [
                "variable `",
                sym.get_name(),
                "`\n",
                "---\n",
                "type: `",
                &type_name,
                "`\n",
            ]
            .join(""),
        })
    }

    fn describe_type_markup(
        &self,
        sym: &pasko_frontend::symbol::Symbol,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
    ) -> Option<MarkupContent> {
        let type_name = semantic_context
            .type_system
            .get_type_name(sym.get_type()?, &semantic_context.symbol_map);
        Some(MarkupContent {
            kind: MarkupKind::Markdown,
            value: [
                "type `",
                sym.get_name(),
                "`\n",
                "---\n",
                "alias of: `",
                &type_name,
                "`\n",
            ]
            .join(""),
        })
    }

    fn describe_field_markup(
        &self,
        sym: &pasko_frontend::symbol::Symbol,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
    ) -> Option<MarkupContent> {
        let type_name = semantic_context
            .type_system
            .get_type_name(sym.get_type()?, &semantic_context.symbol_map);
        let mut values = vec![
            "field `",
            sym.get_name(),
            "`\n",
            "---\n",
            "type: `",
            &type_name,
            "`\n",
        ];
        let record_type_name;
        if let Some(associated_record_type) = sym.associated_record_type() {
            record_type_name = semantic_context
                .type_system
                .get_type_name(associated_record_type, &semantic_context.symbol_map);
            values.append(&mut vec!["\n", "record type: `", &record_type_name, "`\n"]);
        }
        Some(MarkupContent {
            kind: MarkupKind::Markdown,
            value: values.join(""),
        })
    }

    fn describe_symbol(
        &self,
        sym: &pasko_frontend::symbol::Symbol,
        semantic_context: &pasko_frontend::semantic::SemanticContext,
    ) -> Option<HoverContents> {
        match sym.get_kind() {
            pasko_frontend::symbol::SymbolKind::Variable => Some(HoverContents::Markup(
                self.describe_variable_markup(sym, semantic_context)?,
            )),
            pasko_frontend::symbol::SymbolKind::Procedure => Some(HoverContents::Markup(
                self.describe_procedure_markup(sym, semantic_context),
            )),
            pasko_frontend::symbol::SymbolKind::Function => Some(HoverContents::Markup(
                self.describe_function_markup(sym, semantic_context),
            )),
            pasko_frontend::symbol::SymbolKind::Field => Some(HoverContents::Markup(
                self.describe_field_markup(sym, semantic_context)?,
            )),
            pasko_frontend::symbol::SymbolKind::Type => Some(HoverContents::Markup(
                self.describe_type_markup(sym, semantic_context)?,
            )),
            _ => None,
        }
    }
}

struct ASTVariableAccess<'a> {
    offset: usize,
    input: &'a String,

    // Output
    text: Option<String>,
}

impl<'a> ASTVariableAccess<'a> {
    fn is_in_span(&self, span: &pasko_frontend::span::SpanLoc) -> bool {
        span.0 <= self.offset && self.offset < span.1 && self.text.is_none()
    }

    // Returns true if the text was captured.
    fn capture_text_if_in_span(&mut self, span: &pasko_frontend::span::SpanLoc) -> bool {
        if self.is_in_span(span) {
            let start = span.0;
            let end = span.1;

            self.text = Some(self.input[start..std::cmp::min(self.offset, end)].to_string());
            return true;
        }
        false
    }
}

impl<'a> pasko_frontend::visitor::VisitorMut for ASTVariableAccess<'a> {
    fn unhandled_node_pre(
        &mut self,
        _class: &str,
        span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) -> bool {
        // Limit ourselves to nodes that include the offset we are looking for
        self.is_in_span(span)
    }

    fn visit_pre_assig(
        &mut self,
        _n: &pasko_frontend::ast::Assig,
        span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) -> bool {
        !self.capture_text_if_in_span(span)
    }

    fn visit_stmt_error(
        &mut self,
        _n: &pasko_frontend::ast::StmtError,
        span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) {
        self.capture_text_if_in_span(span);
    }

    fn visit_expr_error(
        &mut self,
        _n: &pasko_frontend::ast::ExprError,
        span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) {
        self.capture_text_if_in_span(span);
    }
}

struct ASTSymbolSearch<'a> {
    offset: usize,
    semantic_context: &'a pasko_frontend::semantic::SemanticContext,

    // Output
    found_symbol: Option<pasko_frontend::symbol::SymbolId>,
}

impl<'a> ASTSymbolSearch<'a> {
    fn register_symbol(&mut self, id: pasko_frontend::symbol::SymbolId) {
        self.found_symbol = Some(id);
    }

    fn register_symbol_from_span(&mut self, id: pasko_frontend::span::SpanId) {
        if self.found_symbol.is_some() {
            return;
        }
        self.found_symbol = self.semantic_context.get_ast_symbol(id);
    }

    fn is_in_span(&self, span: &pasko_frontend::span::SpanLoc) -> bool {
        span.0 <= self.offset && self.offset < span.1 && self.found_symbol.is_none()
    }
}

struct ASTFunctionOrProcedureSearch<'a> {
    search: ASTSymbolSearch<'a>,
}

impl<'a> pasko_frontend::visitor::VisitorMut for ASTFunctionOrProcedureSearch<'a> {
    fn unhandled_node_pre(
        &mut self,
        _class: &str,
        span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) -> bool {
        // Limit ourselves to nodes that include the offset we are looking for
        self.search.is_in_span(span)
    }

    fn visit_post_function_definition(
        &mut self,
        n: &pasko_frontend::ast::FunctionDefinition,
        _span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) {
        self.search.register_symbol_from_span(n.0.id());
    }

    fn visit_post_procedure_definition(
        &mut self,
        n: &pasko_frontend::ast::ProcedureDefinition,
        _span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) {
        self.search.register_symbol_from_span(n.0.id());
    }
}

struct ASTIdentifierSearch<'a> {
    search: ASTSymbolSearch<'a>,
}

impl<'a> pasko_frontend::visitor::VisitorMut for ASTIdentifierSearch<'a> {
    fn unhandled_node_pre(
        &mut self,
        _class: &str,
        span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) -> bool {
        // Limit ourselves to nodes that include the offset we are looking for
        self.search.is_in_span(span)
    }

    fn visit_assig_variable(
        &mut self,
        _n: &pasko_frontend::ast::AssigVariable,
        _span: &pasko_frontend::span::SpanLoc,
        id: pasko_frontend::span::SpanId,
    ) {
        self.search.register_symbol_from_span(id);
    }

    fn visit_post_stmt_procedure_call(
        &mut self,
        n: &pasko_frontend::ast::StmtProcedureCall,
        _span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) {
        let callee = &n.0;
        if self.search.is_in_span(callee.loc()) {
            self.search.register_symbol_from_span(callee.id());
        }
    }

    fn visit_post_expr_function_call(
        &mut self,
        n: &pasko_frontend::ast::ExprFunctionCall,
        _span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) {
        let callee = &n.0;
        if self.search.is_in_span(callee.loc()) {
            self.search.register_symbol_from_span(callee.id());
        }
    }

    fn visit_post_assig_field_access(
        &mut self,
        n: &pasko_frontend::ast::AssigFieldAccess,
        _span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) {
        let field = &n.1;
        if self.search.is_in_span(field.loc()) {
            self.search.register_symbol_from_span(field.id());
        }
    }

    fn visit_type_identifier(
        &mut self,
        _n: &pasko_frontend::ast::TypeIdentifier,
        span: &pasko_frontend::span::SpanLoc,
        id: pasko_frontend::span::SpanId,
    ) {
        if !self.search.is_in_span(span) {
            return;
        }
        if let Some(ty) = self.search.semantic_context.get_ast_type(id) {
            if self.search.semantic_context.type_system.is_named_type(ty) {
                let sym_id = self
                    .search
                    .semantic_context
                    .type_system
                    .named_type_get_symbol(ty);
                self.search.register_symbol(sym_id);
            }
        }
    }

    fn visit_post_variable_declaration(
        &mut self,
        n: &pasko_frontend::ast::VariableDeclaration,
        _span: &pasko_frontend::span::SpanLoc,
        _id: pasko_frontend::span::SpanId,
    ) {
        let _ = n.0.iter().try_for_each(|e| {
            if self.search.is_in_span(e.loc()) {
                self.search.register_symbol_from_span(e.id());
                return ControlFlow::Break(());
            }
            ControlFlow::Continue(())
        });
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
