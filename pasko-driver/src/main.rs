use clap::{Args, Parser, ValueEnum};
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitCode;

use pasko_codegen::{self};
use pasko_frontend::{self, dump, visitor::Visitable};

mod diagnostics;

#[derive(Parser)]
#[command(name = "pasko")]
#[command(author = "Roger Ferrer <rofirrim@gmail.com>")]
#[command(version = "0.1")]
#[command(about = "pasko driver", long_about = None)]
struct Cli {
    #[arg(help = "Input file to compile")]
    file: PathBuf,

    #[arg(short, long)]
    output: Option<PathBuf>,

    #[arg(short, long, value_enum, default_value_t = Mode::Codegen)]
    mode: Mode,

    #[arg(
        short,
        long,
        default_value_t = false,
        help = "Be verbose during the compilation process"
    )]
    verbose: bool,

    #[arg(short, default_value_t = false, help = "Only emit object, do not link")]
    compile_only: bool,

    #[arg(long, help = "Path to the directory containing the pasko runtime")]
    pasko_runtime: Option<PathBuf>,

    #[arg(long, help = "Target to generate code for")]
    target: Option<String>,

    #[command(flatten, next_help_heading = "Debug / Testing")]
    debug_flags: DebugFlags,
}

#[derive(Args)]
struct DebugFlags {
    #[arg(
        long,
        default_value_t = false,
        help = "Do not print IDs when dumping AST"
    )]
    ast_dump_no_ids: bool,

    #[arg(
        long,
        default_value_t = false,
        help = "The compiler must fail during parsing"
    )]
    must_fail_parse: bool,

    #[arg(
        long,
        default_value_t = false,
        help = "The compiler must fail during semantic checking"
    )]
    must_fail_semantic: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Mode {
    ParseOnly,
    ASTDumpPre,
    ASTDump,
    ParseSemantic,
    Codegen,
    IRDump,
}

fn main() -> ExitCode {
    env_logger::init();

    let cli = Cli::parse();

    let extension = cli.file.extension();
    if extension.is_none() || extension.unwrap() != "pas" {
        eprintln!(
            "Input file must have extension '.pas' (input file specified is '{}')",
            cli.file.to_string_lossy()
        );
        return ExitCode::FAILURE;
    }

    let input_filename = cli.file.to_string_lossy().to_string();

    // Read input.
    let input = fs::read_to_string(&cli.file);
    if let Err(e) = input {
        eprintln!("Error while reading file '{}': {}", input_filename, e);
        return ExitCode::FAILURE;
    }
    let input = input.unwrap();

    // Initialize diagnostic engine.
    let mut diagnostics = pasko_frontend::diagnostics::Diagnostics::new();

    if cli.verbose {
        eprintln!("Parsing '{}'", input_filename);
    }

    // Parse input.
    let parse_result = pasko_frontend::parser::parse_pasko_program(&input, &mut diagnostics);

    // Create the diagnostic emitter used by the semantic checks.
    let simple_emitter = diagnostics::SimpleEmitter::new(&input_filename, &input);

    // AST processing.
    if cli.mode == Mode::ParseOnly || parse_result.is_none() {
        // Report any parsing error and finish.
        assert!(
            parse_result.is_some() || diagnostics.num_error() > 0,
            "if the parse fails we must diagnose an error"
        );
        diagnostics.report(&simple_emitter);
        if cli.debug_flags.must_fail_parse {
            if parse_result.is_none() {
                return ExitCode::SUCCESS;
            } else {
                return ExitCode::FAILURE;
            }
        }
        return ExitCode::from(diagnostics);
    }

    let mut program = parse_result.unwrap();

    // FIXME: The input is used to build the linemap but there is no point to build it twice, one for semantic
    // and another one for the diagnostics and the dumper. Either we get it from the semantic context
    // or we push it onto it.
    let mut semantic_context = pasko_frontend::semantic::SemanticContext::new(&input);

    if cli.mode == Mode::ASTDumpPre {
        let mut dumper = dump::ASTDumper::new(&input, &semantic_context);
        if cli.debug_flags.ast_dump_no_ids {
            dumper.set_no_ids();
        }
        program
            .get()
            .walk_mut(&mut dumper, program.loc(), program.id());
        println!("{}", dumper);
        return ExitCode::SUCCESS;
    }

    if cli.verbose {
        eprintln!("Semantic checking '{}'", input_filename);
    }

    pasko_frontend::semantic::check_program(&mut program, &mut semantic_context, &mut diagnostics);

    diagnostics.report(&simple_emitter);

    if cli.mode == Mode::ASTDump {
        let mut dumper = dump::ASTDumper::new(&input, &semantic_context);
        if cli.debug_flags.ast_dump_no_ids {
            dumper.set_no_ids();
        }
        program
            .get()
            .walk_mut(&mut dumper, program.loc(), program.id());
        println!("{}", dumper);
        return ExitCode::SUCCESS;
    }

    if cli.mode == Mode::ParseSemantic || diagnostics.num_error() > 0 {
        if cli.debug_flags.must_fail_semantic {
            if diagnostics.num_error() > 0 {
                return ExitCode::SUCCESS;
            } else {
                return ExitCode::FAILURE;
            }
        }
        return ExitCode::from(diagnostics);
    }

    let mut object_filename = cli.file.clone();
    object_filename.set_extension("o");

    // Code generation
    if cli.verbose {
        eprintln!(
            "Code generation into '{}'",
            object_filename.to_string_lossy()
        );
    }

    let ir_dump = cli.mode == Mode::IRDump;

    pasko_codegen::codegen::codegen(
        cli.target,
        &program,
        &semantic_context,
        &object_filename,
        ir_dump,
    );

    if ir_dump || cli.compile_only {
        return ExitCode::SUCCESS;
    }

    let mut executable_filename = cli.file.clone();
    executable_filename.set_extension("");

    // Now link.
    let mut linker_invocation = Command::new("gcc");
    linker_invocation
        .arg("-o")
        .arg(executable_filename.clone())
        .arg(object_filename.clone());

    if let Some(pasko_runtime) = cli.pasko_runtime {
        linker_invocation
            .arg("-L")
            .arg(pasko_runtime.clone())
            .arg(format!("-Wl,-rpath,{}", pasko_runtime.to_string_lossy()))
            .arg("-lpasko_runtime")
            .arg("-lm");
    }

    if cli.verbose {
        eprintln!(
            "Linking '{}' to '{}'",
            object_filename.to_string_lossy(),
            executable_filename.to_string_lossy(),
        );
        eprintln!(
            " '{}' {}",
            linker_invocation.get_program().to_string_lossy(),
            linker_invocation
                .get_args()
                .map(|x| format!("'{}'", x.to_string_lossy()))
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
    // For now call gcc but consider calling the linker directly.
    let status = linker_invocation.status().expect("failed to execute 'gcc'");

    if !status.success() {
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
