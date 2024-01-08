use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitCode;

fn main() -> ExitCode {
    // Make sure tools have been built
    let mut cargo_invocation = Command::new(env!("CARGO"));
    cargo_invocation.arg("build");
    let mut topleveldir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    topleveldir.pop();
    cargo_invocation.arg(format!("--manifest-path={}/Cargo.toml", topleveldir.display()));

    eprintln!("Running:");
    eprintln!(
        " '{}' {}",
        cargo_invocation.get_program().to_string_lossy(),
        cargo_invocation
            .get_args()
            .map(|x| format!("'{}'", x.to_string_lossy()))
            .collect::<Vec<_>>()
            .join(" ")
    );

    let status = cargo_invocation.status().expect("failed to execute 'cargo'");
    if !status.success() {
        return ExitCode::FAILURE;
    }

    // println!("{}", env!("CARGO_MANIFEST_DIR"));

    // Run testsuit.
    let mut builddir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let mut bindir = builddir.clone();

    builddir.push("testsuite");

    // FIXME: I'm sure there is a neat way to fix this, perhaps using build.rs and configuring the proper lit.site.cfg?
    bindir.pop();
    bindir.push("debug");

    // We ignore the error in case the directory already exists (we can't avoid a TOCTOU here though).
    let _result = fs::create_dir(builddir.clone());

    let mut srcdir = env::current_dir().expect("could not obtain the current directory");
    srcdir.push("testsuite");

    println!("Binary executables = '{}'", bindir.display());
    println!("Testsuite source = '{}'", srcdir.display());
    println!("Testsuite build = '{}'", builddir.display());

    let mut lit_invocation = Command::new("lit");
    // extend-path
    lit_invocation.arg("--param");
    let mut param = OsString::from("tools-dir=");
    param.push(bindir);
    lit_invocation.arg(param);
    // builddir
    lit_invocation.arg("--param");
    let mut param = OsString::from("build-dir=");
    param.push(builddir);
    lit_invocation.arg(param);
    // -sv
    lit_invocation.arg("-sv");
    // srcdir
    lit_invocation.arg(srcdir);

    // Display
    eprintln!("Running:");
    eprintln!(
        " '{}' {}",
        lit_invocation.get_program().to_string_lossy(),
        lit_invocation
            .get_args()
            .map(|x| format!("'{}'", x.to_string_lossy()))
            .collect::<Vec<_>>()
            .join(" ")
    );

    let status = lit_invocation.status().expect("failed to execute 'lit'. Check your PATH");
    if !status.success() {
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
