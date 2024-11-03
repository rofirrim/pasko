use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitCode;

fn run_single_test(
    pasko_binary: &PathBuf,
    srcdir: &PathBuf,
    test_name: &str,
) -> Result<(), String> {
    let mut test_file = srcdir.clone();
    test_file.push(test_name);

    let mut pasko_invocation = Command::new(pasko_binary);
    pasko_invocation.args(["-c", test_file.to_str().unwrap()]);

    eprintln!("Running:");
    eprintln!(
        " '{}' {}",
        pasko_invocation.get_program().to_string_lossy(),
        pasko_invocation
            .get_args()
            .map(|x| format!("'{}'", x.to_string_lossy()))
            .collect::<Vec<_>>()
            .join(" ")
    );

    let status = pasko_invocation
        .status()
        .expect("failed to execute 'pasko'. Check your PATH");

    if status.success() {
        Ok(())
    } else {
        Err(test_name.to_string())
    }
}

fn run_tests(pasko_binary: &PathBuf, srcdir: &PathBuf) -> Result<(), String> {
    run_single_test(&pasko_binary, &srcdir, "iso7185pat.pas")?;

    Ok(())
}

fn main() -> ExitCode {
    let mut cargo_invocation = Command::new(env!("CARGO"));
    cargo_invocation.arg("build");
    let mut topleveldir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    topleveldir.pop();
    cargo_invocation.arg(format!(
        "--manifest-path={}/Cargo.toml",
        topleveldir.display()
    ));

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

    let status = cargo_invocation
        .status()
        .expect("failed to execute 'cargo'");
    if !status.success() {
        return ExitCode::FAILURE;
    }

    // println!("{}", env!("CARGO_MANIFEST_DIR"));

    // Run testsuit.
    let mut builddir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let mut bindir = builddir.clone();

    builddir.push("standard-test-moore");

    // FIXME: I'm sure there is a neat way to do this.
    bindir.pop();
    bindir.push("debug");

    // We ignore the error in case the directory already exists (we can't avoid a TOCTOU here though).
    let _result = fs::create_dir(builddir.clone());

    let mut srcdir = env::current_dir().expect("could not obtain the current directory");
    srcdir.push("standard-test-moore");

    let mut pasko_binary = bindir.clone();
    pasko_binary.push("pasko-driver");

    println!("Pasko binary = '{}'", pasko_binary.display());
    println!("Testsuite source = '{}'", srcdir.display());
    println!("Testsuite build = '{}'", builddir.display());

    match run_tests(&pasko_binary, &srcdir) {
        Ok(()) => {
            return ExitCode::SUCCESS;
        }
        Err(name) => {
            eprintln!("Test '{}' failed", name);
            return ExitCode::FAILURE;
        }
    }
}
