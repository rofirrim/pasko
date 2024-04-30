# Pasko compiler

This is a very simple compiler whose goal is to implement [ISO 7185:1990](https://archive.org/details/iso-iec-7185-1990-Pascal).

> ⚠️ This is still very much in MVP state, so don't expect much yet. ⚠️

It is implemented in [rust](https://www.rust-lang.org) and uses the following main crates as dependences:

- [lalrpop](https://crates.io/crates/lalrpop) for the syntactic parser
- [cranelift](https://cranelift.dev) for the code generation
- [clap](https://crates.io/crates/clap) for the command arguments handling of the driver
- [lazy_static](https://crates.io/crates/lazy_static) for global initialization of internal compiler state
- [paste](https://crates.io/crates/paste) for a macro that helps with code generation of visitor

### Goals

- Implement all of ISO 7185:1990 up to level 1

### Non-goals

- Extensions to make it nicer, closer to Turbo Pascal or to ISO 10206:1990

### How to build

##### Requirements

- An x86-64 Linux machine
  
  - more platforms may be added in the future

- Rust (tested with 1.75)

- Meson (tested with 1.3.1)
  
  - Used to build the runtime

- GCC
  
  - Used to build the runtime
  
  - Used to link the pasko programs

##### Compiler

1. Clone the repository

2. At the top level: `cargo build`

##### Runtime

1. Enter `pasko-runtime`

2. `meson setup builddir`

3. `cd builddir`

4. `make`

##### Tests

To run the tests you will need both LLVM's `FileCheck` and `lit` installed and available in your path.

`cargo test` will invoke the tests

The main testsuite is in `pasko-testing/testsuite`.

### How to use

**Note:** an installation procedure is not yet in place

From inside the build-directory

1. `cargo run -- myprogram.pas --pasko-runtime=<top-level-dir>/pasko-runtime/builddir`

2. If this succeeds you will have a `myprogram` file that you can execute

### Missing features

⚠️ Still many missing features! ⚠️

| Feature                                                | State | Notes                                                       |
| ------------------------------------------------------ | ----- | ----------------------------------------------------------- |
| Variadic types                                         | ❌     | Not even supported in the parser yet.                       |
| Set types                                              | ❌     |                                                             |
| File type                                              | ❌     |                                                             |
| Forward declarations of procedures and functions       | ❌     |                                                             |
| Conformable arrays in functions and procedures         | ❌     |                                                             |
| Nested procedures and functions                        | ❌     |                                                             |
| Required procedures/functions: Math                    | ❌     |                                                             |
| Required procedures/functions: File functions          | ❌     |                                                             |
| Required procedures/functions: I/O                     | ⚠️    | Only `writeln` and `readln` without a file  are implemented. |
| Total digits and fraction width in `write` / `writeln` | ❌     | Runtime is ready. Crashes in codegen.                       |
| `input` and `output` support in `program`              | ❌     | They are ignored                                            |

##### I found a bug / I want to contribute

Please send me an email.

In order to keep my sanity at bay, this project does not have issues enabled.

#### Licence

The pasko components are all GPLv3.
