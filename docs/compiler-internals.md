# Compiler internals

> This is likely to become outdated as the compiler evolves:
> "Use the source, Luke"

## Parser

The parser is based on the grammar of the spec (with changes where it would become unfeasible). We use `lalrpop` to parse it. `lalrpop` implements a LR(1) parser.

- The parser is in `pasko_frontend/src/pasko.lalrpop`

The parser uses a `Spanned` entity defined in `pasko_frontend/src/span.rs` that gives the entity a location (`SpanLoc`) and an external identifier (`SpanId`).

ASTs are defined in `pasko_frontend/src/ast.rs` and AST nodes are represented using `SpannedBox<T>` which a `Spanned<Option<Box<T>>` (defined in `pasko_frontend/src/span.rs`). The different components of an AST node are `SpannedBox` if those are other AST nodes or `Spanned` for non-AST (terminal) types such as `Spanned<String>`. 

When a node represents an alternative of other nodes (for instance `Expr` or `Stmt`) we use an `enum` that wraps a single class (without `Spanned`). Note that in this case the enum itself will be used as a `SpannedBox` elsewhere so it means all the variants share the same identifier and location, this is the reason why the variants wrap a class directly, not a `SpannedBox`.

### Lexer

The lexer is handwritten and found in `pasko_frontend/src/lexer.rs`. Uses a very simple `skip`/`peek` interface that internally is implemented with a very simple queue so we can peek ahead when the lexing requires it (e.g. `1..2` has to be lexed as `[int:1] [ellipsis] [int:2]` instead of `[real:1.] [dot] [int:2]`)

The lexer implements the `Iterator` trait that is used by the parser created by `lalrpop` to get the tokens. The lexer is responsible of correctly computing the span of the returned token (accidentally we use a `Spanned` type unrelated to the `Spanned<T>` described above).

## Visitor

In order to examine and mutate the trees created by the parser there is a visitor module.

This module defines 4 traits for visitors and the corresponding 4 traits for AST nodes.

- `Visitor` is an (immutable) visitor that cannot mutate the AST nodes. This is not used anywhere but is there for completeness.
  
  - A `Visitor` can visit a `Visitable` using `walk`.

- `VisitorMut` is a mutable visitor that cannot mutate the AST nodes. 
  
  - An example of this is the visitor used to dump the tree or the code generating one: we don't need to change the AST at this step but the visitor itself must be able to change its state while visiting the tree.
  
  - A `VisitorMut` can visit a `Visitable` using a `walk_mut`.

- `MutatingVisitor` is an (immutable) visitor that can mutate the AST nodes. This is not used anywhere but is there for completeness.
  
  - A `MutatingVisitor` can visit a `MutatingVisitable` using `mutating_walk`

- `MutatingVisitorMut` is a mutable visitor that can mutate the AST nodes.
  
  - This is used by the semantic phase to fix-up nodes that the parser did not have enough information to represent precisely. For instance, constants, variables and (0-argument) function references cannot be distinguished while parsing so they are fixed up while doing the semantic checks.
  
  - A `MutatingVisitorMut` can visit a `MutatingVisitable` using `mutating_walk_mut`.

The visitors are used in the following way:

- Create a class for the visitor you want to implement

- Make it implement the proper `{,Mutating}Visitor{,Mut}` you need.

- For each AST node that is a leaf, i.e. it does not contain other `SpannedBox<T>` inside you can implement a `visit_<my_class>` method that will be executed when you walk a tree of the `ast::MyClass` type (e.g. `visit_const_string_literal` for `ast::ConstStringLiteral`).

- For each AST node that contains other `SpannedBox<T>` inside it can be visited in two instants:
  
  - Before visiting the children, by implementing `visit_pre_<my_class>`. This method must retorn a `bool` value stating whether we want to automatically visit the children. The default method does nothing but return `true`, so the children are always visited.
  
  - After visiting the children, by implementing `visit_post_<my_class>`. This method is only invoked if the corresponding `visit_pre_<my_class>` returned true (recall that the default methode does this).
  
  - This mechanism gives you a lot of flexibility:
    
    - If you only are computing synthesised information (i.e. bottom-up) like it happens when typechecking expressions you only need to implement the post method. E.g. if you only implement `visit_post_bin_op` will already have visited the left hand side and the right hand side children.
    
    - If you need to compute the children information in a more precise order, then you override only the `visit_pre_<my_class>`, explicit walk the children you want and then you return `false`. Similarly you can use this if you do not want to visit the children at all.

- For AST nodes that represent alternatives (e.g. `Expr`, `Stmt`) you can also implement methods for them both `visit_pre_<my_class>` and `visit_post_<my_class>` but this is mostly useful for mutating visitors.
  
  - For instance, semantic has a `visit_pre_expr` that fixes ups variables that actually refer to constants or are function calls (to functions with 0 parameters). By doing this we can fix all the expressions regardless where they appear.

- The methods for visits always receive a decomposed version of the node: the class `T` (as wrapped by a `SpannedBox<T>`), the span of the class (`SpanLoc`) and the identifier (`SpanId`)
  
  - As mentioned above, we use enums (like `Expr` and `Stmt`) to represent nodes with many alternatives. In this case the span and identifier of the enum are passed to the method but the node is the one wrapped by the variant. For instance: `visit_post_bin_op` receives an `ast::ExprBinOp`  which is the class wrapped by the variant `Expr::BinOp`.
  
  - Mutating visitors will have to copy the location and span before `mutating_walk{,_mut}` because the span (`SpanLoc`) returned by a tree is a reference. Both `SpanLoc` and `SpanID` are `Copy`, though.

- To walk a node invoke the corresponding `{,mutating_}walk{,_mut}`.
  
  - For instance, top-level visitors such as semantic, dump or codegen start from a `SpannedBox<ast::Program>` that is originally provided by the parser.

To mutate a node, use `take` to move outs its node so you can put it elsewhere (we do this with conversions). You can also overwrite the current node with another valid node for it (i.e. enums like `Expr` will require a variant such as `Expr::BinOp`). This does not happen often and only `pasko_frontend/src/semantic.rs` contains instances of mutation.

## Semantic

In file `pasko_frontend/src/semantic.rs` we implement a visitor that is responsible for doing all the type checking, symbol and type management.

One thing that this pass does is to link the AST node (using the `id` of their corresponding `SpannedBox<T>`) to other entities such as symbols (`Symbol`), types (`Type`) and constant values (`Constant`).

`Symbol`, `Type` and `Constants` are owned by `SemanticContext` and have their own identifier.

For instance, expressions are linked to the `Type` of the expression (e.g. an expression like `1 div 2` is linked to the integer type while an expression like `1 / 2` is linked to the real type) . Entity references or declarations typically link the identifier part of the tree to the corresponding `Symbol`. The tree is actually linked against a `SymbolId` or `TypeId`. To get the actual object for further inspection, `SemanticContext` has getters that return references to the actual entities.

## Codegen

Codegen uses cranelift to emit an object module that implements the Pasko program.

Codegen is simpler than it looks and it is structured in several modules:

- `program `a visitor that handles and represents the top level entities: functions, global variables and the program statement (i.e. the entry point of the program).
  
  - Program emits one function at a time, to do this it instantiates a visitor defined in the module `function` whose purpose is to emit instructions.

- For each function or procedure (including the program statement) a module `function` is created. This is the one that emits the instructions.
  
  - The function visitor can access the information of `program`, which is basically module-level information (e.g. global variables or external/internal functions)

There are a other ancillary modules:

- `datalocation` defines `DataLocation` for `Symbol` of kind `SymbolKind::Variable`. A variable can either be a global variable (linked to a `cranelift_module::DataId`) or a local variable (linked to a `cranelift_codegen::ir::StackSlot`).

- `runtime` is a tuple that gathers all the runtime functions defined in `pasko-runtime`. Those are initialised in `program`.

The program statement of a Pasko program is emitted as the C `main` function and currently we expect to link the whole program like a C application.


