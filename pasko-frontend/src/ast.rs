// ASTs
use crate::span::{Spanned, SpannedBox};
use std::fmt;

#[derive(Debug)]
pub struct Program(pub SpannedBox<ProgramHeading>, pub SpannedBox<ProgramBlock>);

#[derive(Debug)]
pub struct ProgramHeading(pub Spanned<String>, pub Vec<Spanned<String>>);

#[derive(Debug)]
pub struct ProgramBlock(pub SpannedBox<Block>);

#[derive(Debug)]
pub struct Block(
    pub Option<SpannedBox<LabelDeclarationPart>>,
    pub Option<SpannedBox<ConstantDefinitionPart>>,
    pub Option<SpannedBox<TypeDefinitionPart>>,
    pub Option<SpannedBox<VariableDeclarationPart>>,
    pub Option<SpannedBox<ProcedureAndFunctionDeclarationPart>>,
    pub SpannedBox<StatementPart>,
);

#[derive(Debug)]
pub struct StatementPart(pub SpannedBox<Stmt>);

#[derive(Debug)]
pub struct StmtLabel(pub Spanned<usize>, pub SpannedBox<Stmt>);
#[derive(Debug)]
pub struct StmtAssignment(pub SpannedBox<Assig>, pub SpannedBox<Expr>);
#[derive(Debug)]
pub struct StmtProcedureCall(pub Spanned<String>, pub Option<Vec<SpannedBox<Expr>>>);
#[derive(Debug)]
pub struct StmtGoto(pub Spanned<usize>);
#[derive(Debug)]
pub struct StmtCompound(pub Vec<SpannedBox<Stmt>>);
#[derive(Debug)]
pub struct StmtIf(
    pub SpannedBox<Expr>,
    pub SpannedBox<Stmt>,
    pub Option<SpannedBox<Stmt>>,
);
#[derive(Debug)]
pub struct CaseListElement(pub Vec<SpannedBox<Const>>, pub SpannedBox<Stmt>);
#[derive(Debug)]
pub struct StmtCase(pub SpannedBox<Expr>, pub Vec<SpannedBox<CaseListElement>>);
#[derive(Debug)]
pub struct StmtRepeatUntil(pub Vec<SpannedBox<Stmt>>, pub SpannedBox<Expr>);
#[derive(Debug)]
pub struct StmtWhileDo(pub SpannedBox<Expr>, pub SpannedBox<Stmt>);
#[derive(Debug)]
pub struct StmtFor(
    pub ForKind,
    pub SpannedBox<AssigVariable>,
    pub SpannedBox<Expr>,
    pub SpannedBox<Expr>,
    pub SpannedBox<Stmt>,
);
#[derive(Debug)]
pub struct StmtWith(pub Vec<Spanned<String>>, pub SpannedBox<Stmt>);

#[derive(Debug)]
pub struct StmtEmpty;

#[derive(Debug)]
pub enum Stmt {
    Label(StmtLabel),
    Assignment(StmtAssignment),
    ProcedureCall(StmtProcedureCall),
    Goto(StmtGoto),
    Compound(StmtCompound),
    If(StmtIf),
    Case(StmtCase),
    RepeatUntil(StmtRepeatUntil),
    WhileDo(StmtWhileDo),
    For(StmtFor),
    With(StmtWith),
    Empty(StmtEmpty),
}

#[derive(Debug)]
pub enum ForKind {
    To,
    DownTo,
}

#[derive(Debug)]
pub struct AssigVariable(pub Spanned<String>);
#[derive(Debug)]
pub struct AssigArrayAccess(pub SpannedBox<Assig>, pub Vec<SpannedBox<Expr>>);
#[derive(Debug)]
pub struct AssigFieldAccess(pub SpannedBox<Assig>, pub Spanned<String>);
#[derive(Debug)]
pub struct AssigPointerDeref(pub SpannedBox<Assig>);

#[derive(Debug)]
pub enum Assig {
    Variable(AssigVariable),
    ArrayAccess(AssigArrayAccess),
    FieldAccess(AssigFieldAccess),
    PointerDeref(AssigPointerDeref),
}

#[derive(Debug)]
pub struct LabelDeclarationPart(pub Vec<Spanned<usize>>);

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Sign {
    Plus,
    Minus,
}

#[derive(Debug)]
pub struct ConstInteger(pub Spanned<i64>);
#[derive(Debug)]
pub struct ConstReal(pub Spanned<f64>);
#[derive(Debug)]
pub struct ConstNamed(pub Spanned<String>);
#[derive(Debug)]
pub struct ConstMinusNamed(pub Spanned<String>);
#[derive(Debug)]
pub struct ConstStringLiteral(pub Spanned<String>);
#[derive(Debug)]
pub struct ConstNil();
#[derive(Debug)]
pub struct ConstBool(pub Spanned<bool>);

#[derive(Debug)]
pub enum Const {
    Integer(ConstInteger),
    Real(ConstReal),
    Named(ConstNamed),
    MinusNamed(ConstMinusNamed),
    StringLiteral(ConstStringLiteral),
    Nil(ConstNil),
    Bool(ConstBool),
}

#[derive(Debug)]
pub struct ConstantDefinition(pub Spanned<String>, pub SpannedBox<Const>);

#[derive(Debug)]
pub struct ConstantDefinitionPart(pub Vec<SpannedBox<ConstantDefinition>>);

#[derive(Debug)]
pub struct TypeDefinition(pub Spanned<String>, pub SpannedBox<TypeDenoter>);

#[derive(Debug)]
pub struct TypeDefinitionPart(pub Vec<SpannedBox<TypeDefinition>>);

#[derive(Debug)]
pub struct TypeIdentifier(pub Spanned<String>);
#[derive(Debug)]
pub struct EnumeratedType(pub Vec<Spanned<String>>);
#[derive(Debug)]
pub struct SubrangeType(pub SpannedBox<Const>, pub SpannedBox<Const>);
#[derive(Debug)]
pub struct ArrayType(
    pub Option<Spanned<String>>,
    pub Vec<SpannedBox<TypeDenoter>>,
    pub SpannedBox<TypeDenoter>,
);

#[derive(Debug)]
pub struct FieldList(
    pub Option<Vec<SpannedBox<RecordSection>>>,
    pub Option<SpannedBox<VariantPart>>,
);

#[derive(Debug)]
pub struct VariantPart(
    pub SpannedBox<VariantSelector>,
    pub Vec<SpannedBox<Variant>>,
);

#[derive(Debug)]
pub struct VariantSelector(pub Option<Spanned<String>>, pub SpannedBox<TypeDenoter>);

#[derive(Debug)]
pub struct Variant(pub Vec<SpannedBox<Const>>, pub SpannedBox<FieldList>);

#[derive(Debug)]
pub struct RecordType(pub Option<Spanned<String>>, pub SpannedBox<FieldList>);
#[derive(Debug)]
pub struct SetType(pub Option<Spanned<String>>, pub SpannedBox<TypeDenoter>);
#[derive(Debug)]
pub struct FileType(pub Option<Spanned<String>>, pub SpannedBox<TypeDenoter>);

#[derive(Debug)]
pub struct PointerType(pub SpannedBox<TypeDenoter>);

#[derive(Debug)]
pub enum TypeDenoter {
    TypeIdentifier(TypeIdentifier),
    EnumeratedType(EnumeratedType),
    SubrangeType(SubrangeType),
    ArrayType(ArrayType),
    RecordType(RecordType),
    SetType(SetType),
    FileType(FileType),
    PointerType(PointerType),
}

#[derive(Debug)]
pub struct RecordSection(pub Vec<Spanned<String>>, pub SpannedBox<TypeDenoter>);

#[derive(Debug)]
pub struct VariableDeclarationPart(pub Vec<SpannedBox<VariableDeclaration>>);

#[derive(Debug)]
pub struct VariableDeclaration(pub Vec<Spanned<String>>, pub SpannedBox<TypeDenoter>);

#[derive(Debug)]
pub struct ProcedureForward(
    pub Spanned<String>,
    pub Option<Vec<SpannedBox<FormalParameter>>>,
);
#[derive(Debug)]
pub struct ProcedureDefinition(
    pub Spanned<String>,
    pub Option<Vec<SpannedBox<FormalParameter>>>,
    pub SpannedBox<Block>,
);

#[derive(Debug)]
pub enum ProcedureDeclaration {
    Forward(ProcedureForward),
    Definition(ProcedureDefinition),
}

#[derive(Debug)]
pub struct ProcedureAndFunctionDeclarationPart(
    pub Vec<SpannedBox<ProcedureAndFunctionDeclaration>>,
);

#[derive(Debug)]
pub struct Procedure(pub SpannedBox<ProcedureDeclaration>);

#[derive(Debug)]
pub struct Function(pub SpannedBox<FunctionDeclaration>);

#[derive(Debug)]
pub enum ProcedureAndFunctionDeclaration {
    Procedure(Procedure),
    Function(Function),
}

#[derive(Debug)]
pub struct FunctionForward(
    pub Spanned<String>,
    pub Option<Vec<SpannedBox<FormalParameter>>>,
    pub SpannedBox<TypeIdentifier>,
);
#[derive(Debug)]
pub struct FunctionDefinition(
    pub Spanned<String>,
    pub Option<Vec<SpannedBox<FormalParameter>>>,
    pub SpannedBox<TypeIdentifier>,
    pub SpannedBox<Block>,
);
#[derive(Debug)]
pub struct FunctionLateDefinition(pub Spanned<String>, pub SpannedBox<Block>);

#[derive(Debug)]
pub enum FunctionDeclaration {
    Forward(FunctionForward),
    Definition(FunctionDefinition),
    LateDefinition(FunctionLateDefinition),
}

#[derive(Debug)]
pub struct FormalParameterValue(pub Vec<Spanned<String>>, pub SpannedBox<TypeIdentifier>);
#[derive(Debug)]
pub struct FormalParameterVariable(pub Vec<Spanned<String>>, pub SpannedBox<TypeIdentifier>);
#[derive(Debug)]
pub struct FormalParameterProcedure(
    pub Spanned<String>,
    pub Option<Vec<SpannedBox<FormalParameter>>>,
);
#[derive(Debug)]
pub struct FormalParameterFunction(
    pub Spanned<String>,
    pub Option<Vec<SpannedBox<FormalParameter>>>,
    pub SpannedBox<TypeIdentifier>,
);

#[derive(Debug)]
pub enum FormalParameter {
    Value(FormalParameterValue),
    Variable(FormalParameterVariable),
    Procedure(FormalParameterProcedure),
    Function(FormalParameterFunction),
}

#[derive(Debug)]
pub struct ExprConst(pub SpannedBox<Const>);
#[derive(Debug)]
pub struct ExprSetLiteral(pub Vec<SpannedBox<Expr>>);
#[derive(Debug)]
pub struct ExprRange(pub SpannedBox<Expr>, pub SpannedBox<Expr>);
#[derive(Debug)]
pub struct ExprFunctionCall(pub Spanned<String>, pub Vec<SpannedBox<Expr>>);
#[derive(Debug)]
pub struct ExprVariable(pub SpannedBox<Assig>);
#[derive(Debug)]
pub struct ExprVariableReference(pub SpannedBox<Assig>);
#[derive(Debug)]
pub struct ExprParentheses(pub SpannedBox<Expr>);
#[derive(Debug)]
pub struct ExprUnOp(pub Spanned<UnaryOp>, pub SpannedBox<Expr>);
#[derive(Debug)]
pub struct ExprBinOp(
    pub Spanned<BinOperand>,
    pub SpannedBox<Expr>,
    pub SpannedBox<Expr>,
);
#[derive(Debug)]
pub struct ExprWriteParameter(
    pub SpannedBox<Expr>,
    pub SpannedBox<Expr>,
    pub Option<SpannedBox<Expr>>,
);

#[derive(Debug)]
pub struct ExprConversion(pub SpannedBox<Expr>);

#[derive(Debug)]
pub enum Expr {
    Const(ExprConst),
    SetLiteral(ExprSetLiteral),
    Range(ExprRange),
    FunctionCall(ExprFunctionCall),
    Variable(ExprVariable),
    VariableReference(ExprVariableReference),
    Parentheses(ExprParentheses),
    UnOp(ExprUnOp),
    BinOp(ExprBinOp),
    WriteParameter(ExprWriteParameter),
    Conversion(ExprConversion),
}

#[derive(Debug)]
pub enum BinOperand {
    // Relationals
    GreaterThan,
    GreaterOrEqualThan,
    LowerThan,
    LowerOrEqualThan,
    Equal,
    Different,
    InSet,
    // Additive
    Addition,
    Subtraction,
    LogicalOr,
    // Multiplicative
    Multiplication,
    RealDivision,
    IntegerDivision,
    Modulo,
    LogicalAnd,
}

impl fmt::Display for BinOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinOperand::GreaterThan => ">",
                BinOperand::GreaterOrEqualThan => ">=",
                BinOperand::LowerThan => "<",
                BinOperand::LowerOrEqualThan => "<=",
                BinOperand::Equal => "=",
                BinOperand::Different => "<>",
                BinOperand::InSet => "in",
                BinOperand::Addition => "+",
                BinOperand::Subtraction => "-",
                BinOperand::LogicalOr => "or",
                BinOperand::Multiplication => "*",
                BinOperand::RealDivision => "/",
                BinOperand::IntegerDivision => "div",
                BinOperand::Modulo => "mod",
                BinOperand::LogicalAnd => "and",
            }
        )
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Negation,
    LogicalNot,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::Plus => "+",
                UnaryOp::Negation => "-",
                UnaryOp::LogicalNot => "not",
            }
        )
    }
}
