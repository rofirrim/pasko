#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum RuntimeFunctionId {
    WriteTextfileStr,
    WriteTextfileI64,
    WriteTextfileF64,
    WriteTextfileBool,
    WriteTextfileChar,
    WriteTextfileNewline,
    ReadTextfileI64,
    ReadTextfileF64,
    ReadTextfileNewline,

    // Set operations
    SetNew,
    SetDispose,
    SetCopy,
    SetUnion,
    SetIntersection,
    SetDifference,
    SetContains,
    SetEqual,
    SetNotEqual,
    SetIsSubset,

    // Pointer operations
    PointerNew,
    PointerDispose,

    // Runtime
    Init,
    Finish,

    // File
    OutputFile,
    InputFile,

    RewriteFile,
    RewriteTextfile,

    ResetFile,
    ResetTextfile,

    PutFile,
    PutTextfile,

    GetFile,
    GetTextfile,

    BufferVarFile,
    BufferVarTextfile,

    EofFile,
    EofTextfile,

    EolnTextfile,

    SinF64,
    CosF64,
    ExpF64,
    LnF64,
    ArctanF64,
}
