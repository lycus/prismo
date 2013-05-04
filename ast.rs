pub struct Exp {
    exp: BareExp,
    lineno: uint
}

#[deriving(Eq)]
pub struct Sym(@str);

#[deriving(Eq)]
pub struct RecordName(@str);

#[deriving(Eq)]
pub struct ModuleName(@[Sym]);

#[deriving(Eq)]
pub enum Lit {
    IntegerLiteral(@str),
    FloatingLiteral(@str),
    StringLiteral(@str),
    BytesLiteral(@str)
}

pub enum Pat {
    // _
    AnyPattern,

    // ...
    ManyPattern,

    // A(a, b, c, ...)
    RecordPattern(RecordName, @[Pat]),

    // a(b, c, ...)
    FunctionPattern(Sym, @[Pat]),

    // a | b
    DisjunctivePattern(@Pat, @Pat),

    // a & b
    ConjunctivePattern(@Pat, @Pat),

    // [a, b, c, ...]
    ListPattern(@[Pat]),

    // a as a'
    BoundPattern(Sym, @Pat),

    // some kind of literal
    LiteralPattern(Lit),

    // a
    SymbolPattern(Sym)
}

pub enum BareExp {
    // _
    UnderscoreExpression,

    // if a then b [ else c ]
    IfThenElseExpression(@Exp, @Exp, option::Option<@Exp>),

    // while a do b
    WhileDoExpression(@Exp, @Exp),

    // match a with case b' -> b case c' -> c
    MatchWithExpression(@Exp, @[(Pat, Exp)]),

    // a OP b
    BinaryExpression(@Exp, @str, @Exp),

    // some kind of literal
    LiteralExpression(Lit),

    // { a; b; c } or { a; b; c; } if boolean field is true
    BlockExpression(@[Stmt], bool),

    // a(b, c, d, ...)
    CallExpression(@Exp, @[Exp]),

    // [a, b, c, d]
    ListExpression(@[Exp]),

    // fn (a) -> b
    LambdaExpression(@[Pat], @Exp),

    // a.b
    RecordAccessExpression(@Exp, Sym),

    // a:b
    RecordFunctionBindExpression(@Exp, Sym),

    // a
    SymbolExpression(Sym),

    // A
    RecordNameExpression(RecordName)
}

pub struct Stmt {
    stmt: BareStmt,
    lineno: uint
}

pub enum BareStmt {
    // let a = b
    LetBindingStatement(Pat, Exp),

    // a = b
    ReassignmentStatement(Sym, Exp),

    // ...
    ExpressionStatement(Exp)
}

pub fn mk_expression_statement(exp: Exp) -> Stmt {
    Stmt {
        stmt: ExpressionStatement(exp),
        lineno: exp.lineno
    }
}

pub struct ImportDeclaration {
    module: ModuleName,
    qualified: bool,
    lineno: uint
}

pub struct RecordDeclaration {
    name: RecordName,
    slots: @[Sym],
    lineno: uint
}

pub struct Program {
    imports: @[ImportDeclaration],
    records: @[RecordDeclaration],
    body: @[Stmt]
}
