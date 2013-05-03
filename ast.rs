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
    IntegerLiteral(int),
    StringLiteral(@str)
}

pub enum Pat {
    // _
    AnyPattern,

    // A(a, b, c, ...)
    RecordPattern(@RecordName, @[@Pat]),

    // (a, b, c, ...)
    TuplePattern(@[@Pat]),

    // a as a'
    BindingPattern(@Sym, @Pat),

    // some kind of literal
    LiteralPattern(@Lit),

    // a
    SymbolPattern(@Sym)
}

pub enum BareExp {
    // _
    UnderscoreExpression,

    // if a then b [ else c ]
    IfThenElseExpression(@Exp, @Exp, @Exp),

    // while a do b
    WhileDoExpression(@Exp, @Exp),

    // a OP b
    BinaryExpression(@Exp, @str, @Exp),

    // some kind of literal
    LiteralExpression(@Lit),

    // match a with case b' -> b case c' -> c
    MatchExpression(@Exp, @[(@Pat, @Exp)]),

    // { a; b; c }
    ImplicitBlockExpression(@[@Exp]),

    // { a; b; c; }
    BlockExpression(@[@Exp]),

    // a(b, c, d, ...)
    CallExpression(@Exp, @[@Exp]),

    // (a, b, c, d)
    TupleExpression(@[@Exp]),

    // fn (a) -> b
    LambdaExpression(@Pat, @Exp),

    // a = b
    DeclaredBindingExpression(@Pat, @Exp),

    // a := b
    NonlocalBindingExpression(@Pat, @Exp),

    // a.b
    RecordAccessExpression(@Exp, @Sym),

    // a:b
    RecordFunctionBindingExpression(@Exp, @Sym),

    // a
    SymbolExpression(@Sym)
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
    body: @[Exp]
}
