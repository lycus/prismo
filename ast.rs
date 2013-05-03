use patterns;
use types;

pub struct Exp {
    exp: BareExp,
    lineno: uint
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
    LiteralExpression(@types::Lit),

    // match a with case b' -> b case c' -> c
    MatchExpression(@Exp, @[(@patterns::Pat, @Exp)]),

    // { a; b; c }
    ImplicitBlockExpression(@[@Exp]),

    // { a; b; c; }
    BlockExpression(@[@Exp]),

    // a(b, c, d, ...)
    CallExpression(@Exp, @[@Exp]),

    // (a, b, c, d)
    TupleExpression(@[@Exp]),

    // fn (a) -> b
    LambdaExpression(@patterns::Pat, @Exp),

    // a = b
    DeclaredBindingExpression(@patterns::Pat, @Exp),

    // a := b
    NonlocalBindingExpression(@patterns::Pat, @Exp),

    // a.b
    RecordAccessExpression(@Exp, @types::Sym),

    // a:b
    RecordFunctionBindingExpression(@Exp, @types::Sym),

    // a
    SymbolExpression(@types::Sym)
}
