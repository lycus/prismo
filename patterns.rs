use types;

pub enum Pat {
    // _
    AnyPattern,

    // A(a, b, c, ...)
    RecordPattern(@types::Rec, @[@Pat]),

    // (a, b, c, ...)
    TuplePattern(@[@Pat]),

    // a as a'
    BindingPattern(@types::Sym, @Pat),

    // some kind of literal
    LiteralPattern(@types::Lit),

    // a
    SymbolPattern(@types::Sym)
}
