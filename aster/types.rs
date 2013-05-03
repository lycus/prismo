use core::libc::c_void;

use ast;

pub struct Fun {
    pattern: @[@ast::Pat],
    body: ast::Exp
}

pub enum Val {
    // a
    Symbol(@ast::Sym),

    // 1
    Integer(int),

    // 1.1
    Floating(float),

    // 'hello!'
    Bytes(@[char]),

    // "hello!"
    String(~str),

    // A(a, b, c, ...)
    Record(@ast::RecordDeclaration, @[@Val]),

    // f(a, b, ...) = ...
    Function(@[@Fun]),

    // <handle>
    Handle(c_void)
}
