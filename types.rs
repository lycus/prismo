use core::libc::c_void;

use ast;
use patterns;

#[deriving(Eq)]
pub struct Sym(@str);

pub struct Rec {
    name: @str,
    fields: @[@Sym]
}

#[deriving(Eq)]
pub enum Lit {
    IntegerLiteral(int),
    StringLiteral(@str)
}

pub struct Fun {
    pattern: @[@patterns::Pat],
    body: ast::Exp
}

pub enum Val {
    // a
    Symbol(@Sym),

    // 1
    Integer(int),

    // 1.1
    Floating(float),

    // 'hello!'
    Bytes(@[char]),

    // "hello!"
    String(~str),

    // A(a, b, c, ...)
    Record(@Rec, @[@Val]),

    // f(a, b, ...) = ...
    Function(@[@Fun]),

    // <handle>
    Handle(c_void)
}
