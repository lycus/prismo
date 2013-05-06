use core::libc::c_void;
use core::hashmap::linear;

use ast;
use interp::env;

pub struct Fun<Interp> {
    pattern: @[ast::Pat],
    body: @ast::Exp,
    env: @mut env::Env<Interp>
}

pub enum Val<Interp> {
    // (),
    Unit,

    // a
    Symbol(@ast::Sym),

    // 1
    Integer(int),

    // 1.1
    Floating(float),

    // 'hello!'
    Bytes(@[u8]),

    // "hello!"
    String(@str),

    // [a, b, c, ...]
    List(@[@mut Val<Interp>]),

    // A(a, b, c, ...)
    Record(@ast::RecordDeclaration, @[@Val<Interp>]),

    // f(a, b, ...) = ...
    Function(@[Fun<Interp>]),

    // a.{...}
    Module(@mut linear::LinearMap<ast::Sym, @mut Val<Interp>>),

    // <routine>
    Routine(@fn (&Interp, &env::Env<Interp>) -> Val<Interp>),

    // <handle>
    Handle(c_void),
}

impl <Interp> Eq for Val<Interp> {
    fn eq(&self, other: &Val<Interp>) -> bool {
        match (self, other) {
            (&Unit, &Unit)                  => true,
            (&Integer(i), &Integer(j))      => i == j,
            (&Floating(i), &Floating(j))    => i == j,
            (&Bytes(s), &Bytes(t))          => s == t,
            (&String(s), &String(t))        => s == t,
            _                               => false
        }
    }

    fn ne(&self, other: &Val<Interp>) -> bool {
        !self.eq(other)
    }
}
