use core::hashmap::linear;
use core::to_bytes;

use ast;
use interp::types;

impl to_bytes::IterBytes for ast::Sym {
    fn iter_bytes(&self, lsb0: bool, f: to_bytes::Cb) {
        match self {
            &ast::Sym(s) => s.iter_bytes(lsb0, f)
        }
    }
}

pub struct Env {
    parent: option::Option<@mut Env>,
    vars: linear::LinearMap<ast::Sym, @types::Val>
}

pub impl Env {
    fn new() -> Env {
        Env {
            parent: option::None,
            vars: linear::LinearMap::new()
        }
    }
}
