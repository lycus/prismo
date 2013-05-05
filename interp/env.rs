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

impl to_bytes::IterBytes for ast::DottedName {
    fn iter_bytes(&self, lsb0: bool, f: to_bytes::Cb) {
        match self {
            &ast::DottedName(s) => s.iter_bytes(lsb0, f)
        }
    }
}

pub struct Env<Interp> {
    parent: @mut option::Option<@mut Env<Interp>>,
    vars: @mut linear::LinearMap<ast::DottedName, @types::Val<Interp>>
}

pub impl <Interp> Env<Interp> {
    fn new() -> Env<Interp> {
        Env {
            parent: @mut option::None,
            vars: @mut linear::LinearMap::new()
        }
    }
}
