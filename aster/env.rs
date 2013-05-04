use core::hashmap::linear;
use core::to_bytes;

use ast;
use aster::types;

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

    fn derive(parent: @mut Env) -> Env {
        let mut env = Env::new();
        env.parent = option::Some(parent);
        env
    }

    fn find(&mut self, sym: &ast::Sym) -> option::Option<@types::Val> {
        match self.vars.find(sym) {
            option::None => match self.parent {
                option::Some(parent) => parent.find(sym),
                option::None => option::None
            },

            option::Some(x) => option::Some(*x)
        }
    }

    fn insert(&mut self, sym: &ast::Sym, val: @types::Val) -> bool {
        self.vars.insert(*sym, val)
    }
}
