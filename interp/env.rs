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

pub struct Env<Interp> {
    parent: @option::Option<@mut Env<Interp>>,
    vars: @mut linear::LinearMap<ast::Sym, @types::Val<Interp>>
}

pub impl <Interp> Env<Interp> {
    fn new() -> Env<Interp> {
        Env {
            parent: @option::None,
            vars: @mut linear::LinearMap::new()
        }
    }
}

pub fn declare<Interp>(env: @mut Env<Interp>, sym: &ast::Sym, val: @types::Val<Interp>) -> bool {
    env.vars.insert(*sym, val)
}

pub fn find_containing_env<Interp>(env: @mut Env<Interp>, sym: &ast::Sym) -> option::Option<@mut Env<Interp>> {
    match env.vars.find(sym) {
        option::None => match *env.parent {
            option::None => option::None,
            option::Some(p) => find_containing_env(p, sym)
        },
        option::Some(_) => option::Some(env)
    }
}
