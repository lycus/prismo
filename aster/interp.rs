use ast;
use aster::env;
use aster::types;

pub fn run(env: @env::Env, prog: ast::Program) -> () {
    /*for prog.body.each |stmt| {
        eval_stmt(env, stmt);
    }*/
}
/*
fn bind(env: @env::Env, pat: &ast::Pat, val: @types::Val) -> bool {
    fail!(~":V")
}

fn eval_exp(env: @env::Env, exp: &ast::Exp) -> Result<types::Val, types::Val> {
    fail!(~":V")
}

fn eval_stmt(env: @env::Env, stmt: &ast::Stmt) -> Result<types::Val, types::Val> {
    match stmt {
        ast::LetBindingStatement(pat, exp) => bind(env, pat, eval_exp(exp)),
        ast::ReassignmentStatement(sym, exp) => env.rebind(sym, eval_exp(exp))
    }
}
*/
