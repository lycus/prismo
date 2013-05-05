use ast;
use lexer;
use parser;

use interp::env;
use interp::types;

use core::hashmap::linear;
use core::path;
use core::to_bytes;

use std::sync;

/*
 * aster - AST EvaulatoR
 *
 * aster evaluates a Prismo program via reduction of the AST.
 *
 * "It's pretty terrible."
 * -- Tony, 2013
 */

impl to_bytes::IterBytes for ast::RecordName {
    fn iter_bytes(&self, lsb0: bool, f: to_bytes::Cb) {
        match self {
            &ast::RecordName(s) => s.iter_bytes(lsb0, f)
        }
    }
}

impl to_bytes::IterBytes for ast::DottedName {
    fn iter_bytes(&self, lsb0: bool, f: to_bytes::Cb) {
        match self {
            &ast::DottedName(xs) => xs.iter_bytes(lsb0, f)
        }
    }
}

pub struct Interp {
    record_types: @mut linear::LinearMap<@(ast::DottedName, ast::RecordName), @ast::RecordDeclaration>,
    import_paths: @mut [Path],
    argv: @[@str],
    current_frame: @Frame,
    gil: sync::Mutex
}

pub impl Interp {
    pub fn new(import_paths: @mut [Path], argv: @[@str]) -> Interp {
        Interp {
            record_types: @mut linear::LinearMap::new(),
            import_paths: import_paths,
            argv: argv,
            current_frame: @Frame::new_root_frame(),
            gil: sync::Mutex()
        }
    }
}

pub struct Frame {
    parent: @option::Option<Frame>,
    env: @mut env::Env<Interp>,
    file: @str,
    lineno: uint,
    exception: @option::Option<types::Val<Interp>>
}

pub impl Frame {
    fn new_root_frame() -> Frame {
        Frame {
            parent: @option::None,
            env: @mut env::Env::new(),
            file: @"<root frame>",
            lineno: 0,
            exception: @option::None
        }
    }
}

pub fn import(interp: @mut Interp, name: @ast::DottedName) -> () {
    let mut import_paths1 = copy interp.import_paths;

    let mut parts = (**name).to_owned().map(|x| (*x).to_owned());
    vec::reverse(parts);
    let filename = fmt!("%s.pr", *vec::head(parts));
    parts = vec::tail(parts).to_owned();
    vec::reverse(parts);

    for interp.import_paths.each_mut |import_path| {
        let mut p = copy *import_path;
        p.push_many(parts);
        p.push(filename);
    }

    /*for vec::tail(parts).each |sym| {
         import_path1.push(**sym);
    }
    import_path1.push(filename);
    */

    //let interp1 = Interp::new(import_path1, interp.argv);
}

pub fn run(interp: @mut Interp, prog: ast::Program) -> () {
    for prog.records.each |rec| {
        interp.record_types.insert(@(ast::DottedName(@[]), rec.name), @*rec);
    }
}
