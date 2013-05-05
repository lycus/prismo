use ast;
use lexer;
use parser;

use interp::env;
use interp::types;

use core::hashmap::linear;
use core::to_bytes;

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
            &ast::DottedName(s) => s.iter_bytes(lsb0, f)
        }
    }
}

pub struct Interp {
    record_types: @mut linear::LinearMap<@(ast::DottedName, ast::RecordName), @ast::RecordDeclaration>,
    import_paths: @[Path],
    root: @env::Env<Interp>,
    argv: @[@str],
    current_frame: @option::Option<Frame>
}

pub impl Interp {
    pub fn new(import_paths: @[Path], argv: @[@str]) -> Interp {
        Interp {
            record_types: @mut linear::LinearMap::new(),
            import_paths: import_paths,
            root: @env::Env::new(),
            argv: argv,
            current_frame: @option::None
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

pub fn import_module(interp: @mut Interp, name: &ast::DottedName, qualified: bool) -> () {
    let module_parts = (**name).to_owned().map(|x| (*x).to_owned());
    let mut parts = copy module_parts;

    vec::reverse(parts);
    let filename = fmt!("%s.pr", *vec::head(parts));
    parts = vec::tail(parts).to_owned();
    vec::reverse(parts);

    let mut found = false;
    let mut paths = interp.import_paths.to_owned();

    for paths.each_mut |import_path| {
        let base_path = (copy *import_path).push_many(parts);
        let path = base_path.push(filename);

        if os::path_exists(&path) {
            let mut import_paths1 = interp.import_paths.to_owned();
            import_paths1.unshift(base_path);

            let interp1 = @mut Interp::new(at_vec::from_owned(import_paths1), interp.argv);

            run_file(interp1, &path);

            // qualified imports symbols from child interpreter
            if qualified {
                interp.root.vars.insert(ast::Sym(@"TODO"), @types::Module(interp1.root.vars));
            }

            found = true;
            break;
        }
    };

    if !found {
        fail!(fmt!("couldn't import module %s", str::connect(module_parts, ".")))
    };
}

pub fn run_file(interp: @mut Interp, path: &Path) -> () {
    match io::file_reader(path) {
        result::Ok(r) => {
            let tokens = lexer::lex(r);
            let program = parser::parse(tokens);
            run(interp, program);
        },
        result::Err(f) => fail!(f)
    }
}

pub fn run(interp: @mut Interp, prog: ast::Program) -> () {
    for prog.imports.each |imp| {
        import_module(interp, &imp.module, imp.qualified);
    }

    for prog.records.each |rec| {
        interp.record_types.insert(@(ast::DottedName(@[]), rec.name), @*rec);
    }
}
