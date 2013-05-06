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
    root: @mut env::Env<Interp>,
    argv: @[@str],
    filename: @str,
    current_frame: @option::Option<@mut Frame>
}

pub impl Interp {
    pub fn new(import_paths: @[Path], argv: @[@str], filename: @str) -> Interp {
        Interp {
            record_types: @mut linear::LinearMap::new(),
            import_paths: import_paths,
            root: @mut env::Env::new(),
            argv: argv,
            filename: filename,
            current_frame: @option::None
        }
    }
}

pub struct Frame {
    parent: @option::Option<@mut Frame>,
    env: @mut env::Env<Interp>,
    file: @str,
    lineno: uint,
    exception: @option::Option<types::Val<Interp>>
}

pub impl Frame {
    pub fn new(interp: @mut Interp, env: @mut env::Env<Interp>, file: @str, lineno: uint) -> Frame {
        Frame {
            parent: interp.current_frame,
            env: env,
            file: file,
            lineno: lineno,
            exception: @option::None
        }
    }
}

pub fn import_module(interp: @mut Interp, name: &ast::DottedName, qualified: bool) -> () {
    let module_parts = (**name).to_owned().map(|x| (*x).to_owned());
    let mut parts = copy module_parts;

    vec::reverse(parts);
    let module_name = copy parts[0];
    let filename = fmt!("%s.pr", module_name);
    parts = vec::tail(parts).to_owned();
    vec::reverse(parts);

    let mut found = false;
    let mut paths = interp.import_paths.to_owned();

    for paths.each_mut |import_path| {
        // make a path inside the import path and check if that exists
        let base_path = (copy *import_path).push_many(parts);
        let path = base_path.push(filename);

        if os::path_exists(&path) {
            // make a copy of the import paths then give them to a new sub-interpreter
            let mut import_paths1 = interp.import_paths.to_owned();
            import_paths1.unshift(base_path);

            let interp1 = @mut Interp::new(at_vec::from_owned(import_paths1), interp.argv, path.to_str().to_managed());

            // run the file in the sub-interpreter
            run_file(interp1, &path);

            if qualified {
                // qualified imports symbols from child interpreter into a module
                interp.root.vars.insert(ast::Sym(module_name.to_managed()),
                                        @types::Module(interp1.root.vars));
            } else {
                // otherwise we just plop all the symbols in
                for interp1.root.vars.each_key |k| {
                    interp.root.vars.insert(*k,
                                            *interp1.root.vars.find(k).unwrap());
                }
            }

            found = true;
            break;
        }
    };

    if !found {
        fail!(fmt!("couldn't import module %s", str::connect(module_parts, ".")))
    };
}

fn unify_pattern_basic(interp: @mut Interp, env: @mut env::Env<Interp>, pat: &ast::Pat, val: @types::Val<Interp>) -> () {
    match *pat {
        ast::AnyPattern => (),
        ast::SymbolPattern(sym) => { env.vars.insert(sym, val); },
        _ => fail!(~":V")
    };
}

fn unify_pattern_let(interp: @mut Interp, env: @mut env::Env<Interp>, pat: &ast::LetPat, val: @types::Val<Interp>) -> () {
    match *pat {
        ast::BasicPattern(pat) => unify_pattern_basic(interp, env, &pat, val),
        _ => fail!(~":V")
    };
}

fn unescape_str(inp: &str) -> ~str {
    let mut out = ~[];
    let mut i = 1;
    let mut slice = inp.slice(1, inp.len() - 1);

    while slice.len() > 0 {
        out += ~[if slice.char_at(0) == '\\' {
            match slice.char_at(1) {
                '"' => {
                    i += 1;
                    '"'
                },
                '\'' => {
                    i += 1;
                    '\''
                },
                '\\' => {
                    i += 1;
                    '\\'
                },
                'n' => {
                    i += 1;
                    '\n'
                },
                'r' => {
                    i += 1;
                    '\r'
                },
                't' => {
                    i += 1;
                    '\t'
                },
                'x' => {
                    if slice.len() >= 4 {
                        i += 3;
                        int::from_str_radix(slice.slice(2, 4), 16).unwrap() as char
                    } else {
                        'x'
                    }
                },
                'u' => {
                    if slice.len() >= 6 {
                        i += 5;
                        int::from_str_radix(slice.slice(2, 6), 16).unwrap() as char
                    } else {
                        'u'
                    }
                },
                _ => '\\'
            }
        } else {
            slice.char_at(0)
        }];
        i += 1;

        slice = inp.slice(i, inp.len() - 1);
    }

    str::from_chars(out)
}

fn eval_exp(interp: @mut Interp, env: @mut env::Env<Interp>, exp: &ast::Exp) -> types::Val<Interp> {
    // allocate a new frame
    let frame = @mut Frame::new(interp, env, interp.filename, exp.lineno);
    interp.current_frame = @option::Some(frame);

    let r = match exp.exp {
        ast::UnitExpression => types::Unit,
        ast::LiteralExpression(l) => {
            match l {
                ast::IntegerLiteral(i) => types::Integer(int::from_str(i).unwrap()),
                ast::FloatingLiteral(i) => types::Floating(float::from_str(i).unwrap()),
                ast::StringLiteral(s) => types::String(unescape_str(s).to_managed()),
                ast::BytesLiteral(s) => types::Bytes(at_vec::from_owned(unescape_str(s).to_bytes()))
            }
        },
        ast::LambdaExpression(pat, exp) => types::Function(@[types::Fun {
            pattern: pat,
            body: exp,
            env: env
        }]),
        _ => fail!(~":V")
    };

    interp.current_frame = match *interp.current_frame {
        option::None => @option::None,
        option::Some(frame) => frame.parent
    };

    r
}

fn exec_stmt(interp: @mut Interp, env: @mut env::Env<Interp>, stmt: &ast::Stmt) -> () {
    // allocate a new frame
    let frame = @mut Frame::new(interp, env, interp.filename, stmt.lineno);
    interp.current_frame = @option::Some(frame);

    match stmt.stmt {
        ast::LetBindingStatement(pat, exp) => unify_pattern_let(interp, env, &pat, @eval_exp(interp, env, &exp)),
        ast::ExpressionStatement(exp) => { eval_exp(interp, env, &exp); }
    };
    interp.current_frame = match *interp.current_frame {
        option::None => @option::None,
        option::Some(frame) => frame.parent
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

    for prog.body.each |stmt| {
        exec_stmt(interp, interp.root, stmt);
    }
}
