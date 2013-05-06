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
            &ast::RecordName(n, s) => (n, s).iter_bytes(lsb0, f)
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
    record_types: @mut linear::LinearMap<@ast::RecordName, @ast::RecordDeclaration>,
    import_paths: @[Path],
    root: @mut env::Env<Interp>,
    argv: @[@str],
    filename: @str,
    loaded_files: @[Path], // TODO: implement me!
    current_frame: @mut option::Option<@mut Frame>
}

pub impl Interp {
    pub fn new(import_paths: @[Path], argv: @[@str], filename: @str) -> Interp {
        Interp {
            record_types: @mut linear::LinearMap::new(),
            import_paths: import_paths,
            root: @mut env::Env::new(),
            argv: argv,
            filename: filename,
            loaded_files: @[],
            current_frame: @mut option::None
        }
    }
}

fn wind(interp: @mut Interp, env: @mut env::Env<Interp>, lineno: uint) -> @mut Frame {
    // allocate a new frame
    let frame = @mut Frame::new(interp, env, interp.filename, lineno);
    interp.current_frame = @mut option::Some(frame);
    frame
}

fn unwind(interp: @mut Interp) -> () {
    interp.current_frame = match *interp.current_frame {
        option::None => @mut option::None,
        option::Some(frame) => frame.parent
    };
}

pub struct Frame {
    parent: @mut option::Option<@mut Frame>,
    env: @mut env::Env<Interp>,
    file: @str,
    lineno: uint,
    exception: @mut option::Option<@mut types::Val<Interp>>
}

pub impl Frame {
    pub fn new(interp: @mut Interp, env: @mut env::Env<Interp>, file: @str, lineno: uint) -> Frame {
        Frame {
            parent: interp.current_frame,
            env: env,
            file: file,
            lineno: lineno,
            exception: @mut option::None
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
                let module_name = ast::Sym(module_name.to_managed());

                // qualified imports symbols from child interpreter into a module
                env::declare(interp.root, &module_name,
                             @mut types::Module(interp1.root.vars));

                for interp1.record_types.mutate_values |k, v| {
                    interp.record_types.insert(match *k {
                        @ast::RecordName(modu, name) => @ast::RecordName(@ast::DottedName(**modu + [module_name]), name)
                    }, *v);
                }
            } else {
                // otherwise we just plop all the symbols in
                for interp1.root.vars.mutate_values |k, v| {
                    env::declare(interp.root, k, *v);
                }

                // import record types
                for interp1.record_types.mutate_values |k, v| {
                    interp.record_types.insert(*k, *v);
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

fn unify_pattern_basic(interp: @mut Interp, in_env: @mut env::Env<Interp>, pat: &ast::Pat, val: @mut types::Val<Interp>) -> bool {
    // allocate a temporary environment for pattern matching
    let mut env = @mut env::Env::new();

    if match *pat {
        ast::AnyPattern => true,
        ast::ManyPattern => false,
        ast::UnitPattern => match *val {
            types::Unit => true,
            _ => false
        },
        ast::LiteralPattern(lit) => *val == types:: gen_lit(lit),
        ast::SymbolPattern(sym) => {
            env::declare(env, &sym, val);
            true
        },
        ast::BoundPattern(sym, pat) => {
            let r = unify_pattern_basic(interp, env, pat, val);
            env::declare(env, &sym, val);
            r
        },
        ast::DisjunctivePattern(pat1, pat2) => {
            unify_pattern_basic(interp, env, pat1, val) || unify_pattern_basic(interp, env, pat2, val)
        },
        ast::ConjunctivePattern(pat1, pat2) => {
            unify_pattern_basic(interp, env, pat1, val) && unify_pattern_basic(interp, env, pat2, val)
        },
        ast::RecordPattern(name, pats) => match *val {
            types::Record(_, vals) => {
                // look up the decl in the interpreter and check if it's our decl
                match interp.record_types.find(&@name) {
                    option::None => false,
                    option::Some(_) => {
                        // check we're matching out the correct number of slots
                        // TODO: ManyPattern
                        if pats.len() != vals.len() {
                            false
                        } else if pats.len() == 0 {
                            true
                        } else {
                            let mut ok = true;
                            let mut i = 0;
                            while i < pats.len() {
                                ok = unify_pattern_basic(interp, env, &pats[i], vals[i]);
                                if !ok { break; };
                                i += 1;
                            }
                            ok
                        }
                    }
                }
            },
            _ => false
        },
        ast::ListPattern(pats) => match *val {
            types::List(vals) => {
                if vals.len() < pats.len() {
                    false
                } else {
                    match pats[pats.len() - 1] {
                        ast::ManyPattern => {
                            // pattern match with a spread argument
                            let mut ok = true;
                            let mut i = 0;
                            while i < pats.len() - 1 {
                                ok = unify_pattern_basic(interp, env, &pats[i], vals[i]);
                                if !ok { break; };
                                i += 1;
                            }
                            ok
                        },
                        _ => {
                            if vals.len() > pats.len() {
                                // no spread argument, :(
                                false
                            } else {
                                // pattern match with a full pattern list
                                let mut ok = true;
                                let mut i = 0;
                                while i < pats.len() {
                                    ok = unify_pattern_basic(interp, env, &pats[i], vals[i]);
                                    if !ok { break; };
                                    i += 1;
                                }
                                ok
                            }
                        }
                    }
                }
            },
            _ => false
        }
    } {
        env::merge_into_shallow(in_env, env);
        true
    } else {
        false
    }
}

fn unify_pattern_let(interp: @mut Interp, env: @mut env::Env<Interp>, pat: &ast::LetPat, val: @mut types::Val<Interp>) -> bool {
    match *pat {
        ast::BasicPattern(pat) => unify_pattern_basic(interp, env, &pat, val),
        _ => fail!(~"not implemented: full let-pattern unify")
    }
}

fn eval_exp(interp: @mut Interp, env: @mut env::Env<Interp>, exp: &ast::Exp) -> @mut types::Val<Interp> {
    let frame = interp.current_frame.unwrap();

    let r = match exp.exp {
        ast::UnitExpression => @mut types::Unit,
        ast::LiteralExpression(l) => @mut types::gen_lit(l),
        ast::LambdaExpression(pat, exp) => @mut types::Function(@[types::Fun {
            pattern: pat,
            body: exp,
            env: env
        }]),
        ast::ListExpression(exps) => {
            let mut vals = @[];

            for exps.each |exp| {
                vals += [eval_exp(interp, env, exp)];
                if !frame.exception.is_none() {
                    return @mut types::Unit;
                }
            }

            @mut types::List(vals)
        },
        ast::SymbolExpression(sym) => {
            match env::find(env, &sym) {
                option::Some(x) => x,
                option::None => {
                    frame.exception = @mut option::Some(@mut types::String(fmt!("symbol `%s` not found in scope", *sym).to_managed()));
                    @mut types::Unit
                }
            }
        },
        ast::AccessExpression(lhs, sym) => {
            match *eval_exp(interp, env, lhs) {
                types::Module(vars) => match vars.find(&sym) {
                    option::Some(x) => *x,
                    option::None => {
                        frame.exception = @mut option::Some(@mut types::String(fmt!("symbol `%s` not found in module", *sym).to_managed()));
                        @mut types::Unit
                    }
                },

                _ => fail!(~"not implemented: non-module access")
            }
        }
        _ => fail!(~"not implemented: full expression evaluation")
    };

    r
}

fn exec_stmt(interp: @mut Interp, env: @mut env::Env<Interp>, stmt: &ast::Stmt) -> () {
    let frame = interp.current_frame.unwrap();

    match stmt.stmt {
        ast::LetBindingStatement(pat, exp) => {
            if !unify_pattern_let(interp, env, &pat, eval_exp(interp, env, &exp)) {
                // uh oh, we need to throw an exception and unwind
                frame.exception = @mut option::Some(@mut types::String(@"pattern match refuted"));
            }
        },
        ast::ExpressionStatement(exp) => { eval_exp(interp, env, &exp); }
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
        interp.record_types.insert(@rec.name, @*rec);
        let ast::RecordName(_, r) = rec.name;

        env::declare(interp.root, &ast::Sym(r), @mut if rec.slots.len() == 0 {
            types::Record(@*rec, @[])
        } else {
            types::RecordConstructor(@*rec)
        });
    }

    for prog.body.each |stmt| {
        let mut frame = wind(interp, interp.root, stmt.lineno);

        exec_stmt(interp, interp.root, stmt);

        match *frame.exception {
            option::None => unwind(interp),
            option::Some(f) => {
                let mut current_frame = frame;
                let mut buf = ~[fmt!("%?", f)];
                loop {
                    buf += [fmt!("        from %s:%u", frame.file, frame.lineno)];
                    if current_frame.parent.is_none() { break; }
                    current_frame = current_frame.parent.unwrap();
                }
                io::println(str::connect(buf, "\n"));
                fail!(~"uncaught aster exception");
            }
        }
    }
}
