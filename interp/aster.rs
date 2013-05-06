use ast;
use lexer;
use parser;

use interp::env;
use interp::types;

use core::hashmap::linear;
use core::to_bytes;

use std::sort;

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
    pub fn new(import_paths: @[Path], argv: @[@str]) -> Interp {
        Interp {
            record_types: @mut linear::LinearMap::new(),
            import_paths: import_paths,
            root: @mut env::Env::new(),
            argv: argv,
            filename: @"?",
            loaded_files: @[],
            current_frame: @mut option::None
        }
    }
}

fn wind(interp: @mut Interp, env: @mut env::Env<Interp>, filename: @str, lineno: uint) -> @mut Frame {
    // allocate a new frame
    let frame = @mut Frame::new(interp, env, filename, lineno);
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

            let interp1 = @mut Interp::new(at_vec::from_owned(import_paths1), interp.argv);

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

fn unify_pattern_basic(interp: @mut Interp, env: @mut env::Env<Interp>, pat: &ast::Pat, val: @mut types::Val<Interp>) -> bool {
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
                } else if vals.len() == 0 {
                    true
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
        true
    } else {
        false
    }
}

fn unify_pattern_let(interp: @mut Interp, env: @mut env::Env<Interp>, pat: &ast::LetPat, val: @mut types::Val<Interp>) -> () {
    let frame = interp.current_frame.unwrap();

    match *pat {
        ast::BasicPattern(pat) => {
            let in_env = @mut env::Env::new();
            if unify_pattern_basic(interp, in_env, &pat, val) {
                env::merge_into_shallow(env, in_env);
            } else {
                frame.exception = @mut option::Some(@mut types::String(@"pattern match refuted"));
            }
        },
        _ => fail!(~"not implemented: full let-pattern unify")
    }
}

fn eval_exp(interp: @mut Interp, env: @mut env::Env<Interp>, exp: &ast::Exp) -> @mut types::Val<Interp> {
    let frame = interp.current_frame.unwrap();

    // check for a frame exception
    if frame.exception.is_some() {
        return @mut types::Unit;
    }

    let r = match exp.exp {
        ast::UnitExpression => @mut types::Unit,
        ast::LiteralExpression(l) => @mut types::gen_lit(l),
        ast::LambdaExpression(pat, exp) => @mut types::Function(@[types::Fun {
            pattern: pat,
            body: exp,
            env: env,
            filename: interp.filename
        }]),
        ast::ListExpression(exps) => {
            let mut vals = @[];

            for exps.each |exp| {
                vals += [eval_exp(interp, env, exp)];
                if frame.exception.is_some() {
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
            let v = eval_exp(interp, env, lhs);

            if frame.exception.is_some() {
                return @mut types::Unit;
            }
            match *v {
                types::Module(vars) => match vars.find(&sym) {
                    option::Some(x) => *x,
                    option::None => {
                        frame.exception = @mut option::Some(@mut types::String(fmt!("symbol `%s` not found in module", *sym).to_managed()));
                        @mut types::Unit
                    }
                },
                types::Record(decl, vars) => {
                    match decl.slots.position_elem(&sym) {
                        option::Some(i) => vars[i],
                        option::None => {
                            frame.exception = @mut option::Some(@mut types::String(fmt!("slot `%s` not found in record", *sym).to_managed()));
                            @mut types::Unit
                        }
                    }
                },
                _ => {
                    frame.exception = @mut option::Some(@mut types::String(fmt!("cannot acccess `%s` of non-record/module", *sym).to_managed()));
                    @mut types::Unit
                }
            }
        },
        ast::BlockExpression(stmts, trailing_semicolon) => {
            let mut body = stmts.slice(0, stmts.len());

            if !trailing_semicolon {
                body = stmts.slice(0, stmts.len() - 1);
            }

            for body.each |stmt| {
                exec_stmt(interp, env, stmt);
                if frame.exception.is_some() {
                    return @mut types::Unit;
                }
            }

            if !trailing_semicolon {
                match stmts[stmts.len() - 1].stmt {
                    ast::ExpressionStatement(exp) => {
                        let r = eval_exp(interp, env, &exp);
                        if frame.exception.is_some() {
                            return @mut types::Unit;
                        }
                        r
                    },
                    _ => {
                        frame.exception = @mut option::Some(@mut types::String(@"last statement of block is not an expression"));
                        @mut types::Unit
                    }
                }
            } else {
                @mut types::Unit
            }
        },
        ast::IfThenElseExpression(pred, conseq, option_alt) => {
            let pred_e = eval_exp(interp, env, pred);
            if frame.exception.is_some() {
                return @mut types::Unit;
            }

            match *pred_e {
                types::Boolean(p) => {
                    if p {
                        eval_exp(interp, env, conseq)
                    } else {
                        match option_alt {
                            option::Some(alt) => eval_exp(interp, env, alt),
                            _ => @mut types::Unit
                        }
                    }
                },

                _ => {
                    frame.exception = @mut option::Some(@mut types::String(@"expression does not yield a boolean"));
                    @mut types::Unit
                }
            }
        },
        ast::WhileDoExpression(pred, body) => {
            loop {
                let pred_e = eval_exp(interp, env, pred);
                if frame.exception.is_some() {
                    return @mut types::Unit;
                }

                match *pred_e {
                    types::Boolean(p) => {
                        if (!p) { break; }
                        eval_exp(interp, env, body);
                        if frame.exception.is_some() {
                            return @mut types::Unit;
                        }
                    },

                    _ => {
                        frame.exception = @mut option::Some(@mut types::String(@"expression does not yield a boolean"));
                    }
                }
            }

            @mut types::Unit
        },
        ast::MatchWithExpression(exp, cases) => {
            let mut r = @mut types::Unit;
            let v = eval_exp(interp, env, exp);

            if frame.exception.is_some() {
                return @mut types::Unit;
            }
            for cases.each |&(pat, body)| {
                let in_env = @mut env::Env::new();

                if unify_pattern_basic(interp, in_env, &pat, v) {
                    env::merge_into_shallow(env, in_env);
                    r = eval_exp(interp, env, &body);
                    break;
                }
            }
            r
        },
        ast::CallExpression(f, args) => {
            let f_e = eval_exp(interp, env, f);

            if frame.exception.is_some() {
                return @mut types::Unit;
            }

            let mut raw_vals = @[];
            for args.each |arg| {
                raw_vals += [eval_exp(interp, env, arg)];
                if frame.exception.is_some() {
                    return @mut types::Unit;
                }
            }

            let vals = @mut types::List(raw_vals);

            match *f_e {
                types::Function(funs) => {
                    let funs = sort::merge_sort(funs, |lhs, rhs| -ast::ListPattern((*lhs).pattern).specificity() < -ast::ListPattern((*rhs).pattern).specificity());

                    for funs.each |fun| {
                        let pat = ast::ListPattern(fun.pattern);
                        let child_env = @mut env::Env::new();
                        if unify_pattern_basic(interp, child_env, &pat, vals) {
                            // wind a new frame
                            let mut frame = wind(interp, env, fun.filename, fun.body.lineno);

                            child_env.parent = @option::Some(fun.env);
                            let r = eval_exp(interp, child_env, fun.body);

                            if frame.exception.is_some() {
                                return @mut types::Unit;
                            } else {
                                // unwind the frame if we don't have an exception.
                                unwind(interp)
                            }

                            return r;
                        }
                    }

                    frame.exception = @mut option::Some(@mut types::String(@"no matching overload found"));
                    return @mut types::Unit;
                },
                types::Constructor(decl) => {
                    if decl.slots.len() != raw_vals.len() {
                        frame.exception = @mut option::Some(@mut types::String(@"incorrect number of arguments"));
                        return @mut types::Unit;
                    } else {
                        return @mut types::Record(decl, raw_vals);
                    }
                }
                _ => {
                    frame.exception = @mut option::Some(@mut types::String(@"left-hand side is not callable"));
                    return @mut types::Unit;
                }
            }
        },
        _ => fail!(~"not implemented: full expression evaluation")
    };

    // if we ended up with a routine, run the routine
    match r {
        @types::Routine(f) => f(interp, env),
        _ => r
    }
}

fn exec_stmt(interp: @mut Interp, env: @mut env::Env<Interp>, stmt: &ast::Stmt) -> () {
    match stmt.stmt {
        ast::LetBindingStatement(pat, exp) => unify_pattern_let(interp, env, &pat, eval_exp(interp, env, &exp)),
        ast::ExpressionStatement(exp) => { eval_exp(interp, env, &exp); }
    };
}

pub fn run_file(interp: @mut Interp, path: &Path) -> () {
    match io::file_reader(path) {
        result::Ok(r) => {
            let tokens = lexer::lex(r);
            let program = parser::parse(tokens);
            run(interp, path.to_str().to_managed(), program);
        },
        result::Err(f) => fail!(f)
    }
}

pub fn run(interp: @mut Interp, filename: @str, prog: ast::Program) -> () {
    interp.filename = filename;

    for prog.imports.each |imp| {
        import_module(interp, &imp.module, imp.qualified);
    }

    for prog.records.each |rec| {
        interp.record_types.insert(@rec.name, @*rec);
        let ast::RecordName(_, r) = rec.name;

        env::declare(interp.root, &ast::Sym(r), @mut if rec.slots.len() == 0 {
            types::Record(@*rec, @[])
        } else {
            types::Constructor(@*rec)
        });
    }

    for prog.body.each |stmt| {
        wind(interp, interp.root, filename, stmt.lineno);

        exec_stmt(interp, interp.root, stmt);

        let frame = interp.current_frame.unwrap();

        match *frame.exception {
            option::None => unwind(interp),
            option::Some(f) => {
                let mut current_frame = frame;
                let mut buf = ~[types::repr(f)];
                loop {
                    buf += [fmt!("        from %s:%u", current_frame.file, current_frame.lineno)];
                    if current_frame.parent.is_none() { break; }
                    current_frame = current_frame.parent.unwrap();
                }
                io::println(str::connect(buf, "\n"));
                fail!(~"uncaught aster exception");
            }
        }
    }
}
