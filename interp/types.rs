use core::libc::c_void;
use core::hashmap::linear;

use ast;
use interp::env;

pub struct Fun<Interp> {
    pattern: @[ast::Pat],
    body: @ast::Exp,
    env: @mut env::Env<Interp>,
    filename: @str
}

pub enum Val<Interp> {
    // (),
    Unit,

    // 1
    Integer(int),

    // 1.1
    Floating(float),

    // 'hello!'
    Bytes(@[u8]),

    // "hello!"
    String(@str),

    // true/false
    Boolean(bool),

    // [a, b, c, ...]
    List(@[@mut Val<Interp>]),

    // A(a, b, c, ...)
    Record(@ast::RecordDeclaration, @[@mut Val<Interp>]),

    // <record constructor>
    Constructor(@ast::RecordDeclaration),

    // f(a, b, ...) = ...
    Function(@[Fun<Interp>]),

    // <routine>
    Routine(@fn (@mut Interp, @mut env::Env<Interp>) -> @mut Val<Interp>),

    // <handle>
    Handle(c_void),
}

pub fn repr<Interp>(v: @mut Val<Interp>) -> ~str {
    match v {
        @Unit               => ~"()",
        @Integer(i)         => fmt!("%d", i),
        @Floating(i)        => fmt!("%f", i),
        @Bytes(s)           => fmt!("%?", s),
        @String(s)          => fmt!("%?", s),
        @Boolean(b)         => if b { ~"True" } else { ~"False" },
        @List(xs)           => fmt!("[%s]", str::connect(xs.map(|x| repr(*x)), ", ")),
        @Record(decl, vs)   => fmt!("%s(%s)", decl.name.to_str(), str::connect(vs.map(|x| repr(*x)), ", ")),
        @Constructor(decl)  => fmt!("<constructor for %s>", decl.name.to_str()),
        @Function(_)        => ~"<function>",
        @Routine(_)         => ~"<routine>",
        @Handle(_)          => fmt!("<handle>")
    }
}

impl <Interp> Eq for Val<Interp> {
    fn eq(&self, other: &Val<Interp>) -> bool {
        match (self, other) {
            (&Unit, &Unit)                  => true,
            (&Integer(i), &Integer(j))      => i == j,
            (&Floating(i), &Floating(j))    => i == j,
            (&Bytes(s), &Bytes(t))          => s == t,
            (&String(s), &String(t))        => s == t,
            _                               => false
        }
    }

    fn ne(&self, other: &Val<Interp>) -> bool {
        !self.eq(other)
    }
}

pub fn unescape_str(inp: &str) -> ~str {
    let mut out = ~[];
    let mut i = 1;
    let mut slice = inp.slice(1, inp.len() - 1);

    while slice.len() > 0 {
        out += ~[if slice.char_at(0) == '\\' {
            match slice.char_at(1) {
                // \" -> "
                '"' => {
                    i += 1;
                    '"'
                },

                // \' -> '
                '\'' => {
                    i += 1;
                    '\''
                },

                // \\ -> \
                '\\' => {
                    i += 1;
                    '\\'
                },

                // \n -> <LF>
                'n' => {
                    i += 1;
                    '\n'
                },

                // \r -> <CR>
                'r' => {
                    i += 1;
                    '\r'
                },

                // \t -> <TAB>
                't' => {
                    i += 1;
                    '\t'
                },

                // \x## -> hex escape
                'x' => {
                    if slice.len() >= 4 {
                        i += 3;
                        int::from_str_radix(slice.slice(2, 4), 16).unwrap() as char
                    } else {
                        'x'
                    }
                },

                // \u#### -> unicode escape
                'u' => {
                    if slice.len() >= 6 {
                        i += 5;
                        int::from_str_radix(slice.slice(2, 6), 16).unwrap() as char
                    } else {
                        'u'
                    }
                },

                // otherwise -> \
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

pub fn gen_lit<Interp>(l: ast::Lit) -> Val<Interp> {
    match l {
        ast::IntegerLiteral(i) => Integer(int::from_str(i).unwrap()),
        ast::FloatingLiteral(i) => Floating(float::from_str(i).unwrap()),
        ast::StringLiteral(s) => String(unescape_str(s).to_managed()),
        ast::BytesLiteral(s) => Bytes(at_vec::from_owned(unescape_str(s).to_bytes()))
    }
}
