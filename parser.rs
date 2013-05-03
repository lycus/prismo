use ast;
use lexer;
use types;

struct ParserState {
    tokens: @[lexer::Token],
    last_token: lexer::Token,
    pos: uint
}

impl ParserState {
    fn peek(&mut self) -> lexer::Token {
        self.last_token = self.tokens[self.pos];
        self.last_token
    }

    fn expect(&mut self, type_: lexer::TokenType) -> lexer::Token {
        let token = self.pop();
        if token.type_ != type_ {
            self.fail(fmt!("expected %?, got \"%s\" (%?)"
                           type_, token.value, token.type_));
        }
        token
    }

    fn expect_with_value(&mut self, type_: lexer::TokenType, value: @str) -> lexer::Token {
        let token = self.expect(type_);
        if token.value != value {
            self.fail(fmt!("expected \"%s\", got \"%s\" (%?)"
                           value, token.value, token.type_));
        }
        token
    }

    fn pop(&mut self) -> lexer::Token {
        let token = self.peek();
        self.pos += 1;
        token
    }

    fn fail(&mut self, msg: ~str) -> ! {
        fail!(fmt!("parser failure: %s (line %?, col %?)",
                   msg, self.last_token.lineno, self.last_token.colno));
    }
}

fn parse_symbol(state: @mut ParserState) -> types::Sym {
    types::Sym(state.expect(lexer::SYMBOL).value)
}

fn parse_module_name(state: @mut ParserState) -> ast::Module {
    let mut names = @[parse_symbol(state)];

    loop {
        let token = state.peek();

        if (token.type_ != lexer::DOT) {
            break;
        }
        state.pop();

        let child = parse_symbol(state);
        names += [child];
    }

    state.expect(lexer::SEMICOLON);

    ast::Module(names)
}

fn parse_import_declaration(state: @mut ParserState) -> ast::ImportDeclaration {
    let first = state.expect_with_value(lexer::KEYWORD, @"import");
    let token = state.peek();

    let mut qualified: bool = token.type_ == lexer::KEYWORD &&
                              token.value == @"qualified";

    if qualified {
        state.pop();
    }

    ast::ImportDeclaration {
        module: @parse_module_name(state),
        qualified: qualified,
        lineno: first.lineno
    }
}

fn parse_program(state: @mut ParserState) -> ast::Program {
    let mut imports = @[];

    loop {
        let token = state.peek();
        if (token.type_ == lexer::KEYWORD && token.value == @"import") {
            imports += [parse_import_declaration(state)];
        } else {
            break;
        }
    }

    state.expect(lexer::EOF);

    ast::Program {
        imports: imports,
        records: @[],
        body: @[]
    }
}

pub fn parse(tokens: @[lexer::Token]) -> ast::Program {
    parse_program(@mut ParserState {
        tokens: tokens,
        last_token: tokens[0],
        pos: 0
    })
}
