use ast;
use lexer;

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

fn parse_symbol(state: @mut ParserState) -> ast::Sym {
    ast::Sym(state.expect(lexer::SYMBOL).value)
}

fn parse_module_name(state: @mut ParserState) -> ast::ModuleName {
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

    ast::ModuleName(names)
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
        module: parse_module_name(state),
        qualified: qualified,
        lineno: first.lineno
    }
}

fn parse_record_name(state: @mut ParserState) -> ast::RecordName {
    ast::RecordName(state.expect(lexer::RECORD_NAME).value)
}

fn parse_record_body(state: @mut ParserState) -> @[ast::Sym] {
    let mut slots: @[ast::Sym] = @[];

    slots += [parse_symbol(state)];

    loop {
        let mut token = state.peek();
        if token.type_ != lexer::COMMA {
            break;
        }
        state.pop();

        token = state.peek();
        if token.type_ == lexer::SYMBOL {
            slots += [parse_symbol(state)];
        } else {
            break;
        }
    }

    slots
}

fn parse_record_declaration(state: @mut ParserState) -> ast::RecordDeclaration {
    let first = state.expect_with_value(lexer::KEYWORD, @"record");
    let record_name = parse_record_name(state);
    let mut slots: @[ast::Sym] = @[];

    let token = state.peek();

    if token.type_ == lexer::LPAREN {
        state.pop();
        if state.peek().type_ != lexer::RPAREN {
            slots = parse_record_body(state);
        }
        state.expect(lexer::RPAREN);
    }

    ast::RecordDeclaration {
        name: record_name,
        slots: slots,
        lineno: first.lineno
    }
}

fn parse_program(state: @mut ParserState) -> ast::Program {
    let mut imports = @[];
    let mut records = @[];

    loop {
        let token = state.peek();
        if (token.type_ == lexer::KEYWORD && token.value == @"import") {
            imports += [parse_import_declaration(state)];
            state.expect(lexer::SEMICOLON);
        } else {
            break;
        }
    }

    loop {
        let token = state.peek();
        if (token.type_ == lexer::KEYWORD && token.value == @"record") {
            records += [parse_record_declaration(state)];
            state.expect(lexer::SEMICOLON);
        } else {
            break;
        }
    }

    state.expect(lexer::EOF);

    ast::Program {
        imports: imports,
        records: records,
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
