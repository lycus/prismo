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

    fn expect_any(&mut self, types: ~[lexer::TokenType]) -> lexer::Token {
        let token = self.pop();
        if !types.contains(&token.type_) {
            self.fail(fmt!("expected one of %?, got \"%s\" (%?)"
                           types, token.value, token.type_));
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

fn parse_symbol(state: @mut ParserState) -> ast::Exp {
    let token = state.expect(lexer::SYMBOL);
    ast::Exp {
        exp: ast::SymbolExpression(ast::Sym(token.value)),
        lineno: token.lineno
    }
}

fn parse_module_name(state: @mut ParserState) -> ast::ModuleName {
    let mut parts = @[];

    match parse_symbol(state) {
        ast::Exp { exp: ast::SymbolExpression(part), lineno: _ } => parts += [part],
        _ => state.fail(~"irrefutable pattern match refuted?!")
    }

    loop {
        let token = state.peek();

        if (token.type_ != lexer::DOT) {
            break;
        }
        state.pop();

        match parse_symbol(state) {
            ast::Exp { exp: ast::SymbolExpression(part), lineno: _ } => parts += [part],
            _ => state.fail(~"irrefutable pattern match refuted?!")
        }
    }

    ast::ModuleName(parts)
}

fn parse_import_declaration(state: @mut ParserState) -> ast::ImportDeclaration {
    let first = state.expect_with_value(lexer::KEYWORD, @"import");
    let token = state.peek();

    let qualified = token.type_ == lexer::KEYWORD &&
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

fn parse_record_name(state: @mut ParserState) -> ast::Exp {
    let token = state.expect(lexer::RECORD_NAME);
    ast::Exp {
        exp: ast::RecordNameExpression(ast::RecordName(token.value)),
        lineno: token.lineno
    }
}

fn parse_record_body(state: @mut ParserState) -> @[ast::Sym] {
    let mut slots = @[];

    match parse_symbol(state) {
        ast::Exp { exp: ast::SymbolExpression(slot), lineno: _ } => slots += [slot],
        _ => state.fail(~"irrefutable pattern match refuted?!")
    }

    loop {
        if state.peek().type_ != lexer::COMMA {
            break;
        }
        state.pop();

        if state.peek().type_ == lexer::SYMBOL {
            match parse_symbol(state) {
                ast::Exp { exp: ast::SymbolExpression(slot), lineno: _ } => slots += [slot],
                _ => state.fail(~"irrefutable pattern match refuted?!")
            }
        } else {
            break;
        }
    }

    slots
}

fn parse_record_declaration(state: @mut ParserState) -> ast::RecordDeclaration {
    let first = state.expect_with_value(lexer::KEYWORD, @"record");
    let record_name: ast::RecordName;

    match parse_record_name(state) {
        ast::Exp { exp: ast::RecordNameExpression(name), lineno: _ } => record_name = name,
        _ => state.fail(~"irrefutable pattern match refuted?!")
    }

    let mut slots = @[];

    if state.peek().type_ == lexer::LPAREN {
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

fn parse_lambda(state: @mut ParserState) -> ast::Exp {
    let first = state.expect_with_value(lexer::KEYWORD, @"fn");
    state.expect(lexer::LPAREN);
    // TODO: the rest of this
    state.expect(lexer::RPAREN);
    state.expect(lexer::ARROW);
    ast::Exp {
        exp: ast::LambdaExpression(@[ast::AnyPattern], @parse_expression(state)),
        lineno: first.lineno
    }
}

fn parse_list_body(state: @mut ParserState) -> @[ast::Exp] {
    let mut items = @[parse_expression(state)];

    loop {
        if state.peek().type_ != lexer::COMMA {
            break;
        }
        state.pop();

        if state.peek().type_ == lexer::RBRACK {
            break;
        }

        items += [parse_expression(state)];
    }

    items
}

fn parse_list(state: @mut ParserState) -> ast::Exp {
    let first = state.expect(lexer::LBRACK);

    let mut items = @[];

    if state.peek().type_ != lexer::RBRACK {
        items = parse_list_body(state);
    }

    state.expect(lexer::RBRACK);

    ast::Exp {
        exp: ast::ListExpression(items),
        lineno: first.lineno
    }
}

fn parse_primary(state: @mut ParserState) -> ast::Exp {
    match state.peek().type_ {
        lexer::KEYWORD => parse_lambda(state),
        lexer::LPAREN => parse_parenthesized(state),
        lexer::LBRACK => parse_list(state),
        lexer::SYMBOL => parse_symbol(state),
        _ => {
            state.expect_any(~[lexer::KEYWORD,
                               lexer::LPAREN,
                               lexer::LBRACK,
                               lexer::SYMBOL]);
            state.fail(~"unreachable code reached?!");
        }
    }
}

fn parse_parenthesized(state: @mut ParserState) -> ast::Exp {
    state.expect(lexer::LPAREN);
    let exp = parse_expression(state);
    state.expect(lexer::RPAREN);
    exp
}

fn parse_expression(state: @mut ParserState) -> ast::Exp {
    // TODO: the rest of this
    parse_primary(state)
}

fn parse_statements(state: @mut ParserState) -> (@[ast::Exp], bool) {
    let mut statements = @[];
    let mut trailing_semi = false;

    statements += [parse_expression(state)];

    loop {
        if state.peek().type_ != lexer::SEMICOLON {
            break;
        }
        state.pop();

        if state.peek().type_ == lexer::RBRACE ||
           state.peek().type_ == lexer::EOF {
            trailing_semi = true;
            break;
        }

        statements += [parse_expression(state)];
    }

    (statements, trailing_semi)
}

fn parse_program(state: @mut ParserState) -> ast::Program {
    let mut imports = @[];
    let mut records = @[];
    let mut body = @[];

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

    if state.peek().type_ != lexer::EOF {
        match parse_statements(state) {
            (b, trailing_semi) => {
                body = b;
                if !trailing_semi {
                    state.expect(lexer::SEMICOLON);
                }
            }
        }
    }

    state.expect(lexer::EOF);

    ast::Program {
        imports: imports,
        records: records,
        body: body
    }
}

pub fn parse(tokens: @[lexer::Token]) -> ast::Program {
    parse_program(@mut ParserState {
        tokens: tokens,
        last_token: tokens[0],
        pos: 0
    })
}
