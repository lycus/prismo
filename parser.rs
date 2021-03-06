use ast;
use lexer;

struct ParserState {
    tokens: @[lexer::Token],
    last_token: lexer::Token,
    pos: uint
}

impl ParserState {
    fn peek(&mut self) -> lexer::Token {
        self.last_token = self.peek_by(0);
        self.last_token
    }

    fn peek_by(&mut self, n: uint) -> lexer::Token {
        self.tokens[self.pos + n]
    }

    fn expect(&mut self, type_: lexer::TokenType) -> lexer::Token {
        let token = self.advance();
        if token.type_ != type_ {
            self.fail(fmt!("expected %?, got \"%s\" (%?)"
                           type_, token.value, token.type_));
        }
        token
    }

    fn expect_any(&mut self, types: ~[lexer::TokenType]) -> lexer::Token {
        let token = self.advance();
        if !types.contains(&token.type_) {
            self.fail(fmt!("expected one of %?, got \"%s\" (%?)"
                           types, token.value, token.type_));
        }
        token
    }

    fn advance(&mut self) -> lexer::Token {
        let token = self.peek();
        self.pos += 1;
        token
    }

    fn fail(&mut self, msg: ~str) -> ! {
        fail!(fmt!("parser failure: %s (line %?, col %?)",
                   msg, self.last_token.lineno, self.last_token.colno + 1));
    }
}

fn parse_symbol(state: @mut ParserState) -> (ast::Sym, uint) {
    let token = state.expect(lexer::SYMBOL);
    (ast::Sym(token.value), token.lineno)
}

fn parse_symbol_or_record_name(state: @mut ParserState) -> (ast::Sym, uint) {
    match state.peek().type_ {
        lexer::SYMBOL => {
            parse_symbol(state)
        }

        lexer::RECORD_NAME => {
            let token = state.advance();
            (ast::Sym(token.value), token.lineno)
        }

        _ => {
            state.expect_any(~[lexer::SYMBOL,
                               lexer::RECORD_NAME]);
            state.fail(~"unreachable code reached?!");
        }
    }
}

fn parse_symbol_expression(state: @mut ParserState) -> ast::Exp {
    let (sym, lineno) = parse_symbol_or_record_name(state);

    ast::Exp {
        exp: ast::SymbolExpression(sym),
        lineno: lineno
    }
}

fn parse_literal(state: @mut ParserState) -> ast::Exp {
    let token = state.expect_any(~[lexer::STRING_LITERAL,
                                   lexer::BYTES_LITERAL,
                                   lexer::INTEGER_LITERAL,
                                   lexer::FLOATING_LITERAL]);

    ast::Exp {
        exp: ast::LiteralExpression(match token.type_ {
            lexer::STRING_LITERAL   => ast::StringLiteral(token.value),
            lexer::BYTES_LITERAL    => ast::BytesLiteral(token.value),
            lexer::INTEGER_LITERAL  => ast::IntegerLiteral(token.value),
            lexer::FLOATING_LITERAL => ast::FloatingLiteral(token.value),
            _                       => state.fail(~"irrefutable pattern match refuted?!")
        }),

        lineno: token.lineno
    }
}

fn parse_dotted_name(state: @mut ParserState) -> ast::DottedName {
    let (part, _) = parse_symbol(state);
    let mut parts = @[part];

    loop {
        let token = state.peek();

        if (token.type_ != lexer::DOT) {
            break;
        }
        state.advance();

        let (part, _) = parse_symbol(state);
        parts += [part];
    }

    ast::DottedName(parts)
}

fn parse_import_declaration(state: @mut ParserState) -> ast::ImportDeclaration {
    let first = state.expect(lexer::IMPORT);
    let qualified = state.peek().type_ == lexer::QUALIFIED;

    if qualified {
        state.advance();
    }

    ast::ImportDeclaration {
        module: parse_dotted_name(state),
        qualified: qualified,
        lineno: first.lineno
    }
}

fn parse_record_name(state: @mut ParserState) -> (ast::RecordName, uint) {
    let token = state.expect(lexer::RECORD_NAME);
    (ast::RecordName(@ast::DottedName(@[]), token.value), token.lineno)
}

fn parse_record_body(state: @mut ParserState) -> @[ast::Sym] {
    let (slot, _) = parse_symbol(state);
    let mut slots = @[slot];

    loop {
        if state.peek().type_ != lexer::COMMA {
            break;
        }
        state.advance();

        if state.peek().type_ == lexer::SYMBOL {
            let (slot, _) = parse_symbol(state);
            slots += [slot];
        } else {
            break;
        }
    }

    slots
}

fn parse_record_declaration(state: @mut ParserState) -> ast::RecordDeclaration {
    let first = state.expect(lexer::RECORD);

    let (name, _) = parse_record_name(state);

    let mut slots = @[];

    if state.peek().type_ == lexer::LPAREN {
        state.advance();
        if state.peek().type_ != lexer::RPAREN {
            slots = parse_record_body(state);
        }
        state.expect(lexer::RPAREN);
    }

    ast::RecordDeclaration {
        name: name,
        slots: slots,
        lineno: first.lineno
    }
}

fn parse_dotted_record_pattern(state: @mut ParserState) -> ast::Pat {
    let (part, _) = parse_symbol(state);
    let mut parts = @[part];

    loop {
        let mut token = state.peek();

        if token.type_ != lexer::DOT {
            break;
        }
        token = state.advance();

        if state.peek().type_ == lexer::RECORD_NAME {
            return match parse_record_pattern(state) {
                ast::RecordPattern(ast::RecordName(_, sym), patterns) => ast::RecordPattern(ast::RecordName(@ast::DottedName(parts), sym), patterns),
                _ => state.fail(~"irrefutable pattern match refuted?!")
            };
        }

        let (part, _) = parse_symbol(state);
        parts += [part];
    }

    state.fail(~"unreachable code reached?!")
}

fn parse_record_pattern(state: @mut ParserState) -> ast::Pat {
    let (name, _) = parse_record_name(state);

    let mut patterns = @[];

    if state.peek().type_ == lexer::LPAREN {
        state.advance();
        if state.peek().type_ != lexer::RPAREN {
            patterns = parse_patterns(state);
        }
        state.expect(lexer::RPAREN);
    }

    ast::RecordPattern(name, patterns)
}

fn parse_list_pattern(state: @mut ParserState) -> ast::Pat {
    state.expect(lexer::LBRACK);

    let patterns = if state.peek().type_ == lexer::RBRACK {
        @[]
    } else {
        parse_patterns(state)
    };

    state.expect(lexer::RBRACK);

    ast::ListPattern(patterns)
}

fn parse_literal_pattern(state: @mut ParserState) -> ast::Pat {
    match parse_literal(state) {
        ast::Exp { exp: ast::LiteralExpression(lit), lineno: _ } => ast::LiteralPattern(lit),
        _ => state.fail(~"irrefutable pattern match refuted?!")
    }
}

fn parse_symbol_pattern(state: @mut ParserState) -> ast::Pat {
    let (sym, _) = parse_symbol(state);
    ast::SymbolPattern(sym)
}

fn parse_unit_pattern(state: @mut ParserState) -> ast::Pat {
    state.expect(lexer::LPAREN);
    state.expect(lexer::RPAREN);
    ast::UnitPattern
}

fn parse_parenthesized_pattern(state: @mut ParserState) -> ast::Pat {
    state.expect(lexer::LPAREN);
    let pat = parse_pattern(state);
    state.expect(lexer::RPAREN);
    pat
}

fn parse_primary_pattern(state: @mut ParserState) -> ast::Pat {
    match state.peek().type_ {
        lexer::UNDERSCORE       => {
            state.advance();
            ast::AnyPattern
        },
        lexer::ELLIPSIS         => {
            state.advance();
            ast::ManyPattern
        },
        lexer::RECORD_NAME      => parse_record_pattern(state),
        lexer::LPAREN           => {
            // XXX: LL(2)
            if state.peek_by(1).type_ == lexer::RPAREN {
                parse_unit_pattern(state)
            } else {
                parse_parenthesized_pattern(state)
            }
        },
        lexer::LBRACK           => parse_list_pattern(state),
        lexer::STRING_LITERAL   |
        lexer::BYTES_LITERAL    |
        lexer::INTEGER_LITERAL  |
        lexer::FLOATING_LITERAL => parse_literal_pattern(state),
        lexer::SYMBOL           => {
            // XXX: LL(2)
            if state.peek_by(1).type_ == lexer::DOT {
                parse_dotted_record_pattern(state)
            } else {
                parse_symbol_pattern(state)
            }
        },
        _                       => {
            state.expect_any(~[lexer::UNDERSCORE,
                               lexer::ELLIPSIS,
                               lexer::RECORD_NAME,
                               lexer::LBRACK,
                               lexer::STRING_LITERAL,
                               lexer::BYTES_LITERAL,
                               lexer::INTEGER_LITERAL,
                               lexer::FLOATING_LITERAL,
                               lexer::SYMBOL]);
            state.fail(~"unreachable code reached?!");
        }
    }
}

fn parse_binary_pattern(state: @mut ParserState) -> ast::Pat {
    fn _impl(state: @mut ParserState, pat: @ast::Pat, min_precedence: uint) -> @ast::Pat {
        let mut lhs = pat;
        loop {
            let op = state.peek();
            if op.type_ != lexer::OPERATOR || precedence(op.value) < min_precedence {
                break;
            }
            state.advance();
            let mut rhs = @parse_primary_pattern(state);
            loop {
                let next_op = state.peek();
                if next_op.type_ != lexer::OPERATOR || precedence(next_op.value) <= precedence(op.value) {
                    break;
                }
                rhs = _impl(state, rhs, precedence(next_op.value));
            }
            lhs = match op.value.to_owned() {
                ~"|" => @ast::DisjunctivePattern(lhs, rhs),
                ~"&" => @ast::ConjunctivePattern(lhs, rhs),
                _ => state.fail(fmt!("operator %s not allowed in patterns", op.value))
            }
        }
        lhs
    }

    *_impl(state, @parse_primary_pattern(state), 0)
}

fn parse_bound_pattern(state: @mut ParserState) -> ast::Pat {
    let mut lhs = parse_binary_pattern(state);

    loop {
        if state.peek().type_ != lexer::AS {
            break;
        }

        state.advance();

        let (sym, _) = parse_symbol(state);
        lhs = ast::BoundPattern(sym, @lhs);
    }

    lhs
}

fn parse_pattern(state: @mut ParserState) -> ast::Pat {
    parse_bound_pattern(state)
}

fn parse_patterns(state: @mut ParserState) -> @[ast::Pat] {
    let mut patterns = @[parse_pattern(state)];

    loop {
        if state.peek().type_ != lexer::COMMA {
            break;
        }

        state.advance();

        if state.peek().type_ == lexer::RBRACK ||
           state.peek().type_ == lexer::RPAREN {
            break;
        }

        patterns += @[parse_pattern(state)];
    }

    patterns
}

fn parse_lambda(state: @mut ParserState) -> ast::Exp {
    let first = state.expect(lexer::FN);
    state.expect(lexer::LPAREN);

    let patterns = if state.peek().type_ != lexer::RPAREN {
        parse_patterns(state)
    } else {
        @[]
    };

    state.expect(lexer::RPAREN);
    state.expect(lexer::ARROW);
    ast::Exp {
        exp: ast::LambdaExpression(patterns, @parse_expression(state)),
        lineno: first.lineno
    }
}

fn parse_list_body(state: @mut ParserState) -> @[ast::Exp] {
    let mut items = @[parse_expression(state)];

    loop {
        if state.peek().type_ != lexer::COMMA {
            break;
        }
        state.advance();

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

fn parse_unit(state: @mut ParserState) -> ast::Exp {
    let token = state.expect(lexer::LPAREN);
    state.expect(lexer::RPAREN);

    ast::Exp {
        exp: ast::UnitExpression,
        lineno: token.lineno
    }
}

fn parse_primary(state: @mut ParserState) -> ast::Exp {
    match state.peek().type_ {
        lexer::FN           => parse_lambda(state),
        lexer::LPAREN       => {
            // XXX: LL(2)
            if state.peek_by(1).type_ == lexer::RPAREN {
                parse_unit(state)
            } else {
                parse_parenthesized(state)
            }
        },
        lexer::LBRACK       => parse_list(state),
        lexer::LBRACE       => parse_block(state),
        lexer::SYMBOL       |
        lexer::RECORD_NAME  => parse_symbol_expression(state),
        lexer::STRING_LITERAL   |
        lexer::BYTES_LITERAL    |
        lexer::INTEGER_LITERAL  |
        lexer::FLOATING_LITERAL => parse_literal(state),
        _ => {
            state.expect_any(~[lexer::FN,
                               lexer::LPAREN,
                               lexer::LBRACK,
                               lexer::LBRACE,
                               lexer::SYMBOL,
                               lexer::RECORD_NAME,
                               lexer::STRING_LITERAL,
                               lexer::BYTES_LITERAL,
                               lexer::INTEGER_LITERAL,
                               lexer::FLOATING_LITERAL]);
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

fn parse_block(state: @mut ParserState) -> ast::Exp {
    let token = state.expect(lexer::LBRACE);

    let mut statements = @[];
    let trailing_semi: bool;

    if state.peek().type_ != lexer::RBRACE {
        let (stmts, ts) = parse_statements(state);
        statements = stmts;
        trailing_semi = ts;
    } else {
        trailing_semi = true;
    }

    state.expect(lexer::RBRACE);

    ast::Exp {
        exp: ast::BlockExpression(statements, trailing_semi),
        lineno: token.lineno
    }
}

fn parse_fn_statement(state: @mut ParserState) -> ast::Stmt {
    let token = state.expect(lexer::LET);
    state.expect(lexer::FN);
    let dn = parse_dotted_name(state);

    state.expect(lexer::LPAREN);
    let patterns = if state.peek().type_ != lexer::RPAREN {
        parse_patterns(state)
    } else {
        @[]
    };
    state.expect(lexer::RPAREN);

    state.expect(lexer::ASSIGN);

    ast::Stmt {
        // TODO: correct fn binding
        stmt: ast::FnBindingStatement(dn, ast::Sym(@""), patterns, parse_expression(state)),
        lineno: token.lineno
    }
}

fn parse_let_statement(state: @mut ParserState) -> ast::Stmt {
    let token = state.expect(lexer::LET);
    let pat = parse_pattern(state);
    state.expect(lexer::ASSIGN);

    ast::Stmt {
        stmt: ast::LetBindingStatement(pat, parse_expression(state)),
        lineno: token.lineno
    }
}

fn parse_if_then_else(state: @mut ParserState) -> ast::Exp {
    let first = state.expect(lexer::IF);
    let pred = parse_expression(state);
    state.expect(lexer::THEN);
    let consequent = parse_expression(state);

    ast::Exp {
        exp: ast::IfThenElseExpression(@pred, @consequent, if state.peek().type_ == lexer::ELSE {
            state.advance();
            option::Some(@parse_expression(state))
        } else {
            option::None
        }),
        lineno: first.lineno
    }
}

fn parse_while_do(state: @mut ParserState) -> ast::Exp {
    let first = state.expect(lexer::WHILE);
    let cond = parse_expression(state);
    state.expect(lexer::DO);
    ast::Exp {
        exp: ast::WhileDoExpression(@cond, @parse_expression(state)),
        lineno: first.lineno
    }
}

fn parse_case(state: @mut ParserState) -> (ast::Pat, ast::Exp) {
    state.expect(lexer::CASE);
    let pat = parse_pattern(state);
    state.expect(lexer::ARROW);
    let exp = parse_expression(state);

    (pat, exp)
}

fn parse_cases(state: @mut ParserState) -> @[(ast::Pat, ast::Exp)] {
    let case = parse_case(state);
    let mut cases = @[case];

    loop {
        if state.peek().type_ != lexer::CASE {
            break;
        }
        cases += [parse_case(state)];
    }

    cases
}

fn parse_match_with(state: @mut ParserState) -> ast::Exp {
    let first = state.expect(lexer::MATCH);
    let exp = parse_expression(state);
    state.expect(lexer::WITH);

    let mut cases = @[];
    if state.peek().type_ == lexer::CASE {
        cases = parse_cases(state);
    }

    ast::Exp {
        exp: ast::MatchWithExpression(@exp, cases),
        lineno: first.lineno
    }

}

fn parse_mixfix_expression(state: @mut ParserState) -> ast::Exp {
    match state.peek().type_ {
        lexer::IF           => parse_if_then_else(state),
        lexer::WHILE        => parse_while_do(state),
        lexer::MATCH        => parse_match_with(state),
        _                   => state.fail(~"unreachable code reached?!")
    }
}

fn parse_parameters(state: @mut ParserState) -> @[ast::Exp] {
    let mut params = @[parse_expression(state)];

    loop {
        if state.peek().type_ != lexer::COMMA {
            break;
        }

        state.advance();

        if state.peek().type_ == lexer::RPAREN {
            break;
        }

        params += @[parse_expression(state)];
    }

    params
}

fn parse_postfix_expression(state: @mut ParserState) -> ast::Exp {
    let mut lhs = parse_primary(state);

    loop {
        let token = state.peek();
        match token.type_ {
            lexer::LPAREN => {
                state.advance();

                lhs = ast::Exp {
                    exp: ast::CallExpression(@lhs, if state.peek().type_ != lexer::RPAREN {
                        parse_parameters(state)
                    } else {
                        @[]
                    }),
                    lineno: token.lineno
                };

                state.expect(lexer::RPAREN);
            },

            lexer::COLON => {
                state.advance();

                let (sym, _) = parse_symbol_or_record_name(state);

                lhs = ast::Exp {
                    exp: ast::FunctionBindExpression(@lhs, sym),
                    lineno: token.lineno
                };
            },

            lexer::DOT => {
                state.advance();

                let (sym, _) = parse_symbol_or_record_name(state);

                lhs = ast::Exp {
                    exp: ast::AccessExpression(@lhs, sym),
                    lineno: token.lineno
                };
            },

            _ => break
        }
    }

    lhs
}

fn precedence(op: &str) -> uint {
    match op.char_at(0) {
        '|' => 0,
        '^' => 1 ,
        '&' => 2,
        '<' | '>' => 3,
        '=' | '!' => 4,
        '+' | '-' => 5,
        '*' | '/' | '%' => 6,
        ':' | '.' => 7,
        _ => 8
    }
}

fn parse_binary_expression(state: @mut ParserState) -> ast::Exp {
    fn _impl(state: @mut ParserState, exp: @ast::Exp, min_precedence: uint) -> @ast::Exp {
        let mut lhs = exp;
        loop {
            let op = state.peek();
            if op.type_ != lexer::OPERATOR || precedence(op.value) < min_precedence {
                break;
            }
            state.advance();
            let mut rhs = @parse_postfix_expression(state);
            loop {
                let next_op = state.peek();
                if next_op.type_ != lexer::OPERATOR || precedence(next_op.value) <= precedence(op.value) {
                    break;
                }
                rhs = _impl(state, rhs, precedence(next_op.value));
            }
            lhs = @ast::Exp {
                exp: ast::BinaryExpression(lhs, op.value, rhs),
                lineno: op.lineno
            }
        }
        lhs
    }

    *_impl(state, @parse_postfix_expression(state), 0)
}

fn parse_basic_expression(state: @mut ParserState) -> ast::Exp {
    match state.peek().type_ {
        lexer::IF       |
        lexer::WHILE    |
        lexer::MATCH        => parse_mixfix_expression(state),
        _                   => parse_binary_expression(state)
    }
}

fn parse_assignment_expression(state: @mut ParserState) -> ast::Exp {
    let lhs = parse_basic_expression(state);
    if state.peek().type_ == lexer::ASSIGN {
        let token = state.advance();
        ast::Exp {
            exp: ast::AssignmentExpression(@lhs, @parse_assignment_expression(state)),
            lineno: token.lineno
        }
    } else {
        lhs
    }
}

fn parse_expression(state: @mut ParserState) -> ast::Exp {
    parse_assignment_expression(state)
}

fn parse_statement(state: @mut ParserState) -> ast::Stmt {
    match state.peek().type_ {
        lexer::LET          => {
            // XXX: LL(2)
            if state.peek_by(1).type_ == lexer::FN {
                parse_fn_statement(state)
            } else {
                parse_let_statement(state)
            }
        },
        _                   => ast::mk_expression_statement(parse_expression(state))
    }
}

fn parse_statements(state: @mut ParserState) -> (@[ast::Stmt], bool) {
    let mut statements = @[];
    let mut trailing_semi = false;

    statements += [parse_statement(state)];

    loop {
        if state.peek().type_ != lexer::SEMICOLON {
            break;
        }
        state.advance();

        if state.peek().type_ == lexer::RBRACE ||
           state.peek().type_ == lexer::EOF {
            trailing_semi = true;
            break;
        }

        statements += [parse_statement(state)];
    }

    (statements, trailing_semi)
}

fn parse_program(state: @mut ParserState) -> ast::Program {
    let mut imports = @[];
    let mut records = @[];
    let mut body = @[];

    loop {
        let token = state.peek();
        if (token.type_ == lexer::IMPORT) {
            imports += [parse_import_declaration(state)];
            state.expect(lexer::SEMICOLON);
        } else {
            break;
        }
    }

    loop {
        let token = state.peek();
        if (token.type_ == lexer::RECORD) {
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
