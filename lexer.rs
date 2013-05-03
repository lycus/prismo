use core::io;
use core::char;
use core::unicode;

#[deriving(Eq)]
pub enum TokenType {
    EOF,
    KEYWORD,
    UNDERSCORE,
    ELLIPSIS,
    SEMICOLON,
    COMMA,
    DOT,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACK,
    RBRACK,
    OPERATOR,
    RECORD_NAME,
    SYMBOL,
    STRING_LITERAL,
    BYTES_LITERAL,
    INTEGER_LITERAL,
    FLOATING_LITERAL // TODO
}

impl to_str::ToStr for TokenType {
    fn to_str(&self) -> ~str {
        match self {
            &EOF                => ~"EOF",
            &KEYWORD            => ~"KEYWORD",
            &UNDERSCORE         => ~"UNDERSCORE",
            &ELLIPSIS           => ~"ELLIPSIS",
            &SEMICOLON          => ~"SEMICOLON",
            &COMMA              => ~"COMMA",
            &DOT                => ~"DOT",
            &LPAREN             => ~"LPAREN",
            &RPAREN             => ~"RPAREN",
            &LBRACE             => ~"LBRACE",
            &RBRACE             => ~"RBRACE",
            &LBRACK             => ~"LBRACK",
            &RBRACK             => ~"RBRACK",
            &OPERATOR           => ~"OPERATOR",
            &RECORD_NAME        => ~"RECORD_NAME",
            &SYMBOL             => ~"SYMBOL",
            &STRING_LITERAL     => ~"STRING_LITERAL",
            &BYTES_LITERAL      => ~"BYTES_LITERAL",
            &INTEGER_LITERAL    => ~"INTEGER_LITERAL",
            &FLOATING_LITERAL   => ~"FLOATING_LITERAL",
        }
    }
}

pub struct Token {
    type_: TokenType,
    value: @str,
    lineno: uint,
    colno: uint
}

impl to_str::ToStr for Token {
    fn to_str(&self) -> ~str {
        fmt!("Token { type_ = %s, value = %?, lineno = %?, colno = %? }",
              self.type_.to_str(), self.value, self.lineno, self.colno)
    }
}

fn lex_integer_literal(s: &str) -> (option::Option<(TokenType, @str)>, uint) {
    if !char::is_digit(s.char_at(0)) {
        return (option::None, 0);
    }

    let mut n: uint = 0;
    while n < s.len() && char::is_digit(s.char_at(n)) {
        n += 1
    }

    (option::Some((INTEGER_LITERAL, s.slice(0, n).to_managed())), n)
}

fn lex_string_literal(s: &str) -> (option::Option<(TokenType, @str)>, uint) {
    if s.char_at(0) != '"' {
        return (option::None, 0);
    }

    let mut n: uint = 1;
    while n < s.len() {
        match s.char_at(n) {
            '\\' => n += 2,

            '"' => {
                n += 1;
                break;
            }

            _ => n += 1
        }
    }

    (option::Some((STRING_LITERAL, s.slice(0, n).to_managed())), n)
}

fn lex_bytes_literal(s: &str) -> (option::Option<(TokenType, @str)>, uint) {
    if s.char_at(0) != '\'' {
        return (option::None, 0);
    }

    let mut n: uint = 1;
    while n < s.len() {
        match s.char_at(n) {
            '\\' => n += 2,

            '\'' => {
                n += 1;
                break;
            }

            _ => n += 1
        }
    }

    (option::Some((BYTES_LITERAL, s.slice(0, n).to_managed())), n)
}

fn lex_record_name(s: &str) -> (option::Option<(TokenType, @str)>, uint) {
    if !char::is_uppercase(s.char_at(0)) {
        return (option::None, 0);
    }

    let mut n: uint = 0;
    while n < s.len() && (char::is_alphanumeric(s.char_at(n)) || ['_', '\''].contains(&s.char_at(n))) {
        n += 1
    }

    (option::Some((RECORD_NAME, s.slice(0, n).to_managed())), n)
}

fn lex_symbol(s: &str) -> (option::Option<(TokenType, @str)>, uint) {
    if !char::is_lowercase(s.char_at(0)) && !['_', '\''].contains(&s.char_at(0)) {
        return (option::None, 0);
    }

    let mut n: uint = 0;
    while n < s.len() && (char::is_alphanumeric(s.char_at(n)) || ['_', '\''].contains(&s.char_at(n))) {
        n += 1
    }

    (option::Some((SYMBOL, s.slice(0, n).to_managed())), n)
}

fn lex_whitespace(s: &str) -> (option::Option<(TokenType, @str)>, uint) {
    let mut n: uint = 0;

    while n < s.len() && ['\r', '\t', ' '].contains(&s.char_at(n)) {
        n += 1;
    }

    (option::None, n)
}

fn lex_comment(s: &str) -> (option::Option<(TokenType, @str)>, uint) {
    (option::None, if s.char_at(0) == '#' { s.len() } else { 0 })
}

fn lex_special_character(s: &str) -> (option::Option<(TokenType, @str)>, uint) {
    let mut n: uint;

    n = 3;
    if s.len() >= n && ["..."].contains(&s.substr(0, n)) {
        return (option::Some((match s.substr(0, n) {
            "..." => ELLIPSIS,
            _   => fail!(~"lexer failure: pattern match failed in lex_special_character (somehow!)")
        }, s.substr(0, n).to_managed())), n);
    }

    n = 1;
    if s.len() >= n && ["{", "}", "(", ")", "[", "]", ";", ",", "."].contains(&s.substr(0, n)) {
        return (option::Some((match s.char_at(0) {
            '{' => LBRACE,
            '}' => RBRACE,
            '(' => LPAREN,
            ')' => RPAREN,
            '[' => LBRACK,
            ']' => RBRACK,
            ';' => SEMICOLON,
            ',' => COMMA,
            '.' => DOT,
            _   => fail!(~"lexer failure: pattern match failed in lex_special_character (somehow!)")
        }, s.substr(0, n).to_managed())), n);
    }

    (option::None, 0)
}

fn lex_keyword(s: &str) -> (option::Option<(TokenType, @str)>, uint) {
    let mut n: uint;

    n = 9;
    if s.len() >= n && ["qualified"].contains(&s.substr(0, n)) {
        return (option::Some((KEYWORD, s.substr(0, n).to_managed())), n);
    }

    n = 6;
    if s.len() >= n && ["import", "record"].contains(&s.substr(0, n)) {
        return (option::Some((KEYWORD, s.substr(0, n).to_managed())), n);
    }

    n = 5;
    if s.len() >= n && ["match", "while"].contains(&s.substr(0, n)) {
        return (option::Some((KEYWORD, s.substr(0, n).to_managed())), n);
    }

    n = 4;
    if s.len() >= n && ["case", "else", "then", "with"].contains(&s.substr(0, n)) {
        return (option::Some((KEYWORD, s.substr(0, n).to_managed())), n);
    }

    n = 2;
    if s.len() >= n && ["as", "do", "fn", "if"].contains(&s.substr(0, n)) {
        return (option::Some((KEYWORD, s.substr(0, n).to_managed())), n);
    }

    (option::None, 0)
}

fn is_operator_char(c: char) -> bool {
    return unicode::general_category::Sm(c)
        || unicode::general_category::So(c)
        || ['!', '#', '%', '&', '*', '+', '-', '/', ':', '<', '=', '>', '?', '@', '\\', '^', '|',
            '~', '.', '|'].contains(&c);
}

fn lex_operator(s: &str) -> (option::Option<(TokenType, @str)>, uint) {
    if !is_operator_char(s.char_at(0)) {
        return (option::None, 0);
    }

    let mut n: uint = 0;
    while n < s.len() && (is_operator_char(s.char_at(n))) {
        n += 1
    }

    (option::Some((OPERATOR, s.slice(0, n).to_managed())), n)
}

pub fn lex<R: io::Reader + io::ReaderUtil>(r: R) -> @[Token] {
    let mut tokens = @[];

    let mut lineno = 0;

    while !r.eof() {
        let mut colno = 0;

        let mut line = r.read_line();

        let mut s: &str = line;

        lineno += 1;

        while s.len() > 0 {
            let mut found = false;

            for [
                lex_whitespace,
                lex_comment,
                lex_integer_literal,
                lex_string_literal,
                lex_bytes_literal,
                lex_special_character,
                lex_keyword,
                lex_record_name,
                lex_operator,
                lex_symbol
            ].each |&rule| {
                match rule(s) {
                    (option::Some((type_, value)), eaten) => {
                        tokens += @[Token {
                            type_: type_,
                            value: value,
                            lineno: lineno,
                            colno: colno
                        }];

                        colno += eaten;
                        s = line.slice(colno, line.len());
                        found = eaten != 0;

                        break;
                    }

                    (option::None, eaten) => {
                        colno += eaten;
                        s = line.slice(colno, line.len());
                        found = eaten != 0;
                        if found {
                            break;
                        }
                    }
                }
            }

            if !found {
                fail!(fmt!("lexer failure: could not lex remainder of %? (line %?, col %?)",
                           s, lineno, colno));
            }
        }
    }

    tokens += [Token {
        type_: EOF,
        value: @"",
        lineno: lineno,
        colno: 0
    }];

    tokens
}
