use core::io;
use core::char;
use core::unicode;

#[deriving(Eq)]
pub enum TokenType {
    EOF,
    IMPORT,
    QUALIFIED,
    RECORD,
    MATCH,
    WITH,
    CASE,
    WHILE,
    DO,
    IF,
    THEN,
    ELSE,
    LET,
    AS,
    FN,
    UNDERSCORE,
    ELLIPSIS,
    SEMICOLON,
    COMMA,
    COLON,
    QUALIFY,
    DOT,
    ARROW,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACK,
    RBRACK,
    ASSIGN,
    OPERATOR,
    RECORD_NAME,
    SYMBOL,
    STRING_LITERAL,
    BYTES_LITERAL,
    INTEGER_LITERAL,
    FLOATING_LITERAL // TODO
}

pub struct Token {
    type_: TokenType,
    value: @str,
    lineno: uint,
    colno: uint
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

    (option::Some((match s.slice(0, n) {
        "qualified" => QUALIFIED,
        "import" => IMPORT,
        "record" => RECORD,
        "match" => MATCH,
        "while" => WHILE,
        "case" => CASE,
        "else" => ELSE,
        "then" => THEN,
        "with" => WITH,
        "let" => LET,
        "as" => AS,
        "do" => DO,
        "fn" => FN,
        "if" => IF,
        _ => SYMBOL
    }, s.slice(0, n).to_managed())), n)
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

fn lex_special_characters(s: &str) -> (option::Option<(TokenType, @str)>, uint) {
    let mut n: uint;

    n = 1;
    if s.len() >= n {
        match s.char_at(0) {
            '{' => return (option::Some((LBRACE, s.substr(0, n).to_managed())), n),
            '}' => return (option::Some((RBRACE, s.substr(0, n).to_managed())), n),
            '(' => return (option::Some((LPAREN, s.substr(0, n).to_managed())), n),
            ')' => return (option::Some((RPAREN, s.substr(0, n).to_managed())), n),
            '[' => return (option::Some((LBRACK, s.substr(0, n).to_managed())), n),
            ']' => return (option::Some((RBRACK, s.substr(0, n).to_managed())), n),
            ';' => return (option::Some((SEMICOLON, s.substr(0, n).to_managed())), n),
            ',' => return (option::Some((COMMA, s.substr(0, n).to_managed())), n),
            '_' => return (option::Some((UNDERSCORE, s.substr(0, n).to_managed())), n),
            _   => return (option::None, 0)
        }
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

    (option::Some((match s.slice(0, n) {
        "..." => ELLIPSIS,
        "::" => QUALIFY,
        "->" => ARROW,
        "=" => ASSIGN,
        "." => DOT,
        ":" => COLON,
        _ => OPERATOR
    }, s.slice(0, n).to_managed())), n)
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
                lex_record_name,
                lex_special_characters,
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
