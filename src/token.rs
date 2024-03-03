#[derive(Debug, PartialEq, Clone, Hash, Eq, Copy)]
pub enum TokenType {
    STRING, // STRING
    ILLEGAL,
    EOF,
    IDENT,     // IDENT
    INT,       // INT
    ASSIGN,    // '='
    PLUS,      // '+'
    MINUS,     // '-'
    BANG,      // '!'
    ASTERISK,  // '*'
    SLASH,     // '/'
    LT,        // '<'
    GT,        // '>'
    EQ,        // "=="
    NotEQ,     // "!="
    COMMA,     // ','
    SEMICOLON, // ';'
    LPAREN,    // '('
    RPAREN,    // ')'
    LBRACE,    // '{'
    RBRACE,    // '}'
    FUNCTION,  // 'fn'
    LET,       // 'let'
    TRUE,      // 'true'
    FALSE,     // 'false'
    IF,        // 'if'
    ELSE,      // 'else'
    RETURN,    // 'return'
}

#[derive(Debug, Clone)]
pub struct Token {
    pub typee: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new<T: Into<String>>(typee: TokenType, literal: T) -> Token {
        Token {
            typee,
            literal: literal.into(),
        }
    }
}
pub fn keywords(literal: &str) -> TokenType {
    match literal {
        "fn" => TokenType::FUNCTION,
        "let" => TokenType::LET,
        "true" => TokenType::TRUE,
        "false" => TokenType::FALSE,
        "if" => TokenType::IF,
        "else" => TokenType::ELSE,
        "return" => TokenType::RETURN,
        _ => TokenType::IDENT,
    }
}
