use crate::token::{keywords, Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,      // current position in input (points to current char)
    read_position: usize, // current reading position in input (after current char)
    ch: char,             // current char under examination
}

impl Lexer {
    /// Creates a new Lexer, cloning the input string.
    ///
    /// # Arguments
    ///
    /// * `input` - A string slice that will be cloned to create the Lexer
    pub fn new(input: &str) -> Lexer {
        let mut l = Lexer {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char(); // Call read_char to initialize the first character
        l
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0'; // Represents EOF or no character
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char()
        }
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while self.ch.is_alphabetic() || self.ch == '_' {
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;
        while self.ch.is_digit(10) {
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn peak_char(&mut self) -> char {
        self.input.chars().nth(self.read_position).unwrap_or('\0')
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let tok: Token = match self.ch {
            '=' => match self.peak_char() {
                '=' => {
                    self.read_char();
                    Token::new(TokenType::EQ, &self.input[self.position - 1..self.position])
                }
                _ => Token::new(TokenType::ASSIGN, self.ch),
            },
            ';' => Token::new(TokenType::SEMICOLON, self.ch),
            '(' => Token::new(TokenType::LPAREN, self.ch),
            ')' => Token::new(TokenType::RPAREN, self.ch),
            ',' => Token::new(TokenType::COMMA, self.ch),
            '+' => Token::new(TokenType::PLUS, self.ch),
            '{' => Token::new(TokenType::LBRACE, self.ch),
            '}' => Token::new(TokenType::RBRACE, self.ch),
            '-' => Token::new(TokenType::MINUS, self.ch),
            '!' => match self.peak_char() {
                '=' => {
                    self.read_char();
                    Token::new(
                        TokenType::NotEQ,
                        &self.input[self.position - 1..self.position],
                    )
                }
                _ => Token::new(TokenType::ASSIGN, self.ch),
            },
            '*' => Token::new(TokenType::ASTERISK, self.ch),
            '<' => Token::new(TokenType::LT, self.ch),
            '>' => Token::new(TokenType::GT, self.ch),

            '\0' => Token::new(TokenType::EOF, ""),
            _ => match self.ch {
                ch if ch.is_alphabetic() || ch == '_' => {
                    let literal = self.read_identifier();
                    return Token::new(keywords(literal), literal);
                }
                ch if ch.is_digit(10) => {
                    let literal = self.read_number();
                    return Token::new(TokenType::INT, literal);
                }
                _ => Token::new(TokenType::ILLEGAL, self.ch),
            },
        };
        self.read_char();
        tok
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tok: Token = self.next_token();
        if let Token {
            typee: TokenType::EOF,
            literal: _,
        } = tok
        {
            return None;
        };
        Some(tok)
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use super::{Token, TokenType};

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);";

        let expected_tokens: Vec<Token> = vec![
            Token::new(TokenType::LET, "let"),
            Token::new(TokenType::IDENT, "five"),
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::INT, "5"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::LET, "let"),
            Token::new(TokenType::IDENT, "ten"),
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::LET, "let"),
            Token::new(TokenType::IDENT, "add"),
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::FUNCTION, "fn"),
            Token::new(TokenType::LPAREN, "("),
            Token::new(TokenType::IDENT, "x"),
            Token::new(TokenType::COMMA, ","),
            Token::new(TokenType::IDENT, "y"),
            Token::new(TokenType::RPAREN, ")"),
            Token::new(TokenType::LBRACE, "{"),
            Token::new(TokenType::IDENT, "x"),
            Token::new(TokenType::PLUS, "+"),
            Token::new(TokenType::IDENT, "y"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::RBRACE, "}"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::LET, "let"),
            Token::new(TokenType::IDENT, "result"),
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::IDENT, "add"),
            Token::new(TokenType::LPAREN, "("),
            Token::new(TokenType::IDENT, "five"),
            Token::new(TokenType::COMMA, ","),
            Token::new(TokenType::IDENT, "ten"),
            Token::new(TokenType::RPAREN, ")"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        for (index, expected_token) in expected_tokens.iter().enumerate() {
            let tok: Token = lexer.next_token();
            println!("{:?}", tok);
            if tok.typee != expected_token.typee {
                panic!(
                    "tests[{}] - tokentype wrong. expected={:?}, got={:?}",
                    index, expected_token.typee, tok.typee
                );
            }

            if tok.literal != expected_token.literal {
                panic!(
                    "tests[{}] - literal wrong. expected={:?}, got={:?}",
                    index, expected_token.literal, tok.literal
                );
            }
        }
    }
}
