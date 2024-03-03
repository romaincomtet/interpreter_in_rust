use crate::ast::{
    AllExpression, AllStatement, Expression, ExpressionStatement, Identifier, LetStatement, Node,
    Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, Box<dyn Fn(&Parser) -> AllExpression>>,
    infix_parse_fns: HashMap<TokenType, Box<dyn Fn(&Parser, AllExpression) -> AllExpression>>,
}

pub trait ParsingTokenFunction {
    fn prefix_parse_fn(&self) -> AllExpression;
    fn infix_parse_fn(&self, left_expression: AllExpression) -> AllExpression;
}

#[derive(Debug)]
pub enum Precedence {
    LOWEST,
    EQUALS,  // == LESSGREATER // > or <
    SUM,     //+
    PRODUCT, //*
    PREFIX,  //-Xor!X
    CALL,    // myFunction(X)
}

impl Parser {
    fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: Token::new(TokenType::EOF, String::from("")),
            peek_token: Token::new(TokenType::EOF, String::from("")),
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        p.next_token();
        p.next_token();
        p
    }

    fn register_prefix(
        &mut self,
        t: TokenType,
        prefix_parse_fn: Box<dyn Fn(&Parser) -> AllExpression>,
    ) {
        self.prefix_parse_fns.insert(t, prefix_parse_fn);
    }

    fn register_infix(
        &mut self,
        t: TokenType,
        infix_parse_fn: Box<dyn Fn(&Parser, AllExpression) -> AllExpression>,
    ) {
        self.infix_parse_fns.insert(t, infix_parse_fn);
    }

    pub fn get_errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, t: &TokenType) {
        let message = format!(
            "expected next token to be {:?}, got {:?} instead",
            *t, self.peek_token.typee
        );
        self.errors.push(message);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program { statements: vec![] };

        while self.cur_token.typee != TokenType::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }
        Some(program)
    }

    fn parse_statement(&mut self) -> Option<AllStatement> {
        match self.cur_token.typee {
            TokenType::LET => match self.parse_let_statement() {
                Some(stmt) => Some(AllStatement::LetStatement(stmt)),
                None => None,
            },
            TokenType::RETURN => match self.parse_return_statement() {
                Some(stmt) => Some(AllStatement::ReturnStatement(stmt)),
                None => None,
            },
            _ => match self.parse_expression_statement() {
                Some(stmt) => Some(AllStatement::ExpressionStatement(stmt)),
                None => None,
            },
        }
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let token: Token = self.cur_token.clone();
        // let stmt = AllExpression::Expression(())
        let expression = self.parse_expression(Precedence::LOWEST);
        if (self.peek_token_is(&TokenType::SEMICOLON)) {
            self.next_token()
        }
        Some(ExpressionStatement {
            token,
            value: expression,
        })
    }

    fn parse_expression(&mut self, t: Precedence) -> Option<AllExpression> {
        let maybe_func = match self.prefix_parse_fns.entry(self.cur_token.typee.clone()) {
            Entry::Occupied(entry) => Some(entry.get()),
            Entry::Vacant(_) => None,
        };
        match maybe_func {
            Some(func) => Some((*func)(&self)),
            None => None,
        }
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let token_let = self.cur_token.clone();
        while !self.cur_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(ReturnStatement {
            token: token_let,
            value: Expression {
                node: Node {
                    token_literal: self.cur_token.literal.clone(),
                    string: self.cur_token.literal.clone(),
                },
            },
        })
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let token_let = self.cur_token.clone();
        if !self.expect_peek(&TokenType::IDENT) {
            return None;
        }
        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(&TokenType::ASSIGN) {
            return None;
        }
        // TODO: We're skipping the expressions until we // encounter a semicolon
        while !self.cur_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(LetStatement {
            token: token_let,
            name: ident,
            value: AllExpression::Expression(Expression {
                node: Node {
                    token_literal: self.cur_token.literal.clone(),
                    string: self.cur_token.literal.clone(),
                },
            }),
        })
    }

    fn cur_token_is(&self, t: &TokenType) -> bool {
        self.cur_token.typee == *t
    }

    fn peek_token_is(&self, t: &TokenType) -> bool {
        self.peek_token.typee == *t
    }

    fn expect_peek(&mut self, t: &TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Borrow;

    use super::Parser;
    use crate::ast::{AllStatement, Identifier, LetStatement, NodeTrait, Program, Statement};
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn test_let_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program: Program = p
            .parse_program()
            .expect("parse_program() should not return None");
        check_parser_errors(&p);
        if program.statements.len() != 3 {
            panic!(
                "program.Statements does not contain 3 statements. got={}",
                program.statements.len()
            );
        }
        for (_, stmt) in program.statements.iter().enumerate() {
            match stmt {
                AllStatement::ReturnStatement(stmt) => {
                    if (stmt.token_literal() != "return") {
                        panic!(
                            "returnStmt.TokenLiteral not 'return', got {:?}",
                            stmt.token_literal()
                        )
                    }
                }
                _ => panic!("stmt not *ast.returnStatement. got={:?}", stmt),
            }
        }
    }

    fn check_parser_errors(p: &Parser) {
        let errors = p.get_errors();

        if errors.is_empty() {
            return;
        }
        let mut error_message = format!("parser has {:?} errors\n", errors.len());
        for error in errors.iter() {
            error_message += &format!("parser error: {:?}\n", error);
        }
        panic!("{}", error_message);
    }

    fn test_let_statement(s: &AllStatement, name: &String) -> Option<bool> {
        if s.token_literal() != "let" {
            panic!("s.TokenLiteral not 'let'. got={}", s.token_literal());
        }
        match s {
            AllStatement::LetStatement(let_stmt) => {
                if &let_stmt.name.value != name {
                    panic!(
                        "let_stmt.name.Value not '{:?}. got={:?}",
                        name, let_stmt.name.value
                    );
                };
                if let_stmt.name.token_literal() != name {
                    panic!(
                        "let_stmt.name.token_literal() not '{:?}'. got={:?}",
                        name,
                        let_stmt.name.token_literal()
                    );
                };
                Some(true)
            }
            _ => panic!("s is not a LetStatement. got={:?}", s),
        }
    }
}
