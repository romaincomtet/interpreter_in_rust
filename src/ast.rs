use std::fmt::format;

use crate::token::Token;

#[derive(Debug)]
pub struct Node {
    pub token_literal: String,
    pub string: String,
}

pub trait NodeTrait {
    fn token_literal(&self) -> &String;
    fn string(&self) -> String;
}

#[derive(Debug)]
pub struct Statement {
    pub node: Node,
}

impl NodeTrait for Statement {
    fn token_literal(&self) -> &String {
        &self.node.token_literal
    }

    fn string(&self) -> String {
        self.node.string.clone()
    }
}

#[derive(Debug)]
pub struct Expression {
    pub node: Node,
}

impl NodeTrait for Expression {
    fn token_literal(&self) -> &String {
        &self.node.token_literal
    }

    fn string(&self) -> String {
        self.node.string.clone()
    }
}

#[derive(Debug)]
pub enum AllStatement {
    Statement(Statement), // Assuming Statement is a struct
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

// Implement the NodeTrait trait for the enum
impl NodeTrait for AllStatement {
    fn token_literal(&self) -> &String {
        match self {
            AllStatement::Statement(stmt) => stmt.token_literal(),
            AllStatement::LetStatement(let_stmt) => let_stmt.token_literal(),
            AllStatement::ReturnStatement(ret_stmt) => ret_stmt.token_literal(),
            AllStatement::ExpressionStatement(exp_stmt) => exp_stmt.token_literal(),
        }
    }

    fn string(&self) -> String {
        match self {
            AllStatement::Statement(stmt) => stmt.string(),
            AllStatement::LetStatement(let_stmt) => let_stmt.string(),
            AllStatement::ReturnStatement(ret_stmt) => ret_stmt.string(),
            AllStatement::ExpressionStatement(exp_stmt) => exp_stmt.string(),
        }
    }
}

#[derive(Debug)]
pub enum AllExpression {
    Expression(Expression),
    Identifier(Identifier),
}

// Implement the NodeTrait trait for the enum
impl NodeTrait for AllExpression {
    fn token_literal(&self) -> &String {
        match self {
            AllExpression::Expression(stmt) => stmt.token_literal(),
            AllExpression::Identifier(ident_stmt) => ident_stmt.token_literal(),
        }
    }

    fn string(&self) -> String {
        match self {
            AllExpression::Expression(stmt) => stmt.string(),
            AllExpression::Identifier(ident_stmt) => ident_stmt.string(),
        }
    }
}

pub struct Program {
    pub statements: Vec<AllStatement>,
}

impl NodeTrait for Program {
    fn token_literal(&self) -> &String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            panic!("no statement in program")
        }
    }

    fn string(&self) -> String {
        let mut message: String = String::from("");
        for item in self.statements.iter() {
            message += item.string().as_str()
        }
        message
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,         // (STATEMENT)
    pub name: Identifier,     // name of the variable (EXPRESSION)
    pub value: AllExpression, // value of the variable (EXPRESSION)
}

impl NodeTrait for LetStatement {
    fn token_literal(&self) -> &String {
        &self.token.literal
    }

    fn string(&self) -> String {
        let mut message: String = format!(
            "{} {} = {};",
            self.token_literal(),
            self.name.string(),
            self.value.string()
        );
        message
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token, // only expression can convert to indentifier and here it's the name of the variable
    pub value: String,
}

impl NodeTrait for Identifier {
    fn token_literal(&self) -> &String {
        &self.token.literal
    }

    fn string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Expression,
}

impl NodeTrait for ReturnStatement {
    fn token_literal(&self) -> &String {
        &self.token.literal
    }

    fn string(&self) -> String {
        let mut message: String = format!("{} {};", self.token_literal(), self.value.string());
        message
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub value: Option<AllExpression>,
}

impl NodeTrait for ExpressionStatement {
    fn token_literal(&self) -> &String {
        &self.token.literal
    }

    fn string(&self) -> String {
        let mut message: String = format!(
            "{} {};",
            self.token_literal(),
            match &self.value {
                Some(value) => value.string(),
                None => String::new(),
            }
        );
        message
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![AllStatement::LetStatement(LetStatement {
                token: Token {
                    typee: TokenType::LET,
                    literal: String::from("let"),
                },
                name: Identifier {
                    token: Token {
                        typee: TokenType::IDENT,
                        literal: String::from("myVar"),
                    },
                    value: String::from("myVar"),
                },
                value: AllExpression::Identifier(Identifier {
                    token: Token {
                        typee: TokenType::IDENT,
                        literal: String::from("anotherVar"),
                    },
                    value: String::from("anotherVar"),
                }),
            })],
        };
        println!("{:}", program.string());
        if program.string() != "let myVar = anotherVar;" {
            panic!("program.string() wrong. got={:?}", program.string())
        }
    }
}
