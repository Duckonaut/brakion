use std::io::Cursor;

use crate::filters::ParserTokenFilter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::unit::Unit;
use crate::{repr::*, Config, ErrorModule};

use super::test_span;

fn check_output_stmt(source: &str, expected: Option<Stmt>) {
    let errors = ErrorModule::new_ref();
    let config = Config::default();

    let unit = Unit::new(
        "<test>".to_string(),
        0,
        Box::new(Cursor::new(source.to_string())),
    );

    let mut units = vec![unit];

    let mut lexer = Lexer::new(units.iter_mut().next().unwrap(), &config, errors.clone());
    let filtered = ParserTokenFilter::new(&mut lexer);

    let mut parser = Parser::new(&config, filtered, errors.clone());
    let output = parser.parse_stmt();

    match output {
        crate::parser::ParserResult::Ok(stmt) => {
            if expected.is_none() {
                panic!("Expected no statement, got {:?}", stmt);
            }
            assert_eq!(stmt, expected.unwrap());
        }
        crate::parser::ParserResult::None => {
            if expected.is_some() {
                panic!("Expected a statement, got None");
            }
        }
        crate::parser::ParserResult::Err(err, _) => {
            panic!("Expected a statement, got error: {:?}", err);
        }
    }

    if !errors.lock().unwrap().errors.is_empty() {
        errors.lock().unwrap().dump(&mut units);
        panic!("Errors encountered during parsing");
    }
}

#[test]
fn empty() {
    check_output_stmt("", None);
}

#[test]
fn comment() {
    check_output_stmt("# comment", None);
}

#[test]
fn comment_with_newline() {
    check_output_stmt("# comment\n", None);
}

#[test]
fn comment_with_newline_and_space() {
    check_output_stmt("# comment\n ", None);
}

#[test]
fn expr() {
    check_output_stmt(
        "1;",
        Some(Stmt {
            kind: StmtKind::Expr(Expr {
                kind: ExprKind::Literal(Literal::Int(1)),
                span: test_span(1, 2),
            }),
            span: test_span(1, 3),
        }),
    );
}

#[test]
fn assignment() {
    check_output_stmt(
        "a = 1;",
        Some(Stmt {
            kind: StmtKind::Assign {
                target: Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            name: "a".to_string(),
                            span: test_span(1, 2),
                        },
                    }),
                    span: test_span(1, 2),
                },
                value: Expr {
                    kind: ExprKind::Literal(Literal::Int(1)),
                    span: test_span(5, 6),
                },
            },
            span: test_span(1, 7),
        }),
    );
}

#[test]
fn variable() {
    check_output_stmt(
        "var a = 1;",
        Some(Stmt {
            kind: StmtKind::Variable {
                name: Identifier {
                    name: "a".to_string(),
                    span: test_span(5, 6),
                },
                ty: TypeReference {
                    kind: TypeReferenceKind::Infer,
                    span: None,
                },
                value: Expr {
                    kind: ExprKind::Literal(Literal::Int(1)),
                    span: test_span(9, 10),
                },
            },
            span: test_span(1, 11),
        }),
    );
}

#[test]
fn break_stmt() {
    check_output_stmt(
        "break;",
        Some(Stmt {
            kind: StmtKind::Break,
            span: test_span(1, 7),
        }),
    );
}

#[test]
fn continue_stmt() {
    check_output_stmt(
        "continue;",
        Some(Stmt {
            kind: StmtKind::Continue,
            span: test_span(1, 10),
        }),
    );
}

#[test]
fn return_stmt() {
    check_output_stmt(
        "return;",
        Some(Stmt {
            kind: StmtKind::Return(Expr {
                kind: ExprKind::Literal(Literal::Void),
                span: test_span(1, 7),
            }),
            span: test_span(1, 8),
        }),
    );
}

#[test]
fn empty_block() {
    check_output_stmt(
        "{}",
        Some(Stmt {
            kind: StmtKind::Block(vec![]),
            span: test_span(1, 3),
        }),
    );
}

#[test]
fn block_with_stmt() {
    check_output_stmt(
        "{ 1; }",
        Some(Stmt {
            kind: StmtKind::Block(vec![Stmt {
                kind: StmtKind::Expr(Expr {
                    kind: ExprKind::Literal(Literal::Int(1)),
                    span: test_span(3, 4),
                }),
                span: test_span(3, 5),
            }]),
            span: test_span(1, 7),
        }),
    );
}

#[test]
fn block_with_stmts() {
    check_output_stmt(
        "{ 1; 2; }",
        Some(Stmt {
            kind: StmtKind::Block(vec![
                Stmt {
                    kind: StmtKind::Expr(Expr {
                        kind: ExprKind::Literal(Literal::Int(1)),
                        span: test_span(3, 4),
                    }),
                    span: test_span(3, 5),
                },
                Stmt {
                    kind: StmtKind::Expr(Expr {
                        kind: ExprKind::Literal(Literal::Int(2)),
                        span: test_span(6, 7),
                    }),
                    span: test_span(6, 8),
                },
            ]),
            span: test_span(1, 10),
        }),
    );
}

#[test]
fn if_stmt() {
    check_output_stmt(
        "if true { 2; }",
        Some(Stmt {
            kind: StmtKind::If {
                condition: Expr {
                    kind: ExprKind::Literal(Literal::Bool(true)),
                    span: test_span(4, 8),
                },
                then: Box::new(Stmt {
                    kind: StmtKind::Block(vec![Stmt {
                        kind: StmtKind::Expr(Expr {
                            kind: ExprKind::Literal(Literal::Int(2)),
                            span: test_span(11, 12),
                        }),
                        span: test_span(11, 13),
                    }]),
                    span: test_span(9, 15),
                }),
                otherwise: None,
            },
            span: test_span(1, 15),
        }),
    );
}

#[test]
fn if_else() {
    check_output_stmt(
        "if true { 2; } else { 3; }",
        Some(Stmt {
            kind: StmtKind::If {
                condition: Expr {
                    kind: ExprKind::Literal(Literal::Bool(true)),
                    span: test_span(4, 8),
                },
                then: Box::new(Stmt {
                    kind: StmtKind::Block(vec![Stmt {
                        kind: StmtKind::Expr(Expr {
                            kind: ExprKind::Literal(Literal::Int(2)),
                            span: test_span(11, 12),
                        }),
                        span: test_span(11, 13),
                    }]),
                    span: test_span(9, 15),
                }),
                otherwise: Some(Box::new(Stmt {
                    kind: StmtKind::Block(vec![
                        Stmt {
                            kind: StmtKind::Expr(Expr {
                                kind: ExprKind::Literal(Literal::Int(3)),
                                span: test_span(23, 24),
                            }),
                            span: test_span(23, 25),
                        },
                    ]),
                    span: test_span(21, 27),
                })),
            },
            span: test_span(1, 27),
        }),
    );
}

#[test]
fn if_else_chain() {
    check_output_stmt(
        "if false 1; else if true 2; else 3;",
        Some(Stmt {
            kind: StmtKind::If {
                condition: Expr {
                    kind: ExprKind::Literal(Literal::Bool(false)),
                    span: test_span(4, 9),
                },
                then: Box::new(Stmt {
                    kind: StmtKind::Expr(Expr {
                        kind: ExprKind::Literal(Literal::Int(1)),
                        span: test_span(10, 11),
                    }),
                    span: test_span(10, 12),
                }),
                otherwise: Some(Box::new(Stmt {
                    kind: StmtKind::If {
                        condition: Expr {
                            kind: ExprKind::Literal(Literal::Bool(true)),
                            span: test_span(21, 25),
                        },
                        then: Box::new(Stmt {
                            kind: StmtKind::Expr(Expr {
                                kind: ExprKind::Literal(Literal::Int(2)),
                                span: test_span(26, 27),
                            }),
                            span: test_span(26, 28),
                        }),
                        otherwise: Some(Box::new(Stmt {
                            kind: StmtKind::Expr(Expr {
                                kind: ExprKind::Literal(Literal::Int(3)),
                                span: test_span(34, 35),
                            }),
                            span: test_span(34, 36),
                        })),
                    },
                    span: test_span(18, 36),
                })),
            },
            span: test_span(1, 36),
        }),
    );
}
