use std::io::Cursor;

use crate::filters::ParserTokenFilter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::unit::Unit;
use crate::{repr::*, Config, ErrorModule};

use super::test_span;

fn check_output_stmt(source: &str, expected: Option<Stmt>) {
    let errors = ErrorModule::new();
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
        Ok(stmt) => {
            assert_eq!(stmt, expected);
        }
        Err((err, _)) => {
            panic!("Expected a statement, got error: {:?}", err);
        }
    }

    if !errors.is_empty() {
        errors.dump(&mut units);
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
                    kind: StmtKind::Block(vec![Stmt {
                        kind: StmtKind::Expr(Expr {
                            kind: ExprKind::Literal(Literal::Int(3)),
                            span: test_span(23, 24),
                        }),
                        span: test_span(23, 25),
                    }]),
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

#[test]
fn while_stmt() {
    check_output_stmt(
        "while true a();",
        Some(Stmt {
            kind: StmtKind::While {
                condition: Expr {
                    kind: ExprKind::Literal(Literal::Bool(true)),
                    span: test_span(7, 11),
                },
                body: Box::new(Stmt {
                    kind: StmtKind::Expr(Expr {
                        kind: ExprKind::Call {
                            expr: Box::new(Expr {
                                kind: ExprKind::Variable(NamespacedIdentifier {
                                    namespace: vec![],
                                    ident: Identifier {
                                        name: "a".to_string(),
                                        span: test_span(12, 13),
                                    },
                                }),
                                span: test_span(12, 13),
                            }),
                            args: vec![],
                        },
                        span: test_span(12, 15),
                    }),
                    span: test_span(12, 16),
                }),
            },
            span: test_span(1, 16),
        }),
    );
}

#[test]
fn for_stmt() {
    check_output_stmt(
        "for a in b a();",
        Some(Stmt {
            kind: StmtKind::For {
                name: Identifier {
                    name: "a".to_string(),
                    span: test_span(5, 6),
                },
                iterable: Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            name: "b".to_string(),
                            span: test_span(10, 11),
                        },
                    }),
                    span: test_span(10, 11),
                },
                body: Box::new(Stmt {
                    kind: StmtKind::Expr(Expr {
                        kind: ExprKind::Call {
                            expr: Box::new(Expr {
                                kind: ExprKind::Variable(NamespacedIdentifier {
                                    namespace: vec![],
                                    ident: Identifier {
                                        name: "a".to_string(),
                                        span: test_span(12, 13),
                                    },
                                }),
                                span: test_span(12, 13),
                            }),
                            args: vec![],
                        },
                        span: test_span(12, 15),
                    }),
                    span: test_span(12, 16),
                }),
            },
            span: test_span(1, 16),
        }),
    );
}

#[test]
fn match_stmt_basic() {
    check_output_stmt(
        "match { on true 1; else 2; }",
        Some(Stmt {
            kind: StmtKind::Match {
                expr: None,
                arms: vec![
                    MatchArm {
                        pattern: MatchPattern::Expr(Expr {
                            kind: ExprKind::Literal(Literal::Bool(true)),
                            span: test_span(12, 16),
                        }),
                        body: Box::new(Stmt {
                            kind: StmtKind::Expr(Expr {
                                kind: ExprKind::Literal(Literal::Int(1)),
                                span: test_span(17, 18),
                            }),
                            span: test_span(17, 19),
                        }),
                    },
                    MatchArm {
                        pattern: MatchPattern::Wildcard,
                        body: Box::new(Stmt {
                            kind: StmtKind::Expr(Expr {
                                kind: ExprKind::Literal(Literal::Int(2)),
                                span: test_span(25, 26),
                            }),
                            span: test_span(25, 27),
                        }),
                    },
                ],
            },
            span: test_span(1, 29),
        }),
    );
}

#[test]
fn match_stmt_with_var() {
    check_output_stmt(
        "match a { on void false; else true; }",
        Some(Stmt {
            kind: StmtKind::Match {
                expr: Some(Identifier {
                    name: "a".to_string(),
                    span: test_span(7, 8),
                }),
                arms: vec![
                    MatchArm {
                        pattern: MatchPattern::Expr(Expr {
                            kind: ExprKind::Literal(Literal::Void),
                            span: test_span(14, 18),
                        }),
                        body: Box::new(Stmt {
                            kind: StmtKind::Expr(Expr {
                                kind: ExprKind::Literal(Literal::Bool(false)),
                                span: test_span(19, 24),
                            }),
                            span: test_span(19, 25),
                        }),
                    },
                    MatchArm {
                        pattern: MatchPattern::Wildcard,
                        body: Box::new(Stmt {
                            kind: StmtKind::Expr(Expr {
                                kind: ExprKind::Literal(Literal::Bool(true)),
                                span: test_span(31, 35),
                            }),
                            span: test_span(31, 36),
                        }),
                    },
                ],
            },
            span: test_span(1, 38),
        }),
    );
}

#[test]
fn match_stmt_with_var_type() {
    check_output_stmt(
        "match a { on Foo false; on Bar true; }",
        Some(Stmt {
            kind: StmtKind::Match {
                expr: Some(Identifier {
                    name: "a".to_string(),
                    span: test_span(7, 8),
                }),
                arms: vec![
                    MatchArm {
                        pattern: MatchPattern::Expr(Expr {
                            kind: ExprKind::Variable(NamespacedIdentifier {
                                namespace: vec![],
                                ident: Identifier {
                                    name: "Foo".to_string(),
                                    span: test_span(14, 17),
                                },
                            }),
                            span: test_span(14, 17),
                        }),
                        body: Box::new(Stmt {
                            kind: StmtKind::Expr(Expr {
                                kind: ExprKind::Literal(Literal::Bool(false)),
                                span: test_span(18, 23),
                            }),
                            span: test_span(18, 24),
                        }),
                    },
                    MatchArm {
                        pattern: MatchPattern::Expr(Expr {
                            kind: ExprKind::Variable(NamespacedIdentifier {
                                namespace: vec![],
                                ident: Identifier {
                                    name: "Bar".to_string(),
                                    span: test_span(28, 31),
                                },
                            }),
                            span: test_span(28, 31),
                        }),
                        body: Box::new(Stmt {
                            kind: StmtKind::Expr(Expr {
                                kind: ExprKind::Literal(Literal::Bool(true)),
                                span: test_span(32, 36),
                            }),
                            span: test_span(32, 37),
                        }),
                    },
                ],
            },
            span: test_span(1, 39),
        }),
    );
}

#[test]
fn match_ctor() {
    check_output_stmt(
        "match a { on Foo -> { value: 1 } { hi(); } else { bye(); } }",
        Some(Stmt {
            kind: StmtKind::Match {
                expr: Some(Identifier {
                    name: "a".to_string(),
                    span: test_span(7, 8),
                }),
                arms: vec![
                    MatchArm {
                        pattern: MatchPattern::Expr(Expr {
                            kind: ExprKind::Constructor {
                                ty: NamespacedIdentifier {
                                    namespace: vec![],
                                    ident: Identifier {
                                        name: "Foo".to_string(),
                                        span: test_span(14, 17),
                                    },
                                },
                                fields: vec![FieldConstructor::Named {
                                    name: Identifier {
                                        name: "value".to_string(),
                                        span: test_span(23, 28),
                                    },
                                    value: Expr {
                                        kind: ExprKind::Literal(Literal::Int(1)),
                                        span: test_span(30, 31),
                                    },
                                }],
                            },
                            span: test_span(14, 33),
                        }),
                        body: Box::new(Stmt {
                            kind: StmtKind::Block(vec![Stmt {
                                kind: StmtKind::Expr(Expr {
                                    kind: ExprKind::Call {
                                        expr: Box::new(Expr {
                                            kind: ExprKind::Variable(NamespacedIdentifier {
                                                namespace: vec![],
                                                ident: Identifier {
                                                    name: "hi".to_string(),
                                                    span: test_span(36, 38),
                                                },
                                            }),
                                            span: test_span(36, 38),
                                        }),
                                        args: vec![],
                                    },
                                    span: test_span(36, 40),
                                }),
                                span: test_span(36, 41),
                            }]),
                            span: test_span(34, 43),
                        }),
                    },
                    MatchArm {
                        pattern: MatchPattern::Wildcard,
                        body: Box::new(Stmt {
                            kind: StmtKind::Block(vec![Stmt {
                                kind: StmtKind::Expr(Expr {
                                    kind: ExprKind::Call {
                                        expr: Box::new(Expr {
                                            kind: ExprKind::Variable(NamespacedIdentifier {
                                                namespace: vec![],
                                                ident: Identifier {
                                                    name: "bye".to_string(),
                                                    span: test_span(51, 54),
                                                },
                                            }),
                                            span: test_span(51, 54),
                                        }),
                                        args: vec![],
                                    },
                                    span: test_span(51, 56),
                                }),
                                span: test_span(51, 57),
                            }]),
                            span: test_span(49, 59),
                        }),
                    },
                ],
            },
            span: test_span(1, 61),
        }),
    );
}
