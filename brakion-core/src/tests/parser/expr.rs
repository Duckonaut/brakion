use std::io::Cursor;

use crate::filters::ParserTokenFilter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::unit::{Location, Span, Unit};
use crate::{repr::*, Config, ErrorModule};

fn check_output_expr(source: &str, expected: Option<Expr>) {
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
    let output = parser.parse_expr();

    match output {
        crate::parser::ParserResult::Ok(expr) => {
            if expected.is_none() {
                panic!("Expected no expression, got {:?}", expr);
            }
            assert_eq!(expr, expected.unwrap());
        }
        crate::parser::ParserResult::None => {
            if expected.is_some() {
                panic!("Expected an expression, got None");
            }
        }
        crate::parser::ParserResult::Err(err, _) => {
            panic!("Expected an expression, got error: {:?}", err);
        }
    }

    if !errors.lock().unwrap().errors.is_empty() {
        errors.lock().unwrap().dump(&mut units);
        panic!("Errors encountered during parsing");
    }
}

fn test_span(start: usize, end: usize) -> Span {
    Span::new(0, Location::new(1, 0, start), Location::new(1, 0, end))
}

#[test]
fn empty() {
    check_output_expr("", None);
}

#[test]
fn identifier() {
    check_output_expr(
        "foo",
        Some(Expr {
            kind: ExprKind::Variable(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    span: test_span(1, 4),
                    name: "foo".to_string(),
                },
            }),
            span: test_span(1, 4),
        }),
    );
}

#[test]
fn namespaced_identifier() {
    check_output_expr(
        "foo::Bar",
        Some(Expr {
            kind: ExprKind::Variable(NamespacedIdentifier {
                namespace: vec![Identifier {
                    span: test_span(1, 4),
                    name: "foo".to_string(),
                }],
                ident: Identifier {
                    span: test_span(6, 9),
                    name: "Bar".to_string(),
                },
            }),
            span: test_span(1, 9),
        }),
    );
}

#[test]
fn literal_integer() {
    check_output_expr(
        "123",
        Some(Expr {
            kind: ExprKind::Literal(Literal::Int(123)),
            span: test_span(1, 4),
        }),
    );
}

#[test]
fn literal_float() {
    check_output_expr(
        "123.456",
        Some(Expr {
            kind: ExprKind::Literal(Literal::Float(123.456)),
            span: test_span(1, 8),
        }),
    );
}

#[test]
fn literal_string() {
    check_output_expr(
        "\"foo\"",
        Some(Expr {
            kind: ExprKind::Literal(Literal::String("foo".to_string())),
            span: test_span(1, 6),
        }),
    );
}

#[test]
fn literal_string_escape() {
    check_output_expr(
        "\"foo\\nbar\"",
        Some(Expr {
            kind: ExprKind::Literal(Literal::String("foo\nbar".to_string())),
            span: test_span(1, 11),
        }),
    );
}

#[test]
fn literal_char() {
    check_output_expr(
        "'f'",
        Some(Expr {
            kind: ExprKind::Literal(Literal::Char('f')),
            span: test_span(1, 4),
        }),
    );
}

#[test]
fn literal_void() {
    check_output_expr(
        "void",
        Some(Expr {
            kind: ExprKind::Literal(Literal::Void),
            span: test_span(1, 5),
        }),
    );
}

#[test]
fn literal_bool_true() {
    check_output_expr(
        "true",
        Some(Expr {
            kind: ExprKind::Literal(Literal::Bool(true)),
            span: test_span(1, 5),
        }),
    );
}

#[test]
fn literal_bool_false() {
    check_output_expr(
        "false",
        Some(Expr {
            kind: ExprKind::Literal(Literal::Bool(false)),
            span: test_span(1, 6),
        }),
    );
}

#[test]
fn literal_list_single_elem() {
    check_output_expr(
        "[1]",
        Some(Expr {
            kind: ExprKind::Literal(Literal::List(vec![Expr {
                kind: ExprKind::Literal(Literal::Int(1)),
                span: test_span(2, 3),
            }])),
            span: test_span(1, 4),
        }),
    );
    check_output_expr(
        "[1,]",
        Some(Expr {
            kind: ExprKind::Literal(Literal::List(vec![Expr {
                kind: ExprKind::Literal(Literal::Int(1)),
                span: test_span(2, 3),
            }])),
            span: test_span(1, 5),
        }),
    );
}

#[test]
fn literal_list_multiple_elem() {
    check_output_expr(
        "[1, 2, 3]",
        Some(Expr {
            kind: ExprKind::Literal(Literal::List(vec![
                Expr {
                    kind: ExprKind::Literal(Literal::Int(1)),
                    span: test_span(2, 3),
                },
                Expr {
                    kind: ExprKind::Literal(Literal::Int(2)),
                    span: test_span(5, 6),
                },
                Expr {
                    kind: ExprKind::Literal(Literal::Int(3)),
                    span: test_span(8, 9),
                },
            ])),
            span: test_span(1, 10),
        }),
    );
    check_output_expr(
        "[1, 2, 3,]",
        Some(Expr {
            kind: ExprKind::Literal(Literal::List(vec![
                Expr {
                    kind: ExprKind::Literal(Literal::Int(1)),
                    span: test_span(2, 3),
                },
                Expr {
                    kind: ExprKind::Literal(Literal::Int(2)),
                    span: test_span(5, 6),
                },
                Expr {
                    kind: ExprKind::Literal(Literal::Int(3)),
                    span: test_span(8, 9),
                },
            ])),
            span: test_span(1, 11),
        }),
    );
}

#[test]
fn term_simple() {
    check_output_expr(
        "a + b",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(1, 2),
                            name: "a".to_string(),
                        },
                    }),
                    span: test_span(1, 2),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(5, 6),
                            name: "b".to_string(),
                        },
                    }),
                    span: test_span(5, 6),
                }),
            },
            span: test_span(1, 6),
        }),
    );
}

#[test]
fn term_repeating() {
    check_output_expr(
        "a + b - c + d",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::Sub,
                        left: Box::new(Expr {
                            kind: ExprKind::Binary {
                                op: BinaryOp::Add,
                                left: Box::new(Expr {
                                    kind: ExprKind::Variable(NamespacedIdentifier {
                                        namespace: vec![],
                                        ident: Identifier {
                                            span: test_span(1, 2),
                                            name: "a".to_string(),
                                        },
                                    }),
                                    span: test_span(1, 2),
                                }),
                                right: Box::new(Expr {
                                    kind: ExprKind::Variable(NamespacedIdentifier {
                                        namespace: vec![],
                                        ident: Identifier {
                                            span: test_span(5, 6),
                                            name: "b".to_string(),
                                        },
                                    }),
                                    span: test_span(5, 6),
                                }),
                            },
                            span: test_span(1, 6),
                        }),
                        right: Box::new(Expr {
                            kind: ExprKind::Variable(NamespacedIdentifier {
                                namespace: vec![],
                                ident: Identifier {
                                    span: test_span(9, 10),
                                    name: "c".to_string(),
                                },
                            }),
                            span: test_span(9, 10),
                        }),
                    },
                    span: test_span(1, 10),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(13, 14),
                            name: "d".to_string(),
                        },
                    }),
                    span: test_span(13, 14),
                }),
            },
            span: test_span(1, 14),
        }),
    );
}

#[test]
fn factor_simple() {
    check_output_expr(
        "a * b",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Mul,
                left: Box::new(Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(1, 2),
                            name: "a".to_string(),
                        },
                    }),
                    span: test_span(1, 2),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(5, 6),
                            name: "b".to_string(),
                        },
                    }),
                    span: test_span(5, 6),
                }),
            },
            span: test_span(1, 6),
        }),
    );
}

#[test]
fn factor_repeating() {
    check_output_expr(
        "a * b / c * d",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Mul,
                left: Box::new(Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::Div,
                        left: Box::new(Expr {
                            kind: ExprKind::Binary {
                                op: BinaryOp::Mul,
                                left: Box::new(Expr {
                                    kind: ExprKind::Variable(NamespacedIdentifier {
                                        namespace: vec![],
                                        ident: Identifier {
                                            span: test_span(1, 2),
                                            name: "a".to_string(),
                                        },
                                    }),
                                    span: test_span(1, 2),
                                }),
                                right: Box::new(Expr {
                                    kind: ExprKind::Variable(NamespacedIdentifier {
                                        namespace: vec![],
                                        ident: Identifier {
                                            span: test_span(5, 6),
                                            name: "b".to_string(),
                                        },
                                    }),
                                    span: test_span(5, 6),
                                }),
                            },
                            span: test_span(1, 6),
                        }),
                        right: Box::new(Expr {
                            kind: ExprKind::Variable(NamespacedIdentifier {
                                namespace: vec![],
                                ident: Identifier {
                                    span: test_span(9, 10),
                                    name: "c".to_string(),
                                },
                            }),
                            span: test_span(9, 10),
                        }),
                    },
                    span: test_span(1, 10),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(13, 14),
                            name: "d".to_string(),
                        },
                    }),
                    span: test_span(13, 14),
                }),
            },
            span: test_span(1, 14),
        }),
    );
}

#[test]
fn is() {
    check_output_expr(
        "a is Foo",
        Some(Expr {
            kind: ExprKind::TypeBinary {
                expr: Box::new(Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(1, 2),
                            name: "a".to_string(),
                        },
                    }),
                    span: test_span(1, 2),
                }),
                ty: TypeReference {
                    kind: TypeReferenceKind::Named(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(6, 9),
                            name: "Foo".to_string(),
                        },
                    }),
                    span: Some(test_span(6, 9)),
                },
                op: TypeBinaryOp::Is,
            },
            span: test_span(1, 9),
        }),
    )
}
