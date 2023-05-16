use std::io::Cursor;

use crate::filters::ParserTokenFilter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::unit::Unit;
use crate::{repr::*, Config, ErrorModule};

use crate::tests::test_span;

fn check_output_expr(source: &str, expected: Option<Expr>) {
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
    let output = parser.parse_expr();

    match output {
        Ok(expr) => {
            assert_eq!(expr, expected);
        }
        Err((err, _)) => {
            panic!("Expected an expression, got error: {:?}", err);
        }
    }

    if !errors.is_empty() {
        errors.dump(&mut units);
        panic!("Errors encountered during parsing");
    }
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
fn or() {
    check_output_expr(
        "a or false",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Or,
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
                    kind: ExprKind::Literal(Literal::Bool(false)),
                    span: test_span(6, 11),
                }),
            },
            span: test_span(1, 11),
        }),
    );
}

#[test]
fn and() {
    check_output_expr(
        "a and true",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::And,
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
                    kind: ExprKind::Literal(Literal::Bool(true)),
                    span: test_span(7, 11),
                }),
            },
            span: test_span(1, 11),
        }),
    );
}

#[test]
fn and_or_precedence() {
    // `and` binds tighter than `or`
    // `a and b or c or d and (e or f)` is equivalent to
    // `((a and b) or c) or (d and (e or f))`
    check_output_expr(
        "a and b or c or d and (e or f)",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Or,
                left: Box::new(Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::Or,
                        left: Box::new(Expr {
                            kind: ExprKind::Binary {
                                op: BinaryOp::And,
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
                                            span: test_span(7, 8),
                                            name: "b".to_string(),
                                        },
                                    }),
                                    span: test_span(7, 8),
                                }),
                            },
                            span: test_span(1, 8),
                        }),
                        right: Box::new(Expr {
                            kind: ExprKind::Variable(NamespacedIdentifier {
                                namespace: vec![],
                                ident: Identifier {
                                    span: test_span(12, 13),
                                    name: "c".to_string(),
                                },
                            }),
                            span: test_span(12, 13),
                        }),
                    },
                    span: test_span(1, 13),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::And,
                        left: Box::new(Expr {
                            kind: ExprKind::Variable(NamespacedIdentifier {
                                namespace: vec![],
                                ident: Identifier {
                                    span: test_span(17, 18),
                                    name: "d".to_string(),
                                },
                            }),
                            span: test_span(17, 18),
                        }),
                        right: Box::new(Expr {
                            kind: ExprKind::Binary {
                                op: BinaryOp::Or,
                                left: Box::new(Expr {
                                    kind: ExprKind::Variable(NamespacedIdentifier {
                                        namespace: vec![],
                                        ident: Identifier {
                                            span: test_span(24, 25),
                                            name: "e".to_string(),
                                        },
                                    }),
                                    span: test_span(24, 25),
                                }),
                                right: Box::new(Expr {
                                    kind: ExprKind::Variable(NamespacedIdentifier {
                                        namespace: vec![],
                                        ident: Identifier {
                                            span: test_span(29, 30),
                                            name: "f".to_string(),
                                        },
                                    }),
                                    span: test_span(29, 30),
                                }),
                            },
                            span: test_span(23, 31),
                        }),
                    },
                    span: test_span(17, 31),
                }),
            },
            span: test_span(1, 31),
        }),
    );
}

#[test]
fn equality() {
    check_output_expr(
        "a == b",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Eq,
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
                            span: test_span(6, 7),
                            name: "b".to_string(),
                        },
                    }),
                    span: test_span(6, 7),
                }),
            },
            span: test_span(1, 7),
        }),
    );
}

#[test]
fn nequality() {
    check_output_expr(
        "a != b",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Neq,
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
                            span: test_span(6, 7),
                            name: "b".to_string(),
                        },
                    }),
                    span: test_span(6, 7),
                }),
            },
            span: test_span(1, 7),
        }),
    );
}

#[test]
fn eq_neq_lack_precedence() {
    check_output_expr(
        "a == b != c",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Neq,
                left: Box::new(Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::Eq,
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
                                    span: test_span(6, 7),
                                    name: "b".to_string(),
                                },
                            }),
                            span: test_span(6, 7),
                        }),
                    },
                    span: test_span(1, 7),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(11, 12),
                            name: "c".to_string(),
                        },
                    }),
                    span: test_span(11, 12),
                }),
            },
            span: test_span(1, 12),
        }),
    );

    check_output_expr(
        "a != b == c",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Eq,
                left: Box::new(Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::Neq,
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
                                    span: test_span(6, 7),
                                    name: "b".to_string(),
                                },
                            }),
                            span: test_span(6, 7),
                        }),
                    },
                    span: test_span(1, 7),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(11, 12),
                            name: "c".to_string(),
                        },
                    }),
                    span: test_span(11, 12),
                }),
            },
            span: test_span(1, 12),
        }),
    );
}

#[test]
fn comparisons() {
    check_output_expr(
        "a < b",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Lt,
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

    check_output_expr(
        "a > b",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Gt,
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

    check_output_expr(
        "a <= b",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Leq,
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
                            span: test_span(6, 7),
                            name: "b".to_string(),
                        },
                    }),
                    span: test_span(6, 7),
                }),
            },
            span: test_span(1, 7),
        }),
    );

    check_output_expr(
        "a >= b",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Geq,
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
                            span: test_span(6, 7),
                            name: "b".to_string(),
                        },
                    }),
                    span: test_span(6, 7),
                }),
            },
            span: test_span(1, 7),
        }),
    );
}

#[test]
fn comparisons_left_to_right() {
    check_output_expr(
        "a < b < c",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Lt,
                left: Box::new(Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::Lt,
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
    );

    check_output_expr(
        "a > b > c",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Gt,
                left: Box::new(Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::Gt,
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
    );

    check_output_expr(
        "a > b < c",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Lt,
                left: Box::new(Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::Gt,
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
    );

    check_output_expr(
        "a < b > c",
        Some(Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Gt,
                left: Box::new(Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::Lt,
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

#[test]
fn is_chain() {
    check_output_expr(
        "a is Foo is bool",
        Some(Expr {
            kind: ExprKind::TypeBinary {
                op: TypeBinaryOp::Is,
                expr: Box::new(Expr {
                    kind: ExprKind::TypeBinary {
                        op: TypeBinaryOp::Is,
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
                    },
                    span: test_span(1, 9),
                }),
                ty: TypeReference {
                    kind: TypeReferenceKind::Named(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(13, 17),
                            name: "bool".to_string(),
                        },
                    }),
                    span: Some(test_span(13, 17)),
                },
            },
            span: test_span(1, 17),
        }),
    )
}

#[test]
fn as_type() {
    check_output_expr(
        "a as Foo",
        Some(Expr {
            kind: ExprKind::TypeBinary {
                op: TypeBinaryOp::As,
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
            },
            span: test_span(1, 9),
        }),
    )
}

#[test]
fn as_chain() {
    check_output_expr(
        "a as Foo | void as bool",
        Some(Expr {
            kind: ExprKind::TypeBinary {
                op: TypeBinaryOp::As,
                expr: Box::new(Expr {
                    kind: ExprKind::TypeBinary {
                        op: TypeBinaryOp::As,
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
                            kind: TypeReferenceKind::Union(vec![
                                TypeReference {
                                    kind: TypeReferenceKind::Named(NamespacedIdentifier {
                                        namespace: vec![],
                                        ident: Identifier {
                                            span: test_span(6, 9),
                                            name: "Foo".to_string(),
                                        },
                                    }),
                                    span: Some(test_span(6, 9)),
                                },
                                TypeReference {
                                    kind: TypeReferenceKind::Void,
                                    span: Some(test_span(12, 16)),
                                },
                            ]),
                            span: Some(test_span(6, 16)),
                        },
                    },
                    span: test_span(1, 16),
                }),
                ty: TypeReference {
                    kind: TypeReferenceKind::Named(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(20, 24),
                            name: "bool".to_string(),
                        },
                    }),
                    span: Some(test_span(20, 24)),
                },
            },
            span: test_span(1, 24),
        }),
    )
}

#[test]
fn unary() {
    check_output_expr(
        "!a",
        Some(Expr {
            kind: ExprKind::Unary {
                op: UnaryOp::Not,
                expr: Box::new(Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            span: test_span(2, 3),
                            name: "a".to_string(),
                        },
                    }),
                    span: test_span(2, 3),
                }),
            },
            span: test_span(1, 3),
        }),
    )
}

#[test]
fn unary_chain() {
    check_output_expr(
        "-!-!!a",
        Some(Expr {
            kind: ExprKind::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::Not,
                        expr: Box::new(Expr {
                            kind: ExprKind::Unary {
                                op: UnaryOp::Neg,
                                expr: Box::new(Expr {
                                    kind: ExprKind::Unary {
                                        op: UnaryOp::Not,
                                        expr: Box::new(Expr {
                                            kind: ExprKind::Unary {
                                                op: UnaryOp::Not,
                                                expr: Box::new(Expr {
                                                    kind: ExprKind::Variable(
                                                        NamespacedIdentifier {
                                                            namespace: vec![],
                                                            ident: Identifier {
                                                                span: test_span(6, 7),
                                                                name: "a".to_string(),
                                                            },
                                                        },
                                                    ),
                                                    span: test_span(6, 7),
                                                }),
                                            },
                                            span: test_span(5, 7),
                                        }),
                                    },
                                    span: test_span(4, 7),
                                }),
                            },
                            span: test_span(3, 7),
                        }),
                    },
                    span: test_span(2, 7),
                }),
            },
            span: test_span(1, 7),
        }),
    )
}

#[test]
fn field_access() {
    check_output_expr(
        "a.b",
        Some(Expr {
            kind: ExprKind::Access {
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
                field: Identifier {
                    span: test_span(3, 4),
                    name: "b".to_string(),
                },
            },
            span: test_span(1, 4),
        }),
    )
}

#[test]
fn field_access_chain() {
    check_output_expr(
        "a.b.c",
        Some(Expr {
            kind: ExprKind::Access {
                expr: Box::new(Expr {
                    kind: ExprKind::Access {
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
                        field: Identifier {
                            span: test_span(3, 4),
                            name: "b".to_string(),
                        },
                    },
                    span: test_span(1, 4),
                }),
                field: Identifier {
                    span: test_span(5, 6),
                    name: "c".to_string(),
                },
            },
            span: test_span(1, 6),
        }),
    )
}

#[test]
fn call() {
    check_output_expr(
        "a()",
        Some(Expr {
            kind: ExprKind::Call {
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
                args: vec![],
            },
            span: test_span(1, 4),
        }),
    )
}

#[test]
fn namespace_call() {
    check_output_expr(
        "a::b()",
        Some(Expr {
            kind: ExprKind::Call {
                expr: Box::new(Expr {
                    kind: ExprKind::Variable(NamespacedIdentifier {
                        namespace: vec![Identifier {
                            span: test_span(1, 2),
                            name: "a".to_string(),
                        }],
                        ident: Identifier {
                            span: test_span(4, 5),
                            name: "b".to_string(),
                        },
                    }),
                    span: test_span(1, 5),
                }),
                args: vec![],
            },
            span: test_span(1, 7),
        }),
    )
}

#[test]
fn call_chain() {
    check_output_expr(
        "a()()",
        Some(Expr {
            kind: ExprKind::Call {
                expr: Box::new(Expr {
                    kind: ExprKind::Call {
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
                        args: vec![],
                    },
                    span: test_span(1, 4),
                }),
                args: vec![],
            },
            span: test_span(1, 6),
        }),
    )
}

#[test]
fn call_with_args() {
    check_output_expr(
        "a(b, c)",
        Some(Expr {
            kind: ExprKind::Call {
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
                args: vec![
                    Expr {
                        kind: ExprKind::Variable(NamespacedIdentifier {
                            namespace: vec![],
                            ident: Identifier {
                                span: test_span(3, 4),
                                name: "b".to_string(),
                            },
                        }),
                        span: test_span(3, 4),
                    },
                    Expr {
                        kind: ExprKind::Variable(NamespacedIdentifier {
                            namespace: vec![],
                            ident: Identifier {
                                span: test_span(6, 7),
                                name: "c".to_string(),
                            },
                        }),
                        span: test_span(6, 7),
                    },
                ],
            },
            span: test_span(1, 8),
        }),
    )
}

#[test]
fn call_with_args_trailing_comma() {
    check_output_expr(
        "a(b, c,)",
        Some(Expr {
            kind: ExprKind::Call {
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
                args: vec![
                    Expr {
                        kind: ExprKind::Variable(NamespacedIdentifier {
                            namespace: vec![],
                            ident: Identifier {
                                span: test_span(3, 4),
                                name: "b".to_string(),
                            },
                        }),
                        span: test_span(3, 4),
                    },
                    Expr {
                        kind: ExprKind::Variable(NamespacedIdentifier {
                            namespace: vec![],
                            ident: Identifier {
                                span: test_span(6, 7),
                                name: "c".to_string(),
                            },
                        }),
                        span: test_span(6, 7),
                    },
                ],
            },
            span: test_span(1, 9),
        }),
    )
}

#[test]
fn indexing() {
    check_output_expr(
        "a[0]",
        Some(Expr {
            kind: ExprKind::Index {
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
                index: Box::new(Expr {
                    kind: ExprKind::Literal(Literal::Int(0)),
                    span: test_span(3, 4),
                }),
            },
            span: test_span(1, 5),
        }),
    )
}

#[test]
fn indexing_chain() {
    check_output_expr(
        "a[0][1][b[c]]",
        Some(Expr {
            kind: ExprKind::Index {
                expr: Box::new(Expr {
                    kind: ExprKind::Index {
                        expr: Box::new(Expr {
                            kind: ExprKind::Index {
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
                                index: Box::new(Expr {
                                    kind: ExprKind::Literal(Literal::Int(0)),
                                    span: test_span(3, 4),
                                }),
                            },
                            span: test_span(1, 5),
                        }),
                        index: Box::new(Expr {
                            kind: ExprKind::Literal(Literal::Int(1)),
                            span: test_span(6, 7),
                        }),
                    },
                    span: test_span(1, 8),
                }),
                index: Box::new(Expr {
                    kind: ExprKind::Index {
                        expr: Box::new(Expr {
                            kind: ExprKind::Variable(NamespacedIdentifier {
                                namespace: vec![],
                                ident: Identifier {
                                    span: test_span(9, 10),
                                    name: "b".to_string(),
                                },
                            }),
                            span: test_span(9, 10),
                        }),
                        index: Box::new(Expr {
                            kind: ExprKind::Variable(NamespacedIdentifier {
                                namespace: vec![],
                                ident: Identifier {
                                    span: test_span(11, 12),
                                    name: "c".to_string(),
                                },
                            }),
                            span: test_span(11, 12),
                        }),
                    },
                    span: test_span(9, 13),
                }),
            },
            span: test_span(1, 14),
        }),
    )
}

#[test]
fn indexing_call() {
    check_output_expr(
        "a[0]()",
        Some(Expr {
            kind: ExprKind::Call {
                expr: Box::new(Expr {
                    kind: ExprKind::Index {
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
                        index: Box::new(Expr {
                            kind: ExprKind::Literal(Literal::Int(0)),
                            span: test_span(3, 4),
                        }),
                    },
                    span: test_span(1, 5),
                }),
                args: vec![],
            },
            span: test_span(1, 7),
        }),
    )
}

#[test]
fn field_call() {
    check_output_expr(
        "a.b()",
        Some(Expr {
            kind: ExprKind::Call {
                expr: Box::new(Expr {
                    kind: ExprKind::Access {
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
                        field: Identifier {
                            span: test_span(3, 4),
                            name: "b".to_string(),
                        },
                    },
                    span: test_span(1, 4),
                }),
                args: vec![],
            },
            span: test_span(1, 6),
        }),
    )
}

#[test]
fn call_field() {
    check_output_expr(
        "a().b",
        Some(Expr {
            kind: ExprKind::Access {
                expr: Box::new(Expr {
                    kind: ExprKind::Call {
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
                        args: vec![],
                    },
                    span: test_span(1, 4),
                }),
                field: Identifier {
                    span: test_span(5, 6),
                    name: "b".to_string(),
                },
            },
            span: test_span(1, 6),
        }),
    )
}

#[test]
fn constructor() {
    check_output_expr(
        "Foo -> { a, b: 1 }",
        Some(Expr {
            kind: ExprKind::Constructor {
                ty: NamespacedIdentifier {
                    namespace: vec![],
                    ident: Identifier {
                        span: test_span(1, 4),
                        name: "Foo".to_string(),
                    },
                },
                fields: vec![
                    FieldConstructor::Auto(Identifier {
                        span: test_span(10, 11),
                        name: "a".to_string(),
                    }),
                    FieldConstructor::Named {
                        name: Identifier {
                            span: test_span(13, 14),
                            name: "b".to_string(),
                        },
                        value: Expr {
                            kind: ExprKind::Literal(Literal::Int(1)),
                            span: test_span(16, 17),
                        },
                    },
                ],
            },
            span: test_span(1, 19),
        }),
    );

    check_output_expr(
        "Foo -> { a, b: 1, }",
        Some(Expr {
            kind: ExprKind::Constructor {
                ty: NamespacedIdentifier {
                    namespace: vec![],
                    ident: Identifier {
                        span: test_span(1, 4),
                        name: "Foo".to_string(),
                    },
                },
                fields: vec![
                    FieldConstructor::Auto(Identifier {
                        span: test_span(10, 11),
                        name: "a".to_string(),
                    }),
                    FieldConstructor::Named {
                        name: Identifier {
                            span: test_span(13, 14),
                            name: "b".to_string(),
                        },
                        value: Expr {
                            kind: ExprKind::Literal(Literal::Int(1)),
                            span: test_span(16, 17),
                        },
                    },
                ],
            },
            span: test_span(1, 20),
        }),
    )
}
