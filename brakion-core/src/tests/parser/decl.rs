use std::io::Cursor;

use crate::filters::ParserTokenFilter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::unit::{Location, Span, Unit};
use crate::{repr::*, Config, ErrorModule};

fn check_output_tree(source: &str, expected: &[Decl]) {
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
    let output = parser.parse();

    if !errors.lock().unwrap().errors.is_empty() {
        errors.lock().unwrap().dump(&mut units);
        panic!("Errors encountered during parsing");
    }

    assert_eq!(output, expected);
}

#[test]
fn empty() {
    check_output_tree("", &[]);
}

#[test]
fn function_signature_basic() {
    check_output_tree(
        "fn main() { }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Function(Function {
                signature: FunctionSignature {
                    name: Identifier {
                        span: Span::new(0, Location::new(1, 0, 4), Location::new(1, 0, 8)),
                        name: "main".to_string(),
                    },
                    takes_self: false,
                    self_precondition: None,
                    parameters: vec![],
                    return_type: TypeReference {
                        span: None,
                        kind: TypeReferenceKind::Void,
                    },
                },
                body: vec![],
            }),
        }],
    );
}

#[test]
fn function_signature_return_basic() {
    check_output_tree(
        "fn main() -> u8 { }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Function(Function {
                signature: FunctionSignature {
                    name: Identifier {
                        span: Span::new(0, Location::new(1, 0, 4), Location::new(1, 0, 8)),
                        name: "main".to_string(),
                    },
                    takes_self: false,
                    self_precondition: None,
                    parameters: vec![],
                    return_type: TypeReference {
                        span: Some(Span::new(
                            0,
                            Location::new(1, 0, 14),
                            Location::new(1, 0, 16),
                        )),
                        kind: TypeReferenceKind::Named(NamespacedIdentifier {
                            namespace: vec![],
                            ident: Identifier {
                                span: Span::new(
                                    0,
                                    Location::new(1, 0, 14),
                                    Location::new(1, 0, 16),
                                ),
                                name: "u8".to_string(),
                            },
                        }),
                    },
                },
                body: vec![],
            }),
        }],
    );
}

#[test]
fn function_signature_return_namespaced() {
    check_output_tree(
        "fn main() -> std::types::u8 { }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Function(Function {
                signature: FunctionSignature {
                    name: Identifier {
                        span: Span::new(0, Location::new(1, 0, 4), Location::new(1, 0, 8)),
                        name: "main".to_string(),
                    },
                    takes_self: false,
                    self_precondition: None,
                    parameters: vec![],
                    return_type: TypeReference {
                        kind: TypeReferenceKind::Named(NamespacedIdentifier {
                            namespace: vec![
                                Identifier {
                                    span: Span::new(
                                        0,
                                        Location::new(1, 0, 14),
                                        Location::new(1, 0, 17),
                                    ),
                                    name: "std".to_string(),
                                },
                                Identifier {
                                    span: Span::new(
                                        0,
                                        Location::new(1, 0, 19),
                                        Location::new(1, 0, 24),
                                    ),
                                    name: "types".to_string(),
                                },
                            ],
                            ident: Identifier {
                                span: Span::new(
                                    0,
                                    Location::new(1, 0, 26),
                                    Location::new(1, 0, 28),
                                ),
                                name: "u8".to_string(),
                            },
                        }),
                        span: Some(Span::new(
                            0,
                            Location::new(1, 0, 14),
                            Location::new(1, 0, 28),
                        )),
                    },
                },
                body: vec![],
            }),
        }],
    );
}

#[test]
fn function_signature_return_list() {
    check_output_tree(
        "fn main() -> [u8] { }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Function(Function {
                signature: FunctionSignature {
                    name: Identifier {
                        span: Span::new(0, Location::new(1, 0, 4), Location::new(1, 0, 8)),
                        name: "main".to_string(),
                    },
                    takes_self: false,
                    self_precondition: None,
                    parameters: vec![],
                    return_type: TypeReference {
                        kind: TypeReferenceKind::List(Box::new(TypeReference {
                            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                                namespace: vec![],
                                ident: Identifier {
                                    span: Span::new(
                                        0,
                                        Location::new(1, 0, 15),
                                        Location::new(1, 0, 17),
                                    ),
                                    name: "u8".to_string(),
                                },
                            }),
                            span: Some(Span::new(
                                0,
                                Location::new(1, 0, 15),
                                Location::new(1, 0, 17),
                            )),
                        })),
                        span: Some(Span::new(
                            0,
                            Location::new(1, 0, 14),
                            Location::new(1, 0, 18),
                        )),
                    },
                },
                body: vec![],
            }),
        }],
    );
}

#[test]
fn function_signature_with_params() {
    let expected = &[Decl {
        visibility: Visibility::Private,
        kind: DeclKind::Function(Function {
            signature: FunctionSignature {
                name: Identifier {
                    span: Span::new(0, Location::new(1, 0, 4), Location::new(1, 0, 8)),
                    name: "main".to_string(),
                },
                takes_self: false,
                self_precondition: None,
                parameters: vec![
                    Parameter {
                        name: Identifier {
                            span: Span::new(0, Location::new(1, 0, 9), Location::new(1, 0, 10)),
                            name: "a".to_string(),
                        },
                        ty: TypeReference {
                            kind: TypeReferenceKind::Void,
                            span: Some(Span::new(
                                0,
                                Location::new(1, 0, 12),
                                Location::new(1, 0, 16),
                            )),
                        },
                        kind: ParameterSpec::Basic,
                    },
                    Parameter {
                        name: Identifier {
                            span: Span::new(0, Location::new(1, 0, 18), Location::new(1, 0, 19)),
                            name: "b".to_string(),
                        },
                        ty: TypeReference {
                            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                                namespace: vec![],
                                ident: Identifier {
                                    span: Span::new(
                                        0,
                                        Location::new(1, 0, 21),
                                        Location::new(1, 0, 23),
                                    ),
                                    name: "u8".to_string(),
                                },
                            }),
                            span: Some(Span::new(
                                0,
                                Location::new(1, 0, 21),
                                Location::new(1, 0, 23),
                            )),
                        },
                        kind: ParameterSpec::Basic,
                    },
                    Parameter {
                        name: Identifier {
                            span: Span::new(0, Location::new(1, 0, 25), Location::new(1, 0, 26)),
                            name: "c".to_string(),
                        },
                        ty: TypeReference {
                            kind: TypeReferenceKind::List(Box::new(TypeReference {
                                kind: TypeReferenceKind::Named(NamespacedIdentifier {
                                    namespace: vec![],
                                    ident: Identifier {
                                        span: Span::new(
                                            0,
                                            Location::new(1, 0, 29),
                                            Location::new(1, 0, 31),
                                        ),
                                        name: "u8".to_string(),
                                    },
                                }),
                                span: Some(Span::new(
                                    0,
                                    Location::new(1, 0, 29),
                                    Location::new(1, 0, 31),
                                )),
                            })),
                            span: Some(Span::new(
                                0,
                                Location::new(1, 0, 28),
                                Location::new(1, 0, 32),
                            )),
                        },
                        kind: ParameterSpec::Basic,
                    },
                    Parameter {
                        name: Identifier {
                            span: Span::new(0, Location::new(1, 0, 34), Location::new(1, 0, 35)),
                            name: "d".to_string(),
                        },
                        ty: TypeReference {
                            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                                namespace: vec![],
                                ident: Identifier {
                                    span: Span::new(
                                        0,
                                        Location::new(1, 0, 37),
                                        Location::new(1, 0, 40),
                                    ),
                                    name: "Foo".to_string(),
                                },
                            }),
                            span: Some(Span::new(
                                0,
                                Location::new(1, 0, 37),
                                Location::new(1, 0, 40),
                            )),
                        },
                        kind: ParameterSpec::Preconditioned(TypeReference {
                            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                                namespace: vec![Identifier {
                                    span: Span::new(
                                        0,
                                        Location::new(1, 0, 43),
                                        Location::new(1, 0, 46),
                                    ),
                                    name: "Foo".to_string(),
                                }],
                                ident: Identifier {
                                    span: Span::new(
                                        0,
                                        Location::new(1, 0, 48),
                                        Location::new(1, 0, 51),
                                    ),
                                    name: "Bar".to_string(),
                                },
                            }),
                            span: Some(Span::new(
                                0,
                                Location::new(1, 0, 43),
                                Location::new(1, 0, 51),
                            )),
                        }),
                    },
                ],
                return_type: TypeReference {
                    kind: TypeReferenceKind::List(Box::new(TypeReference {
                        kind: TypeReferenceKind::Named(NamespacedIdentifier {
                            namespace: vec![],
                            ident: Identifier {
                                span: Span::new(
                                    0,
                                    Location::new(1, 0, 57),
                                    Location::new(1, 0, 59),
                                ),
                                name: "u8".to_string(),
                            },
                        }),
                        span: Some(Span::new(
                            0,
                            Location::new(1, 0, 57),
                            Location::new(1, 0, 59),
                        )),
                    })),
                    span: Some(Span::new(
                        0,
                        Location::new(1, 0, 56),
                        Location::new(1, 0, 60),
                    )),
                },
            },
            body: vec![],
        }),
    }];
    check_output_tree(
        "fn main(a: void, b: u8, c: [u8], d: Foo ? Foo::Bar) -> [u8] { }",
        expected,
    );
}
