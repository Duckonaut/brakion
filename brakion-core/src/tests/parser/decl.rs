use std::io::Cursor;

use crate::errors::parser::ParserError;
use crate::filters::ParserTokenFilter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::unit::{Location, Span, Unit};
use crate::{errors, repr::*, Config, ErrorModule};

use super::test_span;

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

fn check_output_errors(source: &str, expected: &[(ParserError, Option<Span>)]) {
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
    let _ = parser.parse();

    let errors = errors.lock().unwrap().errors.clone();

    if errors.is_empty() {
        panic!("No errors encountered during parsing");
    }

    if errors.len() != expected.len() {
        for error in errors.iter() {
            println!("{:?}", error);
        }
        panic!("Expected {} errors, got {}", expected.len(), errors.len());
    }

    for (i, error) in errors.iter().enumerate() {
        match error.kind {
            errors::ErrorKind::ParserError(ref error) => assert_eq!(error, &expected[i].0),
            _ => panic!("Unexpected error kind encountered"),
        }
        assert_eq!(error.span, expected[i].1);
    }
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

#[test]
fn type_basic() {
    check_output_tree(
        "type Foo;",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Type {
                name: Identifier {
                    name: "Foo".to_string(),
                    span: test_span(6, 9),
                },
                body: TypeBody {
                    variants: vec![TypeVariant {
                        name: Identifier {
                            name: "self".to_string(),
                            span: test_span(6, 9),
                        },
                        fields: vec![],
                    }],
                    methods: vec![],
                },
            },
        }],
    );
}

#[test]
fn type_empty() {
    check_output_tree(
        "type Foo { }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Type {
                name: Identifier {
                    name: "Foo".to_string(),
                    span: test_span(6, 9),
                },
                body: TypeBody {
                    variants: vec![TypeVariant {
                        name: Identifier {
                            name: "self".to_string(),
                            span: test_span(6, 9),
                        },
                        fields: vec![],
                    }],
                    methods: vec![],
                },
            },
        }],
    );
}

#[test]
fn type_mathods_only() {
    check_output_tree(
        "type Foo { pub fn bar() { } }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Type {
                name: Identifier {
                    name: "Foo".to_string(),
                    span: test_span(6, 9),
                },
                body: TypeBody {
                    variants: vec![TypeVariant {
                        name: Identifier {
                            name: "self".to_string(),
                            span: test_span(6, 9),
                        },
                        fields: vec![],
                    }],
                    methods: vec![(
                        Visibility::Public,
                        Function {
                            signature: FunctionSignature {
                                name: Identifier {
                                    name: "bar".to_string(),
                                    span: test_span(19, 22),
                                },
                                takes_self: false,
                                self_precondition: None,
                                parameters: vec![],
                                return_type: TypeReference {
                                    kind: TypeReferenceKind::Void,
                                    span: None,
                                },
                            },
                            body: vec![],
                        },
                    )],
                },
            },
        }],
    );
}

#[test]
fn type_self_variant_explicit() {
    check_output_tree(
        "type Foo { self; }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Type {
                name: Identifier {
                    name: "Foo".to_string(),
                    span: test_span(6, 9),
                },
                body: TypeBody {
                    variants: vec![TypeVariant {
                        name: Identifier {
                            name: "self".to_string(),
                            span: test_span(12, 16),
                        },
                        fields: vec![],
                    }],
                    methods: vec![],
                },
            },
        }],
    );
}

#[test]
fn type_self_variant_field() {
    check_output_tree(
        "type Foo { self { a: u8 } }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Type {
                name: Identifier {
                    name: "Foo".to_string(),
                    span: test_span(6, 9),
                },
                body: TypeBody {
                    variants: vec![TypeVariant {
                        name: Identifier {
                            name: "self".to_string(),
                            span: test_span(12, 16),
                        },
                        fields: vec![Field {
                            name: Identifier {
                                name: "a".to_string(),
                                span: test_span(19, 20),
                            },
                            ty: TypeReference {
                                kind: TypeReferenceKind::Named(NamespacedIdentifier {
                                    namespace: vec![],
                                    ident: Identifier {
                                        name: "u8".to_string(),
                                        span: test_span(22, 24),
                                    },
                                }),
                                span: Some(test_span(22, 24)),
                            },
                        }],
                    }],
                    methods: vec![],
                },
            },
        }],
    );
    check_output_tree(
        "type Foo { self { a: u8, } }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Type {
                name: Identifier {
                    name: "Foo".to_string(),
                    span: test_span(6, 9),
                },
                body: TypeBody {
                    variants: vec![TypeVariant {
                        name: Identifier {
                            name: "self".to_string(),
                            span: test_span(12, 16),
                        },
                        fields: vec![Field {
                            name: Identifier {
                                name: "a".to_string(),
                                span: test_span(19, 20),
                            },
                            ty: TypeReference {
                                kind: TypeReferenceKind::Named(NamespacedIdentifier {
                                    namespace: vec![],
                                    ident: Identifier {
                                        name: "u8".to_string(),
                                        span: test_span(22, 24),
                                    },
                                }),
                                span: Some(test_span(22, 24)),
                            },
                        }],
                    }],
                    methods: vec![],
                },
            },
        }],
    );
}

#[test]
fn type_single_variant() {
    check_output_tree(
        "type Foo { Bar; }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Type {
                name: Identifier {
                    name: "Foo".to_string(),
                    span: test_span(6, 9),
                },
                body: TypeBody {
                    variants: vec![TypeVariant {
                        name: Identifier {
                            name: "Bar".to_string(),
                            span: test_span(12, 15),
                        },
                        fields: vec![],
                    }],
                    methods: vec![],
                },
            },
        }],
    );
}

#[test]
fn type_single_variant_fields() {
    check_output_tree(
        "type Foo { Bar { a: u8, b: u8 } }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Type {
                name: Identifier {
                    name: "Foo".to_string(),
                    span: test_span(6, 9),
                },
                body: TypeBody {
                    variants: vec![TypeVariant {
                        name: Identifier {
                            name: "Bar".to_string(),
                            span: test_span(12, 15),
                        },
                        fields: vec![
                            Field {
                                name: Identifier {
                                    name: "a".to_string(),
                                    span: test_span(18, 19),
                                },
                                ty: TypeReference {
                                    kind: TypeReferenceKind::Named(NamespacedIdentifier {
                                        namespace: vec![],
                                        ident: Identifier {
                                            name: "u8".to_string(),
                                            span: test_span(21, 23),
                                        },
                                    }),
                                    span: Some(test_span(21, 23)),
                                },
                            },
                            Field {
                                name: Identifier {
                                    name: "b".to_string(),
                                    span: test_span(25, 26),
                                },
                                ty: TypeReference {
                                    kind: TypeReferenceKind::Named(NamespacedIdentifier {
                                        namespace: vec![],
                                        ident: Identifier {
                                            name: "u8".to_string(),
                                            span: test_span(28, 30),
                                        },
                                    }),
                                    span: Some(test_span(28, 30)),
                                },
                            },
                        ],
                    }],
                    methods: vec![],
                },
            },
        }],
    );
}

#[test]
fn type_multiple_variants() {
    check_output_tree(
        "type Foo { Bar; Baz; }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Type {
                name: Identifier {
                    name: "Foo".to_string(),
                    span: test_span(6, 9),
                },
                body: TypeBody {
                    variants: vec![
                        TypeVariant {
                            name: Identifier {
                                name: "Bar".to_string(),
                                span: test_span(12, 15),
                            },
                            fields: vec![],
                        },
                        TypeVariant {
                            name: Identifier {
                                name: "Baz".to_string(),
                                span: test_span(17, 20),
                            },
                            fields: vec![],
                        },
                    ],
                    methods: vec![],
                },
            },
        }],
    );
}

#[test]
fn type_variants_method() {
    check_output_tree(
        "type Foo { Bar; Baz; fn foo() { } }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Type {
                name: Identifier {
                    name: "Foo".to_string(),
                    span: test_span(6, 9),
                },
                body: TypeBody {
                    variants: vec![
                        TypeVariant {
                            name: Identifier {
                                name: "Bar".to_string(),
                                span: test_span(12, 15),
                            },
                            fields: vec![],
                        },
                        TypeVariant {
                            name: Identifier {
                                name: "Baz".to_string(),
                                span: test_span(17, 20),
                            },
                            fields: vec![],
                        },
                    ],
                    methods: vec![(
                        Visibility::Private,
                        Function {
                            signature: FunctionSignature {
                                name: Identifier {
                                    name: "foo".to_string(),
                                    span: test_span(25, 28),
                                },
                                takes_self: false,
                                self_precondition: None,
                                parameters: vec![],
                                return_type: TypeReference {
                                    kind: TypeReferenceKind::Void,
                                    span: None,
                                },
                            },
                            body: vec![],
                        },
                    )],
                },
            },
        }],
    );
}

#[test]
fn type_variants_methods_interweave() {
    check_output_errors(
        "type Foo { Bar; fn foo() { } Baz; }",
        &[(
            ParserError::VariantMethodInterweave,
            Some(test_span(30, 33)),
        )],
    );
}

#[test]
fn type_variants_methods_interweave_2() {
    check_output_errors(
        "type Foo { fn foo() { } Bar; Baz; }",
        &[(
            ParserError::VariantMethodInterweave,
            Some(test_span(25, 28)),
        )],
    );
}

#[test]
fn mod_empty() {
    check_output_tree(
        "mod foo { }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Module {
                name: Identifier {
                    name: "foo".to_string(),
                    span: test_span(5, 8),
                },
                body: vec![],
            },
        }],
    );
}

#[test]
fn mod_empty_pub() {
    check_output_tree(
        "pub mod foo { }",
        &[Decl {
            visibility: Visibility::Public,
            kind: DeclKind::Module {
                name: Identifier {
                    name: "foo".to_string(),
                    span: test_span(9, 12),
                },
                body: vec![],
            },
        }],
    );
}

#[test]
fn mod_with_decl() {
    check_output_tree(
        "mod foo { fn bar() { } }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Module {
                name: Identifier {
                    name: "foo".to_string(),
                    span: test_span(5, 8),
                },
                body: vec![Decl {
                    visibility: Visibility::Private,
                    kind: DeclKind::Function(Function {
                        signature: FunctionSignature {
                            name: Identifier {
                                name: "bar".to_string(),
                                span: test_span(14, 17),
                            },
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![],
                            return_type: TypeReference {
                                kind: TypeReferenceKind::Void,
                                span: None,
                            },
                        },
                        body: vec![],
                    }),
                }],
            },
        }],
    );
}

#[test]
fn mod_with_decl_pub() {
    check_output_tree(
        "mod foo { pub fn bar() { } }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Module {
                name: Identifier {
                    name: "foo".to_string(),
                    span: test_span(5, 8),
                },
                body: vec![Decl {
                    visibility: Visibility::Public,
                    kind: DeclKind::Function(Function {
                        signature: FunctionSignature {
                            name: Identifier {
                                name: "bar".to_string(),
                                span: test_span(18, 21),
                            },
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![],
                            return_type: TypeReference {
                                kind: TypeReferenceKind::Void,
                                span: None,
                            },
                        },
                        body: vec![],
                    }),
                }],
            },
        }],
    );
}

#[test]
fn trait_empty() {
    check_output_tree(
        "trait Foo { }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Trait {
                name: Identifier {
                    name: "Foo".to_string(),
                    span: test_span(7, 10),
                },
                body: TraitBody { methods: vec![] },
            },
        }],
    );
}

#[test]
fn trait_methods() {
    check_output_tree(
        "trait Foo { fn bar(); fn baz(); }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Trait {
                name: Identifier {
                    name: "Foo".to_string(),
                    span: test_span(7, 10),
                },
                body: TraitBody {
                    methods: vec![
                        FunctionSignature {
                            name: Identifier {
                                name: "bar".to_string(),
                                span: test_span(16, 19),
                            },
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![],
                            return_type: TypeReference {
                                kind: TypeReferenceKind::Void,
                                span: None,
                            },
                        },
                        FunctionSignature {
                            name: Identifier {
                                name: "baz".to_string(),
                                span: test_span(26, 29),
                            },
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![],
                            return_type: TypeReference {
                                kind: TypeReferenceKind::Void,
                                span: None,
                            },
                        },
                    ],
                },
            },
        }],
    );
}

#[test]
fn impl_empty() {
    check_output_tree(
        "impl Foo for Bar { }",
        &[Decl {
            visibility: Visibility::Private,
            kind: DeclKind::Impl {
                type_name: TypeReference {
                    kind: TypeReferenceKind::Named(NamespacedIdentifier {
                        namespace: vec![],
                        ident: Identifier {
                            name: "Bar".to_string(),
                            span: test_span(14, 17),
                        },
                    }),
                    span: Some(test_span(14, 17)),
                },
                trait_name: NamespacedIdentifier {
                    namespace: vec![],
                    ident: Identifier {
                        name: "Foo".to_string(),
                        span: test_span(6, 9),
                    },
                },
                body: vec![],
            },
        }],
    );
}

#[test]
fn impl_empty_pub() {
    check_output_errors(
        "pub impl Foo for Bar { }",
        &[(ParserError::PubInTraitImpl, Some(test_span(1, 4)))],
    );
}
