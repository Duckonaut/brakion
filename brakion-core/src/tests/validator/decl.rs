use crate::{
    repr::{
        Decl, Expr, ExprKind, Function, FunctionSignature, Identifier, Literal,
        NamespacedIdentifier, Parameter, ParameterSpec, Stmt, StmtKind, TypeBinaryOp, TypeBody,
        TypeReference, TypeReferenceKind, TypeVariant, Visibility,
    },
    unit::Span,
    validator::Validator,
    ErrorModule,
};

fn test_ident(name: &str) -> Identifier {
    Identifier {
        name: name.to_string(),
        span: Span::default(),
    }
}

#[test]
fn collapse_simple() {
    let mut decls = vec![Decl::Type {
        visibility: Visibility::Public,
        name: test_ident("Foo"),
        body: TypeBody {
            variants: vec![
                TypeVariant {
                    name: test_ident("Bar"),
                    fields: vec![],
                },
                TypeVariant {
                    name: test_ident("Baz"),
                    fields: vec![],
                },
            ],
            methods: vec![],
        },
    }];

    let error_module = ErrorModule::new();
    let mut validator = Validator::new(error_module, &mut decls);
    let mut functions = [
        Function {
            signature: FunctionSignature {
                name: test_ident("f"),
                takes_self: false,
                self_precondition: None,
                parameters: vec![Parameter {
                    name: test_ident("x"),
                    ty: TypeReference {
                        span: None,
                        kind: TypeReferenceKind::Named(NamespacedIdentifier {
                            namespace: vec![],
                            ident: test_ident("Foo"),
                        }),
                    },
                    kind: ParameterSpec::Preconditioned(TypeReference {
                        span: None,
                        kind: TypeReferenceKind::Named(NamespacedIdentifier {
                            namespace: vec![test_ident("Foo")],
                            ident: test_ident("Bar"),
                        }),
                    }),
                }],
                return_type: TypeReference {
                    span: None,
                    kind: TypeReferenceKind::Void,
                },
            },
            body: vec![Stmt {
                kind: StmtKind::Return(Expr {
                    kind: ExprKind::Literal(Literal::Void),
                    span: Span::default(),
                }),
                span: Span::default(),
            }],
        },
        Function {
            signature: FunctionSignature {
                name: test_ident("f"),
                takes_self: false,
                self_precondition: None,
                parameters: vec![Parameter {
                    name: test_ident("x"),
                    ty: TypeReference {
                        span: None,
                        kind: TypeReferenceKind::Named(NamespacedIdentifier {
                            namespace: vec![],
                            ident: test_ident("Foo"),
                        }),
                    },
                    kind: ParameterSpec::Preconditioned(TypeReference {
                        span: None,
                        kind: TypeReferenceKind::Named(NamespacedIdentifier {
                            namespace: vec![test_ident("Foo")],
                            ident: test_ident("Baz"),
                        }),
                    }),
                }],
                return_type: TypeReference {
                    span: None,
                    kind: TypeReferenceKind::Void,
                },
            },
            body: vec![Stmt {
                kind: StmtKind::Return(Expr {
                    kind: ExprKind::Literal(Literal::Bool(true)),
                    span: Span::default(),
                }),
                span: Span::default(),
            }],
        },
    ];

    let collapsed = validator
        .collapse_preconditioned_functions(&mut functions)
        .unwrap();

    let expected = Function {
        signature: FunctionSignature {
            name: test_ident("f"),
            takes_self: false,
            self_precondition: None,
            parameters: vec![Parameter {
                name: test_ident("x"),
                ty: TypeReference {
                    span: None,
                    kind: TypeReferenceKind::Named(NamespacedIdentifier {
                        namespace: vec![],
                        ident: test_ident("Foo"),
                    }),
                },
                kind: ParameterSpec::Basic,
            }],
            return_type: TypeReference {
                span: None,
                kind: TypeReferenceKind::Void,
            },
        },
        body: vec![
            Stmt {
                kind: StmtKind::If {
                    condition: Expr {
                        kind: ExprKind::TypeBinary {
                            op: TypeBinaryOp::Is,
                            expr: Box::new(Expr {
                                kind: ExprKind::Variable(NamespacedIdentifier {
                                    namespace: vec![],
                                    ident: test_ident("x"),
                                }),
                                span: Span::default(),
                            }),
                            ty: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::Named(NamespacedIdentifier {
                                    namespace: vec![test_ident("Foo")],
                                    ident: test_ident("Bar"),
                                }),
                            },
                        },
                        span: Span::default(),
                    },
                    then: Box::new(Stmt {
                        kind: StmtKind::Block(vec![Stmt {
                            kind: StmtKind::Return(Expr {
                                kind: ExprKind::Literal(Literal::Void),
                                span: Span::default(),
                            }),
                            span: Span::default(),
                        }]),
                        span: Span::default(),
                    }),
                    otherwise: None,
                },
                span: Span::default(),
            },
            Stmt {
                kind: StmtKind::If {
                    condition: Expr {
                        kind: ExprKind::TypeBinary {
                            op: TypeBinaryOp::Is,
                            expr: Box::new(Expr {
                                kind: ExprKind::Variable(NamespacedIdentifier {
                                    namespace: vec![],
                                    ident: test_ident("x"),
                                }),
                                span: Span::default(),
                            }),
                            ty: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::Named(NamespacedIdentifier {
                                    namespace: vec![test_ident("Foo")],
                                    ident: test_ident("Baz"),
                                }),
                            },
                        },
                        span: Span::default(),
                    },
                    then: Box::new(Stmt {
                        kind: StmtKind::Block(vec![Stmt {
                            kind: StmtKind::Return(Expr {
                                kind: ExprKind::Literal(Literal::Bool(true)),
                                span: Span::default(),
                            }),
                            span: Span::default(),
                        }]),
                        span: Span::default(),
                    }),
                    otherwise: None,
                },
                span: Span::default(),
            },
        ],
    };

    dbg!(&collapsed);
    dbg!(&expected);

    assert_eq!(collapsed, expected);
}
