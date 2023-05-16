use crate::{
    repr::{
        Decl, Identifier, NamespacedIdentifier, TraitBody, TypeBody, TypeReference,
        TypeReferenceKind,
        Visibility::{self, Public}, look_up_decl, NamespaceReference,
    },
    tests::test_span,
};

fn dummy_type(name: String) -> Decl {
    let span = test_span(1, name.len() + 1);

    Decl::Type {
        visibility: Public,
        name: crate::repr::Identifier { name, span },
        body: TypeBody {
            variants: vec![],
            methods: vec![],
        },
    }
}

fn bare_namespaced_ident(name: &str) -> NamespacedIdentifier {
    NamespacedIdentifier {
        namespace: vec![],
        ident: Identifier {
            name: name.into(),
            span: test_span(1, name.len() + 1),
        },
    }
}

fn assert_compatible(decls: &[Decl], a: &TypeReferenceKind, b: &TypeReferenceKind) {
    let result = a.is_compatible(b, decls);

    match result {
        Ok(x) => assert!(x),
        Err(err) => panic!(
            "Expected {:?} and {:?} to be compatible, got error: {:?}",
            a, b, err
        ),
    }
}

fn assert_incompatible(decls: &[Decl], a: &TypeReferenceKind, b: &TypeReferenceKind) {
    let result = a.is_compatible(b, decls);

    match result {
        Ok(x) => assert!(!x),
        Err(err) => panic!(
            "Expected {:?} and {:?} to be incompatible, got error: {:?}",
            a, b, err
        ),
    }
}

#[test]
fn lookup() {
    let decls = &[
        dummy_type("Foo".into()),
        dummy_type("Bar".into()),
        dummy_type("Baz".into()),
        Decl::Trait {
            visibility: Visibility::Public,
            name: Identifier {
                name: "Xyz".into(),
                span: test_span(1, 4),
            },
            body: TraitBody { methods: vec![] },
        },
        Decl::Impl {
            trait_name: NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Xyz".into(),
                    span: test_span(1, 4),
                },
            },
            type_name: TypeReference {
                kind: TypeReferenceKind::Named(NamespacedIdentifier {
                    namespace: vec![],
                    ident: Identifier {
                        name: "Foo".into(),
                        span: test_span(1, 4),
                    },
                }),
                span: Some(test_span(1, 4)),
            },
            body: vec![],
        },
    ];

    assert_eq!(look_up_decl(decls, &bare_namespaced_ident("Foo")), Some(NamespaceReference::Decl(&decls[0])));

    assert_eq!(look_up_decl(decls, &bare_namespaced_ident("Bar")), Some(NamespaceReference::Decl(&decls[1])));

    assert_eq!(look_up_decl(decls, &bare_namespaced_ident("Baz")), Some(NamespaceReference::Decl(&decls[2])));

    assert_eq!(look_up_decl(decls, &bare_namespaced_ident("Xyz")), Some(NamespaceReference::Decl(&decls[3])));
}

#[test]
fn infer_everything() {
    let decls = &[dummy_type("Foo".into()), dummy_type("Bar".into())];

    let a = TypeReferenceKind::Infer;

    let b = TypeReferenceKind::Infer;

    assert_compatible(decls, &a, &b);

    let b = TypeReferenceKind::Void;

    assert_compatible(decls, &a, &b);

    let b = TypeReferenceKind::Named(NamespacedIdentifier {
        namespace: vec![],
        ident: Identifier {
            name: "Foo".into(),
            span: test_span(1, 4),
        },
    });

    assert_compatible(decls, &a, &b);

    let b = TypeReferenceKind::Union(vec![
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Foo".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Bar".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
    ]);

    assert_compatible(decls, &a, &b);

    let b = TypeReferenceKind::List(Box::new(TypeReference {
        kind: TypeReferenceKind::Named(NamespacedIdentifier {
            namespace: vec![],
            ident: Identifier {
                name: "Foo".into(),
                span: test_span(1, 4),
            },
        }),
        span: Some(test_span(1, 4)),
    }));

    assert_compatible(decls, &a, &b);
}

#[test]
fn void_to_void() {
    let decls = &[dummy_type("Foo".into()), dummy_type("Bar".into())];

    let a = TypeReferenceKind::Void;

    let b = TypeReferenceKind::Void;

    assert_compatible(decls, &a, &b);
}

#[test]
fn named_to_void() {
    let decls = &[dummy_type("Foo".into()), dummy_type("Bar".into())];

    let a = TypeReferenceKind::Void;

    let b = TypeReferenceKind::Named(NamespacedIdentifier {
        namespace: vec![],
        ident: Identifier {
            name: "Foo".into(),
            span: test_span(1, 4),
        },
    });

    assert_incompatible(decls, &a, &b);
}

#[test]
fn void_to_named() {
    let decls = &[dummy_type("Foo".into()), dummy_type("Bar".into())];

    let a = TypeReferenceKind::Named(NamespacedIdentifier {
        namespace: vec![],
        ident: Identifier {
            name: "Foo".into(),
            span: test_span(1, 4),
        },
    });

    let b = TypeReferenceKind::Void;

    assert_incompatible(decls, &a, &b);
}

#[test]
fn named_to_named() {
    let decls = &[dummy_type("Foo".into()), dummy_type("Bar".into())];

    let a = TypeReferenceKind::Named(NamespacedIdentifier {
        namespace: vec![],
        ident: Identifier {
            name: "Foo".into(),
            span: test_span(1, 4),
        },
    });

    let b = TypeReferenceKind::Named(NamespacedIdentifier {
        namespace: vec![],
        ident: Identifier {
            name: "Foo".into(),
            span: test_span(1, 4),
        },
    });

    assert_compatible(decls, &a, &b);

    let b = TypeReferenceKind::Named(NamespacedIdentifier {
        namespace: vec![],
        ident: Identifier {
            name: "Bar".into(),
            span: test_span(1, 4),
        },
    });

    assert_incompatible(decls, &a, &b);
}

#[test]
fn void_to_union() {
    let decls = &[dummy_type("Foo".into()), dummy_type("Bar".into())];

    let a = TypeReferenceKind::Union(vec![
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Foo".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Bar".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
    ]);

    let b = TypeReferenceKind::Void;

    assert_incompatible(decls, &a, &b);

    let a = TypeReferenceKind::Union(vec![
        TypeReference {
            kind: TypeReferenceKind::Void,
            span: Some(test_span(1, 5)),
        },
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Bar".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
    ]);

    assert_compatible(decls, &a, &b);

    let a = TypeReferenceKind::Union(vec![
        TypeReference {
            kind: TypeReferenceKind::Void,
            span: Some(test_span(1, 5)),
        },
        TypeReference {
            kind: TypeReferenceKind::Void,
            span: Some(test_span(1, 4)),
        },
    ]);

    assert_incompatible(decls, &a, &b);
}

#[test]
fn named_to_union() {
    let decls = &[
        dummy_type("Foo".into()),
        dummy_type("Bar".into()),
        dummy_type("Baz".into()),
    ];

    let a = TypeReferenceKind::Union(vec![
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Foo".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Bar".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
    ]);

    let b = TypeReferenceKind::Named(NamespacedIdentifier {
        namespace: vec![],
        ident: Identifier {
            name: "Foo".into(),
            span: test_span(1, 4),
        },
    });

    assert_compatible(decls, &a, &b);

    let b = TypeReferenceKind::Named(NamespacedIdentifier {
        namespace: vec![],
        ident: Identifier {
            name: "Baz".into(),
            span: test_span(1, 4),
        },
    });

    assert_incompatible(decls, &a, &b);
}

#[test]
fn list_to_list() {
    let decls = &[dummy_type("Foo".into()), dummy_type("Bar".into())];

    let a = TypeReferenceKind::List(Box::new(TypeReference {
        kind: TypeReferenceKind::Named(NamespacedIdentifier {
            namespace: vec![],
            ident: Identifier {
                name: "Foo".into(),
                span: test_span(1, 4),
            },
        }),
        span: Some(test_span(1, 4)),
    }));

    let b = TypeReferenceKind::List(Box::new(TypeReference {
        kind: TypeReferenceKind::Named(NamespacedIdentifier {
            namespace: vec![],
            ident: Identifier {
                name: "Foo".into(),
                span: test_span(1, 4),
            },
        }),
        span: Some(test_span(1, 4)),
    }));

    assert_compatible(decls, &a, &b);

    let b = TypeReferenceKind::List(Box::new(TypeReference {
        kind: TypeReferenceKind::Named(NamespacedIdentifier {
            namespace: vec![],
            ident: Identifier {
                name: "Bar".into(),
                span: test_span(1, 4),
            },
        }),
        span: Some(test_span(1, 4)),
    }));

    assert_incompatible(decls, &a, &b);
}

#[test]
fn list_to_union() {
    let decls = &[dummy_type("Foo".into()), dummy_type("Bar".into())];

    let a = TypeReferenceKind::Union(vec![
        TypeReference {
            kind: TypeReferenceKind::List(Box::new(TypeReference {
                kind: TypeReferenceKind::Named(NamespacedIdentifier {
                    namespace: vec![],
                    ident: Identifier {
                        name: "Foo".into(),
                        span: test_span(1, 4),
                    },
                }),
                span: Some(test_span(1, 4)),
            })),
            span: Some(test_span(1, 4)),
        },
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Bar".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
    ]);

    let b = TypeReferenceKind::List(Box::new(TypeReference {
        kind: TypeReferenceKind::Named(NamespacedIdentifier {
            namespace: vec![],
            ident: Identifier {
                name: "Foo".into(),
                span: test_span(1, 4),
            },
        }),
        span: Some(test_span(1, 4)),
    }));

    assert_compatible(decls, &a, &b);

    let b = TypeReferenceKind::List(Box::new(TypeReference {
        kind: TypeReferenceKind::Named(NamespacedIdentifier {
            namespace: vec![],
            ident: Identifier {
                name: "Bar".into(),
                span: test_span(1, 4),
            },
        }),
        span: Some(test_span(1, 4)),
    }));

    assert_incompatible(decls, &a, &b);
}

#[test]
fn union_to_union() {
    let decls = &[
        dummy_type("Foo".into()),
        dummy_type("Bar".into()),
        dummy_type("Baz".into()),
        Decl::Trait {
            visibility: Visibility::Public,
            name: Identifier {
                name: "Xyz".into(),
                span: test_span(1, 4),
            },
            body: TraitBody { methods: vec![] },
        },
        Decl::Impl {
            trait_name: NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Xyz".into(),
                    span: test_span(1, 4),
                },
            },
            type_name: TypeReference {
                kind: TypeReferenceKind::Named(NamespacedIdentifier {
                    namespace: vec![],
                    ident: Identifier {
                        name: "Foo".into(),
                        span: test_span(1, 4),
                    },
                }),
                span: Some(test_span(1, 4)),
            },
            body: vec![],
        },
    ];

    let a = TypeReferenceKind::Union(vec![
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Foo".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Bar".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
    ]);

    let b = TypeReferenceKind::Union(vec![
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Foo".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Bar".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
    ]);

    assert_compatible(decls, &a, &b);

    let b = TypeReferenceKind::Union(vec![
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Foo".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Baz".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
    ]);

    assert_incompatible(decls, &a, &b);

    let a = TypeReferenceKind::Union(vec![
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Foo".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Bar".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Xyz".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
    ]);

    let b = TypeReferenceKind::Union(vec![
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Foo".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
        TypeReference {
            kind: TypeReferenceKind::Named(NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "Bar".into(),
                    span: test_span(1, 4),
                },
            }),
            span: Some(test_span(1, 4)),
        },
    ]);

    assert_incompatible(decls, &a, &b);
}
