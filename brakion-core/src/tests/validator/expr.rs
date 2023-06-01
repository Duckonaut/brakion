use crate::{
    errors::validator::ValidatorError,
    repr::{
        BinaryOp, BrakionTreeVisitor, Decl, Expr, ExprKind, IntSize, Literal, TypeReferenceKind,
        UnaryOp,
    },
    tests::test_span,
    validator::Validator,
    ErrorModule,
};

fn check_type(decls: &mut Vec<Decl>, expr: &mut Expr, expected_type: &TypeReferenceKind) {
    let error_module = ErrorModule::new();
    let mut validator = Validator::new(error_module, decls);
    let ty = validator.visit_expr(expr);

    if let Err((err, s)) = ty {
        println!("{}", err);
        if let Some(s) = s {
            println!("at {}", s);
        }
        panic!("Type error");
    }

    assert_eq!(ty, Ok(expected_type.clone()));
}

fn expect_error(decls: &mut Vec<Decl>, expr: &mut Expr, expected_error: &ValidatorError) {
    let error_module = ErrorModule::new();
    let mut validator = Validator::new(error_module, decls);
    let ty = validator.visit_expr(expr);

    if let Ok(ty) = ty {
        println!("Expected error, got type {}", ty);
        panic!("Expected error");
    }

    let error = ty.unwrap_err().0;

    assert_eq!(error, *expected_error);
}

#[test]
fn int_sizes() {
    let mut expr = Expr {
        kind: ExprKind::Literal(Literal::Int(0)),
        span: test_span(1, 2),
    };

    let mut decls = vec![];

    check_type(
        &mut decls,
        &mut expr,
        &TypeReferenceKind::Integer(IntSize::I8, false),
    );

    let mut expr = Expr {
        kind: ExprKind::Literal(Literal::Int(256)),
        span: test_span(1, 2),
    };

    check_type(
        &mut decls,
        &mut expr,
        &TypeReferenceKind::Integer(IntSize::I16, false),
    );

    let mut expr = Expr {
        kind: ExprKind::Literal(Literal::Int(65536)),
        span: test_span(1, 2),
    };

    check_type(
        &mut decls,
        &mut expr,
        &TypeReferenceKind::Integer(IntSize::I32, false),
    );

    let mut expr = Expr {
        kind: ExprKind::Literal(Literal::Int(4294967296)),
        span: test_span(1, 2),
    };

    check_type(
        &mut decls,
        &mut expr,
        &TypeReferenceKind::Integer(IntSize::I64, false),
    );
}

#[test]
fn int_signedness() {
    let mut expr = Expr {
        kind: ExprKind::Literal(Literal::Int(0)),
        span: test_span(1, 2),
    };

    let mut decls = vec![];

    check_type(
        &mut decls,
        &mut expr,
        &TypeReferenceKind::Integer(IntSize::I8, false),
    );

    let mut expr = Expr {
        kind: ExprKind::Unary {
            op: UnaryOp::Neg,
            expr: Box::new(expr),
        },
        span: test_span(1, 2),
    };

    check_type(
        &mut decls,
        &mut expr,
        &TypeReferenceKind::Integer(IntSize::I8, true),
    );
}

#[test]
fn add_sub() {
    let mut expr = Expr {
        kind: ExprKind::Binary {
            op: crate::repr::BinaryOp::Add,
            left: Box::new(Expr {
                kind: ExprKind::Literal(Literal::Int(0)),
                span: test_span(1, 2),
            }),
            right: Box::new(Expr {
                kind: ExprKind::Literal(Literal::Int(0)),
                span: test_span(1, 2),
            }),
        },
        span: test_span(1, 2),
    };

    let mut decls = vec![];

    check_type(
        &mut decls,
        &mut expr,
        &TypeReferenceKind::Integer(IntSize::I8, false),
    );

    let mut expr = Expr {
        kind: ExprKind::Binary {
            op: crate::repr::BinaryOp::Sub,
            left: Box::new(Expr {
                kind: ExprKind::Literal(Literal::Int(0)),
                span: test_span(1, 2),
            }),
            right: Box::new(Expr {
                kind: ExprKind::Literal(Literal::Int(0)),
                span: test_span(1, 2),
            }),
        },
        span: test_span(1, 2),
    };

    check_type(
        &mut decls,
        &mut expr,
        &TypeReferenceKind::Integer(IntSize::I8, false),
    );
}

#[test]
fn int_float() {
    let mut expr = Expr {
        kind: ExprKind::Binary {
            op: crate::repr::BinaryOp::Add,
            left: Box::new(Expr {
                kind: ExprKind::Literal(Literal::Float(0.0)),
                span: test_span(1, 2),
            }),
            right: Box::new(Expr {
                kind: ExprKind::Literal(Literal::Int(0)),
                span: test_span(1, 2),
            }),
        },
        span: test_span(1, 2),
    };

    let mut decls = vec![];

    check_type(
        &mut decls,
        &mut expr,
        &TypeReferenceKind::FloatIndeterminate,
    );

    let mut expr = Expr {
        kind: ExprKind::Binary {
            op: crate::repr::BinaryOp::Add,
            left: Box::new(Expr {
                kind: ExprKind::Literal(Literal::Int(0)),
                span: test_span(1, 2),
            }),
            right: Box::new(Expr {
                kind: ExprKind::Literal(Literal::Float(0.0)),
                span: test_span(1, 2),
            }),
        },
        span: test_span(1, 2),
    };

    expect_error(
        &mut decls,
        &mut expr,
        &ValidatorError::BinaryOpTypeMismatch(
            BinaryOp::Add,
            "u8".to_string(),
            "{float}".to_string(),
        ),
    );
}
