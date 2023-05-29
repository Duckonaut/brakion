use crate::{
    repr::{
        BrakionTreeVisitor, Decl, Expr, ExprKind, IntSize, Literal, TypeReferenceKind, UnaryOp,
    },
    tests::test_span,
    validator::Validator,
    ErrorModule,
};

fn check_type(decls: &mut [Decl], expr: &mut Expr, expected_type: &TypeReferenceKind) {
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
        &TypeReferenceKind::IntegerAtLeast(IntSize::I8, false),
    );

    let mut expr = Expr {
        kind: ExprKind::Literal(Literal::Int(256)),
        span: test_span(1, 2),
    };

    check_type(
        &mut decls,
        &mut expr,
        &TypeReferenceKind::IntegerAtLeast(IntSize::I16, false),
    );

    let mut expr = Expr {
        kind: ExprKind::Literal(Literal::Int(65536)),
        span: test_span(1, 2),
    };

    check_type(
        &mut decls,
        &mut expr,
        &TypeReferenceKind::IntegerAtLeast(IntSize::I32, false),
    );

    let mut expr = Expr {
        kind: ExprKind::Literal(Literal::Int(4294967296)),
        span: test_span(1, 2),
    };

    check_type(
        &mut decls,
        &mut expr,
        &TypeReferenceKind::IntegerAtLeast(IntSize::I64, false),
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
        &TypeReferenceKind::IntegerAtLeast(IntSize::I8, false),
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
        &TypeReferenceKind::IntegerAtLeast(IntSize::I8, true),
    );
}
