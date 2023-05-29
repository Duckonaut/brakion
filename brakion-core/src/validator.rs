use std::{cell::UnsafeCell, collections::HashMap};

use crate::{
    errors::{validator::ValidatorError, ErrorModule},
    repr::{
        look_up_decl, BinaryOp, BrakionTreeVisitor, Decl, Expr, ExprKind, FieldConstructor,
        Function, FunctionSignature, IntSize, Literal, NamespaceReference, NamespacedIdentifier,
        Stmt, TraitBody, TypeBinaryOp, TypeBody, TypeReference, TypeReferenceKind, TypeVariant,
        UnaryOp,
    },
    unit::Span,
};

pub struct Validator<'a> {
    error_module: ErrorModule,
    // so we can modify it in place.
    execution_scopes: Vec<ExecutionScope>,
    module_stack: Vec<(String, UnsafeCell<&'a mut [Decl]>)>,
}

struct ExecutionScope {
    variables: Vec<(NamespacedIdentifier, TypeReference)>,
}

impl<'a> Validator<'a> {
    pub fn new(error_module: ErrorModule, decls: &'a mut [Decl]) -> Self {
        Self {
            error_module,
            execution_scopes: Vec::new(),
            module_stack: vec![("root".to_string(), UnsafeCell::new(decls))],
        }
    }

    fn error(&mut self, err: ValidatorError, span: Option<Span>) {
        self.error_module.add_validator_error(err, span);
    }

    fn root(&self) -> &'a mut [Decl] {
        unsafe { *self.module_stack[0].1.get() }
    }

    fn look_up_variable(&self, name: &NamespacedIdentifier) -> Option<TypeReference> {
        for scope in self.execution_scopes.iter().rev() {
            for (var_name, var_type) in scope.variables.iter().rev() {
                if var_name.same(name) {
                    return Some(var_type.clone());
                }
            }
        }

        None
    }

    fn look_up_type_body(&self, name: &NamespacedIdentifier) -> Option<&'a TypeBody> {
        let decl = look_up_decl(self.root(), name)?;

        match decl {
            NamespaceReference::Decl(Decl::Type { body, .. }) => Some(body),
            _ => None,
        }
    }

    fn look_up_trait_body(&self, name: &NamespacedIdentifier) -> Option<&'a TraitBody> {
        let decl = look_up_decl(self.root(), name)?;

        match decl {
            NamespaceReference::Decl(Decl::Trait { body, .. }) => Some(body),
            _ => None,
        }
    }

    fn look_up_type_variant(&self, name: &NamespacedIdentifier) -> Option<&'a TypeVariant> {
        let decl = look_up_decl(self.root(), name)?;

        match decl {
            NamespaceReference::Decl(Decl::Type { body, .. }) => {
                if body.variants.len() == 1 && body.variants.first().unwrap().name.name == "self" {
                    Some(&body.variants[0])
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn look_up_function(&self, name: &NamespacedIdentifier) -> Option<&'a Function> {
        let decl = look_up_decl(self.root(), name)?;

        match decl {
            NamespaceReference::Decl(Decl::Function { function, .. }) => Some(function),
            _ => None,
        }
    }

    fn begin_scope(&mut self) {
        self.execution_scopes.push(ExecutionScope {
            variables: Vec::new(),
        });
    }

    fn end_scope(&mut self) {
        self.execution_scopes.pop();
    }

    fn validate_function_call(
        &mut self,
        name: &NamespacedIdentifier,
        signature: &FunctionSignature,
        self_type_ref: &Option<TypeReferenceKind>,
        args: &mut [Expr],
    ) -> Result<(), (ValidatorError, Option<Span>)> {
        if signature.takes_self {
            let self_type_ref = self_type_ref.as_ref().unwrap();

            let expected_arg_count = signature.parameters.len() + 1;

            if args.len() != expected_arg_count {
                return Err((
                    ValidatorError::WrongArgCount(name.clone(), expected_arg_count, args.len()),
                    Some(Span::from_spans(
                        args.first().unwrap().span,
                        args.last().unwrap().span,
                    )),
                ));
            }

            let self_arg = args.first_mut().unwrap();

            let self_arg_type = Self::visit_expr(self, self_arg)?;

            let compatible = self_type_ref.is_compatible(&self_arg_type, self.root());

            if let Err(err) = compatible {
                return Err((err, Some(self_arg.span)));
            }

            if !compatible.unwrap() {
                return Err((
                    ValidatorError::ArgTypeMismatch(
                        name.clone(),
                        0,
                        self_type_ref.to_string(),
                        self_arg_type.to_string(),
                    ),
                    Some(self_arg.span),
                ));
            }

            for (i, (arg, param)) in args
                .iter_mut()
                .skip(1)
                .zip(signature.parameters.iter())
                .enumerate()
            {
                let arg_type = Self::visit_expr(self, arg)?;

                let compatible = param.ty.kind.is_compatible(&arg_type, self.root());

                if let Err(err) = compatible {
                    return Err((err, Some(arg.span)));
                }

                if !compatible.unwrap() {
                    return Err((
                        ValidatorError::ArgTypeMismatch(
                            name.clone(),
                            i + 1,
                            param.ty.kind.to_string(),
                            arg_type.to_string(),
                        ),
                        Some(arg.span),
                    ));
                }
            }

            Ok(())
        } else {
            let expected_arg_count = signature.parameters.len();

            if args.len() != expected_arg_count {
                return Err((
                    ValidatorError::WrongArgCount(name.clone(), expected_arg_count, args.len()),
                    Some(Span::from_spans(
                        args.first().unwrap().span,
                        args.last().unwrap().span,
                    )),
                ));
            }

            for (i, (arg, param)) in args.iter_mut().zip(signature.parameters.iter()).enumerate() {
                let arg_type = Self::visit_expr(self, arg)?;

                let compatible = param.ty.kind.is_compatible(&arg_type, self.root());

                if let Err(err) = compatible {
                    return Err((err, Some(arg.span)));
                }

                if !compatible.unwrap() {
                    return Err((
                        ValidatorError::ArgTypeMismatch(
                            name.clone(),
                            i,
                            param.ty.kind.to_string(),
                            arg_type.to_string(),
                        ),
                        Some(arg.span),
                    ));
                }
            }

            Ok(())
        }
    }

    pub fn check(&mut self) {
        let root = self.root();

        for decl in root.iter_mut() {
            let result = Self::visit_decl(self, decl);

            if let Err(err) = result {
                self.error(err.0, err.1);
            }
        }
    }
}

impl<'a> BrakionTreeVisitor for Validator<'a> {
    type ExprResult = Result<TypeReferenceKind, (ValidatorError, Option<Span>)>;
    type StmtResult = Result<(), (ValidatorError, Option<Span>)>;
    type DeclResult = Result<(), (ValidatorError, Option<Span>)>;
    type TypeReferenceResult = Result<TypeReferenceKind, (ValidatorError, Option<Span>)>;

    fn visit_decl(&mut self, decl: &mut Decl) -> Self::DeclResult {
        todo!()
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Self::ExprResult {
        let span = expr.span;
        match &mut expr.kind {
            ExprKind::Literal(lit) => match lit {
                Literal::Int(i) => match i {
                    0..=255 => Ok(TypeReferenceKind::Integer(IntSize::I8, false)),
                    0..=65535 => Ok(TypeReferenceKind::Integer(IntSize::I16, false)),
                    0..=4294967295 => Ok(TypeReferenceKind::Integer(IntSize::I32, false)),
                    _ => Ok(TypeReferenceKind::Integer(IntSize::I64, false)),
                },
                Literal::Float(_) => Ok(TypeReferenceKind::FloatIndeterminate),
                Literal::Bool(_) => Ok(TypeReferenceKind::Bool),
                Literal::String(_) => Ok(TypeReferenceKind::String),
                Literal::Char(_) => Ok(TypeReferenceKind::Char),
                Literal::List(l) => {
                    let mut ty = TypeReferenceKind::Infer;

                    for expr in l.iter_mut() {
                        let result = Self::visit_expr(self, expr)?;

                        if let TypeReferenceKind::Infer = ty {
                            ty = result;
                        } else if ty != result {
                            return Err((
                                ValidatorError::ListTypeMismatch(
                                    ty.to_string(),
                                    result.to_string(),
                                ),
                                Some(expr.span),
                            ));
                        }
                    }

                    Ok(TypeReferenceKind::List(Box::new(TypeReference {
                        kind: ty,
                        span: None,
                    })))
                }
                Literal::Void => Ok(TypeReferenceKind::Void),
            },
            ExprKind::Unary { op, expr } => match &op {
                UnaryOp::Neg => {
                    let result = Self::visit_expr(self, expr)?;

                    match result {
                        TypeReferenceKind::FloatIndeterminate => {
                            Ok(TypeReferenceKind::FloatIndeterminate)
                        }
                        TypeReferenceKind::Integer(size, _) => {
                            Ok(TypeReferenceKind::Integer(size, true))
                        }
                        TypeReferenceKind::Float(s) => Ok(TypeReferenceKind::Float(s)),
                        _ => Err((
                            ValidatorError::BadUnaryOp(op.clone(), result.to_string()),
                            Some(span),
                        )),
                    }
                }
                UnaryOp::Not => {
                    let result = Self::visit_expr(self, expr)?;

                    match result {
                        TypeReferenceKind::Bool => Ok(TypeReferenceKind::Bool),
                        _ => Err((
                            ValidatorError::BadUnaryOp(op.clone(), result.to_string()),
                            Some(span),
                        )),
                    }
                }
            },
            ExprKind::Binary { op, left, right } => {
                let left = Self::visit_expr(self, left)?;

                let right = Self::visit_expr(self, right)?;

                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        let compatible = left.is_compatible(&right, self.root());

                        if let Err(err) = compatible {
                            return Err((err, Some(span)));
                        }

                        if !compatible.unwrap() {
                            return Err((
                                ValidatorError::BinaryOpTypeMismatch(
                                    op.clone(),
                                    left.to_string(),
                                    right.to_string(),
                                ),
                                Some(span),
                            ));
                        }

                        Ok(left)
                    }
                    BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Leq | BinaryOp::Geq => {
                        if !left.is_numeric() || !right.is_numeric() {
                            return Err((
                                ValidatorError::BinaryOpTypeMismatch(
                                    op.clone(),
                                    left.to_string(),
                                    right.to_string(),
                                ),
                                Some(span),
                            ));
                        }

                        Ok(TypeReferenceKind::Bool)
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        if left != TypeReferenceKind::Bool || right != TypeReferenceKind::Bool {
                            return Err((
                                ValidatorError::BinaryOpTypeMismatch(
                                    op.clone(),
                                    left.to_string(),
                                    right.to_string(),
                                ),
                                Some(span),
                            ));
                        }

                        Ok(TypeReferenceKind::Bool)
                    }
                    BinaryOp::Eq | BinaryOp::Neq => {
                        if left != right {
                            return Err((
                                ValidatorError::BinaryOpTypeMismatch(
                                    op.clone(),
                                    left.to_string(),
                                    right.to_string(),
                                ),
                                Some(span),
                            ));
                        }

                        Ok(TypeReferenceKind::Bool)
                    }
                }
            }
            ExprKind::TypeBinary { op, ty, expr } => {
                let _ = Self::visit_expr(self, expr)?; // unused, but we need to visit it to check for errors

                let ty = Self::visit_type_reference(self, ty)?;

                match op {
                    TypeBinaryOp::Is => Ok(TypeReferenceKind::Bool),
                    TypeBinaryOp::As => {
                        // The cast is assumed to be valid, will throw at runtime if not
                        Ok(ty)
                    }
                }
            }
            ExprKind::Variable(v) => {
                let var = self.look_up_variable(v);

                if var.is_none() {
                    return Err((ValidatorError::UnknownVariable(v.clone()), Some(expr.span)));
                }

                Ok(var.unwrap().kind)
            }
            ExprKind::Access { expr, field } => {
                let expr_type = Self::visit_expr(self, expr)?;

                match expr_type {
                    TypeReferenceKind::Named(name) => {
                        let type_body = self.look_up_type_body(&name);

                        if type_body.is_none() {
                            return Err((ValidatorError::UnknownType(name), Some(expr.span)));
                        }

                        let mut field_type = None;

                        for variant in type_body.unwrap().variants.iter() {
                            for variant_field in variant.fields.iter() {
                                if variant_field.name == *field {
                                    if field_type.is_some() {
                                        field_type = Some(TypeReference {
                                            kind: TypeReferenceKind::Union(vec![
                                                field_type.unwrap(),
                                                variant_field.ty.clone(),
                                            ]),
                                            span: None,
                                        });
                                    } else {
                                        field_type = Some(variant_field.ty.clone());
                                    }
                                }
                            }
                        }

                        if field_type.is_none() {
                            return Err((
                                ValidatorError::UnknownField(name, field.name.clone()),
                                Some(expr.span),
                            ));
                        }

                        Ok(field_type.unwrap().kind)
                    }
                    _ => Err((
                        ValidatorError::AccessOnNonType(expr_type.to_string()),
                        Some(span),
                    )),
                }
            }
            ExprKind::FunctionCall { name, args } => {
                let function = self.look_up_function(name);

                if let Some(function) = function {
                    self.validate_function_call(name, &function.signature, &None, args)?;

                    return Ok(function.signature.return_type.clone().kind);
                }

                let type_body = self.look_up_type_body(&name.up());

                if let Some(type_body) = type_body {
                    let method = type_body
                        .methods
                        .iter()
                        .find(|(_, method)| method.signature.name.same(&name.ident));

                    if method.is_none() {
                        return Err((
                            ValidatorError::UnknownFunction(name.clone()),
                            Some(expr.span),
                        ));
                    }

                    let (visibility, method) = method.unwrap();

                    // TODO: check visibility

                    let type_ref = TypeReference {
                        kind: TypeReferenceKind::Named(name.up()),
                        span: None,
                    };

                    self.validate_function_call(
                        name,
                        &method.signature,
                        &Some(type_ref.kind),
                        args,
                    )?;

                    return Ok(method.signature.return_type.kind.clone());
                }

                let trait_body = self.look_up_trait_body(&name.up());

                if let Some(trait_body) = trait_body {
                    let method = trait_body
                        .methods
                        .iter()
                        .find(|signature| signature.name.same(&name.ident));

                    if method.is_none() {
                        return Err((
                            ValidatorError::UnknownFunction(name.clone()),
                            Some(expr.span),
                        ));
                    }

                    let method = method.unwrap();

                    let type_ref = TypeReference {
                        kind: TypeReferenceKind::Named(name.up()),
                        span: None,
                    };

                    self.validate_function_call(name, method, &Some(type_ref.kind), args)?;

                    return Ok(method.return_type.kind.clone());
                }

                Err((
                    ValidatorError::UnknownFunction(name.clone()),
                    Some(expr.span),
                ))
            }
            ExprKind::MethodCall {
                expr,
                method: method_name,
                args,
            } => {
                let expr_type = Self::visit_expr(self, expr)?;

                match expr_type {
                    TypeReferenceKind::Named(name) => {
                        let type_body = self.look_up_type_body(&name);

                        if type_body.is_none() {
                            return Err((ValidatorError::UnknownType(name), Some(expr.span)));
                        }

                        let method = type_body
                            .unwrap()
                            .methods
                            .iter()
                            .find(|(_, method)| method.signature.name.same(method_name));

                        if method.is_none() {
                            return Err((
                                ValidatorError::UnknownFunction(
                                    method_name.clone().into_namespaced(),
                                ),
                                Some(expr.span),
                            ));
                        }

                        let (visibility, method) = method.unwrap();

                        // TODO: check visibility

                        let expected_arg_count = method.signature.parameters.len();

                        if args.len() != expected_arg_count {
                            return Err((
                                ValidatorError::WrongArgCount(
                                    name.clone(),
                                    expected_arg_count,
                                    args.len(),
                                ),
                                Some(Span::from_spans(
                                    args.first().unwrap().span,
                                    args.last().unwrap().span,
                                )),
                            ));
                        }

                        for (i, (arg, param)) in args
                            .iter_mut()
                            .skip(1)
                            .zip(method.signature.parameters.iter())
                            .enumerate()
                        {
                            let arg_type = Self::visit_expr(self, arg)?;

                            let compatible = param.ty.kind.is_compatible(&arg_type, self.root());

                            if let Err(err) = compatible {
                                return Err((err, Some(arg.span)));
                            }

                            if !compatible.unwrap() {
                                return Err((
                                    ValidatorError::ArgTypeMismatch(
                                        name.clone(),
                                        i + 1,
                                        param.ty.kind.to_string(),
                                        arg_type.to_string(),
                                    ),
                                    Some(arg.span),
                                ));
                            }
                        }

                        Ok(method.signature.return_type.kind.clone())
                    }
                    _ => Err((
                        ValidatorError::AccessOnNonType(expr_type.to_string()),
                        Some(span),
                    )),
                }
            }
            ExprKind::Index { expr, index } => {
                let expr_type = Self::visit_expr(self, expr)?;

                if let TypeReferenceKind::List(ref t) = expr_type {
                    let index_type = Self::visit_expr(self, index)?;

                    if !index_type.is_integer() {
                        return Err((
                            ValidatorError::IndexNotInt(
                                expr_type.to_string(),
                                index_type.to_string(),
                            ),
                            Some(span),
                        ));
                    }

                    Ok(t.kind.clone())
                } else {
                    Err((
                        ValidatorError::IndexOnNonList(expr_type.to_string()),
                        Some(span),
                    ))
                }
            }
            ExprKind::Constructor { ty, fields } => {
                let type_body = self.look_up_type_body(ty);

                let variant = if let Some(type_body) = type_body {
                    if type_body.variants.len() != 1
                        || type_body.variants.first().unwrap().name.name != "self"
                    {
                        return Err((
                            ValidatorError::ConstructorOfVariantedType(ty.clone()),
                            Some(span),
                        ));
                    }

                    type_body.variants.first().unwrap()
                } else {
                    let variant = self.look_up_type_variant(ty);

                    if variant.is_none() {
                        return Err((ValidatorError::UnknownType(ty.clone()), Some(span)));
                    }

                    variant.unwrap()
                };

                let mut fields_constructed = Vec::new();

                for field in fields {
                    match field {
                        FieldConstructor::Named { name, value } => {
                            if fields_constructed.contains(&name.name) {
                                return Err((
                                    ValidatorError::ConstructorFieldDuplicate(
                                        ty.clone(),
                                        name.name.clone(),
                                    ),
                                    Some(span),
                                ));
                            }

                            let type_field =
                                variant.fields.iter().find(|field| field.name.same(name));

                            if type_field.is_none() {
                                return Err((
                                    ValidatorError::UnknownField(ty.clone(), name.name.clone()),
                                    Some(span),
                                ));
                            }

                            let type_field = type_field.unwrap();

                            let value_type = Self::visit_expr(self, value)?;

                            let compatible =
                                type_field.ty.kind.is_compatible(&value_type, self.root());

                            if let Err(err) = compatible {
                                return Err((err, Some(span)));
                            }

                            if !compatible.unwrap() {
                                return Err((
                                    ValidatorError::ConstructorFieldTypeMismatch(
                                        ty.clone(),
                                        name.name.clone(),
                                        type_field.ty.kind.to_string(),
                                        value_type.to_string(),
                                    ),
                                    Some(span),
                                ));
                            }

                            fields_constructed.push(name.name.clone());
                        }
                        FieldConstructor::Auto(name) => {
                            if fields_constructed.contains(&name.name) {
                                return Err((
                                    ValidatorError::ConstructorFieldDuplicate(
                                        ty.clone(),
                                        name.name.clone(),
                                    ),
                                    Some(span),
                                ));
                            }

                            let variable = self.look_up_variable(&name.namespaced());

                            if variable.is_none() {
                                return Err((
                                    ValidatorError::UnknownVariable(name.namespaced()),
                                    Some(span),
                                ));
                            }

                            let variable = variable.unwrap();

                            let type_field =
                                variant.fields.iter().find(|field| field.name.same(name));

                            if type_field.is_none() {
                                return Err((
                                    ValidatorError::UnknownField(ty.clone(), name.name.clone()),
                                    Some(span),
                                ));
                            }

                            let type_field = type_field.unwrap();

                            let compatible = type_field
                                .ty
                                .kind
                                .is_compatible(&variable.kind, self.root());

                            if let Err(err) = compatible {
                                return Err((err, Some(span)));
                            }

                            if !compatible.unwrap() {
                                return Err((
                                    ValidatorError::ConstructorFieldTypeMismatch(
                                        ty.clone(),
                                        name.name.clone(),
                                        type_field.ty.kind.to_string(),
                                        variable.kind.to_string(),
                                    ),
                                    Some(span),
                                ));
                            }

                            fields_constructed.push(name.name.clone());
                        }
                    }
                }

                for field in variant.fields.iter() {
                    if !fields_constructed.contains(&field.name.name) {
                        return Err((
                            ValidatorError::ConstructorFieldMissing(
                                ty.clone(),
                                field.name.name.clone(),
                            ),
                            Some(span),
                        ));
                    }
                }

                Ok(TypeReferenceKind::Named(ty.clone()))
            }
        }
    }

    fn visit_type_reference(&mut self, ty: &mut TypeReference) -> Self::TypeReferenceResult {
        Ok(ty.kind.clone())
    }
}
