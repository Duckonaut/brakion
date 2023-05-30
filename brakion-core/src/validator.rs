use std::{cell::UnsafeCell, collections::HashMap};

use crate::{
    errors::{validator::ValidatorError, ErrorModule},
    repr::{
        look_up_decl, BinaryOp, BrakionTreeVisitor, Decl, Expr, ExprKind, FieldConstructor,
        Function, FunctionSignature, Identifier, IntSize, Literal, NamespaceReference,
        NamespacedIdentifier, Parameter, ParameterSpec, Stmt, StmtKind, TraitBody, TypeBinaryOp,
        TypeBody, TypeReference, TypeReferenceKind, TypeVariant, UnaryOp,
    },
    unit::Span,
};

pub struct Validator<'a> {
    error_module: ErrorModule,
    // so we can modify it in place.
    execution_scopes: Vec<ExecutionScope>,
    root: UnsafeCell<&'a mut [Decl]>,
    module_stack: Vec<String>,
    current_type: Option<NamespacedIdentifier>,
    current_trait: Option<NamespacedIdentifier>,
}

struct ExecutionScope {
    variables: Vec<(Identifier, TypeReference)>,
}

impl<'a> Validator<'a> {
    pub fn new(error_module: ErrorModule, decls: &'a mut [Decl]) -> Self {
        Self {
            error_module,
            execution_scopes: Vec::new(),
            root: UnsafeCell::new(decls),
            module_stack: vec![],
            current_type: None,
            current_trait: None,
        }
    }

    fn error(&mut self, err: ValidatorError, span: Option<Span>) {
        self.error_module.add_validator_error(err, span);
    }

    fn root(&self) -> &'a mut [Decl] {
        unsafe { *self.root.get() }
    }

    fn look_up_variable(&self, name: &Identifier) -> Option<TypeReference> {
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

    fn set_var_type(&mut self, name: Identifier, type_ref: TypeReference) {
        self.execution_scopes
            .last_mut()
            .unwrap()
            .variables
            .push((name, type_ref));
    }

    fn begin_module(&mut self, name: String) {
        self.module_stack.push(name);
    }

    fn end_module(&mut self) {
        self.module_stack.pop();
    }

    /// Check if preconditions can be collapsed, and collapse them if they can.
    ///
    /// You can visualize this as a table, where each row is a function variant, and each column is
    /// a parameter. Function parameter preconditions act as a pattern to match the runtime
    /// arguments against.
    ///
    /// The goal of this function is to collapse the functions into a single function, which wraps
    /// the bodies of the original functions in checks to ensure that the preconditions are met.
    ///
    /// For example, if we have the following functions:
    /// ```brn
    /// fn foo(a: Foo ? Foo::A, b: Bar) -> Baz
    /// fn foo(a: Foo ? Foo::B, b: Bar) -> Baz
    /// ```
    /// We can collapse them into a single function:
    /// ```brn
    /// fn foo(a: Foo, b: Bar) -> Baz {
    ///     if a is Foo::A {
    ///         <body of first function>
    ///     }
    ///     else if a is Foo::B {
    ///         <body of second function>
    ///     }
    /// }
    /// ```
    ///
    /// A more complex example:
    /// ```brn
    /// fn foo(a: Foo ? Foo::A, b: Bar ? Bar::A) -> Baz
    /// fn foo(a: Foo ? Foo::A, b: Bar ? Bar::B) -> Baz
    /// fn foo(a: Foo ? Foo::B, b: Bar ? Bar::A) -> Baz
    /// fn foo(a: Foo, b: Bar) -> Baz
    /// ```
    ///
    /// The last function is a catch-all, and is required if the preconditions are not exhaustive.
    /// The preconditions are exhaustive if there is a precondition for every possible combination
    /// of variants.
    ///
    /// The above functions can be collapsed into:
    /// ```brn
    /// fn foo(a: Foo, b: Bar) -> Baz {
    ///     if a is Foo::A {
    ///         if b is Bar::A {
    ///             <body of first function>
    ///             return;
    ///         }
    ///         else if b is Bar::B {
    ///             <body of second function>
    ///         }
    ///     }
    ///     else if a is Foo::B {
    ///         if b is Bar::A {
    ///             <body of third function>
    ///         }
    ///     }
    ///
    ///     <body of catch-all function>
    /// }
    /// ```
    ///
    /// Requirements:
    /// - All functions must have the same name
    /// - All functions must have the same return type
    /// - All functions must have the same number of parameters
    /// - All functions must have the same parameter types
    /// - All functions must have the same parameter names
    /// - If a column has a precondition, then there are two possibilities:
    ///     - All rows must have a precondition for that column, and be exhaustive
    ///     - There must be a single catch-all function, in which no other parameters have
    ///       preconditions
    pub(crate) fn collapse_preconditioned_functions(
        &mut self,
        functions: &mut [Function],
    ) -> Result<Function, (ValidatorError, Option<Span>)> {
        assert!(!functions.is_empty());
        assert!(functions.len() > 1);

        // Check that all functions have the same name.
        let name = functions.first().unwrap().signature.name.clone();

        for function in functions.iter() {
            if !function.signature.name.same(&name) {
                return Err((
                    ValidatorError::PreconditionCollapseFailed,
                    Some(function.signature.name.span),
                ));
            }
        }

        // Build a table of the possible variants for each parameter.
        let mut variant_names = Vec::new();

        let should_take_self = functions.first().unwrap().signature.takes_self;

        if should_take_self {
            // First, add the variants for the self parameter, if there is one.
            if let Some(type_name) = &self.current_type {
                let mut self_variants = Vec::new();

                let type_body = self
                    .look_up_type_body(type_name)
                    .expect("type body should exist");

                if type_body.variants.len() == 1 {
                    self_variants.push(TypeReferenceKind::Named(type_name.clone()));
                } else {
                    for variant in type_body.variants.iter() {
                        self_variants.push(TypeReferenceKind::Named(
                            type_name.down(variant.name.clone()),
                        ));
                    }
                }

                variant_names.push(self_variants);
            }
        }

        // Then, add the variants for each parameter.
        for param in functions.first().unwrap().signature.parameters.iter() {
            let mut param_variants = Vec::new();
            match &param.ty.kind {
                TypeReferenceKind::Named(name) => {
                    let type_body = self.look_up_type_body(name);

                    if let Some(type_body) = type_body {
                        if type_body.variants.len() == 1 {
                            param_variants.push(TypeReferenceKind::Named(name.clone()));
                        } else {
                            for variant in type_body.variants.iter() {
                                param_variants.push(TypeReferenceKind::Named(
                                    name.down(variant.name.clone()),
                                ));
                            }
                        }
                    }
                }
                TypeReferenceKind::Union(types) => {
                    for ty in types.iter() {
                        param_variants.push(ty.kind.clone());
                    }
                }
                _ => return Err((ValidatorError::PreconditionCollapseFailed, None)),
            }

            variant_names.push(param_variants);
        }

        // Check that all functions have the same number of parameters, return type, and parameter
        // types and names.
        let parameter_types = functions
            .first()
            .unwrap()
            .signature
            .parameters
            .iter()
            .map(|p| p.ty.kind.clone())
            .collect::<Vec<_>>();

        let parameter_names = functions
            .first()
            .unwrap()
            .signature
            .parameters
            .iter()
            .map(|p| p.name.name.clone())
            .collect::<Vec<_>>();

        let return_type = functions
            .first()
            .unwrap()
            .signature
            .return_type
            .kind
            .clone();

        for function in functions.iter_mut() {
            if function.signature.return_type.kind != return_type {
                return Err((
                    ValidatorError::PreconditionCollapseFailed,
                    Some(function.signature.name.span),
                ));
            }

            if function.signature.takes_self != should_take_self {
                return Err((
                    ValidatorError::PreconditionCollapseFailed,
                    Some(function.signature.name.span),
                ));
            }

            if function.signature.parameters.len() != parameter_types.len() {
                return Err((
                    ValidatorError::PreconditionCollapseFailed,
                    Some(function.signature.name.span),
                ));
            }

            for (param, name) in function
                .signature
                .parameters
                .iter_mut()
                .zip(parameter_names.iter())
            {
                if param.name.name != *name {
                    return Err((
                        ValidatorError::PreconditionCollapseFailed,
                        Some(param.name.span),
                    ));
                }
            }

            for (param, ty) in function
                .signature
                .parameters
                .iter_mut()
                .zip(parameter_types.iter())
            {
                if param.ty.kind != *ty {
                    return Err((
                        ValidatorError::PreconditionCollapseFailed,
                        Some(param.name.span),
                    ));
                }
            }
        }

        // Check if catch-alls column-wise are valid, i.e. if there is a single row with a precondition
        // in a column, then there must be either a single catch-all function, being the same row
        // in all columns, or the preconditions must be exhaustive.

        let parameter_precondition_matrix = functions
            .iter()
            .map(|f| {
                f.signature
                    .parameters
                    .iter()
                    .map(|p| match &p.kind {
                        ParameterSpec::Basic => None,
                        ParameterSpec::Preconditioned(p) => Some(p.kind.clone()),
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        // Check if precondition columns are valid.
        for (i, row) in parameter_precondition_matrix.iter().enumerate() {
            for (j, precondition) in row.iter().enumerate() {
                if precondition.is_some()
                    && !variant_names[j]
                        .iter()
                        .any(|v| v.same(precondition.as_ref().unwrap()))
                {
                    return Err((
                        ValidatorError::PreconditionCollapseFailed,
                        Some(functions[i].signature.name.span),
                    ));
                }
            }
        }

        // Check if there are no rows with duplicate preconditions.
        for (i, row) in parameter_precondition_matrix.iter().enumerate() {
            for (k, other_row) in parameter_precondition_matrix.iter().enumerate() {
                if i != k
                    && row.iter().zip(other_row.iter()).all(|(a, b)| {
                        (a.is_none() && b.is_none())
                            && (a.is_some()
                                && b.is_some()
                                && a.as_ref().unwrap().same(b.as_ref().unwrap()))
                    })
                {
                    return Err((
                        ValidatorError::PreconditionCollapseFailed,
                        Some(functions[i].signature.name.span),
                    ));
                }
            }
        }

        let mut columns_have_preconditions = vec![false; parameter_precondition_matrix[0].len()];

        for row in parameter_precondition_matrix.iter() {
            for (i, precondition) in row.iter().enumerate() {
                if precondition.is_some() {
                    columns_have_preconditions[i] = true;
                }
            }
        }

        let mut catch_all_counts = vec![0; parameter_precondition_matrix[0].len()];

        for row in parameter_precondition_matrix.iter() {
            for (i, precondition) in row.iter().enumerate() {
                if precondition.is_none() {
                    catch_all_counts[i] += 1;
                }
            }
        }

        for (i, count) in catch_all_counts.iter().enumerate() {
            if *count == 0 && columns_have_preconditions[i] {
                // Check if exhaustive.
                let mut variants_covered = vec![];
                for row in parameter_precondition_matrix.iter() {
                    variants_covered.push(row[i].as_ref().unwrap().clone());
                }

                if !variant_names[i]
                    .iter()
                    .all(|v| variants_covered.contains(v))
                {
                    return Err((
                        ValidatorError::PreconditionCollapseFailed,
                        Some(functions[0].signature.name.span),
                    ));
                }
            } else if *count > 1 {
                return Err((
                    ValidatorError::PreconditionCollapseFailed,
                    Some(functions[0].signature.name.span),
                ));
            }
        }

        let mut catch_all = None;

        for (i, function) in functions.iter_mut().enumerate() {
            if catch_all.is_none() {
                if parameter_precondition_matrix[i].iter().all(|p| p.is_none()) {
                    catch_all = Some(function.body.clone());
                }
            } else if parameter_precondition_matrix[i].iter().all(|p| p.is_none()) {
                return Err((
                    ValidatorError::PreconditionCollapseFailed,
                    Some(function.signature.name.span),
                ));
            }
        }

        // At this point, we checked everything we could, we can now collapse the functions.

        let precond_stripped_params = functions
            .first()
            .unwrap()
            .signature
            .parameters
            .iter()
            .map(|p| match &p.kind {
                ParameterSpec::Basic => p.clone(),
                ParameterSpec::Preconditioned(_) => Parameter {
                    name: p.name.clone(),
                    ty: p.ty.clone(),
                    kind: ParameterSpec::Basic,
                },
            })
            .collect::<Vec<_>>();

            dbg!(&precond_stripped_params);

        let collapsed_function_signature = FunctionSignature {
            name,
            parameters: precond_stripped_params,
            return_type: functions.first().unwrap().signature.return_type.clone(),
            takes_self: functions.first().unwrap().signature.takes_self,
            self_precondition: None,
        };

        let mut collapsed_body_statements = vec![];

        for (i, function) in functions.iter().enumerate() {
            let preconditions = &parameter_precondition_matrix[i];

            let og_span = Span::from_spans(
                function.body.first().unwrap().span,
                function.body.last().unwrap().span,
            );

            let branch = Stmt {
                span: og_span,
                kind: StmtKind::Block(function.body.clone()),
            };

            let mut condition = None;

            for (param_name, precond) in parameter_names.iter().zip(preconditions).rev() {
                if precond.is_none() {
                    continue;
                }

                let precond = precond.as_ref().unwrap();

                if condition.is_none() {
                    condition = Some(Expr {
                        span: og_span,
                        kind: ExprKind::TypeBinary {
                            op: TypeBinaryOp::Is,
                            expr: Box::new(Expr {
                                span: og_span,
                                kind: ExprKind::Variable(NamespacedIdentifier {
                                    namespace: vec![],
                                    ident: Identifier::new(og_span, param_name.clone()),
                                }),
                            }),
                            ty: TypeReference {
                                span: None,
                                kind: precond.clone(),
                            },
                        },
                    });
                } else {
                    condition = Some(Expr {
                        span: og_span,
                        kind: ExprKind::Binary {
                            op: BinaryOp::And,
                            left: Box::new(condition.unwrap()),
                            right: Box::new(Expr {
                                span: og_span,
                                kind: ExprKind::TypeBinary {
                                    op: TypeBinaryOp::Is,
                                    expr: Box::new(Expr {
                                        span: og_span,
                                        kind: ExprKind::Variable(NamespacedIdentifier {
                                            namespace: vec![],
                                            ident: Identifier::new(og_span, param_name.clone()),
                                        }),
                                    }),
                                    ty: TypeReference {
                                        span: None,
                                        kind: precond.clone(),
                                    },
                                },
                            }),
                        },
                    });
                }
            }

            if let Some(condition) = condition {
                collapsed_body_statements.push(Stmt {
                    span: og_span,
                    kind: StmtKind::If {
                        condition,
                        then: Box::new(branch),
                        otherwise: None,
                    },
                });
            }
            else {
                collapsed_body_statements.push(branch);
            }
        }

        let collapsed_function = Function {
            signature: collapsed_function_signature,
            body: collapsed_body_statements,
        };

        Ok(collapsed_function)
    }

    fn validate_function(
        &mut self,
        function: &mut Function,
    ) -> Result<(), (ValidatorError, Option<Span>)> {
        self.validate_function_signature(&mut function.signature)?;

        self.begin_scope();

        for param in function.signature.parameters.iter() {
            self.execution_scopes
                .last_mut()
                .unwrap()
                .variables
                .push((param.name.clone(), param.ty.clone()));
        }

        Ok(())
    }

    fn validate_function_signature(
        &mut self,
        signature: &mut FunctionSignature,
    ) -> Result<(), (ValidatorError, Option<Span>)> {
        if signature.takes_self && self.current_type.is_none() && self.current_trait.is_none() {
            self.error(
                ValidatorError::SelfInStaticFunction(signature.name.name.clone()),
                Some(signature.name.span),
            );
        }

        let mut seen: Vec<&str> = Vec::new();

        for param in signature.parameters.iter_mut() {
            if seen.iter().any(|s| *s == param.name.name) {
                return Err((
                    ValidatorError::DuplicateParameter(param.name.name.clone()),
                    Some(param.name.span),
                ));
            }

            seen.push(&param.name.name);

            self.visit_type_reference(&mut param.ty)?;
        }

        Ok(())
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
            Self::visit_decl(self, decl);
        }
    }
}

impl<'a> BrakionTreeVisitor for Validator<'a> {
    type ExprResult = Result<TypeReferenceKind, (ValidatorError, Option<Span>)>;
    type StmtResult = Result<(), (ValidatorError, Option<Span>)>;
    type DeclResult = ();
    type TypeReferenceResult = Result<TypeReferenceKind, (ValidatorError, Option<Span>)>;

    fn visit_decl(&mut self, decl: &mut Decl) -> Self::DeclResult {
        match decl {
            Decl::Module { name, body, .. } => {
                self.begin_module(name.name.clone());

                for decl in body.iter_mut() {
                    Self::visit_decl(self, decl);
                }

                self.end_module();
            }
            Decl::Function { function, .. } => {
                let result = self.validate_function(function);

                if let Err(err) = result {
                    self.error(err.0, err.1);
                }
            }
            Decl::Type { name, body, .. } => {
                self.current_type = Some(NamespacedIdentifier::new_from_strs(
                    &self.module_stack,
                    name.name.clone(),
                ));

                let mut seen = Vec::new();

                for variant in body.variants.iter_mut() {
                    if seen.contains(&variant.name.name) {
                        self.error(
                            ValidatorError::DuplicateVariant(variant.name.name.clone()),
                            Some(variant.name.span),
                        );
                    }

                    seen.push(variant.name.name.clone());

                    let mut seen_fields = Vec::new();

                    for field in variant.fields.iter_mut() {
                        if seen_fields.contains(&field.name.name) {
                            self.error(
                                ValidatorError::DuplicateField(field.name.name.clone()),
                                Some(field.name.span),
                            );
                        }

                        seen_fields.push(field.name.name.clone());

                        let result = self.visit_type_reference(&mut field.ty);

                        if let Err(err) = result {
                            self.error(err.0, err.1);
                        }
                    }
                }

                for method in body.methods.iter_mut() {
                    let result = self.validate_function(&mut method.1);

                    if let Err(err) = result {
                        self.error(err.0, err.1);
                    }
                }

                self.current_type = None;
            }
            Decl::Trait { name, body, .. } => {
                self.current_trait = Some(NamespacedIdentifier::new_from_strs(
                    &self.module_stack,
                    name.name.clone(),
                ));

                for method in body.methods.iter_mut() {
                    let result = self.validate_function_signature(method);

                    if let Err(err) = result {
                        self.error(err.0, err.1);
                    }
                }

                self.current_trait = None;
            }
            Decl::Impl {
                trait_name,
                type_name,
                body,
            } => {
                let impl_trait_body = self.look_up_trait_body(trait_name);

                if impl_trait_body.is_none() {
                    self.error(
                        ValidatorError::UnknownTrait(trait_name.clone()),
                        Some(trait_name.span()),
                    );
                }

                let impl_type_body = self.look_up_type_body(type_name);

                if impl_type_body.is_none() {
                    self.error(
                        ValidatorError::UnknownType(type_name.clone()),
                        Some(type_name.span()),
                    );
                }

                let impl_trait_body = impl_trait_body.unwrap();

                let mut seen = Vec::new();

                self.current_type = Some(type_name.clone());

                for function in body.iter_mut() {
                    if seen.contains(&function.signature.name.name) {
                        self.error(
                            ValidatorError::DuplicateFunction(function.signature.name.name.clone()),
                            Some(function.signature.name.span),
                        );
                    }

                    seen.push(function.signature.name.name.clone());

                    let result = self.validate_function(function);

                    if let Err(err) = result {
                        self.error(err.0, err.1);
                    }
                }

                for trait_sig in impl_trait_body.methods.iter() {
                    if !seen.contains(&trait_sig.name.name) {
                        self.error(
                            ValidatorError::MissingTraitMethod(trait_sig.name.name.clone()),
                            Some(trait_sig.name.span),
                        );
                    }
                }

                self.current_type = None;
            }
        }
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
                if v.ident.name == "self" {
                    if self.current_type.is_none() && self.current_trait.is_none() {
                        return Err((ValidatorError::SelfOutsideOfTraitOrType, Some(expr.span)));
                    }

                    if self.current_type.is_some() {
                        return Ok(TypeReferenceKind::Named(self.current_type.clone().unwrap()));
                    }

                    if self.current_trait.is_some() {
                        return Ok(TypeReferenceKind::Named(
                            self.current_trait.clone().unwrap(),
                        ));
                    }
                }

                let var = self.look_up_variable(&v.ident);

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

                            let variable = self.look_up_variable(name);

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
        match &mut ty.kind {
            TypeReferenceKind::Named(name) => {
                let type_body = self.look_up_type_body(name);

                if type_body.is_none() {
                    let trait_body = self.look_up_trait_body(name);

                    if trait_body.is_none() {
                        return Err((ValidatorError::UnknownType(name.clone()), Some(name.span())));
                    }
                }
            }
            TypeReferenceKind::List(t) => {
                self.visit_type_reference(t)?;
            }
            TypeReferenceKind::Union(types) => {
                let mut types_accounted_for = Vec::new();

                for ty in types {
                    self.visit_type_reference(ty)?;

                    if types_accounted_for.contains(&ty) {
                        return Err((
                            ValidatorError::UnionTypeDuplicate(ty.kind.to_string()),
                            ty.span,
                        ));
                    }

                    types_accounted_for.push(ty);
                }
            }
            _ => {}
        }

        Ok(ty.kind.clone())
    }
}
