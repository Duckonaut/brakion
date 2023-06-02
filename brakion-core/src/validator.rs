use std::{cell::UnsafeCell, collections::HashMap};

use crate::{
    errors::{validator::ValidatorError, ErrorModule},
    repr::{
        list_method_signatures, look_up_decl, look_up_module_mut, string_method_signatures,
        BinaryOp, BrakionTreeVisitor, Decl, Expr, ExprKind, FieldConstructor, Function,
        FunctionSignature, Identifier, IntSize, Literal, MatchPattern, NamespaceReference,
        NamespacedIdentifier, Parameter, ParameterSpec, Stmt, StmtKind, TraitBody, TypeBinaryOp,
        TypeBody, TypeReference, TypeReferenceKind, TypeVariant, UnaryOp, Visibility,
    },
    unit::Span,
};

pub struct Validator<'a> {
    error_module: ErrorModule,
    // so we can modify it in place.
    execution_scopes: Vec<ExecutionScope>,
    root: UnsafeCell<&'a mut Vec<Decl>>,
    module_stack: Vec<String>,
    current_type: Option<NamespacedIdentifier>,
    current_trait: Option<NamespacedIdentifier>,
    current_function_return_type: Option<TypeReferenceKind>,
    in_loop: bool,
}

struct ExecutionScope {
    variables: Vec<(Identifier, TypeReference)>,
}

impl<'a> Validator<'a> {
    pub fn new(error_module: ErrorModule, decls: &'a mut Vec<Decl>) -> Self {
        Self {
            error_module,
            execution_scopes: Vec::new(),
            root: UnsafeCell::new(decls),
            module_stack: vec![],
            current_type: None,
            current_trait: None,
            current_function_return_type: None,
            in_loop: false,
        }
    }

    fn error(&mut self, err: ValidatorError, span: Option<Span>) {
        self.error_module.add_validator_error(err, span);
    }

    fn root(&self) -> &'a mut [Decl] {
        unsafe { *self.root.get() }
    }

    fn root_vec(&self) -> &'a mut Vec<Decl> {
        unsafe { *self.root.get() }
    }

    fn get_current_module_decls(&self) -> &'a mut [Decl] {
        let decls = look_up_module_mut(self.root(), &self.module_stack);

        if let Some(decls) = decls {
            decls
        } else {
            panic!("current module not found");
        }
    }

    fn expand_self_namespaced_identifier(&self, name: &mut NamespacedIdentifier) {
        if name.namespace.is_empty() && name.ident.name == "Self" {
            if self.current_type.is_some() {
                name.ident = self.current_type.clone().unwrap().ident;
            } else if self.current_trait.is_some() {
                name.ident = self.current_trait.clone().unwrap().ident;
            }
        } else if name.namespace.len() == 1 && name.namespace[0].name == "Self" {
            let self_span = name.namespace[0].span;

            if self.current_type.is_some() {
                name.namespace = vec![Identifier {
                    name: self.current_type.as_ref().unwrap().ident.name.clone(),
                    span: self_span,
                }];
            } else if self.current_trait.is_some() {
                name.namespace = vec![Identifier {
                    name: self.current_trait.as_ref().unwrap().ident.name.clone(),
                    span: self_span,
                }];
            }
        }
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

    fn look_up_type_body(&self, name: &mut NamespacedIdentifier) -> Option<&'a TypeBody> {
        self.expand_self_namespaced_identifier(name);

        let decls_to_check = self.get_current_module_decls();

        let decl = look_up_decl(decls_to_check, name);

        if let Some(NamespaceReference::Decl(Decl::Type { body, .. })) = decl {
            for module in self.module_stack.iter().rev() {
                name.namespace
                    .insert(0, Identifier::new(name.span(), module.clone()));
            }

            return Some(body);
        }

        let decl = look_up_decl(self.root(), name)?;

        if let NamespaceReference::Decl(Decl::Type { body, .. }) = decl {
            return Some(body);
        }

        None
    }

    fn look_up_trait_body(&self, name: &mut NamespacedIdentifier) -> Option<&'a TraitBody> {
        self.expand_self_namespaced_identifier(name);

        let decls_to_check = self.get_current_module_decls();

        let decl = look_up_decl(decls_to_check, name);

        if let Some(NamespaceReference::Decl(Decl::Trait { body, .. })) = decl {
            for module in self.module_stack.iter().rev() {
                name.namespace
                    .insert(0, Identifier::new(name.span(), module.clone()));
            }

            return Some(body);
        }

        let decl = look_up_decl(self.root(), name)?;

        if let NamespaceReference::Decl(Decl::Trait { body, .. }) = decl {
            return Some(body);
        }

        None
    }

    fn look_up_type_variant(&self, name: &mut NamespacedIdentifier) -> Option<&'a TypeVariant> {
        self.expand_self_namespaced_identifier(name);

        let decls_to_check = self.get_current_module_decls();

        let decl = look_up_decl(decls_to_check, name);

        if let Some(NamespaceReference::TypeVariant(variant)) = decl {
            for module in self.module_stack.iter().rev() {
                name.namespace
                    .insert(0, Identifier::new(name.span(), module.clone()));
            }

            return Some(variant);
        }

        let decl = look_up_decl(self.root(), name)?;

        if let NamespaceReference::TypeVariant(variant) = decl {
            return Some(variant);
        }

        None
    }

    fn look_up_function(&self, name: &mut NamespacedIdentifier) -> Option<&'a Function> {
        self.expand_self_namespaced_identifier(name);

        let decls_to_check = self.get_current_module_decls();

        let decl = look_up_decl(decls_to_check, name);

        if let Some(NamespaceReference::Decl(Decl::Function { function, .. })) = decl {
            for module in self.module_stack.iter().rev() {
                name.namespace
                    .insert(0, Identifier::new(name.span(), module.clone()));
            }

            return Some(function);
        }

        if let Some(NamespaceReference::TypeMethod(function)) = decl {
            for module in self.module_stack.iter().rev() {
                name.namespace
                    .insert(0, Identifier::new(name.span(), module.clone()));
            }

            return Some(function);
        }

        let decl = look_up_decl(self.root(), name);

        if let Some(NamespaceReference::Decl(Decl::Function { function, .. })) = decl {
            return Some(function);
        }

        if let Some(NamespaceReference::TypeMethod(function)) = decl {
            return Some(function);
        }

        None
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
        for (var_name, var_type) in self
            .execution_scopes
            .last_mut()
            .unwrap()
            .variables
            .iter_mut()
            .rev()
        {
            if var_name.same(&name) {
                *var_type = type_ref;
                return;
            }
        }
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
    ///         var a = a as Foo::A;
    ///         <body of first function>
    ///     }
    ///     else if a is Foo::B {
    ///         var a = a as Foo::B;
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
    ///     if a is Foo::A and b is Bar::A {
    ///         var a = a as Foo::A;
    ///         var b = b as Bar::A;
    ///         <body of first function>
    ///         return;
    ///     }
    ///     else if a is Foo::A and b is Bar::B {
    ///         var a = a as Foo::A;
    ///         var b = b as Bar::B;
    ///         <body of second function>
    ///         return;
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
                    ValidatorError::PreconditionNameMismatch(
                        name.name.clone(),
                        function.signature.name.name.clone(),
                    ),
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

                let mut type_name = type_name.clone();

                let type_body = self
                    .look_up_type_body(&mut type_name)
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
        for param in functions
            .first_mut()
            .unwrap()
            .signature
            .parameters
            .iter_mut()
        {
            let mut param_variants = Vec::new();
            match &mut param.ty.kind {
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
                ty => {
                    return Err((
                        ValidatorError::PreconditionOnInvalidType(ty.to_string()),
                        param.ty.span,
                    ))
                }
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

        let parameter_spans = functions
            .first()
            .unwrap()
            .signature
            .parameters
            .iter()
            .map(|p| p.name.span)
            .collect::<Vec<_>>();

        let return_type = functions
            .first()
            .unwrap()
            .signature
            .return_type
            .kind
            .clone();

        for function in functions.iter_mut() {
            if !function.signature.return_type.kind.same(&return_type) {
                return Err((
                    ValidatorError::PreconditionReturnMismatch(
                        return_type.to_string(),
                        function.signature.return_type.kind.to_string(),
                    ),
                    Some(function.signature.name.span),
                ));
            }

            if function.signature.takes_self != should_take_self {
                return Err((
                    ValidatorError::PreconditionSelfMismatch,
                    Some(function.signature.name.span),
                ));
            }

            if function.signature.parameters.len() != parameter_types.len() {
                return Err((
                    ValidatorError::PreconditionParameterCountMismatch(
                        parameter_types.len(),
                        function.signature.parameters.len(),
                    ),
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
                        ValidatorError::PreconditionParameterMismatch(
                            name.clone(),
                            param.name.name.clone(),
                        ),
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
                if !param.ty.kind.same(ty) {
                    return Err((
                        ValidatorError::PreconditionParameterTypeMismatch(
                            ty.to_string(),
                            param.ty.kind.to_string(),
                        ),
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
                        ParameterSpec::Preconditioned(p) => Some(p.clone()),
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
                        .any(|v| v.same(&precondition.as_ref().unwrap().kind))
                {
                    return Err((
                        ValidatorError::PreconditionParameterInvalid(parameter_names[j].clone()),
                        Some(parameter_spans[j]),
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
                                && a.as_ref().unwrap().kind.same(&b.as_ref().unwrap().kind))
                    })
                {
                    return Err((
                        ValidatorError::PreconditionDuplicate(
                            functions[i].signature.name.name.clone(),
                        ),
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
                    variants_covered.push(row[i].as_ref().unwrap().kind.clone());
                }

                if !variant_names[i]
                    .iter()
                    .all(|v| variants_covered.contains(v))
                {
                    return Err((
                        ValidatorError::PreconditionNotExhaustive(parameter_names[i].clone()),
                        Some(functions[0].signature.name.span),
                    ));
                }
            } else if *count > 1 && columns_have_preconditions[i] {
                return Err((
                    ValidatorError::PreconditionWildcardDuplicate(parameter_names[i].clone()),
                    Some(functions[0].signature.name.span),
                ));
            }
        }

        let mut catch_all = None;
        let mut catch_all_index = None;

        for (i, function) in functions.iter_mut().enumerate() {
            if catch_all.is_none() {
                if parameter_precondition_matrix[i].iter().all(|p| p.is_none()) {
                    catch_all = Some(function.body.clone());
                    catch_all_index = Some(i);
                }
            } else if parameter_precondition_matrix[i].iter().all(|p| p.is_none()) {
                return Err((
                    ValidatorError::PreconditionWildcardInvalid(name.name),
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

        let collapsed_function_signature = FunctionSignature {
            name,
            parameters: precond_stripped_params,
            return_type: functions.first().unwrap().signature.return_type.clone(),
            takes_self: functions.first().unwrap().signature.takes_self,
            self_precondition: None,
        };

        let mut collapsed_body_statements = vec![];

        let function_last_index = functions.len() - 1;

        for (i, function) in functions.iter().enumerate() {
            if let Some(j) = catch_all_index {
                if i == j {
                    continue;
                }
            }
            let preconditions = &parameter_precondition_matrix[i];

            let og_span = Span::from_spans(
                function.body.first().unwrap().span,
                function.body.last().unwrap().span,
            );

            let mut branch_body = function.body.clone();

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
                            ty: precond.clone(),
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
                                    ty: precond.clone(),
                                },
                            }),
                        },
                    });
                }

                branch_body.insert(
                    0,
                    Stmt {
                        span: precond.span.unwrap(),
                        kind: StmtKind::Variable {
                            name: Identifier::new(og_span, param_name.clone()),
                            ty: precond.clone(),
                            value: Expr {
                                span: precond.span.unwrap(),
                                kind: ExprKind::TypeBinary {
                                    op: TypeBinaryOp::As,
                                    expr: Box::new(Expr {
                                        span: og_span,
                                        kind: ExprKind::Variable(NamespacedIdentifier {
                                            namespace: vec![],
                                            ident: Identifier::new(og_span, param_name.clone()),
                                        }),
                                    }),
                                    ty: precond.clone(),
                                },
                            },
                        },
                    },
                );
            }
            

            if i == function_last_index {
                collapsed_body_statements.extend(branch_body);
                break;
            }

            let branch = Stmt {
                span: og_span,
                kind: StmtKind::Block(branch_body),
            };

            if let Some(condition) = condition {
                collapsed_body_statements.push(Stmt {
                    span: og_span,
                    kind: StmtKind::If {
                        condition,
                        then: Box::new(branch),
                        otherwise: None,
                    },
                });
            } else {
                collapsed_body_statements.push(branch);
            }
        }

        if let Some(catch_all) = catch_all {
            collapsed_body_statements.extend(catch_all);
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
        self.current_function_return_type = Some(function.signature.return_type.kind.clone());

        self.begin_scope();

        if function.signature.takes_self {
            let self_type = if self.current_type.is_some() {
                self.current_type.as_ref().unwrap().clone()
            } else if self.current_trait.is_some() {
                self.current_trait.as_ref().unwrap().clone()
            } else {
                return Err((
                    ValidatorError::SelfOutsideOfTraitOrType,
                    Some(function.signature.name.span),
                ));
            };

            self.set_var_type(
                Identifier::new(function.signature.name.span, "self".to_string()),
                TypeReference {
                    span: None,
                    kind: TypeReferenceKind::Named(self_type),
                },
            );
        }

        for param in function.signature.parameters.iter() {
            if let ParameterSpec::Preconditioned(precond) = &param.kind {
                self.set_var_type(param.name.clone(), precond.clone());
            } else {
                self.set_var_type(param.name.clone(), param.ty.clone());
            }
        }

        self.begin_scope();

        let mut has_return_stmt = false;

        for stmt in function.body.iter_mut() {
            if matches!(stmt.kind, StmtKind::Return { .. }) {
                has_return_stmt = true;
            }

            let result = Self::visit_stmt(self, stmt);

            if let Err((err, span)) = result {
                self.error(err, span);
            }
        }

        if !has_return_stmt {
            let can_return_void = function
                .signature
                .return_type
                .kind
                .is_compatible(&TypeReferenceKind::Void, self.root());

            match can_return_void {
                Ok(true) => {
                    function.body.push(Stmt {
                        span: function.signature.name.span,
                        kind: StmtKind::Return(Expr {
                            span: function.signature.name.span,
                            kind: ExprKind::Literal(Literal::Void),
                        }),
                    });
                }
                Ok(false) => {
                    self.error(
                        ValidatorError::NoReturnInFunction,
                        Some(function.signature.name.span),
                    );
                }
                Err(e) => {
                    self.error(e, Some(function.signature.name.span));
                }
            }
        }

        self.end_scope();

        self.end_scope();

        self.current_function_return_type = None;

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

            param.ty.kind = self.visit_type_reference(&mut param.ty)?;
        }

        signature.return_type.kind = self.visit_type_reference(&mut signature.return_type)?;

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
            let self_type_ref = self.visit_type_reference(&mut TypeReference {
                kind: self_type_ref.as_ref().unwrap().clone(),
                span: None,
            })?;

            let expected_arg_count = signature.parameters.len() + 1;

            if args.len() != expected_arg_count {
                let arg_span = if !args.is_empty() {
                    Some(Span::from_spans(
                        args.first().unwrap().span,
                        args.last().unwrap().span,
                    ))
                } else {
                    Some(name.span())
                };
                return Err((
                    ValidatorError::WrongArgCount(name.clone(), expected_arg_count, args.len()),
                    arg_span,
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
                let arg_span = if !args.is_empty() {
                    Some(Span::from_spans(
                        args.first().unwrap().span,
                        args.last().unwrap().span,
                    ))
                } else {
                    Some(name.span())
                };
                return Err((
                    ValidatorError::WrongArgCount(name.clone(), expected_arg_count, args.len()),
                    arg_span,
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

    fn validate_block(&mut self, block: &mut [Stmt]) -> Result<(), (ValidatorError, Option<Span>)> {
        self.begin_scope();

        for stmt in block.iter_mut() {
            Self::visit_stmt(self, stmt)?;
        }

        self.end_scope();

        Ok(())
    }

    /// This function validates the signatures of all functions in the AST.
    /// It is called before the main validation pass.
    /// It is necessary because the main validation pass needs to know the types of all functions,
    /// to fill in the type information of function calls.
    fn validate_function_signatures(&mut self, decls: &mut [Decl]) {
        for decl in decls.iter_mut() {
            match decl {
                Decl::Function { function, .. } => {
                    let result = self.validate_function_signature(&mut function.signature);

                    if let Err((err, span)) = result {
                        self.error(err, span);
                    }
                }
                Decl::Module { name, body, .. } => {
                    self.module_stack.push(name.name.clone());

                    self.validate_function_signatures(body);

                    self.module_stack.pop();
                }
                Decl::Type {
                    name,
                    body: TypeBody { methods, .. },
                    ..
                } => {
                    self.current_type = Some(NamespacedIdentifier::new_from_parts(
                        &self.module_stack,
                        name.name.clone(),
                        name.span,
                    ));

                    for method in methods.iter_mut() {
                        let result = self.validate_function_signature(&mut method.1.signature);

                        if let Err((err, span)) = result {
                            self.error(err, span);
                        }
                    }

                    self.current_type = None;
                }
                Decl::Impl {
                    type_name, body, ..
                } => {
                    self.current_type = Some(type_name.clone());

                    for method in body.iter_mut() {
                        let result = self.validate_function_signature(&mut method.signature);

                        if let Err((err, span)) = result {
                            self.error(err, span);
                        }
                    }

                    self.current_type = None;
                }
                Decl::Trait {
                    name,
                    body: TraitBody { methods, .. },
                    ..
                } => {
                    self.current_trait = Some(NamespacedIdentifier::new_from_parts(
                        &self.module_stack,
                        name.name.clone(),
                        name.span,
                    ));

                    for method in methods.iter_mut() {
                        let result = self.validate_function_signature(method);

                        if let Err((err, span)) = result {
                            self.error(err, span);
                        }
                    }

                    self.current_trait = None;
                }
            }
        }
    }

    fn collapse_all_functions_with_vis(
        &mut self,
        functions: Vec<(Visibility, Function)>,
    ) -> Result<Vec<(Visibility, Function)>, (ValidatorError, Option<Span>)> {
        let mut collapse_map: HashMap<String, Vec<Function>> = HashMap::new();
        let mut visibility_map: HashMap<String, Visibility> = HashMap::new();

        for (visibility, function) in functions {
            let name = function.signature.name.name.clone();

            if let Some(functions) = collapse_map.get_mut(&name) {
                functions.push(function);
            } else {
                collapse_map.insert(name.clone(), vec![function]);
            }

            if let Some(v) = visibility_map.get_mut(&name) {
                if *v != visibility {
                    return Err((
                        ValidatorError::VisibilityMismatch(name.clone(), *v, visibility),
                        None,
                    ));
                }
            } else {
                visibility_map.insert(name, visibility);
            }
        }

        let mut out = Vec::new();

        for ((_, fns), (_, v)) in collapse_map.iter_mut().zip(visibility_map.iter()) {
            if fns.len() == 1 {
                out.push((*v, fns.pop().unwrap()));
                continue;
            }
            let collapsed = self.collapse_preconditioned_functions(fns)?;

            out.push((*v, collapsed));
        }

        Ok(out)
    }

    fn collapse_all_functions(
        &mut self,
        functions: Vec<Function>,
    ) -> Result<Vec<Function>, (ValidatorError, Option<Span>)> {
        let mut collapse_map: HashMap<String, Vec<Function>> = HashMap::new();

        for function in functions {
            let name = function.signature.name.name.clone();

            if let Some(functions) = collapse_map.get_mut(&name) {
                functions.push(function);
            } else {
                collapse_map.insert(name.clone(), vec![function]);
            }
        }

        let mut out = Vec::new();

        for (_, fns) in collapse_map.iter_mut() {
            if fns.len() == 1 {
                out.push(fns.pop().unwrap());
                continue;
            }
            let collapsed = self.collapse_preconditioned_functions(fns)?;

            out.push(collapsed);
        }

        Ok(out)
    }

    /// This function walks through the tree and collapses all function definitions with
    /// preconditions.
    fn collapse_all_function_decls(
        &mut self,
        decls: &mut Vec<Decl>,
    ) -> Result<(), (ValidatorError, Option<Span>)> {
        let function_indices_to_remove: Vec<usize> = decls
            .iter()
            .enumerate()
            .filter_map(|(i, decl)| {
                if let Decl::Function { .. } = decl {
                    Some(i)
                } else {
                    None
                }
            })
            .collect();

        let functions: Vec<(Visibility, Function)> = function_indices_to_remove
            .iter()
            .rev()
            .map(|i| {
                if let Decl::Function {
                    function,
                    visibility,
                } = decls.remove(*i)
                {
                    (visibility, function)
                } else {
                    unreachable!()
                }
            })
            .collect();

        let collapsed = self.collapse_all_functions_with_vis(functions)?;

        for (visibility, function) in collapsed {
            decls.push(Decl::Function {
                visibility,
                function,
            });
        }

        for decl in decls.iter_mut() {
            match decl {
                Decl::Module { name, body, .. } => {
                    self.module_stack.push(name.name.clone());

                    self.collapse_all_function_decls(body)?;

                    self.module_stack.pop();
                }
                Decl::Type {
                    name,
                    body: TypeBody { methods, .. },
                    ..
                } => {
                    self.current_type = Some(NamespacedIdentifier::new_from_parts(
                        &self.module_stack,
                        name.name.clone(),
                        name.span,
                    ));

                    let methods_mine = methods.drain(..).collect::<Vec<_>>();

                    let collapsed = self.collapse_all_functions_with_vis(methods_mine)?;

                    for (visibility, function) in collapsed {
                        methods.push((visibility, function));
                    }

                    self.current_type = None;
                }
                Decl::Impl {
                    type_name, body, ..
                } => {
                    self.current_type = Some(type_name.clone());

                    let methods_mine = body.drain(..).collect::<Vec<_>>();

                    let collapsed = self.collapse_all_functions(methods_mine)?;

                    for function in collapsed {
                        body.push(function);
                    }

                    self.current_type = None;
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn expr_to_type_reference(&mut self, expr: &Expr) -> Option<TypeReference> {
        match expr.kind {
            ExprKind::Literal(Literal::Void) => Some(TypeReference {
                kind: TypeReferenceKind::Void,
                span: Some(expr.span),
            }),
            ExprKind::Literal(Literal::List(ref list)) => {
                if list.len() == 1 {
                    self.expr_to_type_reference(&list[0])
                        .map(|ty| TypeReference {
                            kind: TypeReferenceKind::List(Box::new(ty)),
                            span: Some(expr.span),
                        })
                } else {
                    None
                }
            }
            ExprKind::Variable(ref name) => {
                let mut full_name = name.clone();
                let result = self.look_up_type_body(&mut full_name);

                if result.is_some() {
                    return Some(TypeReference {
                        kind: TypeReferenceKind::Named(full_name),
                        span: Some(expr.span),
                    });
                }

                let mut full_name = name.clone();
                let result = self.look_up_trait_body(&mut full_name);

                if result.is_some() {
                    return Some(TypeReference {
                        kind: TypeReferenceKind::Named(full_name),
                        span: Some(expr.span),
                    });
                }

                let mut full_name = name.clone();
                let result = self.look_up_type_variant(&mut full_name);

                if result.is_some() {
                    return Some(TypeReference {
                        kind: TypeReferenceKind::Named(full_name),
                        span: Some(expr.span),
                    });
                }

                None
            }
            _ => None,
        }
    }

    pub fn check(&mut self) {
        let root = self.root();

        self.validate_function_signatures(root);

        let result = self.collapse_all_function_decls(self.root_vec());

        if let Err((err, span)) = result {
            self.error(err, span);
        }

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
                self.current_type = Some(NamespacedIdentifier::new_from_parts(
                    &self.module_stack,
                    name.name.clone(),
                    name.span,
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
                        } else {
                            field.ty.kind = result.unwrap();
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
            Decl::Trait { .. } => {
                // All trait validation is done in the function signature validation pass.
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
                    return;
                }

                let impl_type_body = self.look_up_type_body(type_name);

                if impl_type_body.is_none() {
                    self.error(
                        ValidatorError::UnknownType(type_name.clone()),
                        Some(type_name.span()),
                    );
                    return;
                }

                let impl_trait_body = impl_trait_body.unwrap();

                let mut seen: Vec<FunctionSignature> = Vec::new();

                self.current_type = Some(type_name.clone());

                for function in body.iter_mut() {
                    if seen
                        .iter()
                        .any(|sig| sig.name.same(&function.signature.name))
                    {
                        self.error(
                            ValidatorError::DuplicateFunction(function.signature.name.name.clone()),
                            Some(function.signature.name.span),
                        );
                    }

                    seen.push(function.signature.clone());

                    let result = self.validate_function(function);

                    if let Err(err) = result {
                        self.error(err.0, err.1);
                    }
                }

                for trait_sig in impl_trait_body.methods.iter() {
                    if !seen.iter().any(|sig| sig.name.same(&trait_sig.name)) {
                        self.error(
                            ValidatorError::MissingTraitMethod(trait_sig.name.name.clone()),
                            Some(trait_sig.name.span),
                        );
                    }
                }

                // Check that the trait methods are implemented correctly, i.e. have the same
                // signatures

                for sig in seen.iter_mut() {
                    let trait_sig = impl_trait_body
                        .methods
                        .iter()
                        .find(|trait_sig| trait_sig.name.same(&sig.name));

                    if let Some(trait_sig) = trait_sig {
                        if sig.parameters.len() != trait_sig.parameters.len()
                            || !sig.parameters.iter().zip(trait_sig.parameters.iter()).all(
                                |(sig_param, trait_param)| {
                                    sig_param.ty.kind.same(&trait_param.ty.kind)
                                        && sig_param.name.same(&trait_param.name)
                                        && sig_param.kind.same(&trait_param.kind)
                                },
                            )
                            || !sig.return_type.kind.same(&trait_sig.return_type.kind)
                        {
                            self.error(
                                ValidatorError::BadTraitMethod(sig.name.name.clone()),
                                Some(sig.name.span),
                            );
                        }
                    }
                }

                self.current_type = None;
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Self::StmtResult {
        match &mut stmt.kind {
            StmtKind::Expr(e) => {
                self.visit_expr(e)?;
            }
            StmtKind::Block(stmts) => {
                self.validate_block(stmts)?;
            }
            StmtKind::Variable { name, ty, value } => {
                let ty_kind = self.visit_type_reference(ty)?;

                let expr_kind = self.visit_expr(value)?;

                let compatible = match ty_kind.is_compatible(&expr_kind, self.root()) {
                    Ok(compatible) => compatible,
                    Err(err) => {
                        return Err((err, Some(value.span)));
                    }
                };

                if !compatible {
                    self.error(
                        ValidatorError::IncompatibleTypes(
                            ty_kind.to_string(),
                            expr_kind.to_string(),
                        ),
                        Some(value.span),
                    );
                }

                if ty_kind.is_infer() {
                    self.set_var_type(
                        name.clone(),
                        TypeReference {
                            kind: expr_kind,
                            span: ty.span,
                        },
                    );
                } else {
                    self.set_var_type(
                        name.clone(),
                        TypeReference {
                            kind: ty_kind,
                            span: ty.span,
                        },
                    );
                }
            }
            StmtKind::Assign { target, value } => {
                let target_kind = match self.visit_expr(target) {
                    Ok(target) => target,
                    Err(err) => {
                        return Err(err);
                    }
                };

                let value_kind = match self.visit_expr(value) {
                    Ok(value) => value,
                    Err(err) => {
                        return Err(err);
                    }
                };

                let compatible = match target_kind.is_compatible(&value_kind, self.root()) {
                    Ok(compatible) => compatible,
                    Err(err) => {
                        return Err((err, Some(value.span)));
                    }
                };

                if !compatible {
                    self.error(
                        ValidatorError::IncompatibleTypes(
                            target_kind.to_string(),
                            value_kind.to_string(),
                        ),
                        Some(value.span),
                    );
                }

                if !matches!(
                    target.kind,
                    ExprKind::Variable(..) | ExprKind::Access { .. }
                ) {
                    self.error(ValidatorError::InvalidAssignment, Some(target.span));
                }
            }
            StmtKind::If {
                condition,
                then,
                otherwise,
            } => {
                let condition_kind = match self.visit_expr(condition) {
                    Ok(condition) => condition,
                    Err(err) => {
                        return Err(err);
                    }
                };

                match condition_kind {
                    TypeReferenceKind::Bool => {}
                    TypeReferenceKind::Union(ref types) => {
                        if !(types.len() == 2 && types.iter().any(|ty| ty.kind.is_void())) {
                            self.error(
                                ValidatorError::BadConditionType(condition_kind.to_string()),
                                Some(condition.span),
                            );
                        }
                    }
                    _ => {
                        self.error(
                            ValidatorError::BadConditionType(condition_kind.to_string()),
                            Some(condition.span),
                        );
                    }
                }

                self.visit_stmt(then)?;

                if let Some(otherwise) = otherwise {
                    self.visit_stmt(otherwise)?;
                }
            }
            StmtKind::While { condition, body } => {
                let condition_kind = match self.visit_expr(condition) {
                    Ok(condition) => condition,
                    Err(err) => {
                        return Err(err);
                    }
                };

                if !matches!(condition_kind, TypeReferenceKind::Bool) {
                    self.error(
                        ValidatorError::BadConditionType(condition_kind.to_string()),
                        Some(condition.span),
                    );
                }

                self.in_loop = true;

                self.visit_stmt(body)?;

                self.in_loop = false;
            }
            StmtKind::For {
                name,
                iterable,
                body,
            } => {
                let iterable_kind = match self.visit_expr(iterable) {
                    Ok(iterable) => iterable,
                    Err(err) => {
                        return Err(err);
                    }
                };

                if let TypeReferenceKind::List(t) = iterable_kind {
                    self.set_var_type(name.clone(), t.as_ref().clone());
                } else {
                    self.error(
                        ValidatorError::BadIterableType(iterable_kind.to_string()),
                        Some(iterable.span),
                    );
                }

                self.in_loop = true;

                self.visit_stmt(body)?;

                self.in_loop = false;
            }
            StmtKind::Match { expr, arms } => match expr {
                Some(ident) => {
                    let var = self.look_up_variable(ident);

                    if var.is_none() {
                        self.error(
                            ValidatorError::UnknownVariable(ident.namespaced()),
                            Some(ident.span),
                        );
                    }

                    let var = var.unwrap();

                    let mut wildcard_encountered = false;

                    for arm in arms {
                        self.begin_scope();

                        if let MatchPattern::Expr(e) = &mut arm.pattern {
                            if let Some(ty) = self.expr_to_type_reference(e) {
                                let compatible = match var.kind.is_compatible(&ty.kind, self.root())
                                {
                                    Ok(compatible) => compatible,
                                    Err(err) => {
                                        self.end_scope();
                                        return Err((err, Some(e.span)));
                                    }
                                };

                                if !compatible {
                                    self.error(
                                        ValidatorError::IncompatibleTypes(
                                            var.kind.to_string(),
                                            ty.kind.to_string(),
                                        ),
                                        Some(e.span),
                                    );
                                }

                                arm.pattern = MatchPattern::Type(ty);
                            }
                        }

                        match &mut arm.pattern {
                            MatchPattern::Expr(e) => {
                                self.visit_expr(e)?;
                            }
                            MatchPattern::Type(t) => {
                                t.kind = self.visit_type_reference(t)?;

                                let compatible = match var.kind.is_compatible(&t.kind, self.root())
                                {
                                    Ok(compatible) => compatible,
                                    Err(err) => {
                                        self.end_scope();
                                        return Err((err, t.span));
                                    }
                                };

                                if !compatible {
                                    self.error(
                                        ValidatorError::IncompatibleTypes(
                                            var.kind.to_string(),
                                            t.kind.to_string(),
                                        ),
                                        t.span,
                                    );
                                }

                                self.set_var_type(
                                    ident.clone(),
                                    TypeReference {
                                        kind: t.kind.clone(),
                                        span: t.span,
                                    },
                                );
                            }
                            MatchPattern::Wildcard => {
                                if wildcard_encountered {
                                    self.error(
                                        ValidatorError::DuplicateWildcard,
                                        Some(arm.body.span),
                                    );
                                } else {
                                    wildcard_encountered = true;
                                }
                            }
                        }

                        self.visit_stmt(&mut arm.body)?;

                        self.end_scope();
                    }
                }
                None => {
                    unimplemented!();
                }
            },
            StmtKind::Return(r) => {
                assert!(self.current_function_return_type.is_some());

                let expr_kind = match self.visit_expr(r) {
                    Ok(expr) => expr,
                    Err(err) => {
                        return Err(err);
                    }
                };

                let return_type = self.current_function_return_type.as_ref().unwrap();

                let compatible = match return_type.is_compatible(&expr_kind, self.root()) {
                    Ok(compatible) => compatible,
                    Err(err) => {
                        return Err((err, Some(r.span)));
                    }
                };

                if !compatible {
                    self.error(
                        ValidatorError::BadReturnType(
                            return_type.to_string(),
                            expr_kind.to_string(),
                        ),
                        Some(r.span),
                    );
                }
            }
            StmtKind::Break => {
                if !self.in_loop {
                    self.error(ValidatorError::BreakOutsideOfLoop, Some(stmt.span));
                }
            }
            StmtKind::Continue => {
                if !self.in_loop {
                    self.error(ValidatorError::ContinueOutsideOfLoop, Some(stmt.span));
                }
            }
        }

        Ok(())
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
                    BinaryOp::Add => {
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

                        if !(left.is_numeric() && right.is_numeric()
                            || left.is_string() && right.is_string())
                        {
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
                    BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
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
                        if !left.same(&right) {
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
                let var = self.look_up_variable(&v.ident);

                if var.is_none() {
                    if v.ident.name == "self" {
                        if self.current_type.is_none() && self.current_trait.is_none() {
                            return Err((
                                ValidatorError::SelfOutsideOfTraitOrType,
                                Some(expr.span),
                            ));
                        }

                        if self.current_type.is_some() {
                            return Ok(TypeReferenceKind::Named(
                                self.current_type.clone().unwrap(),
                            ));
                        }

                        if self.current_trait.is_some() {
                            return Ok(TypeReferenceKind::Named(
                                self.current_trait.clone().unwrap(),
                            ));
                        }
                    }
                    return Err((ValidatorError::UnknownVariable(v.clone()), Some(expr.span)));
                }

                Ok(var.unwrap().kind)
            }
            ExprKind::Access { expr, field } => {
                let expr_type = Self::visit_expr(self, expr)?;

                match expr_type {
                    TypeReferenceKind::Named(name) => {
                        let mut name = name;
                        let type_body = self.look_up_type_body(&mut name);

                        if let Some(type_body) = type_body {
                            let field_type = type_body.field_type(&field.name);

                            if field_type.is_none() {
                                return Err((
                                    ValidatorError::UnknownField(name, field.name.clone()),
                                    Some(expr.span),
                                ));
                            }

                            Ok(field_type.unwrap().kind)
                        } else {
                            let type_variant = self.look_up_type_variant(&mut name);

                            if type_variant.is_none() {
                                return Err((ValidatorError::UnknownType(name), Some(expr.span)));
                            }

                            let field_type = type_variant
                                .unwrap()
                                .fields
                                .iter()
                                .find(|f| f.name.same(field))
                                .map(|f| f.ty.kind.clone());

                            if let Some(field_type) = field_type {
                                Ok(field_type)
                            } else {
                                Err((
                                    ValidatorError::UnknownField(name, field.name.clone()),
                                    Some(expr.span),
                                ))
                            }
                        }
                    }
                    TypeReferenceKind::Union(types) => {
                        let mut types = types;
                        let mut field_types = Vec::new();

                        for type_name in types.iter_mut() {
                            if let TypeReferenceKind::Named(name) = &mut type_name.kind {
                                let type_body = self.look_up_type_body(name);

                                // It's a union, the type must have been validated already

                                let field_type = type_body.unwrap().field_type(&field.name);

                                if field_type.is_none() {
                                    field_types.push(TypeReferenceKind::Void);
                                }

                                field_types.push(field_type.unwrap().kind);
                            } else {
                                return Err((
                                    ValidatorError::FieldlessTypeInUnion,
                                    Some(expr.span),
                                ));
                            }
                        }

                        // deduplication
                        let mut indexes_to_remove = Vec::new();

                        for (i, field_type) in field_types.iter().enumerate() {
                            for (j, other_field_type) in field_types.iter().enumerate() {
                                if i == j {
                                    continue;
                                }

                                if field_type.same(other_field_type) {
                                    indexes_to_remove.push(j);
                                }
                            }
                        }

                        indexes_to_remove.sort();

                        for index in indexes_to_remove.into_iter().rev() {
                            field_types.remove(index);
                        }

                        if field_types.is_empty() {
                            Err((
                                ValidatorError::UnknownFieldInUnion(field.name.clone()),
                                Some(expr.span),
                            ))
                        } else if field_types.len() == 1 {
                            Ok(field_types[0].clone())
                        } else {
                            let field_types = field_types
                                .into_iter()
                                .map(|t| TypeReference {
                                    kind: t,
                                    span: None,
                                })
                                .collect();
                            Ok(TypeReferenceKind::Union(field_types))
                        }
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

                let type_body = self.look_up_type_body(&mut name.up());

                if let Some(type_body) = type_body {
                    let method = type_body
                        .methods
                        .iter()
                        .find(|(_, method)| method.signature.name.same(&name.ident));

                    if method.is_none() {
                        if name.ident.name == "from_str" {
                            dbg!(&type_body);
                        }
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

                let trait_body = self.look_up_trait_body(&mut name.up());

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
                    TypeReferenceKind::Named(mut type_name) => {
                        let type_body = self.look_up_type_body(&mut type_name);

                        if type_body.is_none() {
                            return Err((ValidatorError::UnknownType(type_name), Some(expr.span)));
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

                        let type_ref = TypeReference {
                            kind: TypeReferenceKind::Named(type_name.clone()),
                            span: None,
                        };

                        let mut args_with_self = args.clone();
                        args_with_self.insert(0, (**expr).clone());

                        self.validate_function_call(
                            &method_name.namespaced(),
                            &method.signature,
                            &Some(type_ref.kind),
                            &mut args_with_self,
                        )?;

                        Ok(method.signature.return_type.kind.clone())
                    }
                    TypeReferenceKind::List(ref t) => {
                        let valid_methods = list_method_signatures(t);

                        let method = valid_methods
                            .iter()
                            .find(|method| method.name.same(method_name));

                        if method.is_none() {
                            return Err((
                                ValidatorError::UnknownFunction(
                                    method_name.clone().into_namespaced(),
                                ),
                                Some(expr.span),
                            ));
                        }

                        let method = method.unwrap();

                        let mut args_with_self = args.clone();

                        args_with_self.insert(0, (**expr).clone());

                        self.validate_function_call(
                            &method_name.namespaced(),
                            method,
                            &Some(expr_type.clone()),
                            &mut args_with_self,
                        )?;

                        Ok(method.return_type.kind.clone())
                    }
                    TypeReferenceKind::String => {
                        let valid_methods = string_method_signatures();

                        let method = valid_methods
                            .iter()
                            .find(|method| method.name.same(method_name));

                        if method.is_none() {
                            return Err((
                                ValidatorError::UnknownFunction(
                                    method_name.clone().into_namespaced(),
                                ),
                                Some(expr.span),
                            ));
                        }

                        let method = method.unwrap();

                        let mut args_with_self = args.clone();

                        args_with_self.insert(0, (**expr).clone());

                        self.validate_function_call(
                            &method_name.namespaced(),
                            method,
                            &Some(expr_type.clone()),
                            &mut args_with_self,
                        )?;

                        Ok(method.return_type.kind.clone())
                    }
                    _ => Err((
                        ValidatorError::MethodOnNonType(expr_type.to_string()),
                        Some(span),
                    )),
                }
            }
            ExprKind::Index { expr, index } => {
                let expr_type = Self::visit_expr(self, expr)?;

                match expr_type {
                    TypeReferenceKind::List(ref t) => {
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
                    }
                    TypeReferenceKind::String => {
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

                        Ok(TypeReferenceKind::Char)
                    }
                    _ => Err((
                        ValidatorError::IndexOnNonList(expr_type.to_string()),
                        Some(span),
                    )),
                }
            }
            ExprKind::Constructor { ty, fields } => {
                let type_body = self.look_up_type_body(ty);

                let variant = if let Some(type_body) = type_body {
                    if !(type_body.variants.len() == 1
                        && type_body.variants.first().unwrap().name.name == "self")
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
                        return Err((ValidatorError::UnknownType(ty.clone()), Some(ty.span())));
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
                                    Some(name.span),
                                ));
                            }

                            let type_field =
                                variant.fields.iter().find(|field| field.name.same(name));

                            if type_field.is_none() {
                                return Err((
                                    ValidatorError::UnknownField(ty.clone(), name.name.clone()),
                                    Some(name.span),
                                ));
                            }

                            let type_field = type_field.unwrap();

                            let value_type = Self::visit_expr(self, value)?;

                            let compatible =
                                type_field.ty.kind.is_compatible(&value_type, self.root());

                            if let Err(err) = compatible {
                                return Err((err, Some(value.span)));
                            }

                            if !compatible.unwrap() {
                                return Err((
                                    ValidatorError::ConstructorFieldTypeMismatch(
                                        ty.clone(),
                                        name.name.clone(),
                                        type_field.ty.kind.to_string(),
                                        value_type.to_string(),
                                    ),
                                    Some(value.span),
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
                                    Some(name.span),
                                ));
                            }

                            let variable = self.look_up_variable(name);

                            if variable.is_none() {
                                return Err((
                                    ValidatorError::UnknownVariable(name.namespaced()),
                                    Some(name.span),
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
                                    Some(name.span),
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

                if type_body.is_some() {
                    return Ok(TypeReferenceKind::Named(name.clone()));
                }

                let variant = self.look_up_type_variant(name);

                if variant.is_some() {
                    return Ok(TypeReferenceKind::Named(name.clone()));
                }

                let trait_body = self.look_up_trait_body(name);

                if trait_body.is_some() {
                    return Ok(TypeReferenceKind::Named(name.clone()));
                }

                Err((ValidatorError::UnknownType(name.clone()), Some(name.span())))
            }
            TypeReferenceKind::List(t) => {
                t.kind = self.visit_type_reference(t)?;

                Ok(ty.kind.clone())
            }
            TypeReferenceKind::Union(types) => {
                let mut types_accounted_for = Vec::new();

                for ty in types {
                    ty.kind = self.visit_type_reference(ty)?;

                    if types_accounted_for.contains(&ty) {
                        return Err((
                            ValidatorError::UnionTypeDuplicate(ty.kind.to_string()),
                            ty.span,
                        ));
                    }

                    types_accounted_for.push(ty);
                }

                Ok(ty.kind.clone())
            }
            _ => Ok(ty.kind.clone()),
        }
    }
}
