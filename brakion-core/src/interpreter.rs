use crate::{
    builtin::list_methods,
    errors::{runtime::RuntimeError, ErrorModule},
    repr::{
        look_up_decl, look_up_impl, BinaryOp, BrakionTreeVisitor, Callable, Decl, Expr, ExprKind,
        FieldConstructor, Literal, MatchPattern, NamespaceReference,
        NamespacedIdentifier, Stmt, StmtKind, TypeBinaryOp, TypeReference,
        TypeReferenceKind, UnaryOp,
    },
    unit::Span,
};

use self::value::{Instance, ListInstance, Value};

pub mod value;

pub struct Interpreter<'a> {
    _error_module: ErrorModule,
    root: &'a [Decl],
    execution_scopes: Vec<ExecutionScope<'a>>,
}

struct ExecutionScope<'a> {
    variables: Vec<(String, Value<'a>)>,
}

pub enum ControlFlow<'a> {
    Return(Value<'a>),
    Continue,
    Break,
    None,
}

type ExecutionResult<'a> = Result<ControlFlow<'a>, (RuntimeError, Option<Span>)>;
type EvaluationResult<'a> = Result<Value<'a>, (RuntimeError, Option<Span>)>;

impl<'a> Interpreter<'a> {
    pub fn new(error_module: ErrorModule, decls: &'a Vec<Decl>) -> Self {
        Self {
            _error_module: error_module,
            execution_scopes: Vec::new(),
            root: decls,
        }
    }

    pub fn run(
        &mut self,
        main_function: &NamespacedIdentifier,
        args: &[&str],
    ) -> Result<Value<'a>, (RuntimeError, Option<Span>)> {
        let main_function = look_up_decl(self.root, main_function);

        if let Some(NamespaceReference::Decl(Decl::Function { function, .. })) = main_function {
            let main_args = ListInstance::new(TypeReferenceKind::String);
            for arg in args {
                main_args.push(Value::String(arg.to_string()));
            }

            self.call_function(function, &[Value::List(main_args)])
        } else {
            panic!("Main function not found"); // should be unreachable
        }
    }

    pub fn root(&self) -> &'a [Decl] {
        self.root
    }

    pub(crate) fn begin_scope(&mut self) {
        self.execution_scopes.push(ExecutionScope {
            variables: Vec::new(),
        });
    }

    pub(crate) fn end_scope(&mut self) {
        self.execution_scopes.pop();
    }

    pub(crate) fn declare_variable(&mut self, name: &str, value: Value<'a>) {
        self.execution_scopes
            .last_mut()
            .unwrap()
            .variables
            .push((name.to_string(), value));
    }

    fn set_variable(&mut self, name: &str, value: Value<'a>) {
        for scope in self.execution_scopes.iter_mut().rev() {
            for (var_name, var_value) in scope.variables.iter_mut().rev() {
                if var_name == name {
                    *var_value = value;
                    return;
                }
            }
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<Value<'a>> {
        for scope in self.execution_scopes.iter().rev() {
            for (var_name, value) in scope.variables.iter().rev() {
                if var_name == name {
                    return Some(value.clone());
                }
            }
        }

        None
    }

    fn lookup_function(&self, name: &NamespacedIdentifier) -> Option<&'a dyn Callable> {
        let decl = look_up_decl(self.root, name);

        if let Some(NamespaceReference::Decl(Decl::Function { function, .. })) = decl {
            return Some(function);
        }

        if let Some(NamespaceReference::TypeMethod(function)) = decl {
            return Some(function);
        }

        if let Some(NamespaceReference::Decl(Decl::NativeFunction(native_function))) = decl {
            return Some(native_function);
        }

        None
    }

    fn lookup_trait_method_impl(
        &self,
        name: &str,
        type_name: &NamespacedIdentifier,
        trait_name: &NamespacedIdentifier,
    ) -> Option<&'a dyn Callable> {
        let type_decl = look_up_decl(self.root, type_name).unwrap();
        let type_name = match type_decl {
            NamespaceReference::Decl(Decl::Type { .. }) => type_name.clone(),
            NamespaceReference::TypeVariant(_) => type_name.up(),
            _ => panic!()
        };

        let decl = look_up_impl(self.root, trait_name, &type_name);

        if let Some(body) = decl {
            for method in body.iter() {
                if method.signature.name.name == name {
                    return Some(method);
                }
            }
        }

        None
    }

    fn call_function(
        &mut self,
        function: &dyn Callable,
        args: &[Value<'a>],
    ) -> EvaluationResult<'a> {
        function.call(self, args)
    }

    pub(crate) fn execute_block(&mut self, block: &[Stmt]) -> ExecutionResult<'a> {
        self.begin_scope();

        let mut result = Ok(ControlFlow::None);
        for stmt in block {
            result = self.visit_stmt(stmt);

            match result {
                Ok(ControlFlow::Return(_)) => break,
                Ok(ControlFlow::Continue) => break,
                Ok(ControlFlow::Break) => break,
                Err(err) => {
                    self.end_scope();

                    return Err(err);
                }
                _ => {}
            }
        }

        self.end_scope();

        result
    }
}

impl<'a> BrakionTreeVisitor for Interpreter<'a> {
    type ExprResult = EvaluationResult<'a>;
    type StmtResult = ExecutionResult<'a>;
    type DeclResult = ();
    type TypeReferenceResult = ();

    fn visit_decl(&mut self, _: &Decl) -> Self::DeclResult {}

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::StmtResult {
        match &stmt.kind {
            StmtKind::Expr(e) => {
                self.visit_expr(e)?;

                Ok(ControlFlow::None)
            }
            StmtKind::Block(b) => self.execute_block(b),
            StmtKind::Variable { name, value, .. } => {
                let value = self.visit_expr(value)?;

                self.declare_variable(&name.name, value);

                Ok(ControlFlow::None)
            }
            StmtKind::Assign { target, value } => {
                let value = self.visit_expr(value)?;

                match &target.kind {
                    ExprKind::Variable(name) => {
                        self.set_variable(&name.ident.name, value);
                    }
                    ExprKind::Access { expr, field } => {
                        let target = self.visit_expr(expr)?;

                        if let Value::Instance(instance) = target {
                            instance.set_field(&field.name, value);
                        } else {
                            panic!("Cannot assign to non-instance");
                        }
                    }
                    _ => todo!(),
                }

                Ok(ControlFlow::None)
            }
            StmtKind::If {
                condition,
                then,
                otherwise,
            } => {
                let condition = self.visit_expr(condition)?;

                if condition.truthy() {
                    self.visit_stmt(then)
                } else if let Some(otherwise) = otherwise {
                    self.visit_stmt(otherwise)
                } else {
                    Ok(ControlFlow::None)
                }
            }
            StmtKind::While { condition, body } => {
                let mut result = Ok(ControlFlow::None);

                while self.visit_expr(condition)?.truthy() {
                    result = self.visit_stmt(body);

                    match result {
                        Ok(ControlFlow::Return(_)) => break,
                        Ok(ControlFlow::Continue) => continue,
                        Ok(ControlFlow::Break) => break,
                        Err(err) => return Err(err),
                        _ => {}
                    }
                }

                match result {
                    Ok(ControlFlow::Return(_)) => result,
                    Ok(ControlFlow::Continue) => Ok(ControlFlow::None),
                    Ok(ControlFlow::Break) => Ok(ControlFlow::None),
                    Err(err) => Err(err),
                    _ => Ok(ControlFlow::None),
                }
            }
            StmtKind::For {
                name,
                iterable,
                body,
            } => {
                let iterable = self.visit_expr(iterable)?;

                match iterable {
                    Value::List(list) => {
                        let mut result = Ok(ControlFlow::None);

                        for value in list.get_elements().iter() {
                            self.begin_scope();
                            self.declare_variable(&name.name, value.clone());

                            result = self.visit_stmt(body);

                            self.end_scope();

                            match result {
                                Ok(ControlFlow::Return(_)) => break,
                                Ok(ControlFlow::Continue) => continue,
                                Ok(ControlFlow::Break) => break,
                                Err(err) => return Err(err),
                                _ => {}
                            }
                        }

                        match result {
                            Ok(ControlFlow::Return(_)) => result,
                            Ok(ControlFlow::Continue) => Ok(ControlFlow::None),
                            Ok(ControlFlow::Break) => Ok(ControlFlow::None),
                            Err(err) => Err(err),
                            _ => Ok(ControlFlow::None),
                        }
                    }
                    _ => panic!("Cannot iterate over non-list"),
                }
            }
            StmtKind::Match { expr, arms } => {
                let mut should_run = Vec::new();
                let mut wildcard_index = None;
                if let Some(e) = expr {
                    let variable = self.lookup_variable(&e.name).unwrap();

                    for arm in arms {
                        match &arm.pattern {
                            MatchPattern::Expr(e) => {
                                let value = self.visit_expr(e)?;

                                if value == variable {
                                    should_run.push(true);
                                } else if let Value::Bool(b) = value {
                                    should_run.push(b);
                                } else {
                                    should_run.push(false);
                                }
                            }
                            MatchPattern::Type(t) => {
                                if variable.is_type(&t.kind, self.root()) {
                                    should_run.push(true);
                                } else {
                                    should_run.push(false);
                                }
                            }
                            MatchPattern::Wildcard => {
                                wildcard_index = Some(should_run.len());
                                should_run.push(false);
                            }
                        }
                    }
                } else {
                    for arm in arms {
                        match &arm.pattern {
                            MatchPattern::Expr(e) => {
                                let value = self.visit_expr(e)?;

                                should_run.push(value.truthy());
                            }
                            MatchPattern::Type(_) => panic!("Cannot match type without expression"),
                            MatchPattern::Wildcard => {
                                wildcard_index = Some(should_run.len());
                                should_run.push(false);
                            }
                        }
                    }
                }

                if let Some(wi) = wildcard_index {
                    if should_run.iter().all(|b| !b) {
                        should_run[wi] = true;
                    }
                }

                let mut result = Ok(ControlFlow::None);

                for (i, arm) in arms.iter().enumerate() {
                    if should_run[i] {
                        self.begin_scope();

                        result = self.visit_stmt(&arm.body);

                        self.end_scope();

                        match result {
                            Ok(ControlFlow::None) => {}
                            _ => break,
                        }
                    }
                }

                result
            }
            StmtKind::Return(e) => {
                let value = self.visit_expr(e)?;

                Ok(ControlFlow::Return(value))
            }
            StmtKind::Break => Ok(ControlFlow::Break),
            StmtKind::Continue => Ok(ControlFlow::Continue),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult {
        let expr_span = expr.span;
        match &expr.kind {
            ExprKind::Literal(l) => match l {
                Literal::Int(i) => Ok(Value::U64(*i)),
                Literal::Float(f) => Ok(Value::F64(*f)),
                Literal::String(s) => Ok(Value::String(s.clone())),
                Literal::Char(c) => Ok(Value::Char(*c)),
                Literal::Bool(b) => Ok(Value::Bool(*b)),
                Literal::List(l) => {
                    let list = ListInstance::new(crate::repr::TypeReferenceKind::Infer);

                    for element in l {
                        list.push(self.visit_expr(element)?);
                    }

                    Ok(Value::List(list))
                }
                Literal::Void => Ok(Value::Void),
            },
            ExprKind::Unary { op, expr } => {
                let expr = self.visit_expr(expr)?;

                match op {
                    UnaryOp::Neg => {
                        if expr.is_numeric() {
                            Ok(-expr)
                        } else {
                            Err((RuntimeError::InvalidUnary(op.clone()), Some(expr_span)))
                        }
                    }
                    UnaryOp::Not => {
                        if matches!(expr, Value::Bool(_)) {
                            Ok(!expr)
                        } else {
                            Err((RuntimeError::InvalidUnary(op.clone()), Some(expr_span)))
                        }
                    }
                }
            }
            ExprKind::Binary { op, left, right } => {
                let left = self.visit_expr(left)?;

                match op {
                    BinaryOp::Add => {
                        let right = self.visit_expr(right)?;
                        if left.is_numeric() && right.is_numeric() {
                            let cast_right =
                                right.cast(&left.get_type_reference(), self.root()).unwrap();
                            Ok(left + cast_right)
                        } else if (left.is_string() && right.is_string())
                            || (left.is_string() && right.is_char())
                            || (left.is_char() && right.is_string())
                        {
                            Ok(Value::String(format!("{}{}", left, right)))
                        } else {
                            Err((RuntimeError::InvalidBinary(op.clone()), Some(expr_span)))
                        }
                    }
                    BinaryOp::Sub => {
                        let right = self.visit_expr(right)?;
                        if left.is_numeric() && right.is_numeric() {
                            let cast_right =
                                right.cast(&left.get_type_reference(), self.root()).unwrap();
                            Ok(left - cast_right)
                        } else {
                            Err((RuntimeError::InvalidBinary(op.clone()), Some(expr_span)))
                        }
                    }
                    BinaryOp::Mul => {
                        let right = self.visit_expr(right)?;
                        if left.is_numeric() && right.is_numeric() {
                            Ok(left * right)
                        } else {
                            Err((RuntimeError::InvalidBinary(op.clone()), Some(expr_span)))
                        }
                    }
                    BinaryOp::Div => {
                        let right = self.visit_expr(right)?;
                        if left.is_numeric() && right.is_numeric() {
                            if right.is_zero() {
                                Err((RuntimeError::DivisionByZero, Some(expr_span)))
                            } else {
                                Ok(left / right)
                            }
                        } else {
                            Err((RuntimeError::InvalidBinary(op.clone()), Some(expr_span)))
                        }
                    }
                    BinaryOp::And => {
                        if left.is_bool() {
                            if !left.truthy() {
                                Ok(Value::Bool(false))
                            } else {
                                let right = self.visit_expr(right)?;
                                if right.is_bool() {
                                    Ok(Value::Bool(right.truthy()))
                                } else {
                                    Err((RuntimeError::InvalidBinary(op.clone()), Some(expr_span)))
                                }
                            }
                        } else {
                            Err((RuntimeError::InvalidBinary(op.clone()), Some(expr_span)))
                        }
                    }
                    BinaryOp::Or => {
                        if left.is_bool() {
                            if left.truthy() {
                                Ok(left)
                            } else {
                                let right = self.visit_expr(right)?;
                                if right.is_bool() {
                                    Ok(right)
                                } else {
                                    Err((RuntimeError::InvalidBinary(op.clone()), Some(expr_span)))
                                }
                            }
                        } else {
                            Err((RuntimeError::InvalidBinary(op.clone()), Some(expr_span)))
                        }
                    }
                    BinaryOp::Eq => {
                        let right = self.visit_expr(right)?;

                        Ok(Value::Bool(left == right))
                    }
                    BinaryOp::Neq => {
                        let right = self.visit_expr(right)?;
                        Ok(Value::Bool(left != right))
                    }
                    BinaryOp::Lt => {
                        let right = self.visit_expr(right)?;
                        if left.is_numeric() && right.is_numeric() {
                            Ok(Value::Bool(left < right))
                        } else {
                            Err((RuntimeError::InvalidBinary(op.clone()), Some(expr_span)))
                        }
                    }
                    BinaryOp::Gt => {
                        let right = self.visit_expr(right)?;
                        if left.is_numeric() && right.is_numeric() {
                            Ok(Value::Bool(left > right))
                        } else {
                            Err((RuntimeError::InvalidBinary(op.clone()), Some(expr_span)))
                        }
                    }
                    BinaryOp::Leq => {
                        let right = self.visit_expr(right)?;
                        if left.is_numeric() && right.is_numeric() {
                            Ok(Value::Bool(left <= right))
                        } else {
                            Err((RuntimeError::InvalidBinary(op.clone()), Some(expr_span)))
                        }
                    }
                    BinaryOp::Geq => {
                        let right = self.visit_expr(right)?;
                        if left.is_numeric() && right.is_numeric() {
                            Ok(Value::Bool(left >= right))
                        } else {
                            Err((RuntimeError::InvalidBinary(op.clone()), Some(expr_span)))
                        }
                    }
                }
            }
            ExprKind::TypeBinary { op, expr, ty } => {
                let expr = self.visit_expr(expr)?;

                match op {
                    TypeBinaryOp::Is => Ok(Value::Bool(expr.is_type(&ty.kind, self.root))),
                    TypeBinaryOp::As => {
                        let cast = expr.cast(&ty.kind, self.root);

                        if let Some(cast) = cast {
                            Ok(cast)
                        } else {
                            Err((
                                RuntimeError::InvalidCast(
                                    expr.type_to_string(),
                                    ty.kind.to_string(),
                                ),
                                Some(expr_span),
                            ))
                        }
                    }
                }
            }
            ExprKind::Variable(v) => {
                if let Some(value) = self.lookup_variable(v.ident.name.as_str()) {
                    Ok(value.clone())
                } else {
                    Err((
                        RuntimeError::UndefinedVariable(v.ident.name.clone()),
                        Some(expr_span),
                    ))
                }
            }
            ExprKind::Access { expr, field } => {
                let expr = self.visit_expr(expr)?;

                if let Value::Instance(instance) = expr {
                    if let Some(value) = instance.get_field(field.name.as_str()) {
                        Ok(value)
                    } else {
                        panic!("Field {} not found", field.name); // should be unreachable
                    }
                } else {
                    // should be unreachable
                    panic!(
                        "Cannot access field {} of non-instance value {}",
                        field.name,
                        expr.type_to_string()
                    );
                }
            }
            ExprKind::FunctionCall { name, args } => {
                let function = self.lookup_function(name);

                if let Some(function) = function {
                    let args = args
                        .iter()
                        .map(|arg| self.visit_expr(arg))
                        .collect::<Result<Vec<_>, _>>()?;

                    self.call_function(function, &args)
                } else if !args.is_empty() {
                    let arg = self.visit_expr(&args[0])?;

                    if let Value::Instance(i) = arg {
                        if let Some(func) = self.lookup_trait_method_impl(
                            &name.ident.name,
                            if let TypeReferenceKind::Named(name) = &i.get_type_reference() {
                                name
                            } else {
                                panic!("Expected named type reference")
                            },
                            &name.up(),
                        ) {
                            let args = args
                                .iter()
                                .map(|arg| self.visit_expr(arg))
                                .collect::<Result<Vec<_>, _>>()?;

                            self.call_function(func, &args)
                        } else {
                            panic!("Function {} not found", name); // should be unreachable
                        }
                    } else {
                        panic!("Function {} not found", name); // should be unreachable
                    }
                } else {
                    panic!("Function {} not found", name); // should be unreachable
                }
            }
            ExprKind::MethodCall { expr, method, args } => {
                let expr = self.visit_expr(expr)?;

                match expr {
                    Value::Instance(i) => {
                        if let Some(func) = i.lookup_method(method.name.as_str()) {
                            let mut full_args = vec![Value::Instance(i)];
                            full_args.extend(
                                args.iter()
                                    .map(|arg| self.visit_expr(arg))
                                    .collect::<Result<Vec<_>, _>>()?,
                            );

                            self.call_function(func, &full_args)
                        } else {
                            panic!("Method {} not found", method.name); // should be unreachable
                        }
                    }
                    Value::List(l) => {
                        let list_methods = list_methods(&TypeReference {
                            span: None,
                            kind: l.get_type_reference(),
                        });

                        if let Some(func) = list_methods
                            .iter()
                            .find(|f| f.signature.name.name == method.name)
                        {
                            let mut full_args = vec![Value::List(l)];
                            full_args.extend(
                                args.iter()
                                    .map(|arg| self.visit_expr(arg))
                                    .collect::<Result<Vec<_>, _>>()?,
                            );

                            self.call_function(func, &full_args)
                        } else {
                            panic!("Method {} not found", method.name); // should be unreachable
                        }
                    }
                    _ => panic!(
                        "Cannot call method on non-instance value {}",
                        expr.type_to_string()
                    ), // should be unreachable
                }
            }
            ExprKind::Index { expr, index } => {
                let expr = self.visit_expr(expr)?;
                let index_span = index.span;
                let index = self.visit_expr(index)?;

                if let Some(i) = index.get_int_as_usize() {
                    match expr {
                        Value::List(list) => {
                            if let Some(value) = list.get(i) {
                                Ok(value)
                            } else {
                                Err((RuntimeError::IndexOutOfBounds(i), Some(index_span)))
                            }
                        }
                        Value::String(s) => {
                            if let Some(value) = s.chars().nth(i) {
                                Ok(Value::Char(value))
                            } else {
                                Err((RuntimeError::IndexOutOfBounds(i), Some(index_span)))
                            }
                        }
                        _ => {
                            // should be unreachable
                            panic!("Cannot index non-list value {}", expr.type_to_string());
                        }
                    }
                } else {
                    // should be unreachable
                    panic!("Index {} is not an integer", index.type_to_string());
                }
            }
            ExprKind::Constructor { ty, fields } => {
                let ty_decl = match look_up_decl(self.root, ty) {
                    Some(decl) => decl,
                    None => panic!("Type {} not found", ty),
                };

                let ty_ref_kind = TypeReferenceKind::Named(ty.clone());

                let instance = match ty_decl {
                    NamespaceReference::Decl(Decl::Type { body, .. }) => {
                        if !body.variants.iter().any(|v| v.name.name == "self") {
                            panic!("Type {} does not have a self variant", ty);
                        }

                        let variant = body
                            .variants
                            .iter()
                            .find(|v| v.name.name == "self")
                            .unwrap();

                        Instance::new(ty_ref_kind, body, variant)
                    }
                    NamespaceReference::TypeVariant(variant) => {
                        if let NamespaceReference::Decl(Decl::Type { body, .. }) =
                            look_up_decl(self.root, &ty.up()).unwrap()
                        {
                            Instance::new(ty_ref_kind, body, variant)
                        } else {
                            panic!("Type's parent is not a type")
                        }
                    }
                    _ => panic!("Cannot construct non-type value {}", ty),
                };

                for field_constructor in fields {
                    match field_constructor {
                        FieldConstructor::Named { name, value } => {
                            let value = self.visit_expr(value)?;

                            instance.set_field(name.name.as_str(), value);
                        }
                        FieldConstructor::Auto(name) => {
                            let value = self
                                .lookup_variable(name.name.as_str())
                                .expect("Variable not found")
                                .clone();

                            instance.set_field(name.name.as_str(), value);
                        }
                    }
                }

                Ok(Value::Instance(instance))
            }
        }
    }

    fn visit_type_reference(&mut self, _: &TypeReference) -> Self::TypeReferenceResult {}
}
