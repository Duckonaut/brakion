use crate::{
    errors::{runtime::RuntimeError, validator::ValidatorError, ErrorModule},
    repr::{
        BinaryOp, BrakionTreeVisitor, Decl, Expr, ExprKind, Identifier, Literal, Stmt,
        TypeBinaryOp, TypeReference, UnaryOp,
    },
    unit::Span,
};

use self::value::{ListInstance, Value};

mod value;

pub struct Interpreter<'a> {
    error_module: ErrorModule,
    root: &'a [Decl],
    execution_scopes: Vec<ExecutionScope>,
}

struct ExecutionScope {
    variables: Vec<(String, Value)>,
}

pub enum ControlFlow {
    Return(Value),
    Continue,
    Break,
}

type ExecutionResult = Result<ControlFlow, RuntimeError>;
type EvaluationResult = Result<Value, RuntimeError>;

impl<'a> Interpreter<'a> {
    pub fn new(error_module: ErrorModule, decls: &'a Vec<Decl>) -> Self {
        Self {
            error_module,
            execution_scopes: Vec::new(),
            root: decls,
        }
    }

    fn error(&mut self, err: ValidatorError, span: Option<Span>) {
        self.error_module.add_validator_error(err, span);
    }

    fn begin_scope(&mut self) {
        self.execution_scopes.push(ExecutionScope {
            variables: Vec::new(),
        });
    }

    fn end_scope(&mut self) {
        self.execution_scopes.pop();
    }

    fn lookup_variable(&self, name: &str) -> Option<&Value> {
        for scope in self.execution_scopes.iter().rev() {
            for (var_name, value) in scope.variables.iter().rev() {
                if var_name == name {
                    return Some(value);
                }
            }
        }

        None
    }
}

impl<'a> BrakionTreeVisitor for Interpreter<'a> {
    type ExprResult = EvaluationResult;
    type StmtResult = ExecutionResult;
    type DeclResult = ();
    type TypeReferenceResult = ();

    fn visit_decl(&mut self, _: &Decl) -> Self::DeclResult {}

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult {
        match &expr.kind {
            ExprKind::Literal(l) => match l {
                Literal::Int(i) => Ok(Value::U64(*i)),
                Literal::Float(f) => Ok(Value::F64(*f)),
                Literal::String(s) => Ok(Value::String(s.clone())),
                Literal::Char(c) => Ok(Value::Char(*c)),
                Literal::Bool(b) => Ok(Value::Bool(*b)),
                Literal::List(l) => {
                    let list = ListInstance::new(crate::repr::TypeReferenceKind::Integer(
                        crate::repr::IntSize::I32,
                        true,
                    ));

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
                            Err(RuntimeError::InvalidUnary(op.clone()))
                        }
                    }
                    UnaryOp::Not => {
                        if matches!(expr, Value::Bool(_)) {
                            Ok(!expr)
                        } else {
                            Err(RuntimeError::InvalidUnary(op.clone()))
                        }
                    }
                }
            }
            ExprKind::Binary { op, left, right } => {
                let left = self.visit_expr(left)?;
                let right = self.visit_expr(right)?;

                match op {
                    BinaryOp::Add => {
                        if left.is_numeric() && right.is_numeric() {
                            Ok(left + right)
                        } else if (left.is_string() && right.is_string())
                            || (left.is_string() && right.is_char())
                            || (left.is_char() && right.is_string())
                        {
                            Ok(Value::String(format!("{}{}", left, right)))
                        } else {
                            Err(RuntimeError::InvalidBinary(op.clone()))
                        }
                    }
                    BinaryOp::Sub => {
                        if left.is_numeric() && right.is_numeric() {
                            Ok(left - right)
                        } else {
                            Err(RuntimeError::InvalidBinary(op.clone()))
                        }
                    }
                    BinaryOp::Mul => {
                        if left.is_numeric() && right.is_numeric() {
                            Ok(left * right)
                        } else {
                            Err(RuntimeError::InvalidBinary(op.clone()))
                        }
                    }
                    BinaryOp::Div => {
                        if left.is_numeric() && right.is_numeric() {
                            Ok(left / right)
                        } else {
                            Err(RuntimeError::InvalidBinary(op.clone()))
                        }
                    }
                    BinaryOp::And => {
                        if left.is_bool() && right.is_bool() {
                            Ok(Value::Bool(left.truthy() && right.truthy()))
                        } else {
                            Err(RuntimeError::InvalidBinary(op.clone()))
                        }
                    }
                    BinaryOp::Or => {
                        if left.is_bool() && right.is_bool() {
                            Ok(Value::Bool(left.truthy() || right.truthy()))
                        } else {
                            Err(RuntimeError::InvalidBinary(op.clone()))
                        }
                    }
                    BinaryOp::Eq => Ok(Value::Bool(left == right)),
                    BinaryOp::Neq => Ok(Value::Bool(left != right)),
                    BinaryOp::Lt => {
                        if left.is_numeric() && right.is_numeric() {
                            Ok(Value::Bool(left < right))
                        } else {
                            Err(RuntimeError::InvalidBinary(op.clone()))
                        }
                    }
                    BinaryOp::Gt => {
                        if left.is_numeric() && right.is_numeric() {
                            Ok(Value::Bool(left > right))
                        } else {
                            Err(RuntimeError::InvalidBinary(op.clone()))
                        }
                    }
                    BinaryOp::Leq => {
                        if left.is_numeric() && right.is_numeric() {
                            Ok(Value::Bool(left <= right))
                        } else {
                            Err(RuntimeError::InvalidBinary(op.clone()))
                        }
                    }
                    BinaryOp::Geq => {
                        if left.is_numeric() && right.is_numeric() {
                            Ok(Value::Bool(left >= right))
                        } else {
                            Err(RuntimeError::InvalidBinary(op.clone()))
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
                            Err(RuntimeError::InvalidCast(
                                expr.type_to_string(),
                                ty.kind.to_string(),
                            ))
                        }
                    }
                }
            }
            ExprKind::Variable(v) => {
                if let Some(value) = self.lookup_variable(v.ident.name.as_str()) {
                    Ok(value.clone())
                } else {
                    Err(RuntimeError::UndefinedVariable(v.ident.name.clone()))
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
            ExprKind::FunctionCall { name, args } => todo!(),
            ExprKind::MethodCall { expr, method, args } => todo!(),
            ExprKind::Index { expr, index } => {
                let expr = self.visit_expr(expr)?;
                let index = self.visit_expr(index)?;

                if let Value::List(list) = expr {
                    if let Some(i) = index.get_int_as_usize() {
                        if let Some(value) = list.get(i) {
                            Ok(value)
                        } else {
                            Err(RuntimeError::IndexOutOfBounds(i))
                        }
                    } else {
                        // should be unreachable
                        panic!("Index {} is not an integer", index.type_to_string());
                    }
                } else {
                    // should be unreachable
                    panic!(
                        "Cannot index non-list value {}",
                        expr.type_to_string()
                    );
                }
            },
            ExprKind::Constructor { ty, fields } => todo!(),
        }
    }

    fn visit_type_reference(&mut self, ty: &TypeReference) -> Self::TypeReferenceResult {}
}
