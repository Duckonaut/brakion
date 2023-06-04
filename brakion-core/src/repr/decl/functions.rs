use crate::{
    errors::runtime::RuntimeError,
    interpreter::{value::Value, ControlFlow, Interpreter}, unit::Span,
};

use super::{Identifier, Stmt, TypeReference};

#[derive(Debug, Hash, PartialEq, Clone)]
pub struct FunctionSignature {
    pub name: Identifier,
    pub takes_self: bool,
    pub self_precondition: Option<TypeReference>,
    pub parameters: Vec<Parameter>,
    pub return_type: TypeReference,
}

#[derive(Debug, Hash, PartialEq)]
pub struct Function {
    pub signature: FunctionSignature,
    pub body: Vec<Stmt>,
}

pub struct NativeFunction {
    pub signature: FunctionSignature,
    pub(crate) body: Box<dyn CallableBody>,
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunction")
            .field("signature", &self.signature)
            .field("body", &"...")
            .finish()
    }
}

impl std::hash::Hash for NativeFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.signature.hash(state);
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.signature == other.signature
    }
}

pub(crate) trait Callable {
    fn call<'a>(
        &self,
        interpreter: &mut Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, (RuntimeError, Option<Span>)>;

    fn arity(&self) -> usize;
}

impl Callable for Function {
    fn call<'a>(
        &self,
        interpreter: &mut Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, (RuntimeError, Option<Span>)> {
        let expected_arg_count = self.arity();

        if args.len() != expected_arg_count {
            panic!(
                "Expected {} arguments, got {}",
                expected_arg_count,
                args.len()
            ); // should be unreachable
        }

        interpreter.begin_scope();

        for (i, arg) in args.iter().enumerate() {
            if self.signature.takes_self {
                if i == 0 {
                    interpreter.declare_variable("self", arg.clone());
                    continue;
                } else {
                    interpreter.declare_variable(
                        &self.signature.parameters[i - 1].name.name,
                        arg.cast(
                            &self.signature.parameters[i - 1].ty.kind,
                            interpreter.root(),
                        )
                        .unwrap(),
                    );
                }
            } else {
                let param = &self.signature.parameters[i];

                interpreter.declare_variable(
                    &param.name.name,
                    arg.cast(&param.ty.kind, interpreter.root()).unwrap(),
                );
            }
        }

        let result = interpreter.execute_block(&self.body);

        interpreter.end_scope();

        match result {
            Ok(ControlFlow::Return(value)) => Ok(
                if let Some(value) =
                    value.cast(&self.signature.return_type.kind, interpreter.root())
                {
                    value
                } else {
                    panic!("Invalid return type") // should be unreachable
                },
            ),
            Ok(ControlFlow::Continue) => panic!("Continue outside loop"), // should be unreachable
            Ok(ControlFlow::Break) => panic!("Break outside loop"),       // should be unreachable
            Ok(ControlFlow::None) => panic!("Function did not return"),   // should be unreachable
            Err(err) => Err(err),
        }
    }

    fn arity(&self) -> usize {
        if self.signature.takes_self {
            self.signature.parameters.len() + 1
        } else {
            self.signature.parameters.len()
        }
    }
}

pub(crate) trait CallableBody {
    fn call<'a>(
        &self,
        interpreter: &mut Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError>;
}

impl<T> CallableBody for T
where
    for<'a> T: Fn(&mut Interpreter<'a>, &[Value<'a>]) -> Result<Value<'a>, RuntimeError>,
{
    fn call<'a>(
        &self,
        interpreter: &mut Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        self(interpreter, args)
    }
}

impl Callable for NativeFunction {
    fn call<'a>(
        &self,
        interpreter: &mut Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, (RuntimeError, Option<Span>)> {
        let mut cast_args =
            Vec::with_capacity(args.len() + if self.signature.takes_self { 1 } else { 0 });

        for (i, arg) in args.iter().enumerate() {
            if self.signature.takes_self {
                if i == 0 {
                    cast_args.push(arg.clone());
                    continue;
                } else {
                    cast_args.push(
                        arg.cast(
                            &self.signature.parameters[i - 1].ty.kind,
                            interpreter.root(),
                        )
                        .unwrap(),
                    );
                }
            } else {
                let param = &self.signature.parameters[i];

                cast_args.push(arg.cast(&param.ty.kind, interpreter.root()).unwrap());
            }
        }
        let result = self.body.call(interpreter, &cast_args);

        match result {
            Ok(v) => Ok(v),
            Err(err) => Err((err, None)),
        }
    }

    fn arity(&self) -> usize {
        self.signature.parameters.len()
    }
}

#[derive(Debug, Hash, PartialEq, Clone)]
pub struct Parameter {
    pub name: Identifier,
    pub ty: TypeReference,
    pub kind: ParameterSpec,
}

#[derive(Debug, Hash, PartialEq, Clone)]
pub enum ParameterSpec {
    Basic,
    Preconditioned(TypeReference),
}

impl ParameterSpec {
    pub fn same(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Basic, Self::Basic) => true,
            (Self::Preconditioned(ty1), Self::Preconditioned(ty2)) => ty1.kind.same(&ty2.kind),
            _ => false,
        }
    }
}
