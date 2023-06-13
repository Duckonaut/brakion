use crate::{
    errors::runtime::RuntimeError,
    interpreter::value::Value,
    repr::{
        Decl, FunctionSignature, Identifier, IntSize, NativeFunction, Parameter, ParameterSpec,
        TraitBody, TypeReference, TypeReferenceKind, Visibility,
    },
    unit::Span,
};

macro_rules! integer_decls {
    ($name:tt, $size:path, $signed:expr) => {
        Decl::Module {
            visibility: Visibility::Public,
            name: Identifier::new(Span::default(), stringify!($name)),
            body: vec![
                Decl::NativeFunction(NativeFunction {
                    signature: FunctionSignature {
                        name: Identifier::new(Span::default(), "to_str"),
                        takes_self: false,
                        self_precondition: None,
                        parameters: vec![Parameter {
                            name: Identifier::new(Span::default(), "value"),
                            ty: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::Integer($size, $signed),
                            },
                            kind: ParameterSpec::Basic,
                        }],
                        return_type: TypeReference {
                            span: None,
                            kind: TypeReferenceKind::String,
                        },
                    },
                    body: Box::new($name::to_str),
                }),
                Decl::NativeFunction(NativeFunction {
                    signature: FunctionSignature {
                        name: Identifier::new(Span::default(), "parse"),
                        takes_self: false,
                        self_precondition: None,
                        parameters: vec![Parameter {
                            name: Identifier::new(Span::default(), "value"),
                            ty: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::String,
                            },
                            kind: ParameterSpec::Basic,
                        }],
                        return_type: TypeReference {
                            span: None,
                            kind: TypeReferenceKind::Integer($size, $signed),
                        },
                    },
                    body: Box::new($name::parse),
                }),
            ],
        }
    };
}

macro_rules! integer_impls {
    ($name:tt, $size:path) => {
        mod $name {
            use super::*;

            pub fn to_str<'a>(
                _: &mut crate::interpreter::Interpreter<'a>,
                args: &[Value<'a>],
            ) -> Result<Value<'a>, RuntimeError> {
                if let $size(i) = &args[0] {
                    Ok(Value::String(i.to_string()))
                } else {
                    panic!("to_str() called with non-integer argument");
                }
            }

            pub fn parse<'a>(
                _: &mut crate::interpreter::Interpreter<'a>,
                args: &[Value<'a>],
            ) -> Result<Value<'a>, RuntimeError> {
                if let Value::String(s) = &args[0] {
                    Ok($size(s.parse::<$name>().unwrap().try_into().unwrap()))
                } else {
                    panic!("parse() called with non-string argument");
                }
            }
        }
    };
}

pub fn builtin_decls() -> Vec<Decl> {
    vec![Decl::Module {
        visibility: Visibility::Public,
        name: Identifier::new(Span::default(), "std"),
        body: vec![
            Decl::Module {
                visibility: Visibility::Public,
                name: Identifier::new(Span::default(), "io"),
                body: vec![
                    Decl::NativeFunction(NativeFunction {
                        signature: FunctionSignature {
                            name: Identifier::new(Span::default(), "print"),
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![Parameter {
                                name: Identifier::new(Span::default(), "value"),
                                ty: TypeReference {
                                    span: None,
                                    kind: TypeReferenceKind::String,
                                },
                                kind: ParameterSpec::Basic,
                            }],
                            return_type: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::Void,
                            },
                        },
                        body: Box::new(builtin_print),
                    }),
                    Decl::NativeFunction(NativeFunction {
                        signature: FunctionSignature {
                            name: Identifier::new(Span::default(), "println"),
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![Parameter {
                                name: Identifier::new(Span::default(), "value"),
                                ty: TypeReference {
                                    span: None,
                                    kind: TypeReferenceKind::String,
                                },
                                kind: ParameterSpec::Basic,
                            }],
                            return_type: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::Void,
                            },
                        },
                        body: Box::new(builtin_println),
                    }),
                ],
            },
            integer_decls!(i8, IntSize::I8, true),
            integer_decls!(i16, IntSize::I16, true),
            integer_decls!(i32, IntSize::I32, true),
            integer_decls!(i64, IntSize::I64, true),
            integer_decls!(u8, IntSize::I8, false),
            integer_decls!(u16, IntSize::I16, false),
            integer_decls!(u32, IntSize::I32, false),
            integer_decls!(u64, IntSize::I64, false),
            Decl::Module {
                visibility: Visibility::Public,
                name: Identifier::new(Span::default(), "bool"),
                body: vec![
                    Decl::NativeFunction(NativeFunction {
                        signature: FunctionSignature {
                            name: Identifier::new(Span::default(), "to_str"),
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![Parameter {
                                name: Identifier::new(Span::default(), "value"),
                                ty: TypeReference {
                                    span: None,
                                    kind: TypeReferenceKind::Bool,
                                },
                                kind: ParameterSpec::Basic,
                            }],
                            return_type: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::String,
                            },
                        },
                        body: Box::new(bool::to_str),
                    }),
                    Decl::NativeFunction(NativeFunction {
                        signature: FunctionSignature {
                            name: Identifier::new(Span::default(), "parse"),
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![Parameter {
                                name: Identifier::new(Span::default(), "value"),
                                ty: TypeReference {
                                    span: None,
                                    kind: TypeReferenceKind::String,
                                },
                                kind: ParameterSpec::Basic,
                            }],
                            return_type: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::Bool,
                            },
                        },
                        body: Box::new(bool::parse),
                    }),
                ],
            },
            Decl::Module {
                visibility: Visibility::Public,
                name: Identifier::new(Span::default(), "str"),
                body: vec![
                    Decl::NativeFunction(NativeFunction {
                        signature: FunctionSignature {
                            name: Identifier::new(Span::default(), "len"),
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![Parameter {
                                name: Identifier::new(Span::default(), "value"),
                                ty: TypeReference {
                                    span: None,
                                    kind: TypeReferenceKind::String,
                                },
                                kind: ParameterSpec::Basic,
                            }],
                            return_type: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::Integer(IntSize::I64, false),
                            },
                        },
                        body: Box::new(str::len),
                    }),
                    Decl::NativeFunction(NativeFunction {
                        signature: FunctionSignature {
                            name: Identifier::new(Span::default(), "is_integer"),
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![Parameter {
                                name: Identifier::new(Span::default(), "value"),
                                ty: TypeReference {
                                    span: None,
                                    kind: TypeReferenceKind::String,
                                },
                                kind: ParameterSpec::Basic,
                            }],
                            return_type: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::Bool,
                            },
                        },
                        body: Box::new(str::is_integer),
                    }),
                    Decl::NativeFunction(NativeFunction {
                        signature: FunctionSignature {
                            name: Identifier::new(Span::default(), "substr"),
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![
                                Parameter {
                                    name: Identifier::new(Span::default(), "value"),
                                    ty: TypeReference {
                                        span: None,
                                        kind: TypeReferenceKind::String,
                                    },
                                    kind: ParameterSpec::Basic,
                                },
                                Parameter {
                                    name: Identifier::new(Span::default(), "start"),
                                    ty: TypeReference {
                                        span: None,
                                        kind: TypeReferenceKind::Integer(IntSize::I64, false),
                                    },
                                    kind: ParameterSpec::Basic,
                                },
                                Parameter {
                                    name: Identifier::new(Span::default(), "end"),
                                    ty: TypeReference {
                                        span: None,
                                        kind: TypeReferenceKind::Integer(IntSize::I64, false),
                                    },
                                    kind: ParameterSpec::Basic,
                                },
                            ],
                            return_type: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::String,
                            },
                        },
                        body: Box::new(str::substr),
                    }),
                ],
            },
            Decl::Module {
                visibility: Visibility::Public,
                name: Identifier::new(Span::default(), "char"),
                body: vec![
                    Decl::NativeFunction(NativeFunction {
                        signature: FunctionSignature {
                            name: Identifier::new(Span::default(), "to_str"),
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![Parameter {
                                name: Identifier::new(Span::default(), "value"),
                                ty: TypeReference {
                                    span: None,
                                    kind: TypeReferenceKind::Char,
                                },
                                kind: ParameterSpec::Basic,
                            }],
                            return_type: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::String,
                            },
                        },
                        body: Box::new(char::to_str),
                    }),
                    Decl::NativeFunction(NativeFunction {
                        signature: FunctionSignature {
                            name: Identifier::new(Span::default(), "is_digit"),
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![Parameter {
                                name: Identifier::new(Span::default(), "value"),
                                ty: TypeReference {
                                    span: None,
                                    kind: TypeReferenceKind::Char,
                                },
                                kind: ParameterSpec::Basic,
                            }],
                            return_type: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::Bool,
                            },
                        },
                        body: Box::new(char::is_digit),
                    }),
                    Decl::NativeFunction(NativeFunction {
                        signature: FunctionSignature {
                            name: Identifier::new(Span::default(), "is_alpha"),
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![Parameter {
                                name: Identifier::new(Span::default(), "value"),
                                ty: TypeReference {
                                    span: None,
                                    kind: TypeReferenceKind::Char,
                                },
                                kind: ParameterSpec::Basic,
                            }],
                            return_type: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::Bool,
                            },
                        },
                        body: Box::new(char::is_alpha),
                    }),
                    Decl::NativeFunction(NativeFunction {
                        signature: FunctionSignature {
                            name: Identifier::new(Span::default(), "is_whitespace"),
                            takes_self: false,
                            self_precondition: None,
                            parameters: vec![Parameter {
                                name: Identifier::new(Span::default(), "value"),
                                ty: TypeReference {
                                    span: None,
                                    kind: TypeReferenceKind::Char,
                                },
                                kind: ParameterSpec::Basic,
                            }],
                            return_type: TypeReference {
                                span: None,
                                kind: TypeReferenceKind::Bool,
                            },
                        },
                        body: Box::new(char::is_whitespace),
                    }),
                ],
            },
            Decl::Module {
                visibility: Visibility::Public,
                name: Identifier::new(Span::default(), "iter"),
                body: vec![Decl::NativeFunction(NativeFunction {
                    signature: FunctionSignature {
                        name: Identifier::new(Span::default(), "range"),
                        takes_self: false,
                        self_precondition: None,
                        parameters: vec![
                            Parameter {
                                name: Identifier::new(Span::default(), "start"),
                                ty: TypeReference {
                                    span: None,
                                    kind: TypeReferenceKind::Integer(IntSize::I64, false),
                                },
                                kind: ParameterSpec::Basic,
                            },
                            Parameter {
                                name: Identifier::new(Span::default(), "end"),
                                ty: TypeReference {
                                    span: None,
                                    kind: TypeReferenceKind::Integer(IntSize::I64, false),
                                },
                                kind: ParameterSpec::Basic,
                            },
                        ],
                        return_type: TypeReference {
                            span: None,
                            kind: TypeReferenceKind::List(Box::new(TypeReference {
                                span: None,
                                kind: TypeReferenceKind::Integer(IntSize::I64, false),
                            })),
                        },
                    },
                    body: Box::new(iter::range),
                })],
            },
            Decl::Trait {
                visibility: Visibility::Public,
                name: Identifier::new(Span::default(), "Display"),
                body: TraitBody {
                    methods: vec![FunctionSignature {
                        name: Identifier::new(Span::default(), "display"),
                        takes_self: true,
                        self_precondition: None,
                        parameters: vec![],
                        return_type: TypeReference {
                            span: None,
                            kind: TypeReferenceKind::String,
                        },
                    }],
                },
            },
            Decl::NativeFunction(NativeFunction {
                signature: FunctionSignature {
                    name: Identifier::new(Span::default(), "exit"),
                    takes_self: false,
                    self_precondition: None,
                    parameters: vec![Parameter {
                        name: Identifier::new(Span::default(), "code"),
                        ty: TypeReference {
                            span: None,
                            kind: TypeReferenceKind::Integer(IntSize::I32, true),
                        },
                        kind: ParameterSpec::Basic,
                    }],
                    return_type: TypeReference {
                        span: None,
                        kind: TypeReferenceKind::Void,
                    },
                },
                body: Box::new(builtin_exit),
            }),
            Decl::NativeFunction(NativeFunction {
                signature: FunctionSignature {
                    name: Identifier::new(Span::default(), "dbg"),
                    takes_self: false,
                    self_precondition: None,
                    parameters: vec![Parameter {
                        name: Identifier::new(Span::default(), "value"),
                        ty: TypeReference {
                            span: None,
                            kind: TypeReferenceKind::Infer,
                        },
                        kind: ParameterSpec::Basic,
                    }],
                    return_type: TypeReference {
                        span: None,
                        kind: TypeReferenceKind::Void,
                    },
                },
                body: Box::new(debug),
            }),
        ],
    }]
}

fn builtin_print<'a>(
    _: &mut crate::interpreter::Interpreter<'a>,
    args: &[Value<'a>],
) -> Result<Value<'a>, RuntimeError> {
    if let Value::String(s) = &args[0] {
        print!("{}", s);
    } else {
        panic!("print() called with non-string argument");
    };
    Ok(Value::Void)
}

fn builtin_println<'a>(
    _: &mut crate::interpreter::Interpreter<'a>,
    args: &[Value<'a>],
) -> Result<Value<'a>, RuntimeError> {
    if let Value::String(s) = &args[0] {
        println!("{}", s);
    } else {
        panic!("println() called with non-string argument");
    };
    Ok(Value::Void)
}

fn builtin_exit<'a>(
    _: &mut crate::interpreter::Interpreter<'a>,
    args: &[Value<'a>],
) -> Result<Value<'a>, RuntimeError> {
    if let Value::I32(i) = args[0] {
        std::process::exit(i);
    } else {
        panic!("exit() called with non-integer argument");
    };
}

integer_impls!(i8, Value::I8);
integer_impls!(i16, Value::I16);
integer_impls!(i32, Value::I32);
integer_impls!(i64, Value::I64);
integer_impls!(u8, Value::U8);
integer_impls!(u16, Value::U16);
integer_impls!(u32, Value::U32);
integer_impls!(u64, Value::U64);

mod bool {
    use super::*;
    pub fn to_str<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::Bool(b) = &args[0] {
            Ok(Value::String(b.to_string()))
        } else {
            panic!("to_str() called with non-bool argument");
        }
    }
    pub fn parse<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::String(s) = &args[0] {
            Ok(Value::Bool(s.parse::<bool>().unwrap()))
        } else {
            panic!("parse() called with non-string argument");
        }
    }
}

mod str {
    use super::*;
    pub fn len<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::String(b) = &args[0] {
            Ok(Value::U64(b.len() as u64))
        } else {
            panic!("len() called with non-string argument");
        }
    }
    pub fn is_integer<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::String(s) = &args[0] {
            Ok(Value::Bool(s.parse::<i64>().is_ok()))
        } else {
            panic!("is_integer() called with non-string argument");
        }
    }
    pub fn substr<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::String(s) = &args[0] {
            let start = if let Value::U64(i) = args[1] {
                i as usize
            } else {
                panic!("substr() called with non-integer argument");
            };
            let end = if let Value::U64(i) = args[2] {
                i as usize
            } else {
                panic!("substr() called with non-integer argument");
            };
            Ok(Value::String(s[start..end].to_string()))
        } else {
            panic!("substr() called with non-string argument");
        }
    }
}

mod char {
    use super::*;
    pub fn to_str<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::Char(c) = &args[0] {
            Ok(Value::String(c.to_string()))
        } else {
            panic!("to_str() called with non-char argument");
        }
    }
    pub fn is_digit<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::Char(c) = &args[0] {
            Ok(Value::Bool(c.is_ascii_digit()))
        } else {
            panic!("is_digit() called with non-char argument");
        }
    }
    pub fn is_alpha<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::Char(c) = &args[0] {
            Ok(Value::Bool(c.is_alphabetic()))
        } else {
            panic!("is_alpha() called with non-char argument");
        }
    }
    pub fn is_whitespace<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::Char(c) = &args[0] {
            Ok(Value::Bool(c.is_whitespace()))
        } else {
            panic!("is_whitespace() called with non-char argument");
        }
    }
}

pub fn list_methods(type_ref: &TypeReference) -> Vec<NativeFunction> {
    vec![
        NativeFunction {
            signature: FunctionSignature {
                name: Identifier {
                    name: "push".to_string(),
                    span: Span::default(),
                },
                takes_self: true,
                parameters: vec![Parameter {
                    name: Identifier {
                        name: "value".to_string(),
                        span: Span::default(),
                    },
                    ty: type_ref.clone(),
                    kind: ParameterSpec::Basic,
                }],
                self_precondition: None,
                return_type: TypeReference {
                    kind: TypeReferenceKind::Void,
                    span: None,
                },
            },
            body: Box::new(list::push),
        },
        NativeFunction {
            signature: FunctionSignature {
                name: Identifier {
                    name: "pop".to_string(),
                    span: Span::default(),
                },
                takes_self: true,
                parameters: vec![],
                self_precondition: None,
                return_type: type_ref.clone(),
            },
            body: Box::new(list::pop),
        },
        NativeFunction {
            signature: FunctionSignature {
                name: Identifier {
                    name: "get".to_string(),
                    span: Span::default(),
                },
                takes_self: true,
                parameters: vec![Parameter {
                    name: Identifier {
                        name: "index".to_string(),
                        span: Span::default(),
                    },
                    ty: TypeReference {
                        kind: TypeReferenceKind::Integer(IntSize::I64, false),
                        span: None,
                    },
                    kind: ParameterSpec::Basic,
                }],
                self_precondition: None,
                return_type: type_ref.clone(),
            },
            body: Box::new(list::get),
        },
        NativeFunction {
            signature: FunctionSignature {
                name: Identifier {
                    name: "set".to_string(),
                    span: Span::default(),
                },
                takes_self: true,
                parameters: vec![
                    Parameter {
                        name: Identifier {
                            name: "index".to_string(),
                            span: Span::default(),
                        },
                        ty: TypeReference {
                            kind: TypeReferenceKind::Integer(IntSize::I64, false),
                            span: None,
                        },
                        kind: ParameterSpec::Basic,
                    },
                    Parameter {
                        name: Identifier {
                            name: "value".to_string(),
                            span: Span::default(),
                        },
                        ty: type_ref.clone(),
                        kind: ParameterSpec::Basic,
                    },
                ],
                self_precondition: None,
                return_type: TypeReference {
                    kind: TypeReferenceKind::Void,
                    span: None,
                },
            },
            body: Box::new(list::set),
        },
        NativeFunction {
            signature: FunctionSignature {
                name: Identifier {
                    name: "len".to_string(),
                    span: Span::default(),
                },
                takes_self: true,
                parameters: vec![],
                self_precondition: None,
                return_type: TypeReference {
                    kind: TypeReferenceKind::Integer(IntSize::I64, false),
                    span: None,
                },
            },
            body: Box::new(list::len),
        },
    ]
}

mod list {
    use super::*;
    pub fn push<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::List(l) = &args[0] {
            l.push(args[1].clone());
            Ok(Value::Void)
        } else {
            panic!("push() called with non-list argument");
        }
    }
    pub fn pop<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::List(l) = &args[0] {
            Ok(l.pop().unwrap_or(Value::Void))
        } else {
            panic!("pop() called with non-list argument");
        }
    }
    pub fn get<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::List(l) = &args[0] {
            if let Value::U64(i) = args[1] {
                Ok(l.get(i as usize).unwrap_or(Value::Void))
            } else {
                panic!("get() called with non-integer argument");
            }
        } else {
            panic!("get() called with non-list argument");
        }
    }
    pub fn set<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::List(l) = &args[0] {
            if let Value::U64(i) = args[1] {
                l.set(i as usize, args[2].clone());
                Ok(Value::Void)
            } else {
                panic!("set() called with non-integer argument");
            }
        } else {
            panic!("set() called with non-list argument");
        }
    }
    pub fn len<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::List(l) = &args[0] {
            Ok(Value::U64(l.len() as u64))
        } else {
            panic!("len() called with non-list argument");
        }
    }
}

mod iter {
    use crate::interpreter::value::ListInstance;

    use super::*;
    pub fn range<'a>(
        _: &mut crate::interpreter::Interpreter<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError> {
        if let Value::U64(start) = args[0] {
            if let Value::U64(end) = args[1] {
                let li = ListInstance::new(TypeReferenceKind::Integer(IntSize::I64, false));
                for i in start..end {
                    li.push(Value::U64(i));
                }

                Ok(Value::List(li))
            } else {
                panic!("range() called with non-integer argument");
            }
        } else {
            panic!("range() called with non-integer argument");
        }
    }
}

pub fn debug<'a>(
    _interpreter: &mut crate::interpreter::Interpreter<'a>,
    args: &[Value<'a>],
) -> Result<Value<'a>, RuntimeError> {
    match &args[0] {
        Value::I8(i) => eprint!("{}", i),
        Value::I16(i) => eprint!("{}", i),
        Value::I32(i) => eprint!("{}", i),
        Value::I64(i) => eprint!("{}", i),
        Value::U8(i) => eprint!("{}", i),
        Value::U16(i) => eprint!("{}", i),
        Value::U32(i) => eprint!("{}", i),
        Value::U64(i) => eprint!("{}", i),
        Value::F32(i) => eprint!("{}", i),
        Value::F64(i) => eprint!("{}", i),
        Value::Bool(b) => eprint!("{}", b),
        Value::String(s) => eprint!("\"{}\"", s),
        Value::Char(c) => eprint!("'{}'", c),
        Value::Void => eprint!("void"),
        Value::Instance(i) => {
            eprint!("{} {{ ", i.get_type_variant().name.name);
            for field_name in i.get_type_variant().fields.iter() {
                eprint!("{}: ", field_name.name.name);
                debug(_interpreter, &[i.get_field(&field_name.name.name).unwrap()])?;
                eprint!(", ");
            }
            eprint!("}}");
        }
        Value::List(_) => todo!(),
    }

    Ok(Value::Void)
}
