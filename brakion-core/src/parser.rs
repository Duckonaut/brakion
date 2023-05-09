use crate::errors::parser::ParserError;
use crate::lexer::TokenProducer;
use crate::repr::*;
use crate::tokens::{Token, TokenKind};
use crate::unit::Span;
use crate::{errors::ErrorModuleRef, Config};

// Macro for trying to get a match from a list of functions
// If a function returns a value, the token is returned
// If a function returns an error, the error is returned
// If a function returns None, the next function is tried
macro_rules! alternatives {
    () => {
        ParserResult::None
    };
    ($e:expr) => {
        $e
    };
    ($head:expr $(,$tail:expr)* $(,)?) => {
        match $head {
            ParserResult::Ok(t) => ParserResult::Ok(t),
            ParserResult::Err(e, s) => ParserResult::Err(e, s),
            ParserResult::None => alternatives!($($tail,)*),
        }
    };
}

// Macro as an alternative to ? for ParserResult since it isn't overloadable yet
// Track https://github.com/rust-lang/rust/issues/84277
macro_rules! propagate {
    ($e:expr) => {
        match $e {
            ParserResult::Ok(t) => t,
            ParserResult::Err(e, s) => return ParserResult::Err(e, s),
            ParserResult::None => return ParserResult::None,
        }
    };
}

#[derive(Debug)]
pub struct Parser<'a, P>
where
    P: TokenProducer,
{
    config: &'a Config,
    pub token_producer: P,
    errors: ErrorModuleRef,
    is_at_end: bool,
    token: Option<Token>,
}

#[derive(Debug)]
#[must_use]
pub(crate) enum ParserResult<T> {
    Ok(T),
    None,
    Err(ParserError, Option<Span>),
}

impl<'a, T> Parser<'a, T>
where
    T: TokenProducer,
{
    pub fn new(config: &'a Config, token_producer: T, errors: ErrorModuleRef) -> Self {
        let mut s = Self {
            config,
            token_producer,
            errors,
            is_at_end: false,
            token: None,
        };

        s.next_token();
        s
    }

    pub fn parse(&mut self) -> Vec<Decl> {
        let mut decls = Vec::new();

        while !self.is_at_end {
            let start_span = self.token.as_ref().unwrap().span.unwrap();
            let result = self.parse_decl();

            match result {
                ParserResult::Ok(decl) => {
                    eprintln!("Parsed decl");
                    dbg!(&decl);
                    decls.push(decl);
                }
                ParserResult::None => {
                    self.errors.lock().unwrap().add_parser_error(
                        ParserError::ExpectedDecl,
                        Some(Span::from_spans(
                            start_span,
                            self.token.as_ref().unwrap().span.unwrap(),
                        )),
                    );
                    break;
                },
                ParserResult::Err(err, span) => {
                    let fatal = err.is_fatal();
                    self.errors.lock().unwrap().add_parser_error(
                        err,
                        match span {
                            Some(span) => Some(span),
                            None => Some(Span::from_spans(
                                start_span,
                                self.token.as_ref().unwrap().span.unwrap(),
                            )),
                        },
                    );

                    if !fatal {
                        self.synchronize();
                    } else {
                        break;
                    }
                }
            }
        }

        decls
    }

    // Consumes tokens until a new "starting" token is found
    fn synchronize(&mut self) {
        self.next_token();

        while !self.is_at_end {
            if *self.token_kind() == TokenKind::Semicolon {
                self.next_token();
                return;
            }

            match self.token_kind() {
                TokenKind::Pub
                | TokenKind::Mod
                | TokenKind::Fn
                | TokenKind::Type
                | TokenKind::Trait
                | TokenKind::Impl => return,
                _ => (),
            }

            self.next_token();
        }
    }

    pub(crate) fn parse_decl(&mut self) -> ParserResult<Decl> {
        // impl is special, does not have a visibility
        let imp = self.parse_impl_decl();

        if let ParserResult::Ok(imp) = imp {
            return ParserResult::Ok(Decl {
                visibility: Visibility::Private,
                kind: imp,
            });
        } else if let ParserResult::Err(err, span) = imp {
            return ParserResult::Err(err, span);
        }

        let visibility = propagate!(self.parse_visibility());

        let kind = propagate!(alternatives!(
            self.parse_module_decl(),
            self.parse_function_decl(),
            self.parse_type_decl(),
            self.parse_trait_decl(),
        ));

        ParserResult::Ok(Decl { visibility, kind })
    }

    pub(crate) fn parse_visibility(&mut self) -> ParserResult<Visibility> {
        if self.match_token(TokenKind::Pub) {
            ParserResult::Ok(Visibility::Public)
        } else {
            ParserResult::Ok(Visibility::Private)
        }
    }

    pub(crate) fn parse_impl_decl(&mut self) -> ParserResult<DeclKind> {
        if !self.match_token(TokenKind::Impl) {
            return ParserResult::None;
        }

        let trait_name = propagate!(self.parse_namespaced_identifier());

        propagate!(self.consume_token(TokenKind::For, ParserError::ExpectedToken(TokenKind::For),));

        let ty = propagate!(self.parse_type());

        propagate!(self.consume_token(
            TokenKind::LeftBrace,
            ParserError::ExpectedToken(TokenKind::LeftBrace),
        ));

        let mut body = Vec::new();

        while !self.match_token(TokenKind::RightBrace) && !self.is_at_end {
            let start_span = self.token_span();
            if self.match_token(TokenKind::Pub) {
                self.errors.lock().unwrap().add_parser_error(
                    ParserError::PubInTraitImpl,
                    start_span,
                );
            }

            let start_span = self.token_span();
            let function = self.parse_function();

            match function {
                ParserResult::Ok(function) => body.push(function),
                ParserResult::None => {
                    return ParserResult::Err(
                        ParserError::ExpectedFunction,
                        start_span,
                    );
                }
                ParserResult::Err(err, span) => {
                    self.errors.lock().unwrap().add_parser_error(err, span);
                }
            }
        }

        ParserResult::Ok(DeclKind::Impl {
            trait_name,
            type_name: ty,
            body,
        })
    }

    pub(crate) fn parse_module_decl(&mut self) -> ParserResult<DeclKind> {
        if !self.match_token(TokenKind::Mod) {
            return ParserResult::None;
        }

        let name = propagate!(self.parse_identifier());

        propagate!(self.consume_token(
            TokenKind::LeftBrace,
            ParserError::ExpectedToken(TokenKind::LeftBrace),
        ));

        let mut body = Vec::new();

        while !self.match_token(TokenKind::RightBrace) {
            let decl = propagate!(self.parse_decl());
            body.push(decl);
        }

        ParserResult::Ok(DeclKind::Module { name, body })
    }

    pub(crate) fn parse_function_decl(&mut self) -> ParserResult<DeclKind> {
        let function = propagate!(self.parse_function());

        ParserResult::Ok(DeclKind::Function(function))
    }

    pub(crate) fn parse_function(&mut self) -> ParserResult<Function> {
        let signature = propagate!(self.parse_function_signature());
        let body = propagate!(self.parse_executable_block());

        ParserResult::Ok(Function { signature, body })
    }

    pub(crate) fn parse_type_decl(&mut self) -> ParserResult<DeclKind> {
        if !self.match_token(TokenKind::Type) {
            return ParserResult::None;
        }

        let name = propagate!(self.parse_identifier());
        let name_span = name.span;

        if self.match_token(TokenKind::Semicolon) {
            return ParserResult::Ok(DeclKind::Type {
                name,
                body: TypeBody {
                    variants: vec![TypeVariant {
                        name: Identifier {
                            span: name_span,
                            name: "self".to_string(),
                        },
                        fields: Vec::new(),
                    }],
                    methods: Vec::new(),
                },
            });
        }

        propagate!(self.consume_token(
            TokenKind::LeftBrace,
            ParserError::ExpectedToken(TokenKind::LeftBrace),
        ));

        let mut variants = Vec::new();

        while !self.is_at_end {
            if self.token_kind() == &TokenKind::Pub || self.token_kind() == &TokenKind::Fn {
                break;
            }
            else if self.match_token(TokenKind::RightBrace) {
                return ParserResult::Ok(DeclKind::Type {
                    name,
                    body: TypeBody {
                        variants,
                        methods: Vec::new(),
                    },
                });
            }

            let variant_name = propagate!(self.parse_identifier());
            if self.match_token(TokenKind::Semicolon) {
                variants.push(TypeVariant {
                    name: variant_name,
                    fields: Vec::new(),
                });
            } else {
                propagate!(self.consume_token(
                    TokenKind::LeftBrace,
                    ParserError::ExpectedToken(TokenKind::LeftBrace),
                ));

                let mut fields = Vec::new();

                while !self.match_token(TokenKind::RightBrace) && !self.is_at_end {
                    let field_name = propagate!(self.parse_identifier());
                    propagate!(self.consume_token(
                        TokenKind::Colon,
                        ParserError::ExpectedToken(TokenKind::Colon),
                    ));
                    let field_type = propagate!(self.parse_type());

                    fields.push(Field {
                        name: field_name,
                        ty: field_type,
                    });

                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }

                variants.push(TypeVariant {
                    name: variant_name,
                    fields,
                });
            }
        }

        let mut methods = Vec::new();

        while !self.match_token(TokenKind::RightBrace) && !self.is_at_end {
            let visibility = propagate!(self.parse_visibility());
            let f = propagate!(self.parse_function());
            methods.push((visibility, f));
        }

        ParserResult::Ok(DeclKind::Type {
            name,
            body: TypeBody { variants, methods },
        })
    }

    pub(crate) fn parse_trait_decl(&mut self) -> ParserResult<DeclKind> {
        if !self.match_token(TokenKind::Trait) {
            return ParserResult::None;
        }

        let name = propagate!(self.parse_identifier());

        propagate!(self.consume_token(
            TokenKind::LeftBrace,
            ParserError::ExpectedToken(TokenKind::LeftBrace),
        ));

        let mut methods = Vec::new();

        while !self.match_token(TokenKind::RightBrace) && !self.is_at_end {
            let f = propagate!(self.parse_function_signature());
            propagate!(self.consume_token(
                TokenKind::Semicolon,
                ParserError::ExpectedToken(TokenKind::Semicolon),
            ));
            methods.push(f);
        }

        ParserResult::Ok(DeclKind::Trait {
            name,
            body: TraitBody { methods },
        })
    }

    pub(crate) fn parse_function_signature(&mut self) -> ParserResult<FunctionSignature> {
        if !self.match_token(TokenKind::Fn) {
            return ParserResult::None;
        }

        let name = propagate!(self.parse_identifier());
        propagate!(self.consume_token(
            TokenKind::LeftParen,
            ParserError::ExpectedToken(TokenKind::LeftParen),
        ));
        let mut takes_self = false;
        let mut self_precondition = None;
        let mut parameters = Vec::new();
        if !self.match_token(TokenKind::RightParen) {
            let name = propagate!(self.parse_identifier());

            if name.name == "self" {
                takes_self = true;

                if self.match_token(TokenKind::Question) {
                    let precond = propagate!(self.parse_type());
                    self_precondition = Some(precond);
                }
            } else {
                propagate!(self.consume_token(
                    TokenKind::Colon,
                    ParserError::ExpectedToken(TokenKind::Colon),
                ));
                let ty = propagate!(self.parse_type());

                let kind = if self.match_token(TokenKind::Question) {
                    ParameterSpec::Preconditioned(propagate!(self.parse_type()))
                } else {
                    ParameterSpec::Basic
                };

                parameters.push(Parameter { name, ty, kind });
            }

            if !self.match_token(TokenKind::Comma) {
                propagate!(self.consume_token(
                    TokenKind::RightParen,
                    ParserError::ExpectedToken(TokenKind::RightParen),
                ));
            } else {
                while !self.match_token(TokenKind::RightParen) && !self.is_at_end {
                    let param = propagate!(self.parse_function_param());
                    parameters.push(param);
                    if !self.match_token(TokenKind::Comma) {
                        propagate!(self.consume_token(
                            TokenKind::RightParen,
                            ParserError::ExpectedToken(TokenKind::RightParen),
                        ));
                        break;
                    }
                }
            }
        }

        let return_type = if self.match_token(TokenKind::Arrow) {
            propagate!(self.parse_type())
        } else {
            TypeReference::Void
        };

        ParserResult::Ok(FunctionSignature {
            name,
            takes_self,
            self_precondition,
            parameters,
            return_type,
        })
    }

    pub(crate) fn parse_function_param(&mut self) -> ParserResult<Parameter> {
        let name = propagate!(self.parse_identifier());
        propagate!(self.consume_token(
            TokenKind::Colon,
            ParserError::ExpectedToken(TokenKind::Colon),
        ));
        let ty = propagate!(self.parse_type());

        let kind = if self.match_token(TokenKind::Question) {
            ParameterSpec::Preconditioned(propagate!(self.parse_type()))
        } else {
            ParameterSpec::Basic
        };

        ParserResult::Ok(Parameter { name, ty, kind })
    }

    pub(crate) fn parse_executable_block(&mut self) -> ParserResult<Vec<Stmt>> {
        if !self.match_token(TokenKind::LeftBrace) {
            return ParserResult::None;
        }

        let mut stmts = Vec::new();

        loop {
            if self.match_token(TokenKind::RightBrace) {
                break;
            } else if let ParserResult::Ok(block) = self.parse_executable_block() {
                stmts.push(Stmt::Block(block));
            } else {
                self.next_token();
            }
        }

        ParserResult::Ok(stmts)
    }

    pub(crate) fn parse_type(&mut self) -> ParserResult<TypeReference> {
        let mut tys = vec![propagate!(self.parse_type_primary())];

        while self.match_token(TokenKind::Pipe) && !self.is_at_end {
            let ty = propagate!(self.parse_type_primary());
            tys.push(ty);
        }

        if tys.len() == 1 {
            ParserResult::Ok(tys.pop().unwrap())
        } else {
            ParserResult::Ok(TypeReference::Union(tys))
        }
    }

    pub(crate) fn parse_type_primary(&mut self) -> ParserResult<TypeReference> {
        if self.match_token(TokenKind::Void) {
            ParserResult::Ok(TypeReference::Void)
        } else if self.match_token(TokenKind::LeftBracket) {
            let ty = propagate!(self.parse_type());
            propagate!(self.consume_token(
                TokenKind::RightBracket,
                ParserError::ExpectedToken(TokenKind::RightBracket),
            ));

            ParserResult::Ok(TypeReference::List(Box::new(ty)))
        } else {
            let ty = propagate!(self.parse_namespaced_identifier());

            ParserResult::Ok(TypeReference::Named(ty))
        }
    }

    pub(crate) fn parse_namespaced_identifier(&mut self) -> ParserResult<NamespacedIdentifier> {
        let mut namespace = Vec::new();
        let mut ident = propagate!(self.parse_identifier());
        loop {
            if self.match_token(TokenKind::DoubleColon) {
                namespace.push(ident);
                ident = propagate!(self.parse_identifier());
            } else {
                break;
            }
        }

        ParserResult::Ok(NamespacedIdentifier { namespace, ident })
    }

    pub(crate) fn parse_identifier(&mut self) -> ParserResult<Identifier> {
        if let TokenKind::Identifier(name) = self.token_kind().clone() {
            let span = self.token_span().unwrap();
            self.next_token();
            ParserResult::Ok(Identifier { name, span })
        } else {
            ParserResult::Err(ParserError::ExpectedIdentifier, self.token_span())
        }
    }

    fn next_token(&mut self) {
        self.token = self.token_producer.next();
        self.is_at_end =
            self.is_at_end || self.token.is_none() || self.token_kind() == &TokenKind::Eof;
    }

    fn token_span(&self) -> Option<Span> {
        self.token.as_ref().unwrap().span
    }

    fn token_kind(&self) -> &TokenKind {
        &self.token.as_ref().unwrap().kind
    }

    fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.is_at_end {
            return false;
        }

        if self.token_kind() == &kind {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn consume_token(&mut self, kind: TokenKind, err: ParserError) -> ParserResult<()> {
        if self.match_token(kind) {
            ParserResult::Ok(())
        } else {
            ParserResult::Err(err, self.token_span())
        }
    }
}
