use crate::errors::parser::ParserError;
use crate::lexer::TokenProducer;
use crate::repr::*;
use crate::tokens::{Token, TokenKind};
use crate::unit::Span;
use crate::{errors::ErrorModuleRef, Config};

/// Macro for trying to get a match from a list of functions
/// If a function returns a value, the token is returned
/// If a function returns an error, the error is returned
/// If a function returns None, the next function is tried
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

/// Macro as an alternative to ? for ParserResult since it isn't overloadable yet
/// Track https://github.com/rust-lang/rust/issues/84277
macro_rules! propagate {
    ($e:expr) => {
        match $e {
            ParserResult::Ok(t) => t,
            ParserResult::Err(e, s) => return ParserResult::Err(e, s),
            ParserResult::None => return ParserResult::None,
        }
    };
}

/// Parser for the language
///
/// Implements a recursive descent parser.
#[derive(Debug)]
pub struct Parser<'a, P>
where
    P: TokenProducer,
{
    config: &'a Config,
    token_producer: P,
    errors: ErrorModuleRef,
    is_at_end: bool,
    token: Option<Token>,
    last_span: Option<Span>,
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
    /// Creates a Parser from a TokenProducer and references to the Config and ErrorModule
    pub fn new(config: &'a Config, token_producer: T, errors: ErrorModuleRef) -> Self {
        let mut s = Self {
            config,
            token_producer,
            errors,
            is_at_end: false,
            token: None,
            last_span: None,
        };

        s.next_token();
        s
    }

    /// Parses the tokens into a list of Decls.
    /// If there are any errors, they are added to the ErrorModule
    /// The ErrorModule should be checked for errors after parsing.
    /// If there are any errors, the returned list of Decls should not be used.
    pub fn parse(&mut self) -> Vec<Decl> {
        let mut decls = Vec::new();

        while !self.is_at_end {
            let result = self.parse_decl();

            match result {
                ParserResult::Ok(decl) => {
                    decls.push(decl);
                }
                ParserResult::None => {
                    self.errors
                        .lock()
                        .unwrap()
                        .add_parser_error(ParserError::ExpectedDecl, self.token_span());
                    break;
                }
                ParserResult::Err(err, span) => {
                    let fatal = err.is_fatal();
                    self.errors.lock().unwrap().add_parser_error(
                        err,
                        match span {
                            Some(span) => Some(span),
                            None => self.token_span(),
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

    fn parse_impl_decl(&mut self) -> ParserResult<DeclKind> {
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
                self.errors
                    .lock()
                    .unwrap()
                    .add_parser_error(ParserError::PubInTraitImpl, start_span);
            }

            let start_span = self.token_span();
            let function = self.parse_function();

            match function {
                ParserResult::Ok(function) => body.push(function),
                ParserResult::None => {
                    return ParserResult::Err(ParserError::ExpectedFunction, start_span);
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

    fn parse_module_decl(&mut self) -> ParserResult<DeclKind> {
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

    fn parse_function_decl(&mut self) -> ParserResult<DeclKind> {
        let function = propagate!(self.parse_function());

        ParserResult::Ok(DeclKind::Function(function))
    }

    fn parse_function(&mut self) -> ParserResult<Function> {
        let signature = propagate!(self.parse_function_signature());
        let body = propagate!(self.parse_executable_block());

        ParserResult::Ok(Function { signature, body })
    }

    fn parse_type_decl(&mut self) -> ParserResult<DeclKind> {
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
            } else if self.match_token(TokenKind::RightBrace) {
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
                        if self.match_token(TokenKind::RightBrace) {
                            break;
                        } else {
                            return ParserResult::Err(
                                ParserError::ExpectedToken(TokenKind::Comma),
                                self.token_span(),
                            );
                        }
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

    fn parse_trait_decl(&mut self) -> ParserResult<DeclKind> {
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

    fn parse_function_signature(&mut self) -> ParserResult<FunctionSignature> {
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
                    if parameters.len() == self.config.max_function_arguments {
                        return ParserResult::Err(
                            ParserError::TooManyFunctionParameters,
                            self.token_span(),
                        );
                    }

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
            TypeReference {
                kind: TypeReferenceKind::Void,
                span: None,
            }
        };

        ParserResult::Ok(FunctionSignature {
            name,
            takes_self,
            self_precondition,
            parameters,
            return_type,
        })
    }

    fn parse_function_param(&mut self) -> ParserResult<Parameter> {
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

    fn parse_executable_block(&mut self) -> ParserResult<Vec<Stmt>> {
        let opening_brace_span = self.token_span();
        if !self.match_token(TokenKind::LeftBrace) {
            return ParserResult::None;
        }

        let mut stmts = Vec::new();

        loop {
            if self.match_token(TokenKind::RightBrace) {
                break;
            }
            if self.is_at_end {
                return ParserResult::Err(ParserError::UnterminatedScope, opening_brace_span);
            }

            let stmt = propagate!(self.parse_stmt());

            stmts.push(stmt);
        }

        ParserResult::Ok(stmts)
    }

    pub(crate) fn parse_stmt(&mut self) -> ParserResult<Stmt> {
        let start_span = self.token_span();

        let stmt_kind = propagate!(alternatives!(
            self.parse_block_stmt(),
            self.parse_variable_stmt(),
            self.parse_if_stmt(),
            self.parse_while_stmt(),
            self.parse_for_stmt(),
            self.parse_match_stmt(),
            self.parse_return_stmt(),
            self.parse_break_stmt(),
            self.parse_continue_stmt(),
            self.parse_assignment_or_expr_stmt(), // must be last, catch-all, handles expressions
        ));

        ParserResult::Ok(Stmt {
            kind: stmt_kind,
            span: Span::from_spans(start_span.unwrap(), self.last_token_span().unwrap()),
        })
    }

    fn parse_block_stmt(&mut self) -> ParserResult<StmtKind> {
        ParserResult::Ok(StmtKind::Block(propagate!(self.parse_executable_block())))
    }

    fn parse_variable_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::Var) {
            return ParserResult::None;
        }

        let name = propagate!(self.parse_identifier());

        let ty = if self.match_token(TokenKind::Colon) {
            propagate!(self.parse_type())
        } else {
            TypeReference {
                kind: TypeReferenceKind::Infer,
                span: None,
            }
        };

        propagate!(self.consume_token(
            TokenKind::Equal,
            ParserError::ExpectedToken(TokenKind::Equal),
        ));

        let value = propagate!(self.parse_expr());

        propagate!(self.consume_token(
            TokenKind::Semicolon,
            ParserError::ExpectedToken(TokenKind::Semicolon),
        ));

        ParserResult::Ok(StmtKind::Variable { name, ty, value })
    }

    fn parse_if_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::If) {
            return ParserResult::None;
        }

        let condition = propagate!(self.parse_expr());

        let then = propagate!(self.parse_stmt());

        let otherwise = if self.match_token(TokenKind::Else) {
            Some(propagate!(self.parse_stmt()))
        } else {
            None
        };

        ParserResult::Ok(StmtKind::If {
            condition,
            then: Box::new(then),
            otherwise: otherwise.map(Box::new),
        })
    }

    fn parse_while_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::While) {
            return ParserResult::None;
        }

        let condition = propagate!(self.parse_expr());

        let body = propagate!(self.parse_stmt());

        ParserResult::Ok(StmtKind::While {
            condition,
            body: Box::new(body),
        })
    }

    fn parse_for_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::For) {
            return ParserResult::None;
        }

        let name = propagate!(self.parse_identifier());

        propagate!(self.consume_token(TokenKind::In, ParserError::ExpectedToken(TokenKind::In),));

        let iterable = propagate!(self.parse_expr());

        let body = propagate!(self.parse_stmt());

        ParserResult::Ok(StmtKind::For {
            name,
            iterable,
            body: Box::new(body),
        })
    }

    fn parse_match_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::Match) {
            return ParserResult::None;
        }

        let brace_span = self.token_span();
        let expr = if !self.match_token(TokenKind::LeftBrace) {
            let r = Some(propagate!(self.parse_identifier()));
            propagate!(self.consume_token(
                TokenKind::LeftBrace,
                ParserError::ExpectedToken(TokenKind::LeftBrace),
            ));
            r
        } else {
            None
        };

        let mut arms = Vec::new();

        loop {
            if self.match_token(TokenKind::RightBrace) {
                break;
            } else if self.is_at_end {
                return ParserResult::Err(ParserError::UnterminatedScope, brace_span);
            }

            if self.match_token(TokenKind::On) {
                let pattern = propagate!(self.parse_match_pattern());

                let body = propagate!(self.parse_stmt());

                arms.push(MatchArm {
                    pattern,
                    body: Box::new(body),
                });
            } else if self.match_token(TokenKind::Else) {
                let body = propagate!(self.parse_stmt());
                arms.push(MatchArm {
                    pattern: MatchPattern::Wildcard,
                    body: Box::new(body),
                });
            } else {
                return ParserResult::Err(ParserError::ExpectedMatchArm, self.token_span());
            }
        }

        ParserResult::Ok(StmtKind::Match { expr, arms })
    }

    fn parse_match_pattern(&mut self) -> ParserResult<MatchPattern> {
        let expr = propagate!(self.parse_expr());

        let type_ref = expr_to_type_ref(&expr);

        if self.match_token(TokenKind::Pipe) {
            if type_ref.is_none() {
                return ParserResult::Err(ParserError::ExpectedType, Some(expr.span));
            } else {
                let mut types = vec![type_ref.unwrap()];
                while self.match_token(TokenKind::Pipe) {
                    let ty = propagate!(self.parse_type());
                    types.push(ty);
                }
                return ParserResult::Ok(MatchPattern::Type(TypeReference {
                    kind: TypeReferenceKind::Union(types),
                    span: Some(Span::from_spans(expr.span, self.last_token_span().unwrap())),
                }));
            }
        }

        ParserResult::Ok(MatchPattern::Expr(expr))
    }

    fn parse_return_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::Return) {
            return ParserResult::None;
        }

        let return_span = self.token_span().unwrap();

        if !self.match_token(TokenKind::Semicolon) {
            let expr = propagate!(self.parse_expr());
            propagate!(self.consume_token(
                TokenKind::Semicolon,
                ParserError::ExpectedToken(TokenKind::Semicolon),
            ));
            ParserResult::Ok(StmtKind::Return(expr))
        } else {
            ParserResult::Ok(StmtKind::Return(Expr {
                kind: ExprKind::Literal(Literal::Void),
                span: return_span,
            }))
        }
    }

    fn parse_break_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::Break) {
            return ParserResult::None;
        }

        propagate!(self.consume_token(
            TokenKind::Semicolon,
            ParserError::ExpectedToken(TokenKind::Semicolon),
        ));

        ParserResult::Ok(StmtKind::Break)
    }

    fn parse_continue_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::Continue) {
            return ParserResult::None;
        }

        propagate!(self.consume_token(
            TokenKind::Semicolon,
            ParserError::ExpectedToken(TokenKind::Semicolon),
        ));

        ParserResult::Ok(StmtKind::Continue)
    }

    fn parse_assignment_or_expr_stmt(&mut self) -> ParserResult<StmtKind> {
        let expr = propagate!(self.parse_expr());

        if self.match_token(TokenKind::Semicolon) {
            return ParserResult::Ok(StmtKind::Expr(expr));
        }

        propagate!(self.consume_token(
            TokenKind::Equal,
            ParserError::ExpectedToken(TokenKind::Equal)
        ));

        let value = propagate!(self.parse_expr());

        propagate!(self.consume_token(
            TokenKind::Semicolon,
            ParserError::ExpectedToken(TokenKind::Semicolon),
        ));

        ParserResult::Ok(StmtKind::Assign {
            target: expr,
            value,
        })
    }

    pub(crate) fn parse_expr(&mut self) -> ParserResult<Expr> {
        self.parse_logical_or_expr()
    }

    fn parse_binary_left_assoc_expr(
        &mut self,
        mut next_level: impl FnMut(&mut Self) -> ParserResult<Expr>,
        pairs: &[(TokenKind, BinaryOp)],
    ) -> ParserResult<Expr> {
        let mut expr = propagate!(next_level(self));

        loop {
            if self.is_at_end {
                break;
            }
            let token = self.token_kind();

            if pairs.iter().any(|(k, _)| *k == *token) {
                let op = pairs.iter().find(|(k, _)| *k == *token).unwrap().1.clone();
                self.next_token();
                let right = propagate!(next_level(self));
                expr = Expr {
                    span: Span::from_spans(expr.span, right.span),
                    kind: ExprKind::Binary {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    },
                };
            } else {
                break;
            }
        }

        ParserResult::Ok(expr)
    }

    // TODO: or/and short-circuit, so it could be more efficient to parse them as
    // right-associative?
    fn parse_logical_or_expr(&mut self) -> ParserResult<Expr> {
        self.parse_binary_left_assoc_expr(
            Self::parse_logical_and_expr,
            &[(TokenKind::Or, BinaryOp::Or)],
        )
    }

    fn parse_logical_and_expr(&mut self) -> ParserResult<Expr> {
        self.parse_binary_left_assoc_expr(
            Self::parse_equality_expr,
            &[(TokenKind::And, BinaryOp::And)],
        )
    }

    fn parse_equality_expr(&mut self) -> ParserResult<Expr> {
        self.parse_binary_left_assoc_expr(
            Self::parse_is_expr,
            &[
                (TokenKind::EqualEqual, BinaryOp::Eq),
                (TokenKind::BangEqual, BinaryOp::Neq),
            ],
        )
    }

    fn parse_is_expr(&mut self) -> ParserResult<Expr> {
        let mut expr = propagate!(self.parse_comparison_expr());
        loop {
            let token = self.token_kind();
            if *token == (TokenKind::Is) {
                self.next_token();
                let ty = propagate!(self.parse_type());
                expr = Expr {
                    span: Span::from_spans(expr.span, ty.span.unwrap()),
                    kind: ExprKind::TypeBinary {
                        expr: Box::new(expr),
                        op: TypeBinaryOp::Is,
                        ty,
                    },
                };
            } else {
                break;
            }
        }
        ParserResult::Ok(expr)
    }

    fn parse_comparison_expr(&mut self) -> ParserResult<Expr> {
        self.parse_binary_left_assoc_expr(
            Self::parse_term_expr,
            &[
                (TokenKind::Greater, BinaryOp::Gt),
                (TokenKind::GreaterEqual, BinaryOp::Geq),
                (TokenKind::Less, BinaryOp::Lt),
                (TokenKind::LessEqual, BinaryOp::Leq),
            ],
        )
    }

    fn parse_term_expr(&mut self) -> ParserResult<Expr> {
        self.parse_binary_left_assoc_expr(
            Self::parse_factor_expr,
            &[
                (TokenKind::Plus, BinaryOp::Add),
                (TokenKind::Minus, BinaryOp::Sub),
            ],
        )
    }

    fn parse_factor_expr(&mut self) -> ParserResult<Expr> {
        self.parse_binary_left_assoc_expr(
            Self::parse_as_expr,
            &[
                (TokenKind::Star, BinaryOp::Mul),
                (TokenKind::Slash, BinaryOp::Div),
            ],
        )
    }

    fn parse_as_expr(&mut self) -> ParserResult<Expr> {
        let mut expr = propagate!(self.parse_unary_expr());
        loop {
            let token = self.token_kind();
            if *token == (TokenKind::As) {
                self.next_token();
                let ty = propagate!(self.parse_type());
                expr = Expr {
                    span: Span::from_spans(expr.span, ty.span.unwrap()),
                    kind: ExprKind::TypeBinary {
                        expr: Box::new(expr),
                        op: TypeBinaryOp::As,
                        ty,
                    },
                };
            } else {
                break;
            }
        }
        ParserResult::Ok(expr)
    }

    fn parse_unary_expr(&mut self) -> ParserResult<Expr> {
        let mut ops = vec![];

        if self.is_at_end {
            return self.parse_primary_expr();
        }

        let start_span = self.token_span().unwrap();

        loop {
            if self.match_token(TokenKind::Bang) {
                ops.push(UnaryOp::Not);
            } else if self.match_token(TokenKind::Minus) {
                ops.push(UnaryOp::Neg);
            } else {
                break;
            }
        }

        let mut expr = propagate!(self.parse_primary_expr());

        for op in ops.into_iter().rev() {
            let expr_span = expr.span;
            expr = Expr {
                kind: ExprKind::Unary {
                    op,
                    expr: Box::new(expr),
                },
                span: Span::from_spans(start_span, expr_span),
            };
        }

        ParserResult::Ok(expr)
    }

    fn parse_primary_expr(&mut self) -> ParserResult<Expr> {
        if self.match_token(TokenKind::LeftParen) {
            let paren_span = self.last_token_span().unwrap();
            let mut expr = propagate!(self.parse_expr());
            propagate!(self.consume_token(
                TokenKind::RightParen,
                ParserError::ExpectedToken(TokenKind::RightParen),
            ));
            let span = Span::from_spans(paren_span, self.last_token_span().unwrap());
            expr.span = span;
            return ParserResult::Ok(expr);
        }
        if self.is_at_end {
            return ParserResult::None;
        }
        let start_span = self.token_span().unwrap();
        let literal_result = self.parse_literal();
        match literal_result {
            ParserResult::Ok(literal) => {
                return ParserResult::Ok(Expr {
                    kind: ExprKind::Literal(literal),
                    span: Span::from_spans(start_span, self.last_token_span().unwrap()),
                })
            }
            ParserResult::Err(e, s) => return ParserResult::Err(e, s),
            ParserResult::None => {}
        }

        self.parse_identifier_starting_expr()
    }

    fn parse_identifier_starting_expr(&mut self) -> ParserResult<Expr> {
        if self.is_at_end {
            return ParserResult::None;
        }
        let name = propagate!(self.parse_namespaced_identifier());
        let name_span = name.span();

        let mut expr = Expr {
            kind: ExprKind::Variable(name),
            span: name_span,
        };

        if self.match_token(TokenKind::Arrow) {
            propagate!(self.consume_token(
                TokenKind::LeftBrace,
                ParserError::ExpectedToken(TokenKind::LeftBrace),
            ));

            let brace_span = self.last_token_span();
            let mut fields = vec![];
            loop {
                if self.match_token(TokenKind::RightBrace) {
                    break;
                } else if self.is_at_end {
                    return ParserResult::Err(ParserError::UnterminatedScope, brace_span);
                }

                let field_name = propagate!(self.parse_identifier());

                if self.match_token(TokenKind::Colon) {
                    let expr = propagate!(self.parse_expr());
                    fields.push(FieldConstructor::Named {
                        name: field_name,
                        value: expr,
                    });
                } else {
                    fields.push(FieldConstructor::Auto(field_name));
                }

                if !self.match_token(TokenKind::Comma) {
                    if self.match_token(TokenKind::RightBrace) {
                        break;
                    } else {
                        return ParserResult::Err(
                            ParserError::ExpectedToken(TokenKind::RightBrace),
                            Some(self.token_span().unwrap()),
                        );
                    }
                }
            }

            let ty = match expr.kind {
                ExprKind::Variable(name) => Some(name),
                _ => None,
            };

            if let Some(ty) = ty {
                expr = Expr {
                    kind: ExprKind::Constructor { ty, fields },
                    span: Span::from_spans(expr.span, self.last_token_span().unwrap()),
                };
            } else {
                return ParserResult::Err(ParserError::ExpectedType, Some(expr.span));
            }
        }

        while !self.is_at_end {
            if self.match_token(TokenKind::Dot) {
                let field = propagate!(self.parse_identifier());
                let field_span = field.span;
                let expr_span = expr.span;

                expr = Expr {
                    kind: ExprKind::Access {
                        expr: Box::new(expr),
                        field,
                    },
                    span: Span::from_spans(expr_span, field_span),
                };
            } else if self.match_token(TokenKind::LeftParen) {
                let paren_span = self.last_token_span();
                let mut args = vec![];
                loop {
                    if self.match_token(TokenKind::RightParen) {
                        break;
                    } else if self.is_at_end {
                        return ParserResult::Err(ParserError::UnterminatedScope, paren_span);
                    }

                    let arg = propagate!(self.parse_expr());
                    args.push(arg);
                    if !self.match_token(TokenKind::Comma) {
                        if !self.match_token(TokenKind::RightParen) {
                            return ParserResult::Err(ParserError::UnterminatedScope, paren_span);
                        }
                        break;
                    }
                }
                let expr_span = expr.span;

                expr = Expr {
                    kind: ExprKind::Call {
                        expr: Box::new(expr),
                        args,
                    },
                    span: Span::from_spans(expr_span, self.last_token_span().unwrap()),
                };
            } else if self.match_token(TokenKind::LeftBracket) {
                let index = propagate!(self.parse_expr());

                propagate!(self.consume_token(
                    TokenKind::RightBracket,
                    ParserError::ExpectedToken(TokenKind::RightBracket),
                ));

                let index_span = index.span;
                let expr_span = expr.span;

                expr = Expr {
                    kind: ExprKind::Index {
                        expr: Box::new(expr),
                        index: Box::new(index),
                    },
                    span: Span::from_spans(expr_span, index_span),
                };
            } else {
                break;
            }
        }

        ParserResult::Ok(expr)
    }

    fn parse_literal(&mut self) -> ParserResult<Literal> {
        if self.match_token(TokenKind::True) {
            return ParserResult::Ok(Literal::Bool(true));
        } else if self.match_token(TokenKind::False) {
            return ParserResult::Ok(Literal::Bool(false));
        } else if self.match_token(TokenKind::Void) {
            return ParserResult::Ok(Literal::Void);
        } else if self.match_token(TokenKind::LeftBracket) {
            let mut elements = vec![];
            while !self.match_token(TokenKind::RightBracket) {
                let element = propagate!(self.parse_expr());
                elements.push(element);
                if !self.match_token(TokenKind::Comma) {
                    propagate!(self.consume_token(
                        TokenKind::RightBracket,
                        ParserError::ExpectedToken(TokenKind::RightBracket),
                    ));
                    break;
                }
            }
            return ParserResult::Ok(Literal::List(elements));
        }

        if self.token.is_none() {
            return ParserResult::None;
        }

        // handle the not-so-simple token kinds

        let result = match self.token.as_ref().unwrap().kind {
            TokenKind::Integer(i) => ParserResult::Ok(Literal::Int(i)),
            TokenKind::Float(f) => ParserResult::Ok(Literal::Float(f)),
            TokenKind::String(ref s) => ParserResult::Ok(Literal::String(s.clone())),
            TokenKind::Char(c) => ParserResult::Ok(Literal::Char(c)),
            _ => ParserResult::None,
        };

        match result {
            ParserResult::Ok(lit) => {
                self.next_token();
                ParserResult::Ok(lit)
            }
            _ => result,
        }
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
            let tys_span = Span::from_spans(
                tys.first().unwrap().span.unwrap(),
                tys.last().unwrap().span.unwrap(),
            );
            ParserResult::Ok(TypeReference {
                kind: TypeReferenceKind::Union(tys),
                span: Some(tys_span),
            })
        }
    }

    fn parse_type_primary(&mut self) -> ParserResult<TypeReference> {
        if self.match_token(TokenKind::Void) {
            ParserResult::Ok(TypeReference {
                kind: TypeReferenceKind::Void,
                span: self.last_token_span(),
            })
        } else if self.match_token(TokenKind::LeftBracket) {
            let start_span = self.last_token_span().unwrap();
            let ty = propagate!(self.parse_type());
            propagate!(self.consume_token(
                TokenKind::RightBracket,
                ParserError::ExpectedToken(TokenKind::RightBracket),
            ));
            let end_span = self.last_token_span().unwrap();

            ParserResult::Ok(TypeReference {
                kind: TypeReferenceKind::List(Box::new(ty)),
                span: Some(Span::from_spans(start_span, end_span)),
            })
        } else {
            let ty = propagate!(self.parse_namespaced_identifier());
            let ty_span = ty.span();

            ParserResult::Ok(TypeReference {
                kind: TypeReferenceKind::Named(ty),
                span: Some(ty_span),
            })
        }
    }

    fn parse_namespaced_identifier(&mut self) -> ParserResult<NamespacedIdentifier> {
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

    fn parse_identifier(&mut self) -> ParserResult<Identifier> {
        if let TokenKind::Identifier(name) = self.token_kind().clone() {
            let span = self.token_span().unwrap();
            self.next_token();
            ParserResult::Ok(Identifier { name, span })
        } else {
            ParserResult::Err(ParserError::ExpectedIdentifier, self.token_span())
        }
    }

    fn next_token(&mut self) {
        if !(self.last_span.is_none() && !self.is_at_end && self.token.is_none()) {
            self.last_span = self.token_span();
        }
        self.token = self.token_producer.next();
        self.is_at_end =
            self.is_at_end || self.token.is_none() || self.token_kind() == &TokenKind::Eof;
    }

    fn token_span(&self) -> Option<Span> {
        self.token.as_ref().unwrap().span
    }

    fn last_token_span(&self) -> Option<Span> {
        self.last_span
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
