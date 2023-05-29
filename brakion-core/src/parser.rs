use crate::errors::parser::ParserError;
use crate::lexer::TokenProducer;
use crate::repr::*;
use crate::tokens::{Token, TokenKind};
use crate::unit::Span;
use crate::{errors::ErrorModule, Config};

/// Macro for trying to get a match from a list of functions
/// If a function returns a value, the token is returned
/// If a function returns an error, the error is returned
/// If a function returns the `InvalidStart` error, the next function is tried
macro_rules! alternatives {
    () => {
        Err((ParserError::InvalidStart, None))
    };
    ($e:expr) => {
        $e
    };
    ($head:expr $(,$tail:expr)* $(,)?) => {
        match $head {
            Ok(t) => Ok(t),
            Err((ParserError::InvalidStart, _)) => alternatives!($($tail),*),
            Err((e, s)) => Err((e, s)),
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
    errors: ErrorModule,
    is_at_end: bool,
    token: Option<Token>,
    last_span: Option<Span>,
}

type ParserResult<T> = Result<T, (ParserError, Option<Span>)>;

impl<'a, T> Parser<'a, T>
where
    T: TokenProducer,
{
    /// Creates a Parser from a TokenProducer and references to the Config and ErrorModule
    pub fn new(config: &'a Config, token_producer: T, errors: ErrorModule) -> Self {
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

        loop {
            let result = self.parse_decl();

            match result {
                Ok(Some(decl)) => {
                    decls.push(decl);
                }
                Ok(None) => {
                    if self.is_at_end {
                        break;
                    } else {
                        self.errors
                            .add_parser_error(ParserError::ExpectedDecl, self.token_span());

                        self.synchronize(&[
                            TokenKind::Pub,
                            TokenKind::Impl,
                            TokenKind::Mod,
                            TokenKind::Fn,
                            TokenKind::Type,
                            TokenKind::Trait,
                        ]);
                    }
                }
                Err((err, span)) => {
                    let fatal = err.is_fatal();
                    self.errors.add_parser_error(
                        err,
                        match span {
                            Some(span) => Some(span),
                            None => self.token_span(),
                        },
                    );

                    if !fatal {
                        self.synchronize(&[
                            TokenKind::Pub,
                            TokenKind::Impl,
                            TokenKind::Mod,
                            TokenKind::Fn,
                            TokenKind::Type,
                            TokenKind::Trait,
                        ]);
                    } else {
                        break;
                    }
                }
            }
        }

        decls
    }

    // Consumes tokens until a new "starting" token is found
    fn synchronize(&mut self, stoppers: &[TokenKind]) {
        while !self.is_at_end && !stoppers.contains(self.token_kind()) {
            self.next_token();
        }
    }

    pub(crate) fn parse_decl(&mut self) -> ParserResult<Option<Decl>> {
        let result = alternatives!(
            self.parse_impl_decl(),
            self.parse_visibility_starting_decl(),
        );

        match result {
            Ok(decl) => Ok(Some(decl)),
            Err((ParserError::InvalidStart, _)) => Ok(None),
            Err((err, span)) => Err((err, span)),
        }
    }

    fn parse_visibility_starting_decl(&mut self) -> ParserResult<Decl> {
        let visibility = self.parse_visibility()?;

        alternatives!(
            self.parse_module_decl(visibility),
            self.parse_function_decl(visibility),
            self.parse_type_decl(visibility),
            self.parse_trait_decl(visibility),
        )
    }

    pub(crate) fn parse_visibility(&mut self) -> ParserResult<Visibility> {
        if self.match_token(TokenKind::Pub) {
            Ok(Visibility::Public)
        } else {
            Ok(Visibility::Private)
        }
    }

    fn parse_impl_decl(&mut self) -> ParserResult<Decl> {
        if !self.match_token(TokenKind::Impl) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let trait_name = self.parse_namespaced_identifier()?;

        self.consume_token(TokenKind::For, ParserError::ExpectedToken(TokenKind::For))?;

        let ty = self.parse_namespaced_identifier()?;

        self.consume_token(
            TokenKind::LeftBrace,
            ParserError::ExpectedToken(TokenKind::LeftBrace),
        )?;

        let mut body = Vec::new();

        let mut impl_method = self.parse_impl_method()?;

        while impl_method.is_some() {
            body.push(impl_method.unwrap());

            impl_method = self.parse_impl_method()?;
        }

        self.consume_token(
            TokenKind::RightBrace,
            ParserError::ExpectedToken(TokenKind::RightBrace),
        )?;

        Ok(Decl::Impl {
            trait_name,
            type_name: ty,
            body,
        })
    }

    fn parse_impl_method(&mut self) -> ParserResult<Option<Function>> {
        if self.match_token(TokenKind::Pub) {
            self.errors
                .add_parser_error(ParserError::PubInTraitImpl, self.last_token_span());
        }

        let function = self.parse_function();

        match function {
            Ok(function) => Ok(Some(function)),
            Err((ParserError::InvalidStart, _)) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn parse_module_decl(&mut self, visibility: Visibility) -> ParserResult<Decl> {
        if !self.match_token(TokenKind::Mod) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let name = self.require_identifier()?;

        self.consume_token(
            TokenKind::LeftBrace,
            ParserError::ExpectedToken(TokenKind::LeftBrace),
        )?;

        let mut body = Vec::new();

        let mut result = self.parse_decl()?;

        while result.is_some() {
            body.push(result.unwrap());

            result = self.parse_decl()?;
        }

        self.consume_token(TokenKind::RightBrace, ParserError::ExpectedDecl)?;

        Ok(Decl::Module {
            visibility,
            name,
            body,
        })
    }

    fn parse_function_decl(&mut self, visibility: Visibility) -> ParserResult<Decl> {
        let function = self.parse_function()?;

        Ok(Decl::Function {
            visibility,
            function,
        })
    }

    fn parse_function(&mut self) -> ParserResult<Function> {
        let signature = self.parse_function_signature()?;
        let body = match self.parse_executable_block() {
            Ok(body) => body,
            Err((ParserError::InvalidStart, s)) => {
                return Err((ParserError::ExpectedBody, s));
            }
            Err((err, span)) => {
                return Err((err, span));
            }
        };

        Ok(Function { signature, body })
    }

    fn parse_type_decl(&mut self, visibility: Visibility) -> ParserResult<Decl> {
        if !self.match_token(TokenKind::Type) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let name = self.require_identifier()?;
        let name_span = name.span;

        if self.match_token(TokenKind::Semicolon) {
            return Ok(Decl::Type {
                visibility,
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

        self.consume_token(
            TokenKind::LeftBrace,
            ParserError::ExpectedToken(TokenKind::LeftBrace),
        )?;

        let mut variants = Vec::new();

        let mut variant = self.parse_type_variant()?;

        while variant.is_some() {
            variants.push(variant.unwrap());

            variant = self.parse_type_variant()?;
        }

        if variants.is_empty() {
            variants.push(TypeVariant {
                name: Identifier {
                    span: name_span,
                    name: "self".to_string(),
                },
                fields: Vec::new(),
            });
        }

        let mut methods = Vec::new();

        let mut method = self.parse_type_method()?;

        while method.is_some() {
            methods.push(method.unwrap());
            method = self.parse_type_method()?;
        }

        if let TokenKind::Identifier(_) = self.token_kind() {
            return Err((ParserError::VariantMethodInterweave, self.token_span()));
        }

        self.consume_token(
            TokenKind::RightBrace,
            ParserError::ExpectedToken(TokenKind::RightBrace),
        )?;

        Ok(Decl::Type {
            visibility,
            name,
            body: TypeBody { variants, methods },
        })
    }

    fn parse_type_variant(&mut self) -> ParserResult<Option<TypeVariant>> {
        let variant_name = match self.parse_identifier()? {
            Some(name) => name,
            None => {
                if self.match_token(TokenKind::Self_) {
                    Identifier {
                        span: self.last_token_span().unwrap(),
                        name: "self".to_string(),
                    }
                } else {
                    return Ok(None);
                }
            }
        };

        if self.match_token(TokenKind::Semicolon) {
            Ok(Some(TypeVariant {
                name: variant_name,
                fields: Vec::new(),
            }))
        } else {
            self.consume_token(
                TokenKind::LeftBrace,
                ParserError::ExpectedToken(TokenKind::LeftBrace),
            )?;

            let mut fields = Vec::new();

            let mut field = self.parse_type_variant_field()?;

            while field.is_some() {
                fields.push(field.unwrap());

                if !self.match_token(TokenKind::Comma) {
                    break;
                }

                field = self.parse_type_variant_field()?;
            }

            self.consume_token(
                TokenKind::RightBrace,
                ParserError::ExpectedToken(TokenKind::RightBrace),
            )?;

            Ok(Some(TypeVariant {
                name: variant_name,
                fields,
            }))
        }
    }

    fn parse_type_variant_field(&mut self) -> ParserResult<Option<Field>> {
        let field_name = match self.parse_identifier()? {
            Some(name) => name,
            None => {
                if self.match_token(TokenKind::Self_) {
                    Identifier {
                        span: self.last_token_span().unwrap(),
                        name: "self".to_string(),
                    }
                } else {
                    return Ok(None);
                }
            }
        };

        self.consume_token(
            TokenKind::Colon,
            ParserError::ExpectedToken(TokenKind::Colon),
        )?;

        let field_type = self.parse_type()?;

        Ok(Some(Field {
            name: field_name,
            ty: field_type,
        }))
    }

    fn parse_type_method(&mut self) -> ParserResult<Option<(Visibility, Function)>> {
        if *self.token_kind() != TokenKind::Fn && *self.token_kind() != TokenKind::Pub {
            return Ok(None);
        }

        let visibility = self.parse_visibility()?;

        let function = self.parse_function()?;

        Ok(Some((visibility, function)))
    }

    fn parse_trait_decl(&mut self, visibility: Visibility) -> ParserResult<Decl> {
        if !self.match_token(TokenKind::Trait) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let name = self.require_identifier()?;

        self.consume_token(
            TokenKind::LeftBrace,
            ParserError::ExpectedToken(TokenKind::LeftBrace),
        )?;

        let mut methods = Vec::new();

        let mut method = self.parse_trait_method()?;

        while method.is_some() {
            methods.push(method.unwrap());

            method = self.parse_trait_method()?;
        }

        self.consume_token(
            TokenKind::RightBrace,
            ParserError::ExpectedToken(TokenKind::RightBrace),
        )?;

        Ok(Decl::Trait {
            visibility,
            name,
            body: TraitBody { methods },
        })
    }

    fn parse_trait_method(&mut self) -> ParserResult<Option<FunctionSignature>> {
        if *self.token_kind() != TokenKind::Fn && *self.token_kind() != TokenKind::Pub {
            return Ok(None);
        }

        let function = self.parse_function_signature()?;

        self.consume_token(
            TokenKind::Semicolon,
            ParserError::ExpectedToken(TokenKind::Semicolon),
        )?;

        Ok(Some(function))
    }

    fn parse_function_signature(&mut self) -> ParserResult<FunctionSignature> {
        if !self.match_token(TokenKind::Fn) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let name = self.require_identifier()?;
        self.consume_token(
            TokenKind::LeftParen,
            ParserError::ExpectedToken(TokenKind::LeftParen),
        )?;
        let mut takes_self = false;
        let mut self_precondition = None;
        let mut parameters = Vec::new();

        if self.match_token(TokenKind::Self_) {
            takes_self = true;

            if self.match_token(TokenKind::Question) {
                let precond = self.parse_type()?;
                self_precondition = Some(precond);
            }

            if !self.match_token(TokenKind::Comma) {
                self.consume_token(
                    TokenKind::RightParen,
                    ParserError::ExpectedToken(TokenKind::RightParen),
                )?;

                let return_type = if self.match_token(TokenKind::Arrow) {
                    self.parse_type()?
                } else {
                    TypeReference {
                        kind: TypeReferenceKind::Void,
                        span: None,
                    }
                };

                return Ok(FunctionSignature {
                    name,
                    takes_self,
                    self_precondition,
                    parameters,
                    return_type,
                });
            }
        }

        let mut param = self.parse_function_param()?;

        while param.is_some() {
            if parameters.len() == self.config.max_function_arguments {
                return Err((ParserError::TooManyFunctionParameters, self.token_span()));
            }

            parameters.push(param.unwrap());

            if !self.match_token(TokenKind::Comma) {
                break;
            }

            param = self.parse_function_param()?;
        }

        self.consume_token(
            TokenKind::RightParen,
            ParserError::ExpectedToken(TokenKind::RightParen),
        )?;

        let return_type = if self.match_token(TokenKind::Arrow) {
            self.parse_type()?
        } else {
            TypeReference {
                kind: TypeReferenceKind::Void,
                span: None,
            }
        };

        Ok(FunctionSignature {
            name,
            takes_self,
            self_precondition,
            parameters,
            return_type,
        })
    }

    fn parse_function_param(&mut self) -> ParserResult<Option<Parameter>> {
        let name = match self.parse_identifier()? {
            Some(name) => name,
            None => return Ok(None),
        };

        self.consume_token(
            TokenKind::Colon,
            ParserError::ExpectedToken(TokenKind::Colon),
        )?;
        let ty = self.parse_type()?;

        let kind = if self.match_token(TokenKind::Question) {
            ParameterSpec::Preconditioned(self.parse_type()?)
        } else {
            ParameterSpec::Basic
        };

        Ok(Some(Parameter { name, ty, kind }))
    }

    fn parse_executable_block(&mut self) -> ParserResult<Vec<Stmt>> {
        if !self.match_token(TokenKind::LeftBrace) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }
        let mut stmts = Vec::new();

        let mut stmt = self.parse_stmt()?;

        while stmt.is_some() {
            stmts.push(stmt.unwrap());

            stmt = self.parse_stmt()?;
        }

        self.consume_token(TokenKind::RightBrace, ParserError::ExpectedStmt)?;

        Ok(stmts)
    }

    pub(crate) fn parse_stmt(&mut self) -> ParserResult<Option<Stmt>> {
        let start_span = self.token_span();

        let result = alternatives!(
            self.parse_block_stmt(), // first all the statements that start with a keyword
            self.parse_variable_stmt(),
            self.parse_if_stmt(),
            self.parse_while_stmt(),
            self.parse_for_stmt(),
            self.parse_match_stmt(),
            self.parse_return_stmt(),
            self.parse_break_stmt(),
            self.parse_continue_stmt(),
            self.parse_assignment_or_expr_stmt(), // must be last, catch-all, handles expressions
        );

        match result {
            Ok(kind) => Ok(Some(Stmt {
                kind,
                span: Span::from_spans(start_span.unwrap(), self.last_token_span().unwrap()),
            })),
            Err((ParserError::InvalidStart, _)) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn require_stmt(&mut self) -> ParserResult<Stmt> {
        match self.parse_stmt()? {
            Some(stmt) => Ok(stmt),
            None => Err((ParserError::ExpectedStmt, self.token_span())),
        }
    }

    fn parse_block_stmt(&mut self) -> ParserResult<StmtKind> {
        Ok(StmtKind::Block(self.parse_executable_block()?))
    }

    fn parse_variable_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::Var) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let name = self.require_identifier()?;

        let ty = if self.match_token(TokenKind::Colon) {
            self.parse_type()?
        } else {
            TypeReference {
                kind: TypeReferenceKind::Infer,
                span: None,
            }
        };

        self.consume_token(
            TokenKind::Equal,
            ParserError::ExpectedToken(TokenKind::Equal),
        )?;

        let value = self.require_expr()?;

        self.consume_token(
            TokenKind::Semicolon,
            ParserError::ExpectedToken(TokenKind::Semicolon),
        )?;

        Ok(StmtKind::Variable { name, ty, value })
    }

    fn parse_if_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::If) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let condition = self.require_expr()?;

        let then = self.require_stmt()?;

        let otherwise = if self.match_token(TokenKind::Else) {
            Some(self.require_stmt()?)
        } else {
            None
        };

        Ok(StmtKind::If {
            condition,
            then: Box::new(then),
            otherwise: otherwise.map(Box::new),
        })
    }

    fn parse_while_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::While) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let condition = self.require_expr()?;

        let body = self.require_stmt()?;

        Ok(StmtKind::While {
            condition,
            body: Box::new(body),
        })
    }

    fn parse_for_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::For) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let name = self.require_identifier()?;

        self.consume_token(TokenKind::In, ParserError::ExpectedToken(TokenKind::In))?;

        let iterable = self.require_expr()?;

        let body = self.require_stmt()?;

        Ok(StmtKind::For {
            name,
            iterable,
            body: Box::new(body),
        })
    }

    fn parse_match_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::Match) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let expr = match self.parse_identifier()? {
            Some(expr) => Some(expr),
            None => {
                if self.match_token(TokenKind::Self_) {
                    Some(Identifier {
                        name: "self".to_string(),
                        span: self.last_token_span().unwrap(),
                    })
                } else {
                    None
                }
            }
        };

        self.consume_token(
            TokenKind::LeftBrace,
            ParserError::ExpectedToken(TokenKind::LeftBrace),
        )?;

        let mut arms = Vec::new();

        let mut arm = self.parse_match_arm()?;

        while arm.is_some() {
            arms.push(arm.unwrap());

            arm = self.parse_match_arm()?;
        }

        self.consume_token(
            TokenKind::RightBrace,
            ParserError::ExpectedToken(TokenKind::RightBrace),
        )?;

        Ok(StmtKind::Match { expr, arms })
    }

    fn parse_match_arm(&mut self) -> ParserResult<Option<MatchArm>> {
        if self.match_token(TokenKind::On) {
            let pattern = self.parse_match_pattern()?;

            let body = self.require_stmt()?;

            Ok(Some(MatchArm {
                pattern,
                body: Box::new(body),
            }))
        } else if self.match_token(TokenKind::Else) {
            let body = self.require_stmt()?;

            Ok(Some(MatchArm {
                pattern: MatchPattern::Wildcard,
                body: Box::new(body),
            }))
        } else {
            Ok(None)
        }
    }

    fn parse_match_pattern(&mut self) -> ParserResult<MatchPattern> {
        let expr = self.require_expr()?;

        let type_ref = expr_to_type_ref(&expr);

        if self.match_token(TokenKind::Pipe) {
            if type_ref.is_none() {
                return Err((ParserError::ExpectedType, Some(expr.span)));
            } else {
                let mut types = vec![type_ref.unwrap()];
                while self.match_token(TokenKind::Pipe) {
                    let ty = self.parse_type()?;
                    types.push(ty);
                }
                return Ok(MatchPattern::Type(TypeReference {
                    kind: TypeReferenceKind::Union(types),
                    span: Some(Span::from_spans(expr.span, self.last_token_span().unwrap())),
                }));
            }
        }

        Ok(MatchPattern::Expr(expr))
    }

    fn parse_return_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::Return) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let return_span = self.last_token_span().unwrap();

        let expr = match self.parse_expr()? {
            Some(expr) => expr,
            None => Expr {
                kind: ExprKind::Literal(Literal::Void),
                span: return_span,
            },
        };

        self.consume_token(
            TokenKind::Semicolon,
            ParserError::ExpectedToken(TokenKind::Semicolon),
        )?;

        Ok(StmtKind::Return(expr))
    }

    fn parse_break_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::Break) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        self.consume_token(
            TokenKind::Semicolon,
            ParserError::ExpectedToken(TokenKind::Semicolon),
        )?;

        Ok(StmtKind::Break)
    }

    fn parse_continue_stmt(&mut self) -> ParserResult<StmtKind> {
        if !self.match_token(TokenKind::Continue) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        self.consume_token(
            TokenKind::Semicolon,
            ParserError::ExpectedToken(TokenKind::Semicolon),
        )?;

        Ok(StmtKind::Continue)
    }

    fn parse_assignment_or_expr_stmt(&mut self) -> ParserResult<StmtKind> {
        let expr = match self.parse_expr()? {
            Some(expr) => expr,
            None => {
                self.consume_token(TokenKind::Semicolon, ParserError::InvalidStart)?;

                let last_token_span = self.last_token_span().unwrap();
                return Ok(StmtKind::Expr(Expr {
                    kind: ExprKind::Literal(Literal::Void),
                    span: last_token_span,
                }));
            }
        };

        if self.match_token(TokenKind::Semicolon) {
            return Ok(StmtKind::Expr(expr));
        }

        self.consume_token(
            TokenKind::Equal,
            ParserError::ExpectedToken(TokenKind::Equal),
        )?;

        let value = self.require_expr()?;

        self.consume_token(
            TokenKind::Semicolon,
            ParserError::ExpectedToken(TokenKind::Semicolon),
        )?;

        Ok(StmtKind::Assign {
            target: expr,
            value,
        })
    }

    pub(crate) fn parse_expr(&mut self) -> ParserResult<Option<Expr>> {
        let result = self.parse_logical_or_expr();

        match result {
            Ok(expr) => Ok(Some(expr)),
            Err((ParserError::InvalidStart, _)) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn require_expr(&mut self) -> ParserResult<Expr> {
        match self.parse_expr()? {
            Some(expr) => Ok(expr),
            None => Err((ParserError::ExpectedExpr, self.token_span())),
        }
    }

    fn parse_binary_left_assoc_expr(
        &mut self,
        mut next_level: impl FnMut(&mut Self) -> ParserResult<Expr>,
        pairs: &[(TokenKind, BinaryOp)],
    ) -> ParserResult<Expr> {
        let mut expr = next_level(self)?;

        loop {
            if self.is_at_end {
                break;
            }
            let token = self.token_kind();

            if pairs.iter().any(|(k, _)| *k == *token) {
                let op = pairs.iter().find(|(k, _)| *k == *token).unwrap().1.clone();
                self.next_token();
                let right = next_level(self)?;
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

        Ok(expr)
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
        let mut expr = self.parse_comparison_expr()?;
        loop {
            let token = self.token_kind();
            if *token == (TokenKind::Is) {
                self.next_token();
                let ty = self.parse_type()?;
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
        Ok(expr)
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
        let mut expr = self.parse_unary_expr()?;
        loop {
            let token = self.token_kind();
            if *token == TokenKind::As {
                self.next_token();
                let ty = self.parse_type()?;
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
        Ok(expr)
    }

    fn parse_unary_expr(&mut self) -> ParserResult<Expr> {
        let mut ops_spans = vec![];

        let mut start_span = self.token_span();

        loop {
            let op = if self.match_token(TokenKind::Bang) {
                UnaryOp::Not
            } else if self.match_token(TokenKind::Minus) {
                UnaryOp::Neg
            } else {
                break;
            };

            ops_spans.push((op, start_span.unwrap()));
            start_span = self.token_span();
        }

        let mut expr = self.parse_primary_expr()?;

        for (op, s) in ops_spans.into_iter().rev() {
            let expr_span = expr.span;
            expr = Expr {
                kind: ExprKind::Unary {
                    op,
                    expr: Box::new(expr),
                },
                span: Span::from_spans(s, expr_span),
            };
        }

        Ok(expr)
    }

    fn parse_primary_expr(&mut self) -> ParserResult<Expr> {
        alternatives!(
            self.parse_grouping_expr(),
            self.parse_literal_expr(),
            self.parse_identifier_starting_expr(),
        )
    }

    fn parse_grouping_expr(&mut self) -> ParserResult<Expr> {
        if !self.match_token(TokenKind::LeftParen) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }
        let paren_span = self.last_token_span().unwrap();

        let mut expr = self.require_expr()?;

        self.consume_token(
            TokenKind::RightParen,
            ParserError::ExpectedToken(TokenKind::RightParen),
        )?;

        let span = Span::from_spans(paren_span, self.last_token_span().unwrap());

        expr.span = span;

        Ok(expr)
    }

    fn parse_literal_expr(&mut self) -> ParserResult<Expr> {
        let start_span = self.token_span();
        let literal = self.parse_literal()?;
        Ok(Expr {
            kind: ExprKind::Literal(literal),
            span: Span::from_spans(start_span.unwrap(), self.last_token_span().unwrap()),
        })
    }

    fn parse_identifier_starting_expr(&mut self) -> ParserResult<Expr> {
        if !matches!(self.token_kind(), TokenKind::Identifier(_))
            && !matches!(self.token_kind(), TokenKind::Self_)
        {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let name = if self.match_token(TokenKind::Self_) {
            NamespacedIdentifier {
                namespace: vec![],
                ident: Identifier {
                    name: "self".to_string(),
                    span: self.last_token_span().unwrap(),
                },
            }
        } else {
            self.parse_namespaced_identifier()?
        };

        if let Some(expr) = self.parse_constructor(&name)? {
            return Ok(expr);
        }

        let name_span = name.span();

        let mut expr = Expr {
            kind: ExprKind::Variable(name),
            span: name_span,
        };

        let mut result = self.parse_variable_followup(expr)?;

        while result.is_ok() {
            expr = result.unwrap();
            result = self.parse_variable_followup(expr)?;
        }

        expr = result.unwrap_err();

        Ok(expr)
    }

    fn parse_constructor(&mut self, name: &NamespacedIdentifier) -> ParserResult<Option<Expr>> {
        if !self.match_token(TokenKind::Arrow) {
            return Ok(None);
        }

        self.consume_token(
            TokenKind::LeftBrace,
            ParserError::ExpectedToken(TokenKind::LeftBrace),
        )?;

        let mut fields = vec![];

        let mut field_name = self.parse_identifier()?;

        while field_name.is_some() {
            if self.match_token(TokenKind::Colon) {
                let expr = self.require_expr()?;

                fields.push(FieldConstructor::Named {
                    name: field_name.unwrap(),
                    value: expr,
                });
            } else {
                fields.push(FieldConstructor::Auto(field_name.unwrap()));
            }

            if !self.match_token(TokenKind::Comma) {
                break;
            }

            field_name = self.parse_identifier()?;
        }

        self.consume_token(
            TokenKind::RightBrace,
            ParserError::ExpectedToken(TokenKind::RightBrace),
        )?;

        Ok(Some(Expr {
            kind: ExprKind::Constructor {
                ty: name.clone(),
                fields,
            },
            span: Span::from_spans(name.span(), self.last_token_span().unwrap()),
        }))
    }

    fn parse_variable_followup(&mut self, expr: Expr) -> ParserResult<Result<Expr, Expr>> {
        if self.match_token(TokenKind::Dot) {
            let field = self.require_identifier()?;
            let field_span = field.span;
            let expr_span = expr.span;

            Ok(Ok(Expr {
                kind: ExprKind::Access {
                    expr: Box::new(expr),
                    field,
                },
                span: Span::from_spans(expr_span, field_span),
            }))
        } else if self.match_token(TokenKind::LeftParen) {
            let mut args = vec![];

            let mut arg = self.parse_expr()?;

            while arg.is_some() {
                args.push(arg.unwrap());

                if !self.match_token(TokenKind::Comma) {
                    break;
                }

                arg = self.parse_expr()?;
            }

            self.consume_token(
                TokenKind::RightParen,
                ParserError::ExpectedToken(TokenKind::RightParen),
            )?;

            let closing_paren_span = self.last_token_span().unwrap();
            let expr_span = expr.span;

            match expr.kind {
                ExprKind::Variable(name) => Ok(Ok(Expr {
                    kind: ExprKind::FunctionCall { name, args },
                    span: Span::from_spans(expr_span, closing_paren_span),
                })),
                ExprKind::Access { expr, field } => Ok(Ok(Expr {
                    kind: ExprKind::MethodCall {
                        expr,
                        method: field,
                        args,
                    },
                    span: Span::from_spans(expr_span, closing_paren_span),
                })),
                _ => Err((ParserError::BadCall, Some(expr.span))),
            }
        } else if self.match_token(TokenKind::LeftBracket) {
            let index = self.require_expr()?;

            self.consume_token(
                TokenKind::RightBracket,
                ParserError::ExpectedToken(TokenKind::RightBracket),
            )?;

            let closing_brace_span = self.last_token_span().unwrap();
            let expr_span = expr.span;

            Ok(Ok(Expr {
                kind: ExprKind::Index {
                    expr: Box::new(expr),
                    index: Box::new(index),
                },
                span: Span::from_spans(expr_span, closing_brace_span),
            }))
        } else {
            Ok(Err(expr))
        }
    }

    fn parse_literal(&mut self) -> ParserResult<Literal> {
        alternatives!(self.parse_list_literal(), self.parse_single_token_literal(),)
    }

    fn parse_list_literal(&mut self) -> ParserResult<Literal> {
        if !self.match_token(TokenKind::LeftBracket) {
            return Err((ParserError::InvalidStart, self.token_span()));
        }

        let mut elements = vec![];

        let mut elem = self.parse_expr()?;

        while elem.is_some() {
            elements.push(elem.unwrap());

            if !self.match_token(TokenKind::Comma) {
                break;
            }

            elem = self.parse_expr()?;
        }

        self.consume_token(
            TokenKind::RightBracket,
            ParserError::ExpectedToken(TokenKind::RightBracket),
        )?;

        Ok(Literal::List(elements))
    }

    fn parse_single_token_literal(&mut self) -> ParserResult<Literal> {
        let result = match self.token.as_ref().unwrap().kind {
            TokenKind::Integer(i) => Ok(Literal::Int(i)),
            TokenKind::Float(f) => Ok(Literal::Float(f)),
            TokenKind::String(ref s) => Ok(Literal::String(s.clone())),
            TokenKind::Char(c) => Ok(Literal::Char(c)),
            TokenKind::True => Ok(Literal::Bool(true)),
            TokenKind::False => Ok(Literal::Bool(false)),
            TokenKind::Void => Ok(Literal::Void),
            _ => Err((ParserError::InvalidStart, self.token_span())),
        };

        match result {
            Ok(lit) => {
                self.next_token();
                Ok(lit)
            }
            _ => result,
        }
    }

    pub(crate) fn parse_type(&mut self) -> ParserResult<TypeReference> {
        let mut tys = vec![self.parse_type_primary()?];

        while self.match_token(TokenKind::Pipe) {
            let ty = self.parse_type_primary()?;

            tys.push(ty);
        }

        if tys.len() == 1 {
            Ok(tys.pop().unwrap())
        } else {
            let tys_span = Span::from_spans(
                tys.first().unwrap().span.unwrap(),
                tys.last().unwrap().span.unwrap(),
            );
            Ok(TypeReference {
                kind: TypeReferenceKind::Union(tys),
                span: Some(tys_span),
            })
        }
    }

    fn parse_type_primary(&mut self) -> ParserResult<TypeReference> {
        if self.match_token(TokenKind::Void) {
            Ok(TypeReference {
                kind: TypeReferenceKind::Void,
                span: self.last_token_span(),
            })
        } else if self.match_token(TokenKind::LeftBracket) {
            let start_span = self.last_token_span().unwrap();
            let ty = self.parse_type()?;
            self.consume_token(
                TokenKind::RightBracket,
                ParserError::ExpectedToken(TokenKind::RightBracket),
            )?;
            let end_span = self.last_token_span().unwrap();

            Ok(TypeReference {
                kind: TypeReferenceKind::List(Box::new(ty)),
                span: Some(Span::from_spans(start_span, end_span)),
            })
        } else {
            let ty = self.parse_namespaced_identifier()?;
            let ty_span = ty.span();

            Ok(TypeReference {
                kind: TypeReferenceKind::Named(ty),
                span: Some(ty_span),
            })
        }
    }

    fn parse_namespaced_identifier(&mut self) -> ParserResult<NamespacedIdentifier> {
        let mut ident = match self.parse_identifier()? {
            Some(ident) => ident,
            None => {
                return Err((ParserError::InvalidStart, self.token_span()));
            }
        };

        let mut namespace = Vec::new();

        while self.match_token(TokenKind::DoubleColon) {
            namespace.push(ident);

            ident = self.require_identifier()?;
        }

        Ok(NamespacedIdentifier { namespace, ident })
    }

    fn parse_identifier(&mut self) -> ParserResult<Option<Identifier>> {
        if let TokenKind::Identifier(name) = self.token_kind().clone() {
            let span = self.token_span().unwrap();
            self.next_token();
            Ok(Some(Identifier { name, span }))
        } else {
            Ok(None)
        }
    }

    fn require_identifier(&mut self) -> ParserResult<Identifier> {
        match self.parse_identifier()? {
            Some(ident) => Ok(ident),
            None => Err((ParserError::ExpectedIdentifier, self.token_span())),
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
        if self.is_at_end {
            return self.last_span;
        }
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
            Ok(())
        } else {
            Err((err, self.token_span()))
        }
    }
}
