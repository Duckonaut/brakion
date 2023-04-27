use crate::{
    lexer::TokenProducer,
    tokens::{Token, TokenKind},
};

pub struct ParserTokenFilter<'u, T>
where
    T: TokenProducer,
{
    tokens: &'u mut T,
}

impl<'u, T> ParserTokenFilter<'u, T>
where
    T: TokenProducer,
{
    pub fn new(tokens: &'u mut T) -> Self {
        Self { tokens }
    }
}

impl<'u, T> TokenProducer for ParserTokenFilter<'u, T>
where
    T: TokenProducer,
{
    fn next(&mut self) -> Option<Token> {
        let mut token = self.tokens.next();

        while let Some(Token {
            kind: TokenKind::Comment(_),
            ..
        }) = token
        {
            token = self.tokens.next();
        }

        token
    }
}
