use crate::token::{Operator, Span, SpannedToken, Token};

pub struct Lexer {}

impl Lexer {
    pub fn lex(&self, input: &str) -> Vec<SpannedToken> {
        let mut start_index = 0;
        let mut end_index = 1;

        let mut line_num = 0;
        let mut position = 0;

        let mut tokens: Vec<SpannedToken> = Vec::new();
        let mut string = false;

        let mut line_comment = false;
        let mut block_comment = false;

        let input_len = input.len();

        while start_index < input_len && end_index <= input_len {
            // let ustart_index = input.char_indices().map(|(i, _)| i).nth(start_index).unwrap();
            // let uend_index = input.char_indices().map(|(i, _)| i).nth(end_index).unwrap();

            let sub_str = &input[start_index..(input.ceil_char_boundary(end_index))];
            // let sub_str = unsafe { input.get_unchecked(start_index..end_index) };
            // let sub_str = get_utf8_slice(input, start_index, end_index).expect("Unable to slice unicode string!");

            // let next =
            let next = if string {
                None
            } else {
                // input.chars().nth(end_index)
                if end_index + 1 <= input.len() {
                    let end = input.ceil_char_boundary(end_index + 1);
                    if end <= input.len() {
                        input[input.floor_char_boundary(end_index)..end]
                            .chars()
                            .next()
                    } else {
                        None
                    }
                } else {
                    None
                }
            };

            if let (Some(token), lex_length) = self.try_lex(sub_str, next, string) {
                match token {
                    Token::Operator(Operator::BlockCommentClose) if block_comment => block_comment = false,
                    Token::Newline if line_comment => {
                        if end_index + 1 < input_len && &input[start_index..end_index + 1] == "\r\n"
                        {
                            start_index += 1;
                        }

                        line_num += 1;
                        position = 0;

                        line_comment = false
                    }
                    _ if block_comment || line_comment => (),
                    Token::Operator(Operator::BlockCommentOpen) => block_comment = true,
                    Token::Operator(Operator::LineComment) => line_comment = true,
                    Token::Operator(Operator::Quote) => string = true,
                    Token::String => {
                        let tok = self.parse_template(
                            &sub_str[0..sub_str.len() - 1],
                            line_num,
                            position,
                            tokens.len() as _,
                        );

                        let token = SpannedToken::new(tok.1, tok.0);

                        position += token.0.length;
                        tokens.push(token);

                        string = false
                    }
                    Token::Whitespace => position += 1,
                    Token::Newline => {
                        if end_index + 1 < input_len && &input[start_index..end_index + 1] == "\r\n"
                        {
                            start_index += 1;
                        }

                        line_num += 1;
                        position = 0;
                    }
                    Token::Ident(_) => {
                        let token = SpannedToken::new(
                            token,
                            Span {
                                line_num,
                                position,
                                length: (end_index - start_index) as u32,
                                token_index: tokens.len() as u32,
                            },
                        );

                        tokens.push(token);
                        position += (end_index - start_index) as u32;
                    }
                    token => {
                        let length = lex_length.unwrap_or(end_index - start_index) as u32;
                        let token = SpannedToken::new(
                            token,
                            Span {
                                line_num,
                                position,
                                length,
                                token_index: tokens.len() as u32,
                            },
                        );

                        tokens.push(token);
                        position += length;

                        start_index += length as usize;
                        end_index = start_index + 1;

                        continue;
                    }
                }

                start_index += sub_str.len();
                end_index = start_index + 1;
            } else {
                // println!("{}", input[end_index-1..end_index].len());
                end_index += (sub_str.len() - sub_str.chars().count()) + 1;
                // end_index += 1;
            }
        }

        tokens.push(SpannedToken::new(
            Token::Newline,
            Span {
                line_num,
                position,
                length: 1,
                token_index: tokens.len() as u32,
            },
        ));

        tokens
    }

    pub fn try_lex(
        &self,
        input: &str,
        next: Option<char>,
        string: bool,
    ) -> (Option<Token>, Option<usize>) {
        if string {
            if let Some('\'') = input.chars().last() {
                return (Some(Token::String), None);
            }

            return (None, None);
        }

        if input.len() == 1 {
            // match single character symbols
            match input.chars().next() {
                Some('[') => return (Some(Token::Operator(Operator::OpenSquare)), None),
                Some(']') => return (Some(Token::Operator(Operator::CloseSquare)), None),
                Some('(') => return (Some(Token::Operator(Operator::OpenParen)), None),
                Some(')') => return (Some(Token::Operator(Operator::CloseParen)), None),
                Some('{') => return (Some(Token::Operator(Operator::OpenBrace)), None),
                Some('}') => return (Some(Token::Operator(Operator::CloseBrace)), None),
                Some('<') => return (Some(Token::Operator(Operator::OpenAngle)), None),
                Some('>') => return (Some(Token::Operator(Operator::CloseAngle)), None),

                Some(':') => return (Some(Token::Operator(Operator::Colon)), None),
                Some('.') => match next {
                    Some('.') => return (None, None),
                    _ => return (Some(Token::Operator(Operator::Dot)), None),
                },
                Some(',') => return (Some(Token::Operator(Operator::Comma)), None),

                Some('+') => return (Some(Token::Operator(Operator::Plus)), None),
                Some('-') => match next {
                    Some('>') => return (None, None),
                    _ => return (Some(Token::Operator(Operator::Minus)), None),
                },
                Some('*') => match next {
                    Some('*') => return (None, None),
                    _ => return (Some(Token::Operator(Operator::Multiply)), None),
                },
                Some('|') => match next {
                    Some('|') => return (None, None),
                    _ => return (Some(Token::Operator(Operator::Pipe)), None),
                },
                Some('&') => match next {
                    Some('&') => return (None, None),
                    _ => return (Some(Token::Operator(Operator::Ampersand)), None),
                },
                Some('/') => match next {
                    Some('/') => return (None, None),
                    _ => return (Some(Token::Operator(Operator::Divide)), None),
                },

                Some('!') => return (Some(Token::Operator(Operator::Exclamation)), None),
                Some('@') => return (Some(Token::Operator(Operator::At)), None),
                Some('#') => return (Some(Token::Operator(Operator::Pound)), None),
                Some('$') => return (Some(Token::Operator(Operator::Dollar)), None),
                Some('%') => return (Some(Token::Operator(Operator::Percent)), None),
                Some('^') => return (Some(Token::Operator(Operator::Carot)), None),
                Some(';') => return (Some(Token::Operator(Operator::SemiColon)), None),
                Some('~') => return (Some(Token::Operator(Operator::Tilde)), None),
                Some('`') => return (Some(Token::Operator(Operator::BackTick)), None),
                // Some('\'') => return (Some(Token::Operator(Operator::SingleQuote)), None),
                Some('\'') => return (Some(Token::Operator(Operator::Quote)), None),
                Some('?') => return (Some(Token::Operator(Operator::Question)), None),

                Some('=') => return (Some(Token::Operator(Operator::Equals)), None),

                Some('\r' | '\n') => return (Some(Token::Newline), None),
                Some(c) if c.is_whitespace() => return (Some(Token::Whitespace), None),
                _ => (),
            }
        }

        let mut chars = input.chars();

        match (chars.next(), chars.next(), chars.next()) {
            (Some('*'), Some('*'), _) => return (Some(Token::Operator(Operator::Exponent)), None),
            (Some('|'), Some('|'), _) => return (Some(Token::Operator(Operator::Or)), None),
            (Some('&'), Some('&'), _) => return (Some(Token::Operator(Operator::And)), None),
            (Some('-'), Some('>'), _) => return (Some(Token::Operator(Operator::Arrow)), None),

            (Some('/'), Some('/'), _) => {
                return (Some(Token::Operator(Operator::LineComment)), None)
            }
            (Some('/'), Some('*'), _) => {
                return (Some(Token::Operator(Operator::BlockCommentOpen)), None)
            }
            (Some('*'), Some('/'), _) => {
                return (Some(Token::Operator(Operator::BlockCommentClose)), None)
            }

            (Some('.'), Some('.'), Some('.')) => {
                return (Some(Token::Operator(Operator::TripleDot)), None)
            }
            (Some('.'), Some('.'), Some('=')) => {
                return (Some(Token::Operator(Operator::TripleDot)), None)
            }
            (Some('.'), Some('.'), _) => match next {
                Some('.' | '=') => return (None, None),
                _ => return (Some(Token::Operator(Operator::DoubleDot)), None),
            },
            _ => (),
        }

        let del = next.map(|c| !(c.is_numeric() || c == '.')).unwrap_or(true);

        let count = input
            .chars()
            .fold(0u8, |acc, c| if c == '.' { 1 + acc } else { acc });

        if count == 1 && next == Some('.') {
            let val = input[..input.len() - 1].parse().unwrap_or(0.0f64);
            return (Some(Token::Float(val)), None);
        }

        if !input.chars().any(|c| !(c.is_numeric() || c == '.')) && count <= 1 && del {
            if count == 1 {
                let val = input.parse().unwrap_or(0.0f64);
                return (Some(Token::Float(val)), None);
            } else {
                let val = input.parse().unwrap_or(0u64);
                return (Some(Token::Integer(val)), None);
            }
        }

        // If the next character is a delimeter
        let del = next
            .map(|c| !(c.is_alphanumeric() || c == '_'))
            .unwrap_or(true);

        // match identifiers
        if input
            .chars()
            .next()
            .filter(|f| f.is_alphabetic() || *f == '_')
            .is_some()
            & !input.chars().any(|c| !(c.is_alphanumeric() || c == '_'))
            && del
        {
            return (Some(Token::Ident(input.to_string())), None);
        }

        (None, None)
    }

    pub fn parse_template(
        &self,
        input: &str,
        line: u32,
        mut position: u32,
        token_index: u32,
    ) -> (Span, Token) {
        let mut iindex = 0;
        let mut last_index = 0;
        let mut last_span = Span::default();
        let mut open = 0;

        let mut toks = Vec::new();
        let sindex = position + 1;

        for i in 0..input.chars().count() {
            let c = input.chars().nth(i).unwrap();

            if c == '}' {
                open -= 1;
            }

            if c == '{' && open == 0 {
                let st = input[last_index..i].to_string();
                let len = st.len();
                last_span = Span {
                    line_num: line,
                    position: position + len as u32,
                    length: 1,
                    token_index,
                };

                let token = SpannedToken::new(
                    Token::Ident(st),
                    Span {
                        line_num: line,
                        position: position + 1,
                        length: len as u32,
                        token_index,
                    },
                );

                position += len as u32 + 1;

                toks.push(Template::String(token));
                iindex = i;
                open += 1
            } else if c == '}' && open == 0 {
                let st = &input[iindex + 1..i];
                let utoks = self.lex(st);
                let tok_len = utoks.len();
                let p = utoks
                    .into_iter()
                    .map(|f| {
                        SpannedToken::new(
                            f.1,
                            Span {
                                line_num: f.0.line_num + line,
                                position: f.0.position + position + 1, // + 2 for { and "
                                length: f.0.length,
                                token_index,
                            },
                        )
                    })
                    .take(tok_len)
                    .collect::<Vec<_>>();

                last_index = i + 1;
                position += st.len() as u32 + 2;

                toks.push(Template::Template(
                    p,
                    SpannedToken::new(Token::Operator(Operator::OpenBrace), last_span),
                    SpannedToken::new(
                        Token::Operator(Operator::CloseBrace),
                        Span {
                            line_num: line,
                            position: position - 1,
                            length: 1,
                            token_index,
                        },
                    ),
                ));
            }
        }

        {
            let st = input[last_index..input.len()].to_string();
            let len = st.len();

            let token = SpannedToken::new(
                Token::Ident(st),
                Span {
                    line_num: line,
                    position,
                    length: len as u32,
                    token_index,
                },
            );

            toks.push(Template::String(token));
        }

        (
            Span {
                length: input.len() as u32 + 2,
                line_num: line,
                position: sindex - 1,
                token_index,
            },
            Token::TemplateString(toks),
        )
    }
}

#[derive(Clone, Debug)]
pub enum Template {
    String(SpannedToken),
    Template(Vec<SpannedToken>, SpannedToken, SpannedToken),
}