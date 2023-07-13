use std::fmt::{Display, Formatter};
use std::ops::RangeInclusive;

#[derive(Debug, Clone)]
pub enum ParserError {
    EmptyInput,
    NotFound,
}

pub trait Parser<V>: Fn(&str) -> Result<(V, &str), ParserError> {}

impl<V, T: Fn(&str) -> Result<(V, &str), ParserError>> Parser<V> for T {}

pub fn and_then<V>(parser1: impl Parser<V>, parser2: impl Parser<V>) -> impl Parser<Vec<V>> {
    move |input| {
        let (res1, remain1) = parser1(input)?;
        let (res2, remain2) = parser2(remain1)?;
        Ok((vec![res1, res2], remain2))
    }
}

pub fn map<V, K>(parser: impl Parser<V>, f: impl Fn((V, &str)) -> (K, &str)) -> impl Parser<K> {
    move |input| Ok(f(parser(input)?))
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::EmptyInput => write!(f, "Empty input"),
            ParserError::NotFound => write!(f, "Not found",),
        }
    }
}

impl std::error::Error for ParserError {}

#[derive(Clone)]
pub enum CharMatch {
    Char(char),
    CharRange(RangeInclusive<char>),
}

pub fn match_any_of(match_chars: &[CharMatch]) -> impl Parser<()> + '_ {
    move |input| {
        if input.is_empty() {
            Err(ParserError::EmptyInput)
        } else {
            let found = input.chars().next().expect("Unexpected empty str");
            for match_char in match_chars {
                match match_char {
                    CharMatch::Char(c) => {
                        if *c == found {
                            return Ok(((), &input[1..]));
                        }
                    }
                    CharMatch::CharRange(range) => {
                        if range.contains(&found) {
                            return Ok(((), &input[1..]));
                        }
                    }
                }
            }

            Err(ParserError::NotFound)
        }
    }
}

pub fn match_until_err<'a>(parser: impl Parser<()>) -> impl Parser<()> {
    move |input| {
        let mut input = input;
        while let Ok((_, remain)) = parser(input) {
            input = remain;
        }

        Ok(((), input))
    }
}
