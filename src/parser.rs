use std::fmt::{Display, Formatter};
use std::ops::RangeBounds;
use std::ops::RangeInclusive;

#[derive(Debug, Clone)]
pub enum ParserError {
    EmptyInput,
    NotFound {
        expected: String,
        found: String,
        remain: String,
    },
}

pub trait Parser<V> = for<'a> Fn(&'a str) -> Result<(V, &'a str), ParserError>;

pub fn parse_char(match_char: char) -> impl Parser<char> {
    move |input| {
        if input.is_empty() {
            Err(ParserError::EmptyInput)
        } else if input.chars().next().expect("Unexpected empty str") == match_char {
            Ok((match_char, &input[1..]))
        } else {
            Err(ParserError::NotFound {
                expected: [match_char].into_iter().collect(),
                found: [input.chars().next().expect("Unexpected empty str")]
                    .into_iter()
                    .collect(),
                remain: input.to_owned(),
            })
        }
    }
}

pub fn parse_string(match_string: impl Into<String>) -> impl Parser<String> {
    let m: String = match_string.into();
    move |input: &str| {
        let mut remain = input;
        for char in m.chars() {
            match parse_char(char)(remain) {
                Ok((_, rem)) => {
                    remain = rem;
                }
                Err(ParserError::NotFound { found, remain, .. }) => {
                    return Err(ParserError::NotFound {
                        expected: m.clone(),
                        found: [found].into_iter().collect::<String>(),
                        remain,
                    });
                }
                Err(ParserError::EmptyInput) => return Err(ParserError::EmptyInput),
            }
        }
        Ok((m.clone(), remain))
    }
}

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

fn or_else<V>(parser1: impl Parser<V>, parser2: impl Parser<V>) -> impl Parser<Vec<V>> {
    move |input| {
        if let Ok((res, remain)) = parser1(input) {
            Ok((vec![res], remain))
        } else {
            let (res, remain) = parser2(input)?;
            Ok((vec![res], remain))
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::EmptyInput => write!(f, "Empty input"),
            ParserError::NotFound {
                expected,
                found,
                remain,
            } => write!(
                f,
                "Expected: {}, Found: {}, Remain: {}",
                expected, found, remain
            ),
        }
    }
}

impl std::error::Error for ParserError {}

#[derive(Clone)]
pub enum CharMatch {
    Char(char),
    CharRange(RangeInclusive<char>),
}

pub fn match_any_of(match_chars: Vec<CharMatch>) -> impl Parser<String> {
    move |input| {
        if input.is_empty() {
            Err(ParserError::EmptyInput)
        } else {
            let found = input.chars().next().expect("Unexpected empty str");
            for match_char in &match_chars {
                match match_char {
                    CharMatch::Char(c) => {
                        if *c == found {
                            return Ok((format!("{}", found), &input[1..]));
                        }
                    }
                    CharMatch::CharRange(range) => {
                        if range.contains(&found) {
                            return Ok((format!("{}", found), &input[1..]));
                        }
                    }
                }
            }
            Err(ParserError::NotFound {
                expected: match_chars
                    .iter()
                    .map(|c| match c {
                        CharMatch::Char(c) => c.to_string(),
                        CharMatch::CharRange(range) => {
                            format!("{}-{}", range.start(), range.end())
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", "),
                found: [found].into_iter().collect(),
                remain: input.to_owned(),
            })
        }
    }
}

pub fn match_until_err<'a>(parser: impl Parser<String>) -> impl Parser<String> {
    move |input| {
        let mut input = input;
        let mut parsed = String::new();
        while let Ok((p, remain)) = parser(input) {
            parsed.push_str(&p);
            input = remain;
        }

        Ok((parsed, input))
    }
}
