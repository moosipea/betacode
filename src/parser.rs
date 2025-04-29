use std::error::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace1,
    combinator::{eof, opt, peek, value},
    multi::many0,
    IResult, Parser,
};

mod chars {
    pub const INTERPUNCT: &'static str = "\u{00b7}";
    pub const GREEK_QUESTION_MARK: &'static str = "\u{037e}";
    pub const SINGLE_QUOTE_CLOSING: &'static str = "\u{2019}";
    pub const EM_DASH: &'static str = "\u{2014}";
    pub const PRIME: &'static str = "\u{02b9}";
    pub const HYPHEN: &'static str = "\u{2010}";

    pub const SMOOTH_BREATHING: &'static str = "\u{0313}";
    pub const ROUGH_BREATHING: &'static str = "\u{0314}";

    pub const ACUTE_ACCENT: &'static str = "\u{0301}";
    pub const CIRCUMFLEX_ACCENT: &'static str = "\u{0342}";
    pub const GRAVE_ACCENT: &'static str = "\u{0300}";
    pub const DIAERESIS: &'static str = "\u{0308}";
    pub const IOTA_SUBSCRIPT: &'static str = "\u{0345}";
    pub const MACRON: &'static str = "\u{0304}";
    pub const BREVE: &'static str = "\u{0306}";
    pub const COMBINING_DOT_BELOW: &'static str = "\u{0323}";

    pub const ALPHA: &str = "α";
    pub const BETA: &str = "β";
    pub const GAMMA: &str = "γ";
    pub const DELTA: &str = "δ";
    pub const EPSILON: &str = "ε";
    pub const ZETA: &str = "ζ";
    pub const ETA: &str = "η";
    pub const THETA: &str = "θ";
    pub const IOTA: &str = "ι";
    pub const KAPPA: &str = "κ";
    pub const LAMBDA: &str = "λ";
    pub const MU: &str = "μ";
    pub const NU: &str = "ν";
    pub const XI: &str = "ξ";
    pub const OMICRON: &str = "ο";
    pub const PI: &str = "π";
    pub const RHO: &str = "ρ";
    pub const SIGMA: &str = "σ";
    pub const FINAL_SIGMA: &str = "ς";
    pub const TAU: &str = "τ";
    pub const UPSILON: &str = "υ";
    pub const PHI: &str = "φ";
    pub const CHI: &str = "χ";
    pub const PSI: &str = "ψ";
    pub const OMEGA: &str = "ω";

    pub const DIGAMMA: &str = "ϝ";
    pub const LUNATE_SIGMA: &str = "ϲ";
}

fn parse_end_of_word(input: &str) -> IResult<&str, &str> {
    alt((eof, multispace1, parse_punctuation)).parse(input)
}

fn parse_sigma(input: &str) -> IResult<&str, &str> {
    alt((
        value(chars::LUNATE_SIGMA, tag("s3")),
        value(chars::FINAL_SIGMA, alt((tag("s2"), tag("j")))),
        value(chars::FINAL_SIGMA, (tag("s"), peek(parse_end_of_word))),
        value(chars::SIGMA, alt((tag("s1"), tag("s")))),
    ))
    .parse(input)
}

fn parse_raw_letter(input: &str) -> IResult<&str, &str> {
    alt([
        value(chars::ALPHA, tag("a")),
        value(chars::BETA, tag("b")),
        value(chars::GAMMA, tag("g")),
        value(chars::DELTA, tag("d")),
        value(chars::EPSILON, tag("e")),
        value(chars::ZETA, tag("z")),
        value(chars::ETA, tag("h")),
        value(chars::THETA, tag("q")),
        value(chars::IOTA, tag("i")),
        value(chars::KAPPA, tag("k")),
        value(chars::LAMBDA, tag("l")),
        value(chars::MU, tag("m")),
        value(chars::NU, tag("n")),
        value(chars::XI, tag("c")),
        value(chars::OMICRON, tag("o")),
        value(chars::PI, tag("p")),
        value(chars::RHO, tag("r")),
        value(chars::TAU, tag("t")),
        value(chars::UPSILON, tag("u")),
        value(chars::PHI, tag("f")),
        value(chars::CHI, tag("x")),
        value(chars::PSI, tag("y")),
        value(chars::OMEGA, tag("w")),
        value(chars::DIGAMMA, tag("v")),
    ])
    .or(parse_sigma)
    .parse(input)
}

struct OtherDiacritics<'a> {
    breathing_or_diaeresis: &'a str,
    length: &'a str,
    accent: &'a str,
    dot_below: &'a str, // Wtf is this
}

impl OtherDiacritics<'_> {
    fn parse(input: &str) -> IResult<&str, OtherDiacritics<'_>> {
        let (input, breathing_or_diaeresis) = opt(alt((
            tag(")").map(|_| chars::SMOOTH_BREATHING),
            tag("(").map(|_| chars::ROUGH_BREATHING),
            tag("+").map(|_| chars::DIAERESIS),
        )))
        .parse(input)?;

        let (input, length) = opt(alt((
            tag("&").map(|_| chars::MACRON),
            tag("'").map(|_| chars::BREVE),
        )))
        .parse(input)?;

        let (input, accent) = opt(alt((
            tag("/").map(|_| chars::ACUTE_ACCENT),
            tag("\\").map(|_| chars::GRAVE_ACCENT),
            tag("=").map(|_| chars::CIRCUMFLEX_ACCENT),
        )))
        .parse(input)?;

        let (input, dot_below) = opt(tag("?").map(|_| chars::COMBINING_DOT_BELOW)).parse(input)?;

        IResult::Ok((
            input,
            OtherDiacritics {
                breathing_or_diaeresis: breathing_or_diaeresis.unwrap_or_default(),
                length: length.unwrap_or_default(),
                accent: accent.unwrap_or_default(),
                dot_below: dot_below.unwrap_or_default(),
            },
        ))
    }
}

fn parse_iota_subscript(input: &str) -> IResult<&str, &str> {
    tag("|").map(|_| chars::IOTA_SUBSCRIPT).parse(input)
}

fn parse_lowercase_letter(input: &str) -> IResult<&str, String> {
    let (input, letter) = parse_raw_letter(input)?;
    let (input, other_diacritics) = OtherDiacritics::parse(input)?;
    let (input, iota_subscript) = opt(parse_iota_subscript)
        .map(Option::unwrap_or_default)
        .parse(input)?;

    let result = [
        letter,
        other_diacritics.breathing_or_diaeresis,
        other_diacritics.length,
        other_diacritics.accent,
        other_diacritics.dot_below,
        iota_subscript,
    ]
    .concat();

    IResult::Ok((input, result))
}

fn parse_uppercase_letter(input: &str) -> IResult<&str, String> {
    let (input, _) = tag("*")(input)?;
    let (input, other_diacritics) = OtherDiacritics::parse(input)?;
    let (input, letter) = parse_raw_letter(input)?;
    let (input, iota_subscript) = opt(parse_iota_subscript)
        .map(Option::unwrap_or_default)
        .parse(input)?;

    let result = [
        letter,
        other_diacritics.breathing_or_diaeresis,
        other_diacritics.length,
        other_diacritics.accent,
        other_diacritics.dot_below,
        iota_subscript,
    ]
    .concat();

    IResult::Ok((input, result))
}

fn parse_letter(input: &str) -> IResult<&str, String> {
    alt((parse_uppercase_letter, parse_lowercase_letter)).parse(input)
}

fn parse_punctuation(input: &str) -> IResult<&str, &str> {
    alt((
        tag("."),
        tag(","),
        tag(":").map(|_| chars::INTERPUNCT),
        tag(";").map(|_| chars::GREEK_QUESTION_MARK),
        tag("'").map(|_| chars::SINGLE_QUOTE_CLOSING),
        tag("_").map(|_| chars::EM_DASH),
        tag("#").map(|_| chars::PRIME),
        tag("-").map(|_| chars::HYPHEN),
    ))
    .parse(input)
}

enum Token<'a> {
    Owned(String),
    Borrowed(&'a str),
}

pub fn parse_betacode(input: &str) -> Result<String, Box<dyn Error + '_>> {
    let (_, tokens) = many0(alt((
        multispace1.map(Token::Borrowed),
        parse_punctuation.map(Token::Borrowed),
        parse_letter.map(Token::Owned),
    )))
    .parse_complete(input)?;

    let mut accumulator = String::new();
    for token in tokens {
        match token {
            Token::Owned(inner) => accumulator.push_str(&inner),
            Token::Borrowed(inner) => accumulator.push_str(inner),
        }
    }

    Ok(accumulator)
}
