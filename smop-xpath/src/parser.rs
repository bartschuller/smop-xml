use crate::ast::{Literal, PrimaryExpr};
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag};
use nom::character::complete::{char, digit0, digit1};
use nom::combinator::{map, recognize, value};
use nom::error::ParseError;
use nom::multi::many0;
use nom::sequence::{delimited, tuple};
use nom::IResult;
use rust_decimal::Decimal;
use std::str::FromStr;

// 56
fn primary_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PrimaryExpr, E> {
    map(literal, |l| PrimaryExpr::Literal(l))(input)
}

// 57
fn literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Literal, E> {
    alt((numeric_literal, string_literal))(input)
}
// 58
fn numeric_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Literal, E> {
    alt((decimal_literal, integer_literal))(input)
}
// 113
fn integer_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Literal, E> {
    map(digit1, |s: &str| {
        Literal::Integer(s.parse::<i64>().unwrap())
    })(input)
}

// 114
fn decimal_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Literal, E> {
    // ("." Digits) | (Digits "." [0-9]*)
    map(
        alt((
            recognize(tuple((char('.'), digit1))),
            recognize(tuple((digit1, char('.'), digit0))),
        )),
        |s: &str| Literal::Decimal(Decimal::from_str(s).unwrap()),
    )(input)
}

// 116
// ('"' (EscapeQuot | [^"])* '"') | ("'" (EscapeApos | [^'])* "'")
fn string_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Literal, E> {
    map(
        alt((
            delimited(
                tag("\""),
                map(
                    many0(alt((value("\"", tag("\"\"")), is_not("\"")))),
                    |v: Vec<&str>| v.concat(),
                ),
                tag("\""),
            ),
            delimited(
                tag("'"),
                map(
                    many0(alt((value("'", tag("''")), is_not("'")))),
                    |v: Vec<&str>| v.concat(),
                ),
                tag("'"),
            ),
        )),
        |s| Literal::String(s),
    )(input)
}

#[cfg(test)]
mod tests {
    use crate::ast::Literal;
    use crate::parser::{decimal_literal, integer_literal, literal, numeric_literal};
    use nom::error::ErrorKind::Digit;
    use nom::error::{convert_error, ErrorKind, VerboseError};
    use nom::Err::{Error, Failure};
    use rust_decimal::Decimal;
    use std::str::FromStr;

    #[test]
    fn int_literal1() {
        let input = "foo";
        let output = integer_literal::<(&str, ErrorKind)>(input);
        assert_eq!(output, Err(Error(("foo", Digit))))
    }

    #[test]
    fn int_literal2() {
        let input = "foo";
        let output = integer_literal::<VerboseError<&str>>(input);
        let err = match output {
            Err(Error(e)) | Err(Failure(e)) => convert_error(input, e),
            _ => unreachable!(),
        };
        assert_eq!(err, "0: at line 1, in Digit:\nfoo\n^\n\n")
    }

    #[test]
    fn int_literal3() {
        let input = "1234";
        let output = integer_literal::<(&str, ErrorKind)>(input);
        assert_eq!(output, Ok(("", Literal::Integer(1234))))
    }

    #[test]
    fn decimal_literal1() {
        let input = "1234.56789";
        let output = decimal_literal::<(&str, ErrorKind)>(input);
        assert_eq!(
            output,
            Ok((
                "",
                Literal::Decimal(Decimal::from_str("1234.56789").unwrap())
            ))
        )
    }

    #[test]
    fn numeric_literal1() {
        let input = "1234.56789";
        let output = numeric_literal::<(&str, ErrorKind)>(input);
        assert_eq!(
            output,
            Ok((
                "",
                Literal::Decimal(Decimal::from_str("1234.56789").unwrap())
            ))
        )
    }

    #[test]
    fn numeric_literal2() {
        let input = "1234";
        let output = numeric_literal::<(&str, ErrorKind)>(input);
        assert_eq!(output, Ok(("", Literal::Integer(1234))))
    }

    #[test]
    fn string_literal1() {
        let input = "'foo'";
        let output = literal::<(&str, ErrorKind)>(input);
        assert_eq!(output, Ok(("", Literal::String("foo".to_string()))))
    }

    #[test]
    fn string_literal2() {
        let input = "\"foo\"";
        let output = literal::<(&str, ErrorKind)>(input);
        assert_eq!(output, Ok(("", Literal::String("foo".to_string()))))
    }

    #[test]
    fn string_literal3() {
        let input = "'foo''bar'";
        let output = literal::<(&str, ErrorKind)>(input);
        assert_eq!(output, Ok(("", Literal::String("foo'bar".to_string()))))
    }

    #[test]
    fn string_literal4() {
        let input = "\"foo\"\"bar\"";
        let output = literal::<(&str, ErrorKind)>(input);
        assert_eq!(output, Ok(("", Literal::String("foo\"bar".to_string()))))
    }

    #[test]
    fn string_literal5() {
        let input = "\"foo''bar\"";
        let output = literal::<(&str, ErrorKind)>(input);
        assert_eq!(output, Ok(("", Literal::String("foo''bar".to_string()))))
    }
}
