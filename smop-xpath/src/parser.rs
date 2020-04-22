use crate::ast::NumericLiteral;
use crate::ast::NumericLiteral::IntegerLiteral;
use nom::character::complete::digit1;
use nom::combinator::map;
use nom::error::ParseError;
use nom::IResult;

// 113
fn parse_integer_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, NumericLiteral, E> {
    map(digit1, |s: &str| IntegerLiteral(s.parse::<i64>().unwrap()))(input)
}

#[cfg(test)]
mod tests {
    use crate::ast::NumericLiteral::IntegerLiteral;
    use crate::parser::parse_integer_literal;
    use nom::error::ErrorKind::Digit;
    use nom::error::{convert_error, ErrorKind, VerboseError};
    use nom::Err::{Error, Failure};

    #[test]
    fn int_literal1() {
        let input = "foo";
        let output = parse_integer_literal::<(&str, ErrorKind)>(input);
        assert_eq!(output, Err(Error(("foo", Digit))))
    }

    #[test]
    fn int_literal2() {
        let input = "foo";
        let output = parse_integer_literal::<VerboseError<&str>>(input);
        let err = match output {
            Err(Error(e)) | Err(Failure(e)) => convert_error(input, e),
            _ => unreachable!(),
        };
        assert_eq!(err, "0: at line 1, in Digit:\nfoo\n^\n\n")
    }

    #[test]
    fn int_literal3() {
        let input = "1234";
        let output = parse_integer_literal::<(&str, ErrorKind)>(input);
        assert_eq!(output, Ok(("", IntegerLiteral(1234))))
    }
}
