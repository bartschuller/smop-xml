use crate::ast::{Expr, Literal};
use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, take_until};
use nom::character::complete::{alphanumeric1, anychar, char, digit0, digit1, none_of};
use nom::combinator::{all_consuming, map, not, opt, recognize, value};
use nom::error::{convert_error, ParseError, VerboseError};
use nom::multi::{many0, many1, many_till, separated_nonempty_list};
use nom::sequence::{delimited, preceded, tuple};
use nom::Err::{Error, Failure, Incomplete};
use nom::{Compare, IResult, InputLength, InputTake};
use rust_decimal::Decimal;
use std::str::FromStr;

pub(crate) fn parse(input: &str) -> Result<Expr, String> {
    let output = xpath_expr::<VerboseError<&str>>(input);
    match output {
        Ok((_, e)) => Ok(e),
        Err(Error(e)) | Err(Failure(e)) => Err(convert_error(input, e)),
        Err(Incomplete(_)) => unreachable!(),
    }
}

fn xpath_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    all_consuming(delimited(iws0, expr, iws0))(input)
}

// 6
fn expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    map(
        separated_nonempty_list(iws0_tag(","), expr_single),
        |mut v| {
            if v.len() == 1 {
                v.remove(0)
            } else {
                Expr::Sequence(v)
            }
        },
    )(input)
}

// 7
fn expr_single<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    primary_expr(input)
}

// 15
fn if_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    //  	IfExpr 	   ::=    	"if" "(" Expr ")" "then" ExprSingle "else" ExprSingle
    map(
        tuple((
            iws0_tag("if"),
            iws0_tag("("),
            expr,
            iws0_tag(")"),
            iws0_tag("then"),
            expr_single,
            iws0_tag("else"),
            expr_single,
        )),
        |(_, _, cond_expr, _, _, then_expr, _, else_expr)| {
            Expr::IfThenElse(
                Box::new(cond_expr),
                Box::new(then_expr),
                Box::new(else_expr),
            )
        },
    )(input)
}

// 56
fn primary_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    map(literal, Expr::Literal)(input)
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
        Literal::String,
    )(input)
}

// 121
fn comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (), E> {
    // "(:" (CommentContents | Comment)* ":)"
    value(
        (),
        delimited(
            tag("(:"),
            many0(alt((comment_contents, comment))),
            tag(":)"),
        ),
    )(input)
}

// 126
fn comment_contents<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (), E> {
    // (Char+ - (Char* ('(:' | ':)') Char*))
    value(
        (),
        many1(preceded(not(alt((tag("(:"), tag(":)")))), anychar)),
    )(input)
}

// Ignorable whitespace, including comments
fn iws0<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (), E> {
    value((), many0(alt((value((), is_a(" \t\r\n")), comment))))(input)
}

// Ignorable whitespace, including comments
fn iws1<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (), E> {
    value((), many1(alt((value((), is_a(" \t\r\n")), comment))))(input)
}

// Like tag("foo") but surrounded by iws
fn iws0_tag<'a, Error: ParseError<&'a str>>(
    t: &'a str,
) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, Error> {
    move |i| delimited(iws0, tag(t), iws0)(i)
}

// optional ws, but fail if next is alphanumeric
fn ws_if_alphanum<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (), E> {
    // FIXME
    preceded(not(alphanumeric1), iws0)(input)
}

fn non_delimiting_terminal<'a, Error: ParseError<&'a str>>(
    t: &'a str,
) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, Error> {
    move |i| delimited(iws0, tag(t), ws_if_alphanum)(i)
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Literal};
    use crate::parser::{
        comment, decimal_literal, expr, if_expr, integer_literal, iws0, literal, numeric_literal,
        xpath_expr,
    };
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::anychar;
    use nom::combinator::{not, value};
    use nom::error::ErrorKind::Digit;
    use nom::error::{convert_error, ErrorKind, VerboseError};
    use nom::multi::many1;
    use nom::sequence::preceded;
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

    #[test]
    fn comment1() {
        let input = "(::)";
        let output = comment::<(&str, ErrorKind)>(input);
        assert_eq!(output, Ok(("", ())))
    }
    #[test]
    fn comment2() {
        let input = "(: FIXME :)";
        let output = comment::<(&str, ErrorKind)>(input);
        assert_eq!(output, Ok(("", ())))
    }
    #[test]
    fn comment3() {
        let input = "(: (: :)";
        let output = comment::<VerboseError<&str>>(input);
        let err = match output {
            Err(Error(e)) | Err(Failure(e)) => convert_error(input, e),
            _ => unreachable!(),
        };
        assert_eq!(err, "0: at line 1, in Tag:\n(: (: :)\n        ^\n\n")
    }
    #[test]
    fn iws1() {
        let input = " \n\n(: FIXME :)\r\n (: \n :)\nFOO";
        let output = iws0::<(&str, ErrorKind)>(input);
        assert_eq!(output, Ok(("FOO", ())))
    }
    #[test]
    fn sequence1() {
        let input = "1,'two'";
        let output = expr::<(&str, ErrorKind)>(input);
        assert_eq!(
            output,
            Ok((
                "",
                Expr::Sequence(vec![
                    Expr::Literal(Literal::Integer(1)),
                    Expr::Literal(Literal::String("two".to_string()))
                ])
            ))
        )
    }

    #[test]
    fn if_then_else1() {
        let input = "if (3) then 1 else 2";
        let output = if_expr::<(&str, ErrorKind)>(input);
        assert_eq!(
            output,
            Ok((
                "",
                Expr::IfThenElse(
                    Box::new(Expr::Literal(Literal::Integer(3))),
                    Box::new(Expr::Literal(Literal::Integer(1))),
                    Box::new(Expr::Literal(Literal::Integer(2)))
                )
            ))
        )
    }
    #[test]
    #[ignore]
    fn if_then_else2() {
        let input = "if(3)then1else2";
        let output = if_expr::<(&str, ErrorKind)>(input);
        assert_eq!(output, Err(Error(("then1else2", ErrorKind::Tag))))
    }

    #[test]
    fn error1() {
        let input = "1,'two";
        let output = xpath_expr::<(&str, ErrorKind)>(input);
        assert_eq!(output, Err(Error((",'two", ErrorKind::Eof))))
    }

    #[test]
    fn error2() {
        let input = "1,'two";
        let output = xpath_expr::<VerboseError<&str>>(input);
        let err = match output {
            Err(Error(e)) | Err(Failure(e)) => convert_error(input, e),
            _ => unreachable!(),
        };
        assert_eq!(err, "0: at line 1, in Eof:\n1,\'two\n ^\n\n")
    }

    #[test]
    fn whitespace1() {
        let input = "1, 2";
        let output = expr::<(&str, ErrorKind)>(input);
        assert_eq!(
            output,
            Ok((
                "",
                Expr::Sequence(vec![
                    Expr::Literal(Literal::Integer(1)),
                    Expr::Literal(Literal::Integer(2))
                ])
            ))
        )
    }

    #[test]
    fn whitespace2() {
        let input = "(:here we go: :) 1 (: one :), 2 (: that's it :)\n";
        let output = xpath_expr::<(&str, ErrorKind)>(input);
        assert_eq!(
            output,
            Ok((
                "",
                Expr::Sequence(vec![
                    Expr::Literal(Literal::Integer(1)),
                    Expr::Literal(Literal::Integer(2))
                ])
            ))
        )
    }
}
