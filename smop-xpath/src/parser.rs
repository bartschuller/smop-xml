use crate::ast::{Expr, Literal};
use crate::xdm::QName;
use pest_consume::match_nodes;
use pest_consume::Error;
use pest_consume::Parser;
use rust_decimal::Decimal;
use std::borrow::Cow;
use std::str::FromStr;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(Parser)]
#[grammar = "parser.pest"]
pub struct XpathParser;

#[pest_consume::parser]
impl XpathParser {
    fn Xpath(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Expr(expr), EOI(_)] => expr,
        ))
    }
    fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }
    fn Expr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [ExprSingle(expr)..] => {
                let mut v: Vec<Expr> = expr.collect();
                if v.len() == 1 {
                    v.remove(0)
                } else {
                    Expr::Sequence(v)
                }
            }
        ))
    }
    fn ExprSingle(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Literal(lit)] => Expr::Literal(lit),
            [IfExpr(e)] => e,
            [ParenthesizedExpr(e)] => e,
            [ContextItemExpr(e)] => e,
            [FunctionCall(e)] => e,
        ))
    }
    fn ParenthesizedExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Expr(e)] => e,
            [] => Expr::Sequence(vec![])
        ))
    }
    fn ContextItemExpr(_input: Node) -> Result<Expr> {
        Ok(Expr::ContextItem)
    }
    fn FunctionCall(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [EQName(f), ExprSingle(a)..] => Expr::FunctionCall(f, a.collect()),
        ))
    }
    fn EQName(input: Node) -> Result<QName> {
        Ok(match_nodes!(input.into_children();
            [URIQualifiedName(q)] => q,
            [QName(q)] => q,
        ))
    }
    fn URIQualifiedName(input: Node) -> Result<QName> {
        Ok(match_nodes!(input.into_children();
            [BracedURILiteralContent(ns), NCName(name)] => QName::new(name, Some(ns), None),
        ))
    }
    fn BracedURILiteralContent(input: Node) -> Result<&str> {
        Ok(input.as_str())
    }
    fn NCName(input: Node) -> Result<&str> {
        Ok(input.as_str())
    }
    fn QName(input: Node) -> Result<QName> {
        Ok(match_nodes!(input.into_children();
            [UnprefixedName(q)] => q,
        ))
    }
    fn UnprefixedName(input: Node) -> Result<QName> {
        Ok(QName::new(input.as_str(), None, None))
    }
    fn Literal(input: Node) -> Result<Literal> {
        Ok(match_nodes!(input.into_children();
            [DoubleLiteral(lit)] => lit,
            [DecimalLiteral(lit)] => lit,
            [IntegerLiteral(lit)] => lit,
            [StringLiteral(lit)] => lit,
        ))
    }
    fn IntegerLiteral(input: Node) -> Result<Literal> {
        Ok(Literal::Integer(input.as_str().parse().unwrap()))
    }
    fn DoubleLiteral(input: Node) -> Result<Literal> {
        Ok(Literal::Double(input.as_str().parse().unwrap()))
    }
    fn DecimalLiteral(input: Node) -> Result<Literal> {
        Ok(Literal::Decimal(Decimal::from_str(input.as_str()).unwrap()))
    }
    fn StringLiteral(input: Node) -> Result<Literal> {
        // we matched the complete string literal, with outer quotes and possibly escaped quotes.
        // Get the inner string and unescape where needed.
        let str = input.as_str();
        let last = str.len() - 1;
        // we want to know which of the 2 possible quote characters were used.
        let quote = &str[0..1];
        let qchar = quote.chars().nth(0).unwrap();
        let mut two_quotes = String::new();
        two_quotes.push(qchar);
        two_quotes.push(qchar);
        let inner = &str[1..last];
        let unescaped = if inner.contains(quote) {
            Cow::Owned(inner.replace(two_quotes.as_str(), quote))
        } else {
            Cow::Borrowed(inner)
        };
        Ok(Literal::String(unescaped))
    }
    fn IfExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Expr(i), ExprSingle(t), ExprSingle(e)] => Expr::IfThenElse(Box::new(i), Box::new(t), Box::new(e)),
        ))
    }
}

pub fn p3_parse(input: &str) -> Result<Expr> {
    let root = XpathParser::parse(Rule::Xpath, input);
    let root2 = root.clone();
    pest_ascii_tree::print_ascii_tree(root2.map(|n| n.as_pairs().to_owned()));

    let root = root?.single()?;
    let parse = XpathParser::Xpath(root);
    parse.map_err(|e| {
        e.with_path("literal string").renamed_rules(|rule| {
            match *rule {
                Rule::EOI => "end of input",
                Rule::Literal => "a literal",
                _ => {
                    println!("unhandled grammar prettifier: {:?}", rule);
                    "UNHANDLED"
                }
            }
            .to_owned()
        })
    })
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Literal};
    use crate::parser::p3_parse;
    use rust_decimal::Decimal;
    use std::borrow::Cow;
    use std::str::FromStr;

    #[test]
    fn int_literal1() {
        let output = p3_parse("foo");
        assert!(output.is_err())
    }

    #[test]
    fn int_literal2() {
        let output = p3_parse("1234");
        assert_eq!(output, Ok(Expr::Literal(Literal::Integer(1234))))
    }

    #[test]
    fn decimal_literal1() {
        let output = p3_parse("1234.56789");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::Decimal(
                Decimal::from_str("1234.56789").unwrap()
            )))
        )
    }

    #[test]
    fn string_literal1() {
        let output = p3_parse("'foo'");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String(Cow::Borrowed("foo"))))
        )
    }

    #[test]
    fn string_literal2() {
        let output = p3_parse("\"foo\"");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String(Cow::Borrowed("foo"))))
        )
    }

    #[test]
    fn string_literal3() {
        let output = p3_parse("'foo''bar'");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String(Cow::Borrowed("foo'bar"))))
        )
    }

    #[test]
    fn string_literal4() {
        let output = p3_parse("\"foo\"\"bar\"");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String(Cow::Borrowed("foo\"bar"))))
        )
    }

    #[test]
    fn string_literal5() {
        let output = p3_parse("\"foo''bar\"");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String(Cow::Borrowed("foo''bar"))))
        )
    }

    #[test]
    fn comment1() {
        let output = p3_parse("(::)()");
        assert_eq!(output, Ok(Expr::Sequence(vec![])))
    }
    #[test]
    fn comment2() {
        let output = p3_parse("(: foobar :)()");
        assert_eq!(output, Ok(Expr::Sequence(vec![])))
    }
    #[test]
    fn comment3() {
        let output = p3_parse("(: (: :)");
        assert!(output.is_err())
    }
    #[test]
    fn iws1() {
        let output = p3_parse(" \n\n(: FIXME :)\r\n (: \n :)\n()");
        assert_eq!(output, Ok(Expr::Sequence(vec![])))
    }
    #[test]
    fn sequence1() {
        let output = p3_parse("1,'two'");
        assert_eq!(
            output,
            Ok(Expr::Sequence(vec![
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::String(Cow::Borrowed("two")))
            ]))
        )
    }

    #[test]
    fn if_then_else1() {
        let output = p3_parse("if (3) then 1 else 2");
        assert_eq!(
            output,
            Ok(Expr::IfThenElse(
                Box::new(Expr::Literal(Literal::Integer(3))),
                Box::new(Expr::Literal(Literal::Integer(1))),
                Box::new(Expr::Literal(Literal::Integer(2)))
            ))
        )
    }
    #[test]
    #[ignore]
    fn if_then_else2() {
        let output = p3_parse("if(3)then1else2");
        assert!(output.is_err())
    }

    #[test]
    fn error1() {
        let output = p3_parse("1,'two");
        assert!(output.is_err())
    }

    #[test]
    fn whitespace1() {
        let output = p3_parse("1, 2");
        assert_eq!(
            output,
            Ok(Expr::Sequence(vec![
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::Integer(2))
            ]))
        )
    }

    #[test]
    fn whitespace2() {
        let output = p3_parse("(:here we go: :) 1 (: one :), 2 (: that's it :)\n");
        assert_eq!(
            output,
            Ok(Expr::Sequence(vec![
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::Integer(2))
            ]))
        )
    }

    #[test]
    fn bool1() {
        let output = p3_parse("true()");
        assert!(!output.is_err())
    }

    #[test]
    fn fn1() {
        let output = p3_parse("Q{http://example.com/}myfunc()");
        assert!(!output.is_err())
    }
}
