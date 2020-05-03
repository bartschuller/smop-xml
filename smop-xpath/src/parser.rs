use crate::ast::{ArithmeticOp, Expr, Literal};
use crate::types::{Item, Occurrence, SequenceType};
use crate::xdm::QName;
use pest_consume::match_nodes;
use pest_consume::Error;
use pest_consume::Parser;
use rust_decimal::Decimal;
use std::borrow::Cow;
use std::str::FromStr;

pub type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, &'i StaticContext<'i>>;

#[derive(Default)]
pub struct StaticContext<'a> {
    pub _dummy: &'a str,
}
impl<'a, 'i> StaticContext<'a>
where
    'i: 'a,
{
    pub fn parse(&self, input: &'i str) -> Result<Expr> {
        let root = XpathParser::parse_with_userdata(Rule::Xpath, input, self);
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
}

#[derive(Parser)]
#[grammar = "parser.pest"]
pub struct XpathParser;

#[pest_consume::parser]
impl XpathParser {
    pub fn Xpath(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Expr(expr), EOI(_)] => expr,
        ))
    }
    fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }
    fn Expr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [ExprSingle(expr)] => expr,
            [ExprSingle(expr)..] => {
                    Expr::Sequence(expr.collect())
            }
        ))
    }
    fn ExprSingle(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [IfExpr(e)] => e,
            [OrExpr(e)] => e,
        ))
    }
    fn OrExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [AndExpr(expr)] => expr,
            [AndExpr(expr)..] => {
                    Expr::Or(expr.collect())
            }
        ))
    }
    fn AndExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [ComparisonExpr(expr)] => expr,
            [ComparisonExpr(expr)..] => {
                    Expr::And(expr.collect())
            }
        ))
    }
    fn ComparisonExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [StringConcatExpr(e)] => e, // FIXME
        ))
    }
    fn StringConcatExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [RangeExpr(e)] => e, // FIXME
        ))
    }
    fn RangeExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [AdditiveExpr(e)] => e, // FIXME
        ))
    }
    //   A - B + C - D
    // is equivalent to
    //   ((A - B) + C) - D
    fn AdditiveExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [MultiplicativeExpr(e)] => e,
            [MultiplicativeExpr(e1), AdditiveExpr1(oe)..] => {
                oe.fold(e1, |a, (op, b)| Expr::Arithmetic(Box::new(a), op, Box::new(b)))
            }
        ))
    }
    fn AdditiveExpr1(input: Node) -> Result<(ArithmeticOp, Expr)> {
        Ok(match_nodes!(input.into_children();
            [AdditiveOp(op), MultiplicativeExpr(e)] => (op, e),
        ))
    }
    fn AdditiveOp(input: Node) -> Result<ArithmeticOp> {
        Ok(match input.as_str() {
            "+" => ArithmeticOp::Plus,
            "-" => ArithmeticOp::Minus,
            &_ => unreachable!(),
        })
    }
    fn MultiplicativeExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [UnionExpr(e)] => e, // FIXME
        ))
    }
    fn UnionExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [IntersectExceptExpr(e)] => e, // FIXME
        ))
    }
    fn IntersectExceptExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [InstanceofExpr(e)] => e, // FIXME
        ))
    }
    fn InstanceofExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [TreatExpr(e)] => e,
            [TreatExpr(e), SequenceType(t)] => Expr::InstanceOf(Box::new(e), t),
        ))
    }
    fn TreatExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [CastableExpr(e)] => e, // FIXME
        ))
    }
    fn CastableExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [CastExpr(e)] => e, // FIXME
        ))
    }
    fn CastExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [ArrowExpr(e)] => e, // FIXME
        ))
    }
    fn ArrowExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [UnaryExpr(e)] => e, // FIXME
        ))
    }
    fn UnaryExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [SimpleMapExpr(e)] => e, // FIXME
        ))
    }
    fn SimpleMapExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [PathExpr(e)] => e, // FIXME
        ))
    }
    fn PathExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [RelativePathExpr(e)] => e, // FIXME
        ))
    }
    fn RelativePathExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [StepExpr(e)] => e, // FIXME
        ))
    }
    fn StepExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [PostfixExpr(e)] => e, // FIXME
            [AxisStep(e)] => e, // FIXME
        ))
    }
    fn AxisStep(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [ReverseStep(r), PredicateList(p)] => todo!("handle AxisStep"), // FIXME
            [ForwardStep(f), PredicateList(p)] => todo!("handle AxisStep"), // FIXME
        ))
    }
    fn ReverseStep(_input: Node) -> Result<()> {
        todo!("handle ReverseStep")
    }
    fn ForwardStep(_input: Node) -> Result<()> {
        todo!("handle ForwardStep")
    }
    fn PredicateList(_input: Node) -> Result<()> {
        todo!("handle PredicateList")
    }
    fn PostfixExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [PrimaryExpr(e)] => e, // FIXME
        ))
    }
    fn SequenceType(input: Node) -> Result<SequenceType> {
        Ok(match_nodes!(input.into_children();
            [EmptySequence(st)] => st,
            [ItemType(it)] => SequenceType::Item(it, Occurrence::One),
            [ItemType(it), OccurrenceIndicator(oi)] => SequenceType::Item(it, oi),
        ))
    }
    fn EmptySequence(_input: Node) -> Result<SequenceType> {
        Ok(SequenceType::EmptySequence)
    }
    fn ItemType(input: Node) -> Result<Item> {
        Ok(match_nodes!(input.into_children();
            [Item(it)] => it,
            [AtomicOrUnionType(it)] => it,
        ))
    }
    fn Item(_input: Node) -> Result<Item> {
        Ok(Item::Item)
    }
    fn AtomicOrUnionType(input: Node) -> Result<Item> {
        Ok(match_nodes!(input.into_children();
            [EQName(qname)] => Item::Item,
        ))
    }
    fn OccurrenceIndicator(input: Node) -> Result<Occurrence> {
        Ok(match input.as_str() {
            "?" => Occurrence::Optional,
            "*" => Occurrence::ZeroOrMore,
            "+" => Occurrence::OneOrMore,
            &_ => unreachable!(),
        })
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
            [PrefixedName(q)] => q,
            [UnprefixedName(q)] => q,
        ))
    }
    fn PrefixedName(input: Node) -> Result<QName> {
        Ok(match_nodes!(input.into_children();
            [Prefix(p), LocalPart(l)] => QName::new(l, None, Some(p)),
        ))
    }
    fn Prefix(input: Node) -> Result<&str> {
        Ok(input.as_str())
    }
    fn LocalPart(input: Node) -> Result<&str> {
        Ok(input.as_str())
    }
    fn UnprefixedName(input: Node) -> Result<QName> {
        Ok(QName::new(input.as_str(), None, None))
    }
    fn PrimaryExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Literal(lit)] => Expr::Literal(lit),
            [ParenthesizedExpr(e)] => e,
            [ContextItemExpr(e)] => e,
            [FunctionCall(e)] => e,
        ))
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

#[cfg(test)]
mod tests {
    use super::StaticContext;
    use crate::ast::{ArithmeticOp, Expr, Literal};
    use crate::types::SequenceType;
    use crate::xdm::QName;
    use rust_decimal::Decimal;
    use std::borrow::Cow;
    use std::str::FromStr;

    #[test]
    fn int_literal1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1234");
        assert_eq!(output, Ok(Expr::Literal(Literal::Integer(1234))))
    }

    #[test]
    fn decimal_literal1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1234.56789");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::Decimal(
                Decimal::from_str("1234.56789").unwrap()
            )))
        )
    }

    #[test]
    fn string_literal1() {
        let context: StaticContext = Default::default();
        let output = context.parse("'foo'");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String(Cow::Borrowed("foo"))))
        )
    }

    #[test]
    fn string_literal2() {
        let context: StaticContext = Default::default();
        let output = context.parse("\"foo\"");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String(Cow::Borrowed("foo"))))
        )
    }

    #[test]
    fn string_literal3() {
        let context: StaticContext = Default::default();
        let output = context.parse("'foo''bar'");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String(Cow::Borrowed("foo'bar"))))
        )
    }

    #[test]
    fn string_literal4() {
        let context: StaticContext = Default::default();
        let output = context.parse("\"foo\"\"bar\"");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String(Cow::Borrowed("foo\"bar"))))
        )
    }

    #[test]
    fn string_literal5() {
        let context: StaticContext = Default::default();
        let output = context.parse("\"foo''bar\"");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String(Cow::Borrowed("foo''bar"))))
        )
    }

    #[test]
    fn comment1() {
        let context: StaticContext = Default::default();
        let output = context.parse("(::)()");
        assert_eq!(output, Ok(Expr::Sequence(vec![])))
    }
    #[test]
    fn comment2() {
        let context: StaticContext = Default::default();
        let output = context.parse("(: foobar :)()");
        assert_eq!(output, Ok(Expr::Sequence(vec![])))
    }
    #[test]
    fn comment3() {
        let context: StaticContext = Default::default();
        let output = context.parse("(: (: :)");
        assert!(output.is_err())
    }
    #[test]
    fn iws1() {
        let context: StaticContext = Default::default();
        let output = context.parse(" \n\n(: FIXME :)\r\n (: \n :)\n()");
        assert_eq!(output, Ok(Expr::Sequence(vec![])))
    }
    #[test]
    fn sequence1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1,'two'");
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
        let context: StaticContext = Default::default();
        let output = context.parse("if (3) then 1 else 2");
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
        let context: StaticContext = Default::default();
        let output = context.parse("if(3)then1else2");
        assert!(output.is_err())
    }

    #[test]
    fn error1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1,'two");
        assert!(output.is_err())
    }

    #[test]
    fn whitespace1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1, 2");
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
        let context: StaticContext = Default::default();
        let output = context.parse("(:here we go: :) 1 (: one :), 2 (: that's it :)\n");
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
        let context: StaticContext = Default::default();
        let output = context.parse("true()");
        assert!(!output.is_err())
    }

    #[test]
    fn fn1() {
        let context: StaticContext = Default::default();
        let output = context.parse("Q{http://example.com/}myfunc(., 1)");
        assert_eq!(
            output,
            Ok(Expr::FunctionCall(
                QName::new("myfunc", Some("http://example.com/"), None),
                vec![Expr::ContextItem, Expr::Literal(Literal::Integer(1))]
            ))
        )
    }

    #[test]
    fn or1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1 or 0");
        assert_eq!(
            output,
            Ok(Expr::Or(vec![
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::Integer(0))
            ]))
        )
    }
    #[test]
    fn and1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1 and 0");
        assert_eq!(
            output,
            Ok(Expr::And(vec![
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::Integer(0))
            ]))
        )
    }
    #[test]
    fn or_and1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1 or 2 and 0");
        assert_eq!(
            output,
            Ok(Expr::Or(vec![
                Expr::Literal(Literal::Integer(1)),
                Expr::And(vec![
                    Expr::Literal(Literal::Integer(2)),
                    Expr::Literal(Literal::Integer(0))
                ])
            ]))
        )
    }
    #[test]
    fn arith1() {
        let context: StaticContext = Default::default();
        use ArithmeticOp::{Minus, Plus};
        use Expr::{Arithmetic, Literal as Lit};
        use Literal::Integer;
        let output = context.parse("1 - 2 + 3 - 4");
        assert_eq!(
            output,
            Ok(Arithmetic(
                Box::new(Arithmetic(
                    Box::new(Arithmetic(
                        Box::new(Lit(Integer(1))),
                        Minus,
                        Box::new(Lit(Integer(2)))
                    )),
                    Plus,
                    Box::new(Lit(Integer(3)))
                )),
                Minus,
                Box::new(Lit(Integer(4)))
            ))
        )
    }
    #[test]
    fn instance_of1() {
        let context: StaticContext = Default::default();
        let output = context.parse(". instance of xs:integer");
        assert_eq!(output, Ok(Expr::ContextItem))
    }
}
