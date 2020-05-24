use crate::ast::{ArithmeticOp, Axis, Expr, Literal, NodeTest, ValueComp};
use crate::context::StaticContext;
use crate::types::{Item, KindTest, Occurrence, SequenceType};
use crate::xdm::{QName, XdmError};
use pest_consume::match_nodes;
use pest_consume::Error;
use pest_consume::Parser;
use rust_decimal::Decimal;
use std::str::FromStr;

pub type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i, 's_ctx> = pest_consume::Node<'i, Rule, &'s_ctx StaticContext>;

impl<R> From<XdmError> for pest::error::Error<R> {
    fn from(_xe: XdmError) -> Self {
        unimplemented!()
    }
}

pub(crate) fn parse(ctx: &StaticContext, input: &str) -> Result<Expr> {
    let root = XpathParser::parse_with_userdata(Rule::Xpath, input, ctx);
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
            [StringConcatExpr(e1), ValueComp(c), StringConcatExpr(e2)] => {
                Expr::ValueComp(Box::new(e1), c, Box::new(e2))
            }
            //[StringConcatExpr(e1), GeneralComp(c), StringConcatExpr(e2)] => , // FIXME
        ))
    }
    fn ValueComp(input: Node) -> Result<ValueComp> {
        Ok(match input.as_str() {
            "eq" => ValueComp::EQ,
            "ne" => ValueComp::NE,
            "lt" => ValueComp::LT,
            "le" => ValueComp::LE,
            "gt" => ValueComp::GT,
            "ge" => ValueComp::GE,
            &_ => unreachable!(),
        })
    }
    //fn GeneralComp(input: Node) -> Result {}
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
            [CastableExpr(e)] => e,
            [CastableExpr(e), SequenceType(st)] => Expr::TreatAs(Box::new(e), st),
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
            [InitialSlash(e), RelativePathExpr(e)] => e, // FIXME
            [RelativePathExpr(e)] => e, // FIXME
        ))
    }
    fn InitialSlash(_input: Node) -> Result<Expr> {
        // (fn:root(self::node()) treat as document-node())
        todo!("handle initial slash")
    }
    fn RelativePathExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [StepExpr(e)] => e,
            [StepExpr(e), SlashStep(v)..] => {
                v.into_iter().fold(e, |e1, e2|Expr::Path(Box::new(e1), Box::new(e2)))
            }
        ))
    }
    fn SlashStep(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Slash(_), StepExpr(e)] => e,
            [SlashSlash(_), StepExpr(e)] => e, // FIXME
        ))
    }
    fn Slash(_input: Node) -> Result<bool> {
        Ok(false)
    }
    fn SlashSlash(_input: Node) -> Result<bool> {
        Ok(true)
    }
    fn StepExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [PostfixExpr(e)] => e, // FIXME
            [AxisStep(e)] => e, // FIXME
        ))
    }
    fn AxisStep(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [ReverseStep(s), PredicateList(p)] => Expr::Step(s.0, s.1, p),
            [ForwardStep(s), PredicateList(p)] => Expr::Step(s.0, s.1, p),
        ))
    }
    fn ReverseStep(input: Node) -> Result<(Axis, NodeTest)> {
        Ok(match_nodes!(input.into_children();
            [ReverseAxis(a), NodeTest(t)] => (a, t),
        ))
    }
    fn ForwardStep(input: Node) -> Result<(Axis, NodeTest)> {
        Ok(match_nodes!(input.into_children();
            [ForwardAxis(a), NodeTest(t)] => (a, t),
            [AbbrevForwardStep(at)] => (at.0, at.1),
        ))
    }
    fn AbbrevForwardStep(input: Node) -> Result<(Axis, NodeTest)> {
        Ok(match_nodes!(input.into_children();
            [NodeTest(t)] => (Axis::Child, t),
            [AttributeIndicator(_), NodeTest(t)] => (Axis::Attribute, t),
        ))
    }
    fn AttributeIndicator(_input: Node) -> Result<()> {
        Ok(())
    }
    fn ForwardAxis(input: Node) -> Result<Axis> {
        Ok(match input.as_str() {
            "child" => Axis::Child,
            "attribute" => Axis::Attribute,
            "self" => Axis::Self_,
            _ => unimplemented!(),
        })
    }
    fn ReverseAxis(input: Node) -> Result<Axis> {
        Ok(match input.as_str() {
            "parent" => Axis::Parent,
            _ => unreachable!(),
        })
    }
    fn NodeTest(input: Node) -> Result<NodeTest> {
        Ok(match_nodes!(input.into_children();
            [NameTest(qname)] => NodeTest::NameTest(qname),
            [KindTest(kind)] => NodeTest::KindTest(kind),
        ))
    }
    fn KindTest(input: Node) -> Result<KindTest> {
        Ok(match_nodes!(input.into_children();
            [AnyKindTest(kt)] => kt,
            [DocumentTest(kt)] => kt,
        ))
    }
    fn DocumentTest(input: Node) -> Result<KindTest> {
        Ok(match_nodes!(input.into_children();
            [] => KindTest::Document,
        ))
    }
    fn AnyKindTest(_input: Node) -> Result<KindTest> {
        Ok(KindTest::AnyKind)
    }
    fn NameTest(input: Node) -> Result<QName> {
        let sc = input.user_data().clone();
        Ok(match_nodes!(input.into_children();
            [EQName(mut qname)] => {
                sc.qname_for_element(&mut qname);
                qname
            }
        ))
    }
    fn PredicateList(input: Node) -> Result<Vec<Expr>> {
        Ok(match_nodes!(input.into_children();
            [Predicate(expr)..] => expr.collect(),
        ))
    }
    fn PostfixExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [PrimaryExpr(e)] => e,
            [PrimaryExpr(e), Predicate(pe)] => Expr::Predicate(Box::new(e), Box::new(pe)),
            // FIXME generalize for more predicates and other stuff
        ))
    }
    fn Predicate(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Expr(expr)] => expr,
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
            [KindTest(kt)] => Item::KindTest(kt),
        ))
    }
    fn Item(_input: Node) -> Result<Item> {
        Ok(Item::Item)
    }
    fn AtomicOrUnionType(input: Node) -> Result<Item> {
        let sc = input.user_data();
        Ok(match_nodes!(input.children();
            [EQName(qname)] => Item::AtomicOrUnion(sc.schema_type(&qname).map_err(|e|input.error(e.message))?),
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
        // We check whether the function exists in the typing phase
        match_nodes!(input.clone().into_children();
            [EQName(f), ExprSingle(a)..] => {
                let args: Vec<_> = a.collect();
                Ok(Expr::FunctionCall(f, args))
            }
        )
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
    fn BracedURILiteralContent(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }
    fn NCName(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }
    fn QName(input: Node) -> Result<QName> {
        Ok(match_nodes!(input.into_children();
            [PrefixedName(q)] => q,
            [UnprefixedName(q)] => q,
        ))
    }
    fn PrefixedName(input: Node) -> Result<QName> {
        let sc = input.user_data().clone();
        Ok(match_nodes!(input.into_children();
            [Prefix(p), LocalPart(l)] => QName::new(l, Some(sc.namespace(&p).unwrap()), Some(p)),
        ))
    }
    fn Prefix(input: Node) -> Result<String> {
        let sc = input.user_data();
        let prefix = input.as_str();
        if sc.prefix_defined(prefix) {
            Ok(prefix.to_string())
        } else {
            Err(input.error("prefix not found in static context"))
        }
    }
    fn LocalPart(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }
    fn UnprefixedName(input: Node) -> Result<QName> {
        Ok(QName::new(input.as_str().to_string(), None, None))
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
            inner.replace(two_quotes.as_str(), quote)
        } else {
            inner.to_string()
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
    use crate::xdm::QName;
    use rust_decimal::Decimal;
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
            Ok(Expr::Literal(Literal::String("foo".to_string())))
        )
    }

    #[test]
    fn string_literal2() {
        let context: StaticContext = Default::default();
        let output = context.parse("\"foo\"");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String("foo".to_string())))
        )
    }

    #[test]
    fn string_literal3() {
        let context: StaticContext = Default::default();
        let output = context.parse("'foo''bar'");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String("foo'bar".to_string())))
        )
    }

    #[test]
    fn string_literal4() {
        let context: StaticContext = Default::default();
        let input = "\"foo\"\"bar\"";
        let output = context.parse(input);
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String("foo\"bar".to_string())))
        );
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn string_literal5() {
        let context: StaticContext = Default::default();
        let output = context.parse("\"foo''bar\"");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String("foo''bar".to_string())))
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
                Expr::Literal(Literal::String("two".to_string()))
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
        let input = "(:here we go: :) 1 (: one :), 2 (: that's it :)\n";
        let output = context.parse(input);
        assert_eq!(
            output,
            Ok(Expr::Sequence(vec![
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::Integer(2))
            ]))
        );
        assert_eq!("(1, 2)", format!("{}", output.unwrap()))
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
        let input = "Q{http://example.com/}myfunc(., 1)";
        let output = context.parse(input);
        assert_eq!(
            output,
            Ok(Expr::FunctionCall(
                QName::new(
                    "myfunc".to_string(),
                    Some("http://example.com/".to_string()),
                    None
                ),
                vec![Expr::ContextItem, Expr::Literal(Literal::Integer(1))]
            ))
        );
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn fn2() {
        let context: StaticContext = Default::default();
        let output = context.parse("nosuch:myfunc()");
        assert!(output.is_err())
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
        let input = "1 - 2 + 3 - 4";
        let output = context.parse(input);
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
        );
        assert_eq!(input, format!("{}", output.unwrap()))
    }
    #[test]
    fn instance_of1() {
        let context: StaticContext = Default::default();
        let input = ". instance of xs:integer";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }
    #[test]
    fn path1() {
        let context: StaticContext = Default::default();
        let input = "child::a/child::b/child::c";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }
    #[test]
    fn comparison1() {
        let context: StaticContext = Default::default();
        let input = "1 eq 2";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }
    #[test]
    fn predicate1() {
        let context: StaticContext = Default::default();
        let input = "child::a[2]";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }
}
