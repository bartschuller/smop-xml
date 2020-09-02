use crate::ast::{
    ArithmeticOp, Axis, CombineOp, Comp, Expr, Literal, NodeComp, NodeTest, Quantifier, Wildcard,
};
use crate::context::StaticContext;
use crate::types::{Item, KindTest, Occurrence, SequenceType};
use crate::xdm::XdmError;
use itertools::Itertools;
use pest_consume::*;
use rust_decimal::Decimal;
use smop_xmltree::nod::QName;
use std::str::FromStr;

pub type Result<T> = std::result::Result<T, pest::error::Error<Rule>>;
type Node<'i, 's_ctx> = pest_consume::Node<'i, Rule, &'s_ctx StaticContext>;

impl<R> From<XdmError> for pest::error::Error<R> {
    fn from(_xe: XdmError) -> Self {
        unimplemented!()
    }
}

pub(crate) fn parse(ctx: &StaticContext, input: &str) -> Result<Expr<()>> {
    let root = XpathParser::parse_with_userdata(Rule::Xpath, input, ctx);
    //let root2 = root.clone();
    //pest_ascii_tree::print_ascii_tree(root2.map(|n| n.as_pairs().to_owned()));
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
#[allow(non_snake_case)]
impl XpathParser {
    // Numbers refer to the XPath 3.1 grammar
    // 1
    pub fn Xpath(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [Expr(expr), EOI(_)] => expr,
        ))
    }
    fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }
    // 3
    fn Param(input: Node) -> Result<(QName, Option<SequenceType>)> {
        Ok(match_nodes!(input.into_children();
            [EQName(qname), SequenceType(st)] => (qname, Some(st)),
            [EQName(qname)] => (qname, None),
        ))
    }
    // 3
    fn EnclosedExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [Expr(e)] => e,
            [] => Expr::Sequence(vec![], ()),
        ))
    }
    // 6
    fn Expr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [ExprSingle(expr)] => expr,
            [ExprSingle(expr)..] => {
                    Expr::Sequence(expr.collect(), ())
            }
        ))
    }
    // 7
    fn ExprSingle(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [ForExpr(e)] => e,
            [LetExpr(e)] => e,
            [QuantifiedExpr(e)] => e,
            [IfExpr(e)] => e,
            [OrExpr(e)] => e,
        ))
    }
    // 8
    fn ForExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [SimpleForClause(bs), ExprSingle(e)] => bs.into_iter().rev().fold(e, |e,b|{
                Expr::For(b.0, b.1, Box::new(e), ())
            }),
        ))
    }
    // 9
    fn SimpleForClause(input: Node) -> Result<Vec<(QName, Box<Expr<()>>)>> {
        Ok(match_nodes!(input.into_children();
            [SimpleForBinding(b)..] => b.collect(),
        ))
    }
    // 10
    fn SimpleForBinding(input: Node) -> Result<(QName, Box<Expr<()>>)> {
        Ok(match_nodes!(input.into_children();
            [EQName(qn), ExprSingle(e)] => (qn, Box::new(e)),
        ))
    }
    // 11
    fn LetExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [SimpleLetClause(bs), ExprSingle(e)] => bs.into_iter().rev().fold(e, |e,b|{
                Expr::Let(b.0, Box::new(b.1), Box::new(e), ())
            }),
        ))
    }
    // 12
    fn SimpleLetClause(input: Node) -> Result<Vec<(QName, Expr<()>)>> {
        Ok(match_nodes!(input.into_children();
            [SimpleLetBinding(b)..] => b.collect(),
        ))
    }
    // 13
    fn SimpleLetBinding(input: Node) -> Result<(QName, Expr<()>)> {
        Ok(match_nodes!(input.into_children();
            [EQName(qn), ExprSingle(e)] => (qn, e),
        ))
    }
    // 14
    fn QuantifiedExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [SomeOrEvery(se), SimpleForBinding(b).., ExprSingle(p)] =>
              Expr::Quantified(se, b.collect(), Box::new(p), ()),
        ))
    }
    fn SomeOrEvery(input: Node) -> Result<Quantifier> {
        Ok(match input.as_str() {
            "some" => Quantifier::Some,
            "every" => Quantifier::Every,
            &_ => unreachable!(),
        })
    }
    // 15
    fn IfExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [Expr(i), ExprSingle(t), ExprSingle(e)] => Expr::IfThenElse(Box::new(i), Box::new(t), Box::new(e), ()),
        ))
    }
    // 16
    fn OrExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [AndExpr(expr)] => expr,
            [AndExpr(expr)..] => {
                    Expr::Or(expr.collect(), ())
            }
        ))
    }
    // 17
    fn AndExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [ComparisonExpr(expr)] => expr,
            [ComparisonExpr(expr)..] => {
                    Expr::And(expr.collect(), ())
            }
        ))
    }
    // 18
    fn ComparisonExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [StringConcatExpr(e)] => e,
            [StringConcatExpr(e1), ValueComp(c), StringConcatExpr(e2)] =>
                Expr::ValueComp(Box::new(e1), c, Box::new(e2), ()),
            [StringConcatExpr(e1), GeneralComp(c), StringConcatExpr(e2)] =>
                Expr::GeneralComp(Box::new(e1), c, Box::new(e2), ()),
            [StringConcatExpr(e1), NodeComp(c), StringConcatExpr(e2)] =>
                Expr::NodeComp(Box::new(e1), c, Box::new(e2), ()),
        ))
    }
    // 19
    fn StringConcatExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [RangeExpr(e)] => e,
            [RangeExpr(es)..] => Expr::Concat(es.collect(), ())
        ))
    }
    // 20
    fn RangeExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [AdditiveExpr(e)] => e,
            [AdditiveExpr(e1), AdditiveExpr(e2)] => Expr::Range(Box::new(e1), Box::new(e2), ()),
        ))
    }
    // 21
    fn AdditiveExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [MultiplicativeExpr(e)] => e,
            [MultiplicativeExpr(e1), AdditiveExpr1(oe)..] => {
                oe.fold(e1, |a, (op, b)| Expr::Arithmetic(Box::new(a), op, Box::new(b), ()))
            }
        ))
    }
    fn AdditiveExpr1(input: Node) -> Result<(ArithmeticOp, Expr<()>)> {
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
    // 22
    fn MultiplicativeExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [UnionExpr(e)] => e,
            [UnionExpr(e1), MultiplicativeExpr1(oe)..] => {
                oe.fold(e1, |a, (op, b)| Expr::Arithmetic(Box::new(a), op, Box::new(b), ()))
            }
        ))
    }
    fn MultiplicativeExpr1(input: Node) -> Result<(ArithmeticOp, Expr<()>)> {
        Ok(match_nodes!(input.into_children();
            [MultiplicativeOp(op), UnionExpr(e)] => (op, e),
        ))
    }
    fn MultiplicativeOp(input: Node) -> Result<ArithmeticOp> {
        Ok(match input.as_str() {
            "*" => ArithmeticOp::Mul,
            "div" => ArithmeticOp::Div,
            "idiv" => ArithmeticOp::Idiv,
            "mod" => ArithmeticOp::Mod,
            &_ => unreachable!(),
        })
    }
    // 23
    fn UnionExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [IntersectExceptExpr(e)] => e,
            [IntersectExceptExpr(e1), IntersectExceptExpr(es)..] => {
                es.fold(e1, |a, b| Expr::Combine(Box::new(a), CombineOp::Union, Box::new(b), ()))
            }
        ))
    }
    // 24
    fn IntersectExceptExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [InstanceofExpr(e)] => e,
            [InstanceofExpr(e), IntersectOrExcept(c_es)..] => {
                c_es.fold(e, |a, (op, b)|Expr::Combine(Box::new(a), op, Box::new(b), ()))
            }
        ))
    }
    fn IntersectOrExcept(input: Node) -> Result<(CombineOp, Expr<()>)> {
        Ok(match_nodes!(input.into_children();
            [IntersectExpr(c_e)] => c_e,
            [ExceptExpr(c_e)] => c_e,
        ))
    }
    fn IntersectExpr(input: Node) -> Result<(CombineOp, Expr<()>)> {
        Ok(match_nodes!(input.into_children();
            [InstanceofExpr(e)] => (CombineOp::Intersect, e),
        ))
    }
    fn ExceptExpr(input: Node) -> Result<(CombineOp, Expr<()>)> {
        Ok(match_nodes!(input.into_children();
            [InstanceofExpr(e)] => (CombineOp::Except, e),
        ))
    }
    // 25
    fn InstanceofExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [TreatExpr(e)] => e,
            [TreatExpr(e), SequenceType(t)] => Expr::InstanceOf(Box::new(e), t, ()),
        ))
    }
    // 26
    fn TreatExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [CastableExpr(e)] => e,
            [CastableExpr(e), SequenceType(st)] => Expr::TreatAs(Box::new(e), st, ()),
        ))
    }
    // 27
    fn CastableExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [CastExpr(e)] => e, // FIXME
        ))
    }
    // 28
    fn CastExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [ArrowExpr(e)] => e, // FIXME
        ))
    }
    // 29
    fn ArrowExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [UnaryExpr(e)] => e,
            [UnaryExpr(e), ArrowFunctionSpecifier(fs)..] => arrow_exprs(e, fs.collect()),
        ))
    }
    // 30
    fn UnaryExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [SimpleMapExpr(e)] => e,
            [AdditiveOp(os).., SimpleMapExpr(e)] => {
                if os.into_iter().filter(|o| *o == ArithmeticOp::Minus).count() % 2 == 1 {
                    Expr::UnaryMinus(Box::new(e), ())
                } else {
                    e
                }
            }
        ))
    }
    // 32
    fn GeneralComp(input: Node) -> Result<Comp> {
        Ok(match input.as_str() {
            "=" => Comp::EQ,
            "!=" => Comp::NE,
            "<" => Comp::LT,
            "<=" => Comp::LE,
            ">" => Comp::GT,
            ">=" => Comp::GE,
            &_ => unreachable!(),
        })
    }
    // 33
    fn ValueComp(input: Node) -> Result<Comp> {
        Ok(match input.as_str() {
            "eq" => Comp::EQ,
            "ne" => Comp::NE,
            "lt" => Comp::LT,
            "le" => Comp::LE,
            "gt" => Comp::GT,
            "ge" => Comp::GE,
            &_ => unreachable!(),
        })
    }
    fn NodeComp(input: Node) -> Result<NodeComp> {
        Ok(match input.as_str() {
            "is" => NodeComp::Is,
            "<<" => NodeComp::Before,
            ">>" => NodeComp::After,
            &_ => unreachable!(),
        })
    }
    // 35
    fn SimpleMapExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [PathExpr(es)..] => es.into_iter()
                                  .fold1(|e1, e2| Expr::SimpleMap(Box::new(e1), Box::new(e2), ()))
                                  .unwrap(),
        ))
    }
    // 36
    fn PathExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [InitialSlashSlash(_), RelativePathExpr(mut v)] => {
                v.insert(0, slash_ast());
                v.insert(1, slash_slash_ast());
                path(v)
            },
            [InitialSlash(_), RelativePathExpr(mut v)] => {
                v.insert(0, slash_ast());
                path(v)
            },
            [InitialSlash(_)] => slash_ast(),
            [RelativePathExpr(v)] => path(v), // FIXME
        ))
    }
    fn InitialSlash(_input: Node) -> Result<()> {
        Ok(())
    }
    fn InitialSlashSlash(_input: Node) -> Result<()> {
        Ok(())
    }
    // 37
    fn RelativePathExpr(input: Node) -> Result<Vec<Expr<()>>> {
        Ok(match_nodes!(input.into_children();
            [StepExpr(e)] => vec![e],
            [StepExpr(e), SlashStep(v)..] => {
                let mut v: Vec<Expr<()>> = v.flatten().collect();
                v.insert(0, e);
                v
            }
        ))
    }
    fn SlashStep(input: Node) -> Result<Vec<Expr<()>>> {
        Ok(match_nodes!(input.into_children();
            [Slash(_), StepExpr(e)] => vec![e],
            [SlashSlash(_), StepExpr(e)] => {
                vec![slash_slash_ast(), e]
            }
        ))
    }
    fn Slash(_input: Node) -> Result<()> {
        Ok(())
    }
    fn SlashSlash(_input: Node) -> Result<()> {
        Ok(())
    }
    // 38
    fn StepExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [PostfixExpr(e)] => e, // FIXME
            [AxisStep(e)] => e, // FIXME
        ))
    }
    // 39
    fn AxisStep(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [ReverseStep(s), PredicateList(p)] => Expr::Step(s.0, s.1, p, ()),
            [ForwardStep(s), PredicateList(p)] => Expr::Step(s.0, s.1, p, ()),
        ))
    }
    // 40
    fn ForwardStep(input: Node) -> Result<(Axis, NodeTest)> {
        Ok(match_nodes!(input.into_children();
            [ForwardAxis(a), NodeTest(t)] => (a, t),
            [AbbrevForwardStep(at)] => (at.0, at.1),
        ))
    }
    // 41
    fn ForwardAxis(input: Node) -> Result<Axis> {
        Ok(match input.as_str() {
            "child" => Axis::Child,
            "descendant" => Axis::Descendant,
            "attribute" => Axis::Attribute,
            "self" => Axis::Self_,
            "descendant-or-self" => Axis::DescendantOrSelf,
            "following-sibling" => Axis::FollowingSibling,
            "following" => Axis::Following,
            "namespace" => return Err(input.error("err:XPST0010 namespace axis is not supported")),
            _ => unreachable!(),
        })
    }
    // 42
    fn AbbrevForwardStep(input: Node) -> Result<(Axis, NodeTest)> {
        Ok(match_nodes!(input.into_children();
            [NodeTest(t)] => (Axis::Child, t),
            [AttributeIndicator(_), NodeTest(t)] => (Axis::Attribute, t),
        ))
    }
    fn AttributeIndicator(_input: Node) -> Result<()> {
        Ok(())
    }
    // 43
    fn ReverseStep(input: Node) -> Result<(Axis, NodeTest)> {
        Ok(match_nodes!(input.into_children();
            [ReverseAxis(a), NodeTest(t)] => (a, t),
            [AbbrevReverseStep(_)] => (Axis::Parent, NodeTest::KindTest(KindTest::AnyKind)),
        ))
    }
    // 44
    fn ReverseAxis(input: Node) -> Result<Axis> {
        Ok(match input.as_str() {
            "parent" => Axis::Parent,
            "ancestor" => Axis::Ancestor,
            "preceding-sibling" => Axis::PrecedingSibling,
            "preceding" => Axis::Preceding,
            "ancestor-or-self" => Axis::AncestorOrSelf,
            _ => unreachable!(),
        })
    }
    // 45
    fn AbbrevReverseStep(_input: Node) -> Result<()> {
        Ok(())
    }
    // 46
    fn NodeTest(input: Node) -> Result<NodeTest> {
        let sc = input.user_data().clone();
        Ok(match_nodes!(input.into_children();
            [EQName(mut qname)] => {
                sc.qname_for_element(&mut qname);
                NodeTest::NameTest(qname)
            },
            [Wildcard(wildcard)] => NodeTest::WildcardTest(wildcard),
            [KindTest(kind)] => NodeTest::KindTest(kind),
        ))
    }
    // 48
    fn Wildcard(input: Node) -> Result<Wildcard> {
        let sc = input.user_data().clone();
        Ok(match_nodes!(input.clone().into_children();
            [Star(_)] => Wildcard::Any,
            [NCName(prefix), Star(_)] =>
                match sc.namespace(prefix.as_str()) {
                    None => return Err(input.error("err:XPST0081 prefix not found")),
                    Some(ns) => Wildcard::AnyInNs(ns),
                },
            [Star(_), NCName(name)] => Wildcard::AnyWithLocalName(name),
            [BracedURILiteralContent(ns), Star(_)] => Wildcard::AnyInNs(ns),
        ))
    }
    fn Star(_input: Node) -> Result<()> {
        Ok(())
    }
    // 49
    fn PostfixExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [PrimaryExpr(e)] => e,
            [PrimaryExpr(e), Predicate(pe)] => Expr::Filter(Box::new(e), Box::new(pe), ()),
            // FIXME generalize for more predicates and other stuff
        ))
    }
    // 50
    fn ArgumentList(input: Node) -> Result<Vec<Expr<()>>> {
        Ok(match_nodes!(input.into_children();
            [ExprSingle(a)..] => {
                let args: Vec<_> = a.collect();
                args
            },
        ))
    }
    // 51
    fn PredicateList(input: Node) -> Result<Vec<Expr<()>>> {
        Ok(match_nodes!(input.into_children();
            [Predicate(expr)..] => expr.collect(),
        ))
    }
    // 52
    fn Predicate(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [Expr(expr)] => expr,
        ))
    }
    // 55
    fn ArrowFunctionSpecifier(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [FunctionCallEQName(qname), ArgumentList(args)] => Expr::FunctionCall(qname, args, ())
            //[VarRef(eq_name)] =>
            //[ParenthesizedExpr(e)] =>
        ))
    }
    // 56
    fn PrimaryExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [Literal(lit)] => Expr::Literal(lit, ()),
            [VarRef(eq_name)] => Expr::VarRef(eq_name, ()),
            [ParenthesizedExpr(e)] => e,
            [ContextItemExpr(e)] => e,
            [FunctionItemExpr(e)] => e,
            [MapConstructor(e)] => e,
            [ArrayConstructor(e)] => e,
            [FunctionCall(e)] => e,
            [UnaryLookup(e)] => e,
        ))
    }
    // 57
    fn Literal(input: Node) -> Result<Literal> {
        Ok(match_nodes!(input.into_children();
            [DoubleLiteral(lit)] => lit,
            [DecimalLiteral(lit)] => lit,
            [IntegerLiteral(lit)] => lit,
            [StringLiteral(lit)] => lit,
        ))
    }
    // 59
    fn VarRef(input: Node) -> Result<QName> {
        Ok(match_nodes!(input.into_children();
            [EQName(eq)] => eq
        ))
    }
    // 61
    fn ParenthesizedExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [Expr(e)] => e,
            [] => Expr::Sequence(vec![], ())
        ))
    }
    // 62
    fn ContextItemExpr(_input: Node) -> Result<Expr<()>> {
        Ok(Expr::ContextItem(()))
    }
    // 63
    fn FunctionCall(input: Node) -> Result<Expr<()>> {
        // We check whether the function exists in the typing phase
        match_nodes!(input.clone().into_children();
            [FunctionCallEQName(f), ArgumentList(args)] =>
                Ok(Expr::FunctionCall(f, args, ())),
        )
    }
    // 66
    fn FunctionItemExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [NamedFunctionRef(e)] => unimplemented!(),
            [InlineFunctionExpr(e)] => e,
        ))
    }
    // 67
    fn NamedFunctionRef(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [EQName(qname), IntegerLiteral(i)] => Expr::Sequence(vec![], ()), // FIXME
        ))
    }
    // 68
    fn InlineFunctionExpr(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [ParamListMaybe(pl), AsTypeMaybe(st), EnclosedExpr(e)] => {
                Expr::InlineFunction(pl, st, Box::new(e), ())
            }
        ))
    }
    fn ParamListMaybe(input: Node) -> Result<Vec<(QName, Option<SequenceType>)>> {
        Ok(match_nodes!(input.into_children();
            [Param(p)..] => p.collect(),
            [] => vec![],
        ))
    }
    fn AsTypeMaybe(input: Node) -> Result<Option<SequenceType>> {
        Ok(match_nodes!(input.into_children();
            [SequenceType(st)] => Some(st),
            [] => None,
        ))
    }
    // 69
    fn MapConstructor(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [MapConstructorEntry(es)..] => Expr::Map(es.collect(), ()),
        ))
    }
    // 70
    fn MapConstructorEntry(input: Node) -> Result<(Expr<()>, Expr<()>)> {
        Ok(match_nodes!(input.into_children();
            [ExprSingle(k), ExprSingle(v)] => (k, v),
        ))
    }
    // 73
    fn ArrayConstructor(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [ExprSingle(es)..] => Expr::ArraySquare(es.collect(), ()),
            [EnclosedExpr(e)] => Expr::ArrayCurly(Box::new(e), ()),
        ))
    }
    // 76
    fn UnaryLookup(input: Node) -> Result<Expr<()>> {
        Ok(match_nodes!(input.into_children();
            [NamedFunctionRef(e)] => Expr::Sequence(vec![], ()), // FIXME
        ))
    }
    // 79
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
    // 80
    fn OccurrenceIndicator(input: Node) -> Result<Occurrence> {
        Ok(match input.as_str() {
            "?" => Occurrence::Optional,
            "*" => Occurrence::ZeroOrMore,
            "+" => Occurrence::OneOrMore,
            &_ => unreachable!(),
        })
    }
    // 81
    fn ItemType(input: Node) -> Result<Item> {
        Ok(match_nodes!(input.into_children();
            [Item(it)] => it,
            [AtomicOrUnionType(it)] => it,
            [KindTest(kt)] => Item::KindTest(kt),
            // FIXME
        ))
    }
    fn Item(_input: Node) -> Result<Item> {
        Ok(Item::Item)
    }
    // 82
    fn AtomicOrUnionType(input: Node) -> Result<Item> {
        let sc = input.user_data();
        Ok(match_nodes!(input.children();
            [EQName(qname)] => Item::AtomicOrUnion(sc.schema_type(&qname).map_err(|e|input.error(e.message))?),
        ))
    }
    // 83
    fn KindTest(input: Node) -> Result<KindTest> {
        Ok(match_nodes!(input.into_children();
            [AnyKindTest(kt)] => kt,
            [DocumentTest(kt)] => kt,
            [ElementTest(kt)] => kt,
            [TextTest(kt)] => kt,
            [NamespaceNodeTest(kt)] => kt,
        ))
    }
    // 84
    fn AnyKindTest(_input: Node) -> Result<KindTest> {
        Ok(KindTest::AnyKind)
    }
    // 85
    fn DocumentTest(input: Node) -> Result<KindTest> {
        Ok(match_nodes!(input.into_children();
            [] => KindTest::Document, // FIXME
        ))
    }
    // 86
    fn TextTest(input: Node) -> Result<KindTest> {
        Ok(KindTest::Text)
    }
    // 88
    fn NamespaceNodeTest(input: Node) -> Result<KindTest> {
        Ok(KindTest::NamespaceNode)
    }
    // 94
    // ElementTest = { "element" ~ "(" ~ (ElementNameOrWildcard ~ ("," ~ TypeName ~ "?"?)?)? ~ ")" }
    fn ElementTest(input: Node) -> Result<KindTest> {
        Ok(match_nodes!(input.into_children();
            [] => KindTest::Element, // FIXME
        ))
    }
    // 112
    fn EQName(input: Node) -> Result<QName> {
        Ok(match_nodes!(input.into_children();
            [URIQualifiedName(q)] => q,
            [QName(q)] => q,
        ))
    }
    fn FunctionCallEQName(input: Node) -> Result<QName> {
        Ok(match_nodes!(input.into_children();
            [URIQualifiedName(q)] => q,
            [FunctionCallQName(q)] => q,
        ))
    }
    // 113
    fn IntegerLiteral(input: Node) -> Result<Literal> {
        match input.as_str().parse() {
            Ok(i) => Ok(Literal::Integer(i)),
            Err(e) => Err(input.error(format!("err:FOAR0002 {}", e.to_string()))),
        }
    }
    // 114
    fn DecimalLiteral(input: Node) -> Result<Literal> {
        match Decimal::from_str(input.as_str()) {
            Ok(d) => Ok(Literal::Decimal(d)),
            Err(e) => Err(input.error(format!("err:FOAR0002 {}", e.to_string()))),
        }
    }
    // 115
    fn DoubleLiteral(input: Node) -> Result<Literal> {
        Ok(Literal::Double(input.as_str().parse().unwrap()))
    }
    // 116
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
    // 117
    fn URIQualifiedName(input: Node) -> Result<QName> {
        Ok(match_nodes!(input.into_children();
            [BracedURILiteralContent(ns), NCName(name)] => QName::new(name, Some(ns), None),
        ))
    }
    // 118
    fn BracedURILiteralContent(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }
    // 122
    fn QName(input: Node) -> Result<QName> {
        Ok(match_nodes!(input.into_children();
            [PrefixedName(q)] => q,
            [UnprefixedName(q)] => q,
        ))
    }
    fn FunctionCallQName(input: Node) -> Result<QName> {
        Ok(match_nodes!(input.into_children();
            [PrefixedName(q)] => q,
            [FunctionCallUnprefixedName(q)] => q,
        ))
    }
    // 123
    fn NCName(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }
    // Numbers refer to the grammar in REC-xml-names
    // 8
    fn PrefixedName(input: Node) -> Result<QName> {
        let sc = input.user_data().clone();
        Ok(match_nodes!(input.into_children();
            [Prefix(p), LocalPart(l)] => QName::new(l, Some(sc.namespace(&p).unwrap()), Some(p)),
        ))
    }
    // 9
    fn UnprefixedName(input: Node) -> Result<QName> {
        Ok(QName::new(input.as_str().to_string(), None, None))
    }
    fn FunctionCallUnprefixedName(input: Node) -> Result<QName> {
        Ok(QName::new(input.as_str().to_string(), None, None))
    }
    // 10
    fn Prefix(input: Node) -> Result<String> {
        let sc = input.user_data();
        let prefix = input.as_str();
        if sc.prefix_defined(prefix) {
            Ok(prefix.to_string())
        } else {
            Err(input.error("prefix not found in static context"))
        }
    }
    // 11
    fn LocalPart(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }
}

fn path(v: Vec<Expr<()>>) -> Expr<()> {
    v.into_iter()
        .fold1(|e1, e2| Expr::Path(Box::new(e1), Box::new(e2), ()))
        .unwrap()
}

fn slash_ast() -> Expr<()> {
    Expr::TreatAs(
        Box::new(Expr::FunctionCall(
            QName::wellknown("fn:root"),
            vec![Expr::Step(
                Axis::Self_,
                NodeTest::KindTest(KindTest::AnyKind),
                vec![],
                (),
            )],
            (),
        )),
        SequenceType::Item(Item::KindTest(KindTest::Document), Occurrence::One),
        (),
    )
}

fn slash_slash_ast() -> Expr<()> {
    Expr::Step(
        Axis::DescendantOrSelf,
        NodeTest::KindTest(KindTest::AnyKind),
        vec![],
        (),
    )
}

// transforms
// e => fs1() => fs2()
// into
// fs2(fs1(e))
fn arrow_exprs(e: Expr<()>, fs: Vec<Expr<()>>) -> Expr<()> {
    fs.into_iter().fold(e, |e1, e2| match e2 {
        Expr::FunctionCall(qname, mut args, ()) => {
            args.insert(0, e1);
            Expr::FunctionCall(qname, args, ())
        }
        _ => unreachable!("expected only function calls"),
    })
}

#[cfg(test)]
mod tests {
    use super::StaticContext;
    use crate::ast::{ArithmeticOp, Expr, Literal};
    use crate::context::ExpandedName;
    use rust_decimal::Decimal;
    use smop_xmltree::nod::QName;
    use std::rc::Rc;
    use std::str::FromStr;

    #[test]
    fn int_literal1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1234");
        assert_eq!(output, Ok(Expr::Literal(Literal::Integer(1234), ())))
    }

    #[test]
    fn int_literal2() {
        let context: StaticContext = Default::default();
        let output = context.parse("999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999");
        assert_eq!(
            output.expect_err("expected an error").code,
            ExpandedName::new("http://www.w3.org/2005/xqt-errors", "FOAR0002")
        )
    }

    #[test]
    fn decimal_literal1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1234.56789");
        assert_eq!(
            output,
            Ok(Expr::Literal(
                Literal::Decimal(Decimal::from_str("1234.56789").unwrap()),
                ()
            ))
        )
    }

    #[test]
    fn string_literal1() {
        let context: StaticContext = Default::default();
        let output = context.parse("'foo'");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String("foo".to_string()), ()))
        )
    }

    #[test]
    fn string_literal2() {
        let context: StaticContext = Default::default();
        let output = context.parse("\"foo\"");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String("foo".to_string()), ()))
        )
    }

    #[test]
    fn string_literal3() {
        let context: StaticContext = Default::default();
        let output = context.parse("'foo''bar'");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String("foo'bar".to_string()), ()))
        )
    }

    #[test]
    fn string_literal4() {
        let context: StaticContext = Default::default();
        let input = "\"foo\"\"bar\"";
        let output = context.parse(input);
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String("foo\"bar".to_string()), ()))
        );
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn string_literal5() {
        let context: StaticContext = Default::default();
        let output = context.parse("\"foo''bar\"");
        assert_eq!(
            output,
            Ok(Expr::Literal(Literal::String("foo''bar".to_string()), ()))
        )
    }

    #[test]
    fn comment1() {
        let context: StaticContext = Default::default();
        let output = context.parse("(::)()");
        assert_eq!(output, Ok(Expr::Sequence(vec![], ())))
    }

    #[test]
    fn comment2() {
        let context: StaticContext = Default::default();
        let output = context.parse("(: foobar :)()");
        assert_eq!(output, Ok(Expr::Sequence(vec![], ())))
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
        assert_eq!(output, Ok(Expr::Sequence(vec![], ())))
    }

    #[test]
    fn sequence1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1,'two'");
        assert_eq!(
            output,
            Ok(Expr::Sequence(
                vec![
                    Expr::Literal(Literal::Integer(1), ()),
                    Expr::Literal(Literal::String("two".to_string()), ())
                ],
                ()
            ))
        )
    }

    #[test]
    fn if_then_else1() {
        let context: StaticContext = Default::default();
        let output = context.parse("if (3) then 1 else 2");
        assert_eq!(
            output,
            Ok(Expr::IfThenElse(
                Box::new(Expr::Literal(Literal::Integer(3), ())),
                Box::new(Expr::Literal(Literal::Integer(1), ())),
                Box::new(Expr::Literal(Literal::Integer(2), ())),
                ()
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
            Ok(Expr::Sequence(
                vec![
                    Expr::Literal(Literal::Integer(1), ()),
                    Expr::Literal(Literal::Integer(2), ())
                ],
                ()
            ))
        )
    }

    #[test]
    fn whitespace2() {
        let context: StaticContext = Default::default();
        let input = "(:here we go: :) 1 (: one :), 2 (: that's it :)\n";
        let output = context.parse(input);
        assert_eq!(
            output,
            Ok(Expr::Sequence(
                vec![
                    Expr::Literal(Literal::Integer(1), ()),
                    Expr::Literal(Literal::Integer(2), ())
                ],
                ()
            ))
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
                vec![
                    Expr::ContextItem(()),
                    Expr::Literal(Literal::Integer(1), ())
                ],
                ()
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
    fn arrow1() {
        let context: StaticContext = Default::default();
        let input = "'3.14' => xs:double() => string()";
        let output = context.parse(input);
        let expected = r#"string(xs:double("3.14"))"#;
        assert_eq!(expected, format!("{}", output.unwrap()))
    }

    #[test]
    fn or1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1 or 0");
        assert_eq!(
            output,
            Ok(Expr::Or(
                vec![
                    Expr::Literal(Literal::Integer(1), ()),
                    Expr::Literal(Literal::Integer(0), ())
                ],
                ()
            ))
        )
    }

    #[test]
    fn and1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1 and 0");
        assert_eq!(
            output,
            Ok(Expr::And(
                vec![
                    Expr::Literal(Literal::Integer(1), ()),
                    Expr::Literal(Literal::Integer(0), ())
                ],
                ()
            ))
        )
    }

    #[test]
    fn or_and1() {
        let context: StaticContext = Default::default();
        let output = context.parse("1 or 2 and 0");
        assert_eq!(
            output,
            Ok(Expr::Or(
                vec![
                    Expr::Literal(Literal::Integer(1), ()),
                    Expr::And(
                        vec![
                            Expr::Literal(Literal::Integer(2), ()),
                            Expr::Literal(Literal::Integer(0), ())
                        ],
                        ()
                    )
                ],
                ()
            ))
        )
    }

    #[test]
    fn arith1() {
        let context: StaticContext = Default::default();
        use ArithmeticOp::{Minus, Plus};
        use Literal::Integer;
        let input = "1 - 2 + 3 - 4";
        let output = context.parse(input);
        assert_eq!(
            output,
            Ok(Expr::Arithmetic(
                Box::new(Expr::Arithmetic(
                    Box::new(Expr::Arithmetic(
                        Box::new(Expr::Literal(Integer(1), ())),
                        Minus,
                        Box::new(Expr::Literal(Integer(2), ())),
                        ()
                    )),
                    Plus,
                    Box::new(Expr::Literal(Integer(3), ())),
                    ()
                )),
                Minus,
                Box::new(Expr::Literal(Integer(4), ())),
                ()
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
    fn path2() {
        let context: StaticContext = Default::default();
        let input = "ancestor-or-self::a/parent::b/child::c";
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
    fn comparison2() {
        let context: StaticContext = Default::default();
        let input = "1 = 2";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn comparison3() {
        let context: StaticContext = Default::default();
        let input = r#"child::book[attribute::isbn = "9876543210123"] << child::book[attribute::isbn = "9876543210120"]"#;
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn comparison4() {
        let context: StaticContext = Default::default();
        let input = r#"child::book[attribute::isbn = "9876543210123"] is child::book/child::title[. = "How to Cook"]"#;
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

    #[test]
    fn for1() {
        let context: StaticContext = Default::default();
        let input = "for $i in (1, 2) return $i";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn for2() {
        let context: StaticContext = Default::default();
        let input = "for $i in (10, 20) return for $j in (1, $i) return $i + $j";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn for3() {
        let context: StaticContext = Default::default();
        let input = "for $i in (10, 20),
                               $j in (1, $i)
                           return ($i + $j)";
        let equiv = "for $i in (10, 20) return for $j in (1, $i) return $i + $j";
        let output = context.parse(input);
        assert_eq!(equiv, format!("{}", output.unwrap()))
    }

    #[test]
    fn let1() {
        let context: StaticContext = Default::default();
        let input = "let $x := 2 return $x";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn let2() {
        let context: StaticContext = Default::default();
        let input = "let $x := 2, $y := 3 return $x + $y";
        let equiv = "let $x := 2 return let $y := 3 return $x + $y";
        let output = context.parse(input);
        assert_eq!(equiv, format!("{}", output.unwrap()))
    }

    #[test]
    fn quant1() {
        let context: StaticContext = Default::default();
        let input = "some $x in (1, 2, 3), $y in (2, 3, 4) satisfies $x + $y = 4";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn quant2() {
        let context: StaticContext = Default::default();
        let input = "every $x in (1, 2, 3), $y in (2, 3, 4) satisfies $x + $y = 4";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn string_concat1() {
        let context: StaticContext = Default::default();
        let input = r#""con" || "cat" || "enate""#;
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn range1() {
        let context: StaticContext = Default::default();
        let input = "1 to 5";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn array1() {
        let context: StaticContext = Default::default();
        let input = r#"[1, "two", []]"#;
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn array2() {
        let context: StaticContext = Default::default();
        let input = r#"array {1, "two", []}"#;
        let equiv = r#"array {(1, "two", [])}"#;
        let output = context.parse(input);
        assert_eq!(equiv, format!("{}", output.unwrap()))
    }

    #[test]
    fn array3() {
        let context: StaticContext = Default::default();
        let input = r#"array {}"#;
        let equiv = r#"array {()}"#;
        let output = context.parse(input);
        assert_eq!(equiv, format!("{}", output.unwrap()))
    }

    #[test]
    fn map1() {
        let context: StaticContext = Default::default();
        let input = r#"map {"key": "value", 1.2: 5e-6}"#;
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn anon_function1() {
        let context: StaticContext = Default::default();
        let input = "function($a) { $a }";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn anon_function2() {
        let context: StaticContext = Default::default();
        let input = "function($a as xs:double, $b as xs:double) as xs:double { $a * $b }";
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }

    #[test]
    fn slash1() {
        let context: StaticContext = Default::default();
        let input = "/";
        let output = context.parse(input);
        assert_eq!(
            "fn:root(self::node()) treat as document-node()",
            format!("{}", output.unwrap())
        )
    }

    #[test]
    fn slashslash1() {
        let context: StaticContext = Default::default();
        let input = "//center/child::*";
        let output = context.parse(input);
        assert_eq!(
            "fn:root(self::node()) treat as document-node()/descendant-or-self::node()/child::center/child::*",
            format!("{}", output.unwrap()))
    }

    #[test]
    fn slashslash2() {
        let context: StaticContext = Default::default();
        //let input = "(fn:root(self::node()) treat as document-node())/descendant-or-self::node()/center/child::*";
        let input = "//center/child::*";
        let output = context.parse(input);
        assert_eq!(
            "fn:root(self::node()) treat as document-node()/descendant-or-self::node()/child::center/child::*",
            format!("{}", output.unwrap()))
    }

    #[test]
    fn dotdot1() {
        let context: StaticContext = Default::default();
        let input = "..";
        let output = context.parse(input);
        assert_eq!("parent::node()", format!("{}", output.unwrap()))
    }

    #[test]
    fn kindtest1() {
        let context: StaticContext = Default::default();
        let input = "text()";
        let output = context.parse(input);
        let typed = output.unwrap().type_(Rc::new(context));
        assert_eq!("child::text()", format!("{}", typed.unwrap()))
    }

    #[test]
    fn kindtest2() {
        let context: StaticContext = Default::default();
        let input = "center/node()";
        let output = context.parse(input);
        let typed = output.unwrap().type_(Rc::new(context));
        assert_eq!("child::center/child::node()", format!("{}", typed.unwrap()))
    }

    #[test]
    fn union1() {
        let context: StaticContext = Default::default();
        let input = r#"a|b|c"#;
        let equiv = r#"child::a union child::b union child::c"#;
        let output = context.parse(input);
        assert_eq!(equiv, format!("{}", output.unwrap()))
    }

    #[test]
    fn simple_map1() {
        let context: StaticContext = Default::default();
        let input = r#"(1 to 5)!"*""#;
        let output = context.parse(input);
        assert_eq!(input, format!("{}", output.unwrap()))
    }
}
