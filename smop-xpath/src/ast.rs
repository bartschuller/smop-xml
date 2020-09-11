use crate::context::StaticContext;
use crate::runtime::CompiledExpr;
use crate::types::{Item, KindTest, SchemaType};
use crate::types::{Occurrence, SequenceType};
use crate::xdm::*;
use itertools::Itertools;
use rust_decimal::Decimal;
use smop_xmltree::nod::{NodeKind, QName};
use smop_xmltree::option_ext::OptionExt;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Expr<T> {
    Literal(Literal, T),
    VarRef(QName, T),
    Sequence(Vec<Expr<T>>, T),
    ContextItem(T),
    IfThenElse(Box<Expr<T>>, Box<Expr<T>>, Box<Expr<T>>, T),
    FunctionCall(QName, Vec<Expr<T>>, T),
    Or(Vec<Expr<T>>, T),
    And(Vec<Expr<T>>, T),
    Concat(Vec<Expr<T>>, T),
    Arithmetic(Box<Expr<T>>, ArithmeticOp, Box<Expr<T>>, T),
    UnaryMinus(Box<Expr<T>>, T),
    InstanceOf(Box<Expr<T>>, SequenceType, T),
    TreatAs(Box<Expr<T>>, SequenceType, T),
    Cast {
        expression: Box<Expr<T>>,
        simple_type: Rc<SchemaType>,
        optional: bool,
        only_check: bool,
        t: T,
    },
    Path(Box<Expr<T>>, Box<Expr<T>>, T),
    Step(Axis, NodeTest, Vec<Expr<T>>, T),
    ValueComp(Box<Expr<T>>, Comp, Box<Expr<T>>, T),
    GeneralComp(Box<Expr<T>>, Comp, Box<Expr<T>>, T),
    NodeComp(Box<Expr<T>>, NodeComp, Box<Expr<T>>, T),
    Filter(Box<Expr<T>>, Box<Expr<T>>, T),
    For(QName, Box<Expr<T>>, Box<Expr<T>>, T),
    Let(QName, Box<Expr<T>>, Box<Expr<T>>, T),
    Quantified(Quantifier, Vec<(QName, Box<Expr<T>>)>, Box<Expr<T>>, T),
    Range(Box<Expr<T>>, Box<Expr<T>>, T),
    ArraySquare(Vec<Expr<T>>, T),
    ArrayCurly(Box<Expr<T>>, T),
    Map(Vec<(Expr<T>, Expr<T>)>, T),
    InlineFunction(
        Vec<(QName, Option<SequenceType>)>,
        Option<SequenceType>,
        Box<Expr<T>>,
        T,
    ),
    Combine(Box<Expr<T>>, CombineOp, Box<Expr<T>>, T),
    SimpleMap(Box<Expr<T>>, Box<Expr<T>>, T),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CombineOp {
    Union,
    Intersect,
    Except,
}
impl Display for CombineOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            CombineOp::Union => "union",
            CombineOp::Intersect => "intersect",
            CombineOp::Except => "except",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Quantifier {
    Some,
    Every,
}
impl Display for Quantifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Quantifier::Some => "some",
            Quantifier::Every => "every",
        };
        write!(f, "{}", s)
    }
}
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Axis {
    // forward
    Child,
    Descendant,
    Attribute,
    Self_,
    DescendantOrSelf,
    FollowingSibling,
    Following,
    // reverse
    Parent,
    Ancestor,
    PrecedingSibling,
    Preceding,
    AncestorOrSelf,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NodeTest {
    KindTest(KindTest),
    WildcardTest(Wildcard),
    NameTest(QName),
}

impl NodeTest {
    pub(crate) fn matches(
        &self,
        node_kind: NodeKind,
        principal_node_kind: NodeKind,
        node_name: Option<QName>,
    ) -> bool {
        match self {
            NodeTest::KindTest(kt) => match kt {
                KindTest::Document => node_kind == NodeKind::Document,
                KindTest::Element(opt_name, _type) => {
                    node_kind == NodeKind::Element
                        && match (opt_name, node_name) {
                            (None, _) => true,
                            (Some(qn1), Some(ref qn2)) => qn1.eqv(qn2),
                            (Some(_), None) => false,
                        }
                }
                KindTest::Attribute(opt_name, _type) => {
                    node_kind == NodeKind::Attribute
                        && match (opt_name, node_name) {
                            (None, _) => true,
                            (Some(qn1), Some(ref qn2)) => qn1.eqv(qn2),
                            (Some(_), None) => false,
                        }
                }
                KindTest::SchemaElement(_) => unimplemented!(),
                KindTest::SchemaAttribute(_) => unimplemented!(),
                KindTest::PI(_) => node_kind == NodeKind::PI,
                KindTest::Comment => node_kind == NodeKind::Comment,
                KindTest::Text => node_kind == NodeKind::Text,
                KindTest::NamespaceNode => false, // namespace axis is not supported
                KindTest::AnyKind => true,
            },
            NodeTest::WildcardTest(wc) => {
                if node_kind != principal_node_kind {
                    return false;
                }
                if let Some(qname) = node_name {
                    match wc {
                        Wildcard::Any => true,
                        Wildcard::AnyWithLocalName(local_name) => {
                            qname.name.as_str() == local_name.as_str()
                        }
                        Wildcard::AnyInNs(ns) => qname.ns.as_str() == Some(ns.as_str()),
                    }
                } else {
                    *wc == Wildcard::Any
                }
            }
            NodeTest::NameTest(test_qname) => {
                if node_kind != principal_node_kind {
                    return false;
                }
                if let Some(node_qname) = node_name {
                    node_qname.eqv(test_qname)
                } else {
                    false
                }
            }
        }
    }
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Wildcard {
    Any,
    AnyWithLocalName(String),
    AnyInNs(String),
}

impl Display for Wildcard {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Wildcard::Any => write!(f, "*"),
            Wildcard::AnyWithLocalName(name) => write!(f, "*:{}", name),
            Wildcard::AnyInNs(ns) => write!(f, "Q{{{}}}:*", ns),
        }
    }
}
#[derive(Debug, PartialEq)]
pub enum ArithmeticOp {
    Plus,
    Minus,
    Mul,
    Div,
    Idiv,
    Mod,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Decimal(Decimal),
    Double(f64),
    String(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Comp {
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,
}
impl Comp {
    pub fn general_str(&self) -> &'static str {
        match self {
            Comp::EQ => "=",
            Comp::NE => "!=",
            Comp::LT => "<",
            Comp::LE => "<=",
            Comp::GT => ">",
            Comp::GE => ">=",
        }
    }
}
impl Display for Comp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Comp::EQ => "eq",
            Comp::NE => "ne",
            Comp::LT => "lt",
            Comp::LE => "le",
            Comp::GT => "gt",
            Comp::GE => "ge",
        };
        write!(f, "{}", s)
    }
}
#[derive(Debug, PartialEq)]
pub enum NodeComp {
    Is,
    Before,
    After,
}
impl Display for NodeComp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            NodeComp::Is => "is",
            NodeComp::Before => "<<",
            NodeComp::After => ">>",
        };
        write!(f, "{}", s)
    }
}
impl Display for ArithmeticOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ArithmeticOp::Plus => f.write_str("+"),
            ArithmeticOp::Minus => f.write_str("-"),
            ArithmeticOp::Mul => f.write_str("*"),
            ArithmeticOp::Div => f.write_str("div"),
            ArithmeticOp::Idiv => f.write_str("idiv"),
            ArithmeticOp::Mod => f.write_str("mod"),
        }
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Decimal(d) => write!(f, "{}", d),
            Literal::Double(d) => write!(f, "{:e}", d),
            Literal::String(s) => write!(f, "\"{}\"", s.replace("\"", "\"\"")),
        }
    }
}

impl<T> Expr<T> {
    pub(crate) fn t(&self) -> &T {
        match self {
            Expr::Literal(_, t) => t,
            Expr::VarRef(_, t) => t,
            Expr::Sequence(_, t) => t,
            Expr::ContextItem(t) => t,
            Expr::IfThenElse(_, _, _, t) => t,
            Expr::FunctionCall(_, _, t) => t,
            Expr::Or(_, t) => t,
            Expr::And(_, t) => t,
            Expr::Concat(_, t) => t,
            Expr::Arithmetic(_, _, _, t) => t,
            Expr::UnaryMinus(_, t) => t,
            Expr::InstanceOf(_, _, t) => t,
            Expr::TreatAs(_, _, t) => t,
            Expr::Path(_, _, t) => t,
            Expr::Step(_, _, _, t) => t,
            Expr::ValueComp(_, _, _, t) => t,
            Expr::GeneralComp(_, _, _, t) => t,
            Expr::NodeComp(_, _, _, t) => t,
            Expr::Filter(_, _, t) => t,
            Expr::For(_, _, _, t) => t,
            Expr::Let(_, _, _, t) => t,
            Expr::Quantified(_, _, _, t) => t,
            Expr::Range(_, _, t) => t,
            Expr::ArraySquare(_, t) => t,
            Expr::ArrayCurly(_, t) => t,
            Expr::Map(_, t) => t,
            Expr::InlineFunction(_, _, _, t) => t,
            Expr::Combine(_, _, _, t) => t,
            Expr::SimpleMap(_, _, t) => t,
            Expr::Cast { t, .. } => t,
        }
    }
}

impl Axis {
    pub(crate) fn type_(&self, _ctx: &StaticContext) -> XdmResult<SequenceType> {
        Ok(match self {
            Axis::Child => SequenceType::Item(
                Item::KindTest(KindTest::Element(None, None)),
                Occurrence::ZeroOrMore,
            ),
            Axis::Descendant => {
                SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore)
            }
            Axis::Attribute => SequenceType::Item(
                Item::KindTest(KindTest::Attribute(None, None)),
                Occurrence::ZeroOrMore,
            ),
            Axis::Self_ => SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::One),
            Axis::DescendantOrSelf => {
                SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore)
            }
            Axis::FollowingSibling => {
                SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore)
            }
            Axis::Following => {
                SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore)
            }
            Axis::Parent => {
                SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::Optional)
            }
            Axis::Ancestor => {
                SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore)
            }
            Axis::PrecedingSibling => {
                SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore)
            }
            Axis::Preceding => {
                SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore)
            }
            Axis::AncestorOrSelf => {
                SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore)
            }
        })
    }
}
impl Display for Axis {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Axis::Child => write!(f, "child"),
            Axis::Descendant => write!(f, "descendant"),
            Axis::Attribute => write!(f, "attribute"),
            Axis::Self_ => write!(f, "self"),
            Axis::DescendantOrSelf => write!(f, "descendant-or-self"),
            Axis::FollowingSibling => write!(f, "following-sibling"),
            Axis::Following => write!(f, "following"),
            Axis::Parent => write!(f, "parent"),
            Axis::Ancestor => write!(f, "ancestor"),
            Axis::PrecedingSibling => write!(f, "preceding-sibling"),
            Axis::Preceding => write!(f, "preceding"),
            Axis::AncestorOrSelf => write!(f, "ancestor-or-self"),
        }
    }
}
impl<T> Display for Expr<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(l, _) => l.fmt(f),
            Expr::VarRef(qname, _) => write!(f, "${}", qname),
            Expr::Sequence(v, _) => write!(f, "({})", v.iter().format(", ")),
            Expr::ContextItem(_) => f.write_str("."),
            Expr::IfThenElse(i, t, e, _) => write!(f, "if ({}) then {} else {}", i, t, e),
            Expr::FunctionCall(name, args, _) => {
                write!(f, "{}({})", name, args.iter().format(", "))
            }
            Expr::Or(v, _) => write!(f, "{}", v.iter().format(" or ")),
            Expr::And(v, _) => write!(f, "{}", v.iter().format(" and ")),
            Expr::Concat(v, _) => write!(f, "{}", v.iter().format(" || ")),
            Expr::Arithmetic(e1, o, e2, _) => write!(f, "{} {} {}", e1, o, e2),
            Expr::UnaryMinus(e, _) => write!(f, "-{}", e),
            Expr::InstanceOf(e, t, _) => write!(f, "{} instance of {}", e, t),
            Expr::TreatAs(e, t, _) => write!(f, "{} treat as {}", e, t),
            Expr::Path(e1, e2, _) => write!(f, "{}/{}", e1, e2),
            Expr::Step(a, nt, ps, _) => {
                write!(f, "{}::{}", a, nt)?;
                for p in ps {
                    write!(f, "[{}]", p)?;
                }
                Ok(())
            }
            Expr::ValueComp(e1, vc, e2, _) => write!(f, "{} {} {}", e1, vc, e2),
            Expr::GeneralComp(e1, gc, e2, _) => write!(f, "{} {} {}", e1, gc.general_str(), e2),
            Expr::NodeComp(e1, nc, e2, _) => write!(f, "{} {} {}", e1, nc, e2),
            Expr::Filter(e, p, _) => write!(f, "{}[{}]", e, p),
            Expr::For(qname, bs, ret, _) => write!(f, "for ${} in {} return {}", qname, bs, ret),
            Expr::Let(qname, bs, ret, _) => write!(f, "let ${} := {} return {}", qname, bs, ret),
            Expr::Quantified(quantifier, bindings, predicate, _) => {
                write!(f, "{} ", quantifier)?;
                let last = bindings.len() - 1;
                for x in bindings.iter().enumerate() {
                    let (i, (var, expr)) = x;
                    write!(f, "${} in {}", var, expr)?;
                    if i < last {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " satisfies {}", predicate)
            }
            Expr::Range(from, to, _) => write!(f, "{} to {}", from, to),
            Expr::ArraySquare(v, _) => write!(f, "[{}]", v.iter().format(", ")),
            Expr::ArrayCurly(e, _) => write!(f, "array {{{}}}", e),
            Expr::Map(v, _) => write!(
                f,
                "map {{{}}}",
                v.iter()
                    .format_with(", ", |kv, f| { f(&format_args!("{}: {}", kv.0, kv.1)) })
            ),
            Expr::InlineFunction(v, ost, b, _) => {
                let vs = v.iter().format_with(", ", |kv, f| {
                    f(&kv
                        .1
                        .as_ref()
                        .map(|t| format!("${} as {}", kv.0, t))
                        .unwrap_or_else(|| format!("${}", kv.0)))
                });
                let st = ost
                    .as_ref()
                    .map(|st| format!("as {} ", st))
                    .unwrap_or("".to_string());
                write!(f, "function({}) {}{{ {} }}", vs, st, b)
            }
            Expr::Combine(left, op, right, _) => write!(f, "{} {} {}", left, op, right),
            Expr::SimpleMap(e1, e2, _) => write!(f, "({})!{}", e1, e2),
            Expr::Cast {
                expression,
                simple_type,
                optional,
                only_check,
                t: _,
            } => write!(
                f,
                "{} cast{} as {}{}",
                expression,
                if *only_check { "able" } else { "" },
                simple_type,
                if *optional { " ?" } else { "" }
            ),
        }
    }
}

impl Display for NodeTest {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            NodeTest::KindTest(kt) => write!(f, "{}", kt),
            NodeTest::NameTest(qname) => write!(f, "{}", qname),
            NodeTest::WildcardTest(wildcard) => write!(f, "{}", wildcard),
        }
    }
}
impl Literal {
    pub(crate) fn compile(self) -> XdmResult<CompiledExpr> {
        Ok(match self {
            Literal::Integer(i) => CompiledExpr::new(move |_c| Ok(Xdm::Integer(i))),
            Literal::Decimal(d) => CompiledExpr::new(move |_c| Ok(Xdm::Decimal(d))),
            Literal::Double(d) => CompiledExpr::new(move |_c| Ok(Xdm::Double(d))),
            Literal::String(s) => CompiledExpr::new(move |_c| Ok(Xdm::String(s.clone()))),
        })
    }
    pub(crate) fn type_(&self, ctx: &StaticContext) -> SequenceType {
        match self {
            Literal::Integer(_) => SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:integer")).unwrap()),
                Occurrence::One,
            ),
            Literal::Decimal(_) => SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:decimal")).unwrap()),
                Occurrence::One,
            ),
            Literal::Double(_) => SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:double")).unwrap()),
                Occurrence::One,
            ),
            Literal::String(_) => SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:string")).unwrap()),
                Occurrence::One,
            ),
        }
    }
}
impl Comp {
    pub(crate) fn comparison_true(&self, c: i8) -> bool {
        match (self, c) {
            (Comp::EQ, 0) => true,
            (Comp::EQ, _) => false,
            (Comp::NE, 0) => false,
            (Comp::NE, _) => true,
            (Comp::LT, -1) => true,
            (Comp::LT, _) => false,
            (Comp::LE, 1) => false,
            (Comp::LE, _) => true,
            (Comp::GT, 1) => true,
            (Comp::GT, _) => false,
            (Comp::GE, -1) => false,
            (Comp::GE, _) => true,
        }
    }
}
