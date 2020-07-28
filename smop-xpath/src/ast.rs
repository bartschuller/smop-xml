use crate::context::StaticContext;
use crate::runtime::CompiledExpr;
use crate::types::{Item, KindTest};
use crate::types::{Occurrence, SequenceType};
use crate::xdm::*;
use crate::xpath_functions_31::{double_compare, string_compare};
use itertools::Itertools;
use num_traits::ToPrimitive;
use rust_decimal::Decimal;
use smop_xmltree::nod::{Node, NodeKind, QName};
use smop_xmltree::option_ext::OptionExt;
use std::borrow::Borrow;
use std::collections::BTreeSet;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::Add;
use std::ops::{Div, Mul, Rem, Sub};
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
    Path(Box<Expr<T>>, Box<Expr<T>>, T),
    Step(Axis, NodeTest, Vec<Expr<T>>, T),
    ValueComp(Box<Expr<T>>, Comp, Box<Expr<T>>, T),
    GeneralComp(Box<Expr<T>>, Comp, Box<Expr<T>>, T),
    NodeComp(Box<Expr<T>>, NodeComp, Box<Expr<T>>, T),
    Predicate(Box<Expr<T>>, Box<Expr<T>>, T),
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
    fn matches(
        &self,
        node_kind: NodeKind,
        principal_node_kind: NodeKind,
        node_name: Option<QName>,
    ) -> bool {
        match self {
            NodeTest::KindTest(kt) => match kt {
                KindTest::Document => node_kind == NodeKind::Document,
                KindTest::Element => node_kind == NodeKind::Element,
                KindTest::Attribute => node_kind == NodeKind::Attribute,
                KindTest::SchemaElement => unimplemented!(),
                KindTest::SchemaAttribute => unimplemented!(),
                KindTest::PI => node_kind == NodeKind::PI,
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
            Expr::Predicate(_, _, t) => t,
            Expr::For(_, _, _, t) => t,
            Expr::Let(_, _, _, t) => t,
            Expr::Quantified(_, _, _, t) => t,
            Expr::Range(_, _, t) => t,
            Expr::ArraySquare(_, t) => t,
            Expr::ArrayCurly(_, t) => t,
            Expr::Map(_, t) => t,
            Expr::InlineFunction(_, _, _, t) => t,
            Expr::Combine(_, _, _, t) => t,
        }
    }
}
impl Expr<(SequenceType, Rc<StaticContext>)> {
    pub(crate) fn compile(self) -> XdmResult<CompiledExpr> {
        match self {
            Expr::Literal(l, _) => l.compile(),
            Expr::VarRef(qname, _) => Ok(CompiledExpr::new(move |c| {
                c.varref(&qname).ok_or_else(|| {
                    XdmError::xqtm("XPST0008", format!("variable `{}` not found", qname))
                })
            })),
            Expr::Sequence(s, _t) => {
                let compiled_vec: XdmResult<Vec<_>> = s.into_iter().map(|x| x.compile()).collect();
                compiled_vec.map(|compiled_vec| {
                    CompiledExpr::new(move |c| {
                        let v: XdmResult<Vec<_>> =
                            compiled_vec.iter().map(|e| e.execute(c)).collect();
                        v.map(|vecx| {
                            Xdm::Sequence(
                                vecx.into_iter()
                                    .flat_map(|x| match x {
                                        Xdm::Sequence(v) => v,
                                        _ => vec![x],
                                    })
                                    .collect(),
                            )
                        })
                    })
                })
            }
            Expr::IfThenElse(condition_expr, if_expr, else_expr, _) => {
                let condition = condition_expr.compile()?;
                let if_branch = if_expr.compile()?;
                let else_branch = else_expr.compile()?;
                Ok(CompiledExpr::new(move |c| {
                    if condition.execute(c)?.boolean()? {
                        if_branch.execute(c)
                    } else {
                        else_branch.execute(c)
                    }
                }))
            }
            Expr::ContextItem(_) => Ok(CompiledExpr::new(move |c| {
                if let Some(ref focus) = c.focus {
                    match &focus.sequence {
                        Xdm::Sequence(v) => v
                            .get(focus.position)
                            .map(|x| x.clone())
                            .ok_or(XdmError::xqtm("XPDY0002", "context item is undefined")),
                        x if focus.position == 0 => Ok((*x).clone()),
                        _ => Err(XdmError::xqtm("XPDY0002", "context item is undefined")),
                    }
                } else {
                    Err(XdmError::xqtm("XPDY0002", "context item is undefined"))
                }
            })),
            Expr::FunctionCall(qname, args, (_st, static_context)) => {
                let code = (static_context.function(&qname, args.len()).unwrap().code)();
                let compiled_vec: XdmResult<Vec<_>> =
                    args.into_iter().map(|x| x.compile()).collect();
                compiled_vec.map(|compiled_vec| {
                    CompiledExpr::new(move |c| {
                        let v: XdmResult<Vec<Xdm>> =
                            compiled_vec.iter().map(|e| e.execute(c)).collect();
                        let v_ok = v?;
                        code.execute(c, v_ok)
                    })
                })
            }
            Expr::Or(v, _) => {
                let v_c: XdmResult<Vec<_>> = v.into_iter().map(|x| x.compile()).collect();
                let v_c = v_c?;
                Ok(CompiledExpr::new(move |c| {
                    for e in v_c.iter() {
                        let res = e.execute(c)?.boolean()?;
                        if res {
                            return Ok(Xdm::Boolean(true));
                        }
                    }
                    Ok(Xdm::Boolean(false))
                }))
            }
            Expr::And(v, _) => {
                let v_c: XdmResult<Vec<_>> = v.into_iter().map(|x| x.compile()).collect();
                let v_c = v_c?;
                Ok(CompiledExpr::new(move |c| {
                    for e in v_c.iter() {
                        let res = e.execute(c)?.boolean()?;
                        if !res {
                            return Ok(Xdm::Boolean(false));
                        }
                    }
                    Ok(Xdm::Boolean(true))
                }))
            }
            Expr::Concat(v, _) => {
                let v_c: XdmResult<Vec<_>> = v.into_iter().map(|x| x.compile()).collect();
                let v_c = v_c?;
                Ok(CompiledExpr::new(move |c| {
                    let strings: XdmResult<Vec<_>> = v_c
                        .iter()
                        .map(|e| e.execute(c).map(|x| x.string()))
                        .collect();
                    let strings: XdmResult<Vec<_>> = strings?.into_iter().collect();
                    Ok(Xdm::String(strings?.concat()))
                }))
            }
            Expr::Arithmetic(l, o, r, t) => match t.0 {
                SequenceType::EmptySequence => {
                    println!("warning: compiling away arithmetic op to empty sequence");
                    Ok(CompiledExpr::new(move |_c| Ok(Xdm::Sequence(vec![]))))
                }
                SequenceType::Item(i, _) => {
                    // if o != ArithmeticOp::Plus {
                    //     todo!("operations beside plus")
                    // }
                    let l_c = l.compile()?;
                    let r_c = r.compile()?;
                    let type_string = i.to_string();
                    macro_rules! operation {
                        ($type_string:ident, $l:ident, $op:ident, $r:ident) => {
                            Ok(match $type_string.as_ref() {
                                "xs:integer" => CompiledExpr::new(move |c| {
                                    Ok(Xdm::Integer(
                                        $l.execute(c)?.integer()?.$op($r.execute(c)?.integer()?),
                                    ))
                                }),
                                "xs:decimal" => CompiledExpr::new(move |c| {
                                    Ok(Xdm::Decimal(
                                        $l.execute(c)?.decimal()?.$op($r.execute(c)?.decimal()?),
                                    ))
                                }),
                                "xs:double" | "xs:anyAtomicType" => CompiledExpr::new(move |c| {
                                    Ok(Xdm::Double(
                                        $l.execute(c)?.double()?.$op($r.execute(c)?.double()?),
                                    ))
                                }),
                                _ => todo!("compile more Arithmetic cases"),
                            })
                        };
                    }
                    match o {
                        ArithmeticOp::Plus => operation![type_string, l_c, add, r_c],
                        ArithmeticOp::Minus => operation![type_string, l_c, sub, r_c],
                        ArithmeticOp::Mul => operation![type_string, l_c, mul, r_c],
                        ArithmeticOp::Div => operation![type_string, l_c, div, r_c],
                        ArithmeticOp::Mod => operation![type_string, l_c, rem, r_c],
                        ArithmeticOp::Idiv => Ok(match type_string.as_ref() {
                            "xs:integer" => CompiledExpr::new(move |c| {
                                Ok(Xdm::Integer(
                                    l_c.execute(c)?.integer()?.div(r_c.execute(c)?.integer()?),
                                ))
                            }),
                            "xs:decimal" => CompiledExpr::new(move |c| {
                                let oi = l_c
                                    .execute(c)?
                                    .decimal()?
                                    .div(r_c.execute(c)?.decimal()?)
                                    .to_i64();
                                if let Some(i) = oi {
                                    Ok(Xdm::Integer(i))
                                } else {
                                    Err(XdmError::xqtm(
                                        "FOAR0002",
                                        "overflow/underflow in idiv on xs:decimal",
                                    ))
                                }
                            }),
                            "xs:double" | "xs:anyAtomicType" => CompiledExpr::new(move |c| {
                                Ok(Xdm::Integer(
                                    l_c.execute(c)?.double()?.div(r_c.execute(c)?.double()?) as i64,
                                ))
                            }),
                            _ => todo!("compile more Arithmetic cases"),
                        }),
                    }
                }
            },
            Expr::UnaryMinus(e, t) => match t.0 {
                SequenceType::EmptySequence => {
                    println!("warning: compiling away unary minus to empty sequence");
                    Ok(CompiledExpr::new(move |_c| Ok(Xdm::Sequence(vec![]))))
                }
                SequenceType::Item(i, _) => {
                    let e_c = e.compile()?;
                    let type_string = i.to_string();
                    Ok(match type_string.as_ref() {
                        "xs:integer" => CompiledExpr::new(move |c| {
                            Ok(Xdm::Integer(-e_c.execute(c)?.integer()?))
                        }),
                        "xs:decimal" => CompiledExpr::new(move |c| {
                            Ok(Xdm::Decimal(-e_c.execute(c)?.decimal()?))
                        }),
                        "xs:double" | "xs:anyAtomicType" => {
                            CompiledExpr::new(move |c| Ok(Xdm::Double(-e_c.execute(c)?.double()?)))
                        }
                        _ => todo!("compile more unary minus cases: {}", type_string),
                    })
                }
            },
            Expr::InstanceOf(e, st, _t) => {
                let ce = e.compile()?;
                Ok(CompiledExpr::new(move |c| {
                    let x = ce.execute(c)?;
                    let st2 = x.dynamic_type(&c.static_context)?;
                    let lub = SequenceType::lub(&c.static_context, &st, &st2)?;
                    Ok(Xdm::Boolean(lub == st))
                }))
            }
            Expr::TreatAs(e, _st, _) => {
                let ce = e.compile()?;
                // FIXME there's probably more that should be done here.
                Ok(ce)
            }
            Expr::Path(s1, s2, _) => {
                let e1 = s1.compile()?;
                let e2 = s2.compile()?;
                Ok(CompiledExpr::new(move |c| {
                    let x1 = e1.execute(c)?;
                    let raw_result = match x1 {
                        Xdm::NodeSeq(NodeSeq::RoXmlIter(mut ai)) => {
                            let mut result: Vec<Xdm> = Vec::new();
                            while let Some(ronode) = ai.next() {
                                let x = Xdm::NodeSeq(NodeSeq::RoXml(ronode));
                                let context = c.clone_with_focus(x, ai.position);
                                let res = e2.execute(&context)?;
                                result.push(res);
                            }
                            if result.len() == 1 {
                                Ok(result.remove(0))
                            } else {
                                Ok(Xdm::Sequence(result))
                            }
                        }
                        Xdm::NodeSeq(NodeSeq::RoXml(ref _ronode)) => {
                            let context = c.clone_with_focus(x1, 0);
                            e2.execute(&context)
                        }
                        Xdm::Sequence(v) => {
                            let mut result: Vec<Xdm> = Vec::new();
                            for x in v.into_iter().enumerate() {
                                let context = c.clone_with_focus(x.1, x.0);
                                let res = e2.execute(&context)?;
                                result.push(res);
                            }

                            Ok(Xdm::flatten(result))
                        }
                        _ => Err(XdmError::xqtm(
                            "XPTY0019",
                            format!("Not a node seq: {:?}", x1),
                        )),
                    }?;

                    let mut values_vec: Vec<Xdm> = Vec::new();
                    let mut nodes_btree: BTreeSet<Node> = BTreeSet::new();
                    for x in raw_result {
                        match x {
                            Xdm::NodeSeq(NodeSeq::RoXml(node)) => {
                                nodes_btree.insert(node);
                            }
                            y => values_vec.push(y),
                        }
                    }
                    if nodes_btree.is_empty() {
                        Ok(Xdm::flatten(values_vec))
                    } else if values_vec.is_empty() {
                        Ok(Xdm::flatten(
                            nodes_btree
                                .into_iter()
                                .map(|n| Xdm::NodeSeq(NodeSeq::RoXml(n)))
                                .collect(),
                        ))
                    } else {
                        Err(XdmError::xqtm(
                            "XPTY0018",
                            "result of a path operator contains both nodes and non-nodes",
                        ))
                    }
                }))
            }
            Expr::Step(axis, nt, ps, _) => {
                let nt = Box::new(nt);
                let predicates: XdmResult<Vec<_>> = ps.into_iter().map(|x| x.compile()).collect();
                let predicates = predicates?;
                Ok(CompiledExpr::new(move |c| {
                    let ci = c
                        .focus
                        .as_ref()
                        .ok_or(XdmError::xqtm("XPDY0002", "context item is undefined"))?;
                    let ro_node = match &ci.sequence {
                        Xdm::NodeSeq(NodeSeq::RoXml(n)) => Ok(n),
                        _ => Err(XdmError::xqtm("", "didn't get a roxml node")),
                    }?;
                    let node_iterator: Box<dyn Iterator<Item = (usize, Node)>> = match axis {
                        Axis::Child => Box::new(
                            ro_node
                                .children()
                                .filter(|child| {
                                    nt.matches(
                                        child.node_kind(),
                                        NodeKind::Element,
                                        child.node_name(),
                                    )
                                })
                                .enumerate(),
                        ),
                        Axis::Attribute => Box::new(
                            ro_node
                                .attributes()
                                .filter(|a| {
                                    nt.matches(a.node_kind(), NodeKind::Attribute, a.node_name())
                                })
                                .enumerate(),
                        ),
                        Axis::Self_ => Box::new(
                            std::iter::once(ro_node.clone())
                                .filter(|ro_node| {
                                    nt.matches(
                                        ro_node.node_kind(),
                                        NodeKind::Element,
                                        ro_node.node_name(),
                                    )
                                })
                                .enumerate(),
                        ),
                        Axis::DescendantOrSelf => Box::new(
                            ro_node
                                .descendants_or_self()
                                .filter(|child| {
                                    nt.matches(
                                        child.node_kind(),
                                        NodeKind::Element,
                                        child.node_name(),
                                    )
                                })
                                .enumerate(),
                        ),
                        Axis::Descendant => Box::new(
                            ro_node
                                .descendants_or_self()
                                .skip(1)
                                .filter(|child| {
                                    nt.matches(
                                        child.node_kind(),
                                        NodeKind::Element,
                                        child.node_name(),
                                    )
                                })
                                .enumerate(),
                        ),
                        Axis::Parent => Box::new(
                            ro_node
                                .parent()
                                .into_iter()
                                .filter(|parent| {
                                    nt.matches(
                                        parent.node_kind(),
                                        NodeKind::Element,
                                        parent.node_name(),
                                    )
                                })
                                .enumerate(),
                        ),
                        _ => unimplemented!("axis {}", axis),
                    };
                    let result_nodes: Vec<_> = node_iterator
                        .filter_map(|(pos, result_node)| {
                            let node = Xdm::NodeSeq(NodeSeq::RoXml(result_node));
                            let mut include = true;
                            let context = c.clone_with_focus(node, pos);
                            for predicate in &predicates {
                                let pred = predicate.execute(&context).unwrap();
                                match pred {
                                    Xdm::Decimal(_) | Xdm::Integer(_) | Xdm::Double(_) => {
                                        if pred.integer().unwrap() as usize - 1 != pos {
                                            include = false;
                                            break;
                                        }
                                    }
                                    _ => {
                                        if !pred.boolean().unwrap() {
                                            include = false;
                                            break;
                                        }
                                    }
                                }
                            }
                            if include {
                                Some(context.focus.unwrap().sequence)
                            } else {
                                None
                            }
                        })
                        .collect();
                    Ok(Xdm::flatten(result_nodes))
                    // FIXME this should be done at compile time
                }))
            }
            Expr::ValueComp(e1, vc, e2, _) => {
                let c1 = e1.compile()?;
                let c2 = e2.compile()?;
                Ok(CompiledExpr::new(move |c| {
                    let a1 = c1.execute(c)?.atomize()?;
                    let a2 = c2.execute(c)?.atomize()?;
                    match (a1, a2) {
                        (Xdm::Sequence(v), _) | (_, Xdm::Sequence(v)) if v.is_empty() => {
                            Ok(Xdm::Sequence(vec![]))
                        }
                        (Xdm::Sequence(_), _) | (_, Xdm::Sequence(_)) => Err(XdmError::xqtm(
                            "XPTY0004",
                            "value comparison argument is a sequence",
                        )),
                        (x1, x2) => x1.xpath_compare(&x2, vc).map(Xdm::Boolean),
                    }
                }))
            }
            Expr::GeneralComp(e1, vc, e2, _) => {
                let c1 = e1.compile()?;
                let c2 = e2.compile()?;
                Ok(CompiledExpr::new(move |c| {
                    let a1_seq = c1.execute(c)?.atomize()?;
                    let a2_seq = c2.execute(c)?.atomize()?;
                    let a2_vec: Vec<_> = a2_seq.into_iter().collect();
                    let true_val = Ok(Xdm::Boolean(true));
                    for ref a1 in a1_seq {
                        for a2 in a2_vec.iter() {
                            match (a1, a2) {
                                (Xdm::String(s1), Xdm::String(s2)) => {
                                    if vc.comparison_true(string_compare(s1.as_str(), s2.as_str()))
                                    {
                                        return true_val;
                                    }
                                }
                                (Xdm::Double(_), _)
                                | (Xdm::Decimal(_), _)
                                | (Xdm::Integer(_), _)
                                | (_, Xdm::Double(_))
                                | (_, Xdm::Decimal(_))
                                | (_, Xdm::Integer(_)) => {
                                    if vc.comparison_true(double_compare(
                                        a1.double()?.borrow(),
                                        a2.double()?.borrow(),
                                    )) {
                                        return true_val;
                                    }
                                }
                                (a1, a2) => unimplemented!("{:?} {} {:?}", a1, vc, a2),
                            }
                        }
                    }
                    return Ok(Xdm::Boolean(false));
                }))
            }
            Expr::NodeComp(_e1, _nc, _e2, _) => unimplemented!(),
            Expr::Predicate(e, p, _) => {
                let ce = e.compile()?;
                let cp = p.compile()?;
                Ok(CompiledExpr::new(move |c| {
                    let seq = ce.execute(c)?;
                    let mut iter = seq.into_iter().enumerate();
                    let mut result: Vec<Xdm> = Vec::new();
                    while let Some((pos, x)) = iter.next() {
                        let context = c.clone_with_focus(x, pos);
                        let pred = cp.execute(&context)?;
                        match pred {
                            Xdm::Decimal(_) | Xdm::Integer(_) | Xdm::Double(_) => {
                                if pred.integer()? as usize - 1 == pos {
                                    result.push(context.focus.unwrap().sequence);
                                }
                            }
                            _ => {
                                if pred.boolean()? {
                                    result.push(context.focus.unwrap().sequence);
                                }
                            }
                        }
                    }
                    if result.len() == 1 {
                        Ok(result.remove(0))
                    } else {
                        Ok(Xdm::Sequence(result))
                    }
                }))
            }
            Expr::For(qname, in_expr, ret_expr, _) => {
                let ic = in_expr.compile()?;
                let rc = ret_expr.compile()?;

                Ok(CompiledExpr::new(move |c| {
                    let binding_seq = ic.execute(c)?;
                    let mut result: Vec<Xdm> = Vec::new();
                    for val in binding_seq {
                        let context = c.clone_with_variable(qname.clone(), val);
                        let ret_val = rc.execute(&context)?;
                        result.extend(ret_val.into_iter());
                    }
                    if result.len() == 1 {
                        Ok(result.remove(0))
                    } else {
                        Ok(Xdm::Sequence(result))
                    }
                }))
            }
            Expr::Let(qname, binding_seq, ret_expr, _) => {
                let b_compiled = binding_seq.compile()?;
                let r_compiled = ret_expr.compile()?;
                Ok(CompiledExpr::new(move |c| {
                    let val = b_compiled.execute(c)?;
                    let context = c.clone_with_variable(qname.clone(), val);
                    r_compiled.execute(&context)
                }))
            }
            Expr::Quantified(quantifier, bindings, predicate, _) => {
                let bs_compiled: XdmResult<Vec<(QName, CompiledExpr)>> = bindings
                    .into_iter()
                    .map(|(q, b)| b.compile().map(|comp| (q, comp)))
                    .collect();
                let _bs_compiled = bs_compiled?;
                let pred_compiled = predicate.compile()?;
                let default_return_value = match quantifier {
                    Quantifier::Some => false,
                    Quantifier::Every => true,
                };
                Ok(CompiledExpr::new(move |c| {
                    // for every bs_compiled:
                    // - execute
                    // - create a new context
                    // - set  variable in context
                    // use context for the next bs or if last, for the pred
                    // this is already ok for inside the inner loop:
                    if pred_compiled.execute(c)?.boolean()? != default_return_value {
                        return Ok(Xdm::Boolean(!default_return_value));
                    }
                    Ok(Xdm::Boolean(default_return_value))
                }))
            }
            Expr::Range(_, _, _) => unimplemented!(),
            Expr::ArraySquare(_, _) => unimplemented!(),
            Expr::ArrayCurly(_, _) => unimplemented!(),
            Expr::Map(_, _) => unimplemented!(),
            Expr::InlineFunction(_, _, _, _) => unimplemented!(),
            Expr::Combine(left, op, right, _) => {
                let left_c = left.compile()?;
                let right_c = right.compile()?;
                Ok(CompiledExpr::new(move |c| {
                    left_c.execute(c)?.combine(right_c.execute(c)?, op)
                }))
            }
        }
    }
}

impl Expr<()> {
    pub(crate) fn type_(
        self,
        ctx: Rc<StaticContext>,
    ) -> XdmResult<Expr<(SequenceType, Rc<StaticContext>)>> {
        match self {
            Expr::Literal(l, _) => {
                let l_type = l.type_(&ctx);
                Ok(Expr::Literal(l, (l_type, ctx)))
            }
            Expr::VarRef(qname, _) => ctx
                .variable_type(&qname)
                .ok_or_else(|| {
                    XdmError::xqtm(
                        "XPST0008",
                        format!("variable ${} not found in static context", qname),
                    )
                })
                .map(|type_| Expr::VarRef(qname, (type_, ctx))),
            Expr::Sequence(v, _) => {
                if v.is_empty() {
                    Ok(Expr::Sequence(vec![], (SequenceType::EmptySequence, ctx)))
                } else {
                    assert!(v.len() > 1);
                    let v_typed: XdmResult<Vec<_>> =
                        v.into_iter().map(|e| e.type_(Rc::clone(&ctx))).collect();
                    let v_typed = v_typed?;
                    let child_types = v_typed.iter().map(|e| e.t().0.clone()).collect();
                    Ok(Expr::Sequence(
                        v_typed,
                        (SequenceType::add_vec(&Rc::clone(&ctx), child_types)?, ctx),
                    ))
                }
            }
            Expr::ContextItem(_) => Ok(Expr::ContextItem((ctx.context_item_type.clone(), ctx))),
            Expr::IfThenElse(i, t, e, _) => {
                let i_typed = i.type_(Rc::clone(&ctx))?;
                let t_typed = t.type_(Rc::clone(&ctx))?;
                let e_typed = e.type_(Rc::clone(&ctx))?;
                let e_type = e_typed.t().0.clone();
                let t_type = t_typed.t().0.clone();
                Ok(Expr::IfThenElse(
                    Box::new(i_typed),
                    Box::new(t_typed),
                    Box::new(e_typed),
                    (SequenceType::lub(&ctx, &t_type, &e_type)?, ctx),
                ))
            }
            Expr::FunctionCall(qname, args, _) => {
                let arity = args.len();
                let args_typed: XdmResult<Vec<_>> =
                    args.into_iter().map(|a| a.type_(Rc::clone(&ctx))).collect();
                let args_typed = args_typed?;
                let ctx2 = Rc::clone(&ctx);
                ctx.function(&qname, arity)
                    .map(|func| {
                        // FIXME check argument types (and remember to special case concat)
                        Expr::FunctionCall(qname.clone(), args_typed, (func.type_.clone(), ctx2))
                    })
                    .ok_or_else(|| {
                        let msg = format!("No function {}#{} found", qname, arity);
                        XdmError::xqtm("XPST0017", msg.as_str())
                    })
            }
            Expr::Or(v, _) => {
                let v_typed: XdmResult<Vec<_>> =
                    v.into_iter().map(|a| a.type_(Rc::clone(&ctx))).collect();
                Ok(Expr::Or(
                    v_typed?,
                    (
                        SequenceType::Item(
                            Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:boolean"))?),
                            Occurrence::One,
                        ),
                        ctx,
                    ),
                ))
            }
            Expr::And(v, _) => {
                let v_typed: XdmResult<Vec<_>> =
                    v.into_iter().map(|a| a.type_(Rc::clone(&ctx))).collect();
                Ok(Expr::And(
                    v_typed?,
                    (
                        SequenceType::Item(
                            Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:boolean"))?),
                            Occurrence::One,
                        ),
                        ctx,
                    ),
                ))
            }
            Expr::Concat(v, _) => {
                let v_typed: XdmResult<Vec<_>> =
                    v.into_iter().map(|a| a.type_(Rc::clone(&ctx))).collect();
                Ok(Expr::Concat(
                    v_typed?,
                    (
                        SequenceType::Item(
                            Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:string"))?),
                            Occurrence::One,
                        ),
                        ctx,
                    ),
                ))
            }
            Expr::Arithmetic(l, op, r, _) => {
                let t1 = l.type_(Rc::clone(&ctx))?;
                let t2 = r.type_(Rc::clone(&ctx))?;
                let t1_type = untyped_to_double(t1.t().0.atomize(&ctx)?, &ctx);
                let t2_type = untyped_to_double(t2.t().0.atomize(&ctx)?, &ctx);
                let result_type = match op {
                    ArithmeticOp::Idiv => SequenceType::Item(
                        Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:integer"))?),
                        Occurrence::One,
                    ),
                    _ => SequenceType::lub(&ctx, &t1_type, &t2_type)?,
                };
                Ok(Expr::Arithmetic(
                    Box::new(t1),
                    op,
                    Box::new(t2),
                    (result_type, ctx),
                ))
            }
            Expr::UnaryMinus(e, _) => {
                let e_typed = e.type_(Rc::clone(&ctx))?;
                let e_type = e_typed.t().0.clone();
                Ok(Expr::UnaryMinus(Box::new(e_typed), (e_type, ctx)))
            }
            Expr::InstanceOf(e, st, _) => {
                let e_typed = e.type_(Rc::clone(&ctx))?;
                Ok(Expr::InstanceOf(
                    Box::new(e_typed),
                    st,
                    (
                        SequenceType::Item(
                            Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:boolean"))?),
                            Occurrence::One,
                        ),
                        ctx,
                    ),
                ))
            }
            Expr::TreatAs(e, st, _) => {
                let e_typed = e.type_(Rc::clone(&ctx))?;
                Ok(Expr::TreatAs(Box::new(e_typed), st.clone(), (st, ctx)))
            }
            Expr::Path(e1, e2, _) => {
                let e1_typed = e1.type_(Rc::clone(&ctx))?;
                let e2_typed = e2.type_(Rc::clone(&ctx))?;
                let e2_type = e2_typed.t().0.clone();
                Ok(Expr::Path(
                    Box::new(e1_typed),
                    Box::new(e2_typed),
                    (e2_type, ctx),
                ))
            }
            Expr::Step(a, nt, ps, _) => {
                let ps_typed: XdmResult<Vec<_>> =
                    ps.into_iter().map(|a| a.type_(Rc::clone(&ctx))).collect();
                Ok(Expr::Step(
                    a,
                    nt,
                    ps_typed?,
                    (a.type_(&Rc::clone(&ctx))?, ctx),
                ))
            }
            Expr::ValueComp(e1, vc, e2, _) => {
                let e1_typed = e1.type_(Rc::clone(&ctx))?;
                let e2_typed = e2.type_(Rc::clone(&ctx))?;
                let ret_type = SequenceType::Item(
                    Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:boolean"))?),
                    Occurrence::Optional,
                );
                Ok(Expr::ValueComp(
                    Box::new(e1_typed),
                    vc,
                    Box::new(e2_typed),
                    (ret_type, ctx),
                ))
            }
            Expr::GeneralComp(e1, gc, e2, _) => {
                let e1_typed = e1.type_(Rc::clone(&ctx))?;
                let e2_typed = e2.type_(Rc::clone(&ctx))?;
                let ret_type = SequenceType::Item(
                    Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:boolean"))?),
                    Occurrence::One,
                );
                Ok(Expr::GeneralComp(
                    Box::new(e1_typed),
                    gc,
                    Box::new(e2_typed),
                    (ret_type, ctx),
                ))
            }
            Expr::NodeComp(e1, gc, e2, _) => {
                let e1_typed = e1.type_(Rc::clone(&ctx))?;
                let e2_typed = e2.type_(Rc::clone(&ctx))?;
                let ret_type = SequenceType::Item(
                    Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:boolean"))?),
                    Occurrence::Optional,
                );
                Ok(Expr::NodeComp(
                    Box::new(e1_typed),
                    gc,
                    Box::new(e2_typed),
                    (ret_type, ctx),
                ))
            }
            Expr::Predicate(e, p, _) => {
                let e_typed = e.type_(Rc::clone(&ctx))?;
                let p_typed = p.type_(Rc::clone(&ctx))?;
                let e_type = e_typed.t().0.clone();
                Ok(Expr::Predicate(
                    Box::new(e_typed),
                    Box::new(p_typed),
                    (e_type, ctx),
                ))
            }
            Expr::For(qname, bs, e, _) => {
                let bs_typed = bs.type_(Rc::clone(&ctx))?;
                let bi_type = match bs_typed.t().0.clone() {
                    SequenceType::EmptySequence => SequenceType::EmptySequence,
                    SequenceType::Item(it, _o) => SequenceType::Item(it, Occurrence::One),
                };
                let mut new_ctx = (&*ctx).clone();
                new_ctx.set_variable_type(&qname, bi_type);
                let e_typed = e.type_(Rc::new(new_ctx))?;
                let e_type = e_typed.t().0.clone();
                Ok(Expr::For(
                    qname,
                    Box::new(bs_typed),
                    Box::new(e_typed),
                    (e_type, ctx),
                ))
            }
            Expr::Let(qname, bs, e, _) => {
                let bs_typed = bs.type_(Rc::clone(&ctx))?;
                let bi_type = bs_typed.t().0.clone();
                let mut new_ctx = (&*ctx).clone();
                new_ctx.set_variable_type(&qname, bi_type);
                let e_typed = e.type_(Rc::new(new_ctx))?;
                let e_type = e_typed.t().0.clone();
                Ok(Expr::Let(
                    qname,
                    Box::new(bs_typed),
                    Box::new(e_typed),
                    (e_type, ctx),
                ))
            }
            Expr::Quantified(quantifier, bindings, predicate, _) => {
                let mut curr_ctx = Rc::clone(&ctx);
                let mut new_ctx: StaticContext;
                let mut bs_typed: Vec<(QName, Box<Expr<(SequenceType, Rc<StaticContext>)>>)> =
                    Vec::with_capacity(bindings.len());
                for x in bindings {
                    let (var, expr) = x;
                    let binding_type = expr.type_(Rc::clone(&curr_ctx))?;
                    let bi_type = match binding_type.t().0.clone() {
                        SequenceType::EmptySequence => SequenceType::EmptySequence,
                        SequenceType::Item(it, _o) => SequenceType::Item(it, Occurrence::One),
                    };
                    bs_typed.push((var.clone(), Box::new(binding_type)));
                    new_ctx = (&*curr_ctx).clone();
                    new_ctx.set_variable_type(&var, bi_type);
                    curr_ctx = Rc::new(new_ctx);
                }
                let result_type = SequenceType::Item(
                    Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:boolean"))?),
                    Occurrence::One,
                );
                let pred_typed = predicate.type_(Rc::clone(&curr_ctx))?;
                Ok(Expr::Quantified(
                    quantifier,
                    bs_typed,
                    Box::new(pred_typed),
                    (result_type, curr_ctx),
                ))
            }
            Expr::Range(_, _, _) => unimplemented!(),
            Expr::ArraySquare(_, _) => unimplemented!(),
            Expr::ArrayCurly(_, _) => unimplemented!(),
            Expr::Map(_, _) => unimplemented!(),
            Expr::InlineFunction(_, _, _, _) => unimplemented!(),
            Expr::Combine(left, op, right, _) => {
                let left_typed = left.type_(Rc::clone(&ctx))?;
                let right_typed = right.type_(Rc::clone(&ctx))?;
                let result_type =
                    SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore);
                Ok(Expr::Combine(
                    Box::new(left_typed),
                    op,
                    Box::new(right_typed),
                    (result_type, ctx),
                ))
            }
        }
    }
}
fn untyped_to_double(st: SequenceType, ctx: &Rc<StaticContext>) -> SequenceType {
    let xs_any_atomic = ctx
        .schema_type(("http://www.w3.org/2001/XMLSchema", "anyAtomicType"))
        .unwrap();
    match &st {
        SequenceType::Item(Item::AtomicOrUnion(schema_type), o)
            if **schema_type == *xs_any_atomic =>
        {
            SequenceType::Item(
                Item::AtomicOrUnion(
                    ctx.schema_type(("http://www.w3.org/2001/XMLSchema", "double"))
                        .unwrap(),
                ),
                o.clone(),
            )
        }

        _ => st,
    }
}
impl Axis {
    pub(crate) fn type_(&self, _ctx: &StaticContext) -> XdmResult<SequenceType> {
        Ok(match self {
            Axis::Child => {
                SequenceType::Item(Item::KindTest(KindTest::Element), Occurrence::ZeroOrMore)
            }
            Axis::Descendant => {
                SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore)
            }
            Axis::Attribute => {
                SequenceType::Item(Item::KindTest(KindTest::Attribute), Occurrence::ZeroOrMore)
            }
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
            Expr::Predicate(e, p, _) => write!(f, "{}[{}]", e, p),
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
    fn compile(self) -> XdmResult<CompiledExpr> {
        Ok(match self {
            Literal::Integer(i) => CompiledExpr::new(move |_c| Ok(Xdm::Integer(i))),
            Literal::Decimal(d) => CompiledExpr::new(move |_c| Ok(Xdm::Decimal(d))),
            Literal::Double(d) => CompiledExpr::new(move |_c| Ok(Xdm::Double(d))),
            Literal::String(s) => CompiledExpr::new(move |_c| Ok(Xdm::String(s.clone()))),
        })
    }
    fn type_(&self, ctx: &StaticContext) -> SequenceType {
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
