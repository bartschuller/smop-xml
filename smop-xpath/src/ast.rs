use crate::context::StaticContext;
use crate::runtime::CompiledExpr;
use crate::types::{Item, KindTest};
use crate::types::{Occurrence, SequenceType};
use crate::xdm::*;
use crate::xpath_functions_31::{decimal_compare, double_compare, string_compare};
use itertools::Itertools;
use rust_decimal::Decimal;
use std::borrow::Borrow;
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Sequence(Vec<Expr>),
    ContextItem,
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    FunctionCall(QName, Vec<Expr>),
    Or(Vec<Expr>),
    And(Vec<Expr>),
    Arithmetic(Box<Expr>, ArithmeticOp, Box<Expr>),
    InstanceOf(Box<Expr>, SequenceType),
    TreatAs(Box<Expr>, SequenceType),
    Path(Box<Expr>, Box<Expr>),
    Step(Axis, NodeTest, Vec<Expr>),
    ValueComp(Box<Expr>, ValueComp, Box<Expr>),
    Predicate(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Axis {
    // forward
    Child,
    Descendant,
    Attribute,
    Self_,
    DescendantOrSelf,
    FollowingSibling,
    Following,
    Namespace,
    // reverse
    Parent,
    Ancestor,
    PrecedingSibling,
    Preceding,
    AncestorOrSelf,
}

#[derive(Debug, PartialEq, Clone)]
pub enum NodeTest {
    KindTest(KindTest),
    // including wildcards by storing "*" in QName parts
    NameTest(QName),
}

#[derive(Debug, PartialEq)]
pub enum ArithmeticOp {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Decimal(Decimal),
    Double(f64),
    String(String),
}

#[derive(Debug, PartialEq)]
pub enum ValueComp {
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,
}
impl Display for ValueComp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            ValueComp::EQ => "eq",
            ValueComp::NE => "ne",
            ValueComp::LT => "lt",
            ValueComp::LE => "le",
            ValueComp::GT => "gt",
            ValueComp::GE => "ge",
        };
        write!(f, "{}", s)
    }
}
impl Display for ArithmeticOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ArithmeticOp::Plus => f.write_str("+"),
            ArithmeticOp::Minus => f.write_str("-"),
        }
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Decimal(d) => write!(f, "{}", d),
            Literal::Double(d) => write!(f, "{}", d),
            Literal::String(s) => write!(f, "\"{}\"", s.replace("\"", "\"\"")),
        }
    }
}
impl Expr {
    pub(crate) fn compile(self, ctx: &StaticContext) -> XdmResult<CompiledExpr> {
        let type_ = self.type_(ctx)?;
        match self {
            Expr::Literal(l) => l.compile(),
            Expr::Sequence(s) => {
                let compiled_vec: XdmResult<Vec<_>> =
                    s.into_iter().map(|x| x.compile(ctx)).collect();
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
            Expr::IfThenElse(condition_expr, if_expr, else_expr) => {
                let condition = condition_expr.compile(ctx)?;
                let if_branch = if_expr.compile(ctx)?;
                let else_branch = else_expr.compile(ctx)?;
                Ok(CompiledExpr::new(move |c| {
                    if condition.execute(c)?.boolean()? {
                        if_branch.execute(c)
                    } else {
                        else_branch.execute(c)
                    }
                }))
            }
            Expr::ContextItem => Ok(CompiledExpr::new(move |c| {
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
            Expr::FunctionCall(qname, args) => {
                let code = (ctx.function(&qname, args.len()).unwrap().code)();
                let compiled_vec: XdmResult<Vec<_>> =
                    args.into_iter().map(|x| x.compile(ctx)).collect();
                compiled_vec.map(|compiled_vec| {
                    CompiledExpr::new(move |c| {
                        let v: XdmResult<Vec<Xdm>> =
                            compiled_vec.iter().map(|e| e.execute(c)).collect();
                        let v_ok = v?;
                        code.execute(c, v_ok)
                    })
                })
            }
            Expr::Or(_) => todo!("implement Or"),
            Expr::And(_) => todo!("implement And"),
            Expr::Arithmetic(l, o, r) => match type_ {
                SequenceType::EmptySequence => {
                    Ok(CompiledExpr::new(move |_c| Ok(Xdm::Sequence(vec![]))))
                }
                SequenceType::Item(i, _) => {
                    if o != ArithmeticOp::Plus {
                        todo!("operations beside plus")
                    }
                    let l_c = l.compile(ctx)?;
                    let r_c = r.compile(ctx)?;
                    let type_string = i.to_string();
                    Ok(match type_string.as_ref() {
                        "xs:integer" => CompiledExpr::new(move |c| {
                            Ok(Xdm::Integer(
                                l_c.execute(c)?.integer()? + r_c.execute(c)?.integer()?,
                            ))
                        }),
                        "xs:decimal" => CompiledExpr::new(move |c| {
                            Ok(Xdm::Decimal(
                                l_c.execute(c)?.decimal()? + r_c.execute(c)?.decimal()?,
                            ))
                        }),
                        "xs:anyAtomicType" => CompiledExpr::new(move |c| {
                            Ok(Xdm::Double(
                                l_c.execute(c)?.double()? + r_c.execute(c)?.double()?,
                            ))
                        }),
                        _ => todo!("compile more Arithmetic cases"),
                    })
                }
            },
            Expr::InstanceOf(e, st) => {
                let ce = e.compile(ctx)?;
                Ok(CompiledExpr::new(move |c| {
                    let x = ce.execute(c)?;
                    let st2 = x.dynamic_type(&c.static_context)?;
                    let lub = SequenceType::lub(&c.static_context, &st, &st2)?;
                    Ok(Xdm::Boolean(lub == st))
                }))
            }
            Expr::TreatAs(e, _st) => {
                let ce = e.compile(ctx)?;
                // FIXME there's probably more that should be done here.
                Ok(ce)
            }
            Expr::Path(s1, s2) => {
                let e1 = s1.compile(ctx)?;
                let e2 = s2.compile(ctx)?;
                Ok(CompiledExpr::new(move |c| {
                    let x1 = e1.execute(c)?;
                    match x1 {
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
                        Xdm::NodeSeq(NodeSeq::RoXml(ronode)) => {
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

                            if result.len() == 1 {
                                Ok(result.remove(0))
                            } else {
                                Ok(Xdm::Sequence(result))
                            }
                        }
                        _ => Err(XdmError::xqtm(
                            "internal",
                            format!("Not a node seq: {:?}", x1),
                        )),
                    }
                }))
            }
            Expr::Step(axis, nt, ps) => {
                let nt = Box::new(nt);
                let predicates: XdmResult<Vec<_>> =
                    ps.into_iter().map(|x| x.compile(ctx)).collect();
                let predicates = predicates?;
                Ok(CompiledExpr::new(move |c| {
                    let ci = c
                        .focus
                        .as_ref()
                        .ok_or(XdmError::xqtm("err:XPDY0002", "context item is undefined"))?;
                    let ro_node = match &ci.sequence {
                        Xdm::NodeSeq(NodeSeq::RoXml(n)) => Ok(n),
                        _ => Err(XdmError::xqtm("", "didn't get a roxml node")),
                    }?;
                    // FIXME this should be done at compile time
                    match (axis, &*nt) {
                        (Axis::Child, NodeTest::NameTest(ref qn)) => {
                            let mut children: Vec<_> = ro_node
                                .children()
                                .enumerate()
                                .filter_map(|(pos, child)| {
                                    if child.is_element() && child.has_tag_name(qn) {
                                        let mut node = Xdm::NodeSeq(NodeSeq::RoXml(child));
                                        let mut include = true;
                                        let context = c.clone_with_focus(node, pos);
                                        for predicate in &predicates {
                                            let pred = predicate.execute(&context).unwrap();
                                            match pred {
                                                Xdm::Decimal(_)
                                                | Xdm::Integer(_)
                                                | Xdm::Double(_) => {
                                                    if pred.integer().unwrap() as usize != pos {
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
                                    } else {
                                        None
                                    }
                                })
                                .collect();
                            if children.len() == 1 {
                                Ok(children.remove(0))
                            } else {
                                Ok(Xdm::Sequence(children))
                            }
                        }
                        (Axis::Attribute, NodeTest::NameTest(ref qn)) => {
                            let mut attrs: Vec<_> = ro_node
                                .attributes()
                                .iter()
                                .filter_map(|a| {
                                    if a.namespace().map(|s| s.to_string()) == qn.ns
                                        && a.name() == qn.name
                                    {
                                        Some(Xdm::NodeSeq(NodeSeq::RoXmlAttr(a)))
                                    } else {
                                        None
                                    }
                                })
                                .collect();
                            if attrs.len() == 1 {
                                Ok(attrs.remove(0))
                            } else {
                                Ok(Xdm::Sequence(attrs))
                            }
                        }
                        (Axis::Self_, nt) => match nt {
                            NodeTest::KindTest(kt) => match kt {
                                KindTest::AnyKind => {
                                    Ok(Xdm::NodeSeq(NodeSeq::RoXml(ro_node.clone())))
                                }
                                _ => unimplemented!(),
                            },
                            NodeTest::NameTest(_) => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    }
                }))
            }
            Expr::ValueComp(e1, vc, e2) => {
                let c1 = e1.compile(ctx)?;
                let c2 = e2.compile(ctx)?;
                Ok(CompiledExpr::new(move |c| {
                    let a1 = c1.execute(c)?.atomize()?;
                    let a2 = c2.execute(c)?.atomize()?;
                    match (a1, a2) {
                        (Xdm::Sequence(v), _) | (_, Xdm::Sequence(v)) if v.is_empty() => {
                            Ok(Xdm::Sequence(vec![]))
                        }
                        (Xdm::Sequence(_), _) | (_, Xdm::Sequence(_)) => Err(XdmError::xqtm(
                            "err:XPTY0004",
                            "value comparison argument is a sequence",
                        )),
                        (Xdm::String(s1), x2) => Ok(Xdm::Boolean(
                            vc.comparison_true(string_compare(s1.as_str(), x2.string()?.as_str())),
                        )),
                        (x1, Xdm::String(s2)) => Ok(Xdm::Boolean(
                            vc.comparison_true(string_compare(x1.string()?.as_str(), s2.as_str())),
                        )),
                        (Xdm::Double(d1), x2) => Ok(Xdm::Boolean(
                            vc.comparison_true(double_compare(&d1, x2.double()?.borrow())),
                        )),
                        (x1, Xdm::Double(d2)) => Ok(Xdm::Boolean(
                            vc.comparison_true(double_compare(x1.double()?.borrow(), &d2)),
                        )),
                        (Xdm::Decimal(d1), x2) => Ok(Xdm::Boolean(
                            vc.comparison_true(decimal_compare(&d1, x2.decimal()?.borrow())),
                        )),
                        (x1, Xdm::Decimal(d2)) => Ok(Xdm::Boolean(
                            vc.comparison_true(decimal_compare(x1.decimal()?.borrow(), &d2)),
                        )),
                        (a1, a2) => unimplemented!("{:?} {} {:?}", a1, vc, a2),
                    }
                }))
            }
            Expr::Predicate(e, p) => {
                let ce = e.compile(ctx)?;
                let cp = p.compile(ctx)?;
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
        }
    }
    pub(crate) fn type_(&self, ctx: &StaticContext) -> XdmResult<SequenceType> {
        match self {
            Expr::Literal(l) => Ok(l.type_(ctx)),
            Expr::Sequence(v) => {
                if v.is_empty() {
                    Ok(SequenceType::EmptySequence)
                } else {
                    assert!(v.len() > 1);
                    let child_types: XdmResult<Vec<_>> = v.iter().map(|e| e.type_(ctx)).collect();
                    SequenceType::add_vec(ctx, child_types?)
                }
            }
            Expr::ContextItem => Ok(SequenceType::EmptySequence),
            Expr::IfThenElse(_, t, e) => {
                let t_type = t.type_(ctx)?;
                let e_type = e.type_(ctx)?;
                SequenceType::lub(ctx, &t_type, &e_type)
            }
            Expr::FunctionCall(qname, args) => ctx
                .function(qname, args.len())
                .map(|func| func.type_.clone())
                .ok_or_else(|| {
                    let msg = format!("No function {}#{} found", qname, args.len());
                    XdmError::xqtm("err:XPST0017", msg.as_str())
                }),
            Expr::Or(_) => todo!("implement type_"),
            Expr::And(_) => todo!("implement type_"),
            Expr::Arithmetic(l, _op, r) => {
                let t1 = l.type_(ctx)?;
                let t2 = r.type_(ctx)?;
                SequenceType::lub(ctx, &t1, &t2)
            }
            Expr::InstanceOf(_, _) => Ok(SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:boolean"))?),
                Occurrence::One,
            )),
            Expr::TreatAs(_, st) => Ok(st.clone()),
            Expr::Path(e1, e2) => e2.type_(ctx),
            Expr::Step(a, _nt, _ps) => a.type_(ctx),
            Expr::ValueComp(_, _, _) => Ok(SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown("xs:boolean"))?),
                Occurrence::Optional,
            )),
            Expr::Predicate(e, _) => e.type_(ctx),
        }
    }
}

impl Axis {
    pub(crate) fn type_(&self, ctx: &StaticContext) -> XdmResult<SequenceType> {
        // match self {
        //     Axis::Child => {},
        //     Axis::Descendant => {},
        //     Axis::Attribute => {},
        //     Axis::Self_ => {},
        //     Axis::DescendantOrSelf => {},
        //     Axis::FollowingSibling => {},
        //     Axis::Following => {},
        //     Axis::Namespace => {},
        //     Axis::Parent => {},
        //     Axis::Ancestor => {},
        //     Axis::PrecedingSibling => {},
        //     Axis::Preceding => {},
        //     Axis::AncestorOrSelf => {},
        // }
        Ok(SequenceType::EmptySequence)
    }
}
impl Display for Axis {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Axis::Child => write!(f, "child"),
            // Axis::Descendant => {},
            // Axis::Attribute => {},
            // Axis::Self_ => {},
            // Axis::DescendantOrSelf => {},
            // Axis::FollowingSibling => {},
            // Axis::Following => {},
            // Axis::Namespace => {},
            // Axis::Parent => {},
            // Axis::Ancestor => {},
            // Axis::PrecedingSibling => {},
            // Axis::Preceding => {},
            // Axis::AncestorOrSelf => {},
            _ => todo!("Display for Axis"),
        }
    }
}
impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(l) => l.fmt(f),
            Expr::Sequence(v) => write!(f, "({})", v.iter().format(", ")),
            Expr::ContextItem => f.write_str("."),
            Expr::IfThenElse(i, t, e) => write!(f, "if ({}) then {} else {}", i, t, e),
            Expr::FunctionCall(name, args) => write!(f, "{}({})", name, args.iter().format(", ")),
            Expr::Or(v) => write!(f, "{}", v.iter().format(" or ")),
            Expr::And(v) => write!(f, "{}", v.iter().format(" and ")),
            Expr::Arithmetic(e1, o, e2) => write!(f, "{} {} {}", e1, o, e2),
            Expr::InstanceOf(e, t) => write!(f, "{} instance of {}", e, t),
            Expr::TreatAs(e, t) => write!(f, "{} treat as {}", e, t),
            Expr::Path(e1, e2) => write!(f, "{}/{}", e1, e2),
            Expr::Step(a, nt, ps) => {
                write!(f, "{}::{}", a, nt)?;
                for p in ps {
                    write!(f, "[{}]", p)?;
                }
                Ok(())
            }
            Expr::ValueComp(e1, vc, e2) => write!(f, "{} {} {}", e1, vc, e2),
            Expr::Predicate(e, p) => write!(f, "{}[{}]", e, p),
        }
    }
}

impl Display for NodeTest {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            NodeTest::KindTest(_) => todo!("Display for NodeTest"),
            NodeTest::NameTest(qname) => write!(f, "{}", qname),
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
impl ValueComp {
    fn comparison_true(&self, c: i8) -> bool {
        match (self, c) {
            (ValueComp::EQ, 0) => true,
            (ValueComp::EQ, _) => false,
            (ValueComp::NE, 0) => false,
            (ValueComp::NE, _) => true,
            (ValueComp::LT, -1) => true,
            (ValueComp::LT, _) => false,
            (ValueComp::LE, 1) => false,
            (ValueComp::LE, _) => true,
            (ValueComp::GT, 1) => true,
            (ValueComp::GT, _) => false,
            (ValueComp::GE, -1) => false,
            (ValueComp::GE, _) => true,
        }
    }
}
