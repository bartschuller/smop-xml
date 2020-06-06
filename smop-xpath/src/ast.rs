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
    Arithmetic(Box<Expr<T>>, ArithmeticOp, Box<Expr<T>>, T),
    InstanceOf(Box<Expr<T>>, SequenceType, T),
    TreatAs(Box<Expr<T>>, SequenceType, T),
    Path(Box<Expr<T>>, Box<Expr<T>>, T),
    Step(Axis, NodeTest, Vec<Expr<T>>, T),
    ValueComp(Box<Expr<T>>, ValueComp, Box<Expr<T>>, T),
    Predicate(Box<Expr<T>>, Box<Expr<T>>, T),
    For(QName, Box<Expr<T>>, Box<Expr<T>>, T),
    Let(QName, Box<Expr<T>>, Box<Expr<T>>, T),
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
            Expr::Arithmetic(_, _, _, t) => t,
            Expr::InstanceOf(_, _, t) => t,
            Expr::TreatAs(_, _, t) => t,
            Expr::Path(_, _, t) => t,
            Expr::Step(_, _, _, t) => t,
            Expr::ValueComp(_, _, _, t) => t,
            Expr::Predicate(_, _, t) => t,
            Expr::For(_, _, _, t) => t,
            Expr::Let(_, _, _, t) => t,
        }
    }
}
impl Expr<(SequenceType, Rc<StaticContext>)> {
    pub(crate) fn compile(self) -> XdmResult<CompiledExpr> {
        let expr_string = format!("{}", &self);
        match self {
            Expr::Literal(l, _) => l.compile(),
            Expr::VarRef(qname, _) => Ok(CompiledExpr::new(move |c| {
                println!("running {}", expr_string);
                c.varref(&qname).ok_or_else(|| {
                    XdmError::xqtm("err:XPST0008", format!("variable `{}` not found", qname))
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
            Expr::FunctionCall(qname, args, t) => {
                let code = (t.1.function(&qname, args.len()).unwrap().code)();
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
            Expr::Or(_, _) => todo!("implement Or"),
            Expr::And(_, _) => todo!("implement And"),
            Expr::Arithmetic(l, o, r, t) => match t.0 {
                SequenceType::EmptySequence => {
                    println!("warning: compiling away arithmetic op to empty sequence");
                    Ok(CompiledExpr::new(move |_c| Ok(Xdm::Sequence(vec![]))))
                }
                SequenceType::Item(i, _) => {
                    if o != ArithmeticOp::Plus {
                        todo!("operations beside plus")
                    }
                    let l_c = l.compile()?;
                    let r_c = r.compile()?;
                    let type_string = i.to_string();
                    Ok(match type_string.as_ref() {
                        "xs:integer" => CompiledExpr::new(move |c| {
                            println!("running integer + : {}", expr_string);
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
                        Xdm::NodeSeq(NodeSeq::RoXml(_ronode)) => {
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
            Expr::Step(axis, nt, ps, _) => {
                let nt = Box::new(nt);
                let predicates: XdmResult<Vec<_>> = ps.into_iter().map(|x| x.compile()).collect();
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
                                        let node = Xdm::NodeSeq(NodeSeq::RoXml(child));
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
                        "err:XPST0008",
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
                        Expr::FunctionCall(qname.clone(), args_typed, (func.type_.clone(), ctx2))
                    })
                    .ok_or_else(|| {
                        let msg = format!("No function {}#{} found", qname, arity);
                        XdmError::xqtm("err:XPST0017", msg.as_str())
                    })
            }
            Expr::Or(v, _) => {
                let v_typed: XdmResult<Vec<_>> =
                    v.into_iter().map(|a| a.type_(Rc::clone(&ctx))).collect();
                Ok(Expr::Or(v_typed?, todo!("implement type_")))
            }
            Expr::And(v, _) => {
                let v_typed: XdmResult<Vec<_>> =
                    v.into_iter().map(|a| a.type_(Rc::clone(&ctx))).collect();
                Ok(Expr::And(v_typed?, todo!("implement type_")))
            }
            Expr::Arithmetic(l, op, r, _) => {
                let t1 = l.type_(Rc::clone(&ctx))?;
                let t2 = r.type_(Rc::clone(&ctx))?;
                let t1_type = t1.t().0.clone();
                let t2_type = t2.t().0.clone();
                Ok(Expr::Arithmetic(
                    Box::new(t1),
                    op,
                    Box::new(t2),
                    (SequenceType::lub(&ctx, &t1_type, &t2_type)?, ctx),
                ))
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
                    SequenceType::EmptySequence => unreachable!(),
                    SequenceType::Item(it, _o) => SequenceType::Item(it, Occurrence::One),
                };
                let mut new_ctx = (&*ctx).clone();
                new_ctx.set_variable_type(qname.clone(), bi_type);
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
                new_ctx.set_variable_type(qname.clone(), bi_type);
                let e_typed = e.type_(Rc::new(new_ctx))?;
                let e_type = e_typed.t().0.clone();
                Ok(Expr::Let(
                    qname,
                    Box::new(bs_typed),
                    Box::new(e_typed),
                    (e_type, ctx),
                ))
            }
        }
    }
}
impl Axis {
    pub(crate) fn type_(&self, _ctx: &StaticContext) -> XdmResult<SequenceType> {
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
            Axis::Descendant => write!(f, "descendant"),
            Axis::Attribute => write!(f, "attribute"),
            Axis::Self_ => write!(f, "self"),
            Axis::DescendantOrSelf => write!(f, "descendant-or-self"),
            Axis::FollowingSibling => write!(f, "following-sibling"),
            Axis::Following => write!(f, "following"),
            Axis::Namespace => write!(f, "namespace"),
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
            Expr::Arithmetic(e1, o, e2, _) => write!(f, "{} {} {}", e1, o, e2),
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
            Expr::Predicate(e, p, _) => write!(f, "{}[{}]", e, p),
            Expr::For(qname, bs, ret, _) => write!(f, "for ${} in {} return {}", qname, bs, ret),
            Expr::Let(qname, bs, ret, _) => write!(f, "let ${} := {} return {}", qname, bs, ret),
        }
    }
}

impl Display for NodeTest {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            NodeTest::KindTest(kt) => write!(f, "{}", kt),
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
