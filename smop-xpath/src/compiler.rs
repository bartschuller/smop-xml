use crate::ast::{ArithmeticOp, Axis, Expr, Quantifier};
use crate::context::StaticContext;
use crate::runtime::CompiledExpr;
use crate::smop_xmltree::AxisIter;
use crate::types::SequenceType;
use crate::xdm::{Xdm, XdmError, XdmResult};
use crate::xpath_functions_31::{double_compare, string_compare};
use num_traits::ToPrimitive;
use smop_xmltree::nod::{Node, NodeKind, QName};
use std::borrow::Borrow;
use std::collections::BTreeSet;
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::rc::Rc;

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
                            Xdm::sequence(
                                vecx.into_iter()
                                    .flat_map(|x| match x {
                                        Xdm::EmptySequence => vec![],
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
                        Xdm::EmptySequence => {
                            Err(XdmError::xqtm("XPDY0002", "context item is undefined"))
                        }
                        x => Ok((*x).clone()),
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
                    Ok(CompiledExpr::new(move |_c| Ok(Xdm::EmptySequence)))
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
                    Ok(CompiledExpr::new(move |_c| Ok(Xdm::EmptySequence)))
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
                        Xdm::Node(ref _ronode) => {
                            let context = c.clone_with_focus(x1, 0);
                            e2.execute(&context)
                        }
                        Xdm::EmptySequence => Ok(Xdm::EmptySequence),
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
                            Xdm::Node(node) => {
                                nodes_btree.insert(node);
                            }
                            y => values_vec.push(y),
                        }
                    }
                    if nodes_btree.is_empty() {
                        Ok(Xdm::flatten(values_vec))
                    } else if values_vec.is_empty() {
                        Ok(Xdm::flatten(
                            nodes_btree.into_iter().map(|n| Xdm::Node(n)).collect(),
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
                        Xdm::Node(n) => Ok(n),
                        _ => Err(XdmError::xqtm("XPTY0020", "context item is not a node")),
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
                        Axis::PrecedingSibling => {
                            if ro_node.node_kind() == NodeKind::Attribute {
                                Box::new(std::iter::empty())
                            } else {
                                Box::new(
                                    AxisIter {
                                        node: ro_node.previous_sibling(),
                                        next: |n| n.previous_sibling(),
                                    }
                                    .filter(|sibling| {
                                        nt.matches(
                                            sibling.node_kind(),
                                            NodeKind::Element,
                                            sibling.node_name(),
                                        )
                                    })
                                    .enumerate(),
                                )
                            }
                        }
                        _ => unimplemented!("axis {}", axis),
                    };
                    // pos needs to change if an earlier predicate filters out some nodes
                    // //foo[@bar=1][5] needs to count not all foos but the filtered ones.
                    // poss maintains the pos values for every predicate.
                    let mut poss = vec![0; predicates.len() + 1];
                    let result_nodes: Vec<_> = node_iterator
                        .filter_map(|(pos, result_node)| {
                            let node = Xdm::Node(result_node);
                            let mut include = true;
                            let mut context = c.clone_with_focus(node, pos);

                            let mut pred_num: usize = 0;
                            for predicate in &predicates {
                                let pred = predicate.execute(&context).unwrap();
                                match pred {
                                    Xdm::Decimal(_) | Xdm::Integer(_) | Xdm::Double(_) => {
                                        if pred.integer().unwrap() as usize - 1 != poss[pred_num] {
                                            include = false;
                                        }
                                    }
                                    _ => {
                                        if !pred.boolean().unwrap() {
                                            include = false;
                                        }
                                    }
                                }
                                poss[pred_num] += 1;
                                if include {
                                    pred_num += 1;
                                    context.focus.as_mut().unwrap().position = poss[pred_num];
                                } else {
                                    break;
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
                        (Xdm::EmptySequence, _) | (_, Xdm::EmptySequence) => Ok(Xdm::EmptySequence),
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
                                    if !a1.is_nan()
                                        && !a2.is_nan()
                                        && vc.comparison_true(double_compare(
                                            a1.double()?.borrow(),
                                            a2.double()?.borrow(),
                                        ))
                                    {
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
            Expr::Filter(e, p, _) => {
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
                    Ok(Xdm::sequence(result))
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
                    Ok(Xdm::sequence(result))
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
            Expr::Range(from, to, _) => {
                let from_c = from.compile()?;
                let to_c = to.compile()?;
                Ok(CompiledExpr::new(move |c| {
                    let from = from_c.execute(c)?;
                    let to = to_c.execute(c)?;
                    if from.count() == 0 || to.count() == 0 {
                        return Ok(Xdm::EmptySequence);
                    }
                    let from_i = from.integer()?;
                    let to_i = to.integer()?;
                    if from_i > to_i {
                        return Ok(Xdm::EmptySequence);
                    }
                    Ok(Xdm::sequence(
                        (from_i..=to_i).map(|i| Xdm::Integer(i)).collect(),
                    ))
                }))
            }
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
            Expr::SimpleMap(s1, s2, _) => {
                let e1 = s1.compile()?;
                let e2 = s2.compile()?;
                Ok(CompiledExpr::new(move |c| {
                    let x1 = e1.execute(c)?;
                    match x1 {
                        Xdm::EmptySequence => Ok(Xdm::EmptySequence),
                        Xdm::Sequence(v) => {
                            let mut result: Vec<Xdm> = Vec::new();
                            for x in v.into_iter().enumerate() {
                                let context = c.clone_with_focus(x.1, x.0);
                                let res = e2.execute(&context)?;
                                result.push(res);
                            }

                            Ok(Xdm::flatten(result))
                        }
                        _ => {
                            let context = c.clone_with_focus(x1, 0);
                            e2.execute(&context)
                        }
                    }
                }))
            }
        }
    }
}
