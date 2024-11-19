use crate::ast::{ArithmeticOp, Expr};
use crate::context::StaticContext;
use crate::types::{Item, KindTest, Occurrence, SequenceType};
use crate::xdm::{XdmError, XdmResult};
use std::rc::Rc;
use xot::xmlname::{NameStrInfo, OwnedName};

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
                        format!("variable ${} not found in static context", qname.full_name()),
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
                        let msg = format!("No function {}#{} found", qname.full_name(), arity);
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
                            Item::AtomicOrUnion(ctx.schema_type(&ctx.wellknown("xs:boolean"))?),
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
                            Item::AtomicOrUnion(ctx.schema_type(&ctx.wellknown("xs:boolean"))?),
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
                            Item::AtomicOrUnion(ctx.schema_type(&ctx.wellknown("xs:string"))?),
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
                        Item::AtomicOrUnion(ctx.schema_type(&ctx.wellknown("xs:integer"))?),
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
                            Item::AtomicOrUnion(ctx.schema_type(&ctx.wellknown("xs:boolean"))?),
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
                    Item::AtomicOrUnion(ctx.schema_type(&ctx.wellknown("xs:boolean"))?),
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
                    Item::AtomicOrUnion(ctx.schema_type(&ctx.wellknown("xs:boolean"))?),
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
                    Item::AtomicOrUnion(ctx.schema_type(&ctx.wellknown("xs:boolean"))?),
                    Occurrence::Optional,
                );
                Ok(Expr::NodeComp(
                    Box::new(e1_typed),
                    gc,
                    Box::new(e2_typed),
                    (ret_type, ctx),
                ))
            }
            Expr::Filter(e, p, _) => {
                let e_typed = e.type_(Rc::clone(&ctx))?;
                let p_typed = p.type_(Rc::clone(&ctx))?;
                let e_type = e_typed.t().0.clone();
                Ok(Expr::Filter(
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
                let mut new_ctx = (*ctx).clone();
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
                let mut new_ctx = (*ctx).clone();
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
                type TypedBinding = (OwnedName, Box<Expr<(SequenceType, Rc<StaticContext>)>>);
                let mut bs_typed: Vec<TypedBinding> = Vec::with_capacity(bindings.len());
                for x in bindings {
                    let (var, expr) = x;
                    let binding_type = expr.type_(Rc::clone(&curr_ctx))?;
                    let bi_type = match binding_type.t().0.clone() {
                        SequenceType::EmptySequence => SequenceType::EmptySequence,
                        SequenceType::Item(it, _o) => SequenceType::Item(it, Occurrence::One),
                    };
                    bs_typed.push((var.clone(), Box::new(binding_type)));
                    new_ctx = (*curr_ctx).clone();
                    new_ctx.set_variable_type(&var, bi_type);
                    curr_ctx = Rc::new(new_ctx);
                }
                let result_type = SequenceType::Item(
                    Item::AtomicOrUnion(ctx.schema_type(&ctx.wellknown("xs:boolean"))?),
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
            Expr::Range(from, to, _) => {
                let from_typed = from.type_(Rc::clone(&ctx))?;
                let to_typed = to.type_(Rc::clone(&ctx))?;
                // FIXME check that these are "xs:integer?"
                let result_type = SequenceType::Item(
                    Item::AtomicOrUnion(ctx.schema_type(&ctx.wellknown("xs:integer"))?),
                    Occurrence::ZeroOrMore,
                );
                Ok(Expr::Range(
                    Box::new(from_typed),
                    Box::new(to_typed),
                    (result_type, ctx),
                ))
            }
            Expr::ArraySquare(v, _) => {
                let v_typed: XdmResult<Vec<_>> =
                    v.into_iter().map(|e| e.type_(Rc::clone(&ctx))).collect();
                let v_typed = v_typed?;
                let occ = Occurrence::from(v_typed.len());
                Ok(Expr::ArraySquare(
                    v_typed,
                    (SequenceType::Item(Item::ArrayTest(None), occ), ctx),
                ))
            }
            Expr::ArrayCurly(_, _) => unimplemented!(),
            Expr::Map(v, _) => {
                let v_typed: XdmResult<Vec<_>> = v
                    .into_iter()
                    .map(|(e1, e2)| Ok((e1.type_(Rc::clone(&ctx))?, e2.type_(Rc::clone(&ctx))?)))
                    .collect();
                let v_typed = v_typed?;
                Ok(Expr::Map(
                    v_typed,
                    (
                        SequenceType::Item(Item::MapTest(None), Occurrence::One),
                        ctx,
                    ),
                ))
            }
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
            Expr::SimpleMap(e1, e2, _) => {
                let e1_typed = e1.type_(Rc::clone(&ctx))?;
                let e2_typed = e2.type_(Rc::clone(&ctx))?;
                let e2_type = e2_typed.t().0.clone();
                Ok(Expr::SimpleMap(
                    Box::new(e1_typed),
                    Box::new(e2_typed),
                    (e2_type, ctx),
                ))
            }
            Expr::Cast {
                expression,
                simple_type,
                optional,
                only_check,
                t: _,
            } => {
                let e_typed = expression.type_(Rc::clone(&ctx))?;
                let result_type = if only_check {
                    SequenceType::Item(
                        Item::AtomicOrUnion(ctx.schema_type(&ctx.wellknown("xs:boolean"))?),
                        Occurrence::One,
                    )
                } else {
                    SequenceType::Item(
                        Item::AtomicOrUnion(Rc::clone(&simple_type)),
                        if optional {
                            Occurrence::Optional
                        } else {
                            Occurrence::One
                        },
                    )
                };
                Ok(Expr::Cast {
                    expression: Box::new(e_typed),
                    simple_type: Rc::clone(&simple_type),
                    optional,
                    only_check,
                    t: (result_type, ctx),
                })
            }
            Expr::PredicatePattern(_, _) | Expr::EquivalentExpressionPattern(_, _) => todo!()
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
