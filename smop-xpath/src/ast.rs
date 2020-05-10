use crate::context::StaticContext;
use crate::runtime::CompiledExpr;
use crate::types::Item;
use crate::types::{Occurrence, SequenceType};
use crate::xdm::*;
use itertools::Itertools;
use rust_decimal::Decimal;
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
}

#[derive(Debug, PartialEq)]
pub enum ArithmeticOp {
    Plus,
    Minus,
}
impl Display for ArithmeticOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ArithmeticOp::Plus => f.write_str("+"),
            ArithmeticOp::Minus => f.write_str("-"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Decimal(Decimal),
    Double(f64),
    String(String),
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
                if c.focus.is_none() {
                    Err(XdmError::xqtm("XPDY0002", "context item is undefined"))
                } else {
                    todo!("implement context/focus")
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
            Expr::InstanceOf(_, _) => todo!("implement instance of"),
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
            Expr::ContextItem => todo!("implement type_"),
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
            Expr::InstanceOf(_, _) => todo!("implement type_"),
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
