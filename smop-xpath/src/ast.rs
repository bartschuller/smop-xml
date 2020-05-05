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
    pub(crate) fn compile(self, ctx: &StaticContext) -> CompiledExpr {
        let type_ = self.type_(ctx).unwrap();
        match self {
            Expr::Literal(l) => l.compile(),
            Expr::Sequence(s) => {
                let compiled_vec: Vec<_> = s.into_iter().map(|x| x.compile(ctx)).collect();
                CompiledExpr::new(move |c| {
                    let v: XdmResult<Vec<_>> = compiled_vec.iter().map(|e| e.execute(c)).collect();
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
            }
            Expr::IfThenElse(condition_expr, if_expr, else_expr) => {
                let condition = condition_expr.compile(ctx);
                let if_branch = if_expr.compile(ctx);
                let else_branch = else_expr.compile(ctx);
                CompiledExpr::new(move |c| {
                    if condition.execute(c)?.boolean()? {
                        if_branch.execute(c)
                    } else {
                        else_branch.execute(c)
                    }
                })
            }
            Expr::ContextItem => CompiledExpr::new(move |c| {
                if c.focus.is_none() {
                    Err(XdmError::xqtm("XPDY0002", "context item is undefined"))
                } else {
                    todo!("implement context/focus")
                }
            }),
            Expr::FunctionCall(_, _) => todo!("implement FunctionCall"),
            Expr::Or(_) => todo!("implement Or"),
            Expr::And(_) => todo!("implement And"),
            Expr::Arithmetic(l, o, r) => match type_ {
                SequenceType::EmptySequence => {
                    CompiledExpr::new(move |_c| Ok(Xdm::Sequence(vec![])))
                }
                SequenceType::Item(i, _) => {
                    if o != ArithmeticOp::Plus {
                        todo!("operations beside plus")
                    }
                    let l_c = l.compile(ctx);
                    let r_c = r.compile(ctx);
                    let type_string = i.to_string();
                    match type_string.as_ref() {
                        "xs:integer" => CompiledExpr::new(move |c| {
                            Ok(Xdm::Integer(
                                l_c.execute(c)?.integer()? + r_c.execute(c)?.integer()?,
                            ))
                        }),
                        _ => todo!("compile more Arithmetic cases"),
                    }
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
                    SequenceType::lub_vec(ctx, child_types?)
                }
            }
            Expr::ContextItem => todo!("implement type_"),
            Expr::IfThenElse(_, _, _) => todo!("implement type_"),
            Expr::FunctionCall(_, _) => todo!("implement type_"),
            Expr::Or(_) => todo!("implement type_"),
            Expr::And(_) => todo!("implement type_"),
            Expr::Arithmetic(l, op, r) => {
                let t1 = l.type_(ctx)?;
                let t2 = r.type_(ctx)?;
                match (t1, t2) {
                    (SequenceType::EmptySequence, _) | (_, SequenceType::EmptySequence) => {
                        Ok(SequenceType::EmptySequence)
                    }
                    (
                        SequenceType::Item(Item::AtomicOrUnion(st1), _),
                        SequenceType::Item(Item::AtomicOrUnion(st2), _),
                    ) => {
                        let q1 = st1.qname.as_ref().unwrap().to_string();
                        let q2 = st2.qname.as_ref().unwrap().to_string();
                        let q = match (q1.as_str(), q2.as_str()) {
                            ("xs:integer", "xs:integer") => ctx.qname("xs", "integer"),
                            ("xs:decimal", "xs:decimal") => ctx.qname("xs", "decimal"),
                            ("xs:double", "xs:double") => ctx.qname("xs", "double"),
                            _ => todo!("Arithmetic type for other combinations of qnames"),
                        };
                        let i = Item::AtomicOrUnion(ctx.schema_type(&q.unwrap())?);
                        Ok(SequenceType::Item(i, Occurrence::One))
                    }
                    _ => todo!("implement more complex types for Arithmetic"),
                }
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
    fn compile(self) -> CompiledExpr {
        match self {
            Literal::Integer(i) => CompiledExpr::new(move |_c| Ok(Xdm::Integer(i))),
            Literal::Decimal(d) => CompiledExpr::new(move |_c| Ok(Xdm::Decimal(d))),
            Literal::Double(d) => CompiledExpr::new(move |_c| Ok(Xdm::Double(d))),
            Literal::String(s) => CompiledExpr::new(move |_c| Ok(Xdm::String(s.clone()))),
        }
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
