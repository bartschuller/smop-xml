use crate::runtime::CompiledExpr;
use crate::types::SequenceType;
use crate::xdm::*;
use rust_decimal::Decimal;

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

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Decimal(Decimal),
    Double(f64),
    String(String),
}

impl Expr {
    pub(crate) fn compile(self) -> CompiledExpr {
        match self {
            Expr::Literal(l) => l.compile(),
            Expr::Sequence(s) => {
                let compiled_vec: Vec<_> = s.into_iter().map(|x| x.compile()).collect();
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
                let condition = condition_expr.compile();
                let if_branch = if_expr.compile();
                let else_branch = else_expr.compile();
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
            Expr::Arithmetic(_, _, _) => todo!("implement Arithmetic"),
            Expr::InstanceOf(_, _) => todo!("implement instance of"),
        }
    }
    pub(crate) fn typer() -> XdmResult<SequenceType> {
        todo!("implement typer")
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
}
