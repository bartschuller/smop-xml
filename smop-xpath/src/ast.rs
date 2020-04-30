use crate::runtime::CompiledExpr;
use crate::xdm::*;
use rust_decimal::Decimal;
use std::borrow::Cow;

// trait XPathExpr<'a> {
//     fn compile(self) -> CompiledExpr<'a>;
// }

#[derive(Debug, PartialEq)]
pub enum Expr<'input> {
    Literal(Literal<'input>),
    Sequence(Vec<Expr<'input>>),
    IfThenElse(Box<Expr<'input>>, Box<Expr<'input>>, Box<Expr<'input>>),
}

#[derive(Debug, PartialEq)]
pub enum Literal<'input> {
    Integer(i64),
    Decimal(Decimal),
    Double(f64),
    String(Cow<'input, str>),
}

impl<'input> Expr<'input> {
    pub(crate) fn compile(self) -> CompiledExpr<'input> {
        match self {
            Expr::Literal(l) => l.compile(),
            Expr::Sequence(s) => {
                let mut compiled_vec: Vec<_> = s.into_iter().map(|x| x.compile()).collect();
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
        }
    }
}

impl<'input> Literal<'input> {
    fn compile(self) -> CompiledExpr<'input> {
        match self {
            Literal::Integer(i) => CompiledExpr::new(move |_c| Ok(Xdm::Integer(i))),
            Literal::Decimal(d) => CompiledExpr::new(move |_c| Ok(Xdm::Decimal(d))),
            Literal::Double(d) => CompiledExpr::new(move |_c| Ok(Xdm::Double(d))),
            Literal::String(s) => {
                let s = s.into_owned();
                CompiledExpr::new(move |_c| Ok(Xdm::String(s.clone())))
            }
        }
    }
}
