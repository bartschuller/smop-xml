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
                    Xdm::Sequence(compiled_vec.iter().map(|e| e.execute(c)).collect())
                })
            }
            Expr::IfThenElse(condition_expr, if_expr, else_expr) => {
                CompiledExpr::new(move |_c| Xdm::Integer(53))
            }
        }
    }
}

impl<'input> Literal<'input> {
    fn compile(self) -> CompiledExpr<'input> {
        match self {
            Literal::Integer(i) => CompiledExpr::new(move |_c| Xdm::Integer(i)),
            Literal::Decimal(d) => CompiledExpr::new(move |_c| Xdm::Decimal(d)),
            Literal::Double(d) => CompiledExpr::new(move |_c| Xdm::Double(d)),
            Literal::String(s) => {
                let s = s.into_owned();
                CompiledExpr::new(move |_c| Xdm::String(s.clone()))
            }
        }
    }
}
