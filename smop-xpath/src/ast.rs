use crate::runtime::CompiledExpr;
use crate::xdm::*;
use rust_decimal::Decimal;

// trait XPathExpr<'a> {
//     fn compile(self) -> CompiledExpr<'a>;
// }

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Sequence(Vec<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Decimal(Decimal),
    Double(f64),
    String(String),
}

impl<'a> Expr {
    pub(crate) fn compile(self) -> CompiledExpr<'a> {
        match self {
            Expr::Literal(l) => l.compile(),
            Expr::Sequence(s) => {
                let mut compiled_vec: Vec<_> = s.into_iter().map(|x| x.compile()).collect();
                if compiled_vec.len() == 1 {
                    compiled_vec.remove(0)
                } else {
                    CompiledExpr::new(move |c| {
                        Xdm::Sequence(compiled_vec.iter().map(|e| e.execute(c)).collect())
                    })
                }
            }
        }
    }
}

impl<'a> Literal {
    fn compile(self) -> CompiledExpr<'a> {
        match self {
            Literal::Integer(i) => CompiledExpr::new(move |_c| Xdm::Integer(i)),
            Literal::Decimal(d) => CompiledExpr::new(move |_c| Xdm::Decimal(d)),
            Literal::Double(d) => CompiledExpr::new(move |_c| Xdm::Double(d)),
            Literal::String(s) => CompiledExpr::new(move |_c| Xdm::String(s.clone())),
        }
    }
}
