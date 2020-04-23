use crate::runtime::CompiledExpr;
use rust_decimal::Decimal;

trait Expr<'a> {
    fn compile(self) -> CompiledExpr<'a>;
}

pub enum PrimaryExpr {
    Literal(Literal),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Decimal(Decimal),
    Double(f64),
    String(String),
}
