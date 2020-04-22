use crate::runtime::CompiledExpr;
use rust_decimal::Decimal;

trait Expr<'a> {
    fn compile(self) -> CompiledExpr<'a>;
}

pub enum PrimaryExpr<'a> {
    Literal(Literal<'a>),
}

pub enum Literal<'a> {
    NumericLiteral(NumericLiteral),
    StringLiteral(&'a str),
}

#[derive(Debug, PartialEq)]
pub enum NumericLiteral {
    IntegerLiteral(i64),
    DecimalLiteral(Decimal),
    DoubleLiteral(f64),
}
