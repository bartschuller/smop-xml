use crate::ast::{Expr, Literal};
use pest_consume::match_nodes;
use pest_consume::Error;
use pest_consume::Parser;
use rust_decimal::Decimal;
use std::str::FromStr;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(Parser)]
#[grammar = "parser3.pest"]
pub struct Parser3;

#[pest_consume::parser]
impl Parser3 {
    fn Xpath(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Expr(expr), EOI(_)] => expr,
        ))
    }
    fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }
    fn Expr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [ExprSingle(expr)..] => Expr::Sequence(expr.collect()),
        ))
    }
    fn ExprSingle(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Literal(lit)] => Expr::Literal(lit),
        ))
    }
    fn Literal(input: Node) -> Result<Literal> {
        Ok(match_nodes!(input.into_children();
            [DoubleLiteral(lit)] => lit,
            [DecimalLiteral(lit)] => lit,
            [IntegerLiteral(lit)] => lit,
            [StringLiteral(lit)] => lit,
        ))
    }
    fn IntegerLiteral(input: Node) -> Result<Literal> {
        Ok(Literal::Integer(input.as_str().parse().unwrap()))
    }
    fn DoubleLiteral(input: Node) -> Result<Literal> {
        Ok(Literal::Double(input.as_str().parse().unwrap()))
    }
    fn DecimalLiteral(input: Node) -> Result<Literal> {
        Ok(Literal::Decimal(Decimal::from_str(input.as_str()).unwrap()))
    }
    fn StringLiteral(input: Node) -> Result<Literal> {
        // we matched the complete string literal, with outer quotes and possibly escaped quotes.
        // Get the inner string and unescape where needed.
        let str = input.as_str();
        let last = str.len() - 1;
        // we want to know which of the 2 possible quote characters were used.
        let quote = &str[0..1];
        let qchar = quote.chars().nth(0).unwrap();
        let mut two_quotes = String::new();
        two_quotes.push(qchar);
        two_quotes.push(qchar);
        let inner = &str[1..last];
        let unescaped = if inner.contains(quote) {
            inner.replace(two_quotes.as_str(), quote)
        } else {
            inner.to_string()
        };
        Ok(Literal::String(unescaped))
    }
}

pub fn p3_parse(input: &str) -> Result<Expr> {
    let root = Parser3::parse(Rule::Xpath, input)?;
    let root = root.single()?;
    Parser3::Xpath(root)
}
