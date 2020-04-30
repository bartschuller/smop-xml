use crate::ast::{Expr, Literal};
use pest_consume::match_nodes;
use pest_consume::Error;
use pest_consume::Parser;
use rust_decimal::Decimal;
use std::borrow::Cow;
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
            [ExprSingle(expr)..] => {
                let mut v: Vec<Expr> = expr.collect();
                if v.len() == 1 {
                    v.remove(0)
                } else {
                    Expr::Sequence(v)
                }
            }
        ))
    }
    fn ExprSingle(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Literal(lit)] => Expr::Literal(lit),
            [IfExpr(e)] => e,
            [ParenthesizedExpr(e)] => e,
        ))
    }
    fn ParenthesizedExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Expr(e)] => e,
            [] => Expr::Sequence(vec![])
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
            Cow::Owned(inner.replace(two_quotes.as_str(), quote))
        } else {
            Cow::Borrowed(inner)
        };
        Ok(Literal::String(unescaped))
    }
    fn IfExpr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [Expr(i), ExprSingle(t), ExprSingle(e)] => Expr::IfThenElse(Box::new(i), Box::new(t), Box::new(e)),
        ))
    }
}

pub fn p3_parse(input: &str) -> Result<Expr> {
    let root = Parser3::parse(Rule::Xpath, input);
    pest_ascii_tree::print_ascii_tree(root.clone().map(|n| n.as_pairs().to_owned()));
    let root = root?.single()?;
    let parse = Parser3::Xpath(root);
    parse.map_err(|e| {
        e.with_path("literal string").renamed_rules(|rule| {
            match *rule {
                Rule::EOI => "end of input",
                Rule::Literal => "a literal",
                _ => {
                    println!("unhandled grammar prettifier: {:?}", rule);
                    "UNHANDLED"
                }
            }
            .to_owned()
        })
    })
}
