use crate::ast::Expr;
use pest::error::Error;
use pest::iterators::Pair;
use pest::Parser;

#[derive(Parser)]
#[grammar = "parser3.pest"]
pub struct Parser3;

pub fn p3_parse(input: &str) -> Result<Expr, Error<Rule>> {
    let expr = Parser3::parse(Rule::Xpath, input)?.next().unwrap();

    fn parse_expr(pair: Pair<Rule>) -> Expr {
        println!("{:?}", pair);
        match pair.as_rule() {
            Rule::Expr => Expr::Sequence(vec![]),
            _ => unreachable!(),
        }
    }

    Ok(parse_expr(expr))
}
