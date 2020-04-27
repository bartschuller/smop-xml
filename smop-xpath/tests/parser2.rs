use xpath::ast::*;
use xpath::parser2::*;

#[test]
fn int_literal3() {
    let input = "1234";
    let output = PrimaryExprParser::new().parse(input).unwrap();
    assert_eq!(Expr::Literal(Literal::Integer(1234)), output)
}
