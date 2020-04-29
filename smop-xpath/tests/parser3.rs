use pest::error::Error;
use xpath::ast::Expr;
use xpath::parser3::p3_parse;
use xpath::parser3::Rule;

#[test]
fn integer_literal() {
    let parse = p3_parse("0.5, 1.4e-3,(: yess! (: 7 :):)23, 'you''ll ''never''', \"''weird''\"");
    println!("{:?}", parse);
    //assert_eq!("54", parse)
}

#[test]
fn ifthen1() {
    let parse = p3_parse("if (1) then 3 else 0.5");
    match parse {
        Ok(e) => {
            println!("{:?}", e);
        }
        Err(e) => {
            println!("{}", e);
        }
    }
}
