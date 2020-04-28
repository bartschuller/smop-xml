use pest::error::Error;
use xpath::ast::Expr;
use xpath::parser3::p3_parse;
use xpath::parser3::Rule;

#[test]
fn integer_literal() {
    let parse = p3_parse("0.5, 1.4e-3, 23, 'you''ll ''never''', \"''weird''\"");
    match parse {
        Ok(expr) => println!("{:?}", expr),
        Err(e) => {
            let e = e.with_path("literal string").renamed_rules(|rule| {
                match *rule {
                    Rule::EOI => "end of input",
                    Rule::Literal => "a literal",
                    _ => {
                        println!("unhandled grammar prettifier: {:?}", rule);
                        "UNHANDLED"
                    }
                }
                .to_owned()
            });
            println!("{}", e);
        }
    }
    //assert_eq!("54", parse)
}
