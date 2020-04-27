use xpath::parser3::p3_parse;

#[test]
fn integer_literal() {
    let parse = p3_parse("0.5, 1");
    println!("{:?}", parse);
    //assert_eq!("54", parse)
}
