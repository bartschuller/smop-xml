use pest::Parser;

#[derive(Parser)]
#[grammar = "parser.pest"]
pub struct DebugParser;

#[test]
fn fn1() {
    let parse_result = DebugParser::parse(Rule::OrExpr, "true() or false()");
    pest_ascii_tree::print_ascii_tree(parse_result.clone());
    assert!(parse_result.is_ok())
}
