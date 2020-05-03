use pest::Parser;

#[derive(Parser)]
#[grammar = "parser.pest"]
pub struct DebugParser;

#[test]
fn fn1() {
    let parse_result = DebugParser::parse(Rule::EQName, "Q{http://example.xom/}a");
    pest_ascii_tree::print_ascii_tree(parse_result.clone());
    assert!(parse_result.is_ok())
}
