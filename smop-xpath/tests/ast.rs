use std::rc::Rc;
use xpath::runtime::DynamicContext;
use xpath::xdm::XdmResult;
use xpath::{StaticContext, Xpath};

#[test]
fn for1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, "for $i in 10, $j in 20 return $j+$i")?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.integer()?, 30);
    Ok(())
}

#[test]
fn for2() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, "for $i in (10, 20) return $i + 1")?;
    let result = xpath.evaluate(&context)?;
    let vec: Vec<i64> = result.into_iter().map(|i| i.integer().unwrap()).collect();
    assert_eq!(vec, vec![11, 21]);
    Ok(())
}

#[test]
fn for3() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(
        &static_context,
        "for $i in (10, 20),
                   $j in (1, $i)
               return ($i + $j)",
    )?;
    let result = xpath.evaluate(&context)?;
    let vec: Vec<i64> = result.into_iter().map(|i| i.integer().unwrap()).collect();
    assert_eq!(vec, vec![11, 20, 21, 40]);
    Ok(())
}

#[test]
fn let1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, "let $i := 10, $j := 20 return $j+$i")?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.integer()?, 30);
    Ok(())
}

#[test]
fn gen_compare1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(
        &static_context,
        "let $a := '1', $b := '2', $c := '2.0' return ($a, $b) = ($c, 3.0)",
    )?;
    let result = xpath.evaluate(&context)?;
    assert!(!result.boolean()?);
    Ok(())
}

#[test]
fn gen_compare2() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(
        &static_context,
        "let $a := '1', $b := '2', $c := '2.0' return ($a, $b) = ($c, 2.0)",
    )?;
    let result = xpath.evaluate(&context)?;
    assert!(result.boolean()?);
    Ok(())
}

#[test]
fn gen_compare3() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, "(1, 2) = (2, 3)")?;
    let result = xpath.evaluate(&context)?;
    assert!(result.boolean()?);
    Ok(())
}

#[test]
fn gen_compare4() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, "(1, 2) != (2, 3)")?;
    let result = xpath.evaluate(&context)?;
    assert!(result.boolean()?);
    Ok(())
}

#[test]
fn and1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, "1 and true() and '0'")?;
    let result = xpath.evaluate(&context)?;
    assert!(result.boolean()?);
    let xpath = Xpath::compile(&static_context, "1 and true() and ''")?;
    let result = xpath.evaluate(&context)?;
    assert!(!result.boolean()?);
    let xpath = Xpath::compile(&static_context, "string((1,1))")?;
    let result = xpath.evaluate(&context);
    assert!(result.is_err());
    let xpath = Xpath::compile(&static_context, "0 and string((1,1))")?;
    let result = xpath.evaluate(&context)?;
    assert!(!result.boolean()?);
    Ok(())
}

#[test]
fn or1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, "0 or false() or ''")?;
    let result = xpath.evaluate(&context)?;
    assert!(!result.boolean()?);
    let xpath = Xpath::compile(&static_context, "'' or 1")?;
    let result = xpath.evaluate(&context)?;
    assert!(result.boolean()?);
    let xpath = Xpath::compile(&static_context, "string((1,1))")?;
    let result = xpath.evaluate(&context);
    assert!(result.is_err());
    let xpath = Xpath::compile(&static_context, "1 or string((1,1))")?;
    let result = xpath.evaluate(&context)?;
    assert!(result.boolean()?);
    Ok(())
}

#[test]
fn quantified1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(
        &static_context,
        "every $a in (1, 2, 3), $b in ($a, 4) satisfies $b gt 0",
    )?;
    let result = xpath.evaluate(&context)?;
    assert!(result.boolean()?);
    Ok(())
}

#[test]
fn concat1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, r#""con" || "cat" || "en" || 8"#)?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.string()?, "concaten8");
    Ok(())
}

#[test]
fn unary_minus1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, "+1")?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.string()?, "1");
    let xpath = Xpath::compile(&static_context, "++1")?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.string()?, "1");
    Ok(())
}

#[test]
fn unary_minus2() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, "-1")?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.string()?, "-1");
    let xpath = Xpath::compile(&static_context, "--1")?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.string()?, "1");
    Ok(())
}

#[test]
fn slashslash1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let expr1 = static_context
        .parse("(fn:root(self::node()) treat as document-node())/descendant-or-self::node()/foo")?;
    let expr2 = static_context.parse("//foo")?;
    assert_eq!(expr1, expr2);
    Ok(())
}

#[test]
fn array1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, r#"array:size([1, 2, (), 3])"#)?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.integer()?, 4);
    Ok(())
}
