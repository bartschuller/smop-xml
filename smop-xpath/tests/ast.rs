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
