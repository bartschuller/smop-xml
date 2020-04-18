use crate::model::SpecType;
use crate::runner::{Environment, TestError, TestRunner, XpathValue};
use sxd_document::Package;
use sxd_xpath::{Context, ExecutionError, Factory, ParserError, Value};

impl From<ParserError> for TestError {
    fn from(pe: ParserError) -> Self {
        TestError::ParseError(Some(pe))
    }
}

impl From<ExecutionError> for TestError {
    fn from(ee: ExecutionError) -> Self {
        TestError::ExecutionError(ee)
    }
}

pub struct SXDRunner<'a> {
    package: &'a Package,
}
impl<'a> SXDRunner<'a> {
    pub fn new(package: &'a Package) -> SXDRunner<'a> {
        let mut runner = SXDRunner { package };
        runner
    }
}
pub struct SXDEnvironment {}
impl SXDEnvironment {
    fn new() -> Self {
        SXDEnvironment {}
    }
}
impl Environment for SXDEnvironment {
    fn bind_value(&mut self, name: &str) {
        unimplemented!()
    }
}
impl XpathValue for Value<'_> {}
impl<'a> TestRunner for SXDRunner<'a> {
    type V = Value<'a>;
    type E = SXDEnvironment;

    fn spec_supported(&self, spec: SpecType) -> bool {
        println!("{:?}", spec);
        false
    }

    fn new_environment(&self) -> SXDEnvironment {
        SXDEnvironment::new()
    }

    fn evaluate(&self, xpath: &str) -> Result<Value<'a>, TestError> {
        let factory = Factory::new();
        let xpath = factory.build(xpath)?.ok_or(TestError::ParseError(None))?;
        let root = self.package.as_document().root();
        let context = Context::new();
        let value = xpath.evaluate(&context, root)?;
        Ok(value)
    }
}
