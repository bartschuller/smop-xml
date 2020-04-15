use crate::runner::{Environment, TestRunner, XpathValue};
use crate::SpecType;
use std::borrow::Borrow;
use std::cell::Cell;
use sxd_document::dom::Document;
use sxd_document::Package;
use sxd_xpath::{Context, Factory, Value};

pub struct SXDRunner<'a> {
    package: &'a Package,
}
impl<'a> SXDRunner<'a> {
    pub fn new(package: &'a Package) -> SXDRunner<'a> {
        let mut runner = SXDRunner { package };
        runner
    }
}
impl Environment for Context<'_> {
    fn bind_value(&mut self, name: &str) {
        unimplemented!()
    }
}
impl XpathValue for Value<'_> {}
impl<'a> TestRunner for SXDRunner<'a> {
    type V = Value<'a>;
    type E = Context<'a>;

    fn spec_supported(&self, spec: SpecType) -> bool {
        println!("{:?}", spec);
        false
    }

    //fn new_environment(&self) -> &Context<'a> {
    //    &self.context
    //}

    fn evaluate(&self, xpath: &str) -> Value<'a> {
        let factory = Factory::new();
        let xpath = factory.build(xpath).unwrap().unwrap();

        let root = self.package.as_document().root();
        let context = Context::new();
        let value = xpath.evaluate(&context, root);
        value.unwrap()
    }
}
