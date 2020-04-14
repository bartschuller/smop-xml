use crate::runner::{Environment, TestRunner, XpathValue};
use crate::{Dependency, SpecType};
use sxd_document::dom::Document;
use sxd_document::Package;
use sxd_xpath::{Context, Factory, Value};

pub struct SXDRunner<'a> {
    document: Document<'a>,
}
impl<'a> SXDRunner<'_> {
    pub fn new() -> Self {
        let package = Package::new();
        let document = package.as_document();
        SXDRunner { document }
    }
}
impl Environment for Context<'_> {
    fn bind_value(&mut self, name: &str) {
        unimplemented!()
    }
}
impl XpathValue for Value<'_> {}
impl<'a> TestRunner for SXDRunner<'a> {
    fn spec_supported(&self, spec: SpecType) -> bool {
        println!("{:?}", spec);
        false
    }

    fn evaluate<E, V>(&self, env: &E, xpath: &str) -> V
    where
        E: Environment,
        V: XpathValue,
    {
        let context = env as &Context;
        let factory = Factory::new();
        let xpath = factory.build(xpath).unwrap().unwrap();

        let root = self.document.root();
        let value = xpath.evaluate(context, root);
        value.unwrap()
    }
}
