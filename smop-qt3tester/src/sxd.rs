use crate::runner::{TestRunner, Environment, XpathValue};
use sxd_xpath::{Value, Context};

pub struct SXDRunner {}
impl SXDRunner {
    pub fn new() -> SXDRunner {
        SXDRunner {}
    }
}
impl Environment for Context<'_> {
    fn bind_value(&mut self, name: &str) {
        unimplemented!()
    }
}
impl XpathValue for Value<'_> {

}
impl TestRunner for SXDRunner {
    fn dependency_satisfied(&self) -> bool {
        unimplemented!()
    }

    fn new_environment(&self) -> Box<dyn Environment> {
        Box::new(Context::new())
    }

    fn evaluate(&self, env: &Environment, xpath: &str) -> Box<dyn XpathValue> {
        unimplemented!()
    }
}