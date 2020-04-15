use crate::SpecType;

pub trait XpathValue {}
pub trait Environment {
    fn bind_value(&mut self, name: &str);
}
pub trait TestRunner {
    type V: XpathValue;
    type E: Environment;
    fn spec_supported(&self, spec: SpecType) -> bool;
    //fn new_environment(&self) -> &Self::E;
    fn evaluate(&self, xpath: &str) -> Self::V;
}
