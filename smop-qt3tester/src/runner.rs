use crate::{Dependency, SpecType};

pub trait XpathValue {}
pub trait Environment {
    fn bind_value(&mut self, name: &str);
}
pub trait TestRunner {
    fn spec_supported(&self, spec: SpecType) -> bool;
    fn evaluate<E, V>(&self, env: &E, xpath: &str) -> V
    where
        E: Environment,
        V: XpathValue;
}
