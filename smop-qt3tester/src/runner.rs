
pub trait XpathValue {}
pub trait Environment {
    fn bind_value(&mut self, name: &str);
}
pub trait TestRunner {
    fn dependency_satisfied(&self) -> bool;
    fn new_environment(&self) -> Box<dyn Environment>;
    fn evaluate(&self, env: &dyn Environment, xpath: &str) -> Box<dyn XpathValue>;
}

