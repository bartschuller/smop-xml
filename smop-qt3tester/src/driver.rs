use crate::runner::TestRunner;
use crate::{EnvironmentSpec, TestSet};
use std::collections::HashMap;

pub(crate) struct Driver<R: TestRunner> {
    runner: Box<R>,
}

impl<R: TestRunner> Driver<R> {
    pub fn new(runner: Box<R>) -> Self {
        Driver { runner }
    }
    pub fn run_tests(
        &self,
        global_environments: &HashMap<String, EnvironmentSpec>,
        test_sets: &Vec<TestSet>,
    ) {
        for test_set in test_sets {
            for case in &test_set.test_cases {
                let environment_spec = case
                    .environment
                    .as_ref()
                    .unwrap_or(&global_environments["empty"]);
                let environment_spec = environment_spec.reference.as_ref().map_or_else(
                    || environment_spec,
                    |s| {
                        test_set
                            .environments
                            .get(s)
                            .unwrap_or(&global_environments[s])
                    },
                );
                //let mut environment = self.runner.new_environment();
                //...
                println!("{}: {}", case.name, case.test);
                let result = self.runner.evaluate(case.test.as_str());
            }
        }
    }
}
