use crate::model::{Dependency, EnvironmentSpec, TestSet};
use crate::runner::Environment;
use crate::runner::TestRunner;
use smop_xmltree::option_ext::OptionExt;
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
            if !self.dependencies_met(&test_set.dependencies) {
                continue;
            }
            for case in &test_set.test_cases {
                if !self.dependencies_met(&case.dependencies) {
                    continue;
                }
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
                            .or_else(|| global_environments.get(s))
                            .unwrap_or_else(|| panic!("No environment named \"{}\" found", s))
                    },
                );
                //println!("env spec: {:?}", environment_spec);
                let mut environment = self.runner.new_environment();
                for source in &environment_spec.sources {
                    if source.role.as_str() == Some(".") {
                        environment.set_context_document(source.file.as_str())
                    } else {
                    }
                }
                print!("{}: {}", case.name, case.test);
                let result = self.runner.evaluate(&environment, case.test.as_str());
                let result = self
                    .runner
                    .check(&result, &case.result)
                    .map_or("pass".to_string(), |s| format!("fail: {}", s));
                println!(": {}", result);
            }
        }
    }
    fn dependencies_met(&self, deps: &Vec<Dependency>) -> bool {
        deps.iter().all(|dep| self.dependency_met(dep))
    }
    fn dependency_met(&self, dep: &Dependency) -> bool {
        //println!("Dep: {:?}", dep);
        match dep {
            Dependency::Calendar => false,
            Dependency::DefaultLanguage => false,
            Dependency::DirectoryAsCollectionUri => false,
            Dependency::Feature => false,
            Dependency::FormatIntegerSequence => false,
            Dependency::Language => false,
            Dependency::Limits => false,
            Dependency::Spec(ref specs) => {
                specs.iter().any(|spec| self.runner.spec_supported(spec))
            }
            Dependency::UnicodeNormalizationForm => false,
            Dependency::UnicodeVersion => false,
            Dependency::XmlVersion => false,
            Dependency::XsdVersion => false,
            Dependency::Not(inner) => !self.dependency_met(inner),
        }
    }
}
