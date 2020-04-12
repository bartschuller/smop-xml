use crate::runner::TestRunner;

pub(crate) struct Driver {
    runner: Box<dyn TestRunner>
}

impl Driver {
    pub fn new(runner: Box<dyn TestRunner>) -> Self {
        Driver { runner }
    }
}