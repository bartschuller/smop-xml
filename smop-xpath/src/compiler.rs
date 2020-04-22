// Experiment with an execution model, inspired by Cloudflare wirefilter
enum Expr {
    Int(i64),
    Add(Box<Expr>, Box<Expr>),
    // Get(String),
    // Set(String, Box<Expr>)
}

struct ExecutionContext {}
struct CompiledExpr<'a>(Box<dyn 'a + Fn() -> i64>);
impl<'a> CompiledExpr<'a> {
    fn new(closure: impl 'a + Fn() -> i64) -> Self {
        CompiledExpr(Box::new(closure))
    }
    pub fn execute(&self) -> i64 {
        self.0()
    }
}

impl<'a> Expr {
    fn compile(self) -> CompiledExpr<'a> {
        match self {
            Expr::Int(i) => CompiledExpr::new(move || i),
            Expr::Add(l, r) => {
                let l = l.compile();
                let r = r.compile();
                CompiledExpr::new(move || l.execute() + r.execute())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::Expr::{Add, Int};

    #[test]
    fn test_compile() {
        let expr = Add(Box::new(Int(2)), Box::new(Int(5)));
        let compiled = expr.compile();
        assert_eq!(compiled.execute(), 7)
    }
}
