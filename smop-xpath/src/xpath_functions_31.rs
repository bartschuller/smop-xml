use crate::functions::{CompiledFunction, Function};
use crate::types::Item;
use crate::types::{Occurrence, SequenceType};
use crate::xdm::{NodeSeq, QName, Xdm, XdmError};
use crate::StaticContext;
use num_traits::identities::Zero;

pub(crate) fn register(ctx: &mut StaticContext) {
    let xs_boolean = QName::wellknown("xs:boolean");

    let fn_boolean_1_meta = Function {
        args: vec![SequenceType::Item(Item::Item, Occurrence::ZeroOrMore)],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_boolean).unwrap()),
            Occurrence::One,
        ),
        code: fn_boolean_1,
    };
    let qname = ctx.qname("fn", "boolean").unwrap();
    ctx.add_function(qname, fn_boolean_1_meta);

    let fn_not_1_meta = Function {
        args: vec![SequenceType::Item(Item::Item, Occurrence::ZeroOrMore)],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_boolean).unwrap()),
            Occurrence::One,
        ),
        code: fn_not_1,
    };
    let qname = ctx.qname("fn", "not").unwrap();
    ctx.add_function(qname, fn_not_1_meta);

    let fn_true_0_meta = Function {
        args: vec![],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_boolean).unwrap()),
            Occurrence::One,
        ),
        code: fn_true_0,
    };
    let qname = ctx.qname("fn", "true").unwrap();
    ctx.add_function(qname, fn_true_0_meta);

    let fn_false_0_meta = Function {
        args: vec![],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_boolean).unwrap()),
            Occurrence::One,
        ),
        code: fn_false_0,
    };
    let qname = ctx.qname("fn", "false").unwrap();
    ctx.add_function(qname, fn_false_0_meta);

    let fn_root_1_meta = Function {
        args: vec![SequenceType::Item(Item::KindTest, Occurrence::Optional)],
        type_: SequenceType::Item(Item::KindTest, Occurrence::Optional),
        code: fn_root_1,
    };
    let qname = ctx.qname("fn", "root").unwrap();
    ctx.add_function(qname, fn_root_1_meta);
}

pub(crate) fn fn_boolean_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, mut args| loop {
        return if let Some(xdm) = args.first() {
            xdm.boolean().map(Xdm::Boolean)
        } else {
            Ok(Xdm::Boolean(false))
        };
    })
}
pub(crate) fn fn_not_1() -> CompiledFunction {
    let boolean = fn_boolean_1();
    CompiledFunction::new(|ctx, args| {
        return if let Some(xdm) = args.first() {
            xdm.boolean().map(|b| Xdm::Boolean(!b))
        } else {
            Ok(Xdm::Boolean(true))
        };
    })
}
pub(crate) fn fn_true_0() -> CompiledFunction {
    CompiledFunction::new(|_ctx, _args| Ok(Xdm::Boolean(true)))
}
pub(crate) fn fn_false_0() -> CompiledFunction {
    CompiledFunction::new(|_ctx, _args| Ok(Xdm::Boolean(false)))
}
pub(crate) fn fn_root_1() -> CompiledFunction {
    CompiledFunction::new(|ctx, args| {
        if let Some(Xdm::NodeSeq(NodeSeq::RoXml(oh))) = args.first() {
            Ok(Xdm::NodeSeq(NodeSeq::RoXml(oh.document().root())))
        } else {
            unreachable!()
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::runtime::DynamicContext;
    use crate::xdm::{Xdm, XdmResult};
    use crate::xpath_functions_31::{fn_boolean_1, fn_not_1};

    #[test]
    fn fn_boolean1() -> XdmResult<()> {
        let ctx: DynamicContext = Default::default();
        let args = vec![];
        let result = fn_boolean_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(false)));
        let args = vec![Xdm::Integer(0)];
        let result = fn_boolean_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(false)));
        let args = vec![Xdm::Double(0.003)];
        let result = fn_boolean_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(true)));
        let args = vec![Xdm::String("".to_string())];
        let result = fn_boolean_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(false)));
        let args = vec![Xdm::String("0".to_string())];
        let result = fn_boolean_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(true)));
        Ok(())
    }
    #[test]
    fn fn_not1() -> XdmResult<()> {
        let ctx: DynamicContext = Default::default();
        let args = vec![];
        let result = fn_not_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(true)));
        let args = vec![Xdm::Integer(0)];
        let result = fn_not_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(true)));
        let args = vec![Xdm::Double(0.003)];
        let result = fn_not_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(false)));
        let args = vec![Xdm::String("".to_string())];
        let result = fn_not_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(true)));
        let args = vec![Xdm::String("0".to_string())];
        let result = fn_not_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(false)));
        Ok(())
    }
}
