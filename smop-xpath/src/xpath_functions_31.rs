use crate::functions::{CompiledFunction, Function};
use crate::types::Item;
use crate::types::{Occurrence, SequenceType};
use crate::xdm::{QName, Xdm, XdmError};
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
}

pub(crate) fn fn_boolean_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, mut args| loop {
        return if let Some(xdm) = args.first() {
            match xdm {
                Xdm::Node(_) => Ok(Xdm::Boolean(true)),
                _ if args.len() > 1 => Err(XdmError::xqtm("err:FORG0006", "Invalid argument type")),
                Xdm::Boolean(b) => Ok(Xdm::Boolean(*b)),
                Xdm::String(s) => Ok(Xdm::Boolean(!s.is_empty())),
                Xdm::Decimal(d) => Ok(Xdm::Boolean(!d.is_zero())),
                Xdm::Integer(i) => Ok(Xdm::Boolean(!i.is_zero())),
                Xdm::Double(d) => Ok(Xdm::Boolean(!(d.is_nan() || d.is_zero()))),
                Xdm::Sequence(v) => {
                    args = v.to_vec();
                    continue;
                }
                _ => Err(XdmError::xqtm("err:FORG0006", "Invalid argument type")),
            }
        } else {
            Ok(Xdm::Boolean(false))
        };
    })
}
pub(crate) fn fn_not_1() -> CompiledFunction {
    let boolean = fn_boolean_1();
    CompiledFunction::new(move |ctx, args| {
        Ok(match boolean.execute(ctx, args)? {
            Xdm::Boolean(b) => Xdm::Boolean(!b),
            _ => unreachable!(),
        })
    })
}
pub(crate) fn fn_true_0() -> CompiledFunction {
    CompiledFunction::new(|_ctx, _args| Ok(Xdm::Boolean(true)))
}
pub(crate) fn fn_false_0() -> CompiledFunction {
    CompiledFunction::new(|_ctx, _args| Ok(Xdm::Boolean(false)))
}

#[cfg(test)]
mod tests {
    use crate::runtime::DynamicContext;
    use crate::xdm::{Xdm, XdmResult};
    use crate::xpath_functions_31::fn_boolean_1;

    #[test]
    fn fn_boolean1() -> XdmResult<()> {
        let ctx: DynamicContext = Default::default();
        let args = vec![];
        let result = fn_boolean_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(false)));
        let args = vec![Xdm::Boolean(true), Xdm::Boolean(false)];
        let result = fn_boolean_1().execute(&ctx, args);
        assert!(result.is_err());
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
}
