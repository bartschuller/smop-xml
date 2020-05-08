use crate::runtime::DynamicContext;
use crate::xdm::{Xdm, XdmError, XdmResult};
use num_traits::identities::Zero;

// op:numeric-add($arg1 as xs:numeric, $arg2 as xs:numeric) as xs:numeric
//fn op_numeric_add(arg1: Xdm, arg2: Xdm) -> XdmResult<Xdm> {}

// Fn(&'context DynamicContext) -> XdmResult<Xdm<'context>
pub(crate) fn fn_boolean<'a>(ctx: &'a DynamicContext, args: Vec<Xdm>) -> XdmResult<Xdm<'a>> {
    if let Some(xdm) = args.first() {
        match xdm {
            Xdm::Node(_) => Ok(Xdm::Boolean(true)),
            _ if args.len() > 1 => Err(XdmError::xqtm("err:FORG0006", "Invalid argument type")),
            Xdm::Boolean(b) => Ok(Xdm::Boolean(*b)),
            Xdm::String(s) => Ok(Xdm::Boolean(!s.is_empty())),
            Xdm::Decimal(d) => Ok(Xdm::Boolean(!d.is_zero())),
            Xdm::Integer(i) => Ok(Xdm::Boolean(!i.is_zero())),
            Xdm::Double(d) => Ok(Xdm::Boolean(!(d.is_nan() || d.is_zero()))),
            Xdm::Sequence(v) => fn_boolean(ctx, v.to_vec()),
            _ => Err(XdmError::xqtm("err:FORG0006", "Invalid argument type")),
        }
    } else {
        Ok(Xdm::Boolean(false))
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime::DynamicContext;
    use crate::xdm::{Xdm, XdmResult};
    use crate::xpath_functions_31::fn_boolean;

    #[test]
    fn fn_boolean1() -> XdmResult<()> {
        let ctx: DynamicContext = Default::default();
        let args = vec![];
        let result = fn_boolean(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(false)));
        let args = vec![Xdm::Boolean(true), Xdm::Boolean(false)];
        let result = fn_boolean(&ctx, args);
        assert!(result.is_err());
        let args = vec![Xdm::Integer(0)];
        let result = fn_boolean(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(false)));
        let args = vec![Xdm::Double(0.003)];
        let result = fn_boolean(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(true)));
        let args = vec![Xdm::String("".to_string())];
        let result = fn_boolean(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(false)));
        let args = vec![Xdm::String("0".to_string())];
        let result = fn_boolean(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(true)));
        Ok(())
    }
}
