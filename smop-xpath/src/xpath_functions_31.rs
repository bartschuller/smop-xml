use crate::functions::{CompiledFunction, Function};
use crate::types::{Item, KindTest};
use crate::types::{Occurrence, SequenceType};
use crate::xdm::{NodeSeq, Xdm, XdmError, XdmResult};
use crate::StaticContext;

use rust_decimal::Decimal;
use smop_xmltree::nod::QName;

pub(crate) fn register(ctx: &mut StaticContext) {
    let xs_boolean = QName::wellknown("xs:boolean");
    let xs_string = QName::wellknown("xs:string");
    let xs_any_atomic_type = QName::wellknown("xs:anyAtomicType");
    let xs_double = QName::wellknown("xs:double");
    let xs_integer = QName::wellknown("xs:integer");

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

    let fn_count_1_meta = Function {
        args: vec![SequenceType::Item(Item::Item, Occurrence::ZeroOrMore)],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_integer).unwrap()),
            Occurrence::One,
        ),
        code: fn_count_1,
    };
    let qname = ctx.qname("fn", "count").unwrap();
    ctx.add_function(qname, fn_count_1_meta);

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
        args: vec![SequenceType::Item(
            Item::KindTest(KindTest::AnyKind),
            Occurrence::Optional,
        )],
        type_: SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::Optional),
        code: fn_root_1,
    };
    let qname = ctx.qname("fn", "root").unwrap();
    ctx.add_function(qname, fn_root_1_meta);

    let fn_string_join_1_meta = Function {
        args: vec![SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_any_atomic_type).unwrap()),
            Occurrence::ZeroOrMore,
        )],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
            Occurrence::One,
        ),
        code: fn_string_join_1,
    };
    let qname = ctx.qname("fn", "string-join").unwrap();
    ctx.add_function(qname, fn_string_join_1_meta);

    let fn_concat_2_meta = Function {
        args: vec![
            SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&xs_any_atomic_type).unwrap()),
                Occurrence::Optional,
            ),
            SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&xs_any_atomic_type).unwrap()),
                Occurrence::Optional,
            ),
        ],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
            Occurrence::One,
        ),
        code: fn_concat_2,
    };
    let qname = ctx.qname("fn", "concat").unwrap();
    ctx.add_function(qname, fn_concat_2_meta);

    let xs_double_1_meta = Function {
        args: vec![SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_any_atomic_type).unwrap()),
            Occurrence::ZeroOrMore,
        )],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_double).unwrap()),
            Occurrence::One,
        ),
        code: xs_double_1,
    };
    let qname = ctx.qname("xs", "double").unwrap();
    ctx.add_function(qname, xs_double_1_meta);

    let fn_string_1_meta = Function {
        args: vec![SequenceType::Item(Item::Item, Occurrence::Optional)],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
            Occurrence::One,
        ),
        code: fn_string_1,
    };
    let qname = ctx.qname("fn", "string").unwrap();
    ctx.add_function(qname.clone(), fn_string_1_meta);
    let fn_string_0_meta = Function {
        args: vec![],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
            Occurrence::One,
        ),
        code: fn_string_0,
    };
    ctx.add_function(qname, fn_string_0_meta);

    let qname = ctx.qname("fn", "name").unwrap();
    let fn_name_1_meta = Function {
        args: vec![SequenceType::Item(
            Item::KindTest(KindTest::AnyKind),
            Occurrence::Optional,
        )],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
            Occurrence::One,
        ),
        code: fn_name_1,
    };
    ctx.add_function(qname, fn_name_1_meta);

    let qname = ctx.qname("fn", "empty").unwrap();
    let fn_empty_1_meta = Function {
        args: vec![SequenceType::Item(
            Item::KindTest(KindTest::AnyKind),
            Occurrence::ZeroOrMore,
        )],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_boolean).unwrap()),
            Occurrence::One,
        ),
        code: fn_empty_1,
    };
    ctx.add_function(qname, fn_empty_1_meta);

    let qname = ctx.qname("fn", "position").unwrap();
    let fn_position_0_meta = Function {
        args: vec![],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_integer).unwrap()),
            Occurrence::One,
        ),
        code: fn_position_0,
    };
    ctx.add_function(qname, fn_position_0_meta);
}

pub(crate) fn fn_boolean_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| args.first().unwrap().boolean().map(Xdm::Boolean))
}
pub(crate) fn fn_count_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| {
        Ok(Xdm::Integer(
            args.first().map(|x| x.count()).unwrap_or(0) as i64
        ))
    })
}
pub(crate) fn fn_not_1() -> CompiledFunction {
    let _boolean = fn_boolean_1();
    CompiledFunction::new(|_ctx, args| args.first().unwrap().boolean().map(|b| Xdm::Boolean(!b)))
}
pub(crate) fn fn_true_0() -> CompiledFunction {
    CompiledFunction::new(|_ctx, _args| Ok(Xdm::Boolean(true)))
}
pub(crate) fn fn_false_0() -> CompiledFunction {
    CompiledFunction::new(|_ctx, _args| Ok(Xdm::Boolean(false)))
}
pub(crate) fn fn_root_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| {
        if let Some(Xdm::NodeSeq(NodeSeq::RoXml(oh))) = args.first() {
            Ok(Xdm::NodeSeq(NodeSeq::RoXml(oh.document().root())))
        } else {
            unreachable!()
        }
    })
}
pub(crate) fn fn_string_join_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, mut args| {
        let x = args.remove(0);
        Ok(match x {
            Xdm::Sequence(v) => {
                let mut res = String::new();
                for x in v {
                    let s = x.string()?;
                    res.push_str(s.as_str());
                }
                Xdm::String(res)
            }
            _ => Xdm::String(x.string()?),
        })
    })
}
pub(crate) fn fn_concat_2() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| {
        let strings: XdmResult<Vec<_>> = args.into_iter().map(|x| x.string()).collect();
        Ok(Xdm::String(strings?.concat()))
    })
}
pub(crate) fn xs_double_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| {
        args.first()
            .map_or(Ok(Xdm::Sequence(vec![])), |x| x.double().map(Xdm::Double))
    })
}
pub(crate) fn fn_string_0() -> CompiledFunction {
    CompiledFunction::new(|ctx, mut _args| {
        ctx.focus
            .as_ref()
            .map_or(Ok(Xdm::String("".to_string())), |focus| {
                focus.sequence.string().map(Xdm::String)
            })
    })
}
pub(crate) fn fn_string_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| {
        args.first().map_or(Ok(Xdm::String("".to_string())), |x| {
            x.string().map(Xdm::String)
        })
    })
}
pub(crate) fn fn_name_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| match args.first().unwrap() {
        Xdm::NodeSeq(NodeSeq::RoXml(node)) => Ok(Xdm::String(
            node.node_name().map_or("".to_string(), |q| q.to_string()),
        )),
        _ => Err(XdmError::xqtm(
            "XPTY0004",
            "expected a node as argument to name()",
        )),
    })
}
pub(crate) fn fn_empty_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| match args.first().unwrap() {
        Xdm::Sequence(v) if v.is_empty() => Ok(Xdm::Boolean(true)),
        _ => Ok(Xdm::Boolean(false)),
    })
}
pub(crate) fn fn_position_0() -> CompiledFunction {
    CompiledFunction::new(|ctx, _args| match &ctx.focus {
        Some(focus) => Ok(Xdm::Integer(focus.position as i64 + 1)),
        None => Err(XdmError::xqtm(
            "XPDY0002",
            "context item not defined in position()",
        )),
    })
}
pub(crate) fn fn_trace_1() -> CompiledFunction {
    CompiledFunction::new(|ctx, args|{
        let val = args.
    })
}
pub(crate) fn string_compare(s1: &str, s2: &str) -> i8 {
    s1.cmp(s2) as i8
}
pub(crate) fn double_compare(d1: &f64, d2: &f64) -> i8 {
    d1.partial_cmp(d2).unwrap() as i8
}
pub(crate) fn decimal_compare(d1: &Decimal, d2: &Decimal) -> i8 {
    d1.cmp(d2) as i8
}
pub(crate) fn integer_compare(i1: &i64, i2: &i64) -> i8 {
    i1.cmp(i2) as i8
}

#[cfg(test)]
mod tests {
    use crate::runtime::DynamicContext;
    use crate::xdm::{Xdm, XdmResult};
    use crate::xpath_functions_31::{fn_boolean_1, fn_not_1, xs_double_1};
    use crate::StaticContext;
    use std::f64::NAN;
    use std::rc::Rc;

    #[test]
    fn fn_boolean1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let ctx: DynamicContext = static_context.new_dynamic_context();
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
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let ctx: DynamicContext = static_context.new_dynamic_context();
        let args = vec![Xdm::Integer(0)];
        let result = fn_not_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(true)));
        let args = vec![Xdm::Double(0.0)];
        let result = fn_not_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(true)));
        let args = vec![Xdm::Double(NAN)];
        let result = fn_not_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(true)));
        let args = vec![Xdm::String("".to_string())];
        let result = fn_not_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(true)));
        let args = vec![Xdm::String("0".to_string())];
        let result = fn_not_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Boolean(false)));
        Ok(())
    }
    #[test]
    fn xs_double1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let ctx: DynamicContext = static_context.new_dynamic_context();
        let args = vec![Xdm::Integer(0)];
        let result = xs_double_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Double(0.0)));
        let args = vec![Xdm::String("NaN".to_string())];
        let result = xs_double_1().execute(&ctx, args);
        if let Xdm::Double(nan) = result? {
            assert!(nan.is_nan());
        } else {
            panic!("not a double");
        }
        let args = vec![Xdm::String("-0.5".to_string())];
        let result = xs_double_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Double(-0.5)));
        Ok(())
    }
}
