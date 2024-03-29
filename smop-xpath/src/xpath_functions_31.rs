use crate::functions::{CompiledFunction, Function};
use crate::types::{Item, KindTest};
use crate::types::{Occurrence, SequenceType};
use crate::xdm::{Xdm, XdmError, XdmResult};
use crate::StaticContext;

use rust_decimal::Decimal;
use smop_xmltree::nod::QName;

pub(crate) fn register(ctx: &mut StaticContext) {
    let xs_boolean = QName::wellknown("xs:boolean");
    let xs_string = QName::wellknown("xs:string");
    let xs_any_atomic_type = QName::wellknown("xs:anyAtomicType");
    let xs_double = QName::wellknown("xs:double");
    let xs_float = QName::wellknown("xs:float");
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
    ctx.add_function(qname.clone(), fn_string_join_1_meta);
    let fn_string_join_2_meta = Function {
        args: vec![
            SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&xs_any_atomic_type).unwrap()),
                Occurrence::ZeroOrMore,
            ),
            SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
                Occurrence::One,
            ),
        ],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
            Occurrence::One,
        ),
        code: fn_string_join_2,
    };
    ctx.add_function(qname, fn_string_join_2_meta);

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

    let xs_float_1_meta = Function {
        args: vec![SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_any_atomic_type).unwrap()),
            Occurrence::ZeroOrMore,
        )],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_float).unwrap()),
            Occurrence::One,
        ),
        code: xs_float_1,
    };
    let qname = ctx.qname("xs", "float").unwrap();
    ctx.add_function(qname, xs_float_1_meta);

    let xs_integer_1_meta = Function {
        args: vec![SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_any_atomic_type).unwrap()),
            Occurrence::ZeroOrMore,
        )],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_integer).unwrap()),
            Occurrence::One,
        ),
        code: xs_integer_1,
    };
    let qname = ctx.qname("xs", "integer").unwrap();
    ctx.add_function(qname, xs_integer_1_meta);

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
    let fn_name_0_meta = Function {
        args: vec![],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
            Occurrence::One,
        ),
        code: fn_name_0,
    };
    ctx.add_function(qname.clone(), fn_name_0_meta);
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

    let qname = ctx.qname("fn", "local-name").unwrap();
    let fn_local_name_0_meta = Function {
        args: vec![],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
            Occurrence::One,
        ),
        code: fn_local_name_0,
    };
    ctx.add_function(qname.clone(), fn_local_name_0_meta);
    let fn_local_name_1_meta = Function {
        args: vec![SequenceType::Item(
            Item::KindTest(KindTest::AnyKind),
            Occurrence::Optional,
        )],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
            Occurrence::One,
        ),
        code: fn_local_name_1,
    };
    ctx.add_function(qname, fn_local_name_1_meta);

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

    let qname = ctx.qname("fn", "trace").unwrap();
    let fn_trace_1_meta = Function {
        args: vec![SequenceType::Item(
            Item::KindTest(KindTest::AnyKind),
            Occurrence::ZeroOrMore,
        )],
        type_: SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore),
        code: fn_trace_1,
    };
    ctx.add_function(qname.clone(), fn_trace_1_meta);
    let fn_trace_2_meta = Function {
        args: vec![
            SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore),
            SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
                Occurrence::One,
            ),
        ],
        type_: SequenceType::Item(Item::KindTest(KindTest::AnyKind), Occurrence::ZeroOrMore),
        code: fn_trace_2,
    };
    ctx.add_function(qname, fn_trace_2_meta);

    let qname = ctx.qname("fn", "string-length").unwrap();
    let fn_string_length_0_meta = Function {
        args: vec![],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_integer).unwrap()),
            Occurrence::One,
        ),
        code: fn_string_length_0,
    };
    ctx.add_function(qname.clone(), fn_string_length_0_meta);
    let fn_string_length_1_meta = Function {
        args: vec![SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
            Occurrence::Optional,
        )],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_integer).unwrap()),
            Occurrence::One,
        ),
        code: fn_string_length_1,
    };
    ctx.add_function(qname, fn_string_length_1_meta);

    let qname = ctx.qname("fn", "last").unwrap();
    let fn_last_0_meta = Function {
        args: vec![],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_integer).unwrap()),
            Occurrence::One,
        ),
        code: fn_last_0,
    };
    ctx.add_function(qname, fn_last_0_meta);

    let qname = ctx.qname("fn", "contains").unwrap();
    let fn_contains_2_meta = Function {
        args: vec![
            SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
                Occurrence::Optional,
            ),
            SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&xs_string).unwrap()),
                Occurrence::Optional,
            ),
        ],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_boolean).unwrap()),
            Occurrence::One,
        ),
        code: fn_contains_2,
    };
    ctx.add_function(qname, fn_contains_2_meta);

    let qname = ctx.qname("fn", "number").unwrap();
    let fn_number_0_meta = Function {
        args: vec![],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_double).unwrap()),
            Occurrence::One,
        ),
        code: fn_number_0,
    };
    ctx.add_function(qname.clone(), fn_number_0_meta);
    let fn_number_1_meta = Function {
        args: vec![SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_any_atomic_type).unwrap()),
            Occurrence::Optional,
        )],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_double).unwrap()),
            Occurrence::One,
        ),
        code: fn_number_1,
    };
    ctx.add_function(qname, fn_number_1_meta);

    let qname = ctx.qname("fn", "exactly-one").unwrap();
    let fn_exactly_one_1_meta = Function {
        args: vec![SequenceType::Item(Item::Item, Occurrence::ZeroOrMore)],
        type_: SequenceType::Item(Item::Item, Occurrence::One),
        code: fn_exactly_one_1,
    };
    ctx.add_function(qname, fn_exactly_one_1_meta);

    let qname = ctx.qname("fn", "deep-equal").unwrap();
    let fn_deep_equal_2_meta = Function {
        args: vec![
            SequenceType::Item(Item::Item, Occurrence::ZeroOrMore),
            SequenceType::Item(Item::Item, Occurrence::ZeroOrMore),
        ],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_boolean).unwrap()),
            Occurrence::One,
        ),
        code: fn_deep_equal_2,
    };
    ctx.add_function(qname, fn_deep_equal_2_meta);

    let qname = ctx.qname("fn", "exists").unwrap();
    let fn_exists_1_meta = Function {
        args: vec![SequenceType::Item(Item::Item, Occurrence::ZeroOrMore)],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_boolean).unwrap()),
            Occurrence::One,
        ),
        code: fn_exists_1,
    };
    ctx.add_function(qname, fn_exists_1_meta);

    let qname = ctx.qname("fn", "data").unwrap();
    let fn_data_1_meta = Function {
        args: vec![SequenceType::Item(Item::Item, Occurrence::ZeroOrMore)],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_any_atomic_type).unwrap()),
            Occurrence::ZeroOrMore,
        ),
        code: fn_data_1,
    };
    ctx.add_function(qname, fn_data_1_meta);

    let qname = ctx.qname("fn", "subsequence").unwrap();
    let fn_subsequence_2_meta = Function {
        args: vec![
            SequenceType::Item(Item::Item, Occurrence::ZeroOrMore),
            SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&xs_double).unwrap()),
                Occurrence::One,
            ),
        ],
        type_: SequenceType::Item(Item::Item, Occurrence::ZeroOrMore),
        code: fn_subsequence_2,
    };
    ctx.add_function(qname, fn_subsequence_2_meta);

    let qname = ctx.qname("array", "size").unwrap();
    let array_size_1_meta = Function {
        args: vec![SequenceType::Item(Item::ArrayTest(None), Occurrence::One)],
        type_: SequenceType::Item(
            Item::AtomicOrUnion(ctx.schema_type(&xs_integer).unwrap()),
            Occurrence::One,
        ),
        code: array_size_1,
    };
    ctx.add_function(qname, array_size_1_meta);

    let qname = ctx.qname("fn", "error").unwrap();
    let error_0_meta = Function {
        args: vec![],
        type_: SequenceType::EmptySequence,
        code: error_0,
    };
    ctx.add_function(qname, error_0_meta);
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
        if let Some(Xdm::Node(oh)) = args.first() {
            Ok(Xdm::Node(oh.document().root()))
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
pub(crate) fn fn_string_join_2() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| {
        let x = &args[0];
        let sep_string = args[1].string()?;
        Ok(match x {
            Xdm::Sequence(v) => {
                let strings: XdmResult<Vec<_>> = v.iter().map(|x| x.string()).collect();
                let res = strings?.join(sep_string.as_str());
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
            .map_or(Ok(Xdm::EmptySequence), |x| x.double().map(Xdm::Double))
    })
}
pub(crate) fn xs_float_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| {
        args.first()
            .map_or(Ok(Xdm::EmptySequence), |x| x.float().map(Xdm::Float))
    })
}
pub(crate) fn xs_integer_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| {
        args.first()
            .map_or(Ok(Xdm::EmptySequence), |x| x.integer().map(Xdm::Integer))
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
pub(crate) fn fn_name_0() -> CompiledFunction {
    CompiledFunction::new(|ctx, _args| match ctx.focus.as_ref().map(|f| &f.sequence) {
        Some(Xdm::Node(ref node)) => Ok(Xdm::String(
            node.node_name().map_or("".to_string(), |q| q.to_string()),
        )),
        _ => Err(XdmError::xqtm(
            "XPTY0004",
            "expected a node in context in call to name()",
        )),
    })
}
pub(crate) fn fn_name_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| match args.first().unwrap() {
        Xdm::Node(node) => Ok(Xdm::String(
            node.node_name().map_or("".to_string(), |q| q.to_string()),
        )),
        _ => Err(XdmError::xqtm(
            "XPTY0004",
            "expected a node as argument to name()",
        )),
    })
}
pub(crate) fn fn_local_name_0() -> CompiledFunction {
    CompiledFunction::new(|ctx, _args| match ctx.focus.as_ref().map(|f| &f.sequence) {
        Some(Xdm::Node(ref node)) => Ok(Xdm::String(
            node.node_name().map_or("".to_string(), |q| q.name),
        )),
        _ => Err(XdmError::xqtm(
            "XPTY0004",
            "expected a node in context in call to local-name()",
        )),
    })
}
pub(crate) fn fn_local_name_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| match args.first().unwrap() {
        Xdm::Node(node) => Ok(Xdm::String(
            node.node_name().map_or("".to_string(), |q| q.name),
        )),
        _ => Err(XdmError::xqtm(
            "XPTY0004",
            "expected a node as argument to local-name()",
        )),
    })
}
pub(crate) fn fn_empty_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| match args.first().unwrap() {
        Xdm::EmptySequence => Ok(Xdm::Boolean(true)),
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
pub(crate) fn fn_last_0() -> CompiledFunction {
    CompiledFunction::new(|ctx, _args| match &ctx.focus {
        Some(focus) => Ok(Xdm::Integer(focus.last() as i64 + 1)),
        None => Err(XdmError::xqtm(
            "XPDY0002",
            "context item not defined in position()",
        )),
    })
}
pub(crate) fn fn_trace_1() -> CompiledFunction {
    CompiledFunction::new(|ctx, mut args| {
        let val = args.remove(0);
        ctx.trace(&val);
        Ok(val)
    })
}
pub(crate) fn fn_trace_2() -> CompiledFunction {
    CompiledFunction::new(|ctx, mut args| {
        let val = args.remove(0);
        let label = args.first().unwrap();
        ctx.trace_label(&val, label)?;
        Ok(val)
    })
}
pub(crate) fn fn_string_length_0() -> CompiledFunction {
    CompiledFunction::new(|ctx, _args| match &ctx.focus {
        Some(focus) => Ok(Xdm::Integer(focus.sequence.string()?.chars().count() as i64)),
        None => Err(XdmError::xqtm(
            "XPDY0002",
            "context item not defined in string-length()",
        )),
    })
}
pub(crate) fn fn_string_length_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| Ok(Xdm::Integer(args[0].string()?.chars().count() as i64)))
}
pub(crate) fn fn_contains_2() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| {
        Ok(Xdm::Boolean(args[0].string()?.contains(&args[1].string()?)))
    })
}
pub(crate) fn fn_number_0() -> CompiledFunction {
    CompiledFunction::new(|ctx, _args| match &ctx.focus {
        Some(focus) => Ok(Xdm::Double(focus.sequence.double()?)),
        None => Err(XdmError::xqtm(
            "XPDY0002",
            "context item not defined in number()",
        )),
    })
}
pub(crate) fn fn_number_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| Ok(Xdm::Double(args[0].double()?)))
}
pub(crate) fn fn_exactly_one_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, mut args| {
        if args[0].count() == 1 {
            Ok(args.remove(0))
        } else {
            Err(XdmError::xqtm(
                "FORG0005",
                "fn:exactly-one called with a sequence containing zero or more than one item",
            ))
        }
    })
}
pub(crate) fn fn_deep_equal_2() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| Ok(Xdm::Boolean(args[0].deep_equal(&args[1]))))
}
pub(crate) fn fn_exists_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, args| Ok(Xdm::Boolean(!matches!(args[0], Xdm::EmptySequence))))
}
pub(crate) fn fn_data_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, mut args| args.remove(0).atomize())
}
pub(crate) fn fn_subsequence_2() -> CompiledFunction {
    CompiledFunction::new(|_ctx, mut args| {
        let starting_loc = 0.max(args[1].double()?.round() as i64 - 1) as usize;
        Ok(match &args[0] {
            Xdm::EmptySequence => Xdm::EmptySequence,
            Xdm::String(_)
            | Xdm::Boolean(_)
            | Xdm::Decimal(_)
            | Xdm::Integer(_)
            | Xdm::Double(_)
            | Xdm::Float(_)
            | Xdm::Node(_)
            | Xdm::Array(_)
            | Xdm::Map(_) => {
                if starting_loc == 0 {
                    args.remove(0)
                } else {
                    Xdm::EmptySequence
                }
            }
            Xdm::Sequence(v) => {
                if starting_loc < v.len() {
                    Xdm::sequence(v.clone().split_off(starting_loc))
                } else {
                    Xdm::EmptySequence
                }
            }
        })
    })
}
pub(crate) fn array_size_1() -> CompiledFunction {
    CompiledFunction::new(|_ctx, mut args| match args.remove(0) {
        Xdm::Array(v) => Ok(Xdm::Integer(v.len() as i64)),
        _ => Err(XdmError::xqtm(
            "XPTY0004",
            "array:size() called on a non-array",
        )),
    })
}
pub(crate) fn error_0() -> CompiledFunction {
    CompiledFunction::new(|_ctx, _args| Err(XdmError::xqtm("FOER0000", "error() called")))
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
pub(crate) fn boolean_compare(b1: &bool, b2: &bool) -> i8 {
    b1.cmp(b2) as i8
}

#[cfg(test)]
mod tests {
    use crate::runtime::DynamicContext;
    use crate::xdm::{Xdm, XdmResult};
    use crate::xpath_functions_31::{fn_boolean_1, fn_not_1, fn_string_length_1, xs_double_1};
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
    #[test]
    fn string_length1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let ctx: DynamicContext = static_context.new_dynamic_context();
        let args = vec![Xdm::Integer(330)];
        let result = fn_string_length_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Integer(3)));
        let args = vec![Xdm::String("😎".to_string())];
        let result = fn_string_length_1().execute(&ctx, args);
        assert_eq!(result, Ok(Xdm::Integer(1)));
        Ok(())
    }
    #[test]
    fn parse_float() -> XdmResult<()> {
        let f: f32 = "-3.4028235E38".parse()?;
        assert!(!f.is_nan());
        Ok(())
    }
}
