use crate::xdm::{XdmError, XdmResult};
use crate::StaticContext;
use smop_xmltree::nod::QName;
use smop_xmltree::option_ext::OptionExt;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum SequenceType {
    EmptySequence,
    Item(Item, Occurrence),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    KindTest(KindTest),
    Item,
    FunctionTest,
    MapTest,
    ArrayTest,
    AtomicOrUnion(Rc<SchemaType>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum KindTest {
    Document,
    Element(Option<QName>, Option<QName>),
    Attribute(Option<QName>, Option<QName>),
    SchemaElement(QName),
    SchemaAttribute(QName),
    PI(Option<String>),
    Comment,
    Text,
    NamespaceNode,
    AnyKind,
}

impl Display for KindTest {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KindTest::Document => write!(f, "document-node()"),
            KindTest::Element(opt_name, opt_type) => match (opt_name, opt_type) {
                (None, None) => write!(f, "element()"),
                (Some(qname), None) => write!(f, "element({})", qname),
                (Some(qname), Some(schema_type)) => {
                    write!(f, "element({}, {})", qname, schema_type)
                }
                (None, Some(schema_type)) => write!(f, "element(*, {})", schema_type),
            },
            KindTest::Attribute(opt_name, opt_type) => match (opt_name, opt_type) {
                (None, None) => write!(f, "attribute()"),
                (Some(qname), None) => write!(f, "attribute({})", qname),
                (Some(qname), Some(schema_type)) => {
                    write!(f, "attribute({}, {})", qname, schema_type)
                }
                (None, Some(schema_type)) => write!(f, "attribute(*, {})", schema_type),
            },
            KindTest::SchemaElement(name) => write!(f, "schema-element({})", name),
            KindTest::SchemaAttribute(name) => write!(f, "schema-attribute({})", name),
            KindTest::PI(opt_target) => write!(
                f,
                "processing-instruction({})",
                opt_target.as_str().unwrap_or("")
            ),
            KindTest::Comment => write!(f, "comment()"),
            KindTest::Text => write!(f, "text()"),
            KindTest::NamespaceNode => write!(f, "namespace-node()"),
            KindTest::AnyKind => write!(f, "node()"),
        }
    }
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Occurrence {
    Optional,
    ZeroOrMore,
    One,
    OneOrMore,
}

impl SequenceType {
    pub(crate) fn lub(
        ctx: &StaticContext,
        st1: &SequenceType,
        st2: &SequenceType,
    ) -> XdmResult<SequenceType> {
        Ok(match (st1, st2) {
            (SequenceType::EmptySequence, st2) => st2.clone(),
            (st1, SequenceType::EmptySequence) => st1.clone(),
            (SequenceType::Item(i1, o1), SequenceType::Item(i2, o2)) => {
                SequenceType::Item(Item::lub(ctx, i1, i2)?, o1.max(o2).clone())
            }
        })
    }
    pub(crate) fn add(
        ctx: &StaticContext,
        st1: &SequenceType,
        st2: &SequenceType,
    ) -> XdmResult<SequenceType> {
        Ok(match (st1, st2) {
            (SequenceType::EmptySequence, st2) => st2.clone(),
            (st1, SequenceType::EmptySequence) => st1.clone(),
            (SequenceType::Item(i1, o1), SequenceType::Item(i2, o2)) => {
                SequenceType::Item(Item::lub(ctx, i1, i2)?, Occurrence::add(o1, o2))
            }
        })
    }
    pub(crate) fn add_vec(ctx: &StaticContext, ts: Vec<SequenceType>) -> XdmResult<SequenceType> {
        assert!(
            ts.len() > 1,
            "add_vec should only be called with more than one element"
        );
        let mut iter = ts.iter();
        let init = iter.next().unwrap();
        iter.fold(Ok(init.clone()), |st1, st2| {
            SequenceType::add(ctx, &st1?, st2)
        })
    }
    pub(crate) fn atomize(&self, ctx: &Rc<StaticContext>) -> XdmResult<Self> {
        Ok(match self {
            SequenceType::EmptySequence => SequenceType::EmptySequence,
            SequenceType::Item(i, o) => SequenceType::Item(
                match i {
                    Item::KindTest(_) => any_atomic_type(ctx),
                    Item::Item => any_atomic_type(ctx),
                    Item::FunctionTest | Item::MapTest => {
                        return Err(XdmError::xqtm(
                            "FOTY0013",
                            "The argument to fn:data() contains a function item",
                        ))
                    }
                    Item::ArrayTest => any_atomic_type(ctx),
                    Item::AtomicOrUnion(st) => Item::AtomicOrUnion(Rc::clone(st)),
                },
                o.clone(),
            ),
        })
    }
}
fn any_atomic_type(ctx: &Rc<StaticContext>) -> Item {
    Item::AtomicOrUnion(
        ctx.schema_type(&QName::wellknown("xs:anyAtomicType"))
            .unwrap(),
    )
}
impl Display for SequenceType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SequenceType::EmptySequence => f.write_str("empty-sequence()"),
            SequenceType::Item(i, o) => write!(f, "{}{}", i, o),
        }
    }
}

impl Item {
    fn lub(_ctx: &StaticContext, i1: &Item, i2: &Item) -> XdmResult<Item> {
        Ok(match (i1, i2) {
            (Item::Item, _) | (_, Item::Item) => Item::Item,
            (Item::AtomicOrUnion(t1), Item::AtomicOrUnion(t2)) => {
                Item::AtomicOrUnion(SchemaType::lub(t1, t2))
            }
            (i1, i2) => {
                println!("lub for other Item types: {}, {}", i1, i2);
                Item::Item
            }
        })
    }
}
impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Item::Item => f.write_str("item()"),
            Item::AtomicOrUnion(schema_type) => write!(f, "{}", schema_type),
            Item::KindTest(kt) => write!(f, "{}", kt),
            &_ => todo!("Display for other Item types: {:?}", self),
            // Item::FunctionTest => {}
            // Item::MapTest => {}
            // Item::ArrayTest => {}
            // Item::ParenthesizedItemType => {}
        }
    }
}

impl Occurrence {
    fn add(o1: &Occurrence, o2: &Occurrence) -> Occurrence {
        if *o1 >= Occurrence::One && *o2 >= Occurrence::One {
            Occurrence::OneOrMore
        } else {
            o1.max(o2).clone()
        }
    }
}
impl Display for Occurrence {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Occurrence::Optional => f.write_str("?"),
            Occurrence::ZeroOrMore => f.write_str("*"),
            Occurrence::One => f.write_str(""),
            Occurrence::OneOrMore => f.write_str("+"),
        }
    }
}

// reference: https://www.w3.org/TR/xmlschema-2/
// and https://www.w3.org/TR/xmlschema-1/#Simple_Type_Definition_details

#[derive(Debug, PartialEq)]
pub struct SchemaType {
    pub(crate) name: Option<String>,
    pub(crate) ns: Option<String>,
    pub(crate) prefix: Option<String>,
    pub(crate) base_type: Option<Rc<SchemaType>>,
    pub(crate) tree: TypeTree,
}

#[derive(Debug, PartialEq)]
pub enum TypeTree {
    Simple(Variety),
    Complex,
}

#[derive(Debug, PartialEq)]
pub enum Variety {
    Atomic,
    List(Rc<SchemaType>),
    Union(Vec<Rc<SchemaType>>),
}

impl SchemaType {
    fn lub(st1: &Rc<SchemaType>, st2: &Rc<SchemaType>) -> Rc<SchemaType> {
        let succ = |st: &Rc<SchemaType>| {
            std::iter::successors(Some(st.clone()), |e| {
                e.base_type.as_ref().map(|s| s.clone())
            })
        };
        for outer in succ(st1) {
            for inner in succ(st2) {
                if inner == outer {
                    return inner.clone();
                }
            }
        }
        unreachable!()
    }
}
impl Display for SchemaType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let (Some(ref name), Some(ref prefix)) = (&self.name, &self.prefix) {
            write!(f, "{}:{}", prefix, name)
        } else if let (Some(ref name), Some(ref ns)) = (&self.name, &self.ns) {
            write!(f, "Q{{{}}}:{}", ns, name)
        } else {
            write!(f, "{:?}", self)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Literal};
    use crate::xdm::XdmResult;
    use crate::StaticContext;
    use std::rc::Rc;

    #[test]
    fn types1() -> XdmResult<()> {
        let sc: Rc<StaticContext> = Rc::new(Default::default());
        let ast = Expr::Literal(Literal::Integer(1), ());
        let type_ = ast.type_(Rc::clone(&sc));
        let res = type_?.t().0.to_string();
        assert_eq!("xs:integer", res);
        let ast = Expr::Sequence(vec![], ());
        let type_ = ast.type_(Rc::clone(&sc));
        let res = type_?.t().0.to_string();
        assert_eq!("empty-sequence()", res);
        let ast = Expr::Sequence(
            vec![
                Expr::Literal(Literal::Integer(1), ()),
                Expr::Literal(Literal::Integer(1), ()),
            ],
            (),
        );
        let type_ = ast.type_(sc);
        let res = type_?.t().0.to_string();
        assert_eq!("xs:integer+", res);
        Ok(())
    }

    #[test]
    #[ignore]
    fn types2() -> XdmResult<()> {
        let sc: Rc<StaticContext> = Rc::new(Default::default());
        let ast = sc.parse("1 * foo")?;
        let type_ = ast.type_(Rc::clone(&sc));
        let res = type_?.t().0.to_string();
        assert_eq!("xs:integer", res);
        let ast = sc.parse("foo")?;
        let type_ = ast.type_(Rc::clone(&sc));
        let res = type_?.t().0.to_string();
        assert_eq!("item()+", res);
        Ok(())
    }
}
