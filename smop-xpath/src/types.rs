use crate::xdm::XdmResult;
use crate::StaticContext;
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
    ParenthesizedItemType,
}

#[derive(Debug, PartialEq, Clone)]
pub enum KindTest {
    Document,
    Element,
    Attribute,
    SchemaElement,
    SchemaAttribute,
    PI,
    Comment,
    Text,
    NamespaceNode,
    AnyKind,
}

impl Display for KindTest {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KindTest::Document => write!(f, "document-node()"),
            KindTest::Element => write!(f, "element()"),
            KindTest::Attribute => write!(f, "attribute()"),
            KindTest::SchemaElement => write!(f, "schema-element()"),
            KindTest::SchemaAttribute => write!(f, "schema-attribute()"),
            KindTest::PI => write!(f, "processing-instruction()"),
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
            (_, _) => unimplemented!("lub for other Item types"),
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

    #[test]
    fn types1() -> XdmResult<()> {
        let sc: StaticContext = Default::default();
        let ast = Expr::Literal(Literal::Integer(1));
        let type_ = ast.type_(&sc);
        let res = type_?.to_string();
        assert_eq!("xs:integer", res);
        let ast = Expr::Sequence(vec![]);
        let type_ = ast.type_(&sc);
        let res = type_?.to_string();
        assert_eq!("empty-sequence()", res);
        let ast = Expr::Sequence(vec![
            Expr::Literal(Literal::Integer(1)),
            Expr::Literal(Literal::Integer(1)),
        ]);
        let type_ = ast.type_(&sc);
        let res = type_?.to_string();
        assert_eq!("xs:integer+", res);
        Ok(())
    }
}
