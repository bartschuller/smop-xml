use crate::xdm::{QName, XdmResult};
use crate::StaticContext;
use itertools::Itertools;
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
    KindTest,
    Item,
    FunctionTest,
    MapTest,
    ArrayTest,
    AtomicOrUnion(Rc<SchemaType>),
    ParenthesizedItemType,
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
        union_ok: bool,
    ) -> XdmResult<SequenceType> {
        Ok(match (st1, st2) {
            (SequenceType::EmptySequence, st2) => st2.clone(),
            (st1, SequenceType::EmptySequence) => st1.clone(),
            (SequenceType::Item(i1, o1), SequenceType::Item(i2, o2)) => {
                SequenceType::Item(Item::lub(ctx, i1, i2, union_ok)?, o1.max(o2).clone())
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
                SequenceType::Item(Item::lub(ctx, i1, i2, false)?, Occurrence::add(o1, o2))
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
    fn lub(ctx: &StaticContext, i1: &Item, i2: &Item, union_ok: bool) -> XdmResult<Item> {
        Ok(match (i1, i2) {
            (Item::Item, _) | (_, Item::Item) => Item::Item,
            (Item::AtomicOrUnion(t1), Item::AtomicOrUnion(t2)) => {
                Item::AtomicOrUnion(SchemaType::lub(t1.clone(), t2.clone(), union_ok))
            }
            (_, _) => unimplemented!("lub for other Item types"),
        })
    }
}
impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Item::Item => f.write_str("item()"),
            Item::AtomicOrUnion(q) => write!(f, "{}", q),
            &_ => todo!("Display for other Item types: {:?}", self),
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
    pub(crate) qname: Option<QName>,
    pub(crate) tree: TypeTree,
}

#[derive(Debug, PartialEq)]
pub enum TypeTree {
    Simple(Rc<SimpleType>),
    Complex,
}

#[derive(Debug, PartialEq)]
pub enum Variety {
    Atomic,
    List(Rc<SimpleType>),
    Union(Vec<Rc<SimpleType>>),
}

#[derive(Debug, PartialEq)]
pub struct SimpleType {
    pub(crate) name: Option<String>,
    pub(crate) ns: Option<String>,
    pub(crate) base_type: Option<Rc<SchemaType>>,
    // facets
    // fundamental facets
    pub(crate) variety: Variety,
}

impl SchemaType {
    fn simple_type(&self) -> Option<Rc<SimpleType>> {
        match &self.tree {
            TypeTree::Simple(st) => Some(st.clone()),
            TypeTree::Complex => None,
        }
    }
    fn lub(t1: Rc<SchemaType>, t2: Rc<SchemaType>, union_ok: bool) -> Rc<SchemaType> {
        if t1 == t2 {
            t1.clone()
        } else {
            match (&*t1, &*t2) {
                (
                    SchemaType {
                        qname: _,
                        tree: TypeTree::Simple(st1),
                    },
                    SchemaType {
                        qname: _,
                        tree: TypeTree::Simple(st2),
                    },
                ) => {
                    let simple_type = if union_ok {
                        Rc::new(SimpleType {
                            name: None,
                            ns: None,
                            base_type: None, // FIXME
                            variety: Variety::Union(vec![st1.clone(), st2.clone()]),
                        })
                    } else {
                        SimpleType::lub(st1, st2)
                    };
                    let schema_type = SchemaType {
                        qname: None,
                        tree: TypeTree::Simple(simple_type),
                    };
                    Rc::new(schema_type)
                }
                _ => todo!("SchemaType::lub for {} and {}", t1, t2),
            }
        }
    }
}
impl Display for SchemaType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(ref qname) = self.qname {
            write!(f, "{}", qname)
        } else {
            write!(f, "{:?}", self)
        }
    }
}

impl SimpleType {
    fn lub(st1: &Rc<SimpleType>, st2: &Rc<SimpleType>) -> Rc<SimpleType> {
        let succ = |st: &Rc<SimpleType>| {
            std::iter::successors(Some(st.clone()), |e| {
                e.base_type
                    .as_ref()
                    .map(|s| s.simple_type().clone())
                    .flatten()
            })
        };
        for base in succ(st1) {
            print!("{} ", base.name.as_ref().unwrap());
        }
        println!();
        for base in succ(st2) {
            print!("{} ", base.name.as_ref().unwrap());
        }
        println!();
        println!("SimpleType::lub({:?}, {:?})", st1, st2);
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
