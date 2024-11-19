use crate::ast::{CombineOp, Comp};
use crate::types::{Item, KindTest, Occurrence, SequenceType};
use crate::xpath_functions_31::{
    boolean_compare, decimal_compare, double_compare, integer_compare, string_compare,
};
use crate::StaticContext;
use itertools::Itertools;
use num_traits::cast::FromPrimitive;
use rust_decimal::prelude::{ToPrimitive, Zero};
use rust_decimal::Decimal;
use std::borrow::Borrow;
use std::collections::{BTreeSet, HashMap};
use std::error::Error;
use std::f64::NAN;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
use std::result::Result;
use xot::{Node, ValueType};
use xot::xmlname::{NameStrInfo, OwnedName};

#[derive(Debug, Clone, PartialEq)]
pub enum Xdm {
    EmptySequence,
    String(String),
    Boolean(bool),
    Decimal(Decimal),
    Integer(i64),
    Double(f64),
    Float(f32),
    Node(Node),
    Array(Vec<Xdm>),
    Map(HashMap<Xdm, Xdm>),
    Sequence(Vec<Xdm>),
}

pub type XdmResult<T> = Result<T, XdmError>;

#[derive(Debug, Clone, PartialEq)]
pub struct XdmError {
    pub code: OwnedName,
    pub message: String,
}

static ERR_NS: &str = "http://www.w3.org/2005/xqt-errors";
static ERR: &str = "err";

impl XdmError {
    pub fn xqtm<S1: Into<String>, S2: Into<String>>(code: S1, msg: S2) -> XdmError {
        XdmError {
            code: OwnedName::new(code.into(), ERR_NS.to_string(), ERR.to_string()),
            message: msg.into(),
        }
    }
}
impl Error for XdmError {}

impl From<xot::Error> for XdmError {
    fn from(re: xot::Error) -> Self {
        XdmError::xqtm("FODC0006", re.to_string())
    }
}
impl From<std::num::ParseFloatError> for XdmError {
    fn from(pfe: std::num::ParseFloatError) -> Self {
        XdmError::xqtm("FORG0001", pfe.to_string())
    }
}
impl Display for XdmError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.code.full_name(), self.message)
    }
}
impl Xdm {
    pub fn sequence(mut v: Vec<Self>) -> Self {
        match v.len() {
            0 => Xdm::EmptySequence,
            1 => v.remove(0),
            _ => Xdm::Sequence(v),
        }
    }
    pub fn flatten(v: Vec<Self>) -> Self {
        let mut res: Vec<Self> = Vec::new();

        fn add(dest: &mut Vec<Xdm>, v: Vec<Xdm>) {
            for x in v {
                match x {
                    Xdm::EmptySequence => {}
                    Xdm::Sequence(inner) => add(dest, inner),
                    _ => dest.push(x),
                }
            }
        }

        add(&mut res, v);
        Xdm::sequence(res)
    }
    pub fn atomize(self, sc: &StaticContext) -> XdmResult<Self> {
        match self {
            Xdm::EmptySequence
            | Xdm::String(_)
            | Xdm::Boolean(_)
            | Xdm::Decimal(_)
            | Xdm::Integer(_)
            | Xdm::Double(_)
            | Xdm::Float(_) => Ok(self),
            Xdm::Node(ns) => Ok(Xdm::String(sc.xot.string_value(ns))), // FIXME
            Xdm::Array(v) => {
                let res: XdmResult<Vec<_>> = v.into_iter().map(|x| x.atomize(sc)).collect();
                Ok(Xdm::flatten(res?))
            }
            Xdm::Map(_) => Err(XdmError::xqtm(
                "FOTY0013",
                "The argument to fn:data() contains a map.",
            )),
            Xdm::Sequence(v) => {
                let res: XdmResult<Vec<_>> = v.into_iter().map(|x| x.atomize(sc)).collect();
                Ok(Xdm::flatten(res?))
            }
        }
    }
    pub fn boolean(&self) -> XdmResult<bool> {
        match self {
            Xdm::String(s) => Ok(!s.is_empty()),
            Xdm::Boolean(b) => Ok(*b),
            Xdm::Decimal(d) => Ok(!d.is_zero()),
            Xdm::Integer(i) => Ok(*i != 0_i64),
            Xdm::Double(d) => Ok(!(d.is_nan() || d.is_zero())),
            Xdm::Float(f) => Ok(!(f.is_nan() || f.is_zero())),
            Xdm::Node(_) => Ok(true),
            Xdm::Array(_) => Err(XdmError::xqtm("FORG0006", "boolean value of array")),
            Xdm::Map(_) => Err(XdmError::xqtm("FORG0006", "boolean value of map")),
            Xdm::EmptySequence => Ok(false),
            Xdm::Sequence(v) => match v.first().unwrap() {
                Xdm::Node(_) => Ok(true),
                _ => Err(XdmError::xqtm(
                    "FORG0006",
                    "boolean of sequence of non-node",
                )),
            },
        }
    }
    pub fn count(&self) -> usize {
        match self {
            Xdm::EmptySequence => 0,
            Xdm::Sequence(v) => v.len(),
            _ => 1,
        }
    }
    pub fn integer(&self, sc: &StaticContext) -> XdmResult<i64> {
        match self.clone().atomize(sc)? {
            Xdm::Decimal(d) => Ok(d.to_i64().ok_or(XdmError::xqtm(
                "FOCA0003",
                "Input value too large for integer",
            ))?),
            Xdm::Integer(i) => Ok(i),
            Xdm::Double(d) => Ok(d as i64),
            Xdm::String(s) => s.parse::<i64>().map_err(|_e| {
                XdmError::xqtm(
                    "FORG0001",
                    format!("Can't cast string '{}' to an integer", s),
                )
            }),
            Xdm::Sequence(_) => Err(XdmError::xqtm(
                "XPTY0004",
                "Expected an integer, got a sequence",
            )),
            _ => todo!("finish integer conversion"),
        }
    }
    pub fn decimal(&self) -> XdmResult<Decimal> {
        match self {
            Xdm::Decimal(d) => Ok(*d),
            Xdm::Integer(i) => Ok(Decimal::new(*i, 0)),
            Xdm::Double(d) => {
                Ok(Decimal::from_f64(*d)
                    .ok_or(XdmError::xqtm("FOCA0002", "Invalid lexical value"))?)
            }
            _ => todo!("finish decimal conversion"),
        }
    }
    pub fn double(&self, sc: &StaticContext) -> XdmResult<f64> {
        match self {
            Xdm::Decimal(d) => Ok(d.to_f64().unwrap_or(NAN)),
            Xdm::Integer(i) => Ok(*i as f64),
            Xdm::Double(d) => Ok(*d),
            Xdm::Float(f) => Ok(f.to_f64().unwrap_or(NAN)),
            Xdm::String(s) => Ok(s.parse::<f64>().unwrap_or(NAN)),
            Xdm::Node(_) => Ok(self.string(sc)?.parse::<f64>().unwrap_or(NAN)),
            _ => {
                println!("xs:double({:?})", self);
                todo!("finish double conversion")
            }
        }
    }
    pub fn float(&self, sc: &StaticContext) -> XdmResult<f32> {
        match self {
            Xdm::Decimal(d) => Ok(d.to_f32().unwrap_or(f32::NAN)),
            Xdm::Integer(i) => Ok(*i as f32),
            Xdm::Double(d) => Ok(d.to_f32().unwrap_or(f32::NAN)),
            Xdm::Float(f) => Ok(*f),
            Xdm::String(s) => Ok(s.parse::<f32>().unwrap_or(f32::NAN)),
            Xdm::Node(_) => Ok(self.string(sc)?.parse::<f32>().unwrap_or(f32::NAN)),
            _ => {
                println!("xs:float({:?})", self);
                todo!("finish float conversion")
            }
        }
    }
    pub fn string(&self, sc: &StaticContext) -> XdmResult<String> {
        match self {
            Xdm::String(s) => Ok(s.clone()),
            Xdm::Boolean(b) => Ok(if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }),
            Xdm::Decimal(d) => {
                if d.is_zero() {
                    Ok("0".to_string())
                } else {
                    Ok(d.to_string())
                }
            }
            Xdm::Integer(i) => Ok(i.to_string()),
            Xdm::Double(d) => {
                if d.abs() >= 0.000001 && d.abs() < 1000000.0 {
                    Ok(d.to_string())
                } else if d.is_zero() {
                    Ok((if d.is_sign_positive() { "0" } else { "-0" }).to_string())
                } else if d.is_infinite() {
                    Ok((if d.is_sign_positive() { "INF" } else { "-INF" }).to_string())
                } else {
                    Ok(format!("{:E}", d))
                }
            }
            Xdm::Float(f) => {
                if f.abs() >= 0.000001 && f.abs() < 1000000.0 {
                    Ok(f.to_string())
                } else if f.is_zero() {
                    Ok((if f.is_sign_positive() { "0" } else { "-0" }).to_string())
                } else if f.is_infinite() {
                    Ok((if f.is_sign_positive() { "INF" } else { "-INF" }).to_string())
                } else {
                    Ok(format!("{:E}", f))
                }
            }
            Xdm::Node(n) => Ok(sc.xot.string_value(n.clone())),
            Xdm::Array(_) => Err(XdmError::xqtm(
                "FOTY0014",
                "can't convert an array to a string",
            )),
            Xdm::Map(_) => Err(XdmError::xqtm(
                "FOTY0014",
                "can't convert a map to a string",
            )),
            Xdm::EmptySequence => Ok("".to_string()),
            Xdm::Sequence(_v) => Err(XdmError::xqtm(
                "XPTY0004",
                "can't convert a sequence to a string",
            )),
        }
    }
    pub fn string_joined(&self, sc: &StaticContext) -> XdmResult<String> {
        self.string(sc).or_else(|error| {
            if let Xdm::Sequence(v) = self {
                Ok(v.iter().map(|x| x.string(sc).unwrap()).join(" "))
            } else {
                Err(error)
            }
        })
    }
    pub fn dynamic_type(&self, ctx: &StaticContext) -> XdmResult<SequenceType> {
        fn simple(ctx: &StaticContext, xs: &str) -> XdmResult<SequenceType> {
            Ok(SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&ctx.wellknown(xs))?),
                Occurrence::One,
            ))
        }
        match self {
            Xdm::String(_) => simple(ctx, "xs:string"),
            Xdm::Boolean(_) => simple(ctx, "xs:boolean"),
            Xdm::Decimal(_) => simple(ctx, "xs:decimal"),
            Xdm::Integer(_) => simple(ctx, "xs:integer"),
            Xdm::Double(_) => simple(ctx, "xs:double"),
            Xdm::Float(_) => simple(ctx, "xs:float"),
            Xdm::Node(n) => Ok(SequenceType::Item(
                Item::KindTest(match ctx.xot.value_type(n.clone()) {
                    ValueType::Document => KindTest::Document,
                    ValueType::Element => KindTest::Element(ctx.xot.node_name_ref(n.clone())?.map(|r| r.to_owned()), None),
                    ValueType::Attribute => KindTest::Attribute(ctx.xot.node_name_ref(n.clone())?.map(|r| r.to_owned()), None),
                    ValueType::Text => KindTest::Text,
                    ValueType::ProcessingInstruction => KindTest::PI(ctx.xot.node_name_ref(n.clone())?.map(|r| r.local_name().to_string())),
                    ValueType::Comment => KindTest::Comment,
                    ValueType::Namespace => KindTest::NamespaceNode,
                }),
                Occurrence::One,
            )),
            Xdm::Array(v) => Ok(SequenceType::Item(
                Item::ArrayTest(None),
                Occurrence::from(v.len()),
            )),
            Xdm::Map(_) => unimplemented!(),
            Xdm::EmptySequence => Ok(SequenceType::EmptySequence),
            Xdm::Sequence(_v) => Ok(SequenceType::Item(Item::Item, Occurrence::OneOrMore)),
        }
    }
    pub fn xpath_compare(&self, other: &Xdm, vc: Comp, sc: &StaticContext) -> XdmResult<Xdm> {
        match (self, other) {
            (Xdm::EmptySequence, _) | (_, Xdm::EmptySequence) => return Ok(Xdm::EmptySequence),
            (Xdm::Sequence(_), _) | (_, Xdm::Sequence(_)) => {
                return Err(XdmError::xqtm(
                    "XPTY0004",
                    "value comparison argument is a sequence",
                ))
            }
            _ => {}
        }
        Ok(Xdm::Boolean(match (self, other) {
            (Xdm::String(s1), x2) => {
                vc.comparison_true(string_compare(s1.as_str(), x2.string(sc)?.as_str()))
            }
            (x1, Xdm::String(s2)) => {
                vc.comparison_true(string_compare(x1.string(sc)?.as_str(), s2.as_str()))
            }
            (Xdm::Double(d1), x2) => {
                !d1.is_nan()
                    && !x2.is_nan()
                    && vc.comparison_true(double_compare(d1, x2.double(sc)?.borrow()))
            }
            (x1, Xdm::Double(d2)) => {
                !d2.is_nan()
                    && !x1.is_nan()
                    && vc.comparison_true(double_compare(x1.double(sc)?.borrow(), d2))
            }
            (Xdm::Decimal(d1), x2) => {
                vc.comparison_true(decimal_compare(d1, x2.decimal()?.borrow()))
            }
            (x1, Xdm::Decimal(d2)) => {
                vc.comparison_true(decimal_compare(x1.decimal()?.borrow(), d2))
            }
            (Xdm::Integer(i1), x2) => {
                vc.comparison_true(integer_compare(i1, x2.integer(sc)?.borrow()))
            }
            (x1, Xdm::Integer(i2)) => {
                vc.comparison_true(integer_compare(x1.integer(sc)?.borrow(), i2))
            }
            (Xdm::Boolean(b1), x2) => {
                vc.comparison_true(boolean_compare(b1, x2.boolean()?.borrow()))
            }
            (x1, Xdm::Boolean(b2)) => {
                vc.comparison_true(boolean_compare(x1.boolean()?.borrow(), b2))
            }
            (a1, a2) => unimplemented!("{:?} {} {:?}", a1, vc, a2),
        }))
    }
    pub(crate) fn combine(self, other: Xdm, op: CombineOp) -> XdmResult<Xdm> {
        unimplemented!();
        // let get_node = |x: Xdm| {
        //     if let Xdm::Node(node) = x {
        //         Ok(node)
        //     } else {
        //         Err(XdmError::xqtm(
        //             "XPTY0004",
        //             "non-node found in combine operation",
        //         ))
        //     }
        // };
        // let left: XdmResult<Vec<Node>> = self.into_iter().map(get_node).collect();
        // let left = BTreeSet::from_iter(left?);
        // let right: XdmResult<Vec<Node>> = other.into_iter().map(get_node).collect();
        // let right = BTreeSet::from_iter(right?);
        // Ok(Xdm::flatten(match op {
        //     CombineOp::Union => left.union(&right).map(|n| Xdm::Node(n.clone())).collect(),
        //     CombineOp::Intersect => left
        //         .intersection(&right)
        //         .map(|n| Xdm::Node(n.clone()))
        //         .collect(),
        //     CombineOp::Except => left
        //         .difference(&right)
        //         .map(|n| Xdm::Node(n.clone()))
        //         .collect(),
        // }))
    }
    #[inline]
    pub(crate) fn is_nan(&self) -> bool {
        matches!(self,
            Xdm::Double(d) if d.is_nan())
    }
    pub fn deep_equal(&self, other: &Xdm, sc: &StaticContext) -> bool {
        match self {
            Xdm::EmptySequence => matches!(other, Xdm::EmptySequence),
            Xdm::String(_)
            | Xdm::Boolean(_)
            | Xdm::Decimal(_)
            | Xdm::Integer(_)
            | Xdm::Double(_)
            | Xdm::Float(_) => {
                self.xpath_compare(other, Comp::EQ, sc)
                    .map_or(false, |x| x.boolean().unwrap_or(false))
                    || (self.is_nan() && other.is_nan())
            }
            Xdm::Node(n1) => match other {
                Xdm::Node(n2) => sc.xot.deep_equal(n1.clone(), n2.clone()),
                _ => false,
            },
            Xdm::Array(_) => unimplemented!(),
            Xdm::Map(_) => unimplemented!(),
            Xdm::Sequence(v1) => match other {
                Xdm::Sequence(v2) if v1.len() == v2.len() => {
                    v1.iter().zip(v2.iter()).all(|(a, b)| a.deep_equal(b, sc))
                }
                _ => false,
            },
        }
    }
}

impl Hash for Xdm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Xdm::String(s) => s.hash(state),
            Xdm::Boolean(b) => b.hash(state),
            Xdm::Decimal(d) => d.hash(state),
            Xdm::Integer(i) => i.hash(state),
            Xdm::Double(_) => panic!("attempt to use a double as a hash key"),
            _ => panic!("attempt to use non-atomic value as a hash key"),
        }
    }
}

impl Eq for Xdm {}

impl IntoIterator for Xdm {
    type Item = Xdm;
    type IntoIter = Box<dyn Iterator<Item = Xdm>>;
    fn into_iter(self) -> Self::IntoIter {
        match self {
            Xdm::String(_)
            | Xdm::Boolean(_)
            | Xdm::Decimal(_)
            | Xdm::Integer(_)
            | Xdm::Double(_)
            | Xdm::Node(_) => Box::new(std::iter::once(self)),

            // Xdm::Array(_) => {}
            // Xdm::Map(_) => {}
            Xdm::EmptySequence => Box::new(std::iter::empty()),
            Xdm::Sequence(v) => Box::new(v.into_iter()),
            _ => todo!("IntoIterator for Xdm: {:?}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use xot::xmlname::{NameStrInfo, OwnedName};
    use crate::xdm::Xdm;

    #[test]
    fn qname1() {
        let qname1 = OwnedName::new("local".to_string(), String::new(), String::new());
        let qname2 = OwnedName::new("local".to_string(), String::new(), String::new());
        let qname3 = OwnedName::new(
            "local".to_string(),
            "http://example.com/".to_string(),
        String::new(),
        );
        let qname4 = OwnedName::new(
            "local".to_string(),
            "http://example.com/".to_string(),
            "ex".to_string(),
        );
        let _qname5 = OwnedName::new(
            "local".to_string(),
            "http://example.com/".to_string(),
            "ex2".to_string(),
        );
        let qname6 = OwnedName::new("other".to_string(), String::new(), String::new());
        assert_ne!(qname1, qname2);
        assert_ne!(qname2, qname3);
        assert_ne!(qname1, qname6);

        //assert_eq!(qname3, qname4);
        //assert_eq!(qname4, qname5);
        //assert_eq!(qname5, qname3);

        assert_eq!(format!("{}", qname1.full_name()), "local");
        assert_eq!(format!("{}", qname2.full_name()), "Q{}local");
        assert_eq!(format!("{}", qname3.full_name()), "Q{http://example.com/}local");
        assert_eq!(format!("{}", qname4.full_name()), "ex:local");
    }

    #[test]
    fn double_to_string() {
        let sc = crate::xdm::StaticContext::default();
        let xdm = Xdm::Double(65535032e2);
        assert_eq!(xdm.string(&sc).unwrap(), "6.5535032E9")
    }

    #[test]
    fn deep_equal1() {
        let sc = crate::xdm::StaticContext::default();
        assert!(Xdm::Double(f64::NAN).deep_equal(&Xdm::Double(f64::NAN), &sc));
        assert!(Xdm::EmptySequence.deep_equal(&Xdm::EmptySequence, &sc));
    }
}
