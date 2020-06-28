mod parse;
//mod parse_bart;
use crate::option_ext::OptionExt;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::num::NonZeroU32;
use std::ops::Deref;
use std::rc::Rc;

// An implementation of https://www.w3.org/TR/xpath-datamodel-31/

/// The <http://www.w3.org/XML/1998/namespace> URI.
pub const NS_XML_URI: &str = "http://www.w3.org/XML/1998/namespace";

/// The <http://www.w3.org/2000/xmlns/> URI.
pub const NS_XMLNS_URI: &str = "http://www.w3.org/2000/xmlns/";

// https://www.w3.org/TR/xpath-datamodel-31/#qnames-and-notations
#[derive(Debug, Clone, PartialEq)]
pub struct QName {
    pub name: String,
    pub ns: Option<String>,
    pub prefix: Option<String>,
}

impl QName {
    /// Note that a prefix is only allowed when a namespace is also provided. The following panics:
    /// ```should_panic
    /// # use smop_xmltree::nod::QName;
    /// let wrong = QName::new("foo".to_string(), None, Some("wrong".to_string()));
    /// ```
    pub fn new(name: String, ns: Option<String>, prefix: Option<String>) -> Self {
        assert!(!(prefix.is_some() && ns.is_none()));
        QName { name, ns, prefix }
    }
}
impl Eq for QName {}
impl Hash for QName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ns.hash(state);
        self.name.hash(state);
        self.prefix.hash(state)
    }
}
impl Display for QName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.ns.is_some() {
            if self.prefix.is_some() {
                write!(f, "{}:{}", self.prefix.as_ref().unwrap(), self.name)
            } else {
                write!(f, "Q{{{}}}{}", self.ns.as_ref().unwrap(), self.name)
            }
        } else {
            write!(f, "{}", self.name)
        }
    }
}

type Range = std::ops::Range<usize>;

/// A short range.
///
/// Just like Range, but only for `u32` and copyable.
#[derive(Clone, Copy, Debug)]
pub struct ShortRange {
    start: u32,
    end: u32,
}

impl From<Range> for ShortRange {
    #[inline]
    fn from(range: Range) -> Self {
        // Casting to `u32` should be safe since we have a 4GiB input data limit.
        debug_assert!(range.start <= std::u32::MAX as usize);
        debug_assert!(range.end <= std::u32::MAX as usize);
        ShortRange::new(range.start as u32, range.end as u32)
    }
}

impl ShortRange {
    #[inline]
    fn new(start: u32, end: u32) -> Self {
        ShortRange { start, end }
    }

    #[inline]
    fn to_urange(self) -> Range {
        self.start as usize..self.end as usize
    }
}

#[derive(Clone, Copy, PartialEq, Debug, PartialOrd)]
pub struct Idx(NonZeroU32);

impl Idx {
    #[inline]
    pub fn new(id: u32) -> Self {
        Idx(NonZeroU32::new(id + 1).unwrap())
    }
    #[inline]
    pub fn get(&self) -> u32 {
        self.0.get() as u32 - 1
    }
    #[inline]
    pub fn get_usize(&self) -> usize {
        self.get() as usize
    }
}

impl From<u32> for Idx {
    #[inline]
    fn from(id: u32) -> Self {
        Idx::new(id)
    }
}
impl From<usize> for Idx {
    #[inline]
    fn from(id: usize) -> Self {
        Idx::new(id as u32)
    }
}

/// A namespace.
///
/// Contains URI and *prefix* pair.
#[derive(Clone, PartialEq, Debug)]
pub struct Namespace {
    name: Option<String>,
    uri: String,
}

impl Namespace {
    /// Returns namespace name/prefix.
    ///
    /// # Examples
    ///
    /// ```
    /// let doc = smop_xmltree::nod::Document::parse(
    ///     "<e xmlns:n='http://www.w3.org'/>"
    /// ).unwrap();
    ///
    /// assert_eq!(doc.root_element().namespaces()[0].name(), Some("n"));
    /// ```
    ///
    /// ```
    /// let doc = smop_xmltree::nod::Document::parse(
    ///     "<e xmlns='http://www.w3.org'/>"
    /// ).unwrap();
    ///
    /// assert_eq!(doc.root_element().namespaces()[0].name(), None);
    /// ```
    #[inline]
    pub fn name(&self) -> Option<&String> {
        self.name.as_ref()
    }

    /// Returns namespace URI.
    ///
    /// # Examples
    ///
    /// ```
    /// let doc = smop_xmltree::nod::Document::parse(
    ///     "<e xmlns:n='http://www.w3.org'/>"
    /// ).unwrap();
    ///
    /// assert_eq!(doc.root_element().namespaces()[0].uri(), "http://www.w3.org");
    /// ```
    #[inline]
    pub fn uri(&self) -> &str {
        self.uri.as_ref()
    }
}

struct Namespaces(Vec<Namespace>);

impl Namespaces {
    #[inline]
    fn push_ns(&mut self, name: Option<String>, uri: String) {
        debug_assert_ne!(name.as_str(), Some(""));
        self.0.push(Namespace { name, uri });
    }

    #[inline]
    fn exists(&self, start: usize, prefix: Option<&str>) -> bool {
        self[start..].iter().any(|ns| ns.name.as_str() == prefix)
    }
}

impl Deref for Namespaces {
    type Target = Vec<Namespace>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct Document {
    strings: Vec<String>,
    qnames: Vec<QName>,
    nodes: Vec<NodeData>,
    attrs: Vec<Attribute>,
    namespaces: Namespaces,
}

impl Document {
    pub fn root(self: &Rc<Self>) -> Node {
        self.node(Idx::new(0))
    }

    #[inline]
    fn node(self: &Rc<Self>, id: Idx) -> Node {
        Node {
            document: Rc::clone(self),
            id,
        }
    }

    /// Returns the root element of the document.
    ///
    /// Unlike `root`, will return a first element node.
    ///
    /// The root element always exists.
    ///
    /// # Examples
    ///
    /// ```
    /// let doc = smop_xmltree::nod::Document::parse("<!-- comment --><e/>").unwrap();
    /// assert!(doc.root_element().has_tag_name("e"));
    /// ```
    #[inline]
    pub fn root_element(self: &Rc<Self>) -> Node {
        // `expect` is safe, because the `Document` is guarantee to have at least one element.
        self.root()
            .first_element_child()
            .expect("XML documents must contain a root element")
    }
}

impl fmt::Debug for Document {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "document[\n  strings: {:?}\n  qnames: {:?}\n  nodes: {:?}\n]",
            self.strings, self.qnames, self.nodes
        )
    }
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Document,
    Element {
        qname: Idx,
        attributes: ShortRange,
        namespaces: ShortRange,
    },
    Attribute(),
    Text(Idx),
    PI(PI),
    Comment(),
}
#[derive(Debug, Clone)]
pub struct NodeData {
    parent: Option<Idx>,
    prev_sibling: Option<Idx>,
    last_child: Option<Idx>,
    next_subtree: Option<Idx>, // next sibling or ancestor's next sibling
    kind: NodeKind,
    range: ShortRange,
}

#[derive(PartialEq, Debug, Clone)]
pub enum NodeType {
    Document,
    Element,
    Attribute,
    Text,
    PI,
    Comment,
}

/// An attribute.
#[derive(Clone)]
pub struct Attribute {
    name: QName,
    value: Idx, // into strings
    range: ShortRange,
    value_range: ShortRange,
}

/// A processing instruction.
#[derive(Clone, Copy, PartialEq, Debug)]
#[allow(missing_docs)]
pub struct PI {
    target: Idx,        // into strings
    value: Option<Idx>, // into strings
}

#[derive(Debug)]
pub struct Node {
    document: Rc<Document>,
    id: Idx,
}
impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.document, &other.document) && self.id.eq(&other.id)
    }
}

impl Node {
    pub fn node_type(&self) -> NodeType {
        match self.d().kind {
            NodeKind::Document => NodeType::Document,
            NodeKind::Element { .. } => NodeType::Element,
            NodeKind::Attribute() => NodeType::Attribute,
            NodeKind::Text(_) => NodeType::Text,
            NodeKind::PI(_) => NodeType::PI,
            NodeKind::Comment() => NodeType::Comment,
        }
    }
    /// Checks that node is an element node.
    #[inline]
    pub fn is_element(&self) -> bool {
        self.node_type() == NodeType::Element
    }

    pub fn parent(&self) -> Option<Node> {
        self.d().parent.as_ref().map(|id| self.document.node(*id))
    }
    pub fn children(&self) -> Children {
        Children {
            first: self.first_child(),
            last: self.last_child(),
        }
    }
    #[inline]
    fn d(&self) -> &NodeData {
        self.document.nodes.get(self.id.get_usize()).unwrap()
    }
    fn first_child(&self) -> Option<Node> {
        self.last_child()
            .map(|_| self.document.node(Idx::new(self.id.get() + 1)))
    }
    fn last_child(&self) -> Option<Node> {
        self.d().last_child.map(|id| self.document.node(id))
    }
    fn previous_sibling(&self) -> Option<Node> {
        self.d().prev_sibling.map(|id| self.document.node(id))
    }
    fn next_sibling(&self) -> Option<Node> {
        self.d()
            .next_subtree
            .map(|id| self.document.node(id))
            .and_then(|n| {
                if n.d().prev_sibling == Some(self.id) {
                    Some(n)
                } else {
                    None
                }
            })
    }

    /// Returns the first element child of this node.
    pub fn first_element_child(&self) -> Option<Self> {
        self.children().find(|n| n.is_element())
    }
}

#[derive(Debug)]
pub struct Children {
    first: Option<Node>,
    last: Option<Node>,
}

impl Iterator for Children {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.first.take();
        if self.first == self.last {
            self.last = None;
        } else {
            self.first = res.as_ref().and_then(Node::next_sibling)
        }
        res
    }
}

#[cfg(test)]
mod tests {
    use crate::nod::{parse, Document, NodeType};

    #[test]
    fn parse1() -> Result<(), parse::Error> {
        let text = r#"<pre:foo></bar>"#;
        let err = Document::parse(text).unwrap_err();
        assert_eq!(err.to_string(), "an unknown namespace prefix 'pre' at 1:2");
        let text = r#"<foo><bar/></foo>"#;
        let doc = Document::parse(text)?;
        println!("\n{:?}\n", doc);
        let root = doc.root();
        assert_eq!(root.node_type(), NodeType::Document);
        assert_eq!(root.parent(), None);
        let last_child = root.last_child();
        println!("{:?}", last_child);
        assert!(last_child.is_some());
        let mut children = root.children();
        println!("{:?}", children);
        let first = children.next();
        assert!(first.is_some());
        let first = first.unwrap();
        assert_eq!(first.node_type(), NodeType::Element);
        Ok(())
    }
}
