pub mod parse;
use crate::option_ext::OptionExt;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::num::NonZeroU32;
use std::ops::Deref;
use std::rc::Rc;

// An implementation of https://www.w3.org/TR/xpath-datamodel-31/

/// The <http://www.w3.org/XML/1998/namespace> URI.
pub const NS_XML_URI: &str = "http://www.w3.org/XML/1998/namespace";

/// The <http://www.w3.org/2000/xmlns/> URI.
pub const NS_XMLNS_URI: &str = "http://www.w3.org/2000/xmlns/";

// https://www.w3.org/TR/xpath-datamodel-31/#qnames-and-notations
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
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
    pub fn wellknown(s: &str) -> Self {
        let colon = s.find(':').unwrap();
        let prefix = &s[..colon];
        let name = &s[colon + 1..];
        let ns = match prefix {
            "xs" => "http://www.w3.org/2001/XMLSchema",
            "fn" => "http://www.w3.org/2005/xpath-functions",
            &_ => panic!("not so well known prefix: {}", prefix),
        };
        QName {
            name: name.to_string(),
            ns: Some(ns.to_string()),
            prefix: Some(prefix.to_string()),
        }
    }
    #[inline]
    pub fn eqv(&self, other: &QName) -> bool {
        self.ns == other.ns && self.name == other.name
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

impl From<&QName> for QName {
    #[inline]
    fn from(v: &QName) -> Self {
        v.clone()
    }
}

impl From<&str> for QName {
    #[inline]
    fn from(v: &str) -> Self {
        QName::new(v.to_string(), None, None)
    }
}

impl From<(&str, &str)> for QName {
    #[inline]
    fn from(v: (&str, &str)) -> Self {
        QName::new(v.1.to_string(), Some(v.0.to_string()), None)
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

#[derive(Clone, Copy, PartialEq, Debug, PartialOrd, Ord, Eq)]
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
    pub fn name(&self) -> Option<&str> {
        self.name.as_str()
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
            "document[\n  strings: {:?}\n  qnames: {:?}\n  nodes: {:?}\n  namespaces: {:?}\n]",
            self.strings, self.qnames, self.nodes, "TODO"
        )
    }
}

#[derive(Debug, Clone)]
pub enum NodeType {
    Document,
    Element {
        qname: Idx,
        num_attributes: u32,
        namespaces: ShortRange,
    },
    Attribute {
        qname: Idx,
        value: Idx, // into strings
    },
    Text(Idx),
    PI(PI),
    Comment(Idx),
}

impl NodeType {
    #[inline]
    fn node_kind(&self) -> NodeKind {
        match self {
            NodeType::Document => NodeKind::Document,
            NodeType::Element { .. } => NodeKind::Element,
            NodeType::Attribute { .. } => NodeKind::Attribute,
            NodeType::Text(_) => NodeKind::Text,
            NodeType::PI(_) => NodeKind::PI,
            NodeType::Comment(_) => NodeKind::Comment,
        }
    }
    #[inline]
    pub fn is_element(&self) -> bool {
        self.node_kind() == NodeKind::Element
    }
    #[inline]
    pub fn is_attribute(&self) -> bool {
        self.node_kind() == NodeKind::Attribute
    }
}

#[derive(Debug, Clone)]
pub struct NodeData {
    parent: Option<Idx>,
    prev_sibling: Option<Idx>,
    last_child: Option<Idx>,
    next_subtree: Option<Idx>, // next sibling or ancestor's next sibling
    node_type: NodeType,
    range: ShortRange,
}

#[derive(PartialEq, Debug, Clone)]
pub enum NodeKind {
    Document,
    Element,
    Attribute,
    Text,
    PI,
    Comment,
}

/// A processing instruction.
#[derive(Clone, Copy, PartialEq, Debug)]
#[allow(missing_docs)]
pub struct PI {
    target: Idx,        // into strings
    value: Option<Idx>, // into strings
}

#[derive(Clone)]
pub struct Node {
    document: Rc<Document>,
    id: Idx,
}
impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "node: {} {}",
            Rc::as_ptr(&self.document) as i64,
            self.id.0
        )
    }
}
impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.document, &other.document) && self.id.eq(&other.id)
    }
}
impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        let res = Rc::as_ptr(&self.document).cmp(&Rc::as_ptr(&other.document));
        if res == Ordering::Equal {
            self.id.cmp(&other.id)
        } else {
            res
        }
    }
}
impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Eq for Node {}

impl Node {
    /// Return the `Rc<Document>` for this node
    pub fn document(&self) -> Rc<Document> {
        Rc::clone(&self.document)
    }

    /// Checks that node is an element node.
    #[inline]
    pub fn is_element(&self) -> bool {
        self.node_kind() == NodeKind::Element
    }

    /// Returns an iterator over this node and its descendants.
    #[inline]
    pub fn descendants_or_self(&self) -> Descendants {
        Descendants::new(self)
    }

    #[inline]
    fn d(&self) -> &NodeData {
        self.document.nodes.get(self.id.get_usize()).unwrap()
    }
    #[inline]
    fn num_attributes(&self) -> u32 {
        match self.d().node_type {
            NodeType::Document => 0,
            NodeType::Element { num_attributes, .. } => num_attributes,
            NodeType::Attribute { .. } => 0,
            NodeType::Text(_) => 0,
            NodeType::PI(_) => 0,
            NodeType::Comment(_) => 0,
        }
    }
    fn first_child(&self) -> Option<Node> {
        self.last_child().map(|_| {
            self.document
                .node(Idx::new(self.id.get() + self.num_attributes() + 1))
        })
    }
    fn last_child(&self) -> Option<Node> {
        self.d().last_child.map(|id| self.document.node(id))
    }
    pub fn previous_sibling(&self) -> Option<Node> {
        self.d().prev_sibling.map(|id| self.document.node(id))
    }
    pub fn next_sibling(&self) -> Option<Node> {
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

    /// Checks that node has a specified tag name.
    ///
    /// # Examples
    ///
    /// ```
    /// let doc = smop_xmltree::nod::Document::parse("<e xmlns='http://www.w3.org'/>").unwrap();
    ///
    /// assert!(doc.root_element().has_tag_name("e"));
    /// assert!(doc.root_element().has_tag_name(("http://www.w3.org", "e")));
    ///
    /// assert!(!doc.root_element().has_tag_name("b"));
    /// assert!(!doc.root_element().has_tag_name(("http://www.w4.org", "e")));
    /// ```
    pub fn has_tag_name<N>(&self, name: N) -> bool
    where
        N: Into<QName>,
    {
        let name = name.into();

        match self.d().node_type {
            NodeType::Element { ref qname, .. } => {
                let qname = &self.document.qnames[qname.get_usize()];
                match name.ns {
                    Some(_) => qname == &name,
                    None => qname.name == name.name,
                }
            }
            _ => false,
        }
    }

    fn first_attribute(&self) -> Option<Node> {
        if self.num_attributes() == 0 {
            None
        } else {
            Some(self.document.node(Idx::new(self.id.get() + 1)))
        }
    }
    fn last_attribute(&self) -> Option<Node> {
        self.first_attribute().map(|node| {
            self.document
                .node(Idx::new(node.id.get() + self.num_attributes()))
        })
    }

    /// Returns element's namespaces.
    ///
    /// # Examples
    ///
    /// ```
    /// let doc = smop_xmltree::nod::Document::parse(
    ///     "<e xmlns:n='http://www.w3.org'/>"
    /// ).unwrap();
    ///
    /// assert_eq!(doc.root_element().namespaces().len(), 1);
    /// ```
    #[inline]
    pub fn namespaces(&self) -> &[Namespace] {
        match self.d().node_type {
            NodeType::Element { ref namespaces, .. } => {
                &self.document.namespaces[namespaces.to_urange()]
            }
            _ => &[],
        }
    }

    pub fn deep_equal(&self, other: &Node) -> bool {
        let kind = self.node_kind();
        if kind != other.node_kind() {
            return false;
        }
        match kind {
            NodeKind::Document => {
                let element_or_text = |n: &Node| match n.node_kind() {
                    NodeKind::Element | NodeKind::Text => true,
                    _ => false,
                };
                let v1: Vec<Node> = self.children().filter(element_or_text).collect();
                let v2: Vec<Node> = other.children().filter(element_or_text).collect();
                if v1.len() != v2.len() {
                    return false;
                }
                v1.iter().zip(v2.iter()).all(|(n1, n2)| n1.deep_equal(n2))
            }
            NodeKind::Element => {
                if self.node_name() != other.node_name()
                    || self.num_attributes() != other.num_attributes()
                {
                    return false;
                }
                let attrs: Vec<Node> = self.attributes().collect();
                other
                    .attributes()
                    .all(|a1| attrs.iter().find(|a2| a1.deep_equal(a2)).is_some())
            }
            NodeKind::Attribute => {
                self.node_name() == other.node_name() && self.string_value() == other.string_value()
            }
            NodeKind::Text | NodeKind::Comment => self.string_value() == other.string_value(),
            NodeKind::PI => {
                self.node_name() == other.node_name() && self.string_value() == other.string_value()
            }
        }
    }

    // XDM
    /// The attributes accessor returns the attributes of a node as an iterator containing zero or more Attribute Nodes.
    /// The order of Attribute Nodes is stable but implementation dependent.
    ///
    /// # Examples
    ///
    /// ```
    /// let doc = smop_xmltree::nod::Document::parse(
    ///     "<e xmlns:n='http://www.w3.org' a='b' n:a='c'/>"
    /// ).unwrap();
    ///
    /// assert_eq!(doc.root_element().attributes().count(), 2);
    /// ```
    pub fn attributes(&self) -> Children {
        Children {
            first: self.first_attribute(),
            last: self.last_attribute(),
        }
    }

    // base_uri

    /// The children accessor returns the children of a node as an iterator containing zero or more nodes.
    pub fn children(&self) -> Children {
        Children {
            first: self.first_child(),
            last: self.last_child(),
        }
    }

    // document_uri
    // is_id
    // is_idrefs
    // namespace_nodes
    // nilled

    /// The node_kind accessor returns an enum identifying the kind of node. It will be one of the following, depending on the kind of node: “attribute”, “comment”, “document”, “element”, “processing-instruction”, or “text”.
    #[inline]
    pub fn node_kind(&self) -> NodeKind {
        self.d().node_type.node_kind()
    }

    /// The node_name accessor returns the name of the node
    pub fn node_name(&self) -> Option<QName> {
        match self.d().node_type {
            NodeType::Element { ref qname, .. } | NodeType::Attribute { ref qname, .. } => {
                Some(self.document.qnames[qname.get_usize()].clone())
            }
            NodeType::PI(ref pi) => Some(QName::new(
                self.document.strings[pi.target.get_usize()].clone(),
                None,
                None,
            )),
            _ => None,
        }
    }

    /// The parent accessor returns the parent of a node
    pub fn parent(&self) -> Option<Node> {
        self.d().parent.as_ref().map(|id| self.document.node(*id))
    }

    /// The string_value accessor returns the string value of a node.
    ///
    /// # Examples
    ///
    /// ```
    /// let doc = smop_xmltree::nod::Document::parse(
    ///     "<r><e>Hello</e> <e><i>World</i></e><!-- nope --></r>"
    /// ).unwrap();
    ///
    /// assert_eq!(doc.root().string_value().as_str(), "Hello World");
    /// ```
    pub fn string_value(&self) -> String {
        match self.d().node_type {
            NodeType::Document | NodeType::Element { .. } => {
                let mut s = String::new();
                let mut it = self.descendants_or_self();
                it.next();
                for n in it {
                    match n.d().node_type {
                        NodeType::Text(idx) => {
                            s.push_str(self.document.strings[idx.get_usize()].as_str())
                        }
                        _ => {}
                    }
                }
                s
            }
            NodeType::Attribute { value, .. } => self.document.strings[value.get_usize()].clone(),
            NodeType::Text(idx) | NodeType::Comment(idx) => {
                self.document.strings[idx.get_usize()].clone()
            }
            NodeType::PI(pi) => pi.value.map_or(String::new(), |idx| {
                self.document.strings[idx.get_usize()].clone()
            }),
        }
    }
    // type_name
    // typed_value
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

/// Iterator over a node and its descendants.
#[derive(Clone)]
pub struct Descendants {
    doc: Rc<Document>,
    current: Idx,
    until: Idx,
}

impl Descendants {
    #[inline]
    fn new(start: &Node) -> Self {
        let until = if start.node_kind() == NodeKind::Attribute {
            Idx::new(start.id.get() + 1)
        } else {
            start.d().next_subtree.unwrap_or_else(|| {
                let mut unt = start.document.nodes.len();
                while start.document.nodes[unt - 1].node_type.is_attribute() {
                    unt -= 1;
                }
                Idx::from(unt)
            })
        };

        Self {
            doc: Rc::clone(&start.document),
            current: start.id,
            until,
        }
    }
}

impl Iterator for Descendants {
    type Item = Node;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let next = if self.current == self.until {
            None
        } else {
            Some(self.doc.node(self.current))
        };

        if next.is_some() {
            let mut new_idx = self.current.get_usize() + 1;
            while new_idx != self.until.get_usize()
                && self.doc.nodes[new_idx].node_type.is_attribute()
            {
                new_idx += 1;
            }
            self.current = Idx::from(new_idx);
        }
        next
    }
}

#[cfg(test)]
mod tests {
    use crate::nod::{parse, Document, NodeKind};

    #[test]
    fn parse1() -> Result<(), parse::Error> {
        let text = r#"<pre:foo></bar>"#;
        let err = Document::parse(text).unwrap_err();
        assert_eq!(err.to_string(), "an unknown namespace prefix 'pre' at 1:2");
        let text = r#"<foo><bar/></foo>"#;
        let doc = Document::parse(text)?;
        println!("\n{:?}\n", doc);
        let root = doc.root();
        assert_eq!(root.node_kind(), NodeKind::Document);
        assert_eq!(root.parent(), None);
        let last_child = root.last_child();
        println!("{:?}", last_child);
        assert!(last_child.is_some());
        let mut children = root.children();
        println!("{:?}", children);
        let first = children.next();
        assert!(first.is_some());
        let first = first.unwrap();
        assert_eq!(first.node_kind(), NodeKind::Element);
        Ok(())
    }
}
