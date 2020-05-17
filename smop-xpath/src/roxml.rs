use crate::ast::NodeTest;
use roxmltree::Node;
use std::fmt;
use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub struct AxisIter<'a, 'input: 'a> {
    node: Option<Node<'a, 'input>>,
    next: fn(&Node<'a, 'input>) -> Option<Node<'a, 'input>>,
    pub(crate) position: usize,
    // can be computed lazily only when needed
    last: Option<usize>,
}

impl<'a, 'input: 'a> Iterator for AxisIter<'a, 'input> {
    type Item = Node<'a, 'input>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.node.take();
        self.node = node.as_ref().and_then(self.next);
        node
    }
}

impl<'a, 'input: 'a> Debug for AxisIter<'a, 'input> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}

impl<'a, 'input: 'a> PartialEq for AxisIter<'a, 'input> {
    fn eq(&self, other: &Self) -> bool {
        unimplemented!()
    }
}
#[cfg(test)]
mod tests {
    use crate::roxml::AxisIter;
    use crate::xdm::{QName, XdmResult};
    use roxmltree::Document;

    #[test]
    fn iterators1() -> XdmResult<()> {
        let doc = Document::parse(r#"<d><e>1</e><e>2</e></d>"#)?;
        let root = doc.root();
        let mut d = root.children();
        let d = d.next().unwrap();
        assert_eq!(d.tag_name().name(), "d");
        let es = d.children().enumerate();

        Ok(())
    }

    fn iterators2() -> XdmResult<()> {
        let doc = Document::parse(r#"<d><e>1</e><e>2</e></d>"#)?;
        let root = doc.root();
        let children = AxisIter {
            node: None,
            next: |n| None,
            position: 0,
            last: None,
        };
        let mut d = root.children();
        let d = d.next().unwrap();
        assert_eq!(d.tag_name().name(), "d");
        let es = d.children().enumerate();

        Ok(())
    }
}
