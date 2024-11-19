use xot::xmlname::NameStrInfo;

#[inline]
pub fn eqv(n1: &dyn NameStrInfo, n2: &dyn NameStrInfo) -> bool {
    n1.namespace() == n2.namespace() && n1.local_name() == n2.local_name()
}
