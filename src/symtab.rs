use std::collections::hash_map::Iter;

use fxhash::FxHashMap;

use crate::{
    expr::Expr,
    intern::{MetaInterner, MetaRef, StrRef},
    lexer::SourceLoc,
};

#[derive(Clone, Debug)]
pub enum Symbol {
    Expr(Expr),
    Value(i32),
}

#[derive(Clone, Debug)]
pub struct MetaSymbol {
    inner: Symbol,
    meta: MetaRef,
}

impl MetaSymbol {
    pub fn inner(&self) -> &Symbol {
        &self.inner
    }

    pub fn meta(&self) -> MetaRef {
        self.meta
    }
}

pub struct Symtab {
    inner: FxHashMap<StrRef, MetaSymbol>,
    hits: FxHashMap<StrRef, SourceLoc>,
    meta_interner: MetaInterner,
    meta: MetaRef,
}

impl Symtab {
    #[inline]
    pub fn new() -> Self {
        let mut meta_interner = MetaInterner::new();
        let meta = meta_interner.intern(&[]);
        Self {
            inner: FxHashMap::default(),
            hits: FxHashMap::default(),
            meta_interner,
            meta,
        }
    }

    pub fn set_meta<M: AsRef<[[StrRef; 2]]>>(&mut self, meta: M) {
        self.meta = self.meta_interner.intern(meta);
    }

    pub fn meta_interner(&self) -> &MetaInterner {
        &self.meta_interner
    }

    pub fn meta_interner_mut(&mut self) -> &mut MetaInterner {
        &mut self.meta_interner
    }

    #[inline]
    pub fn insert(&mut self, key: StrRef, value: Symbol) -> Option<MetaSymbol> {
        self.inner.insert(
            key,
            MetaSymbol {
                inner: value,
                meta: self.meta,
            },
        )
    }

    #[inline]
    pub fn insert_with_meta<M: AsRef<[[StrRef; 2]]>>(
        &mut self,
        key: StrRef,
        value: Symbol,
        meta: M,
    ) -> Option<MetaSymbol> {
        let meta = self.meta_interner.intern(meta.as_ref());
        self.inner.insert(key, MetaSymbol { inner: value, meta })
    }

    #[inline]
    pub fn touch(&mut self, key: StrRef, loc: SourceLoc) {
        self.hits.entry(key).or_insert(loc);
    }

    #[inline]
    pub fn first_reference(&self, key: StrRef) -> Option<&SourceLoc> {
        self.hits.get(&key)
    }

    #[inline]
    pub fn get(&self, key: StrRef) -> Option<&MetaSymbol> {
        self.inner.get(&key)
    }

    #[inline]
    pub fn remove(&mut self, key: StrRef) -> Option<MetaSymbol> {
        self.inner.remove(&key)
    }

    #[inline]
    pub fn references(&self) -> SymtabRefIter<'_> {
        SymtabRefIter {
            inner: self.hits.iter(),
        }
    }
}

impl<'a> IntoIterator for &'a Symtab {
    type IntoIter = SymtabIter<'a>;
    type Item = (&'a StrRef, &'a MetaSymbol);

    fn into_iter(self) -> Self::IntoIter {
        SymtabIter {
            inner: self.inner.iter(),
        }
    }
}

pub struct SymtabIter<'a> {
    inner: Iter<'a, StrRef, MetaSymbol>,
}

impl<'a> Iterator for SymtabIter<'a> {
    type Item = (&'a StrRef, &'a MetaSymbol);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

pub struct SymtabRefIter<'a> {
    inner: Iter<'a, StrRef, SourceLoc>,
}

impl<'a> Iterator for SymtabRefIter<'a> {
    type Item = (&'a StrRef, &'a SourceLoc);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}
