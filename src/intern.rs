use std::{
    borrow::Borrow,
    ffi::OsStr,
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem,
    os::unix::ffi::OsStrExt,
    path::Path,
    slice, str,
};

use fxhash::FxHashSet;
use path_absolutize::Absolutize;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BytesRef(RiskySlice<'static>);

#[derive(Debug, Copy, Clone, Ord, PartialOrd)]
struct RiskySlice<'a> {
    data: *const u8,
    len: usize,
    marker: PhantomData<&'a ()>,
}

impl<'a> Borrow<[u8]> for RiskySlice<'a> {
    #[inline]
    fn borrow(&self) -> &[u8] {
        unsafe { &*slice::from_raw_parts(self.data, self.len) }
    }
}

impl<'a> PartialEq<Self> for RiskySlice<'a> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        unsafe {
            slice::from_raw_parts(self.data, self.len)
                == slice::from_raw_parts(other.data, other.len)
        }
    }
}

impl<'a> Eq for RiskySlice<'a> {}

impl<'a> Hash for RiskySlice<'a> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe { slice::from_raw_parts(self.data, self.len).hash(state) }
    }
}

#[derive(Default, Debug)]
pub struct BytesInterner {
    map: FxHashSet<RiskySlice<'static>>,
    buffers: Vec<Vec<u8>>,
}

impl BytesInterner {
    #[inline]
    pub fn new() -> Self {
        Self {
            map: FxHashSet::default(),
            buffers: vec![Vec::with_capacity(32)],
        }
    }

    pub fn intern<T: AsRef<[u8]>>(&mut self, bytes: T) -> BytesRef {
        let bytes = bytes.as_ref();
        if let Some(&slice) = self.map.get(bytes) {
            // Safety: We preserve pointer validity by chaining buffers together
            //   rather than re-allocating them
            return BytesRef(unsafe { mem::transmute::<_, RiskySlice<'static>>(slice) });
        }
        let slice = self.buffer(bytes);
        self.map.insert(slice);
        BytesRef(slice)
    }

    #[inline]
    pub fn get(&self, bytes: BytesRef) -> Option<&[u8]> {
        self.map.get(&bytes.0).map(|slice| slice.borrow())
    }

    #[inline]
    pub fn eq<T: AsRef<[u8]>>(&self, expected: T, interned: BytesRef) -> Option<bool> {
        self.get(interned).map(|s| expected.as_ref() == s)
    }

    fn buffer(&mut self, slice: &[u8]) -> RiskySlice<'static> {
        let buffer = self.buffers.last().unwrap();
        let capacity = buffer.capacity();
        if capacity < buffer.len() + slice.len() {
            let new_capacity = (capacity.max(slice.len()) + 1).next_power_of_two();
            self.buffers.push(Vec::with_capacity(new_capacity));
        }

        let buffer = self.buffers.last_mut().unwrap();
        let start = buffer.len();
        buffer.extend_from_slice(slice);
        // Safety: We preserve pointer validity by chaining buffers together
        //   rather than re-allocating them
        RiskySlice {
            data: unsafe { buffer.as_ptr().add(start) },
            len: slice.len(),
            marker: PhantomData,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MetaRef(BytesRef);

#[derive(Default, Debug)]
pub struct MetaInterner {
    inner: BytesInterner,
}

impl MetaInterner {
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: BytesInterner::new(),
        }
    }

    #[inline]
    pub fn intern<M: AsRef<[[StrRef; 2]]>>(&mut self, meta: M) -> MetaRef {
        let meta = meta.as_ref();
        let mut strings = meta.to_vec();
        strings.sort();
        let bytes = unsafe {
            // Safety: We preserve pointer validity by chaining buffers together
            //   rather than re-allocating them
            slice::from_raw_parts(
                strings.as_ptr() as *const u8,
                strings.len() * mem::size_of::<[StrRef; 2]>(),
            )
        };
        MetaRef(self.inner.intern(bytes))
    }

    #[inline]
    pub fn get(&self, meta: MetaRef) -> Option<&[[StrRef; 2]]> {
        self.inner
            .get(meta.0)
            // Safety: It *was* a slice of string refs when it came in
            .map(|b| unsafe {
                slice::from_raw_parts(
                    b.as_ptr() as *const [StrRef; 2],
                    b.len() / mem::size_of::<[StrRef; 2]>(),
                )
            })
    }

    #[inline]
    pub fn eq<M: AsRef<[[StrRef; 2]]>>(&self, expected: M, interned: MetaRef) -> Option<bool> {
        let mut strings = expected.as_ref().to_vec();
        strings.sort();
        let bytes = unsafe {
            // Safety: We preserve pointer validity by chaining buffers together
            //   rather than re-allocating them
            slice::from_raw_parts(
                strings.as_ptr() as *const u8,
                strings.len() * mem::size_of::<[StrRef; 2]>(),
            )
        };
        self.inner.eq(bytes, interned.0)
    }

    #[inline]
    pub fn eq_some<S: AsRef<[[StrRef; 2]]>>(&self, expected: S, interned: MetaRef) -> bool {
        matches!(self.eq(expected, interned), Some(true))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct StrRef(BytesRef);

#[derive(Default, Debug)]
pub struct StrInterner {
    inner: BytesInterner,
}

impl StrInterner {
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: BytesInterner::new(),
        }
    }

    #[inline]
    pub fn intern<S: AsRef<str>>(&mut self, string: S) -> StrRef {
        let string = string.as_ref();
        StrRef(self.inner.intern(string.as_bytes()))
    }

    #[inline]
    pub fn get(&self, string: StrRef) -> Option<&str> {
        self.inner
            .get(string.0)
            // Safety: It *was* a string when it came in
            .map(|b| unsafe { str::from_utf8_unchecked(b) })
    }

    #[inline]
    pub fn eq<S: AsRef<str>>(&self, expected: S, interned: StrRef) -> Option<bool> {
        self.inner.eq(expected.as_ref().as_bytes(), interned.0)
    }

    #[inline]
    pub fn eq_some<S: AsRef<str>>(&self, expected: S, interned: StrRef) -> bool {
        matches!(self.eq(expected, interned), Some(true))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct PathRef(BytesRef);

#[derive(Default, Debug)]
pub struct PathInterner {
    inner: BytesInterner,
}

impl PathInterner {
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: BytesInterner::new(),
        }
    }

    #[inline]
    pub fn intern<P: AsRef<Path>>(&mut self, path: P) -> PathRef {
        let path = path.as_ref();
        PathRef(self.inner.intern(path.as_os_str().as_bytes()))
    }

    #[inline]
    pub fn get(&self, pathref: PathRef) -> Option<&Path> {
        self.inner
            .get(pathref.0)
            .map(|b| Path::new(OsStr::from_bytes(b)))
    }

    #[inline]
    pub fn eq<P: AsRef<Path>>(&self, expected: P, interned: PathRef) -> Option<bool> {
        self.inner
            .eq(expected.as_ref().as_os_str().as_bytes(), interned.0)
    }

    #[inline]
    pub fn eq_some<P: AsRef<Path>>(&self, expected: P, interned: PathRef) -> bool {
        matches!(self.eq(expected, interned), Some(true))
    }
}

#[derive(Default, Debug)]
pub struct AbsPathInterner {
    inner: PathInterner,
}

impl AbsPathInterner {
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: PathInterner::new(),
        }
    }

    #[inline]
    pub fn intern<C: AsRef<Path>, P: AsRef<Path>>(&mut self, cwd: C, path: P) -> PathRef {
        let cwd = cwd.as_ref();
        let path = path.as_ref().absolutize_from(cwd).unwrap();
        self.inner.intern(path)
    }

    #[inline]
    pub fn get(&self, pathref: PathRef) -> Option<&Path> {
        self.inner.get(pathref)
    }

    #[inline]
    pub fn eq<P: AsRef<Path>>(&self, expected: P, interned: PathRef) -> Option<bool> {
        self.inner.eq(expected.as_ref(), interned)
    }

    #[inline]
    pub fn eq_some<P: AsRef<Path>>(&self, expected: P, interned: PathRef) -> bool {
        matches!(self.inner.eq(expected, interned), Some(true))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strs() {
        let mut int = StrInterner::new();
        let hello = int.intern("hello");

        assert_eq!("hello", int.get(hello).unwrap());

        let shoes = int.intern("shoes");

        assert_eq!("hello", int.get(hello).unwrap());
        assert_eq!("shoes", int.get(shoes).unwrap());

        let socks = int.intern("socks");

        assert_eq!("hello", int.get(hello).unwrap());
        assert_eq!("shoes", int.get(shoes).unwrap());
        assert_eq!("socks", int.get(socks).unwrap());

        let shirts = int.intern("shirts");

        assert_eq!("hello", int.get(hello).unwrap());
        assert_eq!("shoes", int.get(shoes).unwrap());
        assert_eq!("socks", int.get(socks).unwrap());
        assert_eq!("shirts", int.get(shirts).unwrap());
    }

    #[test]
    fn paths() {
        let mut int = PathInterner::new();
        let hello = int.intern("hello");

        let as_ref = AsRef::<Path>::as_ref;
        assert_eq!(as_ref("hello"), int.get(hello).unwrap());

        let shoes = int.intern("shoes");

        assert_eq!(as_ref("hello"), int.get(hello).unwrap());
        assert_eq!(as_ref("shoes"), int.get(shoes).unwrap());

        let socks = int.intern("socks");

        assert_eq!(as_ref("hello"), int.get(hello).unwrap());
        assert_eq!(as_ref("shoes"), int.get(shoes).unwrap());
        assert_eq!(as_ref("socks"), int.get(socks).unwrap());

        let shirts = int.intern("shirts");

        assert_eq!(as_ref("hello"), int.get(hello).unwrap());
        assert_eq!(as_ref("shoes"), int.get(shoes).unwrap());
        assert_eq!(as_ref("socks"), int.get(socks).unwrap());
        assert_eq!(as_ref("shirts"), int.get(shirts).unwrap());
    }

    #[test]
    fn abs_paths() {
        let mut int = AbsPathInterner::new();
        let hello = int.intern("/foo", "./hello");

        let as_ref = AsRef::<Path>::as_ref;
        assert_eq!(as_ref("/foo/hello"), int.get(hello).unwrap());

        let shoes = int.intern("/foo", "../shoes");

        assert_eq!(as_ref("/foo/hello"), int.get(hello).unwrap());
        assert_eq!(as_ref("/shoes"), int.get(shoes).unwrap());
    }

    #[test]
    fn metas() {
        let mut int = StrInterner::new();
        let hello = int.intern("hello");
        let world = int.intern("world");

        let key = int.intern("key");
        let value = int.intern("value");

        let mut mint = MetaInterner::new();
        let pairs = mint.intern(&[[hello, world], [key, value]]);
        let pairs2 = mint.intern(&[[key, value], [hello, world]]);

        assert_eq!(pairs, pairs2);
        assert!(mint.eq_some(&[[key, value], [hello, world]], pairs));
    }
}
