use core::ops::Deref;

use crate::{Marshal, Demarshal, BitWriter, MarshalError, BitView};

impl<'a, C, T: Marshal<C> + Clone> Marshal<C> for alloc::borrow::Cow<'a, T> {
  fn write<W: BitWriter>(&self, writer: &mut W, ctx: C) -> Result<(), MarshalError> {
    self.as_ref().write(writer, ctx)
  }
}

impl<'a, 'dm, C, T: Demarshal<'dm, C> + Clone> Demarshal<'dm, C> for alloc::borrow::Cow<'a, T> {
  fn read(view: &mut BitView<'dm>, ctx: C) -> Result<Self, MarshalError> {
    let t = T::read(view, ctx)?;
    Ok(alloc::borrow::Cow::Owned(t))
  }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub enum CowStr<'a> {
  Borrowed(&'a str),
  Owned(alloc::string::String)
}

impl<'a> AsRef<str> for CowStr<'a> {
  fn as_ref(&self) -> &str {
    self.deref()
  }
}

impl<'a> Deref for CowStr<'a> {
  type Target = str;

  fn deref(&self) -> &Self::Target {
    match self {
      CowStr::Borrowed(borrowed) => borrowed,
      CowStr::Owned(v) => v.as_str(),
    }
  }
}

impl<'a> PartialEq for CowStr<'a> {
  fn eq(&self, other: &Self) -> bool {
    self.deref() == other.deref()
  }
}

impl<'a> Eq for CowStr<'a> {}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for CowStr<'de> { 
  #[inline] 
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> 
  where 
      D: serde::Deserializer<'de>, 
  { 
    String::deserialize(deserializer).map(CowStr::Owned) 
  } 
}

#[cfg(feature = "serde")]
impl<'a> serde::Serialize for CowStr<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer
  {
    match self {
      CowStr::Borrowed(borrowed) => borrowed.serialize(serializer),
      CowStr::Owned(owned) => owned.serialize(serializer),
    }
  }
}

impl<'a> Marshal<()> for CowStr<'a> {
  fn write<W: crate::BitWriter>(&self, writer: &mut W, _ctx: ()) -> Result<(), crate::MarshalError> {
    let arr = writer.reserve_and_advance_aligned_slice(self.len() + 1)?;
    arr[0..self.len()].copy_from_slice(&self.as_bytes()[..]);
    arr[arr.len() - 1] = 0;
    Ok(())
  }
}

impl<'dm> Demarshal<'dm, ()> for CowStr<'dm> {
  fn read(view: &mut crate::BitView<'dm>, _ctx: ()) -> Result<Self, crate::MarshalError> {
    let buf = view.take_until(0u8)?;
    Ok(CowStr::Borrowed(unsafe { core::str::from_utf8_unchecked(buf) }))
  }
}
