#![cfg_attr(not(feature="std"), no_std)]

#![doc = include_str!("../README.md")]

extern crate alloc;

pub mod rw;
pub mod numeric;

pub use rw::*;
pub use numeric::*;
use schemars::JsonSchema;

use core::{ops::{Deref, DerefMut}, result::Result, mem::MaybeUninit, marker::PhantomData, borrow::Borrow};

use alloc::{string::String, borrow::Cow};

pub use binmarshal_macros::{Marshal, Demarshal, MarshalUpdate, Proxy};

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))] 
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub enum MarshalError {
  BufferTooSmall,
  IllegalValue {
    byte_offset: usize,
    bit_offset: usize,
  },
  IllegalTag,
  CoercionError,
  ExpectedSentinel,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitSpecification<const BITS: usize>;

pub trait Marshal<Context = ()> {
  fn write<W: BitWriter>(&self, writer: &mut W, ctx: Context) -> Result<(), MarshalError>;
}

pub trait Demarshal<'dm, Context = ()> : Sized {
  fn read(view: &mut BitView<'dm>, ctx: Context) -> Result<Self, MarshalError>;
}

impl<'a, C, T: Marshal<C> + ?Sized> Marshal<C> for &'a T {
  fn write<W: BitWriter>(&self, writer: &mut W, ctx: C) -> Result<(), MarshalError> {
    (self as &'a T).write(writer, ctx)
  }
}
// Stolen mercilessly from serde
pub trait DemarshalOwned<Context = ()> : for<'de> Demarshal<'de, Context> {}
impl<T, Context> DemarshalOwned<Context> for T where T: for<'de> Demarshal<'de, Context> {}

pub trait MarshalUpdate<Context = ()> : Sized {
  fn update(&mut self, ctx: &mut Context);
}

impl Marshal<()> for () {
  #[inline(always)]
  fn write<W: BitWriter>(&self, _writer: &mut W, _ctx: ()) -> Result<(), MarshalError> {
    Ok(())
  }
}

impl<'dm> Demarshal<'dm, ()> for () {
  #[inline(always)]
  fn read(_view: &mut BitView<'dm>, _ctx: ()) -> Result<Self, MarshalError> {
    Ok(())
  }
}

impl<C: Clone, T: Marshal<C>, const N: usize> Marshal<C> for [T; N] {
  #[inline]
  fn write<W: BitWriter>(&self, writer: &mut W, ctx: C) -> Result<(), MarshalError> {
    for v in self.iter() {
      v.write(writer, ctx.clone())?;
    }
    Ok(())
  }
}

impl<'dm, C: Clone, T: Demarshal<'dm, C>, const N: usize> Demarshal<'dm, C> for [T; N] {
  fn read(view: &mut BitView<'dm>, ctx: C) -> Result<Self, MarshalError> {
    let mut arr: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };
    for i in 0..N {
      arr[i] = MaybeUninit::new(T::read(view, ctx.clone())?);
    }
    return unsafe { Ok(arr.as_ptr().cast::<[T; N]>().read()) }
  }
}

#[derive(Proxy)]
pub struct Buffer<const N: usize>(pub [u8; N]);

impl<const N: usize> Marshal<()> for Buffer<N> {
  fn write<W: BitWriter>(&self, writer: &mut W, _ctx: ()) -> Result<(), MarshalError> {
    writer.align(1);
    let (arr, _offset) = writer.reserve_and_advance::<N>(N, 0)?;
    arr.copy_from_slice(&self.0[..]);
    Ok(())
  }
}

impl<'dm, const N: usize> Demarshal<'dm, ()> for Buffer<N> {
  fn read(view: &mut BitView<'dm>, _ctx: ()) -> Result<Self, MarshalError> {
    view.align(1);
    view.take::<N>(N, 0).map(|x| Self(x.0.clone()))
  }
}

impl Marshal<()> for String {
  fn write<W: BitWriter>(&self, writer: &mut W, _ctx: ()) -> Result<(), MarshalError> {
    writer.align(1);
    let arr = writer.reserve_and_advance_aligned_slice(self.len() + 1)?;
    let arr_str_bytes = &mut arr[0..self.len()];
    arr_str_bytes.copy_from_slice(&self.as_bytes()[..]);
    arr[arr.len() - 1] = 0;
    Ok(())
  }
}

impl<'de> Demarshal<'de, ()> for String {
  fn read(view: &mut BitView<'de>, _ctx: ()) -> Result<Self, MarshalError> {
    let buf = view.take_until(0u8)?;

    unsafe {
      Ok(String::from_utf8_unchecked(buf.to_vec()))
    }
  }
}

impl Marshal<()> for str {
  fn write<W: BitWriter>(&self, writer: &mut W, _ctx: ()) -> Result<(), MarshalError> {
    let arr = writer.reserve_and_advance_aligned_slice(self.len() + 1)?;
    arr[0..self.len()].copy_from_slice(&self.as_bytes()[..]);
    arr[arr.len() - 1] = 0;
    Ok(())
  }
}

impl<'dm> Demarshal<'dm, ()> for &'dm str {
  fn read(view: &mut BitView<'dm>, _ctx: ()) -> Result<Self, MarshalError> {
    let buf = view.take_until(0u8)?;
    Ok(unsafe { core::str::from_utf8_unchecked(buf) })
  }
}

impl<T: Marshal<()>, E: Marshal<()>> Marshal<()> for core::result::Result<T, E> {
  fn write<W: BitWriter>(&self, writer: &mut W, _ctx: ()) -> Result<(), MarshalError> {
    // We need this for alignment on the LaserCAN, due to a mistake in the 0.1.0 bootloader.
    #[cfg(feature = "lasercan_nop_patch")]
    unsafe { core::arch::asm!("nop") }

    match self {
      Ok(ok) => {
        0u8.write(writer, ())?;
        ok.write(writer, ())
      },
      Err(err) => {
        1u8.write(writer, ())?;
        err.write(writer, ())
      },
    }
  }
}

impl<'dm, T: Demarshal<'dm, ()>, E: Demarshal<'dm, ()>> Demarshal<'dm, ()> for core::result::Result<T, E> {
  fn read(view: &mut BitView<'dm>, _ctx: ()) -> Result<Self, MarshalError> {
    let tag = u8::read(view, ())?;
    match tag {
      0 => {
        T::read(view, ()).map(Ok)
      },
      _ => {
        E::read(view, ()).map(Err)
      },
    }
  }
}

// [T] and Vec<T> are both all-consuming. [u8] and Vec<u8> are special cases if using Payload and LengthTaggedPayload.
impl<'a, C: Clone, T: Marshal<C>> Marshal<C> for [T] {
  fn write<W: BitWriter>(&self, writer: &mut W, ctx: C) -> Result<(), MarshalError> {
    // let arr = writer.reserve_and_advance_aligned_slice(self.len())?;
    // arr.copy_from_slice(self);
    // Ok(())
    for result in self.iter().map(|x| x.write(writer, ctx.clone())) {
      result?;
    }
    Ok(())
  }
}

impl<'a, C: Clone, T: Marshal<C>> Marshal<C> for Vec<T> {
  fn write<W: BitWriter>(&self, writer: &mut W, ctx: C) -> Result<(), MarshalError> {
    for result in self.iter().map(|x| x.write(writer, ctx.clone())) {
      result?;
    }
    Ok(())
  }
}

impl<'dm, C: Clone, T: Demarshal<'dm, C>> Demarshal<'dm, C> for Vec<T> {
  fn read(view: &mut BitView<'dm>, ctx: C) -> Result<Self, MarshalError> {
    let mut v = Vec::new();

    loop {
      match T::read(view, ctx.clone()) {
        Ok(t) => v.push(t),
        Err(MarshalError::BufferTooSmall) => break,
        Err(e) => Err(e)?
      }
    }

    Ok(v)
  }
}

macro_rules! make_referent {
  ($referent_name:ident, $owned_name:ident, $inner:ty, <$($generics:ident),*>) => {
    #[repr(transparent)]
    pub struct $referent_name <$($generics),*> {
      #[allow(unused_parens)]
      phantoms: PhantomData<($($generics),*)>,
      inner: $inner
    }

    pub struct $owned_name <$($generics),*> where $inner: ToOwned {
      #[allow(unused_parens)]
      phantoms: PhantomData<($($generics),*)>,
      inner: <$inner as ToOwned>::Owned
    }

    impl<$($generics),*> $owned_name<$($generics),*> where $inner: ToOwned {
      pub fn new(inner: <$inner as ToOwned>::Owned) -> Self {
        Self { phantoms: PhantomData, inner }
      }
    }

    impl<'a, $($generics),*> From<&'a $inner> for &'a $referent_name <$($generics),*> {
      fn from(r: &'a $inner) -> Self {
        let ptr = r as *const $inner as *const $referent_name <$($generics),*>;
        unsafe { &*ptr }
      }
    }

    impl<'a, $($generics),*> AsRef<$inner> for &'a $referent_name <$($generics),*> {
      fn as_ref(&self) -> &'a $inner {
        &self.inner
      }
    }

    impl<$($generics),*> Deref for $referent_name <$($generics),*> {
      type Target = $inner;

      fn deref(&self) -> &Self::Target {
        &self.inner
      }
    }

    impl<$($generics),*> Deref for $owned_name <$($generics),*> where $inner: ToOwned {
      type Target = <$inner as ToOwned>::Owned;

      fn deref(&self) -> &Self::Target {
        &self.inner
      }
    }

    impl<$($generics),*> DerefMut for $owned_name <$($generics),*> where $inner: ToOwned {
      fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
      }
    }

    impl<$($generics),*> Clone for $owned_name <$($generics),*> where $inner: ToOwned, <$inner as ToOwned>::Owned: Clone {
      fn clone(&self) -> Self { Self { phantoms: PhantomData, inner: self.inner.clone() } }
    }
    
    impl<$($generics),*> core::fmt::Debug for $referent_name <$($generics),*> where $inner: core::fmt::Debug {
      fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result { self.inner.fmt(f) }
    }

    impl<$($generics),*> core::fmt::Debug for $owned_name <$($generics),*> where $inner: ToOwned, <$inner as ToOwned>::Owned: core::fmt::Debug {
      fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result { self.inner.fmt(f) }
    }

    impl<$($generics),*> PartialEq for $referent_name <$($generics),*> where $inner: PartialEq {
      #[inline]
      fn eq(&self, other: &Self) -> bool {
          PartialEq::eq(&self.inner, &other.inner)
      }
      #[inline]
      fn ne(&self, other: &Self) -> bool {
          PartialEq::ne(&self.inner, &other.inner)
      }
    }

    impl<$($generics),*> PartialEq for $owned_name <$($generics),*> where $inner: ToOwned, <$inner as ToOwned>::Owned: PartialEq {
      #[inline]
      fn eq(&self, other: &Self) -> bool {
          PartialEq::eq(&self.inner, &other.inner)
      }
      #[inline]
      fn ne(&self, other: &Self) -> bool {
          PartialEq::ne(&self.inner, &other.inner)
      }
    }

    impl<$($generics),*> Borrow<$referent_name <$($generics),*>> for $owned_name <$($generics),*> where $inner: ToOwned {
      fn borrow<'a>(&'a self) -> &'a $referent_name<$($generics),*> {
        let ptr = Borrow::borrow(&self.inner) as *const $inner as *const $referent_name <$($generics),*>;
        unsafe { &*ptr }
      }
    }

    impl<$($generics),*> ToOwned for $referent_name <$($generics),*> where $inner: ToOwned {
      type Owned = $owned_name <$($generics),*>;

      fn to_owned(&self) -> Self::Owned {
        $owned_name { phantoms: PhantomData, inner: self.inner.to_owned() }
      }
    }

    impl<CTX, $($generics),*> Marshal<CTX> for $owned_name <$($generics),*> where $inner: ToOwned, $referent_name<$($generics),*>: Marshal<CTX> {
      fn write<W: BitWriter>(&self, writer: &mut W, ctx: CTX) -> Result<(), MarshalError> {
        Marshal::<CTX>::write(Borrow::<$referent_name <$($generics),*>>::borrow(self), writer, ctx)
      }
    }

    #[cfg(feature = "serde")]
    impl<$($generics),*> serde::Serialize for $referent_name<$($generics),*> where $inner: serde::Serialize {
      fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
      where
        S: serde::Serializer
      {
        self.inner.serialize(serializer)
      }
    }

    #[cfg(feature = "serde")]
    impl<$($generics),*> serde::Serialize for $owned_name<$($generics),*> where $inner: ToOwned + serde::Serialize {
      fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
      where
        S: serde::Serializer
      {
        Borrow::<$referent_name <$($generics),*>>::borrow(self).serialize(serializer)
      }
    }

    #[cfg(feature = "serde")]
    impl<'de, $($generics),*> serde::de::Deserialize<'de> for $owned_name<$($generics),*> where $inner: ToOwned, <$inner as ToOwned>::Owned: serde::de::Deserialize<'de> {
      fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
      where
        D: serde::Deserializer<'de>
      {
        let t = <$inner as ToOwned>::Owned::deserialize(deserializer)?;
        Ok(Self { phantoms: PhantomData, inner: t })
      }
    }

    #[cfg(feature = "schema")]
    impl<$($generics),*> schemars::JsonSchema for $referent_name<$($generics),*> where $inner: JsonSchema {
      fn schema_name() -> String {
        <$inner>::schema_name()
      }

      fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        <$inner>::json_schema(gen)
      }

      fn is_referenceable() -> bool {
        <$inner>::is_referenceable()
      }

      fn schema_id() -> Cow<'static, str> {
        <$inner>::schema_id()
      }
    }

    #[cfg(feature = "schema")]
    impl<$($generics),*> schemars::JsonSchema for $owned_name<$($generics),*> where $inner: ToOwned, <$inner as ToOwned>::Owned: JsonSchema {
      fn schema_name() -> String {
        <$inner as ToOwned>::Owned::schema_name()
      }

      fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        <$inner as ToOwned>::Owned::json_schema(gen)
      }

      fn is_referenceable() -> bool {
        <$inner as ToOwned>::Owned::is_referenceable()
      }

      fn schema_id() -> Cow<'static, str> {
        <$inner as ToOwned>::Owned::schema_id()
      }
    }
  };
}

make_referent!(Payload, PayloadOwned, [u8], <>);
make_referent!(LengthTaggedPayload, LengthTaggedPayloadOwned, [u8], <L>);
make_referent!(LengthTaggedSlice, LengthTaggedVec, [T], <L, T>);

impl Marshal<()> for Payload {
  fn write<W: BitWriter>(&self, writer: &mut W, _: ()) -> Result<(), MarshalError> {
    let arr = writer.reserve_and_advance_aligned_slice(self.inner.len())?;
    arr.copy_from_slice(&self.inner);
    Ok(())
  }
}

impl<'dm> Demarshal<'dm, ()> for &'dm Payload {
  fn read(view: &mut BitView<'dm>, _: ()) -> Result<Self, MarshalError> {
    view.take_remaining().map(Into::into)
  }
}

impl<'dm> Demarshal<'dm, ()> for PayloadOwned {
  fn read(view: &mut BitView<'dm>, _: ()) -> Result<Self, MarshalError> {
    let m = view.take_remaining()?;
    Ok(PayloadOwned::new(m.to_vec()))
  }
}

impl<L: TryFrom<usize> + Marshal<()>> Marshal<()> for LengthTaggedPayload<L> {
  fn write<W: BitWriter>(&self, writer: &mut W, _: ()) -> Result<(), MarshalError> {
    match L::try_from(self.inner.len()) {
      Ok(v) => {
        v.write(writer, ())?;
        let arr = writer.reserve_and_advance_aligned_slice(self.inner.len())?;
        arr.copy_from_slice(&self.inner);
        Ok(())
      },
      Err(_) => Err(MarshalError::CoercionError),
    }
  }
}

impl<'dm, L: TryInto<usize> + DemarshalOwned> Demarshal<'dm, ()> for &'dm LengthTaggedPayload<L> {
  fn read(view: &mut BitView<'dm>, _: ()) -> Result<Self, MarshalError> {
    let l = L::read(view, ())?;
    let as_usize = l.try_into().map_err(|_| MarshalError::CoercionError)?;
    view.take_aligned_slice(as_usize).map(Into::into)
  }
}

impl<'dm, L: TryInto<usize> + DemarshalOwned> Demarshal<'dm, ()> for LengthTaggedPayloadOwned<L> {
  fn read(view: &mut BitView<'dm>, _: ()) -> Result<Self, MarshalError> {
    let l = L::read(view, ())?;
    let as_usize = l.try_into().map_err(|_| MarshalError::CoercionError)?;
    Ok(LengthTaggedPayloadOwned::new(view.take_aligned_slice(as_usize)?.to_vec()))
  }
}

impl<C: Clone, T: Marshal<C>, L: TryFrom<usize> + Marshal<()>> Marshal<C> for LengthTaggedSlice<L, T> {
  fn write<W: BitWriter>(&self, writer: &mut W, ctx: C) -> Result<(), MarshalError> {
    match L::try_from(self.inner.len()) {
      Ok(v) => {
        v.write(writer, ())?;
        for result in self.inner.iter().map(|x| x.write(writer, ctx.clone())) {
          result?;
        }
        Ok(())
      },
      Err(_) => Err(MarshalError::CoercionError),
    }
  }
}

impl<'dm, C: Clone, T: Demarshal<'dm, C>, L: TryInto<usize> + DemarshalOwned> Demarshal<'dm, C> for LengthTaggedVec<L, T>
  where [T]: ToOwned<Owned = Vec<T>>
{
  fn read(view: &mut BitView<'dm>, ctx: C) -> Result<Self, MarshalError> {
    let l = L::read(view, ())?;
    let as_usize = l.try_into().map_err(|_| MarshalError::CoercionError)?;
    let mut v = Vec::with_capacity(as_usize);
    for _ in 0..as_usize {
      v.push(T::read(view, ctx.clone())?);
    }
    Ok(LengthTaggedVec::new(v))
  }
}

impl<'a, C, T> Marshal<C> for Cow<'a, T>
where
  T: ToOwned + ?Sized + Marshal<C>,
{
  fn write<'f, W: BitWriter>(&'f self, writer: &mut W, ctx: C) -> Result<(), MarshalError> {
    let borrowed = self as &T;
    borrowed.write(writer, ctx)
  }
}

impl<'a, 'dm, C, T> Demarshal<'dm, C> for Cow<'a, T>
where
  T: ToOwned + ?Sized,
  <T as ToOwned>::Owned: Demarshal<'dm, C>
{
  fn read(view: &mut BitView<'dm>, ctx: C) -> Result<Self, MarshalError> {
    let t = <T as ToOwned>::Owned::read(view, ctx)?;
    Ok(alloc::borrow::Cow::Owned(t))
  }
}

#[derive(Proxy)]
pub struct AsymmetricCow<'a, T: 'a + ToOwned + ?Sized>(pub Cow<'a, T>);

impl<'a, T: 'a + ToOwned + ?Sized> AsymmetricCow<'a, T> {
  pub fn into_owned(self) -> <T as ToOwned>::Owned {
    self.0.into_owned()
  }

  pub fn to_mut(&mut self) -> &mut <T as ToOwned>::Owned {
    self.0.to_mut()
  }
  
  pub fn to_static(self) -> AsymmetricCow<'static, T> {
    let inner = self.0.into_owned();
    AsymmetricCow::<'static, T>(Cow::Owned(inner))
  }
}

impl<'a, C, T: ToOwned + ?Sized + Marshal<C>> Marshal<C> for AsymmetricCow<'a, T> {
  fn write<W: BitWriter>(&self, writer: &mut W, ctx: C) -> Result<(), MarshalError> {
    self.0.write(writer, ctx)
  }
}

impl<'dm, C, T> Demarshal<'dm, C> for AsymmetricCow<'dm, T>
where
  T: ToOwned + ?Sized + 'dm,
  &'dm T: Demarshal<'dm, C>
{
  fn read(view: &mut BitView<'dm>, ctx: C) -> Result<Self, MarshalError> {
    let t = <&T>::read(view, ctx)?;
    Ok(AsymmetricCow(alloc::borrow::Cow::Borrowed(t)))
  }
}

#[cfg(test)]
mod tests {
  use crate::*;

  #[test]
  fn test_u8() {
    let bytes = [0b1100_0101, 0b1010_0101];
    let mut reader = BitView::new(&bytes[..]);
    assert_eq!(u8::read(&mut reader, BitSpecification::<3>), Ok(0b110));
    assert_eq!(u8::read(&mut reader, ()), Ok(0b0010_1101));
    assert_eq!(u8::read(&mut reader, BitSpecification::<5>), Ok(0b00101));
    assert_eq!(u8::read(&mut reader, BitSpecification::<1>), Err(MarshalError::BufferTooSmall));

    let mut out = [0u8; 2];
    let mut writer = BufferBitWriter::new(&mut out);
    assert_eq!(0b11001u8.write(&mut writer, BitSpecification::<5>), Ok(()));
    assert_eq!(0b1010_1101u8.write(&mut writer, BitSpecification::<8>), Ok(()));
    assert_eq!(0b011u8.write(&mut writer, BitSpecification::<3>), Ok(()));
    assert_eq!(out[0], 0b1100_1101);
    assert_eq!(out[1], 0b0110_1011);
  }

  #[test]
  fn test_u16() {
    let bytes = [0b1100_0101, 0b1010_0101, 0b1011_1100];
    let mut reader = BitView::new(&bytes[..]);
    assert_eq!(u16::read(&mut reader, BitSpecification::<3>), Ok(0b110));
    assert_eq!(u16::read(&mut reader, ()), Ok(0b0010_1101_0010_1101));
    assert_eq!(u8::read(&mut reader, BitSpecification::<5>), Ok(0b11100));
    assert_eq!(u8::read(&mut reader, BitSpecification::<1>), Err(MarshalError::BufferTooSmall));
  }
}
