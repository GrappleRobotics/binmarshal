#![cfg_attr(not(feature="std"), no_std)]

#![doc = include_str!("../README.md")]

extern crate alloc;

pub mod rw;
pub mod numeric;

pub use rw::*;
pub use numeric::*;

use core::{ops::{Deref, DerefMut}, result::Result, mem::MaybeUninit, marker::PhantomData, convert::Infallible};

use alloc::{vec::Vec, string::String, borrow::Cow};

pub use binmarshal_macros::{Marshal, Demarshal, MarshalUpdate, Proxy};

// See: https://github.com/rust-lang/rust/issues/86935
pub type SelfType<T> = T;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitSpecification<const BITS: usize>;

pub trait Marshal<Context = ()> {
  // type Context;
  type Error;

  fn write<W: BitWriter>(&self, writer: &mut W, ctx: Context) -> Result<(), Self::Error>;
}

pub trait Demarshal<'dm, Context = ()> : Sized {
  // type Context;
  type Error;
  
  fn read(view: &mut BitView<'dm>, ctx: Context) -> Result<Self, Self::Error>;
}

// Stolen mercilessly from serde
pub trait DemarshalOwned<Context = ()> : for<'de> Demarshal<'de, Context> {}
impl<T, Context> DemarshalOwned<Context> for T where T: for<'de> Demarshal<'de, Context> {}

pub trait MarshalUpdate<Context = ()> : Sized {
  fn update(&mut self, ctx: &mut Context);
}

impl Marshal<()> for () {
  type Error = Infallible;

  #[inline(always)]
  fn write<W: BitWriter>(&self, _writer: &mut W, _ctx: ()) -> Result<(), Self::Error> {
    Ok(())
  }
}

impl<'dm> Demarshal<'dm, ()> for () {
  type Error = Infallible;

  #[inline(always)]
  fn read(_view: &mut BitView<'dm>, _ctx: ()) -> Result<Self, Self::Error> {
    Ok(())
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArrayMarshalError<C, T: Marshal<C>> {
  AtIndex(usize, T::Error),
  OutOfBounds,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArrayDemarshalError<'dm, C, T: Demarshal<'dm, C>> {
  AtIndex(usize, T::Error),
  OutOfBounds,
}

impl<C: Clone, T: Marshal<C>, const N: usize> Marshal<C> for [T; N] {
  type Error = ArrayMarshalError<C, T>;

  #[inline]
  fn write<W: BitWriter>(&self, writer: &mut W, ctx: C) -> Result<(), Self::Error> {
    for (idx, v) in self.iter().enumerate() {
      v.write(writer, ctx.clone()).map_err(|e| ArrayMarshalError::AtIndex(idx, e))?;
    }
    Ok(())
  }
}

impl<'dm, C: Clone, T: Demarshal<'dm, C>, const N: usize> Demarshal<'dm, C> for [T; N] {
  type Error = ArrayDemarshalError<'dm, C, T>;

  fn read(view: &mut BitView<'dm>, ctx: C) -> Result<Self, Self::Error> {
    let mut arr: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };
    for i in 0..N {
      arr[i] = MaybeUninit::new(T::read(view, ctx.clone()).map_err(|e| ArrayDemarshalError::AtIndex(i, e))?);
    }
    return unsafe { Ok(arr.as_ptr().cast::<[T; N]>().read()) }
  }
}

#[derive(Proxy)]
pub struct Buffer<const N: usize>(pub [u8; N]);

impl<const N: usize> Marshal<()> for Buffer<N> {
  type Error = MarshalRWError;

  fn write<W: BitWriter>(&self, writer: &mut W, _ctx: ()) -> Result<(), Self::Error> {
    writer.align(1);
    let (arr, _offset) = writer.reserve_and_advance::<N>(N, 0)?;
    arr.copy_from_slice(&self.0[..]);
    Ok(())
  }
}

impl<'dm, const N: usize> Demarshal<'dm, ()> for Buffer<N> {
  type Error = MarshalRWError;

  fn read(view: &mut BitView<'dm>, _ctx: ()) -> Result<Self, Self::Error> {
    view.align(1);
    view.take::<N>(N, 0).map(|x| Self(x.0.clone()))
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TaggedVecMarshalError<C, T: Marshal<C>> {
  AtIndex(usize, T::Error),
  OutOfBounds,
  LengthConversionError
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TaggedVecDemarshalError<'dm, C, T: Demarshal<'dm, C>> {
  AtIndex(usize, T::Error),
  OutOfBounds,
  LengthConversionError
}

#[derive(Proxy)]
pub struct LengthTaggedVec<L, T>(pub Vec<T>, PhantomData<L>);

impl<L, T> LengthTaggedVec<L, T> {
  pub fn new(v: Vec<T>) -> Self {
    Self(v, PhantomData)
  }
}

impl<C: Clone, T: Marshal<C>, L: TryFrom<usize> + Marshal<()>> Marshal<C> for LengthTaggedVec<L, T> {
  type Error = TaggedVecMarshalError<C, T>;

  fn write<W: BitWriter>(&self, writer: &mut W, ctx: C) -> Result<(), Self::Error> {
    match L::try_from(self.0.len()) {
      Ok(v) => {
        v.write(writer, ()).map_err(|_| TaggedVecMarshalError::OutOfBounds)?;
        for (i, result) in self.0.iter().map(|x| x.write(writer, ctx.clone())).enumerate() {
          result.map_err(|e| TaggedVecMarshalError::AtIndex(i, e))?;
        }
        Ok(())
      },
      Err(_) => Err(TaggedVecMarshalError::LengthConversionError),
    }
  }
}

impl<'dm, C: Clone, T: Demarshal<'dm, C>, L: TryInto<usize> + DemarshalOwned> Demarshal<'dm, C> for LengthTaggedVec<L, T> {
  type Error = TaggedVecDemarshalError<'dm, C, T>;

  fn read(view: &mut BitView<'dm>, ctx: C) -> Result<Self, Self::Error> {
    let l = L::read(view, ()).map_err(|_| TaggedVecDemarshalError::OutOfBounds)?;
    let as_usize = l.try_into().map_err(|_| TaggedVecDemarshalError::LengthConversionError)?;
    let mut v = Vec::with_capacity(as_usize);
    for i in 0..as_usize {
      v.push(T::read(view, ctx.clone()).map_err(|e| TaggedVecDemarshalError::AtIndex(i, e))?);
    }
    Ok(LengthTaggedVec::new(v))
  }
}

impl Marshal<()> for String {
  type Error = MarshalRWError;

  fn write<W: BitWriter>(&self, writer: &mut W, _ctx: ()) -> Result<(), Self::Error> {
    writer.align(1);
    let arr = writer.reserve_and_advance_aligned_slice(self.len() + 1)?;
    let arr_str_bytes = &mut arr[0..self.len()];
    arr_str_bytes.copy_from_slice(&self.as_bytes()[..]);
    arr[arr.len() - 1] = 0;
    Ok(())
  }
}

impl<'de> Demarshal<'de, ()> for String {
  type Error = MarshalRWError;

  fn read(view: &mut BitView<'de>, _ctx: ()) -> Result<Self, Self::Error> {
    let mut v = alloc::vec![];
    let mut cont = true;
    while cont {
      let b = view.take_aligned_slice(1);
      match b {
        Ok(slice) if slice[0] == 0x00 => cont = false,
        Ok(slice) => v.push(slice[0]),
        Err(e) => return Err(e),
      }
    }

    // This is actually safe because we check for 0x00 above.
    unsafe {
      Ok(String::from_utf8_unchecked(v))
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResultMarshalError<T: Marshal<()>, E: Marshal<()>> {
  Tag(MarshalRWError),
  Ok(T::Error),
  Err(E::Error)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResultDemarshalError<'dm, T: Demarshal<'dm, ()>, E: Demarshal<'dm, ()>> {
  Tag(MarshalRWError),
  Ok(T::Error),
  Err(E::Error)
}

impl<T: Marshal<()>, E: Marshal<()>> Marshal<()> for core::result::Result<T, E> {
  type Error = ResultMarshalError<T, E>;

  fn write<W: BitWriter>(&self, writer: &mut W, _ctx: ()) -> Result<(), Self::Error> {
    // We need this for alignment on the LaserCAN, due to a mistake in the 0.1.0 bootloader.
    #[cfg(feature = "lasercan_nop_patch")]
    unsafe { core::arch::asm!("nop") }

    match self {
      Ok(ok) => {
        0u8.write(writer, ()).map_err(ResultMarshalError::Tag)?;
        ok.write(writer, ()).map_err(ResultMarshalError::Ok)
      },
      Err(err) => {
        1u8.write(writer, ()).map_err(ResultMarshalError::Tag)?;
        err.write(writer, ()).map_err(ResultMarshalError::Err)
      },
    }
  }
}

impl<'dm, T: Demarshal<'dm, ()>, E: Demarshal<'dm, ()>> Demarshal<'dm, ()> for core::result::Result<T, E> {
  type Error = ResultDemarshalError<'dm, T, E>;

  fn read(view: &mut BitView<'dm>, _ctx: ()) -> Result<Self, Self::Error> {
    let tag = u8::read(view, ()).map_err(ResultDemarshalError::Tag)?;
    match tag {
      0 => {
        T::read(view, ()).map(Ok).map_err(ResultDemarshalError::Ok)
      },
      _ => {
        E::read(view, ()).map(Err).map_err(ResultDemarshalError::Err)
      },
    }
  }
}

impl<'a, C, T: Marshal<C> + Clone> Marshal<C> for alloc::borrow::Cow<'a, T> {
  type Error = T::Error;

  fn write<W: BitWriter>(&self, writer: &mut W, ctx: C) -> Result<(), Self::Error> {
    self.as_ref().write(writer, ctx)
  }
}

impl<'a, 'dm, C, T: Demarshal<'dm, C> + Clone> Demarshal<'dm, C> for alloc::borrow::Cow<'a, T> {
  type Error = T::Error;

  fn read(view: &mut BitView<'dm>, ctx: C) -> Result<Self, Self::Error> {
    let t = T::read(view, ctx)?;
    Ok(Cow::Owned(t))
  }
}

// #[cfg(feature = "anyhow")]
// impl BinMarshal<()> for anyhow::Error {
//   type Context = ();

//   fn write<W: BitWriter>(&self, writer: &mut W, ctx: ()) -> bool {
//     let str = alloc::format!("{}", self);
//     str.write(writer, ctx)
//   }

//   fn read<'a>(view: &mut BitView<'a>, ctx: ()) -> Option<Self> {
//     let str = String::read(view, ctx);
//     str.map(|x| anyhow::Error::msg(x))
//   }

//   fn update(&mut self, _ctx: &mut ()) { }
// }

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
    assert_eq!(u8::read(&mut reader, BitSpecification::<1>), Err(MarshalRWError::OutOfBounds));

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
    assert_eq!(u8::read(&mut reader, BitSpecification::<1>), Err(MarshalRWError::OutOfBounds));
  }
}
