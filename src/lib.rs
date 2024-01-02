#![cfg_attr(not(feature="std"), no_std)]

#![doc = include_str!("../README.md")]

extern crate alloc;

pub mod rw;
pub mod numeric;
pub mod cow;

pub use rw::*;
pub use numeric::*;
pub use cow::*;

use core::{ops::{Deref, DerefMut}, result::Result, mem::MaybeUninit, marker::PhantomData};

use alloc::{vec::Vec, string::String};

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
  ExpectedSentinel
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitSpecification<const BITS: usize>;

pub trait Marshal<Context = ()> {
  fn write<W: BitWriter>(&self, writer: &mut W, ctx: Context) -> Result<(), MarshalError>;
}

pub trait Demarshal<'dm, Context = ()> : Sized {
  fn read(view: &mut BitView<'dm>, ctx: Context) -> Result<Self, MarshalError>;
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

#[derive(Proxy)]
pub struct LengthTaggedVec<L, T>(pub Vec<T>, PhantomData<L>);

impl<L, T> LengthTaggedVec<L, T> {
  pub fn new(v: Vec<T>) -> Self {
    Self(v, PhantomData)
  }
}

impl<C: Clone, T: Marshal<C>, L: TryFrom<usize> + Marshal<()>> Marshal<C> for LengthTaggedVec<L, T> {
  fn write<W: BitWriter>(&self, writer: &mut W, ctx: C) -> Result<(), MarshalError> {
    match L::try_from(self.0.len()) {
      Ok(v) => {
        v.write(writer, ())?;
        for result in self.0.iter().map(|x| x.write(writer, ctx.clone())) {
          result?;
        }
        Ok(())
      },
      Err(_) => Err(MarshalError::CoercionError),
    }
  }
}

impl<'dm, C: Clone, T: Demarshal<'dm, C>, L: TryInto<usize> + DemarshalOwned> Demarshal<'dm, C> for LengthTaggedVec<L, T> {
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

impl<'a> Marshal<()> for &'a str {
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

// Payload is all-consuming, there is no tagged length
#[derive(Proxy)]
pub struct Payload<'a>(&'a [u8]);

impl<'a> Marshal<()> for Payload<'a> {
  fn write<W: BitWriter>(&self, writer: &mut W, _ctx: ()) -> Result<(), MarshalError> {
    let arr = writer.reserve_and_advance_aligned_slice(self.0.len())?;
    arr.copy_from_slice(self.0);
    Ok(())
  }
}

impl<'dm> Demarshal<'dm, ()> for Payload<'dm> {
  fn read(view: &mut BitView<'dm>, _ctx: ()) -> Result<Self, MarshalError> {
    Ok(Payload(view.take_remaining()?))
  }
}

#[derive(Proxy)]
pub struct LengthTaggedPayload<'a, L>(pub &'a [u8], PhantomData<L>);

impl<'a, L> LengthTaggedPayload<'a, L> {
  pub fn new(v: &'a [u8]) -> Self {
    Self(v, PhantomData)
  }
}

impl<'a, L: TryFrom<usize> + Marshal<()>> Marshal<()> for LengthTaggedPayload<'a, L> {
  fn write<W: BitWriter>(&self, writer: &mut W, _ctx: ()) -> Result<(), MarshalError> {
    match L::try_from(self.0.len()) {
      Ok(v) => {
        v.write(writer, ())?;
        let arr = writer.reserve_and_advance_aligned_slice(self.0.len())?;
        arr.copy_from_slice(self.0);
        Ok(())
      },
      Err(_) => Err(MarshalError::CoercionError),
    }
  }
}

impl<'dm, L: TryInto<usize> + DemarshalOwned> Demarshal<'dm, ()> for LengthTaggedPayload<'dm, L> {
  fn read(view: &mut BitView<'dm>, _ctx: ()) -> Result<Self, MarshalError> {
    let l = L::read(view, ())?;
    let as_usize = l.try_into().map_err(|_| MarshalError::CoercionError)?;
    Ok(LengthTaggedPayload(view.take_aligned_slice(as_usize)?, PhantomData))
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
