#![cfg_attr(not(feature="std"), no_std)]

#![doc = include_str!("../README.md")]

extern crate alloc;

use core::{ops::{Deref, DerefMut}, mem::MaybeUninit, marker::PhantomData};

use alloc::{vec::Vec, string::String};
pub use rw::*;

pub use binmarshal_macros::{BinMarshal, Proxy};

pub mod rw;

// See: https://github.com/rust-lang/rust/issues/86935
pub type SelfType<T> = T;

pub trait HasTags {
  type Tags;
}

#[derive(Clone)]
pub struct BitSpecification<const BITS: usize>;

pub trait BinMarshal<Context = ()> : Sized {
  type Context;

  fn write<W: BitWriter>(self, writer: &mut W, ctx: Context) -> bool;
  fn read(view: &mut BitView<'_>, ctx: Context) -> Option<Self>;
  fn update(&mut self, ctx: &mut Context);
}

impl<C> BinMarshal<C> for () {
  type Context = C;

  fn write<W: BitWriter>(self, _writer: &mut W, _ctx: C) -> bool {
    true
  }

  fn read(_view: &mut BitView<'_>, _ctx: C) -> Option<Self> {
    Some(())
  }

  fn update(&mut self, _ctx: &mut C) { }
}

impl<C: Clone, T: BinMarshal<C> + Sized, const N: usize> BinMarshal<C> for [T; N] {
  type Context = ();

  #[inline]
  fn write<W: BitWriter>(self, writer: &mut W, ctx: C) -> bool {
    for v in self {
      if !v.write(writer, ctx.clone()) {
        return false;
      }
    }
    true
  }

  #[inline]
  fn read(view: &mut BitView<'_>, ctx: C) -> Option<Self> {
    let mut arr: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };
    for i in 0..N {
      arr[i] = MaybeUninit::new(T::read(view, ctx.clone())?);
    }
    return unsafe { Some(arr.as_ptr().cast::<[T; N]>().read()) }
  }

  // Context updates can only map 1-to-1
  #[inline]
  fn update(& mut self, _ctx: &mut C) { }
}

macro_rules! unsigned_numeric_impl {
  ( $t:ty ) => {
    impl<const BITS: usize> BinMarshal<BitSpecification<BITS>> for $t {
      type Context = BitSpecification<BITS>;

      #[inline(always)]
      fn write<W: BitWriter>(self, writer: &mut W, _ctx: BitSpecification<BITS>) -> bool {
        let offset = writer.bit_offset();
        if offset + BITS <= <$t>::BITS as usize {
          if let Some((arr, offset)) = writer.reserve_and_advance::<{<$t>::BITS as usize / 8}>(0, BITS) {
            let me = self << (<$t>::BITS as usize - (BITS + offset));
            let me_bytes = me.to_be_bytes();

            let mask = (<$t>::MAX >> (<$t>::BITS as usize - BITS)) << (<$t>::BITS as usize - (BITS + offset));
            let mask_bytes = mask.to_be_bytes();
            
            for i in 0..(<$t>::BITS as usize / 8) {
              arr[i] &= !mask_bytes[i];
              arr[i] |= me_bytes[i];
            }

            true
          } else {
            false
          }
        } else {
          if let Some((arr, offset)) = writer.reserve_and_advance::<{<$t>::BITS as usize / 8 + 1}>(0, BITS) {
            let me_bytes = self.to_be_bytes();
            
            let first_bits = 8 - offset;
            let last_bits = BITS - (<$t>::BITS as usize - offset);

            arr[0] &= !(u8::MAX >> (8 - first_bits));
            arr[0] |= (me_bytes[0] >> (8 - first_bits));

            for i in 1..(<$t>::BITS as usize / 8) {
              arr[i] = ((me_bytes[i - 1] & (u8::MAX >> (8 - first_bits))) << (8 - first_bits)) | (me_bytes[i] >> (8 - first_bits));
            }

            arr[<$t>::BITS as usize / 8] &= !((u8::MAX >> (8 - last_bits)) << (8 - last_bits));
            arr[<$t>::BITS as usize / 8] |= (me_bytes[<$t>::BITS as usize / 8 - 1] & (u8::MAX >> (8 - last_bits))) << (8 - last_bits);

            true
          } else {
            false
          }
        }
      }

      // TODO: Signed integers
      #[inline(always)]
      fn read(view: &mut BitView<'_>, _ctx: BitSpecification<BITS>) -> Option<Self> {
        let offset = view.bit_offset();
        if offset + BITS <= <$t>::BITS as usize {
          // It'll fit in as many bytes as we have
          let arr = view.take::<{<$t>::BITS as usize / 8}>(0, BITS);
          if let Some((arr, offset)) = arr {
            let v = <$t>::from_be_bytes(*arr);
            Some((v >> (<$t>::BITS as usize - (BITS + offset))) & (<$t>::MAX >> (<$t>::BITS as usize - BITS)))
          } else {
            None
          }
        } else {
          // We need to allocate one extra byte
          let arr = view.take::<{(<$t>::BITS as usize / 8) + 1}>(0, BITS);
          if let Some((arr, offset)) = arr {
            let first_bits: usize = <$t>::BITS as usize - offset;
            let second_bits: usize = BITS - first_bits;
            let first_n = unsafe { &*(arr[0..(<$t>::BITS as usize / 8)].as_ptr() as *const [u8; {<$t>::BITS as usize / 8}] ) };
            let v = <$t>::from_be_bytes(*first_n);
            let upper = v & (<$t>::MAX >> (<$t>::BITS as usize - first_bits));
            let lower = arr[(<$t>::BITS as usize / 8)] >> (8 - second_bits);
            Some(upper << (BITS - first_bits) | (lower as $t))
          } else {
            None
          }
        }
      }

      #[inline(always)]
      fn update(& mut self, _ctx: &mut BitSpecification<BITS>) { }
    }

    impl BinMarshal<()> for $t {
      type Context = ();

      #[inline(always)]
      fn write<W: BitWriter>(self, writer: &mut W, _ctx: ()) -> bool {
        self.write(writer, BitSpecification::<{<$t>::BITS as usize}> {})
      }

      #[inline(always)]
      fn read(view: &mut BitView<'_>, _ctx: ()) -> Option<Self> {
        Self::read(view, BitSpecification::<{<$t>::BITS as usize}> {})
      }

      #[inline(always)]
      fn update(&mut self, _ctx: &mut ()) { }
    }
  }
}

unsigned_numeric_impl!(u8);
unsigned_numeric_impl!(u16);
unsigned_numeric_impl!(u32);
unsigned_numeric_impl!(u64);
unsigned_numeric_impl!(u128);

macro_rules! generic_numeric_impl {
  ( $t:ty, $s:expr ) => {
    impl BinMarshal<()> for $t {
      type Context = ();

      #[inline(always)]
      fn write<W: BitWriter>(self, writer: &mut W, _ctx: ()) -> bool {
        Buffer(self.to_be_bytes()).write(writer, _ctx)
      }

      #[inline(always)]
      fn read(view: &mut BitView<'_>, _ctx: ()) -> Option<Self> {
        Some(Self::from_be_bytes(Buffer::<$s>::read(view, _ctx)?.0))
      }

      #[inline(always)]
      fn update(& mut self, _ctx: &mut ()) { }
    }
  }
}

generic_numeric_impl!(i8, 1);
generic_numeric_impl!(i16, 2);
generic_numeric_impl!(i32, 4);
generic_numeric_impl!(i64, 8);
generic_numeric_impl!(i128, 16);

generic_numeric_impl!(f32, 4);
generic_numeric_impl!(f64, 8);

impl BinMarshal<()> for bool {
  type Context = ();

  #[inline(always)]
  fn write<W: BitWriter>(self, writer: &mut W, _: ()) -> bool {
    (if self { 1u8 } else { 0u8 }).write(writer, BitSpecification::<1>)
  }

  #[inline(always)]
  fn read(view: &mut BitView<'_>, _: ()) -> Option<Self> {
    let n = u8::read(view, BitSpecification::<1>);
    n.map(|x| x != 0)
  }

  #[inline(always)]
  fn update(& mut self, _ctx: &mut ()) { }
}

// This makes the assumption that BITS <= 8
impl<const BITS: usize> BinMarshal<BitSpecification<BITS>> for bool {
  type Context = BitSpecification<BITS>;

  #[inline(always)]
  fn write<W: BitWriter>(self, writer: &mut W, ctx: BitSpecification<BITS>) -> bool {
    (if self { 1u8 } else { 0u8 }).write(writer, ctx)
  }

  #[inline(always)]
  fn read(view: &mut BitView<'_>, ctx: BitSpecification<BITS>) -> Option<Self> {
    let n = u8::read(view, ctx);
    n.map(|x| x != 0)
  }

  #[inline(always)]
  fn update(& mut self, _ctx: &mut BitSpecification<BITS>) { }
}

#[derive(Proxy)]
pub struct LengthTaggedVec<L, T>(pub Vec<T>, PhantomData<L>);

impl<L, T> LengthTaggedVec<L, T> {
  pub fn new(v: Vec<T>) -> Self {
    Self(v, PhantomData)
  }
}

impl<C: Clone, T: BinMarshal<C> + Sized, L: TryFrom<usize> + TryInto<usize> + BinMarshal<()>> BinMarshal<C> for LengthTaggedVec<L, T> {
  type Context = C;

  fn write<W: BitWriter>(self, writer: &mut W, ctx: C) -> bool {
    match L::try_from(self.0.len()) {
      Ok(v) => v.write(writer, ()) && self.0.into_iter().map(|x| x.write(writer, ctx.clone())).reduce(|a, b| a && b).unwrap_or(true),
      Err(_) => false,
    }
  }

  fn read(view: &mut BitView<'_>, ctx: C) -> Option<Self> {
    let l = L::read(view, ());
    match l {
      Some(l) => {
        let as_usize: usize = l.try_into().ok()?;
        let mut v = Vec::with_capacity(as_usize);
        for _ in 0..as_usize {
          v.push(T::read(view, ctx.clone())?);
        }
        Some(LengthTaggedVec::new(v))
      },
      None => None
    }
  }

  // Context updates can only map 1-to-1
  fn update(& mut self, _ctx: &mut C) { }
}

#[derive(Proxy)]
pub struct Buffer<const N: usize>(pub [u8; N]);

impl<const N: usize> BinMarshal<()> for Buffer<N> {
  type Context = ();

  #[inline]
  fn write<W: BitWriter>(self, writer: &mut W, _ctx: ()) -> bool {
    writer.align(1);
    if let Some((arr, _offset)) = writer.reserve_and_advance::<N>(N, 0) {
      arr.copy_from_slice(&self[..]);
      true
    } else {
      false
    }
  }

  #[inline]
  fn read(view: &mut BitView<'_>, _ctx: ()) -> Option<Self> {
    view.align(1);
    view.take::<N>(N, 0).map(|x| Self(x.0.clone()))
  }

  #[inline]
  fn update(&mut self, _ctx: &mut ()) { }
}

impl BinMarshal<()> for String {
  type Context = ();

  fn write<W: BitWriter>(self, writer: &mut W, _ctx: ()) -> bool {
    writer.align(1);
    if let Some(arr) = writer.reserve_and_advance_aligned_slice(self.len() + 1) {
      let arr_str_bytes = &mut arr[0..self.len()];
      arr_str_bytes.copy_from_slice(&self.as_bytes()[..]);
      arr[arr.len() - 1] = 0;
      true
    } else {
      false
    }
  }

  fn read(view: &mut BitView<'_>, _ctx: ()) -> Option<Self> {
    let mut v = alloc::vec![];
    let mut cont = true;
    while cont {
      let b = view.take_aligned_slice(1);
      match b {
        Some(slice) if slice[0] == 0x00 => cont = false,
        Some(slice) => v.push(slice[0]),
        None => return None,
      }
    }

    // This is actually safe because we check for 0x00 above.
    unsafe {
      Some(String::from_utf8_unchecked(v))
    }
  }

  fn update(& mut self, _ctx: &mut ()) { }
}

impl<T: BinMarshal<()>, E: BinMarshal<()>> BinMarshal<()> for core::result::Result<T, E> {
  type Context = ();

  fn write<W: BitWriter>(self, writer: &mut W, _ctx: ()) -> bool {
    match self {
      Ok(ok) => {
        0u8.write(writer, ()) && ok.write(writer, ())
      },
      Err(err) => {
        1u8.write(writer, ()) && err.write(writer, ())
      },
    }
  }

  fn read(view: &mut BitView<'_>, _ctx: ()) -> Option<Self> {
    let tag = u8::read(view, ());
    match tag {
      Some(0) => {
        T::read(view, ()).map(|x| Ok(x))
      },
      Some(_) => {
        E::read(view, ()).map(|x| Err(x))
      },
      None => None
    }
  }

  fn update(&mut self, _ctx: &mut ()) { }
}

#[cfg(feature = "anyhow")]
impl BinMarshal<()> for anyhow::Error {
  type Context = ();

  fn write<W: BitWriter>(self, writer: &mut W, ctx: ()) -> bool {
    let str = alloc::format!("{}", self);
    str.write(writer, ctx)
  }

  fn read(view: &mut BitView<'_>, ctx: ()) -> Option<Self> {
    let str = String::read(view, ctx);
    str.map(|x| anyhow::Error::msg(x))
  }

  fn update(&mut self, _ctx: &mut ()) { }
}

#[cfg(test)]
mod tests {
  use crate::{BitSpecification, rw::{BufferBitWriter, BitView}, BinMarshal};

  #[test]
  fn test_u8() {
    let bytes = [0b1100_0101, 0b1010_0101];
    let mut reader = BitView::new(&bytes[..]);
    assert_eq!(u8::read(&mut reader, BitSpecification::<3>), Some(0b110));
    assert_eq!(u8::read(&mut reader, ()), Some(0b0010_1101));
    assert_eq!(u8::read(&mut reader, BitSpecification::<5>), Some(0b00101));
    assert_eq!(u8::read(&mut reader, BitSpecification::<1>), None);

    let mut out = [0u8; 2];
    let mut writer = BufferBitWriter::new(&mut out);
    assert!(0b11001u8.write(&mut writer, BitSpecification::<5>));
    assert!(0b1010_1101u8.write(&mut writer, BitSpecification::<8>));
    assert!(0b011u8.write(&mut writer, BitSpecification::<3>));
    assert_eq!(out[0], 0b1100_1101);
    assert_eq!(out[1], 0b0110_1011);
  }

  #[test]
  fn test_u16() {
    let bytes = [0b1100_0101, 0b1010_0101, 0b1011_1100];
    let mut reader = BitView::new(&bytes[..]);
    assert_eq!(u16::read(&mut reader, BitSpecification::<3>), Some(0b110));
    assert_eq!(u16::read(&mut reader, ()), Some(0b0010_1101_0010_1101));
    assert_eq!(u8::read(&mut reader, BitSpecification::<5>), Some(0b11100));
    assert_eq!(u8::read(&mut reader, BitSpecification::<1>), None);
  }
}
