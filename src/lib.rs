#![cfg_attr(not(test), no_std)]

#![doc = include_str!("../README.md")]

extern crate alloc;

use core::{ops::{Deref, DerefMut}, mem::MaybeUninit, marker::PhantomData};

use alloc::vec::Vec;
use binmarshal_macros::Proxy;
use rw::{BitView, BitWriter};

pub mod rw;

#[derive(Clone)]
pub struct BitSpecification<const BITS: usize>;

pub trait BinMarshal<Context = ()> : Sized {
  fn write<W: BitWriter>(self, writer: &mut W, ctx: Context) -> bool;
  fn read(view: &mut BitView<'_>, ctx: Context) -> Option<Self>;
}

impl<const N: usize> BinMarshal<()> for Buffer<N> {
  fn write<W: BitWriter>(self, writer: &mut W, _ctx: ()) -> bool {
    writer.align(1);
    if let Some((arr, _offset)) = writer.reserve_and_advance::<N>(N, 0) {
      arr.copy_from_slice(&self[..]);
      true
    } else {
      false
    }
  }

  fn read(view: &mut BitView<'_>, _ctx: ()) -> Option<Self> {
    view.align(1);
    view.take::<N>(N, 0).map(|x| Self(x.0.clone()))
  }
}

impl<C: Clone, T: BinMarshal<C> + Sized, const N: usize> BinMarshal<C> for [T; N] {
  fn write<W: BitWriter>(self, writer: &mut W, ctx: C) -> bool {
    for v in self {
      if !v.write(writer, ctx.clone()) {
        return false;
      }
    }
    true
  }

  fn read(view: &mut BitView<'_>, ctx: C) -> Option<Self> {
    let mut arr: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };
    for i in 0..N {
      arr[i] = MaybeUninit::new(T::read(view, ctx.clone())?);
    }
    return unsafe { Some(arr.as_ptr().cast::<[T; N]>().read()) }
  }
}

macro_rules! unsigned_numeric_impl {
  ( $t:ty ) => {
    impl<const BITS: usize> BinMarshal<BitSpecification<BITS>> for $t {
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
    }

    impl BinMarshal<()> for $t {
      #[inline(always)]
      fn write<W: BitWriter>(self, writer: &mut W, _ctx: ()) -> bool {
        self.write(writer, BitSpecification::<{<$t>::BITS as usize}> {})
      }

      #[inline(always)]
      fn read(view: &mut BitView<'_>, _ctx: ()) -> Option<Self> {
        Self::read(view, BitSpecification::<{<$t>::BITS as usize}> {})
      }
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
      fn write<W: BitWriter>(self, writer: &mut W, _ctx: ()) -> bool {
        Buffer(self.to_be_bytes()).write(writer, _ctx)
      }

      fn read(view: &mut BitView<'_>, _ctx: ()) -> Option<Self> {
        Some(Self::from_be_bytes(Buffer::<$s>::read(view, _ctx)?.0))
      }
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

#[derive(Proxy)]
pub struct LengthTaggedVec<T, L>(pub Vec<T>, PhantomData<L>);

impl<T, L> LengthTaggedVec<T, L> {
  pub fn new(v: Vec<T>) -> Self {
    Self(v, PhantomData)
  }
}

impl<C: Clone, T: BinMarshal<C> + Sized, L: TryFrom<usize> + TryInto<usize> + BinMarshal<()>> BinMarshal<C> for LengthTaggedVec<T, L> {
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
}

#[derive(Proxy)]
pub struct Buffer<const N: usize>(pub [u8; N]);

#[cfg(test)]
mod tests {
  use binmarshal_macros::BinMarshal;

  use crate::rw::{BitView, BitWriter, BufferBitWriter};
  use crate::{BinMarshal, BitSpecification, LengthTaggedVec};

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

  #[derive(Debug, Clone, BinMarshal, PartialEq, Eq)]
  struct MyStruct {
    a: u16,
    #[marshal(bits = 12)]
    b: u16,
    #[marshal(bits = 24)]
    c: u32,
    d: u64,
  }

  #[derive(Debug, Clone, BinMarshal, PartialEq, Eq)]
  #[marshal(tag_type = u8)]
  enum MyEnum {
    #[marshal(tag = "1")]
    MyStruct(MyStruct),
    #[marshal(tag = "2")]
    InnerStruct {
      #[marshal(bits = 4)]
      x: u8,
      a: [i8; 2],
      b: i16,
      #[marshal(bits = 4)]
      v: LengthTaggedVec<u8, u8>
    },
    #[marshal(tag = "3")]
    Unit
  }

  #[test]
  fn test_packed() {
    let v = MyStruct {
      a: 0b01011111_01011100,
      b: 0b1011_1011_0001,
      c: 0b1110_0001_1000_0110_1010_0010,
      d: u64::MAX - 644225
    };

    let mut bytes = [0u8; 256];
    let mut writer = BufferBitWriter::new(&mut bytes);
    v.clone().write(&mut writer, ());

    let slice = writer.slice();
    assert_eq!(slice.len(), 15);

    let v2 = MyStruct::read(&mut BitView::new(slice), ());
    assert_eq!(v2, Some(v));
  }

  #[test]
  fn test_packed_enum() {
    let v = MyEnum::InnerStruct { x: 3, a: [-12, 123], b: -999, v: LengthTaggedVec::new(vec![ 0xD, 0xE, 0xA, 0xD ]) };

    let mut bytes = [0u8; 256];
    let mut writer = BufferBitWriter::new(&mut bytes);
    v.clone().write(&mut writer, ());

    let slice = writer.slice();
    assert_eq!(slice.len(), 9);

    let v2 = MyEnum::read(&mut BitView::new(slice), ());
    assert_eq!(v2, Some(v));
  }
}
