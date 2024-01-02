use crate::{Marshal, Demarshal, BitSpecification, Buffer, MarshalError, rw::*};

macro_rules! unsigned_numeric_impl {
  ( $t:ty ) => {
    impl<const BITS: usize> Marshal<BitSpecification<BITS>> for $t {
      #[inline(always)]
      fn write<W: BitWriter>(&self, writer: &mut W, _ctx: BitSpecification<BITS>) -> Result<(), MarshalError> {
        let offset = writer.bit_offset();
        if offset + BITS <= <$t>::BITS as usize {
          let (arr, offset) = writer.reserve_and_advance::<{<$t>::BITS as usize / 8}>(0, BITS)?;
          let me = self << (<$t>::BITS as usize - (BITS + offset));
          let me_bytes = me.to_be_bytes();

          let mask = (<$t>::MAX >> (<$t>::BITS as usize - BITS)) << (<$t>::BITS as usize - (BITS + offset));
          let mask_bytes = mask.to_be_bytes();
          
          for i in 0..(<$t>::BITS as usize / 8) {
            arr[i] &= !mask_bytes[i];
            arr[i] |= me_bytes[i];
          }

          Ok(())
        } else {
          let (arr, offset) = writer.reserve_and_advance::<{<$t>::BITS as usize / 8 + 1}>(0, BITS)?;
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

          Ok(())
        }
      }
    }

    impl<'dm, const BITS: usize> Demarshal<'dm, BitSpecification<BITS>> for $t {
      #[inline(always)]
      fn read(view: &mut BitView<'dm>, _ctx: BitSpecification<BITS>) -> Result<Self, MarshalError> {
        let offset = view.bit_offset();
        if offset + BITS <= <$t>::BITS as usize {
          // It'll fit in as many bytes as we have
          let (arr, offset) = view.take::<{<$t>::BITS as usize / 8}>(0, BITS)?;
          let v = <$t>::from_be_bytes(*arr);
          Ok((v >> (<$t>::BITS as usize - (BITS + offset))) & (<$t>::MAX >> (<$t>::BITS as usize - BITS)))
        } else {
          // We need to allocate one extra byte
          let (arr, offset) = view.take::<{(<$t>::BITS as usize / 8) + 1}>(0, BITS)?;
          let first_bits: usize = <$t>::BITS as usize - offset;
          let second_bits: usize = BITS - first_bits;
          let first_n = unsafe { &*(arr[0..(<$t>::BITS as usize / 8)].as_ptr() as *const [u8; {<$t>::BITS as usize / 8}] ) };
          let v = <$t>::from_be_bytes(*first_n);
          let upper = v & (<$t>::MAX >> (<$t>::BITS as usize - first_bits));
          let lower = arr[(<$t>::BITS as usize / 8)] >> (8 - second_bits);
          Ok(upper << (BITS - first_bits) | (lower as $t))
        }
      }
    }

    impl Marshal<()> for $t {
      #[inline(always)]
      fn write<W: BitWriter>(&self, writer: &mut W, _ctx: ()) -> Result<(), MarshalError> {
        self.write(writer, BitSpecification::<{<$t>::BITS as usize}>)
      }
    }

    impl<'de> Demarshal<'de, ()> for $t {
      #[inline(always)]
      fn read<'a>(view: &mut BitView<'a>, _ctx: ()) -> Result<Self, MarshalError> {
        Self::read(view, BitSpecification::<{<$t>::BITS as usize}>)
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
    impl Marshal<()> for $t {
      #[inline(always)]
      fn write<W: BitWriter>(&self, writer: &mut W, _ctx: ()) -> Result<(), MarshalError> {
        Buffer(self.to_be_bytes()).write(writer, _ctx)
      }
    }

    impl<'de> Demarshal<'de, ()> for $t {
      #[inline(always)]
      fn read<'a>(view: &mut BitView<'a>, _ctx: ()) -> Result<Self, MarshalError> {
        Ok(Self::from_be_bytes(Buffer::<$s>::read(view, _ctx)?.0))
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

impl Marshal<()> for bool {
  #[inline(always)]
  fn write<W: BitWriter>(&self, writer: &mut W, _: ()) -> Result<(), MarshalError> {
    (if *self { 1u8 } else { 0u8 }).write(writer, ())
  }
}

impl<'dm> Demarshal<'dm, ()> for bool {
  #[inline(always)]
  fn read<'a>(view: &mut BitView<'a>, _: ()) -> Result<Self, MarshalError> {
    u8::read(view, ()).map(|x| x != 0)
  }
}

// This makes the assumption that BITS <= 8
impl<const BITS: usize> Marshal<BitSpecification<BITS>> for bool {
  fn write<W: BitWriter>(&self, writer: &mut W, ctx: BitSpecification<BITS>) -> Result<(), MarshalError> {
    (if *self { 1u8 } else { 0u8 }).write(writer, ctx)
  }
}

impl<'dm, const BITS: usize> Demarshal<'dm, BitSpecification<BITS>> for bool {
  #[inline(always)]
  fn read<'a>(view: &mut BitView<'a>, ctx: BitSpecification<BITS>) -> Result<Self, MarshalError> {
    u8::read(view, ctx).map(|x| x != 0)
  }
}
