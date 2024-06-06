use core::result::Result;

use crate::MarshalError;

pub struct BitView<'a> {
  data: &'a [u8],
  offset_byte: usize,
  offset_bit: usize,
}

impl<'a> BitView<'a> {
  pub fn new(data: &'a [u8]) -> Self {
    Self { data, offset_byte: 0, offset_bit: 0 }
  }

  #[inline(always)]
  pub fn remaining(&self) -> (usize, usize) {
    if self.offset_bit != 0 {
      (self.data.len() - self.offset_byte - 1, 8 - self.offset_bit)
    } else {
      (self.data.len() - self.offset_byte, 0)
    }
  }

  #[inline(always)]
  pub fn rewind(&mut self) {
    self.offset_byte = 0;
    self.offset_bit = 0;
  }

  #[inline(always)]
  pub fn fork(&self) -> Self {
    Self { data: self.data, offset_bit: self.offset_bit, offset_byte: self.offset_byte }
  }

  #[inline(always)]
  pub fn bit_offset(&self) -> usize {
    self.offset_bit
  }

  #[inline(always)]
  pub fn align(&mut self, byte_division: usize) {
    if self.offset_bit > 0 {
      self.offset_bit = 0;
      self.offset_byte += 1;
    }
    let m = self.offset_byte % byte_division;
    if m != 0 {
      self.offset_byte += byte_division - m;
    }
  }

  #[inline(always)]
  pub fn check_magic<const N: usize>(&mut self, magic: &[u8; N]) -> Result<(), MarshalError> {
    self.align(1);
    match self.take::<N>(N, 0) {
      Ok((m, _)) if m == magic => Ok(()),
      Ok(_) => Err(MarshalError::IllegalValue { byte_offset: self.offset_byte, bit_offset: self.offset_bit }),
      Err(e) => Err(e)
    }
  }

  #[inline(always)]
  pub fn take<const N: usize>(&mut self, bytes: usize, bits: usize) -> Result<(&'a [u8; N], usize), MarshalError> {
    // Remember - this is safe since we are map-ing. If anything is out of bounds, it will 
    // return None. This gives us a safe array type.
    let out = self.data.get(self.offset_byte..self.offset_byte+N).map(|x| {
      (unsafe { &*(x.as_ptr() as *const [_; N]) }, self.offset_bit)
    });

    if let Some(out) = out {
      self.offset_byte += (self.offset_bit + bits) / 8 + bytes;
      self.offset_bit = (self.offset_bit + bits) % 8;

      Ok(out)
    } else {
      Err(MarshalError::BufferTooSmall)
    }
  }

  #[inline(always)]
  pub fn take_aligned_slice(&mut self, bytes: usize) -> Result<&'a [u8], MarshalError> {
    self.align(1);
    let out = self.data.get(self.offset_byte..self.offset_byte + bytes);

    if let Some(out) = out {
      self.offset_byte += bytes;
      Ok(out)
    } else {
      Err(MarshalError::BufferTooSmall)
    }
  }

  #[inline(always)]
  pub fn take_until(&mut self, sentinel: u8) -> Result<&'a [u8], MarshalError> {
    self.align(1);
    let end = (&self.data[self.offset_byte..]).into_iter().position(|x| *x == sentinel);
    match end {
      Some(end) => {
        let out = Ok(&self.data[self.offset_byte..self.offset_byte + end]);
        self.offset_byte += end + 1;
        out
      },
      None => Err(MarshalError::ExpectedSentinel),
    }
  }

  #[inline(always)]
  pub fn take_remaining(&mut self) -> Result<&'a [u8], MarshalError> {
    self.align(1);
    Ok(&self.data[self.offset_byte..])
  }

  #[inline(always)]
  pub fn advance(&mut self, bytes: usize, bits: usize) {
    self.offset_byte += (self.offset_bit + bits) / 8 + bytes;
    self.offset_bit = (self.offset_bit + bits) % 8;
  }

  #[inline(always)]
  pub fn offset(&self) -> (usize, usize) {
    (self.offset_byte, self.offset_bit)
  }

  #[inline(always)]
  pub fn offset_for_drain(&self) -> usize {
    match self.offset_bit {
      0 => self.offset_byte,
      _ => self.offset_byte + 1
    }
  }
}

pub trait BitWriter {
  fn bit_offset(&self) -> usize;
  fn align(&mut self, byte_division: usize);
  fn reserve_and_advance<const N: usize>(&mut self, bytes: usize, bits: usize) -> Result<(&mut [u8; N], usize), MarshalError>;
  fn reserve_and_advance_aligned_slice(&mut self, bytes: usize) -> Result<&mut [u8], MarshalError>;
  fn advance(&mut self, bytes: usize, bits: usize);
  fn slice(&self) -> &[u8];

  #[inline(always)]
  fn write_magic<const N: usize>(&mut self, magic: &[u8; N]) -> Result<(), MarshalError> {
    self.align(1);
    match self.reserve_and_advance::<N>(N, 0) {
      Ok(buf) => {
        buf.0.copy_from_slice(magic);
        Ok(())
      },
      Err(e) => Err(e),
    }
  }
}

pub struct BufferBitWriter<'a> {
  out: &'a mut [u8],
  offset_byte: usize,
  offset_bit: usize,
}

impl<'a> BufferBitWriter<'a> {
  pub fn new(out: &'a mut [u8]) -> Self {
    Self { out, offset_byte: 0, offset_bit: 0 }
  }
}

impl<'a> BitWriter for BufferBitWriter<'a> {
  #[inline(always)]
  fn bit_offset(&self) -> usize {
    self.offset_bit
  }

  #[inline(always)]
  fn align(&mut self, byte_division: usize) {
    if self.offset_bit > 0 {
      self.offset_bit = 0;
      self.offset_byte += 1;
    }
    let m = self.offset_byte % byte_division;
    if m != 0 {
      self.offset_byte += byte_division - m;
    }
  }

  #[inline(always)]
  fn reserve_and_advance<const N: usize>(&mut self, bytes: usize, bits: usize) -> Result<(&mut [u8; N], usize), MarshalError> {
    // Remember - this is safe since we are map-ing. If anything is out of bounds, it will 
    // return None. This gives us a safe array type.
    let out = self.out.get_mut(self.offset_byte..self.offset_byte+N).map(|x| {
      (unsafe { &mut *(x.as_ptr() as *mut [_; N]) }, self.offset_bit)
    });

    if let Some(out) = out {
      self.offset_byte += (self.offset_bit + bits) / 8 + bytes;
      self.offset_bit = (self.offset_bit + bits) % 8;
      Ok(out)
    } else {
      Err(MarshalError::BufferTooSmall)
    }
  }

  #[inline(always)]
  fn reserve_and_advance_aligned_slice(&mut self, bytes: usize) -> Result<&mut [u8], MarshalError> {
    self.align(1);
    let out = self.out.get_mut(self.offset_byte..self.offset_byte + bytes);

    if let Some(out) = out {
      self.offset_byte += bytes;
      Ok(out)
    } else {
      Err(MarshalError::BufferTooSmall)
    }
  }

  #[inline(always)]
  fn advance(&mut self, bytes: usize, bits: usize) {
    self.offset_byte += (self.offset_bit + bits) / 8 + bytes;
    self.offset_bit = (self.offset_bit + bits) % 8;
  }

  #[inline(always)]
  fn slice(&self) -> &[u8] {
    if self.offset_bit == 0 {
      &self.out[0..self.offset_byte]
    } else {
      &self.out[0..self.offset_byte+1]
    }
  }
}

pub struct VecBitWriter {
  out: alloc::vec::Vec<u8>,
  offset_byte: usize,
  offset_bit: usize
}

impl VecBitWriter {
  pub fn new() -> Self {
    Self { out: alloc::vec![], offset_byte: 0, offset_bit: 0 }
  }
}

impl BitWriter for VecBitWriter {
  #[inline(always)]
  fn bit_offset(&self) -> usize {
    self.offset_bit
  }

  #[inline(always)]
  fn align(&mut self, byte_division: usize) {
    if self.offset_bit > 0 {
      self.offset_bit = 0;
      self.offset_byte += 1;
    }
    let m = self.offset_byte % byte_division;
    if m != 0 {
      self.offset_byte += byte_division - m;
    }
  }

  #[inline(always)]
  fn reserve_and_advance<const N: usize>(&mut self, bytes: usize, bits: usize) -> Result<(&mut [u8; N], usize), MarshalError> {
    // Remember - this is safe since we are map-ing. If anything is out of bounds, it will 
    // return None. This gives us a safe array type.
    self.out.resize(self.offset_byte + N, 0x00);
    let out = self.out.get_mut(self.offset_byte..self.offset_byte+N).map(|x| {
      (unsafe { &mut *(x.as_ptr() as *mut [_; N]) }, self.offset_bit)
    });

    if let Some(out) = out {
      self.offset_byte += (self.offset_bit + bits) / 8 + bytes;
      self.offset_bit = (self.offset_bit + bits) % 8;
      Ok(out)
    } else {
      Err(MarshalError::BufferTooSmall)
    }
  }

  #[inline(always)]
  fn reserve_and_advance_aligned_slice(&mut self, bytes: usize) -> Result<&mut [u8], MarshalError> {
    self.align(1);
    self.out.resize(self.offset_byte + bytes, 0x00);
    let out = self.out.get_mut(self.offset_byte..self.offset_byte + bytes);

    if let Some(out) = out {
      self.offset_byte += bytes;
      Ok(out)
    } else {
      Err(MarshalError::BufferTooSmall)
    }
  }

  #[inline(always)]
  fn advance(&mut self, bytes: usize, bits: usize) {
    self.offset_byte += (self.offset_bit + bits) / 8 + bytes;
    self.offset_bit = (self.offset_bit + bits) % 8;
  }

  #[inline(always)]
  fn slice(&self) -> &[u8] {
    if self.offset_bit == 0 {
      &self.out[0..self.offset_byte]
    } else {
      &self.out[0..self.offset_byte+1]
    }
  }
}
