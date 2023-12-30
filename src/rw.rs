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
  pub fn align(&mut self, byte_division: usize) -> bool {
    if self.offset_bit > 0 {
      self.offset_bit = 0;
      self.offset_byte += 1;
    }
    let m = self.offset_byte % byte_division;
    if m != 0 {
      self.offset_byte += byte_division - m;
    }
    true
  }

  #[inline(always)]
  pub fn take<const N: usize>(&mut self, bytes: usize, bits: usize) -> Option<(&[u8; N], usize)> {
    // Remember - this is safe since we are map-ing. If anything is out of bounds, it will 
    // return None. This gives us a safe array type.
    let out = self.data.get(self.offset_byte..self.offset_byte+N).map(|x| {
      (unsafe { &*(x.as_ptr() as *const [_; N]) }, self.offset_bit)
    });

    if out.is_some() {
      self.offset_byte += (self.offset_bit + bits) / 8 + bytes;
      self.offset_bit = (self.offset_bit + bits) % 8;
    }

    out
  }

  #[inline(always)]
  pub fn take_aligned_slice(&mut self, bytes: usize) -> Option<&[u8]> {
    self.align(1);
    let out = self.data.get(self.offset_byte..self.offset_byte + bytes);

    if out.is_some() {
      self.offset_byte += bytes;
    }

    out
  }

  #[inline(always)]
  pub fn advance(&mut self, bytes: usize, bits: usize) {
    self.offset_byte += (self.offset_bit + bits) / 8 + bytes;
    self.offset_bit = (self.offset_bit + bits) % 8;
  }
}

pub trait BitWriter {
  fn bit_offset(&self) -> usize;
  fn align(&mut self, byte_division: usize) -> bool;
  fn reserve_and_advance<const N: usize>(&mut self, bytes: usize, bits: usize) -> Option<(&mut [u8; N], usize)>;
  fn reserve_and_advance_aligned_slice(&mut self, bytes: usize) -> Option<&mut [u8]>;
  fn advance(&mut self, bytes: usize, bits: usize);
  fn slice(&self) -> &[u8];
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
  fn align(&mut self, byte_division: usize) -> bool {
    if self.offset_bit > 0 {
      self.offset_bit = 0;
      self.offset_byte += 1;
    }
    let m = self.offset_byte % byte_division;
    if m != 0 {
      self.offset_byte += byte_division - m;
    }
    true
  }

  #[inline(always)]
  fn reserve_and_advance<const N: usize>(&mut self, bytes: usize, bits: usize) -> Option<(&mut [u8; N], usize)> {
    // Remember - this is safe since we are map-ing. If anything is out of bounds, it will 
    // return None. This gives us a safe array type.
    let out = self.out.get_mut(self.offset_byte..self.offset_byte+N).map(|x| {
      (unsafe { &mut *(x.as_ptr() as *mut [_; N]) }, self.offset_bit)
    });

    if out.is_some() {
      self.offset_byte += (self.offset_bit + bits) / 8 + bytes;
      self.offset_bit = (self.offset_bit + bits) % 8;
    }
    out
  }

  #[inline(always)]
  fn reserve_and_advance_aligned_slice(&mut self, bytes: usize) -> Option<&mut [u8]> {
    self.align(1);
    let out = self.out.get_mut(self.offset_byte..self.offset_byte + bytes);

    if out.is_some() {
      self.offset_byte += bytes;
    }

    out
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
  fn align(&mut self, byte_division: usize) -> bool {
    if self.offset_bit > 0 {
      self.offset_bit = 0;
      self.offset_byte += 1;
    }
    let m = self.offset_byte % byte_division;
    if m != 0 {
      self.offset_byte += byte_division - m;
    }
    true
  }

  #[inline(always)]
  fn reserve_and_advance<const N: usize>(&mut self, bytes: usize, bits: usize) -> Option<(&mut [u8; N], usize)> {
    // Remember - this is safe since we are map-ing. If anything is out of bounds, it will 
    // return None. This gives us a safe array type.
    self.out.resize(self.offset_byte + N, 0x00);
    let out = self.out.get_mut(self.offset_byte..self.offset_byte+N).map(|x| {
      (unsafe { &mut *(x.as_ptr() as *mut [_; N]) }, self.offset_bit)
    });

    if out.is_some() {
      self.offset_byte += (self.offset_bit + bits) / 8 + bytes;
      self.offset_bit = (self.offset_bit + bits) % 8;
    }
    out
  }

  #[inline(always)]
  fn reserve_and_advance_aligned_slice(&mut self, bytes: usize) -> Option<&mut [u8]> {
    self.align(1);
    self.out.resize(self.offset_byte + bytes, 0x00);
    let out = self.out.get_mut(self.offset_byte..self.offset_byte + bytes);

    if out.is_some() {
      self.offset_byte += bytes;
    }

    out
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
