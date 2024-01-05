use std::borrow::Cow;

use binmarshal::*;

#[derive(Debug, Clone, PartialEq, Marshal, Demarshal)]
struct MyStruct<'a> {
  a: u8,
  b: u16,
  payload: AsymmetricCow<'a, Payload>
}

fn main() {
  let v = MyStruct { a: 12, b: 24, payload: Into::into(Cow::Borrowed(Into::into(&[0xDE, 0xAD, 0xBE, 0xEF][..]))) };

  let mut writer = VecBitWriter::new();
  v.write(&mut writer, ()).unwrap();

  let slice = writer.slice();
  assert_eq!(slice, &[12, 0, 24, 0xDE, 0xAD, 0xBE, 0xEF]);

  let v2 = MyStruct::read(&mut BitView::new(slice), ()).unwrap();
  println!("{:?}", v2);
  // v2.payload.to_mut().push(12);
  match &*v2.payload {
    Cow::Borrowed(_) => println!("Borrowed!"),
    Cow::Owned(_) => println!("Owned!"),
  };
  assert_eq!(v, v2);
}