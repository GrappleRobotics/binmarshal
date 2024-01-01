use binmarshal::*;

#[derive(Debug, Clone, PartialEq, Marshal, Demarshal)]
#[marshal(magic = b"ABCD")]
struct MyStruct {
  a: u8,
  #[marshal(bits = 12)]
  b: u16,
  #[marshal(bits = 4)]
  c: u8,
  d: i32,
  e: f64,
  #[marshal(bits = 1)]
  f: [bool; 8],
  #[marshal(bits = 1)]
  g: LengthTaggedVec<u8, bool>,
  h: String
}

#[derive(Debug, Clone, PartialEq, Marshal, Demarshal)]
#[marshal(tag_type = u8)]
enum MyEnum {
  #[marshal(tag = "0")]
  MyStruct(MyStruct),
  #[marshal(tag = "1")]
  Struct {
    a: u8,
    b: u16
  },
  #[marshal(tag = "2")]
  Unit
}

fn main() {
  let v = MyEnum::MyStruct(MyStruct {
    a: 121,
    b: 2192,
    c: 15,
    d: -1234,
    e: 10.101,
    f: [ true, false, true, true, false, false, true, false ],
    g: LengthTaggedVec::new(vec![ true, false, true, false, false, true, true, true, false, true, true, false ]),
    h: "Hello World".to_owned()
  });

  let mut writer = VecBitWriter::new();
  assert!(matches!(v.clone().write(&mut writer, ()), Ok(())));

  let slice = writer.slice();
  assert_eq!(slice.len(), 36);

  let v2 = MyEnum::read(&mut BitView::new(slice), ());
  assert!(matches!(v2, Ok(v2) if v2 == v));
}