use binmarshal::*;

#[derive(Debug, Clone, PartialEq, Eq, Marshal, Demarshal)]
#[marshal(tag_type = "u8")]
pub enum SomeError<'a> {
  #[marshal(tag = "0")]
  MyErrorVariant(CowStr<'a>)
}

fn main() {
  let v = SomeError::<'static>::MyErrorVariant(CowStr::Borrowed("Hello World"));
  let mut writer = VecBitWriter::new();
  v.write(&mut writer, ()).unwrap();

  let slice = writer.slice();
  let v2 = SomeError::read(&mut BitView::new(slice), ()).unwrap();

  assert_eq!(v, v2)
}