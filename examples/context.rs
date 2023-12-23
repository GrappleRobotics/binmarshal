use binmarshal::{BinMarshal, rw::{BufferBitWriter, BitView, BitWriter}};

struct MyContext {
  variant: u8,
  inner: u8
}

struct Var4Context {
  variant: u8
}

#[derive(Debug, Clone, PartialEq, BinMarshal)]
#[marshal(ctx = Var4Context, tag = "ctx.variant")]
enum Var4Inner {
  #[marshal(tag = "1")]
  Variant1,
  #[marshal(tag = "2")]
  Variant2(u8, u16),
  #[marshal(tag = "3")]
  Variant3,
}

#[derive(Debug, Clone, PartialEq, BinMarshal)]
#[marshal(ctx = MyContext, tag = "ctx.variant")]
enum MyEnum {
  #[marshal(tag = "1")]
  Variant1,
  #[marshal(tag = "2")]
  Variant2,
  #[marshal(tag = "3")]
  Variant3,
  #[marshal(tag = "4")]
  Variant4 {
    #[marshal(ctx = "construct", ctx_member(field = "variant", member = "ctx.inner"))]
    inner: Var4Inner
  }
}

#[derive(Debug, Clone, PartialEq, BinMarshal)]
#[marshal(ctx = MyContext)]
struct InnerStruct {
  #[marshal(ctx = "forward")]
  en: MyEnum
}

#[derive(Debug, Clone, PartialEq, BinMarshal)]
struct MyStruct {
  which_variant: u8,
  which_inner_variant: u8,
  #[marshal(ctx = "construct", ctx_member(field = "variant", member = "which_variant"), ctx_member(field = "inner", member = "which_inner_variant"))]
  inner: InnerStruct
}

fn main() {
  let mut v = MyStruct {
    which_variant: 0,
    which_inner_variant: 0,
    inner: InnerStruct {
      en: MyEnum::Variant4 { inner: Var4Inner::Variant2(131, 4213) }
    }
  };

  v.update(&mut ());

  assert_eq!(v.which_variant, 4);
  assert_eq!(v.which_inner_variant, 2);

  let mut bytes = [0u8; 256];
  let mut writer = BufferBitWriter::new(&mut bytes);
  v.clone().write(&mut writer, ());

  let slice = writer.slice();
  assert_eq!(slice.len(), 5);

  let v2 = MyStruct::read(&mut BitView::new(slice), ());
  assert_eq!(v2, Some(v));
}