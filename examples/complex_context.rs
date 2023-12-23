use binmarshal::BinMarshal;

#[derive(Clone)]
struct Context1 {
  a: u8
}

#[derive(Clone)]
struct Context2 {
  flag: bool,
  a: u8
}

#[derive(Debug, Clone, PartialEq, BinMarshal)]
struct RootStruct {
  a: u8,

  #[marshal(
    ctx = "construct",
    ctx_member(field="a", member="a")
  )]
  b: InnerStruct
}

impl From<Context1> for Context2 {
  fn from(ctx: Context1) -> Self {
    Context2 {
      flag: ctx.a & 0b10000 != 0,
      a: ctx.a & 0b01111,
    }
  }
}

impl From<Context2> for Context1 {
  fn from(ctx: Context2) -> Self {
    Context1 {
      a: ((ctx.flag as u8) << 4) | (ctx.a & 0b01111)
    }
  }
}

#[derive(Debug, Clone, PartialEq, BinMarshal)]
#[marshal(ctx = Context1)]
struct InnerStruct {
  #[marshal(ctx = "coerce")]
  inner: Selector
}

#[derive(Debug, Clone, PartialEq, BinMarshal)]
#[marshal(ctx = Context2, tag = "ctx.a")]
enum Selector {
  #[marshal(tag = "0x12")]
  Something(#[marshal(ctx = "forward")] Maybe<u8, u16>),
  #[marshal(tag = "0x13")]
  Idk,
}


#[derive(Debug, Clone, PartialEq, BinMarshal)]
#[marshal(ctx = Context2, tag = "ctx.flag")]
enum Maybe<T: BinMarshal<()>, F: BinMarshal<()>> {
  #[marshal(tag = "true")]
  True(T),
  #[marshal(tag = "false")]
  False(F)
}

fn main() {
  let mut v = RootStruct {
    a: 0x00,
    b: InnerStruct { inner: Selector::Something(Maybe::True(16)) }
  };

  v.update(&mut ());

  assert_eq!(v.a, 0x12 | (0b1 << 4));
}