use binmarshal::BinMarshal;
use binmarshal_macros::Context;

#[derive(Context)]
struct Context1 {
  a: u8
}

#[derive(Context)]
struct Context2 {
  flag: bool,
  a: u8
}

#[derive(Debug, Clone, PartialEq, BinMarshal)]
struct RootStruct {
  a: u8,
  #[marshal(ctx = "{ a }")]
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
  #[marshal(asymmetric_ctx)]
  inner: InnerEnum
}

#[derive(Debug, Clone, PartialEq, BinMarshal)]
#[marshal(ctx = Context2, tag = "ctx.flag")]
enum InnerEnum {
  #[marshal(tag = "true")]
  Value1,
  #[marshal(tag = "false")]
  Value2
}

fn main() {
  // let v = 
}